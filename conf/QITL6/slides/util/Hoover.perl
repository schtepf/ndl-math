#!/usr/bin/perl -w
# -*-cperl-*-
$| = 1;

use Getopt::Long;
use DirHandle;

our $VERSION = "1.0.2";

## hard-coded cleanup specifications (filenames and patterns):
sub unconditional_delete {      ## always delete these files:
  local($_) = shift;
  return 1 if /~$/ or /^\#.*\#$/ or /\.(xyc|blg)$/;       # emacs backup files, .xyc .blg
  return 1 if $_ eq "texput.log" or $_ eq "missfont.log"; # special tex log files
  return 0;
} # 0 = don't delete, 1 = delete
sub tex_delete {        ## delete these files if corresponding *.tex exists
  local($_) = shift;
  return (1, $`) if /\.(aux|aux\.bak|bbl|dvi|out|toc|lot|lof|fls)$/;    # .aux .bbl .dvi .out .toc .lot .lof
  return (2, $`)                  # .ent .idx .ilg .ind .log .nav .ndx .nin .ps .snm .vrb .tdo
    if /\.(ent|idx|ilg|ind|log|nav|ndx|nin|nlo|ps|snm|tdo|vrb|fdb_latexmk)$/;
  return 0;
} # 0 = don't delete, 1 = delete if *.tex exists or --all, 2 = only delete if *.tex exists 
sub conditional_delete {      ## delete these files if a "base file" exists 
  local($_) = shift;
  return $` if /\.flc$/;                # .flc (emacs font lock cache)
  return 0;
} # 0 = don't delete, $file = delete if $file exists
sub macosx_delete{    ## delete unconditionally with --macosx
  local($_) = shift;
  return 1 if /^\.DS_Store$/;
  return 1 if /^\._/;
  return 0;
}


$Opt_Recurse = 0;   # recurse into subdirectories?
$Opt_All = 0;       # delete TeX temporary files even if there is no associated .tex?
$Opt_OSX = 0;       # delete files specific to Mac OS X (when sharing data with other operating systems)
$Opt_Dry = 0;       # dry run: don't delete any files, just show what would have been done
$Opt_Verbose = 0;   # verbose: show which files are deleted
$Opt_Help = 0;

Getopt::Long::Configure(qw<no_auto_abbrev permute bundling>);
$ok = GetOptions(
     "recurse|r" => \$Opt_Recurse,
     "all|a" => \$Opt_All,
     "macosx|mac|m" => \$Opt_OSX,
     "dry-run|n" => \$Opt_Dry,
     "verbose|v" => \$Opt_Verbose,
     "help|h" => \$Opt_Help,
    );
usage()
  if $Opt_Help or not $ok; 
$Opt_Verbose = 1    # always report file names in dry run
    if $Opt_Dry;

@ARGV = "."
  unless @ARGV;
foreach $dir (@ARGV) {
  if (not -d $dir) {
    print STDERR "Warning: $dir is not a directory (skipped)\n"
  }
  else {
    hoover_dir($dir);
  }
}

## clean up in directory (and recurse into subdirectories if requested)
sub hoover_dir {
  my $dir = shift;
  my @files = ();
  my $dh = new DirHandle $dir;
  if (not defined $dh) {
    print STDERR "Warning: can't read directory $dir (skipped)\n";
    return;
  }
  while (my $f = $dh->read) {
    push @files, $f;
  }
  $dh->close;
  # lookup hash of files (used when checking existence of *.tex file etc.)
  my %lookup = map {$_ => 1} grep {not -d "$dir/$_"} @files;
  # otherwise, ignore symbolic links completely
  @files = grep {not -l "$dir/$_"} @files;
  # split into directories (for recursion) and plain files (for deletion)
  my @dirs = grep {-d} map {"$dir/$_"} grep {not /^\.+$/} @files;
  @files = grep {-f "$dir/$_"} @files;  # special files etc. are ignored
  # delete files conditionally or unconditionally
  foreach $file (@files) {
    # remove these files unconditionally
    if (unconditional_delete($file)) {
      delete_file($dir, $file);
      next;
    }
    ($delete, $basename) = tex_delete($file);
    if ($delete) {
      delete_file($dir, $file)
        if exists $lookup{"$basename.tex"} or ($Opt_All and $delete == 1);
      next;
    }
    $test_file = conditional_delete($file);
    if ($test_file) {
      delete_file($dir, $file)
        if exists $lookup{$test_file};
      next;
    }
    if ($Opt_OSX) {
      delete_file($dir, $file)
        if macosx_delete($file);
    }
  }
  if ($Opt_Recurse) {
    foreach my $d (@dirs) {
      hoover_dir($d);
    }
  }
}

## delete file in specified directory (unless --dry-run was specified)
sub delete_file {
  my $dir = shift;
  my $f = shift;
  my $filename = "$dir/$f";
  if ($Opt_Verbose) {
    print "$filename\n";
  }
  unless ($Opt_Dry) {
    my $ok = unlink $filename;
    print "Warning: can't delete $filename\n"
      unless $ok == 1;
  }
}


# usage();
#   print short usage information and exit
sub usage {
  print STDERR "\n";
  print STDERR "Usage:  Hoover [-r] [-a] [-m] [-n] [<dir> ...]\n\n";
  print STDERR "Hoover deletes all sorts of temporary files (including\n";
  print STDERR "emacs backup / autosave files and TeX temporary files)\n";
  print STDERR "in the specified directories, or in the current directory\n";
  print STDERR "if no arguments are given.\n\n";
  print STDERR "Options:\n";
  print STDERR "  --recurse, -r\n";
  print STDERR "      recurse into subdirectories (doesn't follow symbolic links)\n";
  print STDERR "  --all, -a\n";
  print STDERR "      always delete TeX temporary files (use with caution!)\n";
  print STDERR "      (default: delete only when corresponding *.tex is found)\n";
  print STDERR "  --macosx, --mac, -m\n";
  print STDERR "      remove Mac OS X-specific files (.DS_Store, extended attributes)\n";
  print STDERR "      use with great caution!\n";
  print STDERR "  --dry-run, -n\n";
  print STDERR "      only show what _would_ have been deleted\n";
  print STDERR "  --verbose, -v\n";
  print STDERR "      print names of deleted files\n";
  print STDERR "  --help, -h\n";
  print STDERR "      show this help page\n";
  print STDERR "\n";
  print STDERR "Type 'perldoc Hoover' for more information and terms of use.\n";
  print STDERR "Hoover v$VERSION Copyright (C) 2003-2010 by Stefan Evert\n";
  print STDERR "\n";
  exit 2;
}

__END__

=head1 NAME

Hoover - Clean up temporary files from TeX and Emacs

=head1 SYNOPSIS

  Hoover [-r] [-a] [-m] [-n] [<dir> ...]

=head1 DESCRIPTION

When TeX/LaTeX is used to format a document, many temporary files are created
in the same directory as the source file.  Unfortunately, TeX does not offer
an easy way to remove these temporary files when they are no longer needed.
The B<Hoover> program was designed to take over this job. It also deletes
Emacs backup and cache files.

When B<Hoover> is invoked without any arguments, it will silently delete all
temporary files in the current directory.  Optionally, one or more directories
to be cleaned up can be specified.  With the C<--recurse> (or C<-r>) option,
B<Hoover> will also recursively clean up all subdirectories.  When C<--verbose> 
(or C<-v>) is specified, the names of all deleted files are printed on the screen.

Especially when recursion is enabled, it is recommended to start with a dry
run (C<--dry-run> or C<-n>), which only prints the names of detected temporary
files but does not delete them.  The C<--all> (or C<-a>) option forces
deletion of certain file types even if no corresponding C<.tex> file can be
found. I<Use with caution and read L<"DELETION RULES"> below!>

The C<--macosx> (or C<--mac> or C<-m>) option additionally deletes some files
specific to Mac OS X (C<.DS_Store>, C<._*> for extended attributes on non-HFS
volumes).  This can be useful to clean up directories shared with other
operating systems, but I<use this option with great caution>!

When invoked with the C<--help> (or C<-h>) option, B<Hoover> prints a short
usage summary.  Note that unlike most other Perl scripts, B<Hoover> allows
bundling of single-letter options (the most useful combinations being C<Hoover
-nr>, followed by C<Hoover -vr>).

=head1 DELETION RULES

B<Hoover> bases its decision of which files to delete on file names and
extensions.  These rules are hard-coded: if you want to change them, you will
have to edit the first lines of the script.  Deletion rules can be divided
into two categories: B<unconditional> and B<conditional> deletes.  The latter
can be further subdivided into B<overridable> (viz., with C<--all>) and
B<non-overridable> rules.

=head2 Unconditional deletes

The following types of files are recognised as temporary simply by their name
or extension, and will always be deleted.

=over 4

=item *

All file names ending in C<~> (Emacs backup files).

=item * 

File names beginning I<and> ending with C<#> (Emacs recovery files)

=item * 

Files with the extension C<.xyc> or C<.blg> (created by certain LaTeX packages).

=item *

Files named C<texput.log> or C<missfont.log> (TeX log files).

=back 

=head2 Conditional deletes

The following types of files are only deleted when a corresponding TeX or
LaTeX source file exists, i.e. a file with the same basename followed by the
extension C<.tex>.  For instance, the file F<manual.dvi> will only be deleted
when there is a file F<manual.tex> in the same directory.

=over 4

=item *

Files with one of the following extensions: C<.aux>, C<.bbl>, C<.dvi>,
C<.ent>, C<.idx>, C<.ilg>, C<.ind>, C<.log>, C<.nav>, C<.ndx>, C<.nin>, C<.nlo>,
C<.out>, C<.ps>, C<.snm>, C<.toc>, C<.tdo>, C<.vrb>, C<.fdb_latexmk>

=item *

Files with the extension C<.flc> (Emacs font lock cache) are deleted if a
corresponding file without this extension exists in the same directory (e.g.
F<sort.c.flc> will be deleted if F<sort.c> exists).

=back

=head3 Overridable conditional deletes

For certain conditional deletes, the requirement that a corresponding source
file must exist can be overridden with the C<--all> (or C<-a>) option.  These
involve the following types of files, which are sometimes created by TeX
programs that are invoked with a mistyped file name.

=over 4

=item *

Files with the extension C<.aux>, C<.bbl>, C<.dvi>, C<.out>, C<.toc>, C<.lot>,
C<.lof>.

=back

I<Use the C<--all> option with great caution!> You should always perform a dry
run (C<--dry-run>, C<-n>) first to make sure no important files are deleted
accidentally.

=head3 Optional deletes

Further deletes can be activated with command-line options.  Use these
I<with great caution> and always perform a C<--dry-run> first in order to
make sure that no important files are accidentally deleted.

=over 4

=item *

Files named C<.DS_Store> and files starting with C<._> will be deleted if
the option C<--macosx> (or C<--mac> or C<-m>) is specified.

=back


=head1 COPYRIGHT

This software is provided AS IS and the author makes no warranty as to
its use and performance. You may use the program, redistribute and
modify it under the same terms as Perl itself.

IN NO EVENT WILL THE AUTHOR BE LIABLE TO YOU FOR ANY CONSEQUENTIAL,
INCIDENTAL OR SPECIAL DAMAGES, INCLUDING ANY LOST PROFITS OR LOST
SAVINGS, EVEN IF HE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES, OR FOR ANY CLAIM BY ANY THIRD PARTY.

Copyright (C) 2003-2010 by Stefan Evert (L<http://purl.org/stefan.evert>).

=cut

