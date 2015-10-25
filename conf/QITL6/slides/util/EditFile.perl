#!/usr/bin/perl -w
# -*-cperl-*-
###################################################################
###                                                             ###
###    Author: Stefan Evert                                     ###
###   Purpose: Edit latex file for handout/presentation mode    ###
###   Created: Wed May  5 19:08:19 2004                         ###
###  Modified: Wed May  5 19:31:46 2004 (evert)                 ###
###                                                             ###
###################################################################
# 
# 
# 

use FileHandle;

@Plus = ();
@Minus = ();
while (@ARGV and $ARGV[0] =~ /^([+-])([A-Z]+)$/) {
  ($code, $label) = ($1, $2);
  $rx = qr/\%\s*$label\s*$/; 
  if ($code eq "+") { push @Plus, $rx } else { push @Minus, $rx }
  shift @ARGV;
}

die "Usage:  EditFile.perl -PRESENTATION +HANDOUT slides.tex\n"
  unless @ARGV == 1 and (@Plus > 0 or @Minus > 0);
$filename = shift @ARGV;

$fh = new FileHandle $filename
  or die "Can't open file '$filename'. Aborted\n";
@lines = <$fh>;
$fh->close
  or die "Error reading file '$filename'. Aborted.\n";

$subst = 0;
foreach $line (@lines) {
  $plus = grep { $line =~ $_ } @Plus;
  $minus = grep { $line =~ $_ } @Minus;
  die "Shucks! The following line matches both +XXX and -XXX:\n", $line
    if $plus and $minus;
  $line = "\%\% $line"
    if $minus and $line !~ /^\s*\%\%/;
  $line =~ s/^\s*\%\% ?//
    if $plus;
  $subst++
    if $plus or $minus;
}

if ($subst == 0) {
  print "Warning: no substitutions made in file '$filename'.\n";
  exit 0;
}

$backup = "$filename~";
unlink($backup)
  if -f $backup;
die "Can't create backup file '$backup'. Aborted.\n"
  unless 0 == system("cp", "-p", $filename, $backup);
$fh = new FileHandle "> $filename"
  or die "Can't save modified file '$filename'. Aborted.\n";
map { print $fh $_ } @lines;
$fh->close
  or die "Error writing file '$filename'. Use the backup file '$backup'.\n";
print "Warning: can't restore access times of file '$filename'.\n"
  unless 0 == system("touch", "-r", $backup, $filename);
