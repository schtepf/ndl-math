##
##  GNU Makefile for LaTeX-Beamer slides
## 

include local/config.mk

## external software dependencies:
##   pdfnup   (psnup for PDF files, based on pdflatex and part of the PDFjam package)
##   epstopdf (part of teTeX and other modern latex distributions)
##   jpeg2ps  (converts JPG images to EPS, using the JPG support of newer PostScript dialects)
##   convert  (graphics format conversion utility from the ImageMagick suite)

LATEXMK = perl util/latexmk.perl -silent -pdf

EXTRA_CLEAN := $(EXTRA_CLEAN) workspace.pdf template.pdf

SLIDES := $(FILES:.pdf=.slides.pdf)
HANDOUT := $(FILES:.pdf=.handout.pdf)
NOTES := $(FILES:.pdf=.notes.pdf)

## JPG := $(wildcard img/*.jpg)	# required auto-generated image files (if any)
## JPG_EPS := $(JPG:.jpg=.eps)
## PNG := $(wildcard img/*.png)
## PNG_EPS := $(PNG:.png=.eps)
EPS := $(wildcard img/*.eps)	
EPS := $(filter-out $(JPG_EPS) $(PNG_EPS), $(EPS)) # don't generate .pdf from auto-generated .eps
EPS_PDF := $(EPS:.eps=.pdf)
IMAGES := $(EPS_PDF) $(JPG_EPS) $(PNG_EPS)

.PHONY:	clean realclean install all default images slides handout notes
.PRECIOUS: %.slides.pdf %.handout.pdf %.notes.pdf

default:
	@echo "Type one of the following commands:"
	@echo "  make  all"
	@echo "  make  slides | handout | notes"
	@echo "  make  my_talk ..."
	@echo "  make  install"
	@echo "  make  images"
	@echo "  make  [real]clean"

all:	slides handout notes

slides:	$(SLIDES)

handout:	$(HANDOUT)

notes:	$(NOTES)

images:	$(IMAGES)

install:
	@if [ "$(INSTALL_DIR)NULL" = "NULL" ]; then echo "Can't install: directory (\$$INSTALL_DIR) not set."; exit 1; fi
	@if [ ! -d "$(INSTALL_DIR)" ]; then echo "Can't install: directory $(INSTALL_DIR) doesn't exist."; exit 1; fi
	install -v -m 644 *.slides.pdf *.handout.pdf *.notes.pdf $(INSTALL_DIR)

clean:
	perl util/Hoover.perl -vr
	-rm $(FILES) $(EXTRA_CLEAN)

realclean:	clean
	-rm -f  $(SLIDES) $(HANDOUT) $(NOTES)

test:	$(IMAGES)

%.slides.pdf:	%.tex $(IMAGES)
	-rm -f $@
	perl util/EditFile.perl -HANDOUT -NOTES +SLIDES -NEVER $*.tex
	perl util/Hoover.perl -vr
	-rm -f $*.pdf
	$(LATEXMK) $*.tex
	mv $*.pdf $@

%.handout.pdf:	%.tex $(IMAGES)
	perl util/EditFile.perl +HANDOUT -NOTES -SLIDES -NEVER $*.tex
	perl util/Hoover.perl -vr
	-rm -f $*.pdf
	$(LATEXMK) $*.tex
	-rm -f $@
	pdfnup --nup 2x2 --a4paper --landscape --trim "-8mm -8mm -8mm -8mm" --outfile $@ $*.pdf

%.notes.pdf:	%.tex $(IMAGES)
	perl util/EditFile.perl -HANDOUT +NOTES -SLIDES -NEVER $*.tex
	perl util/Hoover.perl -vr
	-rm -f $*.pdf
	$(LATEXMK) $*.tex
	-rm -f $@
	pdfnup --nup 2x2 --a4paper --landscape --trim "-8mm -8mm -8mm -8mm" --outfile $@ $*.pdf

%:	%.handout.pdf %.notes.pdf %.slides.pdf
	perl util/EditFile.perl -HANDOUT -NOTES +SLIDES -NEVER $*.tex
	@echo "Done"

img/%.pdf:	img/%.eps
	epstopdf $<

img/%.eps:	img/%.jpg
	jpeg2ps $< > $@

img/%.jpg:	img/%.png
	convert $< jpg:$@	# temporary file should be deleted automatically
