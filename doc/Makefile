QUIP        := quip

IMG_DIR     := images
SCRIPT_DIR  := scripts

FIGS        := $(wildcard $(IMG_DIR)/*.fig)
GIFS        := $(wildcard $(IMG_DIR)/*.gif)
PDFS        := $(GIFS:.gif=.pdf) $(FIGS:.fig=.pdf)
EPSFS       := $(GIFS:.gif=.epsf) $(FIGS:.fig=.epsf)

all: tutorial.pdf tutorial.ps tutorial_examples.tar.gz

%.pdf: %.fig
	fig2dev -L pdf $< $@

%.epsf: %.fig
	fig2dev -L eps $< $@

%.pdf: %.gif
	convert $< $@

%.epsf: %.gif
	convert $< $@

%.tex: %.xml \
              $(SCRIPT_DIR)/transformscript.xsl \
              $(SCRIPT_DIR)/transformscript.xquery \
              $(SCRIPT_DIR)/deleteTags.xquery \
              $(SCRIPT_DIR)/latexVerbatim.xquery
	$(QUIP) -exit_on_fail -value_output -input $< -o $@ \
	        -include $(SCRIPT_DIR)/transformscript.xquery \
	        -include $(SCRIPT_DIR)/deleteTags.xquery \
	        $(SCRIPT_DIR)/latexVerbatim.xquery

# NOTE: Strangely enough, the PDF and the DVI output differs a bit, so we keep
# the intermediate files apart in the two rules below.

tutorial.pdf: tutorial.tex bib.bib $(PDFS)
	cp $< tutorial_pdf.tex
	pdflatex tutorial_pdf
	-bibtex tutorial_pdf
	makeindex Klassen
	pdflatex tutorial_pdf
	pdflatex tutorial_pdf
	$(RM) $@
	mv tutorial_pdf.pdf $@

tutorial.dvi: tutorial.tex bib.bib $(EPSFS)
	cp $< tutorial_ltx.tex
	latex tutorial_ltx
	-bibtex tutorial_ltx
	makeindex Klassen
	latex tutorial_ltx
	latex tutorial_ltx
	$(RM) $@
	mv tutorial_ltx.dvi $@

tutorial.ps: tutorial.dvi $(EPSFS)
	dvips -o tutorial.ps tutorial.dvi

tutorial_examples.tar.gz: tutorial.xml extractExamples $(SCRIPT_DIR)/deleteTags.xquery
	./extractExamples

clean::
	$(RM) *.aux *.bbl *.blg *.dvi *.idx *.ilg *.ind *.lof *.log *.pdf *.ps *.tex *.toc
	$(RM) $(PDFS)
	$(RM) $(EPSFS)
	rm -rf bin obj src
	$(RM) code.xml header.xml klasse.xml examples.html tutorial_examples.tar.gz
