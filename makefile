LATEX := xelatex
BUILDDIR := pdf
TEXDIR := tex

# ASSIGNMENT_TEXS := $(addprefix $(TEXDIR)/,21014912\ Case\ Study\ Report\ 24.tex assignment_introduction.tex assignment_method_summary.tex assignment_method_strengths_and_weaknesses.tex assignment_implementation.tex assignment_conclusion.tex assignment_appendix_code.tex code/coppersmith_rsa.mw references.bib)
TEX_FILES := $(addprefix $(TEXDIR)/,21014912*.tex assignment_*.tex)

TARGETNAME := 21014912_Case_Study_Report_24
PDF_FILES := $(BUILDDIR)/$(TARGETNAME).pdf

LATEX_INTERMEDIATE := $(BUILDDIR)/
TEXINPUTS:="./tex/"

.PHONY: clean clean-bcf
.PRECIOUS:= $(BUILDDIR)/%.bcf $(BUILDDIR)/%.bbl $(BUILDDIR)/%.toc


$(BUILDDIR)/%.bcf: $(TEXDIR)/%.tex
	TEXINPUTS="./tex/:" ; $(LATEX) -synctex=1 -interaction=nonstopmode --shell-escape -output-directory=$(BUILDDIR) $<

$(BUILDDIR)/%.bbl: $(BUILDDIR)/%.bcf $(TEXDIR)/references.bib
	biber --input-directory $(TEXDIR) $<

$(BUILDDIR)/%.toc: $(TEXDIR)/%.tex
	TEXINPUTS="./tex/:" ; $(LATEX) -synctex=0 -interaction=nonstopmode --shell-escape -output-directory=$(BUILDDIR) $<

$(BUILDDIR)/%.pdf: $(TEXDIR)/%.tex $(BUILDDIR)/%.bbl $(BUILDDIR)/%.toc $(TEX_FILES)
	TEXINPUTS="./tex/:" ; $(LATEX) -synctex=1 -interaction=nonstopmode --shell-escape -output-directory=$(BUILDDIR) $<

$(BUILDDIR):
	mkdir $(BUILDDIR)

all: $(PDF_FILES) $(BUILDDIR)


clean-bcf:
	-rm $(BUILDDIR)/*.bcf $(BUILDDIR)/*.blg

clean:
	-rm $(BUILDDIR)/*.aux $(BUILDDIR)/*.log $(BUILDDIR)/*.out $(BUILDDIR)/*.toc

info:
	@ls -l $(TEX_FILES) pdf/21014912*.pdf pdf/21014912*.b* pdf/21014912*.toc pdf/21014912*.pdf
