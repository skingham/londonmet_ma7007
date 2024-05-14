LATEX := xelatex
BUILDDIR := pdf
TEXDIR := tex

TEX_FILES := $(addprefix $(TEXDIR)/,21014912*.tex assignment_*.tex)

TARGET_NAME := 21014912_Case_Study_Report_24
PDF_FILES := $(BUILDDIR)/$(TARGET_NAME).pdf

LATEX_INTERMEDIATE := $(BUILDDIR)/
TEXINPUTS:="./tex/"

.PHONY: clean clean-bcf
.SECONDARY: $(BUILDDIR)/$(TARGET_NAME).bcf $(BUILDDIR)/$(TARGET_NAME).bbl $(BUILDDIR)/$(TARGET_NAME).toc


$(BUILDDIR)/%.bbl: $(TEXDIR)/references.bib $(TEX_FILES)
	biber --input-directory $(TEXDIR) $(BUILDDIR)/$*.bcf

$(BUILDDIR)/%.bcf: $(TEXDIR)/%.tex $(TEX_FILES)
	TEXINPUTS="./tex/:" ; $(LATEX) -synctex=1 -interaction=nonstopmode --shell-escape -output-directory=$(BUILDDIR) $<

$(BUILDDIR)/%.toc: $(TEXDIR)/%.tex $(TEX_FILES)
	TEXINPUTS="./tex/:" ; $(LATEX) -synctex=0 -interaction=nonstopmode --shell-escape -output-directory=$(BUILDDIR) $<

$(BUILDDIR)/%.pdf: $(TEXDIR)/%.tex $(BUILDDIR)/%.bcf $(BUILDDIR)/%.bbl $(BUILDDIR)/%.toc $(TEX_FILES)
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
