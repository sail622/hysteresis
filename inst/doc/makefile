# Makefile to use knitr for package vignettes

HTMLS= index.html

all: $(HTMLS) 

clean:
	rm -f *.md *.bbl *.blg *.aux *.out *.log *.png  *.tex

%.html: %.Rmd
	$(R_HOME)/bin/Rscript -e "library(knitr); library(hysteresis); knit2html('$*.Rmd')"