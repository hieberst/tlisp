# $Id$

tlisp.pdf: tlisp.tex tlisp.bib
	pdflatex tlisp
	bibtex tlisp
	makeindex tlisp
	makeindex tlisp.nlo -s nomencl.ist -o tlisp.nls
	pdflatex tlisp
	makeindex tlisp
	makeindex tlisp.nlo -s nomencl.ist -o tlisp.nls
	pdflatex tlisp
	makeindex tlisp
	makeindex tlisp.nlo -s nomencl.ist -o tlisp.nls
	pdflatex tlisp
	pdflatex tlisp

clean:
	-$(RM) *.aux
	-$(RM) *.bak
	-$(RM) *.bbl
	-$(RM) *.blg
	-$(RM) *.gl?
	-$(RM) *.gz
	-$(RM) *.idx
	-$(RM) *.ilg
	-$(RM) *.ind
	-$(RM) *.lo?
	-$(RM) *.nl?
	-$(RM) *.out
	-$(RM) *.toc
	-$(RM) *.*~
	-$(RM) tlisp.pdf
