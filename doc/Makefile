# $Id: Makefile,v 1.1 2004-10-12 20:49:51 steffen Exp $

tlisp.pdf: tlisp.tex tlisp.bib
	pdflatex tlisp
	bibtex tlisp
	makeindex tlisp
	makeindex tlisp.glo -s nomencl.ist -o tlisp.gls
	pdflatex tlisp
	makeindex tlisp
	makeindex tlisp.glo -s nomencl.ist -o tlisp.gls
	pdflatex tlisp
	makeindex tlisp
	makeindex tlisp.glo -s nomencl.ist -o tlisp.gls
	pdflatex tlisp
	pdflatex tlisp

clean:
	-$(RM) *.aux
	-$(RM) *.bak
	-$(RM) *.bbl
	-$(RM) *.blg
	-$(RM) *.gl?
	-$(RM) *.idx
	-$(RM) *.ilg
	-$(RM) *.ind
	-$(RM) *.lo?
	-$(RM) *.out
	-$(RM) *.toc
	-$(RM) tlisp.pdf
