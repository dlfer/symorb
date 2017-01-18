include ../Makefile.local

default: manual.pdf

clean: 
	rm -f *.{aux,log,toc}

all:

manual.pdf:
	pdftex manual.tex && pdftex manual.tex

