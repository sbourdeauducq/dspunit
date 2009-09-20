TEX= $(wildcard *.tex)
EPS= $(wildcard *.eps)
PDF= $(TEX:.tex=.pdf)
PDFEPS= $(EPS:.eps=.pdf)

all: $(PDF)

pst: $(PDF)

eps: $(PDFEPS)

%.pdf: %.tex
	./bin/buildpstricks $<

.PHONY: clean mrproper

clean :
	rm -rf ${OBJ}
	rm -rf Makefile.deps

mrproper: clean
	rm -rf $(EXEC)



