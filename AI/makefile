sprawozdanie : sprawozdanie.pdf partial_clean

%.pdf : %.tex
	        xelatex $*.tex

partial_clean :
	-rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb *.bbl *.blg

clean : partial_clean
	-rm *.pdf
