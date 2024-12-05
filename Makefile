.PHONY: all
all: 
	latexmk -cd -shell-escape -pdfxe report.tex
	makeglossaries report
	latexmk -cd -shell-escape -pdfxe report.tex
