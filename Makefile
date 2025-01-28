.PHONY: all
all: 
	latexmk -shell-escape -cd -pdfxe report.tex
	makeglossaries report
	latexmk -shell-escape -cd -shell-escape -pdfxe report.tex
