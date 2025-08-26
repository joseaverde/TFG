.PHONY: all

UMLIMGS := uml/casos-de-uso.svg uml/gantt.svg uml/arquitectura.svg

all: $(UMLIMGS)
	typst compile report.typ

$(UMLIMGS) : %.svg : %.uml
	plantuml -tsvg $< -o .

clean:
	@rm uml/*.png
	@rm uml/*.svg
