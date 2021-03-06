statsDir=stats

idrisSourcePaths=Idris-dev/libs/prelude/Prelude
# idrisSourcePaths=Idris-dev/libs/base
idrisFiles=$(wildcard ${idrisSourcePaths}/*.idr)
csvFiles=$(patsubst %.idr,%.istats,$(idrisFiles))
ibcFiles=$(patsubst %.idr,%.ibc,$(idrisFiles))

build:
	stack build

fetch-submodules:
	git submodule update --init

# stats: $(shell find ${idrisSourcePaths} -name "*.idr" )
#   sts -o ${statsDir}/$?.stats.csv $?

%.istats: %.idr
	sts -o $@ $<

sts: ${csvFiles}
	cat $(csvFiles) > $(statsDir)/test.csv
	ls stats

stats.csv: ${csvFiles}
	./joinStats.py stats.csv ${csvFiles}
	sort --field-separator=',' --key=4gr -o stats.csv stats.csv

temp: ${csvFiles}
	cat $(csvFiles) > $(statsDir)/test.csv
	ls stats

clean:
	rm -f ${ibcFiles}
	rm -f ${csvFiles}
	rm -rf stats
	mkdir stats
	rm -f rerport/Main.pdf

report: report/Main.tex report/ita.bib
	pdflatex -shell-escape report/Main.tex

report-full: report/Main.tex report/ita.bib
	pdflatex -shell-escape report/Main.tex
	biber report/Main
	pdflatex -shell-escape report/Main.tex
	pdflatex -shell-escape report/Main.tex

typesgraph:
	dot -Tsvg types.dot -o types.svg

print-%: ; @echo $* = $($*)

.PHONY: build fetch-submodules clean stats temp



