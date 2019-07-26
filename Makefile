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

temp: ${csvFiles}
	cat $(csvFiles) > $(statsDir)/test.csv
	ls stats

clean:
	rm -f ${ibcFiles}
	rm -f ${csvFiles}
	rm -rf stats
	mkdir stats

print-%: ; @echo $* = $($*)

.PHONY: build fetch-submodules clean stats temp



