FFILES=$(shell find problems/ -name "*.v" | sort -R)
OFILES=$(patsubst problems/%.v,problems/%.vo,$(FFILES))

all: $(OFILES)

problems/%.vo: problems/%.v
	-htimeout $(LIMIT) coqc "$<" > logs/`basename "$@" .vo`.log 2>&1

ifndef VERBOSE
.SILENT:
endif
