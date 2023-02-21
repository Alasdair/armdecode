.PHONY: armdecode clean

all: armdecode

armdecode:
	dune build --release
	cp _build/default/armdecode.cmxs plugin.cmxs
	chmod +rwx plugin.cmxs

clean:
	-dune clean
