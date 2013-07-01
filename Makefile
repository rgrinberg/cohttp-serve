
bin=/usr/local/bin

PREFIX ?= $(shell dirname $(shell dirname `ocamlc -where`))

BINDIR ?= $(PREFIX)/bin

all:
	ocamlbuild -use-ocamlfind cohttp_serve.native

clean:
	ocamlbuild -clean

opaminstall: 
	cp cohttp_serve.native $(BINDIR)/cohttp_serve

opam uninstall:
	rm -f $(BINDIR)/cohtt_serve
