
all:
	ocamlbuild -use-ocamlfind cohttp_serve.native

clean:
	ocamlbuild -clean
