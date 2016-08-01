all:
	ocamlbuild -classic-display -use-ocamlfind syntax/ppx_ocamllex.native

clean:
	ocamlbuild -clean

.PHONY: all clean
