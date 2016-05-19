.PHONY: run clean

run:
	cat input.lambda | ocamlbuild -use-menhir -package unix main.native --

clean:
	ocamlbuild -clean
