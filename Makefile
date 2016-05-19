OCBFLAGS = \
	-use-menhir \
	-menhir "menhir --external-tokens Token" \
	-package unix \
	-package num \
	-I src \
	-quiet

.PHONY: test build clean
.SILENT: build

test: build
	cat input.lambda | ./main.native

build:
	ocamlbuild $(OCBFLAGS) main.native

clean:
	ocamlbuild -clean
