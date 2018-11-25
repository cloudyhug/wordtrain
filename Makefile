all: main

main: wordtrain.ml
	ocamlbuild wordtrain.native

clean:
	rm -rf _build
	rm -f wordtrain.native