# Simple makefile for now with no automatic dependency generation

TARGET=mine_main

all: native
	mv src/$(TARGET).native minesweeper

native:
	cd src/ && ocamlbuild -r -use-ocamlfind -lib graphics $(TARGET).native && cd ..
