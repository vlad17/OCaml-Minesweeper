# Simple makefile for now with no automatic dependency generation

TARGET=mine_model

all: native
	mv $(TARGET).native minesweeper

native:
	ocamlbuild -lib graphics src/$(TARGET).native
