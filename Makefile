.DEFAULT: all

OCAMLMAKEFILE = ./OCamlMakefile

OCAMLFLAGS = -w Aelz

LIB_PACK_NAME = inspect

OCAMLDOCFLAGS = -intro inspect.doc -t CAML-Inspect

SOURCES = aux.mli aux.ml value.mli value.ml value_cimpl.c dot.mli dot.ml sexpr.mli sexpr.ml

DOC_DIR = doc
DOC_FILES = aux.mli value.mli dot.mli sexpr.mli

RESULT = inspect

all:ncl bcl htdoc
	open $(DOC_DIR)/$(RESULT)/html/index.html

-include $(OCAMLMAKEFILE)
