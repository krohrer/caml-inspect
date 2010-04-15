OCAMLMAKEFILE = ./OCamlMakefile

OCAMLFLAGS = -w Aelz

SOURCES = value.mli value.ml value_cimpl.c dump_dot.mli dump_dot.ml dump_dot.mli dump_sexpr.ml inspect.mli inspect.ml
DOC_FILES = inspect.mli

RESULT = inspect

all:ncl bcl htdoc

-include $(OCAMLMAKEFILE)
