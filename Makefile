OCAMLMAKEFILE = ./OCamlMakefile

OCAMLFLAGS = -w Aelz

SOURCES = value.ml value_cimpl.c dump_dot.ml dump_sexpr.ml inspect.ml

RESULT = inspect

all:ncl bcl

-include $(OCAMLMAKEFILE)
