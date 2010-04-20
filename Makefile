.DEFAULT: all

OCAMLMAKEFILE = ./OCamlMakefile

OCAMLFLAGS = -w Aelz

LIB_PACK_NAME = inspect

OCAMLDOCFLAGS = -intro inspect.doc -t CAML-Inspect

# OCAMLFIND_INSTFLAGS = -optional inspect.mli

SOURCES = aux.mli aux.ml value.mli value.ml value_cimpl.c dot.mli dot.ml sexpr.mli sexpr.ml

DOC_DIR = doc
DOC_FILES = aux.mli value.mli dot.mli sexpr.mli

RESULT = inspect

all:ncl bcl htdoc

LIBINSTALL_FILES = aux.mli value.mli dot.mli sexpr.mli aux.cmi value.cmi dot.cmi sexpr.cmi inspect.cmi inspect.cma inspect.cmxa	inspect.a libinspect_stubs.a dllinspect_stubs.so

install:libinstall

uninstall:libuninstall

-include $(OCAMLMAKEFILE)
