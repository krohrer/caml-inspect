.DEFAULT: all

OCAMLMAKEFILE = ./OCamlMakefile

RESULT = inspect

OCAMLFLAGS = -w Aelz
LIB_PACK_NAME = $(RESULT)
SOURCES = aux.mli aux.ml value.mli value.ml value_cimpl.c dot.mli dot.ml sexpr.mli sexpr.ml

OCAMLDOCFLAGS = -intro $(RESULT).doc -t "CAML-Inspect"
DOC_FILES = aux.mli value.mli dot.mli sexpr.mli

LIBINSTALL_FILES = aux.mli value.mli dot.mli sexpr.mli aux.cmi value.cmi dot.cmi sexpr.cmi $(RESULT).cmi $(RESULT).cma $(RESULT).cmxa $(RESULT).a lib$(RESULT)_stubs.a dll$(RESULT)_stubs.so

all:ncl bcl htdoc
install:libinstall
uninstall:libuninstall

-include $(OCAMLMAKEFILE)
