# to build, you need to : 
# apt-get install ocaml libpcre-ocaml-dev ocaml-findlib libocamlnet-ocaml-dev

OCAMLC = ocamlfind ocamlc -g -c -w x
OPTIONS = -package pcre,unix,netsys,netstring,
LINKER = ocamlfind ocamlc -g

OBJS = digikeyweborder.cmo 

SRC = $(OBJS:.cmo=.ml)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .c .o

.ml.cmo:
	$(OCAMLC) $(OPTIONS) $<
	
digikeyweborder: $(OBJS)
	$(LINKER) -linkpkg $(OPTIONS) $(OBJS) -o digikeyweborder

all: digikeyweborder 				#bytecode

clean: 
	rm -f $(OBJS)
	rm -f *.cmi
	rm -f digikeyweborder
