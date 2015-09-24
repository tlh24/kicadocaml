PROFILE = FALSE

OCAMLC = ocamlfind ocamlc -g -c -w x -w Z
OPTIONS = -package camlp4,pcre,lablGL,labltk,lablGL.togl,unix
LINKER = ocamlfind ocamlc -g

ifeq ($(strip $(PROFILE)),TRUE)
OCAMOPT = ocamlfind ocamlopt -p -g -c -w x -w Z
else 
OCAMOPT = ocamlfind ocamlopt -c -w x -w Z -inline 30
endif
OBJS1 = pts2.cmo comm.cmo grfonte.cmo grfx.cmo modtext.cmo grid2.cmo \
	shape.cmo pad.cmo mod.cmo track.cmo ratnest.cmo drc.cmo align.cmo \
	propagate.cmo schematic.cmo teardrop.cmo
OBJS2 = netlist.cmo doarray.cmo mouse.cmo glwindow.cmo \
	mesh.cmo poly.cmo zone.cmo blockrotate.cmo find.cmo anneal.cmo pull.cmo kicadocaml.cmo 
OBJS = $(OBJS1) $(OBJS2)
#order matters here! 
OPTOBJS = $(OBJS:.cmo=.cmx)
OPTOBJS1 = $(OBJS1:.cmo=.cmx)
SRC = $(OBJS:.cmo=.ml)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .c .o

.ml.cmo:
	$(OCAMLC) $(OPTIONS) $<

.ml.cmx:
	$(OCAMOPT) $(OPTIONS) $<
	
all: kicadocaml.opt 	#native code

byte: kicadocaml  	#bytecode 
	
netlist.cmo:netlist.ml $(OBJS1)
	$(OCAMLC) -syntax camlp4o $(OPTIONS),camlp4 $<
	
netlist.cmx:netlist.ml $(OPTOBJS1)
	$(OCAMOPT) -syntax camlp4o $(OPTIONS),camlp4 $<
	
kicadocaml.cmo:kicadocaml.ml $(OBJS1) zone.ml
	$(OCAMLC) $(OPTIONS),camlp4 $<
	
kicadocaml.cmx:kicadocaml.ml $(OPTOBJS1) zone.ml
	$(OCAMOPT) $(OPTIONS),camlp4 $<
	
kicadocaml: $(OBJS)
	$(LINKER) -linkpkg $(OPTIONS) $(OBJS) -o kicadocaml
	
kicadocaml.opt: $(OPTOBJS)
	ocamlfind ocamlopt -o kicadocaml.opt -inline 30 -linkpkg $(OPTIONS) $(OPTOBJS)
	
dump.odoc : $(SRC)
	ocamlfind ocamldoc $(OPTIONS) $(SRC) -dump dump.odoc


doc: dump.odoc # documenation (e.g. for cameleon)

install: 
	cp kicadocaml.opt /usr/local/bin/kicadocaml
	
deps:
	sudo apt-get install ocaml liblablgl-ocaml-dev libpcre-ocaml-dev ocaml-findlib tk-dev tcl-dev

clean: 
	rm -f $(OBJS) $(OPTOBJS)
	rm -f *.cmi
	rm -f *.o
	rm -f kicadocaml kicadocaml.opt

