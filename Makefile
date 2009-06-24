
OCAMLC = ocamlfind ocamlc -g -c -w x
OCAMOPT = ocamlfind ocamlopt -c -w x -inline 3
OPTIONS = -package pcre,lablGL,labltk,lablGL.togl,unix
LINKER = ocamlfind ocamlc -g

OBJS = pts2.cmo comm.cmo grfonte.cmo grfx.cmo modtext.cmo grid.cmo \
	shape.cmo pad.cmo mod.cmo track.cmo ratnest.cmo drc.cmo align.cmo \
	propagate.cmo schematic.cmo doarray.cmo mouse.cmo glwindow.cmo \
	mesh.cmo poly.cmo zone.cmo blockrotate.cmo find.cmo kicadocaml.cmo 
#order matters here! 
OPTOBJS = $(OBJS:.cmo=.cmx)
SRC = $(OBJS:.cmo=.ml)

.SUFFIXES: .ml .mli .cmo .cmi .cmx .c .o

.ml.cmo:
	$(OCAMLC) $(OPTIONS) $<

.ml.cmx:
	$(OCAMOPT) $(OPTIONS) $<
	
kicadocaml: $(OBJS)
	$(LINKER) -linkpkg $(OPTIONS) $(OBJS) -o kicadocaml
	
kicadocaml.opt: $(OPTOBJS)
	ocamlfind ocamlopt -o kicadocaml.opt -inline 3 -linkpkg $(OPTIONS) $(OPTOBJS) -verbose
	
dump.odoc : $(SRC)
	ocamlfind ocamldoc $(OPTIONS) $(SRC) -dump dump.odoc
	
all: kicadocaml 				#bytecode

opt:	kicadocaml.opt  	#native code

doc: dump.odoc # documenation (e.g. for cameleon)

clean: 
	rm -f $(OBJS) $(OPTOBJS)
	rm -f *.cmi
	rm -f *.o
	rm -f kicadocaml kicadocaml.opt
	
install: 
	cp kicadocaml.opt /usr/local/bin/kicadocaml
