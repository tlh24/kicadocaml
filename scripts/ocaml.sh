#!/bin/sh
ledit | ocaml -I `ocamlfind query pcre`\
 -I `ocamlfind query labltk`\
 -I `ocamlfind query lablGL`\
 -I `ocamlfind query lablGL.togl`\
 -I `ocamlfind query unix`\
 -I `ocamlfind query netsys`\
 -I `ocamlfind query netstring`\
 pcre.cma labltk.cma lablgl.cma togl.cma unix.cma netsys.cma netstring.cma
