#lex&yacc
scanner.ml: scanner.mll
	ocamllex scanner.mll # create scanner.ml
parser.mli: parser.mly ast.mli
	ocamlyacc parser.mly # create parser.ml and parser.mli

#mli
ast.ml: 
	ocamlc -c ast.mli # compile AST types
parser.ml: parser.mli
	ocamlc -c parser.mli # compile parser types

#ml
scanner.cmo: scanner.ml 
	ocamlc -c scanner.ml # compile the scanner
parser.cmo: parser.ml 
	ocamlc -c parser.ml # compile the parser
verishort.cmo: verishort.ml
	ocamlc -c verishort.ml # compile the interpreter
	
#all
vsc: parser.cmo scanner.cmo verishort.cmo
	ocamlc -o vsc parser.cmo scanner.cmo verishort.cmo
all: vsc

run: vsc
	./testbench.pl
