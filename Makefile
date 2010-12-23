#lex&yacc
scanner.ml: scanner.mll
	ocamllex scanner.mll # create scanner.ml
parser.mli: parser.mly ast.ml
	ocamlyacc parser.mly # create parser.ml and parser.mli

#mli
ast.cmo: 
	ocamlc -c ast.ml # compile AST types
imst.cmo: imst.ml
	ocamlc -c imst.ml
parser.ml: parser.mli
	ocamlc -c parser.mli # compile parser types

#ml
scanner.cmo: scanner.ml 
	ocamlc -c scanner.ml # compile the scanner
parser.cmo: parser.ml 
	ocamlc -c parser.ml # compile the parser
#verishort.cmo: verishort.ml
#	ocamlc -c verishort.ml # compile the interpreter
printer.cmo: printer.ml
	ocamlc -c printer.ml # compile Tim's printer
#compile.cmo: compile.ml
#	ocamlc -c compile.ml # compile translator
asttoimst.cmo: asttoimst.ml
	ocamlc -c asttoimst.ml
imsttocode.cmo: imsttocode.ml
	ocamlc -custom -c str.cma imsttocode.ml
	
#all
#vsc: parser.cmo scanner.cmo verishort.cmo 
#	ocamlc -o vsc parser.cmo scanner.cmo verishort.cmo

#imsttocode: ast.cmo imst.cmo parser.cmo scanner.cmo asttoimst.cmo imsttocode.cmo 
#	ocamlc -o printer ast.cmo imst.cmo parser.cmo scanner.cmo asttoimst.cmo imsttocode.cmo 
#all: imsttocode


vsc: ast.cmo imst.cmo parser.cmo scanner.cmo asttoimst.cmo imsttocode.cmo 
	ocamlc -o vsc str.cma ast.cmo imst.cmo parser.cmo scanner.cmo asttoimst.cmo imsttocode.cmo 
all: vsc

helloworld: vsc examples/helloworld.vs examples/hellostim.v
	./vsc examples/helloworld.vs > examples/helloworld.v
	iverilog examples/helloworld.v examples/hellostim.v -o examples/helloworld.out
	./examples/helloworld.out

gcd: vsc examples/gcd.v examples/gcdstim.v
	./vsc examples/gcd.vs > examples/gcd.v
	iverilog examples/gcd.v examples/gcdstim.v -o examples/gcd.out
	./examples/gcd.out

memoryarray: vsc examples/helloworld.v examples/hellostim.v
	iverilog examples/helloworld.v examples/hellostim.v -o examples/hello.out
	./examples/hello.out



clean:
	rm -f ast.cmi ast.cmo asttoimst.cmo compile.cmi compile.cmo imst.cmo output_file parser.cmi parser.cmo parser.ml parser.mli printer printer.cmi printer.cmo scanner.cmi scanner.cmo scanner.ml *.cm* vsc examples/gcd.v examples/gcd.out examples/helloworld.v examples/helloworld.out

run: vsc
	./testbench.pl
