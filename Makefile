# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc
 
all: clean cish

mips:
	$(COMPILER) -c word32.ml
	$(COMPILER) -c mips.ml

cish: mips
	$(COMPILER) -c cish_ast.ml
	ocamlyacc cish_parse.mly
	$(COMPILER) -c cish_parse.mli
	$(COMPILER) -c cish_parse.ml
	ocamllex cish_lex.mll
	$(COMPILER) -c cish_lex.ml
	$(COMPILER) -c cish_eval.ml
	$(COMPILER) -c cish_compile.ml
	$(COMPILER) -c cish.ml
	$(COMPILER) -o cpp_compiler cish_ast.cmo cish_parse.cmo cish_lex.cmo cish_eval.cmo word32.cmo mips.cmo cish_compile.cmo cish.cmo

clean:
	-rm *.cmo *.cmi cpp_compiler cish_parse.ml cish_parse.mli cish_lex.ml
