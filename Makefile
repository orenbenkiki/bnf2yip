
all: pngs/1.png

pngs/1.png: yaml.yip graphviz.m4
	./make_pngs

yaml.yip: bnf2yip ../YamlReference/Text/Yaml/Reference.bnf
	./bnf2yip < ../YamlReference/Text/Yaml/Reference.bnf > yaml.yip

bnf2yip: CharSet.hs FixPrecedence.hs Machine.hs Main.hs ParseSyntax.hs Syntax.hs
	ghc -fglasgow-exts --make Main -o bnf2yip

clean:
	rm -rf *.o *.hi bnf2yip pngs
