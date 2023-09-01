
default: # parsers FORCE
	cabal build -j exe:futz

clean:
	@rm -rf generated
	@cabal clean

src/parse/AbsFutz.hs: src/Futz.bnf
	mkdir -p src/parse
	bnfc -o src/parse/ --haskell src/Futz.bnf


parsers: src/parse/AbsFutz.hs

FORCE: