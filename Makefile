PLANTUML=deps/plantuml.jar

all: graphics/subscription.uml.png

$(PLANTUML):
	mkdir -p deps
	wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -O $@

graphics/subscription.uml.png: graphics/subscription.uml.txt graphics/styles.iuml
	java -jar deps/plantuml.jar -tpng $<

indent:
	stack install hindent
	find src test -name '*.hs' -exec stack exec hindent -- {} \;
