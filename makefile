COMPILER = ghc -Wall

MAIN = Main

all: target clean

target: $(MAIN).hs
		$(COMPILER) $(MAIN).hs

clean:
	rm ${MAIN}.o $(MAIN).hi