COMPILER = ghc -Wall

PROGNAME = today

all: target clean

target: $(PROGNAME).hs   
	$(COMPILER) $(PROGNAME).hs 

clean: 
	rm $(PROGNAME).hi $(PROGNAME).o

	
