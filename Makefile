COMPILER = ghc -Wall

PROGNAME = Today
LOWER_PROGNAME = $(shell echo $(PROGNAME) | tr A-Z a-z)
MAIN = Main

all: target clean

target: $(PROGNAME).hs $(MAIN).hs  
	$(COMPILER) -o $(LOWER_PROGNAME) $(MAIN).hs 

clean: 
	rm $(PROGNAME).hi $(PROGNAME).o
	rm $(MAIN).hi $(MAIN).o
