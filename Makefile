COMPILER = ghc -Wall

PROGNAME = Today
LOWER_PROGNAME = $(shell echo $(PROGNAME) | tr A-Z a-z)
TEST = Test.hs
MAIN = Main

all: test target clean

test: $(PROGNAME).hs $(MAIN).hs $(Test)
	runhaskell $(TEST)

target: $(PROGNAME).hs $(MAIN).hs  
	$(COMPILER) -o $(LOWER_PROGNAME) $(MAIN).hs 

clean: 
	rm $(PROGNAME).hi $(PROGNAME).o
	rm $(MAIN).hi $(MAIN).o
