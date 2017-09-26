SRC = $(wildcard *.ecl)
TAR = $(SRC:.ecl=.eco)

.PHONY: all clean

all: $(TAR)

%.eco:	%.ecl
	eclipse -e "compile(\"$<\",[output:eco,debug:off])"
clean:
	rm -f $(TAR)
