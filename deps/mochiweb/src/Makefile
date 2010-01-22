include ../support/include.mk

all: $(EBIN_FILES_NO_DOCS)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

test: all
	$(ERL) -noshell -pa ../ebin -s mochiweb test -s init stop
