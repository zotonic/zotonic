.PHONY: all clean

SASS ?= sass

all: ../lib/css/admin-keyword.css

../lib/css/admin-keyword.css: admin-keyword.scss
	@mkdir -p ../lib/css
	$(SASS) --no-source-map --style=expanded $< $@

clean:
	rm -f ../lib/css/admin-keyword.css
