.PHONY: all css js test

SASS ?= sass
STYLES := editor-tiptap editor-tiptap-body
CSS := $(addprefix ../lib/css/,$(addsuffix .css,$(STYLES)))

# Keep stylesheet builds independent of the JavaScript package.
all: css
css: $(CSS)

../lib/css/%.css: scss/%.scss $(wildcard scss/_*.scss)
	@mkdir -p ../lib/css
	$(SASS) --no-source-map --style=expanded $< $@

js:
	$(MAKE) -C tiptap

test:
	$(MAKE) -C tiptap test
