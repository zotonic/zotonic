OUT = ../../lib/css/admin-bootstrap5.css
OUT_MIN = ../../lib/css/admin-bootstrap5.min.css

SASS ?= sass
SASS_FLAGS ?= --no-source-map
LOAD_PATHS = --load-path=. --load-path=bootstrap/scss --load-path=../zotonic-admin/scss --load-path=zotonic

SOURCES = $(shell find . ../zotonic-admin/scss -name "*.scss")

all: $(OUT) $(OUT_MIN)

$(OUT): admin-bootstrap.scss $(SOURCES)
	$(SASS) $(SASS_FLAGS) $(LOAD_PATHS) $< $@

$(OUT_MIN): admin-bootstrap.scss $(SOURCES)
	$(SASS) $(SASS_FLAGS) --style=compressed $(LOAD_PATHS) $< $@
