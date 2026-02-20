OUT = ../../lib/css/admin-bootstrap5.css
OUT_MIN = ../../lib/css/admin-bootstrap5.min.css

SASS ?= sass
SASS_FLAGS ?= --no-source-map
LOAD_PATHS = --load-path=. --load-path=bootstrap/scss --load-path=../zotonic-admin/scss --load-path=zotonic

all: $(OUT) $(OUT_MIN)

$(OUT): admin-bootstrap.scss bootstrap/scss/bootstrap.scss compat/_bootstrap3-compat.scss zotonic/_forms.scss ../zotonic-admin/scss/zotonic-admin.scss
	$(SASS) $(SASS_FLAGS) $(LOAD_PATHS) $< $@

$(OUT_MIN): admin-bootstrap.scss bootstrap/scss/bootstrap.scss compat/_bootstrap3-compat.scss zotonic/_forms.scss ../zotonic-admin/scss/zotonic-admin.scss
	$(SASS) $(SASS_FLAGS) --style=compressed $(LOAD_PATHS) $< $@
