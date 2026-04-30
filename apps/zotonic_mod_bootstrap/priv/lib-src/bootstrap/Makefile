BOOTSTRAP_SCSS_DIR = scss
OUT_DIR = ../../lib/bootstrap/css

SASS ?= sass
SASS_FLAGS ?= --no-source-map

all: $(OUT_DIR)/bootstrap.css $(OUT_DIR)/bootstrap.min.css $(OUT_DIR)/bootstrap-theme.css $(OUT_DIR)/bootstrap-theme.min.css

$(OUT_DIR)/bootstrap.css: css/bootstrap.scss $(BOOTSTRAP_SCSS_DIR)/bootstrap.scss compat/_bootstrap3-compat.scss
	mkdir -p $(OUT_DIR)
	$(SASS) $(SASS_FLAGS) $< $@

$(OUT_DIR)/bootstrap.min.css: css/bootstrap.scss $(BOOTSTRAP_SCSS_DIR)/bootstrap.scss compat/_bootstrap3-compat.scss
	mkdir -p $(OUT_DIR)
	$(SASS) $(SASS_FLAGS) --style=compressed $< $@

$(OUT_DIR)/bootstrap-theme.css: css/bootstrap-theme.scss $(BOOTSTRAP_SCSS_DIR)/bootstrap.scss compat/_bootstrap3-compat.scss
	mkdir -p $(OUT_DIR)
	$(SASS) $(SASS_FLAGS) $< $@

$(OUT_DIR)/bootstrap-theme.min.css: css/bootstrap-theme.scss $(BOOTSTRAP_SCSS_DIR)/bootstrap.scss compat/_bootstrap3-compat.scss
	mkdir -p $(OUT_DIR)
	$(SASS) $(SASS_FLAGS) --style=compressed $< $@
