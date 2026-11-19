.PHONY: all
all: ../lib/css/analytics.css

../lib/css/analytics.css: analytics.scss Makefile
	mkdir -p ../lib/css
	sass --no-source-map analytics.scss $@
