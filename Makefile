GNUMAKE?= gmake

all:
	${GNUMAKE} $@

.DEFAULT:
	${GNUMAKE} $@

.PHONY: all
