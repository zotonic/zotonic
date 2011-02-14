SUBDIRS = c_src src ebin

all clean:
	@target=$@ \
	list='$(SUBDIRS)'; for subdir in $$list; do \
		echo "Making $$target in $$subdir"; \
		$(MAKE) -C $$subdir -f Makefile.unix $$target; \
	done;
