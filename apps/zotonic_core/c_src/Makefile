CC ?= cc
CFLAGS ?= -O2
UNAME_S := $(shell uname -s)
TARGET = ../priv/bin/zotonic-sandbox

ifeq ($(UNAME_S),Linux)
LDLIBS += -lseccomp
HAS_LANDLOCK := $(shell \
	printf '%s\n' \
	    '#include <linux/landlock.h>' \
	    'int main(void) { return 0; }' \
	| $(CC) $(CPPFLAGS) -x c - -c -o /dev/null 2>/dev/null \
	&& echo 1 || echo 0)
ifeq ($(HAS_LANDLOCK),1)
CPPFLAGS += -DHAS_LANDLOCK=1
endif
endif

.PHONY: all clean
all: $(TARGET)

$(TARGET): zotonic-sandbox.c Makefile
	mkdir -p ../priv/bin
	$(CC) $(CPPFLAGS) $(CFLAGS) -std=c11 -Wall -Wextra -Werror -o $@ zotonic-sandbox.c $(LDFLAGS) $(LDLIBS)

clean:
	rm -f $(TARGET)
