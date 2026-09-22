CC ?= cc
CFLAGS ?= -O2
UNAME_S := $(shell uname -s)
TARGET = ../priv/bin/zotonic-sandbox

ifeq ($(UNAME_S),Linux)
LDLIBS += -lseccomp
endif

.PHONY: all clean
all: $(TARGET)

$(TARGET): zotonic-sandbox.c Makefile
	mkdir -p ../priv/bin
	$(CC) $(CPPFLAGS) $(CFLAGS) -std=c11 -Wall -Wextra -Werror -o $@ zotonic-sandbox.c $(LDFLAGS) $(LDLIBS)

clean:
	rm -f $(TARGET)
