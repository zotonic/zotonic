CC ?= cc
CFLAGS ?= -O2

UNAME_S := $(shell uname -s)
TARGET := ../priv/bin/zotonic-sandbox

BUILD_SANDBOX := 0
SKIP_REASON :=

define check_header
$(shell echo '#include <$(1)>' | $(CC) $(CPPFLAGS) $(CFLAGS) -E - >/dev/null 2>&1 && echo 1 || echo 0)
endef

ifeq ($(UNAME_S),Darwin)

BUILD_SANDBOX := 1

else ifeq ($(UNAME_S),Linux)

# Check if sandbox header files are available.
ifeq ($(call check_header,linux/landlock.h),0)
SKIP_REASON := linux/landlock.h not found (install kernel headers)
else ifeq ($(call check_header,seccomp.h),0)
SKIP_REASON := seccomp.h not found (install libseccomp-dev)
else
BUILD_SANDBOX := 1
LDLIBS += -lseccomp
endif

else

SKIP_REASON := unsupported platform $(UNAME_S)

endif

ifeq ($(BUILD_SANDBOX),1)
TARGETS := $(TARGET)
endif

.PHONY: all clean

all: $(TARGETS)
ifeq ($(BUILD_SANDBOX),0)
	@echo "WARNING: zotonic-sandbox will not be built: $(SKIP_REASON)"
endif

$(TARGET): zotonic-sandbox.c Makefile
	mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) \
		-std=c11 -Wall -Wextra -Werror \
		-o $@ zotonic-sandbox.c \
		$(LDFLAGS) $(LDLIBS)

clean:
	rm -f $(TARGET)
