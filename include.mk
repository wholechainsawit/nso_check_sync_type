DIR ?= .

MAJOR_NCSVER = $(shell ncs --version | sed 's/^\([0-9]\+\.[0-9]\+\).*$$/\1/')
export MAJOR_NCSVER

RELEASE_PATHS = priv/
NED =

include $(DIR)/package.mk

CHECK_SYNC_PACKAGE_NAME = ncs-$(NCS_VERSION_CLEAN)-tailf-hcc-$(PACKAGE_VERSION)
export CHECK_SYNC_PACKAGE_NAME

CHECK_SYNC_PACKAGE_PATH=$(PACKAGE_DIR)/test/internal/lux/common/packages/$(CHECK_SYNC_PACKAGE_NAME).tar.gz

test-lux: test-setup-ncs $(CHECK_SYNC_PACKAGE_PATH) images

test-setup-ncs:


ifneq ($(RELEASE_FILENAME),$(CHECK_SYNC_PACKAGE_NAME).tar.gz)
$(PACKAGE_DIR)/test/internal/lux/common/packages/$(CHECK_SYNC_PACKAGE_NAME).tar.gz: $(PACKAGE_DIR)/test/internal/lux/common/packages/$(RELEASE_FILENAME)
	@cp $< $@
endif
