include include.mk

images:
	$(MAKE) -C test/internal/lux/realhw $@

clean::
	@rm -f $(PACKAGE_DIR)/test/internal/lux/common/packages/$(RELEASE_FILENAME)
	@rm -f $(HCC_PACKAGE_PATH)
