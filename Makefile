.PHONY: test clean

test:
	@$(MAKE) test-type-inference

test-type-inference:
	@$(MAKE) -C test $@

clean:
	@$(MAKE) -C test $@
	@$(MAKE) -C interpreter $@
	@$(MAKE) -C type-inference $@
