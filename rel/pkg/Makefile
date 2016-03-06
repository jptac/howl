VERSION=0.8.1
COMPONENT_INTERNAL=howl
COMPONENT=fifo-howl
DEPS="erlang" "coreutils" "sudo" "grep" "zlib" "bzip2" "gcc47-libs"

include ../../_build/prod/lib/fifo_utils/priv/pkg.mk

.PHONY: prepare

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT)
	cp -r ../../_build/prod/rel/$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)


package: prepare $(FILE).tgz

clean: clean-pkg
	-rm *.tgz

