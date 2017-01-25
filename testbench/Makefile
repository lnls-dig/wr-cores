# This Makefile can be called by the Continuous Integration (CI) tool to execute all
# testbenches added for CI

TB_DIRS=wr_minic
TB_DIRS+=wrc_core/main_tb

.PHONY: $(TB_DIRS)

all: $(TB_DIRS)

$(TB_DIRS):
	echo $@
	@echo "Run HDL-MAKE"
	cd "$@"; \
	source $(ISE_PATH)/settings64.sh; \
	export XILINX=$(ISE_PATH); \
	$(HDLMAKE_PATH)/hdl-make 2>&1

	@echo "Run make"
	$(MAKE) -C $@ $(TARGET)

	@echo "Run vsim"
	cd "$@"; \
	vsim -c -do "run_ci.do"

clean:
	for d in $(TB_DIRS); do \
		if [ -f $$d/Makefile ]; then \
			$(MAKE) -C $$d $@; \
			rm -f $$d/Makefile; \
		fi \
	done
