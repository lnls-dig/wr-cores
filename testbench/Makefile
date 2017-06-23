# This Makefile can be called by the Continuous Integration (CI) tool to execute all
# testbenches added for CI

TB_DIRS=wr_minic
TB_DIRS+=wrc_core
TB_DIRS+=wr_streamers/streamers_multi_test


test_results_xml=test_results.xml
.PHONY: $(TB_DIRS)

all: $(TB_DIRS) summary summary_total summary_xml

$(TB_DIRS):
	@echo $@
	@echo "Run HDL-MAKE"
	cd "$@"; \
	source $(ISE_PATH)/settings64.sh; \
	export XILINX=$(ISE_PATH); \
	$(HDLMAKE_PATH)/hdl-make 2>&1

	@echo "Run make"
	$(MAKE) -C $@ $(TARGET)

	@echo "Run vsim"
	cd "$@" ;\
	vsim -c -do "run_ci.do" ;\
	echo "vsim returned $$?"

summary: $(TB_DIRS)
	@echo "-------------------------------------------------------------------"
	@echo "Summary:"
	@for d in $(TB_DIRS); do \
		if [ -f $$d/transcript ]; then \
			echo "Warnings for $$d:"; \
			cat $$d/transcript | grep Warning; \
			if [ $$? -eq 1 ]; then echo "None"; fi ;\
			echo "Errors for $$d:"; \
			cat $$d/transcript | grep Error; \
			if [ $$? -eq 1 ]; then echo "None"; fi ;\
		else \
			echo "No transcript file for $$d"; \
		fi \
	done

# Run tasks all before summary_total, because if there is a failure summary_total breaks the make execution
summary_total: summary summary_xml
	@echo "-------------------------------------------------------------------"
	@echo ""
	@echo "Summary total:"
	@echo "+-------------------------------+----------+----------+"
	@echo "| Test bench                    | Warnings |  Errors  |"
	@echo "+-------------------------------+----------+----------+"
	@is_error=0;\
	for d in $(TB_DIRS); do \
		if [ -f $$d/transcript ]; then \
			printf "| %-30s" $$d; \
			printf "| %8d " `cat $$d/transcript | grep Warning | wc -l`; \
			error_n=`cat $$d/transcript | grep Error | wc -l`; \
			printf "| %8d |\n" $$error_n;\
			if [ $$error_n -gt 0 ]; then is_error=1; fi ;\
		else \
			printf "| %-30s" $$d; \
			echo "| No transcript file! |"; is_error=1; \
		fi \
	done ;\
	echo "+-------------------------------+----------+----------+";\
	if [ $$is_error -gt 0 ]; then exit 1; fi ;

summary_xml: summary
	@echo '<?xml version="1.0" encoding="UTF-8"?>' > $(test_results_xml)
	@echo '<testsuites tests="0" failures="0" disabled="0" errors="0" time="0" name="AllTests">' >> $(test_results_xml)
	@for d in $(TB_DIRS); do \
		is_test_error=0;\
		echo -n "  <testsuite name=\""$$d"\" tests=\"1\" failures=\"" >> $(test_results_xml) ;\
		if [ -f $$d/transcript ]; then \
			error_n=`cat $$d/transcript | grep Error | wc -l`; \
			if [ $$error_n -gt 0 ]; then is_test_error=1; fi ;\
			echo -n $$is_test_error >> $(test_results_xml);\
		else \
			is_test_error=2; \
			echo -n "1" >> $(test_results_xml); \
		fi; \
		echo "\" disabled=\"0\" errors=\"0\" time=\"0\">" >> $(test_results_xml) ;\
		echo "    <testcase name=\""$$d"\" status=\"run\" time=\"0\" classname=\""Testbench"\">" >> $(test_results_xml) ;\
		if [ $$is_test_error -eq 1 ]; then \
			echo "      <failure message=\"Errors\" type=\"\"><![CDATA[" >> $(test_results_xml) ;\
			cat $$d/transcript | grep Error >> $(test_results_xml);\
			echo "      ]]></failure>" >> $(test_results_xml) ;\
		fi ;\
		if [ $$is_test_error -eq 2 ]; then \
			echo "      <failure message=\"Output file not found\" type=\"\">" >> $(test_results_xml) ;\
			echo "<![CDATA[Output file not found. Testbench didnt run.]]>" >> $(test_results_xml) ;\
			echo "      </failure>" >> $(test_results_xml) ;\
		fi ;\
		echo "    </testcase>" >> $(test_results_xml) ;\
		echo "  </testsuite>" >> $(test_results_xml) ;\
	done ;\
	echo "</testsuites>" >> $(test_results_xml)

clean:
	@for d in $(TB_DIRS); do \
		if [ -f $$d/Makefile ]; then \
			$(MAKE) -C $$d $@; \
			rm -f $$d/Makefile; \
		fi \
	done

