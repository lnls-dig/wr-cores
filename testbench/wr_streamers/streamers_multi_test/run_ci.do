# Modelsim run script for continuous integration (with return code)
# execute: vsim -c -do "run_ci.do"
vsim -L unisim work.main -voptargs="+acc" -suppress 8684,8683  -sv_seed random 
set StdArithNoWarnings 1
set NumericStdNoWarnings 1
do wave.do
run 100ms
wave zoomfull
radix -hex
coverage save coverage.ucdb
quit -code [coverage attribute -name TESTSTATUS -concise]

