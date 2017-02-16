# Modelsim run script for continuous integration (with return code)
# execute: vsim -c -do "run_ci.do"
vsim -L unisim -t 10fs work.main -voptargs="+acc"
set StdArithNoWarnings 1
set NumericStdNoWarnings 1
do wave.do
radix -hexadecimal
run 200ms
wave zoomfull
radix -hexadecimal
quit -code [coverage attribute -name TESTSTATUS -concise]
