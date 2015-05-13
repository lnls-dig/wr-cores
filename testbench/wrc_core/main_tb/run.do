vlog -sv main.sv +incdir+"." +incdir+../../../sim
make -f Makefile
vsim -L unisim -t 10fs work.main -voptargs="+acc"
set StdArithNoWarnings 1
set NumericStdNoWarnings 1
do wave.do
radix -hexadecimal
run 25ms
wave zoomfull
radix -hexadecimal
