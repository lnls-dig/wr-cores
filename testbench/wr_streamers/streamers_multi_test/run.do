# make -f Makefile > /dev/null 2>&1 
vsim -L unisim work.main -voptargs="+acc" -suppress 8684,8683 -sv_seed random 


set NumericStdNoWarnings 1
set StdArithNoWarnings 1

do wave.do
run 10us
wave zoomfull
radix -dec

