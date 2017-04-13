# make -f Makefile > /dev/null 2>&1
vsim -L unisim -L secureip work.main -voptargs="+acc" -suppress 8684,8683
set NumericStdNoWarnings 1
set StdArithNoWarnings 1

do wave.do
run 40000us
wave zoomfull
radix -hex
