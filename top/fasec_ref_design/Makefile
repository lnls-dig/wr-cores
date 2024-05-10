#!/usr/bin/make

PROJNAME=myproj

all: ${PROJNAME}

${PROJNAME}: system_top.tcl
	vivado -mode batch -source $^ -notrace

clean:
	rm -r ./${PROJNAME} .Xil
	rm *.log *.jou *.str
