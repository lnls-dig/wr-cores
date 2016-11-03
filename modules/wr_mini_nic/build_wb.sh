#!/bin/bash

mkdir -p doc
wbgen2 -D ./doc/minic.html -C minic_regs.h -V minic_wb_slave.vhd -p minic_wbgen2_pkg.vhd --cstyle defines --lang vhdl  -H record -K ../../sim/minic_regs.vh mini_nic.wb
