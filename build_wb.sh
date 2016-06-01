#!/bin/bash

wbgen2  -D ./wb_doc.html -p wr_transmission_wbgen2_pkg.vhd -H record -V wr_transmission_wb.vhd --cstyle defines --lang vhdl -K ../../testbench/include/regs/wr_transmission_wb.svh wr_transmission_wb.wb