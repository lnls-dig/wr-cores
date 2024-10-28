#!/bin/bash

cheby -i irig_slave_regs.cheby --gen-hdl irig_slave_regs.vhd
cheby -i irig_slave_regs.cheby --gen-c irig_slave_regs.h