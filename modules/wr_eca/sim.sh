#! /bin/sh

set -e

for i in 				\
	../../../general-cores/modules/genrams/genram_pkg.vhd			\
	../../../general-cores/modules/wishbone/wishbone_pkg.vhd		\
	../../../general-cores/modules/wishbone/wbgenplus/wbgenplus_pkg.vhd	\
	eca_internals_pkg.vhd		\
	eca_auto_pkg.vhd		\
	eca_queue_auto_pkg.vhd		\
	eca_tlu_auto_pkg.vhd		\
	eca_ac_wbm_auto_pkg.vhd		\
	eca_auto.vhd			\
	eca_queue_auto.vhd		\
	eca_tlu_auto.vhd		\
	eca_ac_wbm_auto.vhd		\
	eca_pkg.vhd			\
	eca_compact.vhd			\
	eca_compact_tb.vhd		\
	eca_bitonic_swap.vhd		\
	eca_bitonic_helper.vhd		\
	eca_bitonic.vhd			\
	eca_bitonic_tb.vhd		\
	eca_sdp.vhd			\
	eca_tdp.vhd			\
	eca_fifo.vhd			\
	eca_fifo_tb.vhd			\
	eca_piso_fifo.vhd		\
	eca_piso_fifo_tb.vhd		\
	eca_rmw.vhd			\
	eca_rmw_tb.vhd			\
	eca_free.vhd			\
	eca_free_tb.vhd			\
	eca_data.vhd			\
	eca_scan.vhd			\
	eca_scan_tb.vhd			\
	eca_tag_channel.vhd		\
	eca_tag_channel_tb.vhd		\
	eca_channel.vhd			\
	eca_channel_tb.vhd		\
	eca_adder.vhd			\
	eca_offset.vhd			\
	eca_wr_time.vhd			\
	eca_walker.vhd			\
	eca_search.vhd			\
	eca_msi.vhd			\
	eca_queue.vhd			\
	eca.vhd				\
	eca_wb_event.vhd		\
	wr_eca.vhd			\
	eca_ac_wbm.vhd			\
	eca_scubus_channel.vhd		\
	eca_tlu_fsm.vhd			\
	eca_tlu.vhd			\
	eca_tb.vhd;			\
do echo $i; ghdl -a --std=93 --ieee=standard --syn-binding  $i
done

echo link
ghdl -e --std=93 --ieee=standard --syn-binding eca_tb

echo run
./eca_tb --wave=testbench.ghw

gtkwave testbench.ghw wave.gtkw
