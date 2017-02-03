`define WIRE_VHD_SV_WRFSRC(vhd_dst, sv_src) \
assign vhd_dst``_i.cyc = sv_src.cyc; \
assign vhd_dst``_i.stb = sv_src.stb; \
assign vhd_dst``_i.adr = sv_src.adr; \
assign vhd_dst``_i.dat = sv_src.dat_o; \
assign vhd_dst``_i.sel = sv_src.sel; \
assign vhd_dst``_i.we  = sv_src.we; \
assign sv_src.ack = vhd_dst``_o.ack; \
assign sv_src.err = vhd_dst``_o.err; \
assign sv_src.stall = vhd_dst``_o.stall;

`define WIRE_VHD_SV_WRFSNK(sv_dst, vhd_src) \
assign sv_dst.cyc = vhd_src``_o.cyc; \
assign sv_dst.stb = vhd_src``_o.stb; \
assign sv_dst.adr = vhd_src``_o.adr; \
assign sv_dst.dat_o = vhd_src``_o.dat; \
assign sv_dst.sel = vhd_src``_o.sel; \
assign sv_dst.we  = vhd_src``_o.we; \
assign vhd_src``_i.ack = sv_dst.ack; \
assign vhd_src``_i.err = sv_dst.err; \
assign vhd_src``_i.stall = sv_dst.stall;

`define WIRE_VHD_SV_WBM(vhd_dst, sv_src) \
assign vhd_dst``_o.cyc = sv_src.cyc; \
assign vhd_dst``_o.stb = sv_src.stb; \
assign vhd_dst``_o.adr = sv_src.adr; \
assign vhd_dst``_o.dat = sv_src.dat_o; \
assign vhd_dst``_o.sel = sv_src.sel; \
assign vhd_dst``_o.we  = sv_src.we; \
assign sv_src.ack = vhd_dst``_i.ack; \
assign sv_src.err = vhd_dst``_i.err; \
assign sv_src.dat_i = vhd_dst``_i.dat; \
assign sv_src.stall = vhd_dst``_i.stall;

int tx_sizes[$], tx_padded[$];

/* this semaphor is used it coordinate transmission of frames by the
 * functino send_frames() and the retransmission of LM32-generated 
 * frames in the main.sv
 */
semaphore  txPkt = new(1);
//////////////////////////////////////////////////////////
/* this function sends an n_packet number of frames with
 * - random content
 * - random size
 * - sequence ID
 * - either random or forced Iter-Frame Gap (IFG)
 */
task send_frames(WBPacketSource src, int n_packets, int ifg = 0 /*[us]*/);
// TODO: improve the IFG: allow to make it tighter
  int i, seed = 0,n1=0,n2=0;
  int cur_size, dir;
  EthPacket pkt, tmpl;
  EthPacket to_ext[$], to_minic[$];
  EthPacketGenerator gen  = new;
  int random_ifg; //us
  int min_ifg = 1; //us
  int max_ifg = 100;//us
  
  tmpl                = new;
  tmpl.src                = '{'h22,'h33,'h44,'h44,'h55,'h66};
  tmpl.dst                = '{'h77,'h88,'h99,'hAA,'hBB,'hCC};
  tmpl.has_smac           = 1;
  tmpl.is_q               = 0;
  tmpl.ethertype	=	{'h0800};

  gen.set_randomization(EthPacketGenerator::SEQ_PAYLOAD | EthPacketGenerator::SEQ_ID ) ;
  gen.set_template(tmpl);
  gen.set_size(1, 1500);

  cur_size = 60;
  dir = 1;
  for(i=0;i<n_packets;i++) begin
    /* switch between incrementing/decrementing */
    if(cur_size == 1495)
      dir = 0;
    if(cur_size == 1)
      dir = 1;
    /* increment/decrement frame size, based on dir */
    if(dir == 1)
      cur_size += 1;
    else
      cur_size -= 1;

    pkt         = gen.gen(cur_size);
    tx_sizes = {tx_sizes, pkt.size};
    tx_padded = {tx_padded, padded_size(pkt)};
    if(ifg == 0) // random
      random_ifg = $dist_uniform(seed, min_ifg, max_ifg);
    else
      random_ifg = ifg;

    $display("IFG: %4d [us] | %s", random_ifg, (ifg==0?"random":"forced"));

    txPkt.get(1);
    src.send(pkt);
    txPkt.put(1);

    #(random_ifg*1000); // this assumes the timescale in the main.sv is [ns]
  end
endtask

function int nopad_size(EthPacket pkt);
  int i;
  if(pkt.size > 64)
    nopad_size = pkt.size;
  else begin
    nopad_size = 1;
    for(i=1; i<pkt.size; i++) begin
      if(pkt.payload[i]==0) break;
      nopad_size = nopad_size + 1;
    end
    nopad_size = nopad_size + 14; //+header
  end
endfunction;

function int padded_size(EthPacket pkt);
  if(pkt.size < 60) padded_size = 60;
  else padded_size = pkt.size;
endfunction;

function int find_pkt_size(EthPacket pkt, int start, int limit);
  int i;
  for(i=start; i<start+limit; i++) begin
    if(pkt.size == tx_sizes[i])
      return i;
  end
  return -1;
endfunction;
