`include "simdrv_defs.svh"
`include "eth_packet.svh"
`include "minic_regs.vh"

`define MAX_PACKET_SIZE 1536

class CSimDrv_Minic;

   const uint32_t TX_DESC_VALID        = (1<<31);
   const uint32_t TX_DESC_WITH_OOB     = (1<<30);
   const uint32_t TX_DESC_HAS_OWN_MAC  = (1<<28);

   `define RX_DESC_VALID(d) ((d) & (1<<31) ? 1 : 0)
   `define RX_DESC_ERROR(d) ((d) & (1<<30) ? 1 : 0)
   `define RX_DESC_HAS_OOB(d)  ((d) & (1<<29) ? 1 : 0)
   `define RX_DESC_SIZE(d)  (((d) & (1<<0) ? -1 : 0) + (d & 'hffe))

   `define c_WRF_DATA    0
   `define c_WRF_OOB     1
   `define c_WRF_STATUS  2
   `define c_WRF_BYTESEL 3
   
   protected CBusAccessor acc_regs, acc_pmem;
   protected uint32_t base_regs, base_pmem;
   protected int pmem_size;
   protected bit little_endian;

   protected uint32_t tx_head, tx_base, tx_avail, tx_size, tx_oob_val;
   protected uint32_t rx_head, rx_base, rx_avail, rx_size;
   

   protected EthPacket rx_queue[$];
   protected EthPacket tx_queue[$];

   const int MINIC_MTU  = 1536;
   
   function new(CBusAccessor regs_, uint32_t base_regs_);
      base_regs      = base_regs_;
      acc_regs       = regs_;
      little_endian  = 0;
   endfunction

   task minic_writel(uint32_t addr, uint32_t val);
      acc_regs.write(base_regs + addr, val, 4);
   endtask

   task minic_readl(uint32_t addr, output uint32_t val);
      uint64_t tmp;
      acc_regs.read(base_regs + addr, tmp, 4);
      val  = tmp;
   endtask

   task minic_write_txword(uint32_t word_type, uint32_t word);
     uint32_t val;
     val = word_type << `MINIC_TX_FIFO_TYPE_OFFSET;
     val = val | (word & `MINIC_TX_FIFO_DAT);
     minic_writel(`ADDR_MINIC_TX_FIFO, val);
   endtask
   
   //task new_tx_buffer();
   //   tx_head = tx_base;
   //   tx_avail = (tx_size - MINIC_MTU) >> 2;
   //   minic_writel(`ADDR_MINIC_TX_ADDR, tx_base);
   //endtask // new_tx_buffers

   //task new_rx_buffer();
   //   rx_head = rx_base;

   //   minic_writel(`ADDR_MINIC_MCR, 0);
   //   minic_writel(`ADDR_MINIC_RX_ADDR, rx_base);
   //   minic_writel(`ADDR_MINIC_RX_SIZE, rx_size >> 2);
   //   minic_writel(`ADDR_MINIC_EIC_ISR, `MINIC_EIC_ISR_RX);
   //   minic_writel(`ADDR_MINIC_MCR, `MINIC_MCR_RX_EN);
   //endtask // new_rx_buffer
   

   task init();
      uint32_t val;
      
      minic_writel(`ADDR_MINIC_EIC_IDR, `MINIC_EIC_IDR_RX);
      minic_writel(`ADDR_MINIC_EIC_ISR, `MINIC_EIC_ISR_RX);

      //tx_base     = base_pmem;
      //tx_size     = pmem_size / 2;
      //rx_base     = base_pmem + pmem_size / 2;
      //rx_size     = pmem_size / 2;
      tx_oob_val  = 12345;

      //enable RX
      minic_readl(`ADDR_MINIC_MCR, val);
      minic_writel(`ADDR_MINIC_MCR, val | `MINIC_MCR_RX_EN);

      minic_writel(`ADDR_MINIC_EIC_IER, `MINIC_EIC_IER_RX);
   endtask // init

   task tx_frame(byte frame[], uint32_t size, bit with_oob, int frame_id, output uint32_t ts, output int port_id);
      int i;
      uint32_t d_hdr, mcr, nwords;
      uint32_t size_words;
      uint16_t word;
      u64_array_t buff;
      int bytesel = 0;

      byte tmp[];
      byte oob[2];
      
      //new_tx_buffer();
      $display("Minic TX frame(%d): size=%d, with_oob=%d", frame_id, size, with_oob);
      
      if(size < 60) size  = 60;
      if(size & 1) begin
        size  = size + 1;
        bytesel = 1;
      end

      //tmp = new[size](payload);
    
      //buff = SimUtils.pack({0,0,0,0, tmp, 0,0,0,0}, 4, 1);
      size_words = size / 2;

      //first we write status word (empty status)
      minic_write_txword(`c_WRF_STATUS, 0);
      
      //then we write the actual frame
      for(i=0;i<size_words-1; i++) begin
        word = (frame[2*i] << 8) | (frame[2*i+1] & 'h00FF);
        minic_write_txword(`c_WRF_DATA, word);
      end
      //write the last word with bytesel or normal
      if(bytesel == 1)
        minic_write_txword(`c_WRF_BYTESEL, frame[2*i] << 8);
      else begin
        word = (frame[2*i] << 8) | (frame[2*i+1] & 'h00FF);
        minic_write_txword(`c_WRF_DATA, word);
      end

      if (with_oob) begin
        minic_write_txword(`c_WRF_OOB, 'h1000);
        minic_write_txword(`c_WRF_OOB, frame_id);
      end
      
      minic_readl(`ADDR_MINIC_MCR, mcr);
      minic_writel(`ADDR_MINIC_MCR, mcr | `MINIC_MCR_TX_START);
   endtask // tx_frame


   task rx_frame(ref byte payload[], output uint32_t size, output bit with_ts, output uint32_t ts);
      uint32_t payload_size, num_words;
      uint64_t desc_hdr;
      uint32_t raw_ts;
      uint32_t rx_addr_cur, mcr, cur_avail;
      u64_array_t pbuff;
      uint32_t val;
      uint32_t typ, data;
      byte tmp_payload[2000];
      
      int i;
      int n_recvd;
      int done;
      uint32_t isr;
      
      //minic_readl(`ADDR_MINIC_EIC_ISR, isr);
      //
      //if(! (isr & `MINIC_EIC_ISR_RX))
      //  return;

      // if RX FIFO is empty, quit immediatelly
      minic_readl(`ADDR_MINIC_MCR, val);
      if ( val & `MINIC_MCR_RX_EMPTY )
        return ;

      size = 0;
      done = 0;
      // get frame from FIFO
      do begin
        minic_readl(`ADDR_MINIC_RX_FIFO, val);
        typ  = (val & `MINIC_RX_FIFO_TYPE) >> `MINIC_RX_FIFO_TYPE_OFFSET;
        data = (val & `MINIC_RX_FIFO_DAT);

        if (typ == `c_WRF_DATA) begin
          tmp_payload[size]   = data >> 8;
          tmp_payload[size+1] = data & 'h00FF;
          size = size + 2;
        end
        else if (typ == `c_WRF_BYTESEL) begin
          tmp_payload[size]   = data >> 8;
          size = size + 1;
        end
        else if (typ == `c_WRF_STATUS && size != 0) begin
          // which means we got status for next frame
          done = 1;
        end

      end while ( !(val & `MINIC_RX_FIFO_EMPTY) && typ != `c_WRF_OOB && done == 0);

      payload = new[size](tmp_payload);

      if (typ != `c_WRF_OOB)
        $display("!! Incomplete frame");

      // now let's see if the frame has OOB
      while (typ == `c_WRF_OOB && !(val & `MINIC_RX_FIFO_EMPTY)) begin
        minic_readl(`ADDR_MINIC_RX_FIFO, val);
        typ  = (val & `MINIC_RX_FIFO_TYPE) >> `MINIC_RX_FIFO_TYPE_OFFSET;
        data = (val & `MINIC_RX_FIFO_DAT);
      end

   endtask // rx_frame
   
   task do_rx();
      byte payload[];
      uint32_t size, psize, ts;
      bit with_ts;

      rx_frame(payload, size, with_ts, ts);

      if(payload.size() > 0)
        begin
           EthPacket pkt;
           int id;
           psize = 0;
           if (payload.size() > 14) begin
             psize = payload.size() - 14; //14 for the header
           end
           pkt  = new;
           pkt.deserialize(payload);
           id = ((pkt.payload[1] << 8) & 'hff00) | pkt.payload[0];
           $display("Minic RX frame(%d): size %d (%d)", id, psize, payload.size());
           rx_queue.push_back(pkt);
        end
      
   endtask // do_rx
   
     
   
   task run();
      uint32_t mcr;
      
      if(tx_queue.size() > 0)
        begin
           minic_readl(`ADDR_MINIC_MCR, mcr);
        //   $display("mcr %x, Minic::q_not_empty %d", mcr, tx_queue.size());
          
           if(mcr & `MINIC_MCR_TX_IDLE) begin
              byte b[];
              uint32_t ts;
              int pid;
              EthPacket pkt;

              pkt  = tx_queue.pop_front();
              pkt.serialize(b);
              tx_frame(b, b.size(), pkt.oob_type == TX_FID ? 1 : 0, pkt.ts.frame_id,ts, pid);
              
           end
        end // if (tx_queue.size() > 0)

      do_rx();
      
      
   endtask // run

   task send(EthPacket pkt);
      tx_queue.push_back(pkt);
   endtask // send

   function poll();
      return rx_queue.size() > 0;
   endfunction // poll

   task recv(ref EthPacket pkt);
      pkt  = rx_queue.pop_front();
   endtask // recv
   
   
   
   
endclass // CSimDrv_Minic
