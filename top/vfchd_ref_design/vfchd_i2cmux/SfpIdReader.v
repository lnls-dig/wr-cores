`timescale 1ns/1ns

module SfpIdReader #(
    parameter g_SfpWbBaseAddress = 0,
	      g_WbAddrWidth = 32)
(
    input                            Clk_ik,
    input                            SfpPlugged_i,
    output reg                       SfpIdValid_o,
    output reg             [127 : 0] SfpPN_b128,
    output reg                       WbCyc_o,
    output reg                       WbStb_o,
    output reg [g_WbAddrWidth-1 : 0] WbAddr_ob,
    input                    [7 : 0] WbData_ib8,
    input                            WbAck_i);

localparam s_NoSfp    = 0,
	     s_WbStart  = 1,
	     s_WbClose  = 2,
	     s_Done     = 3;

reg [1:0] State_q    = s_NoSfp,
	     NextState_a;

reg [3:0] ByteRead_c4;

always @(posedge Clk_ik) State_q <= NextState_a;

always @*
    if (!SfpPlugged_i) NextState_a = s_NoSfp;
    else begin
	NextState_a = State_q;
	case (State_q)
	    s_NoSfp:    if (SfpPlugged_i) NextState_a = s_WbStart;
	    s_WbStart:  if (WbAck_i)      NextState_a = s_WbClose;
	    s_WbClose:  if (!WbAck_i)     NextState_a = &ByteRead_c4 ? s_Done : s_WbStart;
	    s_Done:;
	    default: NextState_a = s_NoSfp;
	endcase
    end

always @(posedge Clk_ik)
    case (State_q)
	s_NoSfp: begin
	    SfpIdValid_o  <= 1'b0;
	    ByteRead_c4 <= 4'd0;
	    WbCyc_o     <= 1'b0;
	    WbStb_o     <= 1'b0;
	    WbAddr_ob   <= g_SfpWbBaseAddress + 40;
	    SfpPN_b128  <= 128'd0;
	end
	s_WbStart: begin
	    WbCyc_o     <= 1'b1;
	    WbStb_o     <= 1'b1;
	    if (NextState_a==s_WbClose) SfpPN_b128 <= {SfpPN_b128[119:0], WbData_ib8};
	end
	s_WbClose: begin
	    WbCyc_o     <= 1'b1;
	    WbStb_o     <= 1'b0;
	    if (NextState_a==s_WbStart) begin
		WbAddr_ob   <= WbAddr_ob   + 1'b1;
		ByteRead_c4 <= ByteRead_c4 + 1'b1;
	    end
	end
	s_Done: begin
	    SfpIdValid_o  <= 1'b1;
	    WbCyc_o     <= 1'b0;
	    WbStb_o     <= 1'b0;
	end
    endcase

endmodule
