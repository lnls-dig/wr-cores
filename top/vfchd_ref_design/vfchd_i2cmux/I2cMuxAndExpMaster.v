//============================================================================================\\
//##################################   Module Information   ##################################\\
//============================================================================================\\
//                                                                                         
// Company: CERN (BE-BI) 
//                                                        
// File Name: I2cExpAndMuxMaster.v  
//
// File versions history:
//
//       DATE          VERSION      AUTHOR             DESCRIPTION
//     - <date>        <version>    Andrea Boccardi    <description>               
//
// Language: Verilog 2005                                                              
//
// Description:
// 
//      The requests are level sensitive and accepted when the module is not busy.
//                                                                                                   
//============================================================================================\\
//############################################################################################\\
//============================================================================================\\

`timescale 1ns/100ps 

module I2cExpAndMuxMaster 
//====================================  Global Parameters  ===================================\\   
#(  parameter g_SclHalfPeriod = 10'd256)      
//========================================  I/O ports  =======================================\\    

(   
    //==== Clocks & Resets ====\\ 
    input             Clk_ik,
    input             Rst_irq,
    //==== Access requests ====\\
    // IO Expanders parameters:
    input             IoExpWrReq_i,    
    output reg        IoExpWrOn_oq,
    input             IoExpRdReq_i,     
    output reg        IoExpRdOn_oq,    
    input       [2:0] IoExpAddr_ib3,
    input       [1:0] IoExpRegAddr_ib2, 
    input       [7:0] IoExpData_ib8,  
    // I2C Mux parameters:
    input             I2cSlaveWrReq_i,
    output reg        I2cSlaveWrOn_o,
    input             I2cSlaveRdReq_i,
    output reg        I2cSlaveRdOn_o,    
    input             I2cMuxAddress_i,
    input       [1:0] I2cMuxChannel_ib2, 
    input       [6:0] I2cSlaveAddr_ib7,
    input       [7:0] I2cSlaveRegAddr_ib8,
    input       [7:0] I2cSlaveByte_ib8,   
    // Status and results: 
    output reg        Busy_o,
    output reg        NewByteRead_op,
    output reg  [7:0] ByteOut_ob8,
    output reg        AckError_op,    
    //==== I2C Bus ====\\ 
    inout             Scl_ioz,
    inout             Sda_ioz    
);     
   
//=======================================  Declarations  =====================================\\    
//==== Local parameters ====\\ 
// FSM:
localparam  s_Idle              = 4'h0,
            s_FetchCommand      = 4'h1,
            s_StartExecution    = 4'h2,
            s_StartSda1         = 4'h3,
            s_StartScl1         = 4'h4,
            s_StartSda0         = 4'h5,
            s_StartScl0         = 4'h6,
            s_SendScl0          = 4'h7,
            s_SendScl1          = 4'h8,
            s_GetScl0           = 4'h9,
            s_GetScl1           = 4'ha,
            s_StopSda0          = 4'hb,
            s_StopScl1          = 4'hc,
            s_StopSda1          = 4'hd;
// Command sequence ROM:
localparam  c_IoExpWriteSeq     = 3'd0,
            c_IoExpReadSeq      = 3'd1,
            c_I2cSlaveRead      = 3'd2,  
            c_I2cSlaveWrite     = 3'd3;
// Commands:
localparam  c_SendStartBit  = 3'd1, 
            c_SendStopBit   = 3'd2,
            c_SendByte      = 3'd3,
            c_GetByte       = 3'd4,
            c_GoToIdle      = 3'b0;           

//==== Wires & Regs ====\\ 
// FSM
reg [3:0] State_qb4, NextState_ab4;    
// Support Logic
reg [2:0] Command_b3;
reg [9:0] SclCounter_c10; 
reg [3:0] BitCounter_c4;
reg       SclOe_e, SdaOe_e;        
reg       I2CAck;  
reg [7:0] ShReg_b8;   
reg [2:0] ActiveSequence_b3;  
// Command Sequences  
reg  [3:0] CommandPointer_c4;          
reg [10:0] IoExpWriteSeq_b11, IoExpReadSeq_b11;
reg  [2:0] IoExpAddr_qb3;
reg  [1:0] IoExpRegAddr_qb2;
reg  [7:0] IoExpData_qb8; 
reg [10:0] I2cMuxRdSlaveRegSeq_b11, I2cMuxWrSlaveRegSeq_b11;
reg        I2cMuxAddress_q;
reg  [1:0] I2cMuxChannel_qb2; 
reg  [6:0] I2cSlaveAddr_qb7;
reg  [7:0] I2cSlaveRegAddr_qb8;
reg  [7:0] I2cSlaveByte_qb8;   

//=======================================  User Logic  =======================================\\    

//==== I2C command's sequences ====\\ 
// Writing a value in a register of one IO expander  
always @(posedge Clk_ik) case(CommandPointer_c4) 
    4'd0: IoExpWriteSeq_b11    <= #1 {c_SendStartBit, 8'h0};
    4'd1: IoExpWriteSeq_b11    <= #1 {c_SendByte, 4'b0100, IoExpAddr_qb3, 1'b0}; //Comment: Selection of the Slave in write mode
    4'd2: IoExpWriteSeq_b11    <= #1 {c_SendByte, 6'b0, IoExpRegAddr_qb2};       //Comment: Writing the Register address
    4'd3: IoExpWriteSeq_b11    <= #1 {c_SendByte, IoExpData_qb8};                //Comment: Writing the Register Value
    4'd4: IoExpWriteSeq_b11    <= #1 {c_SendStopBit, 8'h0};
    default: IoExpWriteSeq_b11 <= #1 {c_GoToIdle, 8'h0};
endcase
    
// Reading a value from a register of one IO expander   
always @(posedge Clk_ik) case(CommandPointer_c4) 
    4'd0: IoExpReadSeq_b11    <= #1 {c_SendStartBit, 8'h0};
    4'd1: IoExpReadSeq_b11    <= #1 {c_SendByte, 4'b0100, IoExpAddr_qb3, 1'b0}; //Comment: Selection of the Slave in write mode
    4'd2: IoExpReadSeq_b11    <= #1 {c_SendByte, 6'b0, IoExpRegAddr_qb2};       //Comment: Writing the Register address
    4'd3: IoExpReadSeq_b11    <= #1 {c_SendStartBit, 8'h0};                     //Comment: Repeated start (new cycle)
    4'd4: IoExpReadSeq_b11    <= #1 {c_SendByte, 4'b0100, IoExpAddr_qb3, 1'b1}; //Comment: Selection of the Slave in read mode
    4'd5: IoExpReadSeq_b11    <= #1 {c_GetByte, 8'h1};                           //Comment: Read of the last Byte (single read)
    4'd6: IoExpReadSeq_b11    <= #1 {c_SendStopBit, 8'h0};
    default: IoExpReadSeq_b11 <= #1 {c_GoToIdle, 8'h0};  
endcase

// Reading a register from a device attached to a I2C mux
always @(posedge Clk_ik) case(CommandPointer_c4) 
    4'h0: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendStartBit, 8'h0};
    4'h1: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendByte, 6'h38, I2cMuxAddress_q, 1'b0};  //Comment: Selection of the I2C Mux in Write mode
    4'h2: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendByte, 5'b0, 1'b1, I2cMuxChannel_qb2}; //Comment: Enabling the desired channel 
    4'h3: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendStopBit, 8'h0};
    4'h4: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendStartBit, 8'h0};
    4'h5: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendByte, I2cSlaveAddr_qb7, 1'b0};         //Comment: Selection of the Slave in write mode
    4'h6: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendByte, I2cSlaveRegAddr_qb8};            //Comment: Writing the Register address
    4'h7: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendStartBit, 8'h0};                      //Comment: Repeated start (new cycle)
    4'h8: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendByte, I2cSlaveAddr_qb7, 1'b1};         //Comment: Selection of the Slave in read mode
    4'h9: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_GetByte, 8'h1};                           //Comment: Read of the last Byte (single read)
    4'ha: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendStopBit, 8'h0};
    4'hb: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendStartBit, 8'h0};
    4'hc: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendByte, 6'h38, I2cMuxAddress_q, 1'b0}; //Comment: Selection of the I2C Mux  in Write mode
    4'hd: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendByte, 5'b0, 3'b0};                   //Comment: Disabling all channels 
    4'he: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_SendStopBit, 8'h0};        
    default: I2cMuxRdSlaveRegSeq_b11 <= #1 {c_GoToIdle, 8'h0};
endcase

// Writing a register from a device attached to a I2C mux
always @(posedge Clk_ik) case(CommandPointer_c4) 
    4'h0: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendStartBit, 8'h0};
    4'h1: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendByte, 6'h38, I2cMuxAddress_q, 1'b0};  //Comment: Selection of the I2C Mux in Write mode
    4'h2: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendByte, 5'b0, 1'b1, I2cMuxChannel_qb2}; //Comment: Enabling the desired channel 
    4'h3: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendStopBit, 8'h0};
    4'h4: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendStartBit, 8'h0};
    4'h5: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendByte, I2cSlaveAddr_qb7, 1'b0};         //Comment: Selection of the Slave in write mode
    4'h6: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendByte, I2cSlaveRegAddr_qb8};            //Comment: Writing the Register address
    4'h7: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendByte, I2cSlaveByte_qb8};               //Comment: Sending the byte
    4'h8: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendStopBit, 8'h0};
    4'h9: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendStartBit, 8'h0};
    4'ha: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendByte, 6'h38, I2cMuxAddress_q, 1'b0}; //Comment: Selection of the I2C Mux  in Write mode
    4'hb: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendByte, 5'b0, 3'b0};                   //Comment: Disabling all channels 
    4'hc: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_SendStopBit, 8'h0};        
    default: I2cMuxWrSlaveRegSeq_b11 <= #1 {c_GoToIdle, 8'h0};
endcase
 
//==== State Machine ====\\ 
always @(posedge Clk_ik) State_qb4 <= #1 Rst_irq ?  s_Idle :  NextState_ab4;

always @* begin
    NextState_ab4 =  State_qb4;
    case(State_qb4)
    // Waiting for a request
    s_Idle: if (~Busy_o&&(IoExpWrReq_i || IoExpRdReq_i || I2cSlaveWrReq_i || I2cSlaveRdReq_i)) NextState_ab4 = s_FetchCommand;
    // Fetching and decoding the command
    s_FetchCommand: NextState_ab4 =  s_StartExecution;
    s_StartExecution: if (Command_b3 == c_SendStartBit) NextState_ab4 = s_StartSda1;
                 else if (Command_b3 == c_SendByte)     NextState_ab4 = s_SendScl0;
                 else if (Command_b3 == c_GetByte)      NextState_ab4 = s_GetScl0;
                 else if (Command_b3 == c_SendStopBit)  NextState_ab4 = s_StopSda0;
                 else if (Command_b3 == c_GoToIdle)     NextState_ab4 = s_Idle; 
    // Start bit sequence            
    s_StartSda1: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_StartScl1;
    s_StartScl1: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_StartSda0;
    s_StartSda0: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_StartScl0;
    s_StartScl0: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_FetchCommand;
    // Byte sending sequence            
    s_SendScl0: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = (BitCounter_c4==4'd9) ? s_FetchCommand : s_SendScl1;
    s_SendScl1: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_SendScl0;
    // Byte getting sequence                          
    s_GetScl0 : if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = (BitCounter_c4==4'd9) ? s_FetchCommand : s_GetScl1;
    s_GetScl1 : if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_GetScl0;
    // Stop bit sequence                      
    s_StopSda0: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_StopScl1;
    s_StopScl1: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_StopSda1;
    s_StopSda1: if (SclCounter_c10 == g_SclHalfPeriod) NextState_ab4 = s_FetchCommand;
    // Protection from bad state condition            
    default: NextState_ab4 = s_Idle;
    endcase 
end 
  
always @(posedge Clk_ik) begin
    if  (Rst_irq) begin
        BitCounter_c4       <= #1  4'b0;
        SclCounter_c10      <= #1 10'b0;
        I2CAck              <= #1  1'b0;  
        ShReg_b8            <= #1  8'b0; 
        Command_b3          <= #1  3'b0;      
        CommandPointer_c4   <= #1  4'h0;
        ActiveSequence_b3   <= #1  3'b0;
        ByteOut_ob8         <= #1  8'b0;
        SclOe_e             <= #1 1'b0;
        SdaOe_e             <= #1 1'b0;   
        Busy_o              <= #1 1'b0;
        IoExpWrOn_oq        <= #1 1'b0;
        IoExpRdOn_oq        <= #1 1'b0;
        I2cSlaveWrOn_o      <= #1 1'b0;
        I2cSlaveRdOn_o      <= #1 1'b0;   
        NewByteRead_op      <= #1 1'b0;
    end else case(State_qb4)
        // Waiting for a request    
        s_Idle : begin
            SclOe_e             <= #1 1'b0;
            SdaOe_e             <= #1 1'b0;         
            CommandPointer_c4   <= #1 4'h0; 
            IoExpWrOn_oq        <= #1 1'b0;
            IoExpRdOn_oq        <= #1 1'b0;
            I2cSlaveWrOn_o      <= #1 1'b0;
            I2cSlaveRdOn_o      <= #1 1'b0; 
            Busy_o              <= #1 1'b0;
            NewByteRead_op      <= #1 1'b0;
            if (NextState_ab4 != s_Idle) begin
                AckError_op <= #1 1'b0;  
                Busy_o      <= #1 1'b1;                
                if (IoExpWrReq_i) begin    
                    ActiveSequence_b3   <= #1 c_IoExpWriteSeq;
                    IoExpWrOn_oq        <= #1 1'b1;
                    IoExpAddr_qb3       <= #1 IoExpAddr_ib3;
                    IoExpRegAddr_qb2    <= #1 IoExpRegAddr_ib2;
                    IoExpData_qb8       <= #1 IoExpData_ib8;                        
                end else if (IoExpRdReq_i) begin
                    ActiveSequence_b3   <= #1 c_IoExpReadSeq;   
                    IoExpRdOn_oq        <= #1 1'b1;
                    IoExpAddr_qb3       <= #1 IoExpAddr_ib3;
                    IoExpRegAddr_qb2    <= #1 IoExpRegAddr_ib2;
                end else if (I2cSlaveWrReq_i) begin
                    ActiveSequence_b3   <= #1 c_I2cSlaveWrite;   
                    I2cSlaveWrOn_o      <= #1 1'b1; 
                    I2cMuxAddress_q     <= #1 I2cMuxAddress_i;
                    I2cMuxChannel_qb2   <= #1 I2cMuxChannel_ib2;
                    I2cSlaveAddr_qb7    <= #1 I2cSlaveAddr_ib7;    
                    I2cSlaveRegAddr_qb8 <= #1 I2cSlaveRegAddr_ib8;  
                    I2cSlaveByte_qb8    <= #1 I2cSlaveByte_ib8;                         
                end else if (I2cSlaveRdReq_i) begin
                    ActiveSequence_b3   <= #1 c_I2cSlaveRead;   
                    I2cSlaveRdOn_o      <= #1 1'b1;
                    I2cMuxAddress_q     <= #1 I2cMuxAddress_i;
                    I2cMuxChannel_qb2   <= #1 I2cMuxChannel_ib2;
                    I2cSlaveAddr_qb7    <= #1 I2cSlaveAddr_ib7;    
                    I2cSlaveRegAddr_qb8 <= #1 I2cSlaveRegAddr_ib8;  
                end 
            end
        end
       // Fetching and decoding the command
        s_FetchCommand: begin
            NewByteRead_op    <= #1 Command_b3 == c_GetByte;
            ByteOut_ob8       <= #1 ShReg_b8;   
            AckError_op       <= #1 Command_b3 == c_SendByte &&  I2CAck;   
            CommandPointer_c4 <= #1 CommandPointer_c4 + 1'b1;
            case (ActiveSequence_b3) 
                c_IoExpWriteSeq:     {Command_b3, ShReg_b8} <= #1 IoExpWriteSeq_b11;         
                c_IoExpReadSeq:      {Command_b3, ShReg_b8} <= #1 IoExpReadSeq_b11;   
                c_I2cSlaveRead:      {Command_b3, ShReg_b8} <= #1 I2cMuxRdSlaveRegSeq_b11; 
                c_I2cSlaveWrite:     {Command_b3, ShReg_b8} <= #1 I2cMuxWrSlaveRegSeq_b11;  
                default:             {Command_b3, ShReg_b8} <= #1 {c_GoToIdle, 8'h0};  
            endcase
        end
        s_StartExecution : begin
            NewByteRead_op    <= #1 1'b0;
            BitCounter_c4     <= #1 4'h0;
            SclCounter_c10    <= #1 10'h0;
            if (NextState_ab4==s_GetScl0)   I2CAck      <= #1 ShReg_b8[0]; 
        end     
        // Start bit sequence                   
        s_StartSda1 : begin
            SdaOe_e <= #1 1'b0;
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (Sda_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;
        end             
        s_StartScl1 : begin
            SclOe_e <= #1 1'b0;
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (Scl_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;        
        end          
        s_StartSda0 : begin
            SdaOe_e <= #1 1'b1;
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (!Sda_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;        
        end             
        s_StartScl0 : begin
            SclOe_e <= #1 1'b1;
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (!Scl_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;        
        end   
        // Byte sending sequence                              
        s_SendScl0 : begin
            SclOe_e             <= #1 1'b1;
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (!Scl_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0; 
            if (SclCounter_c10[8:0]==g_SclHalfPeriod[9:1])  SdaOe_e <= #1 BitCounter_c4[3] ? 1'b0 : !ShReg_b8[7];   
        end
        s_SendScl1 : begin
            SclOe_e             <= #1 1'b0;
            if (NextState_ab4 !=  State_qb4) begin
                BitCounter_c4       <= #1 BitCounter_c4 + 1'b1;
                SclCounter_c10    <= #1 'h0;
                ShReg_b8[7:1]       <= #1 ShReg_b8[6:0];
                I2CAck              <= #1 Sda_ioz; 
            end else if (Scl_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;                          
        end             
       // Byte getting sequence                               
        s_GetScl0 : begin
            SclOe_e             <= #1 1'b1;
            SdaOe_e             <= #1 (BitCounter_c4==4'd8) ? !I2CAck : 1'b0;        
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (!Scl_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;         
        end                              
        s_GetScl1 : begin
            SclOe_e             <= #1 1'b0;
            if (NextState_ab4 !=  State_qb4) begin
                BitCounter_c4       <= #1 BitCounter_c4 + 1'b1;
                SclCounter_c10    <= #1 'h0;
                ShReg_b8            <= #1 (BitCounter_c4==4'd8) ? ShReg_b8 : {ShReg_b8[6:0], Sda_ioz};         
            end else if (Scl_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;               
        end  
        // Stop bit sequence                                       
        s_StopSda0 : begin
            SdaOe_e <= #1 1'b1;
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (!Sda_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;  
        end              
        s_StopScl1 : begin
            SclOe_e <= #1 1'b0;
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (Scl_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;        
        end              
        s_StopSda1 : begin
            SdaOe_e <= #1 1'b0;
            if (NextState_ab4 !=  State_qb4) SclCounter_c10    <= #1 'h0;
            else if (Sda_ioz) SclCounter_c10 <= #1 SclCounter_c10 + 1'b1;
            else SclCounter_c10    <= #1 'h0;        
        end  
        // Protection from bad state condition               
        default: begin
            SclOe_e             <= #1 1'b0;
            SdaOe_e             <= #1 1'b0;         
        end
    endcase
 end
    
assign Scl_ioz = SclOe_e ? 1'b0 : 1'bz;
assign Sda_ioz = SdaOe_e ? 1'b0 : 1'bz;
 

endmodule