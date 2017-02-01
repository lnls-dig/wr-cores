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
//      
//
//      At start up and after each reset the module: 
//          - scan all the IO Exp to set them in the appropriate IO state and read the actual value
//          - for each moddef0 to ground the SFP is access to read the ID of the module
//      After start up the module goes trough all the external requests, in a round robin guaranteed
//      by a mute after each access reset once no requests remains unmuted (for the BlmIn the muting can be masked):
//          - if an IoExp has an interrupt is served with the following order
//              -- BlmIn
//              -- SfpApp12
//                  --- if one of the Sfp is inserted (ModDef going to 0) the ID is read immediately
//              -- SfpApp32
//                  --- if one of the Sfp is inserted (ModDef going to 0) the ID is read immediately
//              -- SfpBstEth
//                  --- if one of the Sfp is inserted (ModDef going to 0) the ID is read immediately        
//              -- SfpLos
//          - if one of the Sfp settings (rateselect and disable) is changed we propagate the change
//          - if one of the GpIo settings is changed we propagate the change
//          - if one of the Leds settings is changed we propagate the change
//          - if there is a request to access one I2c interface we execute it
//      BlmIn can be set to latency deterministic mode. If this is the case a BlmIn int mutes all the requests
//      for a time equal to the longest transaction and is than read and than the mute removed (the time is
//      programmable)
//  
//
//                                                                                                   
//============================================================================================\\
//############################################################################################\\
//============================================================================================\\

`timescale 1ns/100ps 

module I2cExpAndMuxReqArbiter 
//====================================  Global Parameters  ===================================\\   
#(  parameter g_SclHalfPeriod = 10'd256)      
//========================================  I/O ports  =======================================\\    

(   
    //==== Clocks & Resets ====\\ 
    input             Clk_ik,
    input             Rst_irq,
    //==== Master interface ====\\    
    // IO Expanders parameters:
    output reg        IoExpWrReq_oq,    
    input             IoExpWrOn_i,
    output reg        IoExpRdReq_oq,     
    input             IoExpRdOn_i,    
    output reg  [2:0] IoExpAddr_oqb3,
    output reg  [1:0] IoExpRegAddr_oqb2, 
    output reg  [7:0] IoExpData_oqb8,  
    // I2c Mux parameters:
    output reg        I2cSlaveWrReq_oq,
    input             I2cSlaveWrOn_i,
    output reg        I2cSlaveRdReq_oq,
    input             I2cSlaveRdOn_i,    
    output reg        I2cMuxAddress_oq,
    output reg  [1:0] I2cMuxChannel_oqb2, 
    output reg  [6:0] I2cSlaveAddr_oqb7,
    output reg  [7:0] I2cSlaveRegAddr_oqb8,
    output reg  [7:0] I2cSlaveByte_oqb8,   
    // Status and results: 
    input             MasterBusy_i,
    input             MasterNewByteRead_ip,
    input       [7:0] MasterByteOut_ib8,
    input             MasterAckError_i,
    //==== I2c IO expander interrupts ====\\    
    input             IoExpApp12Int_ian,
    input             IoExpApp34Int_ian,
    input             IoExpBstEthInt_ian,
    input             IoExpLosInt_ian,
    input             IoExpBlmInInt_ian,   
    //==== System and Application Interface ====\\ 
    output reg        InitDone_oq,
    // Vme Ga and GaP:
    output reg  [4:0] VmeGa_onqb5,
    output reg        VmeGaP_onq,
    // Leds:
    input       [7:0] Led_ib8,
    output reg  [7:0] StatusLed_ob8,
    // GpIo:
    input             GpIo1A2B_i,
    input             EnGpIo1Term_i,
    input             GpIo2A2B_i,
    input             EnGpIo2Term_i,    
    input             GpIo34A2B_i,
    input             EnGpIo3Term_i,    
    input             EnGpIo4Term_i,    
    output reg        StatusGpIo1A2B_oq,
    output reg        StatusEnGpIo1Term_oq,
    output reg        StatusGpIo2A2B_oq,
    output reg        StatusEnGpIo2Term_oq,    
    output reg        StatusGpIo34A2B_oq,
    output reg        StatusEnGpIo3Term_oq,    
    output reg        StatusEnGpIo4Term_oq, 
    // BlmIn:
    output reg  [7:0] BlmIn_oqb8,
    // AppSfp1:
    output reg        AppSfp1Present_oq,
    output reg [15:0] AppSfp1Id_oq16,
    output reg        AppSfp1TxFault_oq,
    output reg        AppSfp1Los_oq,
    input             AppSfp1TxDisable_i,
    input             AppSfp1RateSelect_i,
    output reg        StatusAppSfp1TxDisable_oq,
    output reg        StatusAppSfp1RateSelect_oq,    
    // AppSfp2:
    output reg        AppSfp2Present_oq,
    output reg [15:0] AppSfp2Id_oq16,
    output reg        AppSfp2TxFault_oq,
    output reg        AppSfp2Los_oq,
    input             AppSfp2TxDisable_i,
    input             AppSfp2RateSelect_i, 
    output reg        StatusAppSfp2TxDisable_oq,
    output reg        StatusAppSfp2RateSelect_oq,    
    // AppSfp3:
    output reg        AppSfp3Present_oq,
    output reg [15:0] AppSfp3Id_oq16,
    output reg        AppSfp3TxFault_oq,
    output reg        AppSfp3Los_oq,
    input             AppSfp3TxDisable_i,
    input             AppSfp3RateSelect_i,  
    output reg        StatusAppSfp3TxDisable_oq,
    output reg        StatusAppSfp3RateSelect_oq,
    // AppSfp4:
    output reg        AppSfp4Present_oq,
    output reg [15:0] AppSfp4Id_oq16,
    output reg        AppSfp4TxFault_oq,
    output reg        AppSfp4Los_oq,
    input             AppSfp4TxDisable_i,
    input             AppSfp4RateSelect_i,  
    output reg        StatusAppSfp4TxDisable_oq,
    output reg        StatusAppSfp4RateSelect_oq,
    // BstSfp:
    output reg        BstSfpPresent_oq,
    output reg [15:0] BstSfpId_oq16,
    output reg        BstSfpTxFault_oq,
    output reg        BstSfpLos_oq,
    input             BstSfpTxDisable_i,
    input             BstSfpRateSelect_i,  
    output reg        StatusBstSfpTxDisable_oq,
    output reg        StatusBstSfpRateSelect_oq,    
    // EthSfp:
    output reg        EthSfpPresent_oq,
    output reg [15:0] EthSfpId_oq16,
    output reg        EthSfpTxFault_oq,
    output reg        EthSfpLos_oq,
    input             EthSfpTxDisable_i,
    input             EthSfpRateSelect_i, 
    output reg        StatusEthSfpTxDisable_oq,
    output reg        StatusEthSfpRateSelect_oq,     
    // CDR:
    output reg        CdrLos_oq,
    output reg        CdrLol_oq,
    //==== WishBone interface for the I2C slaves ====\\ 
    input              I2cWbCyc_i,
    input              I2cWbStb_i,
    input              I2cWbWe_i,
    input       [11:0] I2cWbAdr_ib12,
    input        [7:0] I2cWbDat_ib8,
    output  reg  [7:0] I2cWbDat_ob8,
    output  reg        I2cWbAck_o,    
    //==== WishBone interface for the configuration ====\\ 
    input              WbCyc_i,
    input              WbStb_i,
    input              WbWe_i,
    input       [31:0] WbDat_ib32,
    output  reg [31:0] WbDat_oqb32,
    output  reg        WbAck_oa    
);     
   
//=======================================  Declarations  =====================================\\    
//==== Local parameters ====\\ 
// FSM:
localparam  s_Idle            = 6'd00,
            s_GaRead          = 6'd01,
            s_BlmInRead       = 6'd02,
            s_LosRead         = 6'd03,
            s_GpioWrite       = 6'd04, 
            s_GpioSetMode     = 6'd05, 
            s_LedWrite        = 6'd06, 
            s_LedSetMode      = 6'd07,     
            s_BstEthWrite     = 6'd08, 
            s_BstEthSetMode   = 6'd09, 
            s_App1App2Write   = 6'd10, 
            s_App1App2SetMode = 6'd11, 
            s_App3App4Write   = 6'd12, 
            s_App3App4SetMode = 6'd13, 
            s_BstEthRead      = 6'd14, 
            s_BstReadId1      = 6'd15, 
            s_BstReadId2      = 6'd16,      
            s_BstI2cRead      = 6'd17, 
            s_BstI2cWrite     = 6'd18,       
            s_EthReadId1      = 6'd19, 
            s_EthReadId2      = 6'd20, 
            s_EthI2cRead      = 6'd21, 
            s_EthI2cWrite     = 6'd22, 
            s_App1App2Read    = 6'd23, 
            s_App1ReadId1     = 6'd24, 
            s_App1ReadId2     = 6'd25, 
            s_App1I2cRead     = 6'd26, 
            s_App1I2cWrite    = 6'd27,       
            s_App2ReadId1     = 6'd28, 
            s_App2ReadId2     = 6'd29, 
            s_App2I2cRead     = 6'd30, 
            s_App2I2cWrite    = 6'd31, 
            s_App3App4Read    = 6'd32, 
            s_App3ReadId1     = 6'd33, 
            s_App3ReadId2     = 6'd34, 
            s_App3I2cRead     = 6'd35, 
            s_App3I2cWrite    = 6'd36, 
            s_App4ReadId1     = 6'd37, 
            s_App4ReadId2     = 6'd38,       
            s_App4I2cRead     = 6'd39, 
            s_App4I2cWrite    = 6'd40, 
            s_CdrI2cRead      = 6'd41, 
            s_CdrI2cWrite     = 6'd42,        
            s_Si57xI2cRead    = 6'd43, 
            s_Si57xI2cWrite   = 6'd44; 
 
//==== Wires & Regs ====\\ 

reg [31:0] GlobalConfigReg_q32;
reg [31:0] BlmInConfigReg_q32;
reg [5:0] State_q = s_Idle, NextState_a, State_d = s_Idle;
reg AppSfp12ExpWrReqMask, AppSfp34ExpWrReqMask, BstEthSfpExpWrReqMask, GpioExpWrReqMask, LedExpWrReqMask, AppSfp12ExpRdReqMask, AppSfp34ExpRdReqMask, BstEthSfpExpRdReqMask, LosExRdReqMask;       
reg BlmInRdReqMask, AppSfp1I2cAccReqMask, AppSfp2I2cAccReqMask, AppSfp3I2cAccReqMask, AppSfp4I2cAccReqMask, BstSfpI2cAccReqMask, EthSfpI2cAccReqMask, CdrI2cAccReqMask, Si57xI2cAccReqMask; 
reg [23:0] BlmInMuteOthersCnt_c24 = 0;
reg PreAppSfp1Present_q = 0, 
    PreAppSfp2Present_q = 0, 
    PreAppSfp3Present_q = 0, 
    PreAppSfp4Present_q = 0, 
    PreEthSfpPresent_q  = 0, 
    PreBstSfpPresent_q  = 0; 
reg MasterBusy_d    = 0,
    MasterTrnDone_p = 0;
reg [2:0] IoExpApp12Int_x3, IoExpApp34Int_x3, IoExpBstEthInt_x3, IoExpLosInt_x3, IoExpBlmInInt_x3;
reg [23:0] WaitCounter_c24 = 0;
 
initial InitDone_oq = 0;
 
//=======================================  User Logic  =======================================\\    

//==== WishBone Interface ====\\ 

always @(posedge Clk_ik) 
    if (Rst_irq) begin
        GlobalConfigReg_q32 <= #1 {3'b0, 1'b0, 1'b0, 1'b0, 1'b0, 1'b0, 24'd250_000}; //Comment: all int enabled and the BLMIN timeout set to 2ms (2 SFP plugged at the same time). 1ms is the more reasonable delay if we allow plugging or 500us if we don't
        WbAck_oa            <= #1 1'b0;
    end else begin  
        if (WbCyc_i && WbWe_i && WbStb_i) GlobalConfigReg_q32 <= #1 GlobalConfigReg_q32;
        WbDat_oqb32    <= #1 WbWe_i ? WbDat_ib32 : GlobalConfigReg_q32;
        WbAck_oa       <= #1 WbStb_i&&WbCyc_i;
    end
 
wire        a_BlmInI2cIntDisable         = GlobalConfigReg_q32[28]; 
wire        a_LosI2cIntDisable           = GlobalConfigReg_q32[27]; 
wire        a_BstEthSfpI2cIntDisable     = GlobalConfigReg_q32[26];
wire        a_AppSfp34I2cIntDisable      = GlobalConfigReg_q32[25];
wire        a_AppSfp12I2cIntDisable      = GlobalConfigReg_q32[24];
wire [23:0] a_BlmInMuteOthersTime_b24    = GlobalConfigReg_q32[23:0];  //Comment: if the timeout is 0 the BLMIN works like the others, if is !=0 than it is never muted and mutes the others to guarantee latency determinism
wire        a_BlmInI2cIntTimeOutEnable   = |a_BlmInMuteOthersTime_b24;    


//==== I2C Wishbone Address Decoder ====\\

reg [3:0] SelectedInterface_b4 ;

localparam  c_SelNothing    = 4'd0,
            c_SelAppSfp1    = 4'd1,
            c_SelAppSfp2    = 4'd2,
            c_SelAppSfp3    = 4'd3,
            c_SelAppSfp4    = 4'd4,
            c_SelBstSfp     = 4'd5,
            c_SelEthSfp     = 4'd6,
            c_SelCdr        = 4'd7,
            c_SelSi57x      = 4'd8;

always @* 
    if (I2cWbCyc_i && I2cWbStb_i && ~I2cWbAck_o) casez(I2cWbAdr_ib12)
        12'b000?_????????: SelectedInterface_b4 = c_SelAppSfp1 ;
        12'b001?_????????: SelectedInterface_b4 = c_SelAppSfp2 ;
        12'b010?_????????: SelectedInterface_b4 = c_SelAppSfp3 ;
        12'b011?_????????: SelectedInterface_b4 = c_SelAppSfp4 ;
        12'b100?_????????: SelectedInterface_b4 = c_SelBstSfp  ;
        12'b101?_????????: SelectedInterface_b4 = c_SelEthSfp  ;
        12'b1100_????????: SelectedInterface_b4 = c_SelCdr     ;
        12'b1110_????????: SelectedInterface_b4 = c_SelSi57x   ;
        default:           SelectedInterface_b4 = c_SelNothing ;
        endcase 
    else SelectedInterface_b4 = c_SelNothing;

//==== I2c Exp and Mux Access Requests ====\\ 
 
always @(posedge Clk_ik) begin
    IoExpApp12Int_x3  <= #1 {IoExpApp12Int_x3 [1:0], IoExpApp12Int_ian};
    IoExpApp34Int_x3  <= #1 {IoExpApp34Int_x3 [1:0], IoExpApp34Int_ian};
    IoExpBstEthInt_x3 <= #1 {IoExpBstEthInt_x3[1:0], IoExpBstEthInt_ian};
    IoExpLosInt_x3    <= #1 {IoExpLosInt_x3   [1:0], IoExpLosInt_ian};
    IoExpBlmInInt_x3  <= #1 {IoExpBlmInInt_x3 [1:0], IoExpBlmInInt_ian};
end 
       
wire BlmInRdReq        = ~BlmInRdReqMask        && ~(IoExpBlmInInt_x3[2] || a_BlmInI2cIntDisable);
wire AppSfp12ExpRdReq  = ~AppSfp12ExpRdReqMask  && ~(IoExpApp12Int_x3[2] || a_AppSfp12I2cIntDisable);
wire AppSfp34ExpRdReq  = ~AppSfp34ExpRdReqMask  && ~(IoExpApp34Int_x3[2] || a_AppSfp34I2cIntDisable);
wire BstEthSfpExpRdReq = ~BstEthSfpExpRdReqMask && ~(IoExpBstEthInt_x3[2] || a_BstEthSfpI2cIntDisable);
wire LosExRdReq        = ~LosExRdReqMask        && ~(IoExpLosInt_x3[2] || a_LosI2cIntDisable);
wire AppSfp12ExpWrReq  = ~AppSfp12ExpWrReqMask  && (AppSfp1TxDisable_i^StatusAppSfp1TxDisable_oq) || (AppSfp1RateSelect_i^StatusAppSfp1RateSelect_oq) || (AppSfp2TxDisable_i^StatusAppSfp2TxDisable_oq) || (AppSfp2RateSelect_i^StatusAppSfp2RateSelect_oq);
wire AppSfp34ExpWrReq  = ~AppSfp34ExpWrReqMask  && (AppSfp3TxDisable_i^StatusAppSfp3TxDisable_oq) || (AppSfp3RateSelect_i^StatusAppSfp3RateSelect_oq) || (AppSfp4TxDisable_i^StatusAppSfp4TxDisable_oq) || (AppSfp4RateSelect_i^StatusAppSfp4RateSelect_oq);
wire BstEthSfpExpWrReq = ~BstEthSfpExpWrReqMask && (BstSfpTxDisable_i^StatusBstSfpTxDisable_oq) || (BstSfpRateSelect_i^StatusBstSfpRateSelect_oq) || (EthSfpTxDisable_i^StatusEthSfpTxDisable_oq) || (EthSfpRateSelect_i^StatusEthSfpRateSelect_oq);
wire GpioExpWrReq      = ~GpioExpWrReqMask      && (GpIo1A2B_i^StatusGpIo1A2B_oq || EnGpIo1Term_i^StatusEnGpIo1Term_oq || GpIo2A2B_i^StatusGpIo2A2B_oq || EnGpIo2Term_i^StatusEnGpIo2Term_oq || GpIo34A2B_i^StatusGpIo34A2B_oq || EnGpIo3Term_i^StatusEnGpIo3Term_oq || EnGpIo4Term_i^StatusEnGpIo4Term_oq);    
wire LedExpWrReq       = ~LedExpWrReqMask       && (Led_ib8!=StatusLed_ob8);

wire AppSfp1I2cAccReq  = ~AppSfp1I2cAccReqMask  && SelectedInterface_b4 == c_SelAppSfp1;
wire AppSfp2I2cAccReq  = ~AppSfp2I2cAccReqMask  && SelectedInterface_b4 == c_SelAppSfp2;
wire AppSfp3I2cAccReq  = ~AppSfp3I2cAccReqMask  && SelectedInterface_b4 == c_SelAppSfp3;
wire AppSfp4I2cAccReq  = ~AppSfp4I2cAccReqMask  && SelectedInterface_b4 == c_SelAppSfp4;
wire BstSfpI2cAccReq   = ~BstSfpI2cAccReqMask   && SelectedInterface_b4 == c_SelBstSfp;
wire EthSfpI2cAccReq   = ~EthSfpI2cAccReqMask   && SelectedInterface_b4 == c_SelEthSfp;
wire CdrI2cAccReq      = ~CdrI2cAccReqMask      && SelectedInterface_b4 == c_SelCdr;
wire Si57xI2cAccReq    = ~Si57xI2cAccReqMask    && SelectedInterface_b4 == c_SelSi57x;

//==== I2c Exp and Mux End of transaction detection ====\\

always @(posedge Clk_ik) MasterBusy_d    <= MasterBusy_i;
always @(posedge Clk_ik) MasterTrnDone_p <= MasterBusy_d && ~MasterBusy_i;

//==== State Machine ====\\ 

always @(posedge Clk_ik) State_d <= #1 State_q;

always @(posedge Clk_ik) State_q <= #1 Rst_irq ?  s_Idle :  NextState_a;
 
always @* begin
    NextState_a = State_q;
    case (State_q)
    s_Idle            : if (~MasterBusy_i) begin
                            if (~InitDone_oq) NextState_a = s_GaRead;
                            else if (|BlmInMuteOthersCnt_c24) begin  //Comment: a BlmIn int triggered a mute all to assure the latency determinism of its readout
                                if ((BlmInMuteOthersCnt_c24 == a_BlmInMuteOthersTime_b24) && BlmInRdReq ) NextState_a = s_BlmInRead;
                            end else begin
                                if (BlmInRdReq && ~|a_BlmInMuteOthersTime_b24) NextState_a = s_BlmInRead; //comment: The MuteOthers is set to 0 so no need to wait
                                else if (AppSfp12ExpRdReq )                    NextState_a = s_App1App2Read;
                                else if (AppSfp34ExpRdReq )                    NextState_a = s_App3App4Read;
                                else if (BstEthSfpExpRdReq)                    NextState_a = s_BstEthRead;
                                else if (LosExRdReq       )                    NextState_a = s_LosRead;
                                else if (AppSfp12ExpWrReq )                    NextState_a = s_App1App2Write;
                                else if (AppSfp34ExpWrReq )                    NextState_a = s_App3App4Write;
                                else if (BstEthSfpExpWrReq)                    NextState_a = s_BstEthWrite;
                                else if (GpioExpWrReq     )                    NextState_a = s_GpioWrite;
                                else if (LedExpWrReq      )                    NextState_a = s_LedWrite;
                                else if (AppSfp1I2cAccReq )                    NextState_a = I2cWbWe_i ? s_App1I2cWrite  : s_App1I2cRead; 
                                else if (AppSfp2I2cAccReq )                    NextState_a = I2cWbWe_i ? s_App2I2cWrite  : s_App2I2cRead; 
                                else if (AppSfp3I2cAccReq )                    NextState_a = I2cWbWe_i ? s_App3I2cWrite  : s_App3I2cRead; 
                                else if (AppSfp4I2cAccReq )                    NextState_a = I2cWbWe_i ? s_App4I2cWrite  : s_App4I2cRead; 
                                else if (BstSfpI2cAccReq  )                    NextState_a = I2cWbWe_i ? s_BstI2cWrite   : s_BstI2cRead; 
                                else if (EthSfpI2cAccReq  )                    NextState_a = I2cWbWe_i ? s_EthI2cWrite   : s_EthI2cRead;
                                else if (CdrI2cAccReq     )                    NextState_a = I2cWbWe_i ? s_CdrI2cWrite   : s_CdrI2cRead;
                                else if (Si57xI2cAccReq   )                    NextState_a = I2cWbWe_i ? s_Si57xI2cWrite : s_Si57xI2cRead;
                            end   
                        end
    s_GaRead          : if (MasterTrnDone_p) NextState_a = s_BlmInRead; //Comment: this is executed only in the initialization sequence
    s_BlmInRead       : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_LosRead        ;
    s_LosRead         : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_GpioWrite      ;
    s_GpioWrite       : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_GpioSetMode    ;
    s_GpioSetMode     : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_LedWrite       ;
    s_LedWrite        : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_LedSetMode     ;
    s_LedSetMode      : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_BstEthWrite    ;
    s_BstEthWrite     : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_BstEthSetMode  ;   
    s_BstEthSetMode   : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_App1App2Write  ;
    s_App1App2Write   : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_App1App2SetMode;
    s_App1App2SetMode : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_App3App4Write  ;
    s_App3App4Write   : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_App3App4SetMode;
    s_App3App4SetMode : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_BstEthRead     ;
    s_BstEthRead      : if (MasterTrnDone_p) begin
                            if      (PreBstSfpPresent_q && ~BstSfpPresent_oq) NextState_a = s_BstReadId1;
                            else if (PreEthSfpPresent_q && ~EthSfpPresent_oq) NextState_a = s_EthReadId1;
                            else                                              NextState_a = InitDone_oq ? s_Idle : s_App1App2Read;
                        end
    s_BstReadId1      : if (MasterTrnDone_p) NextState_a = s_BstReadId2;
    s_BstReadId2      : if (MasterTrnDone_p) begin
                            if (PreEthSfpPresent_q && ~EthSfpPresent_oq) NextState_a = s_EthReadId1;
                            else                                         NextState_a = InitDone_oq ? s_Idle : s_App1App2Read;
                        end
    s_EthReadId1      : if (MasterTrnDone_p) NextState_a = s_EthReadId2;
    s_EthReadId2      : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_App1App2Read;
    s_App1App2Read    : if (MasterTrnDone_p) begin
                            if      (PreAppSfp1Present_q && ~AppSfp1Present_oq) NextState_a = s_App1ReadId1;
                            else if (PreAppSfp2Present_q && ~AppSfp2Present_oq) NextState_a = s_App2ReadId1;
                            else                                               NextState_a = InitDone_oq ? s_Idle : s_App3App4Read;
                        end
    s_App1ReadId1     : if (MasterTrnDone_p) NextState_a = s_App1ReadId2;
    s_App1ReadId2     : if (MasterTrnDone_p) begin
                            if (PreAppSfp2Present_q && ~AppSfp2Present_oq) NextState_a = s_App2ReadId1;
                            else                                          NextState_a = InitDone_oq ? s_Idle : s_App3App4Read;
                        end   
    s_App2ReadId1     : if (MasterTrnDone_p) NextState_a = s_App2ReadId2;
    s_App2ReadId2     : if (MasterTrnDone_p) NextState_a = InitDone_oq ? s_Idle : s_App3App4Read;  
    s_App3App4Read    : if (MasterTrnDone_p) begin
                            if      (PreAppSfp3Present_q && ~AppSfp3Present_oq) NextState_a = s_App3ReadId1;
                            else if (PreAppSfp4Present_q && ~AppSfp4Present_oq) NextState_a = s_App4ReadId1;
                            else                                               NextState_a = s_Idle;
                        end
    s_App3ReadId1     : if (MasterTrnDone_p) NextState_a = s_App3ReadId2;
    s_App3ReadId2     : if (MasterTrnDone_p) begin
                            if (PreAppSfp4Present_q && ~AppSfp4Present_oq) NextState_a = s_App4ReadId1;
                            else                                          NextState_a = s_Idle;
                        end   
    s_App4ReadId1     : if (MasterTrnDone_p) NextState_a = s_App4ReadId2;
    s_App4ReadId2     : if (MasterTrnDone_p) NextState_a = s_Idle; 
    s_BstI2cRead      : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_BstI2cWrite     : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_EthI2cRead      : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_EthI2cWrite     : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_App1I2cRead     : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_App1I2cWrite    : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_App2I2cRead     : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_App2I2cWrite    : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_App3I2cRead     : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_App3I2cWrite    : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_App4I2cRead     : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_App4I2cWrite    : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle; 
    s_CdrI2cRead      : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_CdrI2cWrite     : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_Si57xI2cRead    : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    s_Si57xI2cWrite   : if (~(I2cWbCyc_i && I2cWbStb_i)) NextState_a = s_Idle;
    endcase
end
    
always @(posedge Clk_ik) begin
    if (Rst_irq) begin
        InitDone_oq                <= #1 1'b0;
        IoExpWrReq_oq              <= #1 1'b0;    
        IoExpRdReq_oq              <= #1 1'b0;     
        IoExpAddr_oqb3             <= #1 3'b0;
        IoExpRegAddr_oqb2          <= #1 2'b0; 
        IoExpData_oqb8             <= #1 8'b0;  
        I2cSlaveWrReq_oq           <= #1 1'b0;
        I2cSlaveRdReq_oq           <= #1 1'b0;
        I2cMuxAddress_oq           <= #1 1'b0;
        I2cMuxChannel_oqb2         <= #1 2'b0; 
        I2cSlaveAddr_oqb7          <= #1 7'b0;
        I2cSlaveRegAddr_oqb8       <= #1 8'b0;
        I2cSlaveByte_oqb8          <= #1 8'b0;      
        VmeGa_onqb5                <= #1 5'b0;
        VmeGaP_onq                 <= #1 1'b0;
        StatusLed_ob8              <= #1 8'b0;
        StatusGpIo1A2B_oq          <= #1 1'b0;
        StatusEnGpIo1Term_oq       <= #1 1'b0;
        StatusGpIo2A2B_oq          <= #1 1'b0;
        StatusEnGpIo2Term_oq       <= #1 1'b0;    
        StatusGpIo34A2B_oq         <= #1 1'b0;
        StatusEnGpIo3Term_oq       <= #1 1'b0;    
        StatusEnGpIo4Term_oq       <= #1 1'b0; 
        BlmIn_oqb8                 <= #1 8'b0;
        AppSfp1Present_oq          <= #1 1'b0;
        AppSfp1Id_oq16             <= #1 16'b0;
        AppSfp1TxFault_oq          <= #1 1'b0;
        AppSfp1Los_oq              <= #1 1'b0;
        StatusAppSfp1TxDisable_oq  <= #1 1'b0;
        StatusAppSfp1RateSelect_oq <= #1 1'b0;    
        AppSfp2Present_oq          <= #1 1'b0;
        AppSfp2Id_oq16             <= #1 16'b0;
        AppSfp2TxFault_oq          <= #1 1'b0;
        AppSfp2Los_oq              <= #1 1'b0;
        StatusAppSfp2TxDisable_oq  <= #1 1'b0;
        StatusAppSfp2RateSelect_oq <= #1 1'b0;    
        AppSfp3Present_oq          <= #1 1'b0;
        AppSfp3Id_oq16             <= #1 16'b0;
        AppSfp3TxFault_oq          <= #1 1'b0;
        AppSfp3Los_oq              <= #1 1'b0;
        StatusAppSfp3TxDisable_oq  <= #1 1'b0;
        StatusAppSfp3RateSelect_oq <= #1 1'b0; 
        AppSfp4Present_oq          <= #1 1'b0;
        AppSfp4Id_oq16             <= #1 16'b0;
        AppSfp4TxFault_oq          <= #1 1'b0;
        AppSfp4Los_oq              <= #1 1'b0;
        StatusAppSfp4TxDisable_oq  <= #1 1'b0;
        StatusAppSfp4RateSelect_oq <= #1 1'b0; 
        BstSfpPresent_oq           <= #1 1'b0;
        BstSfpId_oq16              <= #1 16'b0;
        BstSfpTxFault_oq           <= #1 1'b0;
        BstSfpLos_oq               <= #1 1'b0;
        StatusBstSfpTxDisable_oq   <= #1 1'b0;
        StatusBstSfpRateSelect_oq  <= #1 1'b0;    
        EthSfpPresent_oq           <= #1 1'b0;
        EthSfpId_oq16              <= #1 16'b0;
        EthSfpTxFault_oq           <= #1 1'b0;
        EthSfpLos_oq               <= #1 1'b0;
        StatusEthSfpTxDisable_oq   <= #1 1'b0;
        StatusEthSfpRateSelect_oq  <= #1 1'b0;     
        CdrLos_oq                  <= #1 1'b0;
        CdrLol_oq                  <= #1 1'b1;
        BlmInRdReqMask             <= #1 1'b0;
        AppSfp12ExpRdReqMask       <= #1 1'b0;
        AppSfp34ExpRdReqMask       <= #1 1'b0;
        BstEthSfpExpRdReqMask      <= #1 1'b0;
        LosExRdReqMask             <= #1 1'b0;
        AppSfp12ExpWrReqMask       <= #1 1'b0;
        AppSfp34ExpWrReqMask       <= #1 1'b0;
        BstEthSfpExpWrReqMask      <= #1 1'b0;
        GpioExpWrReqMask           <= #1 1'b0;
        LedExpWrReqMask            <= #1 1'b0;
        AppSfp1I2cAccReqMask       <= #1 1'b0;
        AppSfp2I2cAccReqMask       <= #1 1'b0;
        AppSfp3I2cAccReqMask       <= #1 1'b0;
        AppSfp4I2cAccReqMask       <= #1 1'b0;
        BstSfpI2cAccReqMask        <= #1 1'b0;
        EthSfpI2cAccReqMask        <= #1 1'b0;
        CdrI2cAccReqMask           <= #1 1'b0;
        Si57xI2cAccReqMask         <= #1 1'b0;
        BlmInMuteOthersCnt_c24     <= #1 24'h0;
        PreAppSfp1Present_q        <= #1 1'b0; 
        PreAppSfp2Present_q        <= #1 1'b0; 
        PreAppSfp3Present_q        <= #1 1'b0; 
        PreAppSfp4Present_q        <= #1 1'b0; 
        PreEthSfpPresent_q         <= #1 1'b0; 
        PreBstSfpPresent_q         <= #1 1'b0;  
        I2cWbAck_o                 <= #1 1'b0;
        WaitCounter_c24            <= #1 24'h0;
    end else begin
        if (~InitDone_oq || ~a_BlmInI2cIntTimeOutEnable) BlmInMuteOthersCnt_c24 <= #1 24'h0;
        else if (|BlmInMuteOthersCnt_c24 || (BlmInRdReq && |a_BlmInMuteOthersTime_b24)) BlmInMuteOthersCnt_c24 <= #1 BlmInMuteOthersCnt_c24 + 1'b1;      
        case(State_q)
        s_Idle            : begin 
            I2cWbAck_o                 <= #1 1'b0;
            WaitCounter_c24            <= #1 24'h0;
            if (~(BlmInRdReq||AppSfp12ExpRdReq||AppSfp34ExpRdReq||BstEthSfpExpRdReq||LosExRdReq||AppSfp12ExpWrReq||AppSfp34ExpWrReq||BstEthSfpExpWrReq||GpioExpWrReq||LedExpWrReq||AppSfp1I2cAccReq||AppSfp2I2cAccReq||AppSfp3I2cAccReq ||AppSfp4I2cAccReq||BstSfpI2cAccReq||EthSfpI2cAccReq||CdrI2cAccReq||Si57xI2cAccReq)) begin
                BlmInRdReqMask             <= #1 1'b0;
                AppSfp12ExpRdReqMask       <= #1 1'b0;
                AppSfp34ExpRdReqMask       <= #1 1'b0;
                BstEthSfpExpRdReqMask      <= #1 1'b0;
                LosExRdReqMask             <= #1 1'b0;
                AppSfp12ExpWrReqMask       <= #1 1'b0;
                AppSfp34ExpWrReqMask       <= #1 1'b0;
                BstEthSfpExpWrReqMask      <= #1 1'b0;
                GpioExpWrReqMask           <= #1 1'b0;
                LedExpWrReqMask            <= #1 1'b0;
                AppSfp1I2cAccReqMask       <= #1 1'b0;
                AppSfp2I2cAccReqMask       <= #1 1'b0;
                AppSfp3I2cAccReqMask       <= #1 1'b0;
                AppSfp4I2cAccReqMask       <= #1 1'b0;
                BstSfpI2cAccReqMask        <= #1 1'b0;
                EthSfpI2cAccReqMask        <= #1 1'b0;
                CdrI2cAccReqMask           <= #1 1'b0;
                Si57xI2cAccReqMask         <= #1 1'b0;           
            end
        end             
        s_GaRead          : begin
            if (State_q!=State_d) IoExpRdReq_oq <= #1 1'b1; 
            else if (IoExpRdOn_i) IoExpRdReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b011;
            IoExpRegAddr_oqb2         <= #1 2'b00; 
            if (MasterNewByteRead_ip) {VmeGaP_onq, VmeGa_onqb5} <= #1 MasterByteOut_ib8[5:0];
        end
        s_BlmInRead       : begin
            if (State_q!=State_d) IoExpRdReq_oq <= #1 1'b1; 
            else if (IoExpRdOn_i) IoExpRdReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b110;
            IoExpRegAddr_oqb2         <= #1 2'b00; 
            if (MasterNewByteRead_ip) BlmIn_oqb8 <= #1 MasterByteOut_ib8;
            BlmInRdReqMask         <= #1 ~a_BlmInI2cIntTimeOutEnable;
            BlmInMuteOthersCnt_c24 <= #1 24'b0;
        end
        s_LosRead         : begin
            if (State_q!=State_d) IoExpRdReq_oq <= #1 1'b1; 
            else if (IoExpRdOn_i) IoExpRdReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b111;
            IoExpRegAddr_oqb2         <= #1 2'b00; 
            if (MasterNewByteRead_ip) {CdrLol_oq, CdrLos_oq, EthSfpLos_oq, BstSfpLos_oq, AppSfp4Los_oq, AppSfp3Los_oq, AppSfp2Los_oq, AppSfp1Los_oq} <= #1 MasterByteOut_ib8;
            LosExRdReqMask         <= #1 1'b1;
        end
        s_GpioWrite       : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
                IoExpData_oqb8 <= #1 {1'b0, EnGpIo4Term_i, EnGpIo3Term_i, EnGpIo2Term_i, EnGpIo1Term_i, GpIo34A2B_i, GpIo2A2B_i, GpIo1A2B_i};
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b100;
            IoExpRegAddr_oqb2         <= #1 2'b01;             
            if (NextState_a != s_GpioWrite) {StatusEnGpIo4Term_oq, StatusEnGpIo3Term_oq, StatusEnGpIo2Term_oq, StatusEnGpIo1Term_oq, StatusGpIo34A2B_oq, StatusGpIo2A2B_oq, StatusGpIo1A2B_oq} <= #1 IoExpData_oqb8[6:0];
            GpioExpWrReqMask         <= #1 1'b1;
        end
        s_GpioSetMode     : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
                IoExpData_oqb8 <= #1 8'b0;
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b100;
            IoExpRegAddr_oqb2         <= #1 2'b11;             
        end
        s_LedWrite        : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
                IoExpData_oqb8 <= #1 ~Led_ib8;
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b101;
            IoExpRegAddr_oqb2         <= #1 2'b01;             
            if (NextState_a != s_LedWrite) StatusLed_ob8 <= #1 ~IoExpData_oqb8;
            LedExpWrReqMask         <= #1 1'b1;
        end
        s_LedSetMode      : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
                IoExpData_oqb8 <= #1 8'b0;
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b101;
            IoExpRegAddr_oqb2         <= #1 2'b11;             
        end        
        s_BstEthWrite     : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
                IoExpData_oqb8 <= #1 {1'b0, EthSfpRateSelect_i, EthSfpTxDisable_i, 1'b0, 1'b0, BstSfpRateSelect_i, BstSfpTxDisable_i, 1'b0};
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b010;
            IoExpRegAddr_oqb2         <= #1 2'b01;             
            if (NextState_a != s_BstEthWrite) begin
                StatusEthSfpRateSelect_oq <= #1 IoExpData_oqb8[6];
                StatusEthSfpTxDisable_oq  <= #1 IoExpData_oqb8[5];
                StatusBstSfpRateSelect_oq <= #1 IoExpData_oqb8[2];
                StatusBstSfpTxDisable_oq  <= #1 IoExpData_oqb8[1];
            end    
            BstEthSfpExpWrReqMask         <= #1 1'b1;
        end
        s_BstEthSetMode   : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b010;
            IoExpRegAddr_oqb2         <= #1 2'b11; 
            IoExpData_oqb8            <= #1 8'b10011001;            
        end
        s_BstEthRead      : begin
            if (State_q!=State_d) IoExpRdReq_oq <= #1 1'b1; 
            else if (IoExpRdOn_i) IoExpRdReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b010;
            IoExpRegAddr_oqb2         <= #1 2'b00; 
            if (MasterNewByteRead_ip) begin
                if (MasterByteOut_ib8[7]) begin
                    EthSfpPresent_oq <= #1 1'b0;
                    EthSfpId_oq16    <= #1 16'h0;
                end  
                PreEthSfpPresent_q <= #1 ~MasterByteOut_ib8[7]; 
                EthSfpTxFault_oq   <= #1 MasterByteOut_ib8[4];
                if (MasterByteOut_ib8[3]) begin
                    BstSfpPresent_oq <= #1 1'b0;
                    BstSfpId_oq16    <= #1 16'h0;
                end  
                PreBstSfpPresent_q  <= #1 ~MasterByteOut_ib8[3];
                BstSfpTxFault_oq    <= #1 MasterByteOut_ib8[0];
            end       
            BstEthSfpExpRdReqMask  <= #1 1'b1;
        end
        s_BstReadId1      : begin
            if ((State_q!=State_d) || |WaitCounter_c24)   WaitCounter_c24  <= #1 WaitCounter_c24 + 1'b1;
            if (&WaitCounter_c24)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;         
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b00; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b0;
            if (MasterNewByteRead_ip) BstSfpId_oq16[7:0] <= #1 MasterByteOut_ib8; 
        end
        s_BstReadId2      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;       
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b00; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b1;
            if (MasterNewByteRead_ip) begin 
                BstSfpId_oq16[15:8] <= #1 MasterByteOut_ib8;
                BstSfpPresent_oq    <= #1 1'b1;
            end
        end   
        s_EthReadId1      : begin
            if ((State_q!=State_d) || |WaitCounter_c24)   WaitCounter_c24  <= #1 WaitCounter_c24 + 1'b1;
            if (&WaitCounter_c24)    I2cSlaveRdReq_oq <= #1 1'b1;            
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;         
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b01; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b0;
            if (MasterNewByteRead_ip) EthSfpId_oq16[7:0] <= #1 MasterByteOut_ib8; 
        end
        s_EthReadId2      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;         
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b01; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b1;
            if (MasterNewByteRead_ip) begin 
                EthSfpId_oq16[15:8] <= #1 MasterByteOut_ib8;
                EthSfpPresent_oq    <= #1 1'b1;
            end
        end 
        s_App1App2Write     : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
                IoExpData_oqb8 <= #1 {1'b0, AppSfp2RateSelect_i, AppSfp2TxDisable_i, 1'b0, 1'b0, AppSfp1RateSelect_i, AppSfp1TxDisable_i, 1'b0};
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b000;
            IoExpRegAddr_oqb2         <= #1 2'b01;             
            if (NextState_a != s_App1App2Write) begin
                StatusAppSfp2RateSelect_oq <= #1 IoExpData_oqb8[6];
                StatusAppSfp2TxDisable_oq  <= #1 IoExpData_oqb8[5];
                StatusAppSfp1RateSelect_oq <= #1 IoExpData_oqb8[2];
                StatusAppSfp1TxDisable_oq  <= #1 IoExpData_oqb8[1];
            end    
            AppSfp12ExpWrReqMask         <= #1 1'b1;
        end
        s_App1App2SetMode   : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b000;
            IoExpRegAddr_oqb2         <= #1 2'b11;
            IoExpData_oqb8            <= #1 8'b10011001;            
        end
        s_App1App2Read      : begin
            if (State_q!=State_d) IoExpRdReq_oq <= #1 1'b1; 
            else if (IoExpRdOn_i) IoExpRdReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3          <= #1 3'b000;
            IoExpRegAddr_oqb2       <= #1 2'b00; 
            if (MasterNewByteRead_ip) begin
                if (MasterByteOut_ib8[7]) begin
                    AppSfp2Present_oq <= #1 1'b0;
                    AppSfp2Id_oq16    <= #1 16'h0;
                end 
                PreAppSfp2Present_q <= #1 ~MasterByteOut_ib8[7]; 
                AppSfp2TxFault_oq   <= #1 MasterByteOut_ib8[4];
                if (MasterByteOut_ib8[3]) begin
                    AppSfp1Present_oq <= #1 1'b0;
                    AppSfp1Id_oq16    <= #1 16'h0;
                end 
                PreAppSfp1Present_q <= #1 ~MasterByteOut_ib8[3];
                AppSfp1TxFault_oq   <= #1 MasterByteOut_ib8[0];
            end    
            AppSfp12ExpRdReqMask    <= #1 1'b1;
        end
        s_App1ReadId1      : begin
            if ((State_q!=State_d) || |WaitCounter_c24)   WaitCounter_c24  <= #1 WaitCounter_c24 + 1'b1;
            if (&WaitCounter_c24)    I2cSlaveRdReq_oq <= #1 1'b1;             
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;         
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b00; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b0;
            if (MasterNewByteRead_ip) AppSfp1Id_oq16[7:0] <= #1 MasterByteOut_ib8; 
        end        
        s_App1ReadId2      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;     
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b00; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b1;
            if (MasterNewByteRead_ip) begin 
                AppSfp1Id_oq16[15:8]  <= #1 MasterByteOut_ib8;
                AppSfp1Present_oq     <= #1 1'b1;
            end
        end  
        s_App2ReadId1      : begin
            if ((State_q!=State_d) || |WaitCounter_c24)   WaitCounter_c24  <= #1 WaitCounter_c24 + 1'b1;
            if (&WaitCounter_c24)    I2cSlaveRdReq_oq <= #1 1'b1;             
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;        
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b01; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b0;
            if (MasterNewByteRead_ip) AppSfp2Id_oq16[7:0] <= #1 MasterByteOut_ib8; 
        end        
        s_App2ReadId2      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b01; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b1;
            if (MasterNewByteRead_ip) begin 
                AppSfp2Id_oq16[15:8] <= #1 MasterByteOut_ib8;
                AppSfp2Present_oq    <= #1 1'b1;
            end
        end 
        s_App3App4Write     : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
                IoExpData_oqb8 <= #1 {1'b0, AppSfp4RateSelect_i, AppSfp4TxDisable_i, 1'b0, 1'b0, AppSfp3RateSelect_i, AppSfp3TxDisable_i, 1'b0};
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b001;
            IoExpRegAddr_oqb2         <= #1 2'b01;             
            if (NextState_a != s_App1App2Write) begin
                StatusAppSfp4RateSelect_oq <= #1 IoExpData_oqb8[6];
                StatusAppSfp4TxDisable_oq  <= #1 IoExpData_oqb8[5];
                StatusAppSfp3RateSelect_oq <= #1 IoExpData_oqb8[2];
                StatusAppSfp3TxDisable_oq  <= #1 IoExpData_oqb8[1];
            end    
            AppSfp34ExpWrReqMask     <= #1 1'b1;
        end
        s_App3App4SetMode   : begin
            if (State_q!=State_d) begin 
                IoExpWrReq_oq  <= #1 1'b1; 
            end else if (IoExpWrOn_i) IoExpWrReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b001;
            IoExpRegAddr_oqb2         <= #1 2'b11; 
            IoExpData_oqb8            <= #1 8'b10011001;            
        end       
        s_App3App4Read      : begin
            if (State_q!=State_d) IoExpRdReq_oq <= #1 1'b1; 
            else if (IoExpRdOn_i) IoExpRdReq_oq <= #1 1'b0; 
            IoExpAddr_oqb3            <= #1 3'b001;
            IoExpRegAddr_oqb2         <= #1 2'b00; 
            if (MasterNewByteRead_ip) begin
                if (MasterByteOut_ib8[7]) begin
                    AppSfp4Present_oq <= #1 1'b0;
                    AppSfp4Id_oq16    <= #1 16'h0;
                end 
                PreAppSfp4Present_q <= #1 ~MasterByteOut_ib8[7]; 
                AppSfp4TxFault_oq   <= #1 MasterByteOut_ib8[4];
                if (MasterByteOut_ib8[3]) begin
                    AppSfp3Present_oq <= #1 1'b0;
                    AppSfp3Id_oq16    <= #1 16'h0;
                end 
                PreAppSfp3Present_q <= #1 ~MasterByteOut_ib8[3];
                AppSfp3TxFault_oq   <= #1 MasterByteOut_ib8[0];
            end    
            AppSfp34ExpRdReqMask  <= #1 1'b1;
            if (NextState_a==s_Idle) InitDone_oq           <= #1 1'b1;
        end
        s_App3ReadId1      : begin
            if ((State_q!=State_d) || |WaitCounter_c24)   WaitCounter_c24  <= #1 WaitCounter_c24 + 1'b1;
            if (&WaitCounter_c24)    I2cSlaveRdReq_oq <= #1 1'b1;             
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b10; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b0;
            if (MasterNewByteRead_ip) AppSfp3Id_oq16[7:0] <= #1 MasterByteOut_ib8; 
        end        
        s_App3ReadId2      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b10; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b1;
            if (MasterNewByteRead_ip) begin 
                AppSfp3Id_oq16[15:8] <= #1 MasterByteOut_ib8;
                AppSfp3Present_oq    <= #1 1'b1;
            end
            if (NextState_a==s_Idle) InitDone_oq           <= #1 1'b1;            
        end  
        s_App4ReadId1      : begin
            if ((State_q!=State_d) || |WaitCounter_c24)   WaitCounter_c24  <= #1 WaitCounter_c24 + 1'b1;
            if (&WaitCounter_c24)    I2cSlaveRdReq_oq <= #1 1'b1;             
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b11; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b0;
            if (MasterNewByteRead_ip) AppSfp4Id_oq16[7:0] <= #1 MasterByteOut_ib8; 
        end        
        s_App4ReadId2      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1;
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0;
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b11; 
            I2cSlaveAddr_oqb7         <= #1 7'h50;
            I2cSlaveRegAddr_oqb8      <= #1 8'b1;
            if (MasterNewByteRead_ip) begin 
                AppSfp4Id_oq16[15:8] <= #1 MasterByteOut_ib8;
                AppSfp4Present_oq    <= #1 1'b1;
            end
            if (NextState_a==s_Idle) InitDone_oq <= #1 1'b1;
        end     
        s_BstI2cRead      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b00; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            if (MasterNewByteRead_ip) I2cWbDat_ob8 <= #1 MasterByteOut_ib8;
            BstSfpI2cAccReqMask        <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_BstI2cWrite     : begin
            if (State_q!=State_d)    I2cSlaveWrReq_oq <= #1 1'b1; 
            else if (I2cSlaveWrOn_i) I2cSlaveWrReq_oq <= #1 1'b0; 
            I2cSlaveByte_oqb8         <= #1 I2cWbDat_ib8;
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b00; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            BstSfpI2cAccReqMask       <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_EthI2cRead      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b01; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            if (MasterNewByteRead_ip) I2cWbDat_ob8 <= #1 MasterByteOut_ib8;
            EthSfpI2cAccReqMask        <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_EthI2cWrite     : begin
            if (State_q!=State_d)    I2cSlaveWrReq_oq <= #1 1'b1; 
            else if (I2cSlaveWrOn_i) I2cSlaveWrReq_oq <= #1 1'b0; 
            I2cSlaveByte_oqb8         <= #1 I2cWbDat_ib8;
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b01; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            EthSfpI2cAccReqMask       <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end        
        s_Si57xI2cRead      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b10; 
            I2cSlaveAddr_oqb7         <= #1 7'h55;
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            if (MasterNewByteRead_ip) I2cWbDat_ob8 <= #1 MasterByteOut_ib8;
            Si57xI2cAccReqMask        <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_Si57xI2cWrite     : begin
            if (State_q!=State_d)    I2cSlaveWrReq_oq <= #1 1'b1; 
            else if (I2cSlaveWrOn_i) I2cSlaveWrReq_oq <= #1 1'b0; 
            I2cSlaveByte_oqb8         <= #1 I2cWbDat_ib8;            
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b10; 
            I2cSlaveAddr_oqb7         <= #1 7'h55;
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            Si57xI2cAccReqMask        <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_CdrI2cRead      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b11; 
            I2cSlaveAddr_oqb7         <= #1 7'h40;
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            if (MasterNewByteRead_ip) I2cWbDat_ob8     <= #1 MasterByteOut_ib8;
            CdrI2cAccReqMask          <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_CdrI2cWrite     : begin
            if (State_q!=State_d)    I2cSlaveWrReq_oq <= #1 1'b1; 
            else if (I2cSlaveWrOn_i) I2cSlaveWrReq_oq <= #1 1'b0; 
            I2cSlaveByte_oqb8         <= #1 I2cWbDat_ib8;            
            I2cMuxAddress_oq          <= #1 1'b1;
            I2cMuxChannel_oqb2        <= #1 2'b11; 
            I2cSlaveAddr_oqb7         <= #1 7'h40;
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            CdrI2cAccReqMask          <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end
        s_App1I2cRead      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b00; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            if (MasterNewByteRead_ip) I2cWbDat_ob8 <= #1 MasterByteOut_ib8;
            AppSfp1I2cAccReqMask      <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_App1I2cWrite     : begin
            if (State_q!=State_d)    I2cSlaveWrReq_oq <= #1 1'b1; 
            else if (I2cSlaveWrOn_i) I2cSlaveWrReq_oq <= #1 1'b0; 
            I2cSlaveByte_oqb8         <= #1 I2cWbDat_ib8;            
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b00; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            AppSfp1I2cAccReqMask      <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_App2I2cRead      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b01; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            if (MasterNewByteRead_ip) I2cWbDat_ob8 <= #1 MasterByteOut_ib8;
            AppSfp2I2cAccReqMask      <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_App2I2cWrite     : begin
            if (State_q!=State_d)    I2cSlaveWrReq_oq <= #1 1'b1; 
            else if (I2cSlaveWrOn_i) I2cSlaveWrReq_oq <= #1 1'b0; 
            I2cSlaveByte_oqb8         <= #1 I2cWbDat_ib8;            
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b01; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            AppSfp2I2cAccReqMask      <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end  
        s_App3I2cRead      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b10; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            if (MasterNewByteRead_ip) I2cWbDat_ob8 <= #1 MasterByteOut_ib8; 
            AppSfp3I2cAccReqMask      <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_App3I2cWrite     : begin
            if (State_q!=State_d)    I2cSlaveWrReq_oq <= #1 1'b1; 
            else if (I2cSlaveWrOn_i) I2cSlaveWrReq_oq <= #1 1'b0; 
            I2cSlaveByte_oqb8         <= #1 I2cWbDat_ib8;            
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b10; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            AppSfp3I2cAccReqMask      <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end  
         s_App4I2cRead      : begin
            if (State_q!=State_d)    I2cSlaveRdReq_oq <= #1 1'b1; 
            else if (I2cSlaveRdOn_i) I2cSlaveRdReq_oq <= #1 1'b0; 
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b11; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            if (MasterNewByteRead_ip)  I2cWbDat_ob8 <= #1 MasterByteOut_ib8;
            AppSfp4I2cAccReqMask      <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        s_App4I2cWrite     : begin
            if (State_q!=State_d)    I2cSlaveWrReq_oq <= #1 1'b1; 
            else if (I2cSlaveWrOn_i) I2cSlaveWrReq_oq <= #1 1'b0; 
            I2cSlaveByte_oqb8         <= #1 I2cWbDat_ib8;            
            I2cMuxAddress_oq          <= #1 1'b0;
            I2cMuxChannel_oqb2        <= #1 2'b11; 
            I2cSlaveAddr_oqb7         <= #1 {6'b101000, I2cWbAdr_ib12[8]};
            I2cSlaveRegAddr_oqb8      <= #1 I2cWbAdr_ib12[7:0];
            AppSfp4I2cAccReqMask      <= #1 1'b1;
            if (MasterTrnDone_p)      I2cWbAck_o   <= #1 1'b1;
        end 
        default           : begin
            InitDone_oq                <= #1 1'b0;
            IoExpWrReq_oq              <= #1 1'b0;    
            IoExpRdReq_oq              <= #1 1'b0;     
            IoExpAddr_oqb3             <= #1 3'b0;
            IoExpRegAddr_oqb2          <= #1 2'b0; 
            IoExpData_oqb8             <= #1 8'b0;  
            I2cSlaveWrReq_oq           <= #1 1'b0;
            I2cSlaveRdReq_oq           <= #1 1'b0;
            I2cMuxAddress_oq           <= #1 1'b0;
            I2cMuxChannel_oqb2         <= #1 2'b0; 
            I2cSlaveAddr_oqb7          <= #1 7'b0;
            I2cSlaveRegAddr_oqb8       <= #1 8'b0;
            I2cSlaveByte_oqb8          <= #1 8'b0;      
            VmeGa_onqb5                <= #1 5'b0;
            VmeGaP_onq                 <= #1 1'b0;
            StatusLed_ob8              <= #1 8'b0;
            StatusGpIo1A2B_oq          <= #1 1'b0;
            StatusEnGpIo1Term_oq       <= #1 1'b0;
            StatusGpIo2A2B_oq          <= #1 1'b0;
            StatusEnGpIo2Term_oq       <= #1 1'b0;    
            StatusGpIo34A2B_oq         <= #1 1'b0;
            StatusEnGpIo3Term_oq       <= #1 1'b0;    
            StatusEnGpIo4Term_oq       <= #1 1'b0; 
            BlmIn_oqb8                 <= #1 8'b0;
            AppSfp1Present_oq          <= #1 1'b0;
            AppSfp1Id_oq16             <= #1 16'b0;
            AppSfp1TxFault_oq          <= #1 1'b0;
            AppSfp1Los_oq              <= #1 1'b0;
            StatusAppSfp1TxDisable_oq  <= #1 1'b0;
            StatusAppSfp1RateSelect_oq <= #1 1'b0;    
            AppSfp2Present_oq          <= #1 1'b0;
            AppSfp2Id_oq16             <= #1 16'b0;
            AppSfp2TxFault_oq          <= #1 1'b0;
            AppSfp2Los_oq              <= #1 1'b0;
            StatusAppSfp2TxDisable_oq  <= #1 1'b0;
            StatusAppSfp2RateSelect_oq <= #1 1'b0;    
            AppSfp3Present_oq          <= #1 1'b0;
            AppSfp3Id_oq16             <= #1 16'b0;
            AppSfp3TxFault_oq          <= #1 1'b0;
            AppSfp3Los_oq              <= #1 1'b0;
            StatusAppSfp3TxDisable_oq  <= #1 1'b0;
            StatusAppSfp3RateSelect_oq <= #1 1'b0; 
            AppSfp4Present_oq          <= #1 1'b0;
            AppSfp4Id_oq16             <= #1 16'b0;
            AppSfp4TxFault_oq          <= #1 1'b0;
            AppSfp4Los_oq              <= #1 1'b0;
            StatusAppSfp4TxDisable_oq  <= #1 1'b0;
            StatusAppSfp4RateSelect_oq <= #1 1'b0; 
            BstSfpPresent_oq           <= #1 1'b0;
            BstSfpId_oq16              <= #1 16'b0;
            BstSfpTxFault_oq           <= #1 1'b0;
            BstSfpLos_oq               <= #1 1'b0;
            StatusBstSfpTxDisable_oq   <= #1 1'b0;
            StatusBstSfpRateSelect_oq  <= #1 1'b0;    
            EthSfpPresent_oq           <= #1 1'b0;
            EthSfpId_oq16              <= #1 16'b0;
            EthSfpTxFault_oq           <= #1 1'b0;
            EthSfpLos_oq               <= #1 1'b0;
            StatusEthSfpTxDisable_oq   <= #1 1'b0;
            StatusEthSfpRateSelect_oq  <= #1 1'b0;     
            CdrLos_oq                  <= #1 1'b0;
            CdrLol_oq                  <= #1 1'b1;
            BlmInRdReqMask             <= #1 1'b0;
            AppSfp12ExpRdReqMask       <= #1 1'b0;
            AppSfp34ExpRdReqMask       <= #1 1'b0;
            BstEthSfpExpRdReqMask      <= #1 1'b0;
            LosExRdReqMask             <= #1 1'b0;
            AppSfp12ExpWrReqMask       <= #1 1'b0;
            AppSfp34ExpWrReqMask       <= #1 1'b0;
            BstEthSfpExpWrReqMask      <= #1 1'b0;
            GpioExpWrReqMask           <= #1 1'b0;
            LedExpWrReqMask            <= #1 1'b0;
            AppSfp1I2cAccReqMask       <= #1 1'b0;
            AppSfp2I2cAccReqMask       <= #1 1'b0;
            AppSfp3I2cAccReqMask       <= #1 1'b0;
            AppSfp4I2cAccReqMask       <= #1 1'b0;
            BstSfpI2cAccReqMask        <= #1 1'b0;
            EthSfpI2cAccReqMask        <= #1 1'b0;
            CdrI2cAccReqMask           <= #1 1'b0;
            Si57xI2cAccReqMask         <= #1 1'b0;
            BlmInMuteOthersCnt_c24     <= #1 24'h0;
            PreAppSfp1Present_q        <= #1 1'b0; 
            PreAppSfp2Present_q        <= #1 1'b0; 
            PreAppSfp3Present_q        <= #1 1'b0; 
            PreAppSfp4Present_q        <= #1 1'b0; 
            PreEthSfpPresent_q         <= #1 1'b0; 
            PreBstSfpPresent_q         <= #1 1'b0;  
            I2cWbAck_o                 <= #1 1'b0;
        end
        endcase
    end
end

endmodule
