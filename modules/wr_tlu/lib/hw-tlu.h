#define GSI_VENDOR_ID 0x651
#define TLU_DEVICE_ID 0x10051981U

//1 pop => (3rd+rec+adr + 1wr+rec+adr))* 4bytes = 32bytes 

#define POP_TS_OPSIZE         32
#define UDP_MAX_POPS          46 // 1500 / 32

#define TLU_READY           0x00 //
#define TLU_CLEAR           0x04
#define TLU_TEST            0x08
#define TLU_ACTIVE_STATUS   0x0C
#define TLU_ACTIVE_SET      0x10
#define TLU_ACTIVE_CLR      0x14
#define TLU_EDGE_STATUS     0x18
#define TLU_EDGE_SET        0x1C
#define TLU_EDGE_CLR        0x20
#define TLU_INT_GLOBAL      0x24
#define TLU_INT_STATUS      0x28
#define TLU_INT_SET         0x2C
#define TLU_INT_CLR         0x30
#define TLU_NUM_CHANNELS    0x34
#define TLU_QUEUE_SIZE      0x38
#define TLU_TIME1           0x50
#define TLU_TIME0           0x54

#define TLU_CH_SELECT       0x58
#define TLU_CH_POP          0x5C
#define TLU_CH_TEST         0x60
#define TLU_CH_FILL_COUNT   0x64
#define TLU_CH_TIME1        0x68
#define TLU_CH_TIME0        0x6C
#define TLU_CH_SUB          0x70
#define TLU_CH_STABLE       0x74
#define TLU_CH_INT_DEST     0x78
#define TLU_CH_INT_MSG      0x7C
