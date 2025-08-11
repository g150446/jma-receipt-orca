        02  P0311.
            03  P0311-TITLE PIC X(256).
            03  P0311-POPUP-SUMMARY PIC X(256).
            03  P0311-POPUP-BODY    PIC X(256).
            03  P0311-POPUP-ICON    PIC X(256).
            03  P0311-POPUP-TIMEOUT PIC S9(9)   BINARY.
     ****** 02  P0311-FIXED1.
              03  P0311-B01.
                04  P0311-STATE PIC S9(9)   BINARY.
              03  P0311-B12.
                04  P0311-STATE PIC S9(9)   BINARY.
              03  P0311-B02.
                04  P0311-STATE PIC S9(9)   BINARY.
              03  P0311-STRYMD.
                04  P0311-TEXTDATA  PIC X(10).
              03  P0311-ENDYMD.
                04  P0311-TEXTDATA  PIC X(10).
              03  P0311-SYUTOKUKBN-COMB.
                04  P0311-SYUTOKUKBN-COUNT PIC S9(9)   BINARY.
                04  P0311-SYUTOKUKBN-ITEM
                        PIC X(30)   OCCURS  4 TIMES.
                04  P0311-SYUTOKUKBN.
                  05  P0311-TEXTDATA  PIC X(30).
              03  P0311-SCROLLEDWINDOW1.
                04  P0311-DOUILST.
                  05  P0311-DOUILST-COUNT PIC S9(9)   BINARY.
                  05  P0311-DOUILST-ROW   PIC S9(9)   BINARY.
                  05  P0311-DOUILST-SELECTDATA
                          PIC X       OCCURS  10 TIMES.
                  05  P0311-FGCOLOR
                          PIC X(256)  OCCURS  10 TIMES.
                  05  P0311-BGCOLOR
                          PIC X(256)  OCCURS  10 TIMES.
                  05  P0311-ITEM      OCCURS  10 TIMES.
                    06  P0311-RENNUM      PIC X(2).
                    06  P0311-SYOKAIKIKAN   PIC X(30).
                    06  P0311-ERRCDMSG    PIC X(10).
                    06  P0311-SRYYMD      PIC X(10).
                    06  P0311-STARTTIME   PIC X(10).
              03  P0311-SYORIMSG-G.
                04  P0311-SYORIMSG-STYLE PIC X(20).
                04  P0311-SYORIMSG  PIC X(100).
