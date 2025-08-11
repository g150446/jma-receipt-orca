        02  P02T2.
            03  P02T2-TITLE PIC X(256).
            03  P02T2-POPUP-SUMMARY PIC X(256).
            03  P02T2-POPUP-BODY    PIC X(256).
            03  P02T2-POPUP-ICON    PIC X(256).
            03  P02T2-POPUP-TIMEOUT PIC S9(9)   BINARY.
      ***** 03  P02T2-FIXED10.
              03  P02T2-B01.
                04  P02T2-STATE PIC S9(9)   BINARY.
              03  P02T2-HKNMSG.
                04  P02T2-TEXTDATA  PIC X(20).
              03  P02T2-SCROLLEDWINDOW1.
                04  P02T2-ONSGENDOLST.
                  05  P02T2-ONSGENDOLST-COUNT PIC S9(9)   BINARY.
                  05  P02T2-ONSGENDOLST-ROW   PIC S9(9)   BINARY.
                  05  P02T2-ONSGENDOLST-SELECT
                          PIC X       OCCURS  99 TIMES.
                  05  P02T2-FGCOLOR
                          PIC X(20)   OCCURS  99 TIMES.
                  05  P02T2-BGCOLOR
                          PIC X(20)   OCCURS  99 TIMES.
                  05  P02T2-ITEM      OCCURS  99 TIMES.
                    06  P02T2-RENNUM   PIC X(3).
                    06  P02T2-ONSGENDO PIC X(10).
