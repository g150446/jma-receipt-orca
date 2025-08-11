        02  Z061.
            03  Z061-TITLE  PIC X(56).
            03  Z061-POPUP-SUMMARY  PIC X(56).
            03  Z061-POPUP-BODY PIC X(56).
            03  Z061-POPUP-ICON PIC X(56).
            03  Z061-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ****  03  Z061-FIXED32.
              03  Z061-B01.
                04  Z061-STATE  PIC S9(9)   BINARY.
              03  Z061-INPUT.
                04  Z061-TEXTDATA   PIC X(40).
              03  Z061-B02.
                04  Z061-STATE  PIC S9(9)   BINARY.
              03  Z061-B09.
                04  Z061-STATE  PIC S9(9)   BINARY.
              03  Z061-B10.
                04  Z061-STATE  PIC S9(9)   BINARY.
              03  Z061-B11.
                04  Z061-STATE  PIC S9(9)   BINARY.
              03  Z061-SELNUM1.
                04  Z061-TEXTDATA   PIC X(3).
              03  Z061-B07.
                04  Z061-STATE  PIC S9(9)   BINARY.
              03  Z061-B12.
                04  Z061-STATE  PIC S9(9)   BINARY.
              03  Z061-B07S.
                04  Z061-STATE  PIC S9(9)   BINARY.
              03  Z061-BASIC-COB.
                04  Z061-BASIC-COUNT  PIC S9(9)   BINARY.
                04  Z061-BASIC-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  Z061-BASIC.
                  05  Z061-TEXTDATA   PIC X(30).
              03  Z061-SCROLLEDWINDOW26.
                04  Z061-LIST.
                  05  Z061-LIST-COUNT  PIC S9(9)   BINARY.
                  05  Z061-LIST-ROW    PIC S9(9)   BINARY.
                  05  Z061-LIST-SELECT
                          PIC X       OCCURS  200 TIMES.
                  05  Z061-FGCOLOR
                          PIC X(56)   OCCURS  200 TIMES.
                  05  Z061-BGCOLOR
                          PIC X(56)   OCCURS  200 TIMES.
                  05  Z061-ITEM       OCCURS  200 TIMES.
                    06  Z061-TNO       PIC X(4).
                    06  Z061-TNAME     PIC X(200).
                    06  Z061-TBASIC    PIC X(08).
                    06  Z061-TDETAIL   PIC X(60).
                    06  Z061-TTIMING   PIC X(60).
                    06  Z061-TREFER-NO  PIC X(4).
              03  Z061-NEXT.
                04  Z061-NEXT-STYLE   PIC X(20).
                04  Z061-NEXT-VALUE   PIC X(30).
              03  Z061-DETAIL-COB.
                04  Z061-DETAIL-COUNT  PIC S9(9)   BINARY.
                04  Z061-DETAIL-ITEM
                        PIC X(62)   OCCURS  30 TIMES.
                04  Z061-DETAIL.
                  05  Z061-TEXTDATA   PIC X(62).
              03  Z061-TIMING-COB.
                04  Z061-TIMING-COUNT  PIC S9(9)   BINARY.
                04  Z061-TIMING-ITEM
                        PIC X(62)   OCCURS  30 TIMES.
                04  Z061-TIMING.
                  05  Z061-TEXTDATA   PIC X(62).
