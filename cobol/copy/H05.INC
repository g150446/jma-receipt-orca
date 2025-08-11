            02  H05.
              03  H05-B03-STATE PIC S9(9)   BINARY.
              03  H05-B09-STATE PIC S9(9)   BINARY.
              03  H05-B12-STATE PIC S9(9)   BINARY.
              03  H05-SELNUM.
                04  H05-VALUE   PIC S9(3).
              03  H05-SCROLLEDWINDOW1.
                04  H05-KANRI-LIST.
                  05  H05-KANRI-LIST-COUNT     PIC S9(9)   BINARY.
                  05  H05-KANRI-LIST-ROW       PIC S9(9)   BINARY.
                  05  H05-KANRI-LIST-ITEM          OCCURS  100 TIMES.
                    06  H05-KANRI-TSEQNUM      PIC X(3).
                    06  H05-KANRI-TSYORIYM     PIC X(6).
                    06  H05-KANRI-TTEISYUTUSAKI
                                               PIC X(12).
                    06  H05-KANRI-TTEISYUTUSAKI2
                                               PIC X(2).
                    06  H05-KANRI-TCNT         PIC X(4).
                    06  H05-KANRI-THOSPCD      PIC X(2).
                    06  H05-KANRI-TERRCD       PIC X(3).
                    06  H05-KANRI-TCREYMD      PIC X(9).
                  05  H05-KANRI-LIST-SELECT    PIC X
                                                   OCCURS  100 TIMES.
              03  H05-SCROLLEDWINDOW2.
                04  H05-BODY-LIST.
                  05  H05-BODY-LIST-HYODAI2   PIC X(6).
                  05  H05-BODY-LIST-HYODAI    PIC X(16).
                  05  H05-BODY-LIST-COUNT     PIC S9(9)   BINARY.
                  05  H05-BODY-LIST-ROW       PIC S9(9)   BINARY.
                  05  H05-BODY-LIST-ROWATTR   PIC S9(9)   BINARY.
                  05  H05-BODY-LIST-ITEM          OCCURS  200 TIMES.
                    06  H05-BODY-TSEQNUM      PIC X(3).
                    06  H05-BODY-TPTNUM       PIC X(20).
                    06  H05-BODY-TNAME        PIC X(40).
                    06  H05-BODY-TBIRTHDAY    PIC X(9).
                    06  H05-BODY-TSEX         PIC X(2).
                    06  H05-BODY-TSRYYM       PIC X(6).
                    06  H05-BODY-THKNJANUM    PIC X(8).
                    06  H05-BODY-TKIGO        PIC X(40).
                    06  H05-BODY-TEDABAN      PIC X(2).
                    06  H05-BODY-TNYUGAIKBN   PIC X(2).
                    06  H05-BODY-TRECESYUBETU PIC X(10).
                    06  H05-BODY-TRECEIPT-KBN PIC X(4).
                    06  H05-BODY-TDISASTER    PIC X(2).
                  05  H05-BODY-LIST-SELECT
                          PIC X       OCCURS  200 TIMES.
                  05  H05-BODY-LIST-FGCOLOR  PIC X(16) OCCURS 200 TIMES.
                  05  H05-BODY-LIST-BGCOLOR  PIC X(16) OCCURS 200 TIMES.
              03  H05-SCROLLEDWINDOW3.
                04  H05-CHK-LIST.
                  05  H05-CHK-LIST-HYODAI2   PIC X(6).
                  05  H05-CHK-LIST-HYODAI    PIC X(16).
                  05  H05-CHK-LIST-COUNT     PIC S9(9)   BINARY.
                  05  H05-CHK-LIST-ROW       PIC S9(9)   BINARY.
                  05  H05-CHK-LIST-ITEM          OCCURS  2 TIMES.
                    06  H05-CHK-TPTNUM       PIC X(20).
                    06  H05-CHK-TNAME        PIC X(40).
                    06  H05-CHK-TBIRTHDAY    PIC X(9).
                    06  H05-CHK-TSEX         PIC X(2).
                    06  H05-CHK-TSRYYM       PIC X(6).
                    06  H05-CHK-THKNJANUM    PIC X(8).
                    06  H05-CHK-TKIGO        PIC X(40).
                    06  H05-CHK-TEDABAN      PIC X(2).
                    06  H05-CHK-TNYUGAIKBN   PIC X(2).
                    06  H05-CHK-TRECESYUBETU PIC X(10).
                    06  H05-CHK-TRECEIPT-KBN PIC X(4).
                    06  H05-CHK-TDISASTER    PIC X(2).
                  05  H05-CHK-LIST-SELECT
                          PIC X       OCCURS  2 TIMES.
                  05  H05-CHK-LIST-FGCOLOR  PIC X(16) OCCURS 2 TIMES.
                  05  H05-CHK-LIST-BGCOLOR  PIC X(16) OCCURS 2 TIMES.
              03  H05-B-SELNUM.
                04  H05-NUMDATA PIC S9(3).
