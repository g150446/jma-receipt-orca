            02  H04.
              03  H04-SYORIYM.
                04  H04-VALUE   PIC X(6).
              03  H04-TEISYUTUSAKI.
                04  H04-VALUE   PIC X(12).
              03  H04-TEISYUTUSAKI2.
                04  H04-VALUE   PIC X(2).
              03  H04-CREYMD.
                04  H04-VALUE   PIC X(9).
              03  H04-SELNUM.
                04  H04-VALUE   PIC S9(4).
              03  H04-SCROLLEDWINDOW1.
                04  H04-KANRI-LIST.
                  05  H04-KANRI-LIST-COUNT     PIC S9(9)   BINARY.
                  05  H04-KANRI-LIST-ROW       PIC S9(9)   BINARY.
                  05  H04-KANRI-LIST-ITEM          OCCURS  100 TIMES.
                    06  H04-KANRI-TSEQNUM      PIC X(3).
                    06  H04-KANRI-TNYUGAIKBN   PIC X(2).
                    06  H04-KANRI-TPTNUM       PIC X(20).
                    06  H04-KANRI-TNAME        PIC X(40).
                    06  H04-KANRI-TSRYYM       PIC X(6).
                    06  H04-KANRI-TRECESYUBETU PIC X(10).
                    06  H04-KANRI-THKNJANUM    PIC X(8).
                    06  H04-KANRI-TKIGO        PIC X(40).
                    06  H04-KANRI-TRECEIPT-KBN PIC X(4).
                    06  H04-KANRI-TDISASTER    PIC X(2).
                    06  H04-KANRI-TERR         PIC X(100).
                    06  H04-KANRI-TERRCD       PIC X(3).
                  05  H04-KANRI-LIST-SELECT    PIC X
                                                   OCCURS  100 TIMES.
                  05  H04-KANRI-LIST-FGCOLOR   PIC X(16)
                                                   OCCURS  100 TIMES.
                  05  H04-KANRI-LIST-BGCOLOR   PIC X(16)
                                                   OCCURS  100 TIMES.
              03  H04-SCROLLEDWINDOW2.
                04  H04-HPOS    PIC S9(9)   BINARY.
                04  H04-VPOS    PIC S9(9)   BINARY.
                04  H04-HENREI-JIYU.
                  05  H04-VALUE   PIC X(2420).
