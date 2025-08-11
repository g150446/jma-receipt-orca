            02  R95.
              03  R95-SRYYMH         PIC X(26).
              03  R95-RENNUM         PIC X(5).
              03  R95-PTNUM          PIC X(21).
              03  R95-NAME           PIC X(30).
              03  R95-HFTNJANUM      PIC X(8).
              03  R95-HJKYSNUM       PIC X(7).
              03  R95-HKOFUNUM       PIC X(15).
              03  R95-TEKSTYMD       PIC X(9).
              03  R95-JKYSNUM-STYLE  PIC X(20).
              03  R95-JKYSNUM        PIC X(7).
              03  R95-KOFUNUM-STYLE  PIC X(20).
              03  R95-KOFUNUM        PIC X(15).
              03  R95-SELNUM.
                04  R95-VALUE        PIC S9(2).
              03  R95-SCROLLEDWINDOW1.
                04  R95-BODY-LIST.
                  05  R95-BODY-LIST-COUNT     PIC S9(9)   BINARY.
                  05  R95-BODY-LIST-ROW       PIC S9(9)   BINARY.
                  05  R95-BODY-LIST-ITEM          OCCURS  20 TIMES.
                    06  R95-BODY-TSEQNUM      PIC X(2).
                    06  R95-BODY-TFTNJANUM    PIC X(8).
                    06  R95-BODY-TJKYSNUM     PIC X(7).
                    06  R95-BODY-TKOFUNUM     PIC X(15).
                  05  R95-BODY-LIST-SELECT    PIC X
                                                   OCCURS  20 TIMES.
