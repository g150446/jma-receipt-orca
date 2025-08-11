        02  P034.
            03  P034-TITLE  PIC X(256).
            03  P034-POPUP-SUMMARY  PIC X(256).
            03  P034-POPUP-BODY PIC X(256).
            03  P034-POPUP-ICON PIC X(256).
            03  P034-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      **    02  P034-FIXED1.
              03  P034-B01.
                04  P034-STATE  PIC S9(9)   BINARY.
              03  P034-ON-MIDASHI-G.
                04  P034-ON-MIDASHI-STYLE  PIC X(20).
                04  P034-ON-MIDASHI   PIC X(100).
              03  P034-LABEL01.
                04  P034-TEXTDATA   PIC X(20).
              03  P034-LABEL02.
                04  P034-TEXTDATA   PIC X(20).
              03  P034-GENDO-MSG.
                04  P034-TEXTDATA   PIC X(150).
              03  P034-SIKKAN-MSG1.
                04  P034-TEXTDATA   PIC X(150).
              03  P034-SIKKAN-MSG2.
                04  P034-TEXTDATA   PIC X(150).
              03  P034-SIKKAN-MSG3.
                04  P034-TEXTDATA   PIC X(150).
              03  P034-ON-KOHMSG-TBL     OCCURS   7.
                04  P034-ON-KOHMSG-STYLE  PIC X(20).
                04  P034-ON-KOHMSG        PIC X(150).
              03  P034-SCROLLEDWINDOW1.
                04  P034-KOHLIST.
                  05  P034-KOHLIST-COUNT  PIC S9(9)   BINARY.
                  05  P034-KOHLIST-ROW    PIC S9(9)   BINARY.
                  05  P034-KOHLIST-SELECT
                          PIC X       OCCURS  40 TIMES.
                  05  P034-KOHLIST-FGCOLOR
                          PIC X(26)   OCCURS  40 TIMES.
                  05  P034-KOHLIST-BGCOLOR
                          PIC X(26)   OCCURS  40 TIMES.
                  05  P034-ITEM       OCCURS  40 TIMES.
                    06  P034-TKOHNUM    PIC X(30).
                    06  P034-TFTNJANUM    PIC X(8).
                    06  P034-TJKYSNUM    PIC X(20).
                    06  P034-TKTEKSTYMD    PIC X(10).
                    06  P034-TKTEKEDYMD    PIC X(10).
              03  P034-SCROLLEDWINDOW2.
                04  P034-TEISYOLIST.
                  05  P034-LABL1.
                    06  P034-TEXTDATA   PIC X(12).

                  05  P034-TEISYOLIST-COUNT  PIC S9(9)   BINARY.
                  05  P034-TEISYOLIST-ROW    PIC S9(9)   BINARY.
                  05  P034-TEISYOLIST-SELECT
                          PIC X       OCCURS  10 TIMES.
                  05  P034-TEISYOLIST-FGCOLOR
                          PIC X(26)   OCCURS  10 TIMES.
                  05  P034-TEISYOLIST-BGCOLOR
                          PIC X(26)   OCCURS  10 TIMES.
                  05  P034-ITEM       OCCURS  10 TIMES.
                    06  P034-TSYLSTYMD    PIC X(10).
                    06  P034-TSYLEDYMD    PIC X(10).
                    06  P034-TSYLHKNKBN    PIC X(12).
                    06  P034-TSYLNINYMD    PIC X(10).
                    06  P034-TSYLSKJGNSTYMD    PIC X(10).
                    06  P034-TSYLKYOUKAI    PIC X(20).
              03  P034-MIDASHI2.
                04  P034-TEXTDATA   PIC X(50).
              03  P034-CHK-KOHMSG-TBL    OCCURS   7.
                04  P034-CHK-KOHMSG-ISACTIVE   PIC X.
                04  P034-CHK-KOHMSG-STYLE  PIC X(20).
                04  P034-CHK-KOHMSG  PIC X(30).
              03  P034-B02.
                04  P034-STATE  PIC S9(9)   BINARY.
              03  P034-FTNJANUM.
                04  P034-TEXTDATA   PIC X(8).
              03  P034-KOHNUM-COMB.
                04  P034-KOHNUM-COUNT  PIC S9(9)   BINARY.
                04  P034-KOHNUM-ITEM
                        PIC X(20)   OCCURS  200 TIMES.
                04  P034-KOHNUM.
                  05  P034-TEXTDATA   PIC X(20).
              03  P034-JKYSNUM.
                04  P034-TEXTDATA   PIC X(20).
              03  P034-TEKSTYMD.
                04  P034-TEXTDATA   PIC X(10).
              03  P034-TEKEDYMD.
                04  P034-TEXTDATA   PIC X(10).
              03  P034-B05.
                04  P034-STATE  PIC S9(9)   BINARY.
              03  P034-B08.
                04  P034-STATE  PIC S9(9)   BINARY.
              03  P034-B03.
                04  P034-STATE  PIC S9(9)   BINARY.
              03  P034-SELNUM.
                04  P034-TEXTDATA   PIC X(2).
              03  P034-LABEL03.
                04  P034-TEXTDATA   PIC X(30).
              03  P034-MONTHMSG.
                04  P034-TEXTDATA   PIC X(60).
              03  P034-LABEL04.
                04  P034-TEXTDATA   PIC X(20).
              03  P034-LABEL05.
                04  P034-TEXTDATA   PIC X(20).
              03  P034-B06.
                04  P034-B06-STATE  PIC S9(9)   BINARY.
                04  P034-B06-LABEL  PIC X(12).
              03  P034-MONTHMSG1.
                04  P034-TEXTDATA   PIC X(60).
              03  P034-MONTHMSG2.
                04  P034-TEXTDATA   PIC X(60).
              03  P034-MONTHMSG3.
                04  P034-TEXTDATA   PIC X(60).
              03  P034-MONTHMSG4.
                04  P034-TEXTDATA   PIC X(60).
              03  P034-ON-MIDASHI2-G.
                04  P034-ON-MIDASHI2-STYLE  PIC X(20).
                04  P034-ON-MIDASHI2   PIC X(60).
