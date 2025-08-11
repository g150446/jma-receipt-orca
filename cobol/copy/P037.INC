        02  P037.
            03  P037-TITLE  PIC X(256).
            03  P037-POPUP-SUMMARY  PIC X(256).
            03  P037-POPUP-BODY PIC X(256).
            03  P037-POPUP-ICON PIC X(256).
            03  P037-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ****  02  P037-FIXED1.
              03  P037-B01.
                04  P037-STATE  PIC S9(9)   BINARY.
              03  P037-FTNJANUM.
                04  P037-TEXTDATA   PIC X(8).
              03  P037-BIRTHDAY.
                04  P037-TEXTDATA   PIC X(10).
              03  P037-NENREI.
                04  P037-TEXTDATA   PIC X(6).
              03  P037-B12.
                04  P037-STATE  PIC S9(9)   BINARY.
              03  P037-B02.
                04  P037-STATE  PIC S9(9)   BINARY.
              03  P037-JKYSNUM.
                04  P037-TEXTDATA   PIC X(20).
              03  P037-TEKSTYMD1.
                04  P037-TEXTDATA   PIC X(10).
              03  P037-MIDASHI.
                04  P037-TEXTDATA   PIC X(50).
              03  P037-IMGAREA.
                04  P037-OBJECTDATA PIC X(36).
              03  P037-PTNUM.
                04  P037-TEXTDATA   PIC X(20).
              03  P037-KANANAME.
                04  P037-TEXTDATA   PIC X(100).
              03  P037-NAME.
                04  P037-TEXTDATA   PIC X(100).
              03  P037-B05.
                04  P037-STATE  PIC S9(9)   BINARY.
              03  P037-SEX.
                04  P037-TEXTDATA   PIC X(4).
              03  P037-B10.
                04  P037-B10-STATE  PIC S9(9)   BINARY.
                04  P037-B10-LABEL  PIC X(20).
              03  P037-HBIRTHDAY.
                04  P037-TEXTDATA   PIC X(10).
              03  P037-TEKEDYMD1.
                04  P037-TEXTDATA   PIC X(10).
              03  P037-SELNUM.
                04  P037-NUMDATA    PIC S9(2).
              03  P037-TEKSTYMD2.
                04  P037-TEXTDATA   PIC X(10).
              03  P037-TEKEDYMD2.
                04  P037-TEXTDATA   PIC X(10).
              03  P037-B09.
                04  P037-B09-STATE  PIC S9(9)   BINARY.
                04  P037-B09-LABEL  PIC X(20).
              03  P037-B06.
                04  P037-STATE  PIC S9(9)   BINARY.
              03  P037-SCROLLEDWINDOW1.
                04  P037-KOHLIST.
                  05  P037-KOHLIST-COUNT  PIC S9(9)   BINARY.
                  05  P037-KOHLIST-ROW    PIC S9(9)   BINARY.
                  05  P037-KOHLIST-SELECT
                          PIC X       OCCURS  10 TIMES.
                  05  P037-FGCOLOR
                          PIC X(25)   OCCURS  10 TIMES.
                  05  P037-BGCOLOR
                          PIC X(25)   OCCURS  10 TIMES.
                  05  P037-KOHLIST-ITEM       OCCURS  10 TIMES.
                    06  P037-RENNUM      PIC X(2).
                    06  P037-TBIRTHDAY    PIC X(10).
                    06  P037-TFTNJANUM    PIC X(8).
                    06  P037-TJKYSNUM    PIC X(20).
                    06  P037-TTEKYMD1    PIC X(26).
                    06  P037-TTEKYMD2    PIC X(26).
                    06  P037-TKBN        PIC X(6).
              03  P037-KOHMSG.
                04  P037-KOHMSG-STYLE   PIC X(20).
                04  P037-KOHMSG-DATA    PIC X(40).

