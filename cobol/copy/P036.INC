        02  P036.
            03  P036-TITLE  PIC X(256).
            03  P036-POPUP-SUMMARY  PIC X(256).
            03  P036-POPUP-BODY PIC X(256).
            03  P036-POPUP-ICON PIC X(256).
            03  P036-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ****  02  P036-FIXED1.
              03  P036-B01.
                04  P036-STATE  PIC S9(9)   BINARY.
              03  P036-MIDASI.
                04  P036-TEXTDATA   PIC X(50).
              03  P036-SCROLLEDWINDOW1.
                04  P036-KNJALIST.
                  05  P036-KNJALIST-COUNT  PIC S9(9)   BINARY.
                  05  P036-KNJALIST-ROW    PIC S9(9)   BINARY.
                  05  P036-KNJALIST-SELECT
                          PIC X       OCCURS  50 TIMES.
                  05  P036-FGCOLOR
                          PIC X(20)   OCCURS  50 TIMES.
                  05  P036-BGCOLOR
                          PIC X(20)   OCCURS  50 TIMES.
                  05  P036-ITEM       OCCURS  50 TIMES.
                    06  P036-RENNUM     PIC X(3).
                    06  P036-PTNUM      PIC X(20).
                    06  P036-NAME       PIC X(100).
                    06  P036-KANANAME   PIC X(100).
                    06  P036-SEX        PIC X(2).
                    06  P036-HKNJANUM   PIC X(8).
                    06  P036-KIGO       PIC X(80).
                    06  P036-NUM        PIC X(80).
                    06  P036-EDABAN     PIC X(2).
              03  P036-SELNUM.
                04  P036-NUMDATA    PIC S9(3).
              03  P036-BIRTHDAY.
                04  P036-TEXTDATA   PIC X(30).
              03  P036-B02.
                04  P036-STATE  PIC S9(9)   BINARY.
              03  P036-B12.
                04  P036-STATE  PIC S9(9)   BINARY.
              03  P036-MIDASI2.
                04  P036-TEXTDATA   PIC X(50).
              03  P036-B08.
                04  P036-STATE  PIC S9(9)   BINARY.
