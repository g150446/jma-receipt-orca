        02  MPDEL.
            03  MPDEL-TITLE PIC X(256).
            03  MPDEL-POPUP-SUMMARY PIC X(256).
            03  MPDEL-POPUP-BODY    PIC X(256).
            03  MPDEL-POPUP-ICON    PIC X(256).
            03  MPDEL-POPUP-TIMEOUT PIC S9(9)   BINARY.
      ***** 02  MPDEL-FIXED32.
              03  MPDEL-B01.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B02.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B03.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B04.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B05.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B06.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B07.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B08.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B09.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B10.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B11.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-B12.
                04  MPDEL-STATE PIC S9(9)   BINARY.
              03  MPDEL-SCROLLEDWINDOW26.
                04  MPDEL-LIST.
                  05  MPDEL-LIST-COUNT PIC S9(9)   BINARY.
                  05  MPDEL-LIST-ROW   PIC S9(9)   BINARY.
                  05  MPDEL-LIST-SELECT
                          PIC X       OCCURS  100 TIMES.
                  05  MPDEL-FGCOLOR
                          PIC X(20)   OCCURS  100 TIMES.
                  05  MPDEL-BGCOLOR
                          PIC X(20)   OCCURS  100 TIMES.
                  05  MPDEL-ITEM      OCCURS  100 TIMES.
                    06  MPDEL-MODE      PIC X(50).
                    06  MPDEL-DELYMD    PIC X(10).
                    06  MPDEL-DELTIME   PIC X(10).
                    06  MPDEL-PTNUM     PIC X(20).
                    06  MPDEL-BIRTHDAY  PIC X(10).
                    06  MPDEL-SEX       PIC X(2).
                    06  MPDEL-NAME      PIC X(200).
                    06  MPDEL-SRYYMD    PIC X(10).
                    06  MPDEL-SRYKA     PIC X(2).
                    06  MPDEL-DRCD      PIC X(10).
                    06  MPDEL-HKNCOMBI  PIC X(4).
                    06  MPDEL-DENPNUM   PIC X(10).
                    06  MPDEL-OPID      PIC X(64).
              03  MPDEL-MIDASI.
                04  MPDEL-TEXTDATA  PIC X(50).
              03  MPDEL-NEXTMSG.
                04  MPDEL-NEXTMSG-STYLE PIC X(20).
                04  MPDEL-NEXTMSG-TEXTDATA  PIC X(50).
              03  MPDEL-SELPTNUM.
                04  MPDEL-TEXTDATA  PIC X(20).

