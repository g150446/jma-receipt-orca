        02  P031.
            03  P031-TITLE  PIC X(256).
            03  P031-POPUP-SUMMARY  PIC X(256).
            03  P031-POPUP-BODY PIC X(256).
            03  P031-POPUP-ICON PIC X(256).
            03  P031-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ***** 02  P031-FIXED1.
              03  P031-B01.
                04  P031-STATE  PIC S9(9)   BINARY.
              03  P031-MIDASI.
                04  P031-MIDASI-STYLE  PIC X(20).
                04  P031-MIDASI-TEXTDATA   PIC X(100).
              03  P031-SCROLLEDWINDOW1.
                04  P031-ONSHILIST.
                  05  P031-ONSHILIST-COUNT  PIC S9(9)   BINARY.
                  05  P031-ONSHILIST-ROW    PIC S9(9)   BINARY.
                  05  P031-ONSHILIST-SELECT
                          PIC X       OCCURS  50 TIMES.
                  05  P031-FGCOLOR
                          PIC X(20)   OCCURS  50 TIMES.
                  05  P031-BGCOLOR
                          PIC X(20)   OCCURS  50 TIMES.
                  05  P031-ITEM       OCCURS  50 TIMES.
                    06  P031-RENNUM   PIC X(3).
                    06  P031-SHOKAI   PIC X(4).
                    06  P031-SYUJYU    PIC X(4).
                    06  P031-SHINRYO    PIC X(4).
                    06  P031-YAKUZAI   PIC X(4).
                    06  P031-KENSIN    PIC X(4).
                    06  P031-GENDO    PIC X(4).
                    06  P031-SIKKAN   PIC X(4).
                    06  P031-SHORITIME    PIC X(20).
                    06  P031-PTNUM    PIC X(20).
                    06  P031-NAME     PIC X(100).
                    06  P031-KANANAME PIC X(100).
                    06  P031-SEX      PIC X(2).
                    06  P031-BIRTHDAY  PIC X(9).
                    06  P031-NENREI    PIC X(6).
                    06  P031-HKNJANUM  PIC X(8).
                    06  P031-KIGONUM  PIC X(160).
                    06  P031-EDABAN   PIC X(2).
                    06  P031-HONKZKKBN   PIC X(2).
                    06  P031-FTNRATE    PIC X(3).
                    06  P031-SIKAKU-YUKO PIC X(16).
                    06  P031-TEKEDYMD PIC X(9).
                    06  P031-POST     PIC X(8).
                    06  P031-ADRS     PIC X(100).
                    06  P031-SHOKAI-NUM   PIC X(50).
              03  P031-SELNUM.
                04  P031-NUMDATA    PIC S9(3).
              03  P031-KAKUNINYMD.
                04  P031-TEXTDATA   PIC X(30).
              03  P031-B02.
                04  P031-STATE  PIC S9(9)   BINARY.
              03  P031-B06.
                04  P031-STATE  PIC S9(9)   BINARY.
              03  P031-B07.
                04  P031-STATE  PIC S9(9)   BINARY.
              03  P031-B12.
                04  P031-STATE  PIC S9(9)   BINARY.
              03  P031-MIDASINEXT.
                04  P031-MIDASINEXT-STYLE  PIC X(20).
                04  P031-MIDASINEXT-TEXTDATA   PIC X(20).
              03  P031-B10.
                04  P031-B10-STATE  PIC S9(9)   BINARY.
                04  P031-B10-LABEL  PIC X(10).
              03  P031-MIDASI2.
                04  P031-MIDASI2-STYLE  PIC X(20).
                04  P031-MIDASI2-TEXTDATA   PIC X(50).
              03  P031-B09.
                04  P031-B09-STATE  PIC S9(9)   BINARY.
                04  P031-B09-LABEL  PIC X(12).
              03  P031-B03.
                04  P031-B03-STATE  PIC S9(9)   BINARY.
                04  P031-B03-LABEL  PIC X(12).
              03  P031-FOOTMSG.
                04  P031-FOOTMSG-STYLE  PIC X(20).
                04  P031-FOOTMSG-TEXTDATA   PIC X(100).
              03  P031-CHNGKBN-COMB.
                04  P031-CHNGKBN-COUNT  PIC S9(9)   BINARY.
                04  P031-CHNGKBN-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  P031-CHNGKBN.
                  05  P031-TEXTDATA   PIC X(30).
              03  P031-B05S.
                04  P031-B05S-STATE  PIC S9(9)   BINARY.
              03  P031-PANDATIMER1.
                04  P031-DURATION   PIC S9(9)   BINARY.

