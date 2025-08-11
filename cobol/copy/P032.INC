        02  P032.
            03  P032-TITLE  PIC X(256).
            03  P032-POPUP-SUMMARY  PIC X(256).
            03  P032-POPUP-BODY PIC X(256).
            03  P032-POPUP-ICON PIC X(256).
            03  P032-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ***** 02  P032-FIXED1.
              03  P032-B01.
                04  P032-STATE  PIC S9(9)   BINARY.
              03  P032-MIDASI.
                04  P032-MIDASI-STYLE  PIC X(20).
                04  P032-MIDASI-TEXTDATA   PIC X(100).
              03  P032-SCROLLEDWINDOW1.
                04  P032-ONSHILIST.
                  05  P032-ONSHILIST-COUNT  PIC S9(9)   BINARY.
                  05  P032-ROW    PIC S9(9)   BINARY.
                  05  P032-SELECTDATA
                          PIC X       OCCURS  50 TIMES.
                  05  P032-FGCOLOR
                          PIC X(20)   OCCURS  50 TIMES.
                  05  P032-BGCOLOR
                          PIC X(20)   OCCURS  50 TIMES.
                  05  P032-ITEM       OCCURS  50 TIMES.
                    06  P032-RENNUM    PIC X(3).
                    06  P032-SHOKAI    PIC X(4).
                    06  P032-SYUJYU    PIC X(4).
                    06  P032-SHINRYO    PIC X(4).
                    06  P032-YAKUZAI   PIC X(4).
                    06  P032-KENSIN    PIC X(4).
                    06  P032-GENDO    PIC X(4).
                    06  P032-SIKKAN   PIC X(4).
                    06  P032-KAKUNYMD   PIC X(10).
                    06  P032-SHORITIME    PIC X(20).
                    06  P032-HKNJANUM    PIC X(8).
                    06  P032-KIGONUM    PIC X(160).
                    06  P032-EDABAN     PIC X(2).
                    06  P032-HONKZKKBN   PIC X(2).
                    06  P032-FTNRATE    PIC X(3).
                    06  P032-SIKAKU-YUKO PIC X(16).
                    06  P032-TEKEDYMD   PIC X(9).
                    06  P032-NAME       PIC X(100).
                    06  P032-KANANAME   PIC X(100).
                    06  P032-POST       PIC X(8).
                    06  P032-ADRS       PIC X(200).
                    06  P032-SHOKAI-NUM   PIC X(50).
              03  P032-SELNUM.
                04  P032-NUMDATA    PIC S9(3).
              03  P032-KAKUNINYMD.
                04  P032-TEXTDATA   PIC X(30).
              03  P032-B02.
                04  P032-STATE  PIC S9(9)   BINARY.
              03  P032-B06.
                04  P032-STATE  PIC S9(9)   BINARY.
              03  P032-B07.
                04  P032-STATE  PIC S9(9)   BINARY.
              03  P032-B12.
                04  P032-STATE  PIC S9(9)   BINARY.
              03  P032-MIDASINEXT.
                04  P032-MIDASINEXT-STYLE  PIC X(20).
                04  P032-MIDASINEXT-TEXTDATA   PIC X(40).
              03  P032-B04.
                04  P032-STATE  PIC S9(9)   BINARY.
              03  P032-B05.
                04  P032-STATE  PIC S9(9)   BINARY.
              03  P032-B10.
                04  P032-B10-STATE  PIC S9(9)   BINARY.
                04  P032-B10-LABEL  PIC X(10).
              03  P032-MIDASI2.
                04  P032-MIDASI2-STYLE  PIC X(20).
                04  P032-MIDASI2-TEXTDATA   PIC X(50).
