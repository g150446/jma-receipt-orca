        02  P033.
            03  P033-TITLE  PIC X(256).
            03  P033-POPUP-SUMMARY  PIC X(256).
            03  P033-POPUP-BODY PIC X(256).
            03  P033-POPUP-ICON PIC X(256).
            03  P033-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ******02  P033-FIXED1.
              03  P033-B01.
                04  P033-STATE  PIC S9(9)   BINARY.
              03  P033-SELNUM.
                04  P033-NUMDATA    PIC S9(3).
              03  P033-B08.
                04  P033-STATE  PIC S9(9)   BINARY.
              03  P033-NAME-G.
                04  P033-NAME-STYLE  PIC X(20).
                04  P033-NAME        PIC X(100).
              03  P033-SEX-G.
                04  P033-SEX-STYLE  PIC X(20).
                04  P033-SEX        PIC X(2).
              03  P033-BIRTHDAY.
                04  P033-TEXTDATA   PIC X(10).
              03  P033-ADRS-G.
                04  P033-ADRS-STYLE  PIC X(20).
                04  P033-ADRS        PIC X(200).
              03  P033-HKNJANUM-G.
                04  P033-HKNJANUM-STYLE  PIC X(20).
                04  P033-HKNJANUM   PIC X(8).
              03  P033-KIGO-G.
                04  P033-KIGO-STYLE  PIC X(20).
                04  P033-KIGO   PIC X(80).
              03  P033-PTNUM.
                04  P033-TEXTDATA   PIC X(20).
              03  P033-KIKAN.
                04  P033-TEXTDATA   PIC X(20).
              03  P033-ON-HKNJANUM-G.
                04  P033-ON-HKNJANUM-STYLE  PIC X(20).
                04  P033-ON-HKNJANUM   PIC X(8).
              03  P033-ON-KIGO-G.
                04  P033-ON-KIGO-STYLE  PIC X(20).
                04  P033-ON-KIGO   PIC X(100).
              03  P033-ON-BIRTHDAY.
                04  P033-TEXTDATA   PIC X(10).
              03  P033-ON-SEX-G.
                04  P033-ON-SEX-STYLE  PIC X(20).
                04  P033-ON-SEX        PIC X(2).
              03  P033-ON-NENREI.
                04  P033-TEXTDATA   PIC X(6).
              03  P033-ON-KANANAME-G.
                04  P033-ON-KANANAME-STYLE  PIC X(20).
                04  P033-ON-KANANAME   PIC X(100).
              03  P033-ON-NAME-G.
                04  P033-ON-NAME-STYLE  PIC X(20).
                04  P033-ON-NAME   PIC X(100).
              03  P033-B10.
                04  P033-STATE  PIC S9(9)   BINARY.
              03  P033-B12.
                04  P033-STATE  PIC S9(9)   BINARY.
              03  P033-HONKZKKBN-G.
                04  P033-HONKZKKBN-STYLE  PIC X(20).
                04  P033-HONKZKKBN   PIC X(4).
              03  P033-ON-HONKZKKBN-G.
                04  P033-ON-HONKZKKBN-STYLE  PIC X(20).
                04  P033-ON-HONKZKKBN   PIC X(4).
              03  P033-B02.
                04  P033-STATE  PIC S9(9)   BINARY.
              03  P033-HKNNUMMEI-G.
                04  P033-HKNNUMMEI-STYLE  PIC X(20).
                04  P033-HKNNUMMEI   PIC X(16).
              03  P033-ON-HKNNUMMEI-G.
                04  P033-ON-HKNNUMMEI-STYLE  PIC X(20).
                04  P033-ON-HKNNUMMEI   PIC X(16).
              03  P033-HKNJANAME-G.
                04  P033-HKNJANAME   PIC X(100).
              03  P033-ON-HKNJANAME.
                04  P033-TEXTDATA   PIC X(100).
              03  P033-SCROLLEDWINDOW1.
                04  P033-DUALLIST.
                  05  P033-COUNT  PIC S9(9)   BINARY.
                  05  P033-ROW    PIC S9(9)   BINARY.
                  05  P033-SELECT
                          PIC X       OCCURS  99 TIMES.
                  05  P033-FGCOLOR
                          PIC X(20)   OCCURS  99 TIMES.
                  05  P033-BGCOLOR
                          PIC X(20)   OCCURS  99 TIMES.
                  05  P033-ITEM       OCCURS  99 TIMES.
                    06  P033-TNUM        PIC X(3).
                    06  P033-TPTNUM      PIC X(20).
                    06  P033-TNAME       PIC X(100).
                    06  P033-TBIRTHDAY   PIC X(10).
                    06  P033-TSEX        PIC X(2).
                    06  P033-TPOST       PIC X(8).
                    06  P033-TADRS       PIC X(200).
              03  P033-ON-EDABAN-G.
                04  P033-ON-EDABAN-STYLE  PIC X(20).
                04  P033-ON-EDABAN   PIC X(2).
              03  P033-ON-HOJOKBN-G.
                04  P033-ON-HOJOKBN-STYLE  PIC X(20).
                04  P033-ON-HOJOKBN   PIC X(6).
              03  P033-ON-ADRS-G.
                04  P033-ON-ADRS-STYLE  PIC X(20).
                04  P033-ON-ADRS   PIC X(200).
              03  P033-ON-POST-G.
                04  P033-ON-POST-STYLE  PIC X(20).
                04  P033-ON-POST   PIC X(8).
              03  P033-ON-SKKGETYMD-G.
                04  P033-ON-SKKGETYMD-STYLE  PIC X(20).
                04  P033-ON-SKKGETYMD   PIC X(10).
              03  P033-ON-NUM-G.
                04  P033-ON-NUM-STYLE  PIC X(20).
                04  P033-ON-NUM   PIC X(100).
              03  P033-ON-HIHKNJANAME-G.
                04  P033-ON-HIHKNJANAME-STYLE  PIC X(20).
                04  P033-ON-HIHKNJANAME   PIC X(100).
              03  P033-POST-G.
                04  P033-POST-STYLE  PIC X(20).
                04  P033-POST        PIC X(8).
              03  P033-NUM-G.
                04  P033-NUM-STYLE  PIC X(20).
                04  P033-NUM   PIC X(80).
              03  P033-EDABAN-G.
                04  P033-EDABAN-STYLE  PIC X(20).
                04  P033-EDABAN   PIC X(2).
              03  P033-SKKGETYMD-G.
                04  P033-SKKGETYMD-STYLE  PIC X(20).
                04  P033-SKKGETYMD   PIC X(10).
              03  P033-HIHKNJANAME-G.
                04  P033-HIHKNJANAME-STYLE  PIC X(20).
                04  P033-HIHKNJANAME   PIC X(100).
              03  P033-HOJOKBN-G.
                04  P033-HOJOKBN-STYLE  PIC X(20).
                04  P033-HOJOKBN   PIC X(6).
              03  P033-KANANAME-G.
                04  P033-KANANAME-STYLE  PIC X(20).
                04  P033-KANANAME        PIC X(100).
              03  P033-ON-KIKAN-G.
                04  P033-ON-KIKAN-STYLE  PIC X(20).
                04  P033-ON-KIKAN        PIC X(20).
              03  P033-ON-SIKAKUYUKO-G.
                04  P033-ON-SIKAKUYUKO-STYLE  PIC X(20).
                04  P033-ON-SIKAKUYUKO   PIC X(40).
              03  P033-ON-MIDASHI.
                04  P033-TEXTDATA   PIC X(100).
              03  P033-CHK-KANANAME.
                04  P033-CHK-KANANAME-ISACTIVE   PIC X.
                04  P033-CHK-KANANAME-LABEL  PIC X(2).
              03  P033-CHK-NAME.
                04  P033-CHK-NAME-ISACTIVE   PIC X.
                04  P033-CHK-NAME-LABEL  PIC X(2).
             03  P033-CHK-ADRS.
                04  P033-CHK-ADRS-ISACTIVE   PIC X.
                04  P033-CHK-ADRS-LABEL  PIC X(2).
              03  P033-B09.
                04  P033-STATE  PIC S9(9)   BINARY.
              03  P033-LABEL01-G.
                04  P033-LABEL01-STYLE  PIC X(20).
                04  P033-LABEL01        PIC X(20).
              03  P033-LABEL02-G.
                04  P033-LABEL02-STYLE  PIC X(20).
                04  P033-LABEL02        PIC X(20).
              03  P033-GENDO-MSG.
                04  P033-TEXTDATA   PIC X(200).
              03  P033-SIKKAN-MSG1.
                04  P033-TEXTDATA   PIC X(200).
              03  P033-SIKKAN-MSG2.
                04  P033-TEXTDATA   PIC X(200).
              03  P033-SIKKAN-MSG3.
                04  P033-TEXTDATA   PIC X(200).
             03  P033-ON-MIDASHI2-G.
                04  P033-ON-MIDASHI2-STYLE  PIC X(20).
                04  P033-ON-MIDASHI2   PIC X(50).
             03  P033-ON-KOHSYOMSG-G.
                04  P033-ON-KOHSYOMSG-STYLE  PIC X(20).
                04  P033-ON-KOHSYOMSG   PIC X(200).
             03  P033-CHK-SEX.
                04  P033-CHK-SEX-ISACTIVE   PIC X.
                04  P033-CHK-SEX-LABEL  PIC X(2).

