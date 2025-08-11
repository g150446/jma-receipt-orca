          02  R98.
              03  R98-B06-STATE          PIC S9(9)   BINARY.
              03  R98-B07-STATE          PIC S9(9)   BINARY.
              03  R98-B09-STATE          PIC S9(9)   BINARY.
              03  R98-B09-LABEL          PIC X(8).
              03  R98-TITLE1             PIC X(8).
              03  R98-TBL                OCCURS  25.
                  04  R98-RENNUM         PIC X(5).
                  04  R98-PTNUM          PIC X(21).
                  04  R98-NAME           PIC X(31).
                  04  R98-FTNJANUM       PIC X(9).
                  04  R98-JKYSNUM        PIC X(21).
                  04  R98-KOFUNUM-STATE  PIC S9(9)   BINARY.
                  04  R98-KOFUNUM        PIC X(15).
                  04  R98-TEKSTYMD       PIC X(9).
              03  R98-SRYYMH.
                  04  R98-SRYYMH-STYLE   PIC X(20).
                  04  R98-SRYYMH-VALUE   PIC X(27).
              03  R98-HYOUJIKBN.
                  04  R98-HYOUJIKBN-STYLE   PIC X(20).
                  04  R98-HYOUJIKBN-VALUE   PIC X(8).
              03  R98-KOFU-TBL            OCCURS  25.
                  04  R98-KOFU-STATE      PIC S9(9)   BINARY.
                  04  R98-KOFU-STYLE      PIC X(20).
                  04  R98-KOFU-LABEL      PIC X(2).
