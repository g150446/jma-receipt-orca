            02  H98.
              03  H98-MSG.
                04  H98-VALUE   PIC X(80).
              03  H98-SCROLLEDWINDOW28.
                04  H98-JOBKANRILST.
                  05  H98-COUNT   PIC S9(9)   BINARY.
                  05  H98-ROW     PIC S9(9)   BINARY.
                  05  H98-ROWATTR PIC S9(9)   BINARY.
                  05  H98-ITEM        OCCURS  2 TIMES.
                    06  H98-RENNUM    PIC X(7).
                    06  H98-SHORINM   PIC X(26).
                    06  H98-STARTTIME PIC X(8).
                    06  H98-ENDTIME   PIC X(8).
                    06  H98-PAGE      PIC X(10).
                    06  H98-ERRMSG    PIC X(100).
                  05  H98-SELECT  PIC X       OCCURS  2 TIMES.
              03  H98-B12-STATE   PIC S9(9)   BINARY.
              03  H98-B12-LABEL   PIC X(14).
              03  H98-DURATION   PIC S9(09) BINARY.
