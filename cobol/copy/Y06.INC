            02  Y06.
              03  Y06-B12-STATE      PIC S9(9)   BINARY.
              03  Y06-YYKYMD-STYLE   PIC X(20).
              03  Y06-YYKYMD         PIC X(22).
              03  Y06-SCROLLEDWINDOW28.
                04  Y06-JOBKANRILST.
                  05  Y06-COUNT   PIC S9(9)   BINARY.
                  05  Y06-ROW     PIC S9(9)   BINARY.
                  05  Y06-ROWATTR PIC S9(9)   BINARY.
                  05  Y06-ITEM        OCCURS  10 TIMES.
                    06  Y06-TRENNUM   PIC X(2).
                    06  Y06-TYYKYMD   PIC X(9).
                    06  Y06-TMARK     PIC X(4).
                    06  Y06-TSYORIYMD PIC X(9).
                    06  Y06-TALLCNT   PIC X(10).
                    06  Y06-TCNT      PIC X(10).
                    06  Y06-TERRCNT   PIC X(10). 
                  05  Y06-SELECT  PIC X       OCCURS  10 TIMES.
              03  Y06-MSG-STYLE   PIC X(20).
              03  Y06-MSG         PIC X(100).

