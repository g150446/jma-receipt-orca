        01  HCM3CV04.
            02  HCM3CV04-PRTYM
                        PIC X(16).
            02  HCM3CV04-SEIYMD
                        PIC X(22).
            02  HCM3CV04-HOSPCD
                        PIC X(20).
            02  HCM3CV04-ADRS
                        PIC X(100).
            02  HCM3CV04-HOSPNAME
                        PIC X(100).
            02  HCM3CV04-KAISETUNAME
                        PIC X(60).
            02  HCM3CV04-IHO-TBL1
                                        OCCURS 11 TIMES.
              03  HCM3CV04-KENSU1
                        PIC X(6).
              03  HCM3CV04-NISSU1
                        PIC X(6).
              03  HCM3CV04-TENSU1
                        PIC X(10).
              03  HCM3CV04-ITBFTN1
                        PIC X(10).
            02  HCM3CV04-IHO-TBL2
                                        OCCURS 9 TIMES.
              03  HCM3CV04-KENSU2
                        PIC X(6).
              03  HCM3CV04-NISSU2
                        PIC X(6).
              03  HCM3CV04-TENSU2
                        PIC X(10).
              03  HCM3CV04-ITBFTN2
                        PIC X(10).
            02  HCM3CV04-IHO-TBL3
                                        OCCURS 12 TIMES.
              03  HCM3CV04-KENSU3
                        PIC X(6).
              03  HCM3CV04-NISSU3
                        PIC X(6).
              03  HCM3CV04-TENSU3
                        PIC X(10).
              03  HCM3CV04-ITBFTN3
                        PIC X(10).
            02  HCM3CV04-IHO-TBL4
                                        OCCURS 9 TIMES.
              03  HCM3CV04-KENSU4
                        PIC X(6).
              03  HCM3CV04-NISSU4
                        PIC X(6).
              03  HCM3CV04-ITBFTN4
                        PIC X(10).
              03  HCM3CV04-TENSU4
                        PIC X(10).
            02  HCM3CV04-IHO-TBL5
                                        OCCURS 9 TIMES.
              03  HCM3CV04-KENSU5
                        PIC X(6).
              03  HCM3CV04-NISSU5
                        PIC X(6).
              03  HCM3CV04-ITBFTN5
                        PIC X(10).
              03  HCM3CV04-TENSU5
                        PIC X(10).
            02  HCM3CV04-KENSUG
                        PIC X(6).
            02  HCM3CV04-TITLE
                        PIC X(50).
            02  HCM3CV04-TITLE1
                        PIC X(10).
            02  HCM3CV04-SRYKA
                        PIC X(40).
            02  HCM3CV04-MARUCHO
                        PIC X(2)        OCCURS 2 TIMES.
            02  HCM3CV04-SEIDONAME
                        PIC X(8)        OCCURS 5 TIMES.
