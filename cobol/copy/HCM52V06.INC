        01  HCM52V06.
            02  HCM52V06-HEAD.
              03  HCM52V06-NYUGAI
                        PIC X(8).
              03  HCM52V06-HSRYYMD
                        PIC X(64).
              03  HCM52V06-HSRYKA
                        PIC X(10).
              03  HCM52V06-SAKYMD
                        PIC X(22).
              03  HCM52V06-PAGE
                        PIC X(5).
              03  HCM52V06-SYOSINMSG
                        PIC X(10).
              03  HCM52V06-SAISINMSG
                        PIC X(10).
              03  HCM52V06-SYOSAIMSG
                        PIC X(10).
              03  HCM52V06-SKYTTLMSG
                        PIC X(12)       OCCURS 15 TIMES.
            02  HCM52V06-SKYTBL
                                        OCCURS 11 TIMES.
              03  HCM52V06-GOKEI
                        PIC X(12).
              03  HCM52V06-SKYMONEY-G.
                04  HCM52V06-SKYMONEY
                        PIC X(12)       OCCURS 10 TIMES.
            02  HCM52V06-JIHITBL.
              03  HCM52V06-TTLJIHIMSG
                        PIC X(10).
              03  HCM52V06-TTLJIHI
                        PIC X(12).
              03  HCM52V06-JIHIMSG
                        PIC X(10)       OCCURS 11 TIMES.
              03  HCM52V06-JIHI
                        PIC X(12)       OCCURS 11 TIMES.
            02  HCM52V06-NYUKINTBL.
              03  HCM52V06-TTLHOHOMSG
                        PIC X(10).
              03  HCM52V06-TTLNYUKIN
                        PIC X(12).
              03  HCM52V06-HOHOMSG
                        PIC X(10)       OCCURS 15 TIMES.
              03  HCM52V06-NYUKIN
                        PIC X(12)       OCCURS 15 TIMES.
            02  HCM52V06-KONNYUKINMSG
                        PIC X(20).
            02  HCM52V06-KONMISYUMSG
                        PIC X(20).
            02  HCM52V06-LOPTION
                        PIC X(500).
            02  HCM52V06-TTLTENSU
                        PIC X(12).
            02  HCM52V06-TENSU
                        PIC X(12)       OCCURS 10 TIMES.
