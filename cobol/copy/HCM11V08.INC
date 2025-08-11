        01  HCM11V08.
            02  HCM11V08-TEISYUTUSAKI
                        PIC X(6).
            02  HCM11V08-PTNUM
                        PIC X(20).
            02  HCM11V08-SRYGG
                        PIC X(4).
            02  HCM11V08-SRYY
                        PIC X(2).
            02  HCM11V08-SRYM
                        PIC X(2).
            02  HCM11V08-PREFNUM
                        PIC X(2).
            02  HCM11V08-HOSPCDN
                        PIC X(20).
            02  HCM11V08-KHNTEN2
                        PIC X(7)        OCCURS 2 TIMES.
            02  HCM11V08-KHNTEN1
                        PIC X(7).
            02  HCM11V08-SRYKBN
                        PIC X(2)        OCCURS 35 TIMES.
            02  HCM11V08-TEKIYO
                        PIC X(52)       OCCURS 35 TIMES.
            02  HCM11V08-KOUTEN
                        PIC X(7)        OCCURS 35 TIMES.
            02  HCM11V08-TOTALTEN
                        PIC X(10)       OCCURS 3 TIMES.
            02  HCM11V08-FTNMONEY
                        PIC X(8)        OCCURS 3 TIMES.
            02  HCM11V08-OCR1
                        PIC X(67).
            02  HCM11V08-OCR2
                        PIC X(67).
            02  HCM11V08-OCR3
                        PIC X(67).
            02  HCM11V08-RJNFTNJANUM.
              03  HCM11V08-RJNFTNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCM11V08-KOH-TBL1.
              03  HCM11V08-KOHFTNJANUM1.
                04  HCM11V08-KOHFTNJANUM1-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCM11V08-KOHFTNJANUM1N
                        PIC X(20).
              03  HCM11V08-KOHJKYSNUM1.
                04  HCM11V08-KOHJKYSNUM1-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCM11V08-KOHJKYSNUM1N
                        PIC X(20).
            02  HCM11V08-KOH-TBL2.
              03  HCM11V08-KOHFTNJANUM2.
                04  HCM11V08-KOHFTNJANUM2-S
                        PIC X(1)        OCCURS 8 TIMES.
              03  HCM11V08-KOHFTNJANUM2N
                        PIC X(20).
              03  HCM11V08-KOHJKYSNUM2.
                04  HCM11V08-KOHJKYSNUM2-S
                        PIC X(1)        OCCURS 7 TIMES.
              03  HCM11V08-KOHJKYSNUM2N
                        PIC X(20).
            02  HCM11V08-HKNJANUM.
              03  HCM11V08-HKNJANUM-S
                        PIC X(1)        OCCURS 8 TIMES.
            02  HCM11V08-KYURATE
                        PIC X(8).
            02  HCM11V08-RJNJKYSNUM.
              03  HCM11V08-RJNJKYSNUM-S
                        PIC X(1)        OCCURS 7 TIMES.
            02  HCM11V08-HKNSBT1
                        PIC X(6).
            02  HCM11V08-HKNSBT2
                        PIC X(6).
            02  HCM11V08-HONKZK
                        PIC X(10).
            02  HCM11V08-KIGO
                        PIC X(34).
            02  HCM11V08-NUM
                        PIC X(34).
            02  HCM11V08-KANANAME
                        PIC X(40).
            02  HCM11V08-KANANAME2
                        PIC X(50).
            02  HCM11V08-SEXNUM
                        PIC X(2).
            02  HCM11V08-BIRTHDAY
                        PIC X(22).
            02  HCM11V08-JIYU
                        PIC X(20).
            02  HCM11V08-SEXN
                        PIC X(2).
            02  HCM11V08-TOKKI
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCM11V08-HOSPTEL
                        PIC X(15).
            02  HCM11V08-SRYKA
                        PIC X(30).
            02  HCM11V08-BEDSU
                        PIC X(4).
            02  HCM11V08-BYOMEI
                        PIC X(54)       OCCURS 5 TIMES.
            02  HCM11V08-SRYYMD
                        PIC X(26)       OCCURS 5 TIMES.
            02  HCM11V08-TENKI
                        PIC X(10)       OCCURS 5 TIMES.
            02  HCM11V08-JINISSU
                        PIC X(2).
            02  HCM11V08-JINISSU1
                        PIC X(2).
            02  HCM11V08-JINISSU2
                        PIC X(2).
            02  HCM11V08-TENSU
                        PIC X(7)        OCCURS 35 TIMES.
            02  HCM11V08-KAISU
                        PIC X(3)        OCCURS 35 TIMES.
            02  HCM11V08-KHNTEN
                        PIC X(7)        OCCURS 5 TIMES.
            02  HCM11V08-HOSPNAME
                        PIC X(80).
            02  HCM11V08-HOSPADRS
                        PIC X(100).
            02  HCM11V08-NAME
                        PIC X(50).
            02  HCM11V08-HEAD1-G.
              03  HCM11V08-HEAD1
                        PIC X(46)       OCCURS 2 TIMES.
            02  HCM11V08-HEAD2-G.
              03  HCM11V08-HEAD2
                        PIC X(2)        OCCURS 7 TIMES.
            02  HCM11V08-HEAD3-G.
              03  HCM11V08-HEAD3
                        PIC X(50)       OCCURS 2 TIMES.
            02  HCM11V08-HEAD4-G.
              03  HCM11V08-HEAD4
                        PIC X(62)       OCCURS 2 TIMES.
            02  HCM11V08-BODY
                        PIC X(52).
            02  HCM11V08-FOOT-G.
              03  HCM11V08-FOOT
                        PIC X(46)       OCCURS 4 TIMES.
            02  HCM11V08-SHOSHINTIME
                        PIC X(10).
            02  HCM11V08-TENKIYMD
                        PIC X(12)       OCCURS 5 TIMES.
            02  HCM11V08-FTNMONEYSAI
                        PIC X(11).
            02  HCM11V08-FTNMONEYSAI2
                        PIC X(11).
            02  HCM11V08-RENNUM
                        PIC X(7).
            02  HCM11V08-SRYKA2
                        PIC X(60).
            02  HCM11V08-KHNTEN3
                        PIC X(7).
            02  HCM11V08-SONOTA
                        PIC X(10).
            02  HCM11V08-PAGE
                        PIC X(5).
            02  HCM11V08-MOJI1
                        PIC X(10).
            02  HCM11V08-MOJI2
                        PIC X(10).
            02  HCM11V08-TOTALTEN1
                        PIC X(10)       OCCURS 4 TIMES.
            02  HCM11V08-FTNMONEY1
                        PIC X(9)        OCCURS 4 TIMES.
            02  HCM11V08-FTNMONEY-GEN
                        PIC X(8).
            02  HCM11V08-GENMEN
                        PIC X(10).
            02  HCM11V08-GENMENKIN
                        PIC X(7).
            02  HCM11V08-GENMEN2
                        PIC X(2).
            02  HCM11V08-LABEL
                        PIC X(14).
            02  HCM11V08-HEAD5
                        PIC X(20).
            02  HCM11V08-DRNAME
                        PIC X(20).
            02  HCM11V08-LOPTION
                        PIC X(100).
            02  HCM11V08-GENMEN3
                        PIC X(20).
            02  HCM11V08-KIGO2
                        PIC X(34).
            02  HCM11V08-NUM2
                        PIC X(34).
            02  HCM11V08-EDABANNAME
                        PIC X(8).
            02  HCM11V08-EDABAN
                        PIC X(2).
