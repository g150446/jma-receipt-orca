        01  HCN68R06.
            02  HCN68R06-SRYYM
                        PIC X(16).
            02  HCN68R06-PTNUM
                        PIC X(20).
            02  HCN68R06-NAME
                        PIC X(50).
            02  HCN68R06-BIRTHDAY
                        PIC X(22).
            02  HCN68R06-SEX
                        PIC X(2).
            02  HCN68R06-LBLSAKYMD
                        PIC X(10).
            02  HCN68R06-SAKYMD
                        PIC X(22).
            02  HCN68R06-MEDKBN-G
                                        OCCURS 68 TIMES.
              03  HCN68R06-MEDKBN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
              03  HCN68R06-MEDKBN-ALL
                        PIC X(2).
            02  HCN68R06-MEDKBN-NAME-G
                                        OCCURS 68 TIMES.
              03  HCN68R06-MEDKBN-NAME
                        PIC X(80).
