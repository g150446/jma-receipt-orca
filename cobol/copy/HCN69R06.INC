        01  HCN69R06.
            02  HCN69R06-SRYYM
                        PIC X(16).
            02  HCN69R06-PTNUM
                        PIC X(20).
            02  HCN69R06-NAME
                        PIC X(50).
            02  HCN69R06-BIRTHDAY
                        PIC X(22).
            02  HCN69R06-SEX
                        PIC X(2).
            02  HCN69R06-SAKYMD
                        PIC X(22).
            02  HCN69R06-LBLSAKYMD
                        PIC X(10).
            02  HCN69R06-ADLTEN-G
                                        OCCURS 4 TIMES.
              03  HCN69R06-ADLTEN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69R06-TTL-ADLTEN-G.
              03  HCN69R06-TTL-ADLTEN-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69R06-HPTSTS-G
                                        OCCURS 30 TIMES.
              03  HCN69R06-HPTSTS-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69R06-CPTSTS-G
                                        OCCURS 5 TIMES.
              03  HCN69R06-CPTSTS-DAY
                        PIC X(2)        OCCURS 31 TIMES.
            02  HCN69R06-LOPTION
                        PIC X(100).
            02  HCN69R06-DESIGNR-G.
              03  HCN69R06-DESIGNR-DAY
                        PIC X(2)        OCCURS 31 TIMES.
