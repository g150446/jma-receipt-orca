        02  W51.
            03  W51-TITLE   PIC X(256).
            03  W51-POPUP-SUMMARY   PIC X(256).
            03  W51-POPUP-BODY  PIC X(256).
            03  W51-POPUP-ICON  PIC X(256).
            03  W51-POPUP-TIMEOUT   PIC S9(9)   BINARY.
     *******02  W51-FIXED1.
              03  W51-B01.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B02.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B12.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B11.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B10.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B09.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B08.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B07.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B06.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B05.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B04.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-B03.
                04  W51-STATE   PIC S9(9)   BINARY.
              03  W51-DENSHISHOHO-TITLE.
                04  W51-DENSHISHOHO-TITLE-VISIBLE  PIC X(01).
              03  W51-ONSHIKAKU-COMB.
                04  W51-ONSHIKAKU-COUNT   PIC S9(9)   BINARY.
                04  W51-ONSHIKAKU-ITEM
                        PIC X(62)   OCCURS  5 TIMES.
                04  W51-ONSHIKAKU.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-POP-TIME.
                04  W51-NUMDATA PIC S9(3).
              03  W51-POP-COND-TIME.
                04  W51-NUMDATA PIC S9(3).
              03  W51-POP-KGMN-TIME.
                04  W51-NUMDATA PIC S9(3).
              03  W51-ONSHIYAKUZAI-COMB.
                04  W51-ONSHIYAKUZAI-COUNT   PIC S9(9)   BINARY.
                04  W51-ONSHIYAKUZAI-ITEM
                        PIC X(62)   OCCURS  5 TIMES.
                04  W51-ONSHIYAKUZAI.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-ONSHIKENSIN-COMB.
                04  W51-ONSHIKENSIN-COUNT   PIC S9(9)   BINARY.
                04  W51-ONSHIKENSIN-ITEM
                        PIC X(62)   OCCURS  5 TIMES.
                04  W51-ONSHIKENSIN.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-ONSHISINRYO-COMB.
                04  W51-ONSHISINRYO-COUNT   PIC S9(9)   BINARY.
                04  W51-ONSHISINRYO-ITEM
                        PIC X(62)   OCCURS  5 TIMES.
                04  W51-ONSHISINRYO.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-ONSHIYKZSINRYO-COMB.
                04  W51-ONSHIYKZSINRYO-COUNT   PIC S9(9)   BINARY.
                04  W51-ONSHIYKZSINRYO-ITEM
                        PIC X(62)   OCCURS  5 TIMES.
                04  W51-ONSHIYKZSINRYO.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-DENSHISHOHO-COMB.
                04  W51-DENSHISHOHO-VISIBLE  PIC X(01).
                04  W51-DENSHISHOHO-COUNT   PIC S9(9)   BINARY.
                04  W51-DENSHISHOHO-ITEM
                        PIC X(62)   OCCURS  5 TIMES.
                04  W51-DENSHISHOHO.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-YOHOCHGKBN-TITLE.
                04  W51-YOHOCHGKBN-TITLE-VISIBLE PIC X.
              03  W51-YOHOCHGKBN-COMB.
                04  W51-YOHOCHGKBN-VISIBLE PIC X.
                04  W51-YOHOCHGKBN-COUNT   PIC S9(9)   BINARY.
                04  W51-YOHOCHGKBN-ITEM
                        PIC X(32)   OCCURS  5 TIMES.
                04  W51-YOHOCHGKBN.
                  05  W51-TEXTDATA    PIC X(32).
              03  W51-YOUHOUNAME-TITLE.
                04  W51-YOUHOUNAME-TITLE-VISIBLE PIC X.
              03  W51-YOUHOUNAME-COMB.
                04  W51-YOUHOUNAME-VISIBLE PIC X.
                04  W51-YOUHOUNAME-COUNT   PIC S9(9)   BINARY.
                04  W51-YOUHOUNAME-ITEM
                        PIC X(32)   OCCURS  5 TIMES.
                04  W51-YOUHOUNAME.
                  05  W51-TEXTDATA    PIC X(32).
              03  W51-CSVFILEDIR-TITLE1.
                04  W51-CSVFILEDIR-T1-VISIBLE PIC X.
              03  W51-CSVFILEDIR-TITLE2.
                04  W51-CSVFILEDIR-T2-VISIBLE PIC X.
              03  W51-CSVFILEDIR-TITLE3.
                04  W51-CSVFILEDIR-T3-VISIBLE PIC X.
              03  W51-CSVFILEDIR.
                04  W51-CSVFILEDIR-VISIBLE PIC X.
                04  W51-CSVFILEDIR-DATA    PIC X(200).
              03  W51-HOUMON-COMB.
                04  W51-HOUMON-COUNT   PIC S9(9)   BINARY.
                04  W51-HOUMON-ITEM
                        PIC X(62)   OCCURS  2 TIMES.
                04  W51-HOUMON.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-ONLINE-COMB.
                04  W51-ONLINE-COUNT   PIC S9(9)   BINARY.
                04  W51-ONLINE-ITEM
                        PIC X(62)   OCCURS  2 TIMES.
                04  W51-ONLINE.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-KOFUTNK-COMB.
                04  W51-KOFUTNK-COUNT   PIC S9(9)   BINARY.
                04  W51-KOFUTNK-ITEM
                        PIC X(62)   OCCURS  2 TIMES.
                04  W51-KOFUTNK.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-FUJYO-COMB.
                04  W51-FUJYO-COUNT   PIC S9(9)   BINARY.
                04  W51-FUJYO-ITEM
                        PIC X(62)   OCCURS  2 TIMES.
                04  W51-FUJYO.
                  05  W51-TEXTDATA    PIC X(62).
              03  W51-HOSPCDF.
                04  W51-HOSPCDF-DATA    PIC X(10).
