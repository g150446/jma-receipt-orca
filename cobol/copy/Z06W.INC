      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�?JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  Z06W.
     *******02  Z06W-FIXED1.
              03  Z06W-SRYCDH.
                04  Z06W-VALUE   PIC X(9).
              03  Z06W-YUKOSTYMDH.
                04  Z06W-VALUE   PIC X(9).
              03  Z06W-YUKOEDYMDH.
                04  Z06W-VALUE   PIC X(9).
              03  Z06W-NAMEH.
                04  Z06W-VALUE   PIC X(100).
              03  Z06W-NAME.
                04  Z06W-VALUE   PIC X(100).
              03  Z06W-KANANAME.
                04  Z06W-VALUE   PIC X(100).
              03  Z06W-YUKOSTYMD.
                04  Z06W-VALUE   PIC X(10).
              03  Z06W-YUKOEDYMD.
                04  Z06W-VALUE   PIC X(10).
              03  Z06W-FORMALNAME.
                04  Z06W-VALUE   PIC X(200).
              03  Z06W-JITEN1COMB.
                04  Z06W-JITEN1.
                  05  Z06W-VALUE   PIC X(12).
                04  Z06W-JITEN1-COUNT   PIC S9(9)   BINARY.
                04  Z06W-JITEN1-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06W-JITEN2COMB.
                04  Z06W-JITEN2.
                  05  Z06W-VALUE   PIC X(12).
                04  Z06W-JITEN2-COUNT   PIC S9(9)   BINARY.
                04  Z06W-JITEN2-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06W-JITEN3COMB.
                04  Z06W-JITEN3.
                  05  Z06W-VALUE   PIC X(12).
                04  Z06W-JITEN3-COUNT   PIC S9(9)   BINARY.
                04  Z06W-JITEN3-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06W-JITEN4COMB.
                04  Z06W-JITEN4.
                  05  Z06W-VALUE   PIC X(12).
                04  Z06W-JITEN4-COUNT   PIC S9(9)   BINARY.
                04  Z06W-JITEN4-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06W-JITEN5COMB.
                04  Z06W-JITEN5.
                  05  Z06W-VALUE   PIC X(12).
                04  Z06W-JITEN5-COUNT   PIC S9(9)   BINARY.
                04  Z06W-JITEN5-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06W-DIGIT1.
                04  Z06W-VALUE   PIC S9(3).
              03  Z06W-DIGIT2.
                04  Z06W-VALUE   PIC S9(3).
              03  Z06W-DIGIT3.
                04  Z06W-VALUE   PIC S9(3).
              03  Z06W-DIGIT4.
                04  Z06W-VALUE   PIC S9(3).
              03  Z06W-DIGIT5.
                04  Z06W-VALUE   PIC S9(3).
              03  Z06W-COMMENTKBN-COMB.
                04  Z06W-COMMENTKBN.
                  05  Z06W-VALUE   PIC X(30).
                04  Z06W-COMMENTKBN-COUNT   PIC S9(9)   BINARY.
                04  Z06W-COMMENTKBN-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
              03  Z06W-COPYCD-G.
                04  Z06W-COPYCD-STATE   PIC S9(9)   BINARY.
                04  Z06W-COPYCD         PIC X(9).
              03  Z06W-B06.
                04  Z06W-B06-STATE   PIC S9(9)   BINARY.
                04  Z06W-B06-LABEL   PIC X(10).
              03  Z06W-B09.
                04  Z06W-B09-STATE   PIC S9(9)   BINARY.
                04  Z06W-B09-LABEL   PIC X(10).
              03  Z06W-B10.
                04  Z06W-B10-STATE   PIC S9(9)   BINARY.
                04  Z06W-B10-LABEL   PIC X(10).
              03  Z06W-B03.
                04  Z06W-B03-STATE   PIC S9(9)   BINARY.
              03  Z06W-B04.
                04  Z06W-B04-STATE   PIC S9(9)   BINARY.
                04  Z06W-B04-LABEL   PIC X(10).
              03  Z06W-B07.
                04  Z06W-B07-STATE   PIC S9(9)   BINARY.
                04  Z06W-B07-LABEL   PIC X(10).
              03  Z06W-B08.
                04  Z06W-B08-STATE   PIC S9(9)   BINARY.
                04  Z06W-B08-LABEL   PIC X(10).
              04  Z06W-MIDASHI.
                05  Z06W-MIDASHI-STYLE  PIC X(20).
                05  Z06W-MIDASHI-DATA   PIC X(80).
              03  Z06W-YOHCODE.
                04  Z06W-TEXTDATA    PIC X(16).
              03  Z06W-YOHNAME.
                04  Z06W-TEXTDATA    PIC X(200).
              03  Z06W-YOUSTYMD.
                04  Z06W-TEXTDATA    PIC X(10).
              03  Z06W-YOUEDYMD.
                04  Z06W-TEXTDATA    PIC X(10).
              03  Z06W-YOHKANA.
                04  Z06W-TEXTDATA    PIC X(480).
              03  Z06W-YOUFLG-COB.
                04  Z06W-YOUFLG-COUNT   PIC S9(9)   BINARY.
                04  Z06W-YOUFLG-ITEM
                        PIC X(10)   OCCURS  2 TIMES.
                04  Z06W-YOUFLG.
                  05  Z06W-TEXTDATA    PIC X(10).
              03  Z06W-BASIC-COB.
                04  Z06W-BASIC-COUNT   PIC S9(9)   BINARY.
                04  Z06W-BASIC-ITEM
                        PIC X(10)   OCCURS  10 TIMES.
                04  Z06W-BASIC.
                  05  Z06W-TEXTDATA    PIC X(10).
              03  Z06W-DETAIL-COB.
                04  Z06W-DETAIL-COUNT   PIC S9(9)   BINARY.
                04  Z06W-DETAIL-ITEM
                        PIC X(32)   OCCURS  30 TIMES.
                04  Z06W-DETAIL.
                  05  Z06W-TEXTDATA    PIC X(32).
              03  Z06W-TIMING-COB.
                04  Z06W-TIMING-COUNT   PIC S9(9)   BINARY.
                04  Z06W-TIMING-ITEM
                        PIC X(62)   OCCURS  30 TIMES.
                04  Z06W-TIMING.
                  05  Z06W-TEXTDATA    PIC X(62).
              03  Z06W-CODEC-COB.
                04  Z06W-CODEC-COUNT   PIC S9(9)   BINARY.
                04  Z06W-CODEC-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  Z06W-CODEC.
                  05  Z06W-TEXTDATA    PIC X(30).
              03  Z06W-LABEL3.
                04  Z06W-LABEL3-VISIBLE    PIC X.
                04  Z06W-LABEL3-TEXTDATA   PIC X(20).
              03  Z06W-YOUFLG3-COB.
                04  Z06W-YOUFLG3-VISIBLE    PIC X.
                04  Z06W-YOUFLG3-COUNT  PIC S9(9)   BINARY.
                04  Z06W-YOUFLG3-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  Z06W-YOUFLG3.
                  05  Z06W-TEXTDATA   PIC X(30).
              03  Z06W-REFER-NO.
                04  Z06W-TEXTDATA    PIC X(4).
              03  Z06W-TONPUKU-COB.
                04  Z06W-TONPUKU-COUNT   PIC S9(9)   BINARY.
                04  Z06W-TONPUKU-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  Z06W-TONPUKU.
                  05  Z06W-TEXTDATA    PIC X(30).
              03  Z06W-TOYTIMNG-COB.
                04  Z06W-TOYTIMNG-COUNT   PIC S9(9)   BINARY.
                04  Z06W-TOYTIMNG-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  Z06W-TOYTIMNG.
                  05  Z06W-TEXTDATA    PIC X(30).
              03  Z06W-TOYTIME-COB.
                04  Z06W-TOYTIME-COUNT   PIC S9(9)   BINARY.
                04  Z06W-TOYTIME-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  Z06W-TOYTIME.
                  05  Z06W-TEXTDATA    PIC X(30).
              03  Z06W-INTERVAL-COB.
                04  Z06W-INTERVAL-COUNT   PIC S9(9)   BINARY.
                04  Z06W-INTERVAL-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  Z06W-INTERVAL.
                  05  Z06W-TEXTDATA    PIC X(30).
              03  Z06W-BUI-COB.
                04  Z06W-BUI-COUNT   PIC S9(9)   BINARY.
                04  Z06W-BUI-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
                04  Z06W-BUI.
                  05  Z06W-TEXTDATA    PIC X(30).

