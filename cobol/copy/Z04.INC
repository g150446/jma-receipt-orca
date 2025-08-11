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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  Z04.
            03  Z04-TITLE   PIC X(100).
            03  Z04-POPUP-SUMMARY   PIC X(100).
            03  Z04-POPUP-BODY  PIC X(100).
            03  Z04-POPUP-ICON  PIC X(100).
            03  Z04-POPUP-TIMEOUT   PIC S9(9)   BINARY.
      ****  02  Z04-FIXED6.
              03  Z04-SRYCDH.
                04  Z04-TEXTDATA    PIC X(9).
              03  Z04-NAMEH.
                04  Z04-TEXTDATA    PIC X(100).
              03  Z04-YUKOSTYMDH.
                04  Z04-TEXTDATA    PIC X(9).
              03  Z04-YUKOEDYMDH.
                04  Z04-TEXTDATA    PIC X(9).
              03  Z04-MIDASHI.
                04  Z04-MIDASHI-STYLE   PIC X(20).
                04  Z04-MIDASHI-DATA    PIC X(20).
              03  Z04-B01.
                04  Z04-STATE   PIC S9(9)   BINARY.
              03  Z04-B02.
                04  Z04-B02-STATE   PIC S9(9)   BINARY.
              03  Z04-B03.
                04  Z04-B03-STATE   PIC S9(9)   BINARY.
              03  Z04-B04.
                04  Z04-STATE   PIC S9(9)   BINARY.
              03  Z04-B05.
                04  Z04-STATE   PIC S9(9)   BINARY.
              03  Z04-B06.
                04  Z04-STATE   PIC S9(9)   BINARY.
              03  Z04-B07.
                04  Z04-STATE   PIC S9(9)   BINARY.
              03  Z04-B08.
                04  Z04-STATE   PIC S9(9)   BINARY.
              03  Z04-B09.
                04  Z04-STATE   PIC S9(9)   BINARY.
              03  Z04-B10.
                04  Z04-STATE   PIC S9(9)   BINARY.
              03  Z04-B11.
                04  Z04-B11-STATE   PIC S9(9)   BINARY.
              03  Z04-B12.
                04  Z04-B12-STATE   PIC S9(9)   BINARY.
              03  Z04-NOTEBOOK1.
                04  Z04-PAGENO  PIC S9(9)   BINARY.
                04  Z04-TG1.
                  05  Z04-KANANAME.
                    06  Z04-TEXTDATA    PIC X(100).
                  05  Z04-SHONAME.
                    06  Z04-TEXTDATA    PIC X(200).
                  05  Z04-NAME.
                    06  Z04-TEXTDATA    PIC X(100).
                  05  Z04-TEN.
                    06  Z04-NUMDATA PIC S9(9)V9(2).
                  05  Z04-OLDTEN.
                    06  Z04-NUMDATA PIC S9(9)V9(2).
                  05  Z04-YAKKAKJNCD.
                    06  Z04-TEXTDATA    PIC X(12).
                  05  Z04-SYOMEIKANREN.
                    06  Z04-TEXTDATA    PIC X(9).
                  05  Z04-KEIKASOCHIYMD.
                    06  Z04-TEXTDATA    PIC X(10).
                  05  Z04-YUKOSTYMD.
                    06  Z04-TEXTDATA    PIC X(10).
                  05  Z04-YUKOEDYMD.
                    06  Z04-TEXTDATA    PIC X(10).
                  05  Z04-CSYYOURYO.
                    06  Z04-NUMDATA PIC S9(5).
                  05  Z04-KOUHYOJYUNNUM.
                    06  Z04-NUMDATA PIC S9(9).
                  05  Z04-LONGTOYOKBN.
                    06  Z04-NUMDATA PIC S9(1).
                  05  Z04-SYUSAISKB.
                    06  Z04-NUMDATA PIC S9(1).
                  05  Z04-FORMALNAME.
                    06  Z04-TEXTDATA    PIC X(200).
                  05  Z04-KANZANCHI.
                    06  Z04-NUMDATA PIC S9(5)V9(5).
                  05  Z04-YAKKAKJNCDR.
                    06  Z04-TEXTDATA    PIC X(12).
                  05  Z04-CHNGCD.
                    06  Z04-TEXTDATA    PIC X(9).
                  05  Z04-CHNGENAME.
                    06  Z04-TEXTDATA    PIC X(100).
                  05  Z04-CHNGCDU.
                    06  Z04-TEXTDATA    PIC X(9).
                  05  Z04-CHNGENAMEU.
                    06  Z04-TEXTDATA    PIC X(100).
                  05  Z04-CHOKINISSU.
                    06  Z04-NUMDATA PIC S9(3).
                  05  Z04-CHOKIKIGEN.
                    06  Z04-TEXTDATA    PIC X(10).
                  05  Z04-CHOKIERR.
                    06  Z04-NUMDATA PIC S9(1).
                  05  Z04-TOUYO-T   OCCURS   10.
                    06  Z04-KGNAGE.
                      07  Z04-VALUE   PIC X(2).
                    06  Z04-JGNAGE.
                      07  Z04-VALUE   PIC X(2).
                    06  Z04-TOUYORYO1.
                      07  Z04-VALUE   PIC S9(5)V9(3).
                    06  Z04-TOUYORYO2.
                      07  Z04-VALUE   PIC S9(5)V9(3).
                  05  Z04-TOUYOERR.
                    06  Z04-NUMDATA PIC S9(1).
                  05  Z04-YKZKBN-COMBO.
                    06  Z04-YKZKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-YKZKBN-ITEM
                            PIC X(16)   OCCURS  6 TIMES.
                    06  Z04-YKZKBN.
                      07  Z04-TEXTDATA    PIC X(16).
                  05  Z04-SINKEIHAKAIKBN-COMBO.
                    06  Z04-SINKEIHAKAIKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-SINKEIHAKAIKBN-ITEM
                            PIC X(18)   OCCURS  2 TIMES.
                    06  Z04-SINKEIHAKAIKBN.
                      07  Z04-TEXTDATA    PIC X(18).
                  05  Z04-MADOKUKBN-COMBO.
                    06  Z04-MADOKUKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-MADOKUKBN-ITEM
                            PIC X(16)   OCCURS  5 TIMES.
                    06  Z04-MADOKUKBN.
                      07  Z04-TEXTDATA    PIC X(16).
                  05  Z04-TENSIKIBETU-COMBO.
                    06  Z04-TENSIKIBETU-COUNT   PIC S9(9)   BINARY.
                    06  Z04-TENSIKIBETU-ITEM
                            PIC X(42)   OCCURS  9 TIMES.
                    06  Z04-TENSIKIBETU.
                      07  Z04-TEXTDATA    PIC X(42).
                  05  Z04-IPNKANZANCHI.
                    06  Z04-NUMDATA PIC S9(5)V9(5).
                  05  Z04-OLDTENSKB-COMBO.
                    06  Z04-OLDTENSKB-COUNT   PIC S9(9)   BINARY.
                    06  Z04-OLDTENSKB-ITEM
                            PIC X(42)   OCCURS  964 TIMES.
                    06  Z04-OLDTENSKB.
                      07  Z04-TEXTDATA    PIC X(42).
                  05  Z04-SEIBUTUGAKUKBN-COMBO.
                    06  Z04-SEIBUTUGAKUKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-SEIBUTUGAKUKBN-ITEM
                            PIC X(32)   OCCURS  2 TIMES.
                    06  Z04-SEIBUTUGAKUKBN.
                      07  Z04-TEXTDATA    PIC X(32).
                  05  Z04-ZOEIZAIKBN-COMBO.
                    06  Z04-ZOEIZAIKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-ZOEIZAIKBN-ITEM
                            PIC X(26)   OCCURS  3 TIMES.
                    06  Z04-ZOEIZAIKBN.
                      07  Z04-TEXTDATA    PIC X(26).
                  05  Z04-ZAIGATAKBN-COMBO.
                    06  Z04-ZAIGATAKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-ZAIGATAKBN-ITEM
                            PIC X(36)   OCCURS  4 TIMES.
                    06  Z04-ZAIGATAKBN.
                      07  Z04-TEXTDATA    PIC X(36).
                  05  Z04-KOUHATUKBN-COMB.
                    06  Z04-KOUHATUKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-KOUHATUKBN-ITEM
                            PIC X(34)   OCCURS  9 TIMES.
                    06  Z04-KOUHATUKBN.
                      07  Z04-TEXTDATA    PIC X(34).
                  05  Z04-ALLSURYOKBN-COMBO.
                    06  Z04-ALLSURYOKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-ALLSURYOKBN-ITEM
                            PIC X(14)   OCCURS  2 TIMES.
                    06  Z04-ALLSURYOKBN.
                      07  Z04-TEXTDATA    PIC X(14).
                  05  Z04-KANZANTANICD-COMBO.
                    06  Z04-KANZANTANICD-COUNT   PIC S9(9)   BINARY.
                    06  Z04-KANZANTANICD-ITEM
                            PIC X(28)   OCCURS  200 TIMES.
                    06  Z04-KANZANTANICD.
                      07  Z04-TEXTDATA    PIC X(28).
                  05  Z04-IPNKISAIKBN-COMB.
                    06  Z04-IPNKISAIKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-IPNKISAIKBN-ITEM
                            PIC X(43)   OCCURS  5 TIMES.
                    06  Z04-IPNKISAIKBN.
                      07  Z04-TEXTDATA    PIC X(43).
                  05  Z04-BUNRUIKBN-COMBO.
                    06  Z04-BUNRUIKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-BUNRUIKBN-ITEM
                            PIC X(43)   OCCURS  20 TIMES.
                    06  Z04-BUNRUIKBN.
                      07  Z04-TEXTDATA    PIC X(43).
                  05  Z04-IPNTANICD-COMB.
                    06  Z04-IPNTANICD-COUNT   PIC S9(9)   BINARY.
                    06  Z04-IPNTANICD-ITEM
                            PIC X(28)   OCCURS  200 TIMES.
                    06  Z04-IPNTANICD.
                      07  Z04-TEXTDATA    PIC X(28).
                  05  Z04-NAIFUKUKBN-COMB.
                    06  Z04-NAIFUKUKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-NAIFUKUKBN-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                    06  Z04-NAIFUKUKBN.
                      07  Z04-TEXTDATA    PIC X(30).
                  05  Z04-SOURYOKBN-COMB.
                    06  Z04-SOURYOKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-SOURYOKBN-ITEM
                            PIC X(14)   OCCURS  5 TIMES.
                    06  Z04-SOURYOKBN.
                      07  Z04-TEXTDATA    PIC X(14).
                  05  Z04-TUKIJGNKAISU.
                    06  Z04-NUMDATA PIC S9(3).
                  05  Z04-TUKIJGNTOUYORYO.
                    06  Z04-NUMDATA PIC S9(5)V9(3).
                  05  Z04-TANICD-COMBO.
                    06  Z04-TANICD-COUNT   PIC S9(9)   BINARY.
                    06  Z04-TANICD-ITEM
                            PIC X(28)   OCCURS  200 TIMES.
                    06  Z04-TANICD.
                      07  Z04-TEXTDATA    PIC X(28).
                  05  Z04-GENERICHEN.
                    06  Z04-TEXTDATA    PIC X(80).
                  05  Z04-GENERICNAME.
                    06  Z04-TEXTDATA    PIC X(200).
                  05  Z04-LABEL2.
                    06  Z04-LABEL2-STYLE   PIC X(20).
                    06  Z04-LABEL2-DATA    PIC X(80).
                  05  Z04-KIKAKUTANI.
                    06  Z04-NUMDATA PIC S9(5).
                  05  Z04-SIPPKBN.
                    06  Z04-NUMDATA PIC S9(1).
                  05  Z04-SIPRYOU1.
                    06  Z04-NUMDATA PIC S9(3).
                  05  Z04-SIPIRISU1.
                    06  Z04-NUMDATA PIC S9(3).
                  05  Z04-SIPRYOU2.
                    06  Z04-NUMDATA PIC S9(3).
                  05  Z04-SIPIRISU2.
                    06  Z04-NUMDATA PIC S9(3).
                  05  Z04-USER-TANICD-G.
                    06  Z04-USER-TANICD-STYLE   PIC X(20).
                    06  Z04-USER-TANICD         PIC X(28).
                  05  Z04-INLABEL1.
                    06  Z04-INLABEL1-STYLE   PIC X(20).
                  05  Z04-INLABEL2.
                    06  Z04-INLABEL2-STYLE   PIC X(20).
                  05  Z04-INLABEL3.
                    06  Z04-INLABEL3-STYLE   PIC X(20).
                  05  Z04-INLABEL4.
                    06  Z04-INLABEL4-STYLE   PIC X(20).
                  05  Z04-INLABEL5.
                    06  Z04-INLABEL5-STYLE   PIC X(20).
                  05  Z04-INLABEL6.
                    06  Z04-INLABEL6-STYLE   PIC X(20).
                  05  Z04-INLABEL7.
                    06  Z04-INLABEL7-STYLE   PIC X(20).
                  05  Z04-INLABEL8.
                    06  Z04-INLABEL8-STYLE   PIC X(20).
                  05  Z04-INLABEL9.
                    06  Z04-INLABEL9-STYLE   PIC X(20).
                  05  Z04-INLABEL10.
                    06  Z04-INLABEL10-STYLE   PIC X(20).
                  05  Z04-INLABEL11.
                    06  Z04-INLABEL11-STYLE   PIC X(20).
                  05  Z04-INLABEL12.
                    06  Z04-INLABEL12-STYLE   PIC X(20).
                  05  Z04-INLABEL13.
                    06  Z04-INLABEL13-STYLE   PIC X(20).
                  05  Z04-ULABEL1.
                    06  Z04-ULABEL1-STYLE     PIC X(20).
                  05  Z04-ULABEL2.
                    06  Z04-ULABEL2-STYLE     PIC X(20).
                  05  Z04-ULABEL3.
                    06  Z04-ULABEL3-STYLE     PIC X(20).
                  05  Z04-ULABEL4.
                    06  Z04-ULABEL4-STYLE     PIC X(20).
                  05  Z04-ULABEL5.
                    06  Z04-ULABEL5-STYLE     PIC X(20).
                  05  Z04-ULABEL6.
                    06  Z04-ULABEL6-STYLE     PIC X(20).
                  05  Z04-ULABEL7.
                    06  Z04-ULABEL7-STYLE     PIC X(20).
                  05  Z04-ULABEL8.
                    06  Z04-ULABEL8-STYLE     PIC X(20).
                  05  Z04-ULABEL9.
                    06  Z04-ULABEL9-STYLE     PIC X(20).
                  05  Z04-ULABEL10.
                    06  Z04-ULABEL10-STYLE     PIC X(20).
                04  Z04-TG2.
                  05  Z04-YAKKAKJNYMD.
                    06  Z04-TEXTDATA    PIC X(10).
                  05  Z04-GECODE.
                    06  Z04-TEXTDATA    PIC X(12).
                  05  Z04-IPNSHOHOKSN-COMBO.
                    06  Z04-IPNSHOHOKSN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-IPNSHOHOKSN-ITEM
                            PIC X(60)   OCCURS  4 TIMES.
                    06  Z04-IPNSHOHOKSN.
                      07  Z04-TEXTDATA    PIC X(60).
                  05  Z04-HIVKBN-COMBO.
                    06  Z04-HIVKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-HIVKBN-ITEM
                            PIC X(20)   OCCURS  2 TIMES.
                    06  Z04-HIVKBN.
                      07  Z04-TEXTDATA    PIC X(20).
                  05  Z04-CYOKISYUSAI.
                    06  Z04-TEXTDATA    PIC X(9).
                  05  Z04-CYOKISENTEIKBN-COMBO.
                    06  Z04-CYOKISENTEIKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z04-CYOKISENTEIKBN-ITEM
                            PIC X(50)   OCCURS  5 TIMES.
                    06  Z04-CYOKISENTEIKBN.
                      07  Z04-TEXTDATA    PIC X(50).
              03  Z04-INMIDASHI.
                04  Z04-INMIDASHI-STYLE   PIC X(20).
              03  Z04-UZMIDASHI.
                04  Z04-UZMIDASHI-STYLE   PIC X(20).


