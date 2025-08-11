      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSHANKCHG.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Á´³Ñ¢ªÈ¾³ÑÊÑ´¹½èÍý
      *  ÊÖµÑ¥¨¥é¡¼¥³¡¼¥É  : 0:Àµ¾ï¡¡1:ÊÑ´¹¥¨¥é¡¼
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANJI-ERR       PIC 9(01).
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-ZENKAKU1        PIC 9(01).
           03  FLG-ZENKAKU2        PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-MAX2            PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
      *
      *
        01  WRK-AREA.
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-INDAY.
               05  WRK-YY              PIC X(10).
               05  WRK-MM              PIC X(02).
               05  WRK-DD              PIC X(02).
      *
           03  WRK-HMM                 PIC 9(02).
           03  WRK-HDD                 PIC 9(02).
      *
           03  WRK-MOJI                PIC X(02).
      *
           03  WRK-KIGO                PIC X(01).
           03  WRK-ZENMOJI             PIC X(02).
           03  WRK-ZENMOJI2            PIC X(02).
      *
           03  WRK-HANKAKU-OT          PIC X(1000).
      *
           03  WRK-ERRCD               PIC X(02).
      *
           03  WRK-MAX-CNT             PIC 9(04).
      *
           03  WRK-IDX1                PIC 9(04).
           03  WRK-IDX2                PIC 9(04).
      *R01.8
           03  WRK-PCNT                PIC 9(04).
      *
      *    ÊÑ´¹¥Æ¡¼¥Ö¥ë
       01  TBL-HENKAN-AREA.
           03  FILLER              PIC X(01)   VALUE   "0".
           03  FILLER              PIC X(02)   VALUE   "£°".
           03  FILLER              PIC X(01)   VALUE   "1".
           03  FILLER              PIC X(02)   VALUE   "£±".
           03  FILLER              PIC X(01)   VALUE   "2".
           03  FILLER              PIC X(02)   VALUE   "£²".
           03  FILLER              PIC X(01)   VALUE   "3".
           03  FILLER              PIC X(02)   VALUE   "£³".
           03  FILLER              PIC X(01)   VALUE   "4".
           03  FILLER              PIC X(02)   VALUE   "£´".
           03  FILLER              PIC X(01)   VALUE   "5".
           03  FILLER              PIC X(02)   VALUE   "£µ".
           03  FILLER              PIC X(01)   VALUE   "6".
           03  FILLER              PIC X(02)   VALUE   "£¶".
           03  FILLER              PIC X(01)   VALUE   "7".
           03  FILLER              PIC X(02)   VALUE   "£·".
           03  FILLER              PIC X(01)   VALUE   "8".
           03  FILLER              PIC X(02)   VALUE   "£¸".
           03  FILLER              PIC X(01)   VALUE   "9".
           03  FILLER              PIC X(02)   VALUE   "£¹".
      *±Ñ»ú
           03  FILLER              PIC X(01)   VALUE   "A".
           03  FILLER              PIC X(02)   VALUE   "£Á".
           03  FILLER              PIC X(01)   VALUE   "B".
           03  FILLER              PIC X(02)   VALUE   "£Â".
           03  FILLER              PIC X(01)   VALUE   "C".
           03  FILLER              PIC X(02)   VALUE   "£Ã".
           03  FILLER              PIC X(01)   VALUE   "D".
           03  FILLER              PIC X(02)   VALUE   "£Ä".
           03  FILLER              PIC X(01)   VALUE   "E".
           03  FILLER              PIC X(02)   VALUE   "£Å".
           03  FILLER              PIC X(01)   VALUE   "F".
           03  FILLER              PIC X(02)   VALUE   "£Æ".
           03  FILLER              PIC X(01)   VALUE   "G".
           03  FILLER              PIC X(02)   VALUE   "£Ç".
           03  FILLER              PIC X(01)   VALUE   "H".
           03  FILLER              PIC X(02)   VALUE   "£È".
           03  FILLER              PIC X(01)   VALUE   "I".
           03  FILLER              PIC X(02)   VALUE   "£É".
           03  FILLER              PIC X(01)   VALUE   "J".
           03  FILLER              PIC X(02)   VALUE   "£Ê".
           03  FILLER              PIC X(01)   VALUE   "K".
           03  FILLER              PIC X(02)   VALUE   "£Ë".
           03  FILLER              PIC X(01)   VALUE   "L".
           03  FILLER              PIC X(02)   VALUE   "£Ì".
           03  FILLER              PIC X(01)   VALUE   "N".
           03  FILLER              PIC X(02)   VALUE   "£Î".
           03  FILLER              PIC X(01)   VALUE   "M".
           03  FILLER              PIC X(02)   VALUE   "£Í".
           03  FILLER              PIC X(01)   VALUE   "O".
           03  FILLER              PIC X(02)   VALUE   "£Ï".
           03  FILLER              PIC X(01)   VALUE   "P".
           03  FILLER              PIC X(02)   VALUE   "£Ð".
           03  FILLER              PIC X(01)   VALUE   "Q".
           03  FILLER              PIC X(02)   VALUE   "£Ñ".
           03  FILLER              PIC X(01)   VALUE   "R".
           03  FILLER              PIC X(02)   VALUE   "£Ò".
           03  FILLER              PIC X(01)   VALUE   "S".
           03  FILLER              PIC X(02)   VALUE   "£Ó".
           03  FILLER              PIC X(01)   VALUE   "T".
           03  FILLER              PIC X(02)   VALUE   "£Ô".
           03  FILLER              PIC X(01)   VALUE   "U".
           03  FILLER              PIC X(02)   VALUE   "£Õ".
           03  FILLER              PIC X(01)   VALUE   "V".
           03  FILLER              PIC X(02)   VALUE   "£Ö".
           03  FILLER              PIC X(01)   VALUE   "W".
           03  FILLER              PIC X(02)   VALUE   "£×".
           03  FILLER              PIC X(01)   VALUE   "X".
           03  FILLER              PIC X(02)   VALUE   "£Ø".
           03  FILLER              PIC X(01)   VALUE   "Y".
           03  FILLER              PIC X(02)   VALUE   "£Ù".
           03  FILLER              PIC X(01)   VALUE   "Z".
           03  FILLER              PIC X(02)   VALUE   "£Ú".
      *±Ñ»ú
           03  FILLER              PIC X(01)   VALUE   "a".
           03  FILLER              PIC X(02)   VALUE   "£á".
           03  FILLER              PIC X(01)   VALUE   "b".
           03  FILLER              PIC X(02)   VALUE   "£â".
           03  FILLER              PIC X(01)   VALUE   "c".
           03  FILLER              PIC X(02)   VALUE   "£ã".
           03  FILLER              PIC X(01)   VALUE   "d".
           03  FILLER              PIC X(02)   VALUE   "£ä".
           03  FILLER              PIC X(01)   VALUE   "e".
           03  FILLER              PIC X(02)   VALUE   "£å".
           03  FILLER              PIC X(01)   VALUE   "f".
           03  FILLER              PIC X(02)   VALUE   "£æ".
           03  FILLER              PIC X(01)   VALUE   "g".
           03  FILLER              PIC X(02)   VALUE   "£ç".
           03  FILLER              PIC X(01)   VALUE   "h".
           03  FILLER              PIC X(02)   VALUE   "£è".
           03  FILLER              PIC X(01)   VALUE   "i".
           03  FILLER              PIC X(02)   VALUE   "£é".
           03  FILLER              PIC X(01)   VALUE   "j".
           03  FILLER              PIC X(02)   VALUE   "£ê".
           03  FILLER              PIC X(01)   VALUE   "k".
           03  FILLER              PIC X(02)   VALUE   "£ë".
           03  FILLER              PIC X(01)   VALUE   "l".
           03  FILLER              PIC X(02)   VALUE   "£ì".
           03  FILLER              PIC X(01)   VALUE   "n".
           03  FILLER              PIC X(02)   VALUE   "£î".
           03  FILLER              PIC X(01)   VALUE   "m".
           03  FILLER              PIC X(02)   VALUE   "£í".
           03  FILLER              PIC X(01)   VALUE   "o".
           03  FILLER              PIC X(02)   VALUE   "£ï".
           03  FILLER              PIC X(01)   VALUE   "p".
           03  FILLER              PIC X(02)   VALUE   "£ð".
           03  FILLER              PIC X(01)   VALUE   "q".
           03  FILLER              PIC X(02)   VALUE   "£ñ".
           03  FILLER              PIC X(01)   VALUE   "r".
           03  FILLER              PIC X(02)   VALUE   "£ò".
           03  FILLER              PIC X(01)   VALUE   "s".
           03  FILLER              PIC X(02)   VALUE   "£ó".
           03  FILLER              PIC X(01)   VALUE   "t".
           03  FILLER              PIC X(02)   VALUE   "£ô".
           03  FILLER              PIC X(01)   VALUE   "u".
           03  FILLER              PIC X(02)   VALUE   "£õ".
           03  FILLER              PIC X(01)   VALUE   "v".
           03  FILLER              PIC X(02)   VALUE   "£ö".
           03  FILLER              PIC X(01)   VALUE   "w".
           03  FILLER              PIC X(02)   VALUE   "£÷".
           03  FILLER              PIC X(01)   VALUE   "x".
           03  FILLER              PIC X(02)   VALUE   "£ø".
           03  FILLER              PIC X(01)   VALUE   "y".
           03  FILLER              PIC X(02)   VALUE   "£ù".
           03  FILLER              PIC X(01)   VALUE   "z".
           03  FILLER              PIC X(02)   VALUE   "£ú".
      *
           03  FILLER              PIC X(01)   VALUE   "-".
           03  FILLER              PIC X(02)   VALUE   "¡Ý".
           03  FILLER              PIC X(01)   VALUE   "(".
           03  FILLER              PIC X(02)   VALUE   "¡Ê".
           03  FILLER              PIC X(01)   VALUE   ")".
           03  FILLER              PIC X(02)   VALUE   "¡Ë".
           03  FILLER              PIC X(01)   VALUE   "\".
           03  FILLER              PIC X(02)   VALUE   "¡ï".
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILLER              PIC X(02)   VALUE   "¡¥".
           03  FILLER              PIC X(01)   VALUE   "/".
           03  FILLER              PIC X(02)   VALUE   "¡¿".
           03  FILLER              PIC X(01)   VALUE   "<".
           03  FILLER              PIC X(02)   VALUE   "¡ã".
           03  FILLER              PIC X(01)   VALUE   ">".
           03  FILLER              PIC X(02)   VALUE   "¡ä".
           03  FILLER              PIC X(01)   VALUE   "*".
           03  FILLER              PIC X(02)   VALUE   "¡ö".
           03  FILLER              PIC X(01)   VALUE   "?".
           03  FILLER              PIC X(02)   VALUE   "¡©".
           03  FILLER              PIC X(01)   VALUE   "%".
           03  FILLER              PIC X(02)   VALUE   "¡ó".
           03  FILLER              PIC X(01)   VALUE   "+".
           03  FILLER              PIC X(02)   VALUE   "¡Ü".
           03  FILLER              PIC X(01)   VALUE   "!".
           03  FILLER              PIC X(02)   VALUE   "¡ª".
           03  FILLER              PIC X(01)   VALUE   "#".
           03  FILLER              PIC X(02)   VALUE   "¡ô".
           03  FILLER              PIC X(01)   VALUE   "&".
           03  FILLER              PIC X(02)   VALUE   "¡õ".
           03  FILLER              PIC X(01)   VALUE   "=".
           03  FILLER              PIC X(02)   VALUE   "¡á".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(02)   VALUE   "¡¤".
           03  FILLER              PIC X(01)   VALUE   "_".
           03  FILLER              PIC X(02)   VALUE   "¡²".
           03  FILLER              PIC X(01)   VALUE   "@".
           03  FILLER              PIC X(02)   VALUE   "¡÷".
           03  FILLER              PIC X(01)   VALUE   "[".
           03  FILLER              PIC X(02)   VALUE   "¡Î".
           03  FILLER              PIC X(01)   VALUE   "]".
           03  FILLER              PIC X(02)   VALUE   "¡Ï".
      *
       01  TBL-HENKAN-AREA-R           REDEFINES   TBL-HENKAN-AREA.
           03  TBL-HENKAN-OCC          OCCURS   83
                                       INDEXED  TBL-IDX.
               05  TBL-HALF            PIC X(01).
               05  TBL-ZEN             PIC X(02).
       01  TBL-MAX                     PIC 9(04)   VALUE   83.
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
           COPY    "CPORCXKANACONV2.INC".
      *
      *    ³ÈÄ¥Ê¸»úÂÐ±þ
           COPY    "CPORCSSTRING.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSHANKCHG.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSHANKCHGAREA.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO                TO  HANKCHG-RC
           MOVE    ZERO                TO  HANKCHG-RC2
           MOVE    SPACE               TO  HANKCHG-OUT-INPUT
      *    ºÇÂç·å¿ôÈ½Äê
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDX     FROM    1000 BY  -1
                   UNTIL       IDX     <   1
               IF     HANKCHG-MAE-INPUT (IDX:1)    NOT =   SPACE
                   MOVE    IDX             TO  IDX-MAX
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           IF      IDX-MAX             =   ZERO
               MOVE    ZERO                TO  HANKCHG-MAX
               MOVE    SPACE               TO  HANKCHG-OUT-INPUT
           ELSE
               MOVE    IDX-MAX             TO  HANKCHG-MAX
           END-IF
      *TEST
      *    DISPLAY " HANKCHG-MAX:"  HANKCHG-MAX
      *    DISPLAY " HANKCHG-MAE-INPUT:"  HANKCHG-MAE-INPUT
      *                    (1:HANKCHG-MAX)
      *
           IF      IDX-MAX             >   ZERO
      *        Á´³Ñ¤ÎÆâÍÆÈ½Äê
               PERFORM 1000-KOUMOKU-CHK-SEC
      *
      *        Á´³ÑÈ½Äê½èÍý
               PERFORM 100-ZENKAKU-CHECK-SEC
      *        È¾³Ñ¤Î¤ß¤Ï¡¢ÊÑ´¹¤Ê¤·
               IF     (FLG-KANA            =   1    )
                 OR   (HANKCHG-RC      NOT =   ZERO )
                   CONTINUE
               ELSE
      *            Á´³ÑÃÖ´¹½èÍý
                   PERFORM 200-ZENKAKU-HENKAN-SEC
      *            È¾³ÑÊÑ´¹¡Ê±Ñ¿ô¡¢Á´³Ñ¥«¥Ê¤Î¤ß¡Ë
                   IF      HANKCHG-RC      =   ZERO
                       PERFORM 300-HANKAKU-HENKAN-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    Á´³Ñ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       1000-KOUMOKU-CHK-SEC           SECTION.
      *
      *    È¾³Ñ¥«¥Ê¡¢³°»ú¡Ê¢£¡ËÊÑ´¹
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    IDX-MAX             TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    HANKCHG-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
           MOVE    KANACONV-OUTDATA    TO  HANKCHG-MAE-INPUT
           MOVE    KANACONV-OUTDATA    TO  HANKCHG-OUT-INPUT
           IF      KANACONV-MAXLEN     =   IDX-MAX
               MOVE    ZERO                TO  HANKCHG-RC2
           ELSE
               MOVE    7                   TO  HANKCHG-RC2
           END-IF
           MOVE    KANACONV-MAXLEN     TO  IDX-MAX
           MOVE    KANACONV-MAXLEN     TO  HANKCHG-MAX
           .
       1000-KOUMOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ²þ¹ÔÈ½Äê½èÍý
      *****************************************************************
       1009-KAIGYO-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-PCNT
           INSPECT     HANKCHG-OUT-INPUT  TALLYING  WRK-PCNT
                                      FOR ALL  X"0A"
      *    ²þ¹Ô¥³¡¼¥É¤¢¤ê¤ÏÊÑ´¹ÉÔ²Ä
           IF      WRK-PCNT            >   ZERO
               MOVE    2                   TO  HANKCHG-RC
           END-IF
      *
           .
       1009-KAIGYO-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    Á´³ÑÈ½Äê½èÍý
      *****************************************************************
       100-ZENKAKU-CHECK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  FLG-KANA
           MOVE    ZERO                TO  FLG-KUHAKU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    HANKCHG-MAX         TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    15                  TO  KANACONV-CHAR-TYPE
           MOVE    HANKCHG-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
      *        Á´³Ñ¤Î¤ß
               MOVE    ZERO                TO  FLG-KANA
           ELSE
      *        È¾³Ñ¤Î¤ß¤ÎÈ½Äê
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    HANKCHG-MAX         TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    113                 TO  KANACONV-CHAR-TYPE
               MOVE    HANKCHG-MAE-INPUT   TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               IF      KANACONV-RETURN     =   ZERO
                   MOVE    1                   TO  FLG-KANA
                   MOVE    HANKCHG-MAE-INPUT   TO  HANKCHG-OUT-INPUT
                   MOVE    KANACONV-MAXLEN     TO  HANKCHG-MAX
               ELSE
      *            º®ºß¥¨¥é¡¼
                   MOVE    2                   TO  FLG-KANA
               END-IF
           END-IF
      *test
           DISPLAY "CHG FLG-KANA:"  FLG-KANA
      *
      *    ²þ¹Ô¥³¡¼¥É¤¢¤ê¤ÏÊÑ´¹ÉÔ²Ä
           PERFORM 1009-KAIGYO-CHK-SEC
           .
       100-ZENKAKU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    È¾³ÑÊÑ´¹Äê½èÍý
      *****************************************************************
       300-HANKAKU-HENKAN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-HANKAKU-OT
           MOVE    1                   TO  IDH2
           PERFORM VARYING     IDH1    FROM    1   BY  2
                   UNTIL      (IDH1        >   HANKCHG-MAX)
                        OR    (IDH2        >   1000 )
                        OR    (HANKCHG-RC  NOT =   ZERO )
               MOVE    HANKCHG-OUT-INPUT (IDH1:2)
                                           TO  WRK-MOJI
      *        ±Ñ¿ôÃÍ¥Æ¡¼¥Ö¥ë¸¡º÷
               SET      TBL-IDX            TO  1
               SEARCH   TBL-HENKAN-OCC     VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO             TO  FLG-ARI
                   WHEN    WRK-MOJI             =   TBL-ZEN
                                                          (TBL-IDX)
                       MOVE    TBL-HALF  (TBL-IDX)
                                                TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                       ADD     1                TO  IDH2
                       MOVE    1                TO  FLG-ARI
               END-SEARCH
               IF      FLG-ARI             =   ZERO
                   EVALUATE   WRK-MOJI
      *                A8 A1
                       WHEN    "¨¡"
      *                A1 DD
                       WHEN    "¡Ý"
      *                A1 BE
                       WHEN    "¡¾"
      *                A1 BD
                       WHEN    "¡½"
      *                A1 BC
                       WHEN    "¡¼"
                           MOVE    "-"          TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                           ADD     1            TO  IDH2
                           MOVE    1            TO  FLG-ARI 
      *               ¶õÇò
                       WHEN    "¡¡"
                           MOVE    SPACE        TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                           ADD     1            TO  IDH2
                           MOVE    1            TO  FLG-ARI
                   END-EVALUATE
               END-IF
      *
               IF      FLG-ARI             =   ZERO
      *            ¿ôÃÍ¡¦±Ñ»úÂÐ¾Ý¤Ê¤·
                   IF      HANKCHG-SYORI       =   "1"
      *                ±Ñ¿ôÃÍ¤Î¤ßÊÑ´¹
                       MOVE    2               TO  HANKCHG-RC2
                       IF      HANKCHG-SYORIKBN     =   "1"
                           MOVE    "?"             TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                           ADD     1               TO  IDH2
                       END-IF
                   ELSE
      *                ¥«¥ÊÊ¸»úÊÑ´¹
                       PERFORM 3001-KANAHENNKAN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      HANKCHG-RC2         =   2
      *       ÊÑ´¹ÉÔ²Ä¤¢¤ê¥¨¥é¡¼
              IF      HANKCHG-SYORIKBN     =   "2"
                  MOVE    1                TO  HANKCHG-RC
                  MOVE    HANKCHG-MAE-INPUT
                                           TO  HANKCHG-OUT-INPUT
              END-IF
           END-IF
           IF      HANKCHG-RC          =   ZERO
               MOVE    WRK-HANKAKU-OT      TO  HANKCHG-OUT-INPUT
               COMPUTE HANKCHG-MAX         =   IDH2    -   1
           END-IF
           .
       300-HANKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´³Ñ¥«¥ÊÊ¸»ú¤òÈ¾³Ñ¥«¥Ê¤ØÊÑ´¹½èÍý
      *****************************************************************
       3001-KANAHENNKAN-SEC           SECTION.
      *
           EVALUATE    WRK-MOJI
                   WHEN    "¥¡"
                       MOVE    "Ž§"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¢"
                       MOVE    "Ž±"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥£"
                       MOVE    "Ž¨"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¤"
                       MOVE    "Ž²"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¥"
                       MOVE    "Ž©"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¦"
                       MOVE    "Ž³"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥§"
                       MOVE    "Žª"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¨"
                       MOVE    "Ž´"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥©"
                       MOVE    "Ž«"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ª"
                       MOVE    "Žµ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "¥«"
                       MOVE    "Ž¶"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¬"
                       MOVE    "Ž¶ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥­"
                       MOVE    "Ž·"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥®"
                       MOVE    "Ž·ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥¯"
                       MOVE    "Ž¸"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥°"
                       MOVE    "Ž¸ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥±"
                       MOVE    "Ž¹"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥²"
                       MOVE    "Ž¹ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥³"
                       MOVE    "Žº"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥´"
                       MOVE    "ŽºŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
      *
                   WHEN    "¥µ"
                       MOVE    "Ž»"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¶"
                       MOVE    "Ž»ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥·"
                       MOVE    "Ž¼"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¸"
                       MOVE    "Ž¼ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥¹"
                       MOVE    "Ž½"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥º"
                       MOVE    "Ž½ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥»"
                       MOVE    "Ž¾"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¼"
                       MOVE    "Ž¾ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥½"
                       MOVE    "Ž¿"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥¾"
                       MOVE    "Ž¿ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
      *
                   WHEN    "¥¿"
                       MOVE    "ŽÀ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥À"
                       MOVE    "ŽÀŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Á"
                       MOVE    "ŽÁ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Â"
                       MOVE    "ŽÁŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Ã"
                       MOVE    "Ž¯"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Ä"
                       MOVE    "ŽÂ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Å"
                        MOVE  "ŽÂŽÞ"      TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Æ"
                       MOVE    "ŽÃ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Ç"
                       MOVE  "ŽÃŽÞ"       TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥È"
                       MOVE    "ŽÄ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥É"
                       MOVE    "ŽÄŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
      * 
                   WHEN    "¥Ê"
                       MOVE    "ŽÅ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Ë"
                       MOVE    "ŽÆ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                  WHEN    "¥Ì"
                       MOVE    "ŽÇ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Í"
                       MOVE    "ŽÈ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Î"
                       MOVE    "ŽÉ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "¥Ï"
                       MOVE    "ŽÊ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Ð"
                       MOVE    "ŽÊŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Ñ"
                       MOVE    "ŽÊŽß"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Ò"
                       MOVE    "ŽË"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Ó"
                       MOVE    "ŽËŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Ô"
                       MOVE    "ŽËŽß"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Õ"
                       MOVE    "ŽÌ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Ö"
                       MOVE    "ŽÌŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥×"
                       MOVE    "ŽÌŽß"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Ø"
                       MOVE    "ŽÍ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Ù"
                       MOVE    "ŽÍŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Ú"
                       MOVE    "ŽÍŽß"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Û"
                       MOVE    "ŽÎ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥Ü"
                       MOVE    "ŽÎŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥Ý"
                       MOVE    "ŽÎŽß"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
      *
                   WHEN    "¥Þ"
                       MOVE    "ŽÏ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ß"
                       MOVE    "ŽÐ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥à"
                       MOVE    "ŽÑ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥á"
                       MOVE    "ŽÒ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥â"
                       MOVE    "ŽÓ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "¥ã"
                       MOVE    "Ž¬"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ä"
                       MOVE    "ŽÔ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥å"
                       MOVE    "Ž­"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥æ"
                       MOVE    "ŽÕ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ç"
                       MOVE    "Ž®"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥è"
                       MOVE    "ŽÖ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "¥é"
                       MOVE    "Ž×"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ê"
                       MOVE    "ŽØ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ë"
                       MOVE    "ŽÙ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ì"
                       MOVE    "ŽÚ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥í"
                       MOVE    "ŽÛ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
                   WHEN    "¥î"
                       MOVE    "ŽÜ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ï"
                       MOVE    "ŽÜ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ð"
                       MOVE    "Ž²"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ñ"
                       MOVE    "Ž´"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ò"
                       MOVE    "Ž¦"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ó"
                       MOVE    "ŽÝ"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ô"
                       MOVE    "Ž³ŽÞ"     TO  WRK-HANKAKU-OT (IDH2:4)
                       ADD     4        TO  IDH2
                   WHEN    "¥õ"
                       MOVE    "Ž¶"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
                   WHEN    "¥ö"
                       MOVE    "Ž¹"      TO  WRK-HANKAKU-OT (IDH2:2)
                       ADD     2        TO  IDH2
      *
               WHEN    OTHER
      *            ÊÑ´¹¥¨¥é¡¼
                   MOVE    2               TO  HANKCHG-RC2
                   IF      HANKCHG-SYORIKBN    =   "1"
                       MOVE    "?"             TO  WRK-HANKAKU-OT
                                                            (IDH2:1)
                       ADD     1               TO  IDH2
                  END-IF
           END-EVALUATE
      *
           .
       3001-KANAHENNKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Á´³Ñ½èÍý
      *****************************************************************
       200-ZENKAKU-HENKAN-SEC           SECTION.
      *
      **   MOVE    2                   TO  HANKCHG-SYUBETU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    HANKCHG-MAX         TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    1                   TO  KANACONV-CONV-FLAG
           MOVE    15                  TO  KANACONV-CHAR-TYPE
           MOVE    HANKCHG-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
               MOVE    KANACONV-OUTDATA    TO  HANKCHG-OUT-INPUT
               MOVE    KANACONV-MAXLEN     TO  HANKCHG-MAX
           ELSE
               MOVE    2                   TO  HANKCHG-RC
           END-IF
      *
           .
       200-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
