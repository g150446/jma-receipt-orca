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
       PROGRAM-ID.             ORCSONSHI002.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾Ú¡¦ÆÃÄê¼ÀÉÂÎÅÍÜ¼õÎÅ¾Ú¥µ¥Ö
      *  ÊÖµÑ¥¨¥é¡¼¥³¡¼¥É  : 
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  21/08/30    ORCAMO        ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      * 05.01.00     ORCAMO       22/12/XX  ¸øÈñ°åÎÅ·ô²èÁü³ÎÇ§ÂÐ±þ¤Ê¤É
      * 05.02.00     ORCAMO       24/02/XX  °åÎÅÉÞ½õÂÐ±þ¤Ê¤É
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
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-ONSKAKU         PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-ONSAIDLST       PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *2022/12
           03  FLG-GENNDO-ERR      PIC 9(01).
      *R05.2
           03  FLG-DOUI-NASI       PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX3                PIC 9(04).
      *
           03  IDX-SK              PIC 9(04).
           03  IDX-SK2             PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-GEN             PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *2024/01 °åÎÅÉÞ½õ
           03  IDX-FUJ             PIC 9(04).
           03  IDX-AID             PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMMDD.
                   07  WRK-SMM             PIC 9(02).
                   07  WRK-SDD             PIC 9(02).
      *
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾ÚÄó¶¡
           03  WRK-GENDO               PIC X(200).
           03  WRK-GENDO-HEN           PIC X(200).
           03  WRK-SIKKAN-HEN          PIC X(200).
      *
           03  WRK-GENDO-SHO           PIC X(50).
           03  WRK-GENDO-TEK           PIC X(50).
           03  WRK-GENDO-STYMD         PIC X(09).
           03  WRK-GENDO-EDYMD         PIC X(09).
           03  WRK-GENDO-SKJGNSTYMD    PIC X(09).
      *
           03  WRK-SIKKAN              PIC X(50).
           03  WRK-SIKKAN-STYMD        PIC X(09).
           03  WRK-SIKKAN-EDYMD        PIC X(09).
           03  WRK-SIKKAN-FTNMONEY     PIC X(06).
           03  WRK-SIKKAN-FTNMONEY-9   PIC 9(06).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
           03  WRK-SIKKAN-MSG          PIC X(200).
      *
           03  WRK-KOH-AREA.
               05  WRK-KOHNUM          PIC X(03).
               05  WRK-KOH-JKYSNUM     PIC X(20).
           03  WRK-TEISYO-AREA.
               05  WRK-TEISYO-KBN          PIC X(01).
               05  WRK-TEISYO-HKNTEKKBN    PIC X(01).
               05  WRK-TEISYO-KYOUKAI      PIC X(01).
               05  WRK-TEISYO-RRIFUK       PIC X(01).
      *
           03  WRK-KOH-TEKSTYMD        PIC X(08).
           03  WRK-KOH-TEKEDYMD        PIC X(08).
           03  WRK-HKN-TEKSTYMD        PIC X(08).
           03  WRK-HKN-TEKEDYMD        PIC X(08).
           03  WRK-MATU-DD             PIC X(02).
      *°åÎÅÉÞ½õ
           03  WRK-FUJYO-HEN           PIC X(200).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *    °åÎÅ·ô
           03  WRK-NYUGAIKBN           PIC X(04).
           03  WRK-SRYYM               PIC X(06).
      *
       01  TBL-ONS-AIDLST-AREA.
           03  TBL-ONS-AIDLST-OCC      OCCURS   10.
               07  TBL-TEISTYMD        PIC X(08).
               07  TBL-TEIEDYMD        PIC X(08).
               07  TBL-SRYYM           PIC X(06).
               07  TBL-KOUFUNUM        PIC X(15).
               07  TBL-NYUGAIKBN       PIC X(01).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ¿ô»úÊÑ´¹ÎÎ°è
           COPY    "CPORCSNUM.INC".
      *
      *    Á´³Ñ¥Á¥§¥Ã¥¯¥Ñ¥é¥á¥¿
           COPY    "CPORCSKANACHK.INC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¶¦ÄÌÎÎ°è
           COPY    "MCPAREA".
      *
      *    ÊÝ¸±¾ðÊó
      *01  HKNNUM-REC.
      *     COPY    "CPHKNNUM.INC".
      *
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§¾È²ñ·ë²Ì¥Æ¡¼¥Ö¥ë
       01  ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC".
      *
      *    ÊÝ¸±¼Ô¹æ¥Þ¥¹¥¿
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *    °åÎÅÉÞ½õ¡¡¸òÉÕÈÖ¹æ
       01  ONSHI-AIDLST-REC.
           COPY    "CPONSHI-AIDLST.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾Ú¤Ê¤ÉÊÔ½¸¥Ñ¥é¥á¥¿
           COPY    "CPORCSONSHI002.INC".
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§¾È²ñ·ë²Ì¥Æ¡¼¥Ö¥ë
       01  LNK-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING
                                   //ONSKAKU-// BY //LNK-ONSKAKU-//.
      *
       PROCEDURE                    DIVISION    USING
           SPA-AREA
           ORCSONSHI002AREA
           LNK-ONSKAKU-REC
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  LNK-ONSHI002-OUT-AREA
           INITIALIZE                  LNK-ONSHI002-OUT02-AREA
           INITIALIZE                  LNK-ONSHI002-RC 
      *
      *    ½é´ü½èÍý
           PERFORM 100-INIT-SEC
      *
           IF     (LNK-ONSHI002-RC     =   ZERO)
             AND  (LNK-ONSHI002-SYOKBN =   "01"
                                       OR  "03")
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§Ì¾¾ÎÊÔ½¸
               PERFORM 210-ONSHIKAKU-HEN-SEC
           END-IF
      *
           IF     (LNK-ONSHI002-SYOKBN =   "02"
                                       OR  "03")
              AND (LNK-ONSHI002-RC     =   ZERO)
      *        ¥ª¥ó»ñ³Ê¸øÈñÆâÍÆÊÑ´¹
               PERFORM 220-ONSHIKAKU-KOHSYORI-SEC
           END-IF
      *2024/01
           IF     (LNK-ONSHI002-SYOKBN =   "02"
                                       OR  "03")
              AND (LNK-ONSHI002-RC     =   ZERO)
      *        ¥ª¥ó»ñ³Ê°åÎÅÉÞ½õ¸¡º÷É½¼¨
               PERFORM 240-ONSHIKAKU-IRYOFUJYO-SEC
           END-IF
      *
           IF     (LNK-ONSHI002-SYOKBN =   "02"
                                       OR  "03")
              AND (LNK-ONSHI002-RC     =   ZERO)
      *        ¥ª¥ó»ñ³ÊÃÏÊý¸øÈñ¸¡º÷É½¼¨
               PERFORM 230-ONSHIKAKU-CHIHOU-SEC
           END-IF
      *    
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    ½èÍý¶èÊ¬
           IF       LNK-ONSHI002-SYOKBN    =   "01"
                                           OR  "02"
                                           OR  "03"
               CONTINUE
           ELSE
               MOVE    1                   TO  LNK-ONSHI002-RC
           END-IF
      *
           IF     (LNK-ONSKAKU-REC     =   SPACE )
               MOVE    1                   TO  LNK-ONSHI002-RC
           END-IF
           MOVE    LNK-ONSKAKU-REC     TO  ONSKAKU-REC
      *R05.2
      *    ÊÝ¸±¾Ú³ÎÇ§¤ÎÆ±°ÕÀßÄê
           MOVE    ZERO                TO  FLG-DOUI-NASI
           IF      (ONSKAKU-SHOKAI-KBN         =   "2" )
               AND (ONSKAKU-ORCA-SHOKAI-KBN    =   "2" )
      *        ÊÝ¸±¾ÚOCR¤Ï´éÇ§¾Ú¤ÈÆ±ÍÍ
               CONTINUE
           ELSE
      *R04.1
      *    ÊÝ¸±¾Ú³ÎÇ§¤Î»þ¤Ï¡¢´µ¼ÔÅÐÏ¿¤ÇÆ±°Õ¤¬¤¢¤ë¤³¤È
               IF      (ONSKAKU-SHOKAI-KBN     =   "2")
                  AND  (LNK-ONSHI002-DOUIFLG   NOT =   "1")
                   MOVE    1                   TO  FLG-DOUI-NASI
               END-IF
           END-IF
           
           .
       100-INT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§ÊÔ½¸½èÍý
      *****************************************************************
       210-ONSHIKAKU-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-GENDO
           MOVE    SPACE               TO  WRK-GENDO-HEN
           MOVE    SPACE               TO  WRK-SIKKAN
           MOVE    SPACE               TO  WRK-SIKKAN-HEN      
      *2024/01
      *    °åÎÅÉÞ½õ
           IF     (ONSKAKU-CARD-CLASS  =   "A1"  )
               PERFORM 21013-FUJYO-HEN-SEC
               GO  TO  210-ONSHIKAKU-HEN-EXT
           END-IF
      *R04.1
      *    ÊÝ¸±¾Ú³ÎÇ§¤Î»þ¤Ï¡¢´µ¼ÔÅÐÏ¿¤ÇÆ±°Õ¤¬¤¢¤ë¤³¤È
      ***  IF      (ONSKAKU-SHOKAI-KBN     =   "2")
      ***     AND  (LNK-ONSHI002-DOUIFLG   NOT =   "1")
      *R05.2   Æ±°Õ¤Ê¤·
           IF      FLG-DOUI-NASI       =   1
               GO  TO  210-ONSHIKAKU-HEN-EXT
           END-IF
      *
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾Ú
           IF     (ONSKAKU-GENDO-DOUIFLG   =   "1" )
               PERFORM 21011-GENDO-HEN-SEC
           END-IF
      *
      *    ÆÃÄê¼ÀÉÂÎÅÍÜ¼õÎÅ¾ÚÄó¶¡
           IF     (ONSKAKU-SIKKAN-DOUIFLG  =   "1" )
               PERFORM 21012-SIKKAN-HEN-SEC
           END-IF
      *
           .
       210-ONSHIKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾ÚÊÔ½¸½èÍý
      *****************************************************************
       21011-GENDO-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-GENDO-SHO
           MOVE    SPACE               TO  WRK-GENDO-TEK
           EVALUATE    ONSKAKU-GENDO-SHOKBN
               WHEN    "01"
                   MOVE    "¸ÂÅÙ³ÛÅ¬ÍÑ¶èÊ¬Ç§Äê¾Ú"
                                           TO  WRK-GENDO-SHO
               WHEN    "02"
                   MOVE    "¸ÂÅÙ³ÛÅ¬ÍÑ¡¦¸º³ÛÇ§Äê¾Ú"
                                           TO  WRK-GENDO-SHO
               WHEN    "03"
                   MOVE    "É¸½àÉéÃ´³Û¸º³ÛÇ§Äê¾Ú"
                                           TO  WRK-GENDO-SHO
               WHEN    OTHER
                   GO  TO  21011-GENDO-HEN-EXT
           END-EVALUATE
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾ÚÅ¬ÍÑ¶èÊ¬
           EVALUATE    ONSKAKU-GENDO-TEKKBN
               WHEN    "A01"
                   MOVE    "¥¢"            TO  WRK-GENDO-TEK
               WHEN    "A02"
                   MOVE    "¥¤"            TO  WRK-GENDO-TEK
               WHEN    "A03"
                   MOVE    "¥¦"            TO  WRK-GENDO-TEK
               WHEN    "A04"
                   MOVE    "¥¨"            TO  WRK-GENDO-TEK
               WHEN    "A05"
                   MOVE    "¥ª"            TO  WRK-GENDO-TEK
               WHEN    "A06"
                   MOVE    "¥ª¡Ê¶­¡Ë"      TO  WRK-GENDO-TEK
               WHEN    "B01"
                   MOVE    "¸½ÌòÊÂ¤ß£³"    TO  WRK-GENDO-TEK
               WHEN    "B02"
                   MOVE    "¸½ÌòÊÂ¤ß£²"    TO  WRK-GENDO-TEK
               WHEN    "B03"
                   MOVE    "¸½ÌòÊÂ¤ß£±"    TO  WRK-GENDO-TEK
               WHEN    "B04"
                   MOVE    "°ìÈÌ"          TO  WRK-GENDO-TEK
               WHEN    "B05"
                   MOVE    "Äã½êÆÀ£²"      TO  WRK-GENDO-TEK
               WHEN    "B06"
                   MOVE    "Äã½êÆÀ£±"      TO  WRK-GENDO-TEK
               WHEN    "B07"
                   MOVE    "Äã½êÆÀ£±¡ÊÏ·Ê¡¡Ë"  TO  WRK-GENDO-TEK
               WHEN    "B08"
                   MOVE    "Äã½êÆÀ£±¡Ê¶­¡Ë"    TO  WRK-GENDO-TEK
      *2022/9
               WHEN    "B09"
                   MOVE    "°ìÈÌ£²"        TO  WRK-GENDO-TEK
               WHEN    "B10"
                   MOVE    "°ìÈÌ£±"        TO  WRK-GENDO-TEK
           END-EVALUATE
           MOVE    ONSKAKU-GENDO-TEKSTYMD  TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-STYMD
      *
           IF      ONSKAKU-GENDO-TEKEDYMD  =   SPACE
                                           OR  ZERO
               MOVE    ALL "9"             TO  WRK-SYMD
           ELSE
               MOVE    ONSKAKU-GENDO-TEKEDYMD  TO  WRK-SYMD
           END-IF
      *
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-EDYMD
      *
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾ÚÄ¹´üÆþ±¡³ºÅöÇ¯·îÆü
           MOVE    ONSKAKU-GENDO-SKJGNSTYMD    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-SKJGNSTYMD
      *
           MOVE    SPACE               TO  WRK-GENDO-HEN
           STRING  ONSKAKU-GENDO-SHOKBN    DELIMITED  BY  SPACE
                   "¡§"                    DELIMITED  BY  SPACE
                   WRK-GENDO-SHO           DELIMITED  BY  SPACE
                   "¡¡"                    DELIMITED  BY  SPACE
                   ONSKAKU-GENDO-TEKKBN    DELIMITED  BY  SPACE
                   "¡§"                    DELIMITED  BY  SPACE
                   WRK-GENDO-TEK           DELIMITED  BY  SPACE
                   "¡¡"
                   WRK-GENDO-STYMD         DELIMITED  BY  SIZE
                   "¡Á"
                   WRK-GENDO-EDYMD         DELIMITED  BY  SIZE
                   "¡¡Ä¹´üÆþ±¡³ºÅöÇ¯·îÆü¡§"
                   WRK-GENDO-SKJGNSTYMD    DELIMITED  BY  SIZE
                                           INTO  WRK-GENDO-HEN
           END-STRING
      *
      *    ·å¿ô¼èÆÀ
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    100                 TO  KANACHK-MAX-CNT
           MOVE    WRK-GENDO-HEN       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  LNK-ONSHI002-GENDO-MAX
           MOVE    WRK-GENDO-HEN       TO  LNK-ONSHI002-GENDO-MSG
      *
           .
       21011-GENDO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆÃÄê¼ÀÉÂÎÅÍÜ¼õÎÅ¾ÚÄó¶¡ÊÔ½¸½èÍý
      *****************************************************************
       21012-SIKKAN-HEN-SEC              SECTION.
      *
           MOVE    SPACE                  TO  WRK-SIKKAN-MSG
           MOVE    ZERO                   TO  IDX-SK2
           PERFORM VARYING    IDX-SK      FROM    1   BY  1
                   UNTIL      IDX-SK      >   3
              IF      ONSKAKU-SIKKAN-SIPKBN(IDX-SK)   NOT =   SPACE
                   PERFORM 210121-SIKKAN-HENSYU-SEC
              END-IF
           END-PERFORM
           .
       21012-SIKKAN-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ÆÃÄê¼ÀÉÂÎÅÍÜ¼õÎÅ¾ÚÄó¶¡ÊÔ½¸½èÍý
      *****************************************************************
       210121-SIKKAN-HENSYU-SEC          SECTION.
      *
           ADD     1               TO  IDX-SK2
           MOVE    SPACE           TO  WRK-SIKKAN
           MOVE    SPACE           TO  WRK-SIKKAN-FTNMONEY
           EVALUATE    ONSKAKU-SIKKAN-SIPKBN(IDX-SK)
               WHEN    "1"
                   MOVE    "¿Í¹©Æ©ÀÏ¡¡¡¡¡¡¡¡"     TO  WRK-SIKKAN
               WHEN    "2"
                   MOVE    "·ì±Õ¶Å¸Ç°ø»Ò¾ã³²"     TO  WRK-SIKKAN
               WHEN    "3"
                   MOVE    "£È£É£Ö´¶À÷¾É¡¡¡¡"     TO  WRK-SIKKAN
           END-EVALUATE
      *
           MOVE    ONSKAKU-SIKKAN-TEKSTYMD (IDX-SK)
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-STYMD
           IF      ONSKAKU-SIKKAN-TEKEDYMD (IDX-SK)    =   SPACE
                                                       OR  ZERO
               MOVE    ALL "9"             TO  WRK-SYMD
           ELSE
               MOVE    ONSKAKU-SIKKAN-TEKEDYMD (IDX-SK)
                                           TO  WRK-SYMD
           END-IF
      *
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-EDYMD
      *
           IF      ONSKAKU-SIKKAN-FTNMONEY(IDX-SK) NOT =   SPACE
               INITIALIZE                      ORCSNUMAREA
               MOVE    ONSKAKU-SIKKAN-FTNMONEY(IDX-SK) TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    ONSKAKU-SIKKAN-FTNMONEY (IDX-SK)
                                           TO   WRK-SIKKAN-FTNMONEY
               ELSE
                   MOVE    SNUM-OUTNUM     TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  WRK-SIKKAN-FTNMONEY
               END-IF
           END-IF
      *
           STRING  ONSKAKU-SIKKAN-SIPKBN(IDX-SK)
                                           DELIMITED  BY  SPACE
                   "¡§"                    DELIMITED  BY  SPACE
                   WRK-SIKKAN              DELIMITED  BY  SPACE
                   "¡¡"                    DELIMITED  BY  SPACE
                   "¡¡"
                   WRK-SIKKAN-STYMD         DELIMITED  BY  SIZE
                   "¡Á"
                   WRK-SIKKAN-EDYMD         DELIMITED  BY  SIZE
                   "¡¡¼«¸ÊÉéÃ´¸ÂÅÙ³Û¡§"
                   WRK-SIKKAN-FTNMONEY      DELIMITED  BY  SIZE
                                            INTO  WRK-SIKKAN-MSG
           END-STRING
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    200                 TO  KANACHK-MAX-CNT
           MOVE    WRK-SIKKAN-MSG      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  LNK-ONSHI002-SIKKAN-MAX
                                                   (IDX-SK2)
           MOVE    WRK-SIKKAN-MSG      TO  LNK-ONSHI002-SIKKAN-MSG
                                                   (IDX-SK2)
           .
       210121-SIKKAN-HENSYU-EXT.
           EXIT.
      *
      *2024/01 
      *****************************************************************
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§¡¡°åÎÅÉÞ½õÆâÍÆÊÔ½¸½èÍý
      *****************************************************************
       21013-FUJYO-HEN-SEC      SECTION.
      *
           MOVE    1                   TO  IDX-FUJ
           PERFORM 210131-FUJYO-MSGHEN-SEC
      *
      *    °åÎÅÉÞ½õ¤Ç¸¡º÷
           INITIALIZE                      ONSKAKU-REC
           MOVE    LNK-ONSKAKU-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSKAKU-FUJYO-UUID
                                       TO  ONSKAKU-FUJYO-UUID
           MOVE    LNK-ONSKAKU-KAKU-KAKUNINYMD
                                       TO  ONSKAKU-KAKU-KAKUNINYMD
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           PERFORM UNTIL       (FLG-ONSKAKU   =   1  )
                         OR    (IDX-FUJ       >=  2  )
      *        À¸Ç¯·îÆü¤¬Æ±¤¸¤³¤È
               IF     (LNK-ONSKAKU-RES-BIRTHDAY
                                           =   ONSKAKU-RES-BIRTHDAY)
      *          ½èÍý·ë²Ì¶èÊ¬
                 AND  (ONSKAKU-RESULT-KBN  =   "1"  )
      *          °åÎÅÉÞ½õ¤Î¤ß
                 AND  (ONSKAKU-CARD-CLASS  =   "A1" )
                 AND  (ONSKAKU-TBL-UUID
                                       NOT =   LNK-ONSKAKU-TBL-UUID)
                   ADD     1                   TO  IDX-FUJ
                   PERFORM 210131-FUJYO-MSGHEN-SEC
               END-IF
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       21013-FUJYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§¡¡°åÎÅÉÞ½õÆâÍÆÊÔ½¸½èÍý
      *****************************************************************
       210131-FUJYO-MSGHEN-SEC      SECTION.
      *
      *    ²ÃÆþ¼Ô»ñ³Ê¼èÆÀÆü
           MOVE    ONSKAKU-KOH-TEKSTYMD (1)
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-STYMD
      *    ²ÃÆþ¼Ô»ñ³ÊÁÓ¼ºÆü
           IF      ONSKAKU-KOH-TEKEDYMD (1)    =   SPACE
                                               OR  ZERO
               MOVE    ALL "9"             TO  WRK-SYMD
           ELSE
               MOVE    ONSKAKU-KOH-TEKEDYMD (1)
                                           TO  WRK-SYMD
           END-IF
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-EDYMD
      *
      *
           MOVE    SPACE               TO  WRK-FUJYO-HEN
      *
           STRING  "ÉéÃ´¼ÔÈÖ¹æ¡§"
                   ONSKAKU-RES-HKNJANUM    DELIMITED BY  SPACE
                   "¡¡¼õµë¼ÔÈÖ¹æ¡§"
                   ONSKAKU-RES-NUM         DELIMITED BY  SPACE
                   "¡¡´ü´Ö¡§"
                   WRK-SIKKAN-STYMD        DELIMITED  BY  SIZE
                   "¡Á"
                   WRK-SIKKAN-EDYMD        DELIMITED  BY  SIZE
                                           INTO  WRK-FUJYO-HEN
           END-STRING
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    200                 TO  KANACHK-MAX-CNT
           MOVE    WRK-FUJYO-HEN       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  LNK-ONSHI002-FUJYO-MAX
                                                   (IDX-FUJ)
           MOVE    WRK-FUJYO-HEN       TO  LNK-ONSHI002-FUJYO-MSG
                                                   (IDX-FUJ)
      *
      *
           INITIALIZE                  TBL-ONS-AIDLST-AREA
           MOVE    ZERO                TO  IDX-AID
      *    ¸òÉÕÈÖ¹æ¤ÎÅ¾µ­¤ò¤¹¤ë¤Î¤ß
           IF      LNK-ONSHI002-KFTNKKBN   =   "1"
               CONTINUE
           ELSE
               GO  TO  210131-FUJYO-MSGHEN-EXT
           END-IF
      *
      *    °åÎÅ·ô¾ðÊó¸¡º÷
           INITIALIZE                  ONSHI-AIDLST-REC
           MOVE    ONSKAKU-HOSPNUM     TO  ONS-AIDLST-HOSPNUM
           MOVE    ONSKAKU-FUJYO-UUID
                                       TO  ONS-AIDLST-FUJYO-UUID
           MOVE    ONSKAKU-RES-HKNJANUM
                                       TO  ONS-AIDLST-FTNJANUM
           MOVE    ONSKAKU-RES-NUM
                                       TO  ONS-AIDLST-JKYSNUM
      *
           MOVE    ONSHI-AIDLST-REC    TO  MCPDATA-REC
           MOVE    "tbl_onshi_aidlst"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-ONSAIDLST-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSAIDLST
           END-IF
           PERFORM UNTIL       (FLG-ONSAIDLST   =   1  )
                         OR    (IDX-AID         >=  10 )
                         OR    (IDX-FUJ         >=  10 )
      *        ´µ¼ÔÈÖ¹æ¤¬°ìÃ×¤·¤Ê¤¤¤ÈÂÐ¾Ý³°
      *???? ¹¹¿·¤Þ¤ÇÈ½ÄêÊÝÎ±
      *         IF     (LNK-ONSKAKU-PTID    NOT =   ZERO   )
      *         AND    (LNK-ONSKAKU-PTID    NOT =   ONS-AIDLST-PTID)
      *             CONTINUE
      *         ELSE
                   ADD     1               TO  IDX-AID
                   MOVE    ONS-AIDLST-TEKSTYMD
                                           TO  TBL-TEISTYMD (IDX-AID)
                   MOVE    ONS-AIDLST-TEKEDYMD
                                           TO  TBL-TEIEDYMD (IDX-AID)
                   MOVE    ONS-AIDLST-NYUGAIKBN
                                           TO  TBL-NYUGAIKBN (IDX-AID)
                   MOVE    ONS-AIDLST-SRYYM
                                           TO  TBL-SRYYM  (IDX-AID)
                   MOVE    ONS-AIDLST-KOFUNUM
                                           TO  TBL-KOUFUNUM (IDX-AID)
                   PERFORM 210131-ONSAIDLST-MSGHEN-SEC
      *         END-IF
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 900-ONSAIDLST-READ-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onshi_aidlst"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       210131-FUJYO-MSGHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§¡¡¸øÈñÆâÍÆÊÔ½¸½èÍý
      *****************************************************************
       210131-ONSAIDLST-MSGHEN-SEC      SECTION.
      *
      *    Í­¸ú³«»ÏÇ¯·îÆü
           MOVE    ONS-AIDLST-TEKSTYMD
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-STYMD
      *    Í­¸ú½ªÎ»Æü
           IF      ONS-AIDLST-TEKEDYMD =   SPACE
                                       OR  ZERO
               MOVE    ALL "9"             TO  WRK-SYMD
           ELSE
               MOVE    ONS-AIDLST-TEKEDYMD
                                           TO  WRK-SYMD
           END-IF
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-EDYMD
      *    ¿ÇÎÅÇ¯·î
           MOVE    ONS-AIDLST-SRYYM
                                       TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD(1:6)     TO  WRK-SRYYM
      *
           EVALUATE    ONS-AIDLST-NYUGAIKBN
           WHEN    "1"
               MOVE    "Æþ±¡"          TO  WRK-NYUGAIKBN
           WHEN    "2"
               MOVE    "³°Íè"          TO  WRK-NYUGAIKBN
           END-EVALUATE
      *
      *
           MOVE    SPACE               TO  WRK-FUJYO-HEN
      *
           STRING  "¡¡¡Ì°åÎÅ·ô"
                   "¡¡¸òÉÕÈÖ¹æ¡§"
                   ONS-AIDLST-KOFUNUM      DELIMITED  BY  SIZE
                   "¡¡¿ÇÎÅÇ¯·î¡§"
                   WRK-SRYYM               DELIMITED  BY  SIZE
                   "¡¡"
                   WRK-NYUGAIKBN           DELIMITED BY  SPACE
                   "¡¡´ü´Ö¡§"
                   WRK-SIKKAN-STYMD        DELIMITED  BY  SIZE
                   "¡Á"
                   WRK-SIKKAN-EDYMD        DELIMITED  BY  SIZE
                   "¡Í"                    DELIMITED  BY  SIZE
                                           INTO  WRK-FUJYO-HEN
           END-STRING
      *
           ADD     1                   TO  IDX-FUJ
           IF      IDX-FUJ             >   10
               GO  TO  210131-ONSAIDLST-MSGHEN-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    200                 TO  KANACHK-MAX-CNT
           MOVE    WRK-FUJYO-HEN       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  LNK-ONSHI002-FUJYO-MAX
                                                   (IDX-FUJ)
           MOVE    WRK-FUJYO-HEN       TO  LNK-ONSHI002-FUJYO-MSG
                                                   (IDX-FUJ)
      *
           .
       210131-ONSAIDLST-MSGHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§¡¡¸øÈñÆâÍÆÊÔ½¸½èÍý
      *****************************************************************
       220-ONSHIKAKU-KOHSYORI-SEC      SECTION.
      *
           MOVE    ZERO                TO  IDX-K
      *R04.1
      *    ÊÝ¸±¾Ú³ÎÇ§¤Î»þ¤Ï¡¢´µ¼ÔÅÐÏ¿¤ÇÆ±°Õ¤¬¤¢¤ë¤³¤È
      ***  IF      (ONSKAKU-SHOKAI-KBN     =   "2" )
      ***    AND   (LNK-ONSHI002-DOUIFLG   NOT =   "1" )
      *R05.2   Æ±°Õ¤Ê¤·
           IF      FLG-DOUI-NASI       =   1
               GO  TO  220-ONSHIKAKU-KOHSYORI-EXT
           END-IF
      *
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾Ú
           IF     (ONSKAKU-GENDO-DOUIFLG   =   "1" )
               PERFORM  2201-GENDO-KOHHEN-SEC
           END-IF
      *
      *    ÆÃÄê¼ÀÉÂÎÅÍÜ¼õÎÅ¾ÚÄó¶¡
           IF     (ONSKAKU-SIKKAN-DOUIFLG  =   "1" )
               PERFORM 2202-SIKKAN-KOHHEN-SEC
           END-IF
      *
           MOVE    IDX-K               TO   LNK-ONSHI002-KOHMAX
           .
       220-ONSHIKAKU-KOHSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾Ú¡¡ÊÔ½¸½èÍý
      *****************************************************************
       2201-GENDO-KOHHEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-TEISYO-AREA
           MOVE    SPACE               TO  WRK-KOH-AREA
      *
           MOVE    ZERO                TO  FLG-GENNDO-ERR
           EVALUATE    ONSKAKU-GENDO-SHOKBN
               WHEN    "01"
      *        ¸ÂÅÙ³ÛÅ¬ÍÑ¶èÊ¬Ç§Äê¾Ú
                    PERFORM 22011-GENDO01-KOHHEN-SEC
               WHEN    "02"
      *        ¸ÂÅÙ³ÛÅ¬ÍÑ¡¦¸º³ÛÇ§Äê¾Ú
                   PERFORM 22012-GENDO02-KOHHEN-SEC
               WHEN    "03"
      *        É¸½àÉéÃ´³Û¸º³ÛÇ§Äê¾Ú
                    PERFORM 22013-GENDO03-KOHHEN-SEC
           END-EVALUATE
      *
      *2022/12
           IF      FLG-GENNDO-ERR      =   1
               STRING    "¡Ì¸ÂÅÙ³ÛÇ§Äê¾Ú¤ÎÀ©ÅÙÆâÍÆ¤¬³ÎÇ§¤Ç¤­¤Þ¤»¤ó¡£"
                         "ÊÝ¸±¼Ô¤Ë¾È²ñ¤·¤Æ²¼¤µ¤¤¡Í"
                                    DELIMITED BY SIZE
                                    INTO  LNK-ONSHI002-GENDO-KEIMSG
               END-STRING
           END-IF
      *
      *    ÊÝ¸±¤Î»ñ³Ê¼èÆÀÆü¤È¤ÎÈ½Äê
           IF      (ONSKAKU-ELDER-TEKSTYMD NOT =   SPACE)
               MOVE    ONSKAKU-ELDER-TEKSTYMD  TO  WRK-HKN-TEKSTYMD
               MOVE    ONSKAKU-ELDER-TEKEDYMD  TO  WRK-HKN-TEKEDYMD
           ELSE
               MOVE    ONSKAKU-TEKSTYMD        TO  WRK-HKN-TEKSTYMD
               MOVE    ONSKAKU-TEKEDYMD        TO  WRK-HKN-TEKEDYMD
           END-IF
           MOVE    ONSKAKU-GENDO-TEKSTYMD  TO  WRK-KOH-TEKSTYMD
           IF      (WRK-HKN-TEKSTYMD       =   ONSKAKU-GENDO-TEKSTYMD)
              OR   (ONSKAKU-GENDO-TEKSTYMD(7:2)    =   "01")
      *        ³«»ÏÆü¤¬Æ±¤¸¤«¡¢£±Æü»Ï¤Þ¤ê
               CONTINUE
           ELSE
      *        ³«»ÏÆü¤¬Åö·î¤Î»þ¡¢£±Æü
               IF      ONSKAKU-GENDO-TEKSTYMD(1:6) =   SPA-SYSYMD(1:6)
                   MOVE    "01"         TO  WRK-KOH-TEKSTYMD(7:2)
               END-IF
           END-IF
      *    ½ªÎ»Æü
           MOVE    ONSKAKU-GENDO-TEKEDYMD   TO  WRK-KOH-TEKEDYMD
      *    ·îËöÆü
           MOVE    SPACE               TO  WRK-MATU-DD
           IF      ONSKAKU-GENDO-TEKEDYMD   =   SPACE
                                            OR  ZERO
                                            OR  ALL "9"
               MOVE    ALL "9"              TO  WRK-KOH-TEKEDYMD
           ELSE
      *       ËöÆü¤òµá¤á¤ë
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-KOH-TEKEDYMD(1:6)   TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-MATU-DD
               END-IF
           END-IF
      *    ÊÝ¸±¤Î½ªÎ»Æü¤ÈÆ±¤¸¤«¡¢·îËöÆü
           IF     (ONSKAKU-GENDO-TEKEDYMD  =   WRK-HKN-TEKEDYMD)
              OR  (ONSKAKU-GENDO-TEKEDYMD(7:2) =   WRK-MATU-DD )
              OR  (WRK-KOH-TEKEDYMD        =   ALL "9"     )
              OR  (WRK-MATU-DD             =   SPACE   )
               CONTINUE
           ELSE
               MOVE    WRK-MATU-DD         TO  WRK-KOH-TEKEDYMD(7:2)
           END-IF
      *
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾Ú¤ÎÆüÉÕ
           MOVE    ONSKAKU-GENDO-TEKSTYMD  TO  LNK-ONSHI002-GENDO-STYMD
           MOVE    ONSKAKU-GENDO-TEKEDYMD  TO  LNK-ONSHI002-GENDO-EDYMD
      *    ¸øÈñ¤¢¤ê
           IF      WRK-KOHNUM      NOT =   SPACE
               ADD     1               TO  IDX-K
               MOVE    WRK-KOHNUM      TO  LNK-ONSHI002-KOHNUM(IDX-K)
               MOVE    WRK-KOH-JKYSNUM TO  LNK-ONSHI002-JKYSNUM
                                                              (IDX-K)
               MOVE    WRK-KOH-TEKSTYMD
                                       TO   LNK-ONSHI002-TEKSTYMD
                                                              (IDX-K)
               MOVE    WRK-KOH-TEKEDYMD
                                       TO   LNK-ONSHI002-TEKEDYMD
                                                              (IDX-K)
      *        ¸øÈñ¤¢¤ê
               MOVE    "1"             TO  LNK-ONSHI002-KOH-ARI
           END-IF
      *    Äã½êÆÀ£±
           IF      WRK-TEISYO-KBN      =   "1"
               MOVE    WRK-KOH-TEKSTYMD
                                       TO   LNK-ONSHI002-STK1-TEKSTYMD
               MOVE    WRK-KOH-TEKEDYMD
                                       TO   LNK-ONSHI002-STK1-TEKEDYMD
      *        Ç§ÄêÈÏ°Ï¶èÊ¬
               MOVE    WRK-TEISYO-HKNTEKKBN
                                       TO  LNK-ONSHI002-STK1-HKNTEKKBN
      *        Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¾Ú
               MOVE    WRK-TEISYO-RRIFUK
                                       TO  LNK-ONSHI002-STK1-RRIFUK
      *        ¶­³¦ÁØ³ºÅö¶èÊ¬
               MOVE    WRK-TEISYO-KYOUKAI
                                       TO  LNK-ONSHI002-STK1-KYOUKAI
      *        Äã½êÆÀ¤¢¤ê
               IF      LNK-ONSHI002-KOH-ARI    =   SPACE
                   MOVE    "2"             TO  LNK-ONSHI002-KOH-ARI
               END-IF
           END-IF
      *    Äã½êÆÀ£²
           IF      WRK-TEISYO-KBN      =   "2"
               MOVE    WRK-KOH-TEKSTYMD
                                       TO   LNK-ONSHI002-STK2-TEKSTYMD
               MOVE    WRK-KOH-TEKEDYMD
                                       TO   LNK-ONSHI002-STK2-TEKEDYMD
      *        Ç§ÄêÈÏ°Ï¶èÊ¬
               MOVE    WRK-TEISYO-HKNTEKKBN
                                       TO  LNK-ONSHI002-STK2-HKNTEKKBN
      *        Ä¹´üÆþ±¡³ºÅöÇ¯·îÆü
               MOVE    ONSKAKU-GENDO-SKJGNSTYMD
                                       TO  LNK-ONSHI002-STK2-SKJGNSTYMD
      *        ¶­³¦ÁØ³ºÅö¶èÊ¬
               MOVE    WRK-TEISYO-KYOUKAI
                                       TO  LNK-ONSHI002-STK2-KYOUKAI
      *        Äã½êÆÀ¤¢¤ê
               IF      LNK-ONSHI002-KOH-ARI    =   SPACE
                   MOVE    "2"             TO  LNK-ONSHI002-KOH-ARI
               END-IF
           END-IF
           .
        2201-GENDO-KOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¸ÂÅÙ³ÛÅ¬ÍÑ¶èÊ¬Ç§Äê¾Ú¡¡ÊÔ½¸½èÍý
      *****************************************************************
       22011-GENDO01-KOHHEN-SEC           SECTION.
      *
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾ÚÅ¬ÍÑ¶èÊ¬
           EVALUATE    ONSKAKU-GENDO-TEKKBN
               WHEN    "A01"
                   MOVE    "966"           TO  WRK-KOHNUM
                   MOVE    "¥¢"            TO  WRK-KOH-JKYSNUM
               WHEN    "A02"
                   MOVE    "966"           TO  WRK-KOHNUM
                   MOVE    "¥¤"            TO  WRK-KOH-JKYSNUM
               WHEN    "A03"
                   MOVE    "967"           TO  WRK-KOHNUM
                   MOVE    "¥¦"            TO  WRK-KOH-JKYSNUM
               WHEN    "A04"
                   MOVE    "967"           TO  WRK-KOHNUM
                   MOVE    "¥¨"            TO  WRK-KOH-JKYSNUM
               WHEN    "B01"
      *            ¸½ÌòÊÂ¤ß£³
                   CONTINUE
               WHEN    "B02"
      *            ¸½ÌòÊÂ¤ß£³
                   MOVE    "946"           TO  WRK-KOHNUM
                   MOVE    "£²"            TO  WRK-KOH-JKYSNUM
               WHEN    "B03"
      *            ¸½ÌòÊÂ¤ß£³
                   MOVE    "946"           TO  WRK-KOHNUM
                   MOVE    "£±"            TO  WRK-KOH-JKYSNUM
               WHEN    "B04"
      *            °ìÈÌ
                   CONTINUE
               WHEN    "B09"
      *            °ìÈÌ£²
                   CONTINUE
               WHEN    "B10"
      *            °ìÈÌ£±
                   CONTINUE
      *2022/12
               WHEN    OTHER
                   IF      ONSKAKU-GENDO-TEKKBN    NOT =   SPACE
                        MOVE    1              TO  FLG-GENNDO-ERR
                   END-IF
           END-EVALUATE
           .
       22011-GENDO01-KOHHEN-EXT.
           EXIT.
      *****************************************************************
      *    ¸ÂÅÙ³ÛÅ¬ÍÑ¡¦¸º³ÛÇ§Äê¾Ú¡¡ÊÔ½¸½èÍý
      *****************************************************************
       22012-GENDO02-KOHHEN-SEC              SECTION.
      *
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾ÚÅ¬ÍÑ¶èÊ¬
           EVALUATE    ONSKAKU-GENDO-TEKKBN
                WHEN    "A05"
      *         ¥ª 
                   MOVE    "967"           TO  WRK-KOHNUM
                   MOVE    "¥ª"            TO  WRK-KOH-JKYSNUM
      *            Äã½êÆÀ£² 
                   MOVE    "2"             TO  WRK-TEISYO-KBN
                   MOVE    "0"             TO  WRK-TEISYO-KYOUKAI
                   MOVE    "0"             TO  WRK-TEISYO-HKNTEKKBN
                WHEN    "A06"
      *         ¥ª¡Ê¶­¡Ë
                   MOVE    "967"           TO  WRK-KOHNUM
                   MOVE    "¥ª"            TO  WRK-KOH-JKYSNUM
      *            Äã½êÆÀ£² 
                   MOVE    "2"             TO  WRK-TEISYO-KBN
      *            ¶­³¦ÁØ
                   MOVE    "1"             TO  WRK-TEISYO-KYOUKAI
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "0"             TO  WRK-TEISYO-HKNTEKKBN
               WHEN    "B05"
      *            Äã½êÆÀ£²
                   MOVE    "2"             TO  WRK-TEISYO-KBN
      *            ¶­³¦ÁØ
                   MOVE    "0"             TO  WRK-TEISYO-KYOUKAI
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "0"             TO  WRK-TEISYO-HKNTEKKBN
              WHEN    "B06"
      *            Äã½êÆÀ£±
                   MOVE    "1"             TO  WRK-TEISYO-KBN
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "0"             TO  WRK-TEISYO-HKNTEKKBN
      *            ¶­³¦ÁØ
                   MOVE    "0"             TO  WRK-TEISYO-KYOUKAI
      *            Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¾Ú
                   MOVE    "0"             TO  WRK-TEISYO-RRIFUK 
               WHEN    "B07"
      *            Äã½êÆÀ£±¡ÊÏ·Ê¡¡Ë
                   MOVE    "1"             TO  WRK-TEISYO-KBN
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "0"             TO  WRK-TEISYO-HKNTEKKBN
      *            ¶­³¦ÁØ
                   MOVE    "0"             TO  WRK-TEISYO-KYOUKAI
      *            Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¾Ú
                   MOVE    "1"             TO  WRK-TEISYO-RRIFUK
               WHEN    "B08"
      *            Äã½êÆÀ£±¡Ê¶­¡Ë
                   MOVE    "1"             TO  WRK-TEISYO-KBN
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "0"             TO  WRK-TEISYO-HKNTEKKBN
      *            ¶­³¦ÁØ
                   MOVE    "1"             TO  WRK-TEISYO-KYOUKAI
      *            Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¾Ú
                   MOVE    "0"             TO  WRK-TEISYO-RRIFUK
      *2022/12
               WHEN    OTHER
                   IF      ONSKAKU-GENDO-TEKKBN    NOT =   SPACE
                        MOVE    1              TO  FLG-GENNDO-ERR
                   END-IF
           END-EVALUATE
           .
       22012-GENDO02-KOHHEN-EXT.
           EXIT.
      *****************************************************************
      *    É¸½àÉéÃ´³Û¸º³ÛÇ§Äê¾Ú¡¡ÊÔ½¸½èÍý
      *****************************************************************
       22013-GENDO03-KOHHEN-SEC              SECTION.
      *
      *    ¸ÂÅÙ³ÛÅ¬ÍÑÇ§Äê¾ÚÅ¬ÍÑ¶èÊ¬
           EVALUATE    ONSKAKU-GENDO-TEKKBN
                WHEN    "A05"
      *         ¥ª 
      *            Äã½êÆÀ£² 
                   MOVE    "2"             TO  WRK-TEISYO-KBN
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "3"             TO  WRK-TEISYO-HKNTEKKBN
                   MOVE    "0"             TO  WRK-TEISYO-KYOUKAI
               WHEN    "A06"
      *         ¥ª¡Ê¶­¡Ë
      *            Äã½êÆÀ£² 
                   MOVE    "2"             TO  WRK-TEISYO-KBN
      *            ¶­³¦ÁØ
                   MOVE    "1"             TO  WRK-TEISYO-KYOUKAI
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "3"             TO  WRK-TEISYO-HKNTEKKBN
               WHEN    "B05"
      *            Äã½êÆÀ£²
                   MOVE    "2"             TO  WRK-TEISYO-KBN
      *            ¶­³¦ÁØ
                   MOVE    "0"             TO  WRK-TEISYO-KYOUKAI
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "3"             TO  WRK-TEISYO-HKNTEKKBN
              WHEN    "B06"
      *            Äã½êÆÀ£±
                   MOVE    "1"             TO  WRK-TEISYO-KBN
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "3"             TO  WRK-TEISYO-HKNTEKKBN
      *            ¶­³¦ÁØ
                   MOVE    "0"             TO  WRK-TEISYO-KYOUKAI
      *            Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¾Ú
                   MOVE    "0"             TO  WRK-TEISYO-RRIFUK 
               WHEN    "B07"
      *            Äã½êÆÀ£±¡ÊÏ·Ê¡¡Ë
                   MOVE    "1"             TO  WRK-TEISYO-KBN
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "3"             TO  WRK-TEISYO-HKNTEKKBN
      *            ¶­³¦ÁØ
                   MOVE    "0"             TO  WRK-TEISYO-KYOUKAI
      *            Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¾Ú
                   MOVE    "1"             TO  WRK-TEISYO-RRIFUK
               WHEN    "B08"
      *            Äã½êÆÀ£±¡Ê¶­¡Ë
                   MOVE    "1"             TO  WRK-TEISYO-KBN
      *            Ç§ÄêÈÏ°Ï
                   MOVE    "3"             TO  WRK-TEISYO-HKNTEKKBN
      *            ¶­³¦ÁØ
                   MOVE    "1"             TO  WRK-TEISYO-KYOUKAI
      *            Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¾Ú
                   MOVE    "0"             TO  WRK-TEISYO-RRIFUK
      *2022/12
               WHEN    OTHER
                   IF      ONSKAKU-GENDO-TEKKBN    NOT =   SPACE
                        MOVE    1              TO  FLG-GENNDO-ERR
                   END-IF
           END-EVALUATE
           .
       22013-GENDO03-KOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆÃÄê¼ÀÉÂÎÅÍÜ¼õÎÅ¾ÚÄó¶¡ ¸øÈñÊÔ½¸½èÍý
      *****************************************************************
       2202-SIKKAN-KOHHEN-SEC        SECTION.
      * 
           MOVE    ZERO                   TO  IDX-SK2
           PERFORM VARYING    IDX-SK      FROM    1   BY  1
                   UNTIL      IDX-SK      >   3
              IF     (ONSKAKU-SIKKAN-SIPKBN(IDX-SK)   NOT =   SPACE )
               AND   (ONSKAKU-SIKKAN-FTNMONEY(IDX-SK) NOT =   SPACE )
                   MOVE    SPACE               TO  WRK-KOH-AREA
      *            ¿ôÃÍÊÑ´¹
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    ONSKAKU-SIKKAN-FTNMONEY(IDX-SK)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )   OR
                          (SNUM-MINKBN     NOT =   SPACE )   OR
                          (SNUM-SYOKBN     NOT =   SPACE )
                       CONTINUE
                   ELSE
                       IF      SNUM-OUTNUM      =   10000
                           MOVE    "972"        TO  WRK-KOHNUM
                       END-IF
                       IF      SNUM-OUTNUM      =   20000
                           MOVE    "974"        TO  WRK-KOHNUM
                       END-IF
                   END-IF
                   IF      WRK-KOHNUM      NOT =   SPACE
                       ADD     1               TO  IDX-K
                       MOVE    WRK-KOHNUM      TO  LNK-ONSHI002-KOHNUM
                                                             (IDX-K)
                       MOVE    ONSKAKU-SIKKAN-TEKSTYMD (IDX-SK)
                                           TO  LNK-ONSHI002-TEKSTYMD
                                                              (IDX-K)
                       IF      ONSKAKU-SIKKAN-TEKEDYMD (IDX-SK)
                                               =   SPACE
                           MOVE    ALL "9"
                                           TO  LNK-ONSHI002-TEKEDYMD
                                                              (IDX-K)
                       ELSE
                           MOVE    ONSKAKU-SIKKAN-TEKEDYMD (IDX-SK)
                                           TO  LNK-ONSHI002-TEKEDYMD
                                                              (IDX-K)
                       END-IF
                       MOVE    "1"         TO  LNK-ONSHI002-SIPKBN
                                                              (IDX-K)
      *                ¸øÈñ¤Ï£±·ï¤È¤¹¤ë
                       MOVE    3           TO  IDX-SK
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2202-SIKKAN-KOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆÃÄê¼ÀÉÂÎÅÍÜ¼õÎÅ¾ÚÄó¶¡ ¸øÈñÊÔ½¸½èÍý
      *****************************************************************
       230-ONSHIKAKU-CHIHOU-SEC        SECTION.
      *
      *    ÃÏÊý¸øÈñ¤Î¸¡º÷
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§UID¸¡º÷½èÍý
           INITIALIZE                      ONSKAKU-REC
           MOVE    LNK-ONSKAKU-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSKAKU-TBL-UUID
                                       TO  ONSKAKU-KOUHI-UUID
      *2024/01
      *    °åÎÅÉÞ½õ
           IF     (LNK-ONSKAKU-CARD-CLASS  =   "A1"  )
              MOVE    LNK-ONSKAKU-FUJYO-UUID
                                       TO  ONSKAKU-KOUHI-UUID
           END-IF
      *
      *    ¾È²ñ¶èÊ¬¡á£¹¡¡¤¬ÃÏÊý¸øÈñ
      *     MOVE    "9"                 TO  ONSKAKU-SHOKAI-KBN
      **    kouhi-uuid¤Ç¤Î¤ß¸¡º÷¤Ø
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
      ***  MOVE    "key12"             TO  MCP-PATHNAME
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           PERFORM UNTIL       (FLG-ONSKAKU   =   1  )
                         OR    (IDX-K         >=  10  )
      *        ÊÝ¸±¤ÎÀ¸Ç¯·îÆü¤ÈÆ±¤¸¤³¤È
               IF     (LNK-ONSKAKU-RES-BIRTHDAY
                                           =   ONSKAKU-RES-BIRTHDAY)
      *          ½èÍý·ë²Ì¶èÊ¬
                 AND  (ONSKAKU-RESULT-KBN  =   "1"  )
      *          ¸øÈñ¾È²ñ¤Î¤ß
                 AND  (ONSKAKU-SHOKAI-KBN  =   "8"
                                           OR  "9"  )
      *2022/12     ¸øÈñ²èÁü²èÌÌ¤«¤éÂÐ¾Ý³°ÀßÄê
                 IF     ONSKAKU-RES-HONKZKKBN  =   "9"
                   CONTINUE
                 ELSE 
      ****
                   ADD     1               TO  IDX-K
                   MOVE    IDX-K           TO  LNK-ONSHI002-KOHMAX
      *            ÉéÃ´¼ÔÈÖ¹æ
                   MOVE     ONSKAKU-RES-HKNJANUM
                                           TO   LNK-ONSHI002-FTNJANUM
                                                               (IDX-K)
      *            ÉéÃ´¼ÔÈÖ¹æ¤«¤é¸øÈñ¤Î¼ïÎà·èÄê
                   PERFORM 2301-FTNJANUM-KOHNUM-SEC
      *            ÊÝ¸±¼ÔÈÖ¹æ¤¢¤ê
                   IF      FLG-HKNJAINF        =   ZERO
                       MOVE    HKNJA-HKNNUM    TO  LNK-ONSHI002-KOHNUM
                                                         (IDX-K)
                   END-IF
      *            ¼õµë¼ÔÈÖ¹æ
                   MOVE     ONSKAKU-RES-NUM
                                           TO  LNK-ONSHI002-JKYSNUM
                                                             (IDX-K)
      *            »ñ³Ê¼èÆÀÆü
                   MOVE    ONSKAKU-KOH-TEKSTYMD (1)
                                           TO  LNK-ONSHI002-TEKSTYMD
                                                              (IDX-K)

                   IF     (ONSKAKU-KOH-TEKSTYMD(2)  NOT =   SPACE )
                      AND (ONSKAKU-KOH-TEKSTYMD(1)
                                           >   ONSKAKU-KOH-TEKSTYMD(2))
      *               ¾®¤µ¤¤Êý¤ò³«»ÏÆü
                       MOVE    ONSKAKU-KOH-TEKSTYMD (2)
                                           TO  LNK-ONSHI002-TEKSTYMD
                                                              (IDX-K)
                   END-IF
      *            ½ªÎ»Æü
                   MOVE    ONSKAKU-KOH-TEKEDYMD(1)
                                           TO  LNK-ONSHI002-TEKEDYMD
                                                             (IDX-K)
                   IF     (ONSKAKU-KOH-TEKEDYMD(2)  NOT =   SPACE )
                     AND  (ONSKAKU-KOH-TEKEDYMD(1)  NOT =   SPACE )
                     AND  (ONSKAKU-KOH-TEKEDYMD(1)
                                           <   ONSKAKU-KOH-TEKEDYMD(2))
                       MOVE    ONSKAKU-KOH-TEKEDYMD(2)
                                           TO  LNK-ONSHI002-TEKEDYMD
                                                             (IDX-K)
                   END-IF
                   IF      LNK-ONSHI002-TEKEDYMD(IDX-K)
                                           =   SPACE
                       MOVE    ALL "9"
                                           TO  LNK-ONSHI002-TEKEDYMD
                                                               (IDX-K)
                   END-IF
      *            ÃÏÊý¸øÈñ¶èÊ¬
                   MOVE    "1"             TO  LNK-ONSHI002-CHIKOU
                                                              (IDX-K)
      *            UUID
                   MOVE    ONSKAKU-TBL-UUID
                                           TO  LNK-ONSHI002-KOHUUID
                                                             (IDX-K)
      *            ÃÏÊý¸øÈñ¤¢¤ê
                   MOVE    "3"             TO  LNK-ONSHI002-KOH-ARI
      ****
                END-IF
      ****
               END-IF
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       230-ONSHIKAKU-CHIHOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉéÃ´¼ÔÈÖ¹æ¤«¤é¸øÈñ¤Î¼ïÎà·èÄê
      *****************************************************************
       2301-FTNJANUM-KOHNUM-SEC        SECTION.
      *
      *    ÊÝ¸±¼ÔÈÖ¹æ¡¡¸¡º÷¥Á¥§¥Ã¥¯
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    ONSKAKU-RES-HKNJANUM  TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ÊÝ¸±¼ÔÈÖ¹æ¤¢¤ê
      *     IF      FLG-HKNJAINF        =   ZERO
      *         MOVE    HKNJA-HKNNUM        TO  LNK-ONSHI002-KOHNUM
      *                                                   (IDX-K)
      *     END-IF
           .
      *
       2301-FTNJANUM-KOHNUM-EXT.
           EXIT.
      *
      *2024/01
      *****************************************************************
      *    ¥ª¥ó»ñ³Ê°åÎÅÉÞ½õ¸¡º÷É½¼¨ ¸øÈñÊÔ½¸½èÍý
      *****************************************************************
       240-ONSHIKAKU-IRYOFUJYO-SEC        SECTION.
      *
      *    MSG¤òºÆÊÔ½¸
           MOVE    ZERO                TO  IDX-FUJ
           INITIALIZE                  LNK-ONSHI002-FUJYO-G
      *    ÁªÂò¤¬°åÎÅÉÞ½õ¤Î»þ
           IF     (LNK-ONSKAKU-CARD-CLASS  =   "A1"  )
               MOVE    LNK-ONSKAKU-REC     TO  ONSKAKU-REC
               PERFORM 2401-KOHFUJYO-HEN-SEC
           END-IF
      *    °åÎÅÉÞ½õ¸¡º÷
           INITIALIZE                      ONSKAKU-REC
           MOVE    LNK-ONSKAKU-HOSPNUM     TO  ONSKAKU-HOSPNUM
      *?????
           IF     (LNK-ONSKAKU-CARD-CLASS  =   "A1"  )
      *        FUJYO-UUID ¤Ç¸¡º÷
               MOVE    LNK-ONSKAKU-FUJYO-UUID
                                           TO  ONSKAKU-FUJYO-UUID
               MOVE    LNK-ONSKAKU-KAKU-KAKUNINYMD
                                           TO  ONSKAKU-KAKU-KAKUNINYMD
               MOVE    "key27"             TO  WRK-MCP-PATHNAME
           ELSE
      *        KOUHI_UUID ¤Ç°åÎÅÉÞ½õ¤ò¸¡º÷
               MOVE    LNK-ONSKAKU-TBL-UUID
                                           TO  ONSKAKU-KOUHI-UUID
               MOVE    "key13"             TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           PERFORM UNTIL       (FLG-ONSKAKU   =   1  )
                         OR    (IDX-K         >=  10  )
      *        À¸Ç¯·îÆü¤ÈÆ±¤¸¤³¤È
               IF     (LNK-ONSKAKU-RES-BIRTHDAY
                                           =   ONSKAKU-RES-BIRTHDAY)
      *          ½èÍý·ë²Ì¶èÊ¬
                 AND  (ONSKAKU-RESULT-KBN  =   "1"  )
      *          °åÎÅÉÞ½õ¤Î¤ß
                 AND  (ONSKAKU-CARD-CLASS  =   "A1" )
                 AND  (ONSKAKU-TBL-UUID
                                       NOT =   LNK-ONSKAKU-TBL-UUID)
      *          °åÎÅÉÞ½õÂÐ±þ¸å
                 AND  (ONSKAKU-FUJYO-UUID
                                       NOT =   SPACE )
                   PERFORM 2401-KOHFUJYO-HEN-SEC
               END-IF
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       240-ONSHIKAKU-IRYOFUJYO-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅÉÞ½õ¸øÈñÊÔ½¸½èÍý
      *****************************************************************
       2401-KOHFUJYO-HEN-SEC          SECTION.
      *
           ADD     1                   TO  IDX-K
           MOVE    IDX-K               TO  LNK-ONSHI002-KOHMAX
      *    ÉéÃ´¼ÔÈÖ¹æ
           MOVE     ONSKAKU-RES-HKNJANUM
                                       TO   LNK-ONSHI002-FTNJANUM
                                                               (IDX-K)
      *    ÉéÃ´¼ÔÈÖ¹æ¤«¤é¸øÈñ¤Î¼ïÎà·èÄê
           PERFORM 2301-FTNJANUM-KOHNUM-SEC
      *    ÊÝ¸±¼ÔÈÖ¹æ¤¢¤ê
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNNUM    TO  LNK-ONSHI002-KOHNUM
                                                         (IDX-K)
           ELSE
      *        À¸³èÊÝ¸î¤È¤¹¤ë
               IF      ONSKAKU-RES-HKNJANUM(1:2)   =   "12"
                   MOVE    "012"           TO  LNK-ONSHI002-KOHNUM
                                                         (IDX-K)
               END-IF
           END-IF
      *    ¼õµë¼ÔÈÖ¹æ
           MOVE     ONSKAKU-RES-NUM    TO  LNK-ONSHI002-JKYSNUM
                                                             (IDX-K)
      *    »ñ³Ê¼èÆÀÆü
           MOVE    ONSKAKU-KOH-TEKSTYMD (1)
                                       TO  LNK-ONSHI002-TEKSTYMD
                                                              (IDX-K)
      *    ½ªÎ»Æü
           MOVE    ONSKAKU-KOH-TEKEDYMD(1)
                                       TO  LNK-ONSHI002-TEKEDYMD
                                                             (IDX-K)
           IF      LNK-ONSHI002-TEKEDYMD(IDX-K)
                                       =   SPACE
               MOVE    ALL "9"             TO  LNK-ONSHI002-TEKEDYMD
                                                               (IDX-K)
           END-IF
      *    °åÎÅÉÞ½õ¶èÊ¬
           MOVE    "4"                 TO  LNK-ONSHI002-CHIKOU
                                                              (IDX-K)
      *    UUID
           MOVE    ONSKAKU-TBL-UUID
                                       TO  LNK-ONSHI002-KOHUUID
                                                             (IDX-K)
      *    °åÎÅÉÞ½õ¤¢¤ê
           MOVE    "4"             TO  LNK-ONSHI002-KOH-ARI
      *    °åÎÅÉÞ½õ¤¢¤ê
           MOVE    "1"             TO  LNK-ONSHI002-FUJ-ARI
      *
      *
           ADD     1               TO  IDX-FUJ
           PERFORM 210131-FUJYO-MSGHEN-SEC
           PERFORM VARYING     IDX3    FROM    1   BY  1
                   UNTIL       IDX3    >   IDX-AID 
               MOVE    TBL-KOUFUNUM (IDX3)
                                       TO  LNK-ONSHI002-KOFUNUM
                                                          (IDX-K IDX3)
               MOVE    TBL-NYUGAIKBN (IDX3)
                                       TO  LNK-ONSHI002-NYUGAIKBN
                                                          (IDX-K IDX3)
               MOVE    TBL-SRYYM (IDX3)
                                       TO  LNK-ONSHI002-SRYYM
                                                          (IDX-K IDX3)
           END-PERFORM
           .
       240-ONSHIKAKU-IRYOFUJYO-EXT.
           EXIT.
      *****************************************************************
      *    À¾Îñ¢ªÏÂÎñFORMATÆüÉÕÊÔ½¸
      *****************************************************************
       8001-SEIWA-YMDEDIT-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
                                       OR  ZERO
                                       OR  ALL "9"
               MOVE    WRK-SYMD            TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               IF      STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
               ELSE
                   MOVE    SPACE               TO  WRK-HENYMD
               END-IF
           END-IF
           .
       8001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥ª¥ó¥é¥¤¥ó»ñ³Ê³ÎÇ§¾È²ñ·ë²ÌÆÉ¹þ
      *****************************************************************
       900-ONSKAKU-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ONSKAKU-REC
               MOVE    ZERO            TO  FLG-ONSKAKU
           ELSE
               INITIALIZE                  ONSKAKU-REC
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           .
       900-ONSKAKU-READ-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¼ÔÈÖ¹æ¥Þ¥¹¥¿¼¡¸¡º÷½èÍý
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO                TO  FLG-HKNJAINF
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ÊÝ¸±¼ÔÈÖ¹æ¥Þ¥¹¥¿¼¡¸¡º÷½èÍý
      *****************************************************************
       900-ONSAIDLST-READ-SEC              SECTION.
      *
           MOVE    "tbl_onshi_aidlst"  TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-AIDLST-REC
               MOVE    ZERO                TO  FLG-ONSAIDLST
           ELSE
               INITIALIZE                  ONSHI-AIDLST-REC
               MOVE    1                   TO  FLG-ONSAIDLST
           END-IF
           .
       900-ONSAIDLST-READ-EXT.
           EXIT.
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£ÂÆÉ¹þ¤ß½èÍý
      *****************************************************************
       920-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
