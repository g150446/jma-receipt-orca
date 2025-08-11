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
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.           ORAPION013R1V2.
      ******************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : APIÏ¢·ÈÍÑ¥â¥¸¥å¡¼¥ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : »ñ³Ê³ÎÇ§¡¡°åÎÅÉÞ½õ¸òÉÕÈÖ¹æÅÐÏ¿½èÍý
      *  ´ÉÍý¼Ô            :
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  24/02/XX    ORCAMO        ¿·µ¬ºîÀ®
      ******************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      ******************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-ONSKAKU             PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-ONSAIDLST           PIC 9(01).
      *
           03  FLG-HENERR              PIC 9(01).
      *
           03  FLG-ARI              PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDH1                    PIC 9(04).
           03  IDH2                    PIC 9(04).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-HOSPCD              PIC X(10).
           03  WRK-SIKIBETU            PIC X(50).
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-TBL-UUID            PIC X(36).
      *
           03  WRK-UID                 PIC X(36).
           03  WRK-RENNUM              PIC 9(03).
      *
           03  WRK-SRYYM               PIC X(06).
           03  WRK-PTID                PIC 9(10).
           03  WRK-MCP-PATHNAME        PIC X(16).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    »þ´ÖÊÔ½¸
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    ¥¨¥é¡¼¥³¡¼¥É
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(200).
      *    Á´³ÑÊÑ´¹ÍÑ 
           03  WRK-KANACHK-MAE-INPUT   PIC X(5000).
           03  WRK-KANACHK-OUT-INPUT   PIC X(5000).
           03  WRK-HENKAN-OT           PIC X(30).
           03  WRK-HENKAN-IN           PIC X(30).
      *
      *    Á´³ÑÈ¾³ÑÊÑ´¹¥Æ¡¼¥Ö¥ë
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
      *
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
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ¿ô»úÊÑ´¹ÎÎ°è
           COPY    "CPORCSNUM.INC".
      *    ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
      *    COPY    "CPORCSPTID.INC".
      *    È¾³Ñ¥Á¥§¥Ã¥¯¥µ¥Ö
           COPY    "CPORCSKANACHK.INC".
      *    ³ÈÄ¥´Á»ú¥Á¥§¥Ã¥¯¥µ¥Ö
      *     COPY    "CPORCSKANACHK2.INC".
      *    Á´³ÑÈ¾³ÑÊÑ´¹
           COPY    "CPORCSHANKCHG.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *    API XMLÆÉ¤ß¹þ¤ßÍÑÄêµÁÂÎ
           COPY    "CPONLINEQUAREQ9.INC".
      *    API XMLÆÉ¤ß¹þ¤ßÍÑÄêµÁÂÎ
           COPY    "CPONLINEQUARES1.INC".
      *    API XMLÆÉ¤ß¹þ¤ß¶¦ÄÌÄêµÁ
      *    COPY    "CPAPIV2REQ.INC".
      ***  COPY    "CPAPI21V3REQ.INC".
        01  XML-APIREQ.
            03  APIREQ-CONTEXT          PIC S9(9)   BINARY.
            03  APIREQ-OBJECT           PIC X(36).
            03  APIREQ-MODE             PIC S9(9)   BINARY.
            03  APIREQ-RECORDNAME       PIC X(128).
            03  APIREQ-PATIENTINFOREQ   PIC X(600000).
      *
           COPY    "MCPDATA2.INC".
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
      *    £Õ£É£Ä¼èÆÀÍÑ¥¨¥ê¥¢
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
           COPY    "CPSK1001.INC".
      *    ¿¦°÷¾ðÊó
           COPY    "CPSK1010.INC".
      *
      *     COPY    "CPSK1051.INC".
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *    ´µ¼Ô
      * 01  PTINF-REC.
      *     COPY    "CPPTINF.INC".
      *
      *    ¥ª¥ó»ñ³Ê³ÎÇ§¾ðÊó
       01  ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC".
      *
      *    °åÎÅÉÞ½õ¡¡¸òÉÕÈÖ¹æ
       01  ONSHI-AIDLST-REC.
           COPY    "CPONSHI-AIDLST.INC".
      *
      *
      *****************************************************************
      *    Ï¢ÍíÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA71SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "**********************"
           DISPLAY   "*  onlinequa13 start  *"
           DISPLAY   "**********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
      *    ½é´ü½èÍý
           PERFORM 100-INIT-SEC
      *    ¼ç½èÍý
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "999"
      *        ¥æ¡¼¥¶£É£Ä¥¨¥é¡¼
               MOVE   404                  TO  APILST05-HTTPSTATUS
           ELSE
      *        ÊÖµÑÎÎ°èÊÔ½¸
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "*   onlinequa13 end   *"
           DISPLAY   "**********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      XML-ONLINEQUAREQ9
      *
      *    ¹¹¿·Æü¼èÆÀ
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           STRING  "A+"                DELIMITED  BY  SIZE
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    ÆüÉÕ¡¦»þ´Ö½ÐÎÏÊÔ½¸
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  QUARES1-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  QUARES1-INFORMATION-TIME
      *
      *    °åÎÅµ¡´Ø¼±ÊÌÈÖ¹æ¥»¥Ã¥È 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "999"               TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      * 
      *    £Ó£Ð£Á¶¦ÄÌÀßÄê
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "E89"            TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRCD
      *
      *    XML¾ðÊó¼è¤ê½Ð¤·
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    ÆþÎÏ¹àÌÜ¥Á¥§¥Ã¥¯½èÍý
           PERFORM 2001-INPUT-CHK-SEC
      *
      *    °åÎÅÉÞ½õ¡¡¸òÉÕÈÖ¹æÅÐÏ¿½èÍý
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-ONSI-AIDLST-SYORI-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML¾ðÊó¤«¤éÆâÍÆ¤ò¼è¤ê½Ð¤¹
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST05-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAREQ9
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlineaidlstreq1"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST05-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlineaidlstreq1"  TO  APIREQ-RECORDNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      **** CALL    "ORCDBMAIN"         USING
      *                                MCPAREA
      *                                XML-APIREQ
      ****                             SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAREQ9
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML¾ðÊó½ñ¤­¹þ¤ß½èÍý
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        Àµ¾ï½ªÎ»
               MOVE    "000"               TO  QUARES1-API-RESULT
               MOVE    "½èÍý½ªÎ»"          TO  QUARES1-API-RESULT-MSG
           ELSE
               DISPLAY "300 WRK-ERRCD=" WRK-ERRCD
               MOVE    WRK-ERRCD           TO  QUARES1-API-RESULT
               MOVE    WRK-ERRMSG          TO  QUARES1-API-RESULT-MSG
            END-IF
      *
           IF      APILST05-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinequares1" TO  MCP-TABLE
           MOVE    1                   TO  QUARES1-MODE
           MOVE    "onlinequares1"     TO  QUARES1-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINEQUARES1
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  QUARES1-OBJECT    TO  APILST05-BODY
               MOVE  "application/xml" TO  APILST05-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    UID¥³¡¼¥É¼èÆÀ½èÍý
      *****************************************************************
       1001-UID-GET-SEC     SECTION.
      *
      *    UID¥³¡¼¥É¼èÆÀ½èÍý
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           .
       1001-UID-GET-EXT.
           EXIT.
      ****
      *****************************************************************
      *    ÆþÎÏ¹àÌÜ¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    ³ÎÇ§ÆüÉÕ
           MOVE    QUAREQ9-KAKU-KAKUNINYMD TO  WRK-HEN-DATE
      *     MOVE    SPACE               TO  WRK-HEN-TIME
      *     PERFORM 802-DAYHEN02-SEC
      *
      *    ÆüÉÕ¥Á¥§¥Ã¥¯¥µ¥Ö
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    QUAREQ9-KAKU-KAKUNINYMD
                                       TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "E01"               TO  WRK-ERRCD
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅÉÞ½õ¡¡¸òÉÕÈÖ¹æ¾ðÊó½èÍý
      *****************************************************************
       2002-ONSI-AIDLST-SYORI-SEC     SECTION.
      *
      *    »ñ³Ê³ÎÇ§¤«¤é´µ¼ÔÈÖ¹æ¼èÆÀ
           PERFORM 20021-ONSKAKU-CHEK-SEC
      *
      *    FUJYO_UUID ÅÐÏ¿ºÑ¤ß¤Ïºï½ü
           PERFORM 2003-SIKAKU-AIDLST-DELETE01-SEC
      *
      *    ¿ÇÎÅÇ¯·î¤ÇÅÐÏ¿ºÑ¤ß¤Ïºï½ü
           PERFORM 2003-SIKAKU-AIDLST-DELETE02-SEC
      *
           MOVE    ZERO               TO  WRK-RENNUM
           PERFORM VARYING     IDX    FROM    1   BY  1
                   UNTIL       IDX    >   2
               IF      QUAREQ9-RES-HKNJANUM (IDX)   NOT =   SPACE
                   MOVE    ZERO               TO  WRK-RENNUM
                   PERFORM VARYING     IDY    FROM    1   BY  1
                           UNTIL       IDY    >   10
      *                °åÎÅ·ô¤Ç¡¢Æþ±¡¡¦³°Íè
                       IF     (QUAREQ9-TICKET-TYPE (IDX IDY)  =   "01")
                         AND  (QUAREQ9-MEDICAL-TYPE (IDX IDY) =   "01"
                                                              OR  "02")
                           PERFORM 20021-ONS-AIDLST-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       2002-ONSI-AIDLST-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅÉÞ½õ¡¡¥ª¥ó»ñ³Ê¾ðÊó¤«¤é´µ¼ÔÈÖ¹æ·èÄê½èÍý
      *****************************************************************
       20021-ONSKAKU-CHEK-SEC      SECTION.
      *
      *    PTID ·èÄê
           MOVE    ZERO               TO  WRK-PTID
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM        TO  ONSKAKU-HOSPNUM
           MOVE    QUAREQ9-FUJYO-UUID TO  ONSKAKU-FUJYO-UUID
           MOVE    QUAREQ9-KAKU-KAKUNINYMD
                                       TO  ONSKAKU-KAKU-KAKUNINYMD
      *
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
           PERFORM UNTIL    FLG-ONSKAKU    =   1
      *        °åÎÅÉÞ½õ ¸øÈñ¾È²ñ¤ÎPTID
               IF      (ONSKAKU-CARD-CLASS     =   "A1" )
                   MOVE    ONSKAKU-PTID        TO  WRK-PTID
                   MOVE    1                   TO  FLG-ONSKAKU
               ELSE
                   MOVE    "key27"             TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20021-ONSKAKU-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    °åÎÅÉÞ½õ¡¡¸òÉÕÈÖ¹æ¾ðÊóÅÐÏ¿½èÍý
      *****************************************************************
       20021-ONS-AIDLST-HENSYU-SEC     SECTION.
      *
      *    UID¥³¡¼¥É¼èÆÀ½èÍý
           PERFORM 1001-UID-GET-SEC
           MOVE    C-UID               TO  WRK-UID
      *
           INITIALIZE                  ONSHI-AIDLST-REC
      *
           MOVE    SPA-HOSPNUM         TO  ONS-AIDLST-HOSPNUM
      *    UID
           MOVE    WRK-UID             TO  ONS-AIDLST-TBL-UUID
      *    FUJYO_UUID
           MOVE    QUAREQ9-FUJYO-UUID
                                       TO  ONS-AIDLST-FUJYO-UUID
      *    ÉéÃ´¼ÔÈÖ¹æ
           MOVE    QUAREQ9-RES-HKNJANUM (IDX)
                                       TO  ONS-AIDLST-FTNJANUM
      *    ¼õµë¼ÔÈÖ¹æ
           MOVE    QUAREQ9-RES-NUM (IDX) 
                                       TO  ONS-AIDLST-JKYSNUM
      *    ´µ¼ÔID
           IF      WRK-PTID        NOT =   ZERO
               MOVE    WRK-PTID            TO  ONS-AIDLST-PTID
           END-IF
      *    Ï¢ÈÖ
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  ONS-AIDLST-RENNUM
      *    °åÎÅ·ô¡¦Ä´ºÞ·ôÍ­¸ú³«»ÏÇ¯·îÆü
           MOVE    QUAREQ9-MDC-TEKSTYMD (IDX IDY)
                                       TO  ONS-AIDLST-TEKSTYMD
      *    °åÎÅ·ô¡¦Ä´ºÞ·ôÍ­¸ú½ªÎ»Ç¯·îÆü
           IF      QUAREQ9-MDC-TEKEDYMD (IDX IDY)  =   SPACE
               MOVE    ALL "9"         TO  ONS-AIDLST-TEKEDYMD
           ELSE
               MOVE    QUAREQ9-MDC-TEKEDYMD (IDX IDY)
                                       TO  ONS-AIDLST-TEKEDYMD
           END-IF
      *    ¿ÇÎÅÊÌ
           MOVE    QUAREQ9-MEDICAL-TYPE (IDX IDY)
                                       TO  ONS-AIDLST-MEDICAL-TYPE
      *    Æþ³°¶èÊ¬(¿ÇÎÅÊÌ)
           EVALUATE    QUAREQ9-MEDICAL-TYPE (IDX IDY)
               WHEN    "01"
                   MOVE    "1"         TO  ONS-AIDLST-NYUGAIKBN
               WHEN    "02"
                   MOVE    "2"         TO  ONS-AIDLST-NYUGAIKBN
           END-EVALUATE
      *    ¿ÇÎÅÇ¯·î
           MOVE    QUAREQ9-SRYYM (IDX IDY)
                                       TO  ONS-AIDLST-SRYYM
      *    ¸òÉÕÈÖ¹æ
           MOVE    QUAREQ9-KOFUNUM (IDX IDY)  TO  WRK-HENKAN-IN
           PERFORM 803-HANKAKU-HEN-SEC
           MOVE    WRK-HENKAN-OT       TO  ONS-AIDLST-KOFUNUM
      *    ËÜ¿Í»ÙÊ§³Û¡Ê¼«¸ÊÉéÃ´³Û¡Ë
           IF      QUAREQ9-FTNGAKU (IDX IDY)   =   SPACE
               MOVE    ZERO                TO  ONS-AIDLST-FTNGAKU
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAREQ9-FTNGAKU (IDX IDY)
                                           TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      ( SNUM-RC           =   ZERO  )
                    MOVE    SNUM-OUTNUM    TO  ONS-AIDLST-FTNGAKU
               END-IF
           END-IF
      *    ²èÌÌ¥Ç¡¼¥¿
           IF      WRK-RENNUM          =   1
               MOVE    QUAREQ9-IMAGE-FILE-STATUS(IDX)
                                       TO  ONS-AIDLST-IMAGE-STATUS
               MOVE    QUAREQ9-IMAGE-DATA (IDX)
                                       TO  ONS-AIDLST-IMAGE-DATA
           END-IF
      *    ÄÉ²Ã½èÍý
           PERFORM 20022-ONSHI-AIDLST-INS-SEC
           .
       20021-ONS-AIDLST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     »ñ³Ê³ÎÇ§¡¡°åÎÅÉÞ½õ¸òÉÕÈÖ¹æ¾ðÊóÅÐÏ¿½èÍý
      *****************************************************************
       20022-ONSHI-AIDLST-INS-SEC     SECTION.
      *
           MOVE    SPA-OPID            TO  ONS-AIDLST-OPID
      *
           MOVE    SMCNDATE-YMD        TO  ONS-AIDLST-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONS-AIDLST-CREHMS
           MOVE    SMCNDATE-YMD        TO  ONS-AIDLST-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-AIDLST-UPHMS
      *
           MOVE    ONSHI-AIDLST-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_aidlst"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "007 ONSHI-AIMAGE INSERT ERR:"  MCP-RC
                       ",KEY:" ONS-AIDLST-KEY
           END-IF
           .
       20022-ONSHI-AIDLST-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *     »ñ³Ê³ÎÇ§¡¡°åÎÅÉÞ½õ¸òÉÕÈÖ¹æ¾ðÊóºï½ü½èÍý
      *****************************************************************
       2003-SIKAKU-AIDLST-DELETE01-SEC SECTION.
      *
           INITIALIZE                  ONSHI-AIDLST-REC
           MOVE    SPA-HOSPNUM         TO  ONS-AIDLST-HOSPNUM
      *    FUJYO_UUID ¤Çºï½ü
           MOVE    QUAREQ9-FUJYO-UUID
                                       TO  ONS-AIDLST-FUJYO-UUID
      *
           MOVE    ONSHI-AIDLST-REC    TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_onshi_aidlst"  TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "81"                TO  WRK-ERRCD
               DISPLAY "007 ONSHI-AIDLST DELETE ERR:"  MCP-RC
                       ",KEY:" ONS-AIDLST-KEY
           END-IF
           .
      *    
       2003-SIKAKU-AIDLST-DELETE01-EXT.
           EXIT.
      *****************************************************************
      *     »ñ³Ê³ÎÇ§¡¡°åÎÅÉÞ½õ¸òÉÕÈÖ¹æ¾ðÊóºï½ü½èÍý
      *****************************************************************
       2003-SIKAKU-AIDLST-DELETE02-SEC     SECTION.
      *
           PERFORM VARYING     IDX    FROM    1   BY  1
                   UNTIL       IDX    >   2
               IF      QUAREQ9-RES-HKNJANUM (IDX)   NOT =   SPACE
                   MOVE    ZERO               TO  WRK-SRYYM
                   PERFORM VARYING     IDY    FROM    1   BY  1
                           UNTIL       IDY    >   10
      *                °åÎÅ·ô¤Ç¡¢Æþ±¡¡¦³°Íè
                       IF     (QUAREQ9-TICKET-TYPE (IDX IDY)  =   "01")
                         AND  (QUAREQ9-MEDICAL-TYPE(IDX IDY)  =   "01"
                                                              OR  "02")
                            IF     QUAREQ9-SRYYM (IDX IDY)
                                                   =   WRK-SRYYM
                                CONTINUE
                            ELSE
                                MOVE    QUAREQ9-SRYYM (IDX IDY)
                                                       TO  WRK-SRYYM
                                PERFORM 2003-SIKAKU-AIDLST-DEL02-SEC
                            END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       2003-SIKAKU-AIDLST-DELETE02-EXT.
           EXIT.
      *****************************************************************
      *     »ñ³Ê³ÎÇ§¡¡°åÎÅÉÞ½õ¸òÉÕÈÖ¹æ¾ðÊóºï½ü½èÍý
      *****************************************************************
       2003-SIKAKU-AIDLST-DEL02-SEC     SECTION.
      *
           INITIALIZE                  ONSHI-AIDLST-REC
           MOVE    SPA-HOSPNUM         TO  ONS-AIDLST-HOSPNUM
      *    ÉéÃ´¼ÔÈÖ¹æ
           MOVE    QUAREQ9-RES-HKNJANUM (IDX)
                                       TO  ONS-AIDLST-FTNJANUM
      *    ¼õµë¼ÔÈÖ¹æ
           MOVE    QUAREQ9-RES-NUM (IDX)
                                       TO  ONS-AIDLST-JKYSNUM
      *    ¿ÇÎÅÇ¯·î
           MOVE    QUAREQ9-SRYYM (IDX IDY)
                                       TO  ONS-AIDLST-SRYYM
      *    PTID¤Ê¤·
           IF      WRK-PTID            =   ZERO
               MOVE    "del2"              TO  WRK-MCP-PATHNAME
           ELSE
      *        PTID·èÄê
               MOVE    WRK-PTID            TO  ONS-AIDLST-PTID
               MOVE    "del3"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    ONSHI-AIDLST-REC    TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_onshi_aidlst"  TO  MCP-TABLE
      **** MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "81"                TO  WRK-ERRCD
               DISPLAY "007 ONSHI-AIDLST DELETE ERR:"  MCP-RC
                       ",KEY:" ONS-AIDLST-KEY
           END-IF
           .
      *    
       2003-SIKAKU-AIDLST-DELETE02-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    ÆüÉÕÊÑ´¹ÊÔ½¸½èÍý
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *     Á´³ÑÊÑ´¹½èÍý
      *****************************************************************
       800-KANACHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-KANACHK-OUT-INPUT
           IF      WRK-KANACHK-MAE-INPUT
                               NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   2    )
                   CONTINUE
               ELSE
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       800-KANACHK-EXT.
           EXIT.

      *****************************************************************
      *     È¾³ÑÊÑ´¹½èÍý
      *****************************************************************
       803-HANKAKU-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSHANKCHGAREA
           MOVE    "1"                 TO  HANKCHG-SYORI
           MOVE    "1"                 TO  HANKCHG-SYORIKBN
           MOVE     WRK-HENKAN-IN      TO  HANKCHG-MAE-INPUT
           CALL    "ORCSHANKCHG"       USING   ORCSHANKCHGAREA
           IF    ( KANACHK-RC          =   ZERO   )
               MOVE    HANKCHG-OUT-INPUT   TO  WRK-HENKAN-OT
           ELSE
      *        ±Ñ¿ôÃÍ°Ê³°¤¬¤¢¤ë
               MOVE    WRK-HENKAN-IN       TO  WRK-HENKAN-OT
           END-IF
           GO  TO  803-HANKAKU-HEN-EXT
      *
           MOVE    ZERO                TO  IDH2
           MOVE    SPACE               TO  WRK-HENKAN-OT
           MOVE    ZERO                TO  FLG-HENERR
           MOVE    ZERO                TO  FLG-ARI
      *
           PERFORM VARYING IDH1    FROM    1   BY  2
                   UNTIL  (IDH1        >   30    )
                   OR     (WRK-HENKAN-IN (IDX:) 
                                       =   SPACE)
                   OR     (IDH2        >=  15    )
               SET     TBL-IDX             TO  1
               SEARCH  TBL-HENKAN-OCC      VARYING   TBL-IDX
                   AT  END
                       MOVE    ZERO             TO  FLG-ARI
                   WHEN    WRK-HENKAN-IN (IDH1:2)   =   TBL-ZEN
                                                          (TBL-IDX)
                       ADD     1               TO  IDH2
                       MOVE    TBL-HALF  (TBL-IDX)
                                               TO  WRK-HENKAN-OT
                                                            (IDH2:1)
                       MOVE    1               TO  FLG-ARI
               END-SEARCH
               IF      FLG-ARI             =   ZERO
                   EVALUATE   WRK-HENKAN-IN (IDH1:2)
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
                           ADD     1           TO  IDH2
                           MOVE    "-"         TO  WRK-HENKAN-OT
                                                            (IDH2:1)
                           MOVE    1           TO  FLG-ARI 
                       WHEN    "¡¡"
                           ADD     1           TO  IDH2
                           MOVE    SPACE       TO  WRK-HENKAN-OT
                                                            (IDH2:1)
                           MOVE    1           TO  FLG-ARI 
                   END-EVALUATE
               END-IF
      *
               IF      FLG-ARI             =   ZERO
      *            ÂÐ¾Ý¤Ê¤·
                   ADD     1               TO  IDH2
                   MOVE    "?"             TO  WRK-HENKAN-OT
                                                            (IDH2:1)
               END-IF
           END-PERFORM
           .
       803-HANKAKU-HEN-EXT.
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
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿ÆÉ¹þ
      *****************************************************************
      * 900-SYSKANRI-READ-SEC         SECTION.
      *
      *     MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *     MOVE    "key10"             TO  MCP-PATHNAME
      *     PERFORM 91-DBSELECT-SEC
      *     IF      MCP-RC              =   ZERO
      *         MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *         MOVE    "key10"             TO  MCP-PATHNAME
      *         PERFORM 920-DBFETCH-SEC
      *         IF      MCP-RC              =   ZERO
      *             MOVE    ZERO                TO  FLG-SYSKANRI
      *         ELSE
      *             MOVE    1                   TO  FLG-SYSKANRI
      *         END-IF
      *     ELSE
      *         MOVE    1                   TO  FLG-SYSKANRI
      *     END-IF
      *
      *     MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *     MOVE    "key10"             TO  MCP-PATHNAME
      *     PERFORM 990-DBCLOSE-SEC
      *
      *     .
      * 900-SYSKANRI-READ-EXT.
      *     EXIT.
      *
      ******************************************************************
      *    »ñ³Ê³ÎÇ§ÆÉ¹þ
      ******************************************************************
      * 900-ONSKAKU-READ-SEC         SECTION.
      *
      *     MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
      *     PERFORM 920-DBFETCH-SEC
      *     IF      MCP-RC              =   ZERO
      *         MOVE    MCPDATA-REC         TO  ONSKAKU-REC
      *         MOVE    ZERO                TO  FLG-ONSKAKU
      *     ELSE
      *         INITIALIZE                      ONSKAKU-REC
      *         MOVE    1                   TO  FLG-ONSKAKU
      *     END-IF
      *
      *     .
      * 900-ONSKAKU-READ-EXT.
      *     EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       990-DBCLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¢¥¯¥»¥¹½èÍý
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.

