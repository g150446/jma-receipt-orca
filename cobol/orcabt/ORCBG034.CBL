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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBG034.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ·î¼¡Ä¢É¼
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : °åÎÅ¶èÊ¬½¸·×É½
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  22/11/17    ORCAMO        ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  05.01.01    ORCAMO       23.01.10  Áí¹ç·×¤Î·å¿ô¤òÁý¤ä¤¹
      *  05.02.01    ORCAMO       23.09.13  ¥Æ¥¹¥È´µ¼Ô¤òÂÐ¾Ý¤È¤·¤Ê¤¤
      *  05.02.02    ORCAMO       24/07/03  ÎáÏÂ£¶Ç¯ÅÙ²þÀµÂÐ±þ
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCMSL55.INC".
      *
      *    ¥¹¥ÑÎÎ°è
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-CHKEND              PIC 9(01).
           03  FLG-HEAD-PRT            PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(03).
           03  CNT-LINE                PIC 9(03).
           03  CNT-IN                  PIC 9(03).
      *
      *    ¥·¥¹¥Æ¥àÎÎ°è
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
           03  IDZ                     PIC 9(03).
           03  IDXX                    PIC 9(03).
      *
      *    ¥Ñ¥é¥á¥¿ÎÎ°è
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-SRYYM          PIC X(06).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-Z4                  PIC ZZZ9.
           03  WRK-Z6                  PIC ZZZZZ9.
      *
           03  WRK-END-DD.
               05  WRK-END-DD-9        PIC 9(02).
      *
       01  SUM-AREA.
           03  SUM-SRYCD-AREA.
      *¿ÇÎÅ½ê
      *(1)
               05  FILLER          PIC 9(09)   VALUE   190122010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Á¡¡¡¡".
               05  FILLER          PIC 9(09)   VALUE   190124310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Á¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190155210.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Á".
               05  FILLER          PIC 9(09)   VALUE   190155410.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Á¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC X(4472) VALUE   SPACE.
      *(2)         
               05  FILLER          PIC 9(09)   VALUE   190122110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Â¡¡¡¡".
               05  FILLER          PIC 9(09)   VALUE   190124410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Â¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190122210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Ã¡¡¡¡".
               05  FILLER          PIC 9(09)   VALUE   190124510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Ã¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190155610.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Â".
               05  FILLER          PIC 9(09)   VALUE   190155810.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Â¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190156010.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Ã".
               05  FILLER          PIC 9(09)   VALUE   190156210.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Ã¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC X(4300) VALUE   SPACE.
      *(3)
               05  FILLER          PIC 9(09)   VALUE   190122310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Ä¡¡¡¡".
               05  FILLER          PIC 9(09)   VALUE   190124610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Ä¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190122410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Å¡¡¡¡".
               05  FILLER          PIC 9(09)   VALUE   190124710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡´ðËÜÎÁ£Å¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190156410.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Ä".
               05  FILLER          PIC 9(09)   VALUE   190156610.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Ä¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190156810.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Å".
               05  FILLER          PIC 9(09)   VALUE   190157010.
               05  FILLER          PIC X(34)   VALUE
               "ÎÅ¾²°Ê³°¡¦Æþ±¡´ðËÜÎÁ£Å¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC X(4300) VALUE   SPACE.
      *ÉÂ±¡
      *(1)
               05  FILLER          PIC 9(09)   VALUE   190121310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Á¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190123710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Á¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Á¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Á¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Á¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Á¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190162210.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£Á¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190121410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Â¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190123810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Â¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Â¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Â¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Â¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Â¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190162410.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£Â¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190121510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Ã¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190123910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Ã¡Ë¡ÊÀ¸³è¡Ë".
              05  FILLER          PIC 9(09)   VALUE   190132810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ã¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ã¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ã¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ã¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190162610.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£Ã¡Ë".
      *
               05  FILLER          PIC X(3741) VALUE   SPACE.
      *(2)         
               05  FILLER          PIC 9(09)   VALUE   190121610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Ä¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190124010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Ä¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ä¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ä¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ä¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ä¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190162810.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£Ä¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190121710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Å¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190124110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Å¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Å¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Å¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190215910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Å¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Å¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190163010.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£Å¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190131610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Æ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190131710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Æ¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Æ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Æ¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Æ¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Æ¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190163210.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£Æ¡Ë".
      *
               05  FILLER          PIC X(3741) VALUE   SPACE.
      *(3)
               05  FILLER          PIC 9(09)   VALUE   190131810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Ç¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190131910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£Ç¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ç¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ç¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ç¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£Ç¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190163410.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£Ç¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£È¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£È¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£È¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190133910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£È¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£È¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£È¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190163610.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£È¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£É¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190132310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£É¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190134010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£É¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190134110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£É¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£É¡Ë¡Ê·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190216810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£É¡Ë¡ÊÀ¸³è¡¦·ÐÁ¼¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190163810.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡Ê£É¡Ë".
      *
               05  FILLER          PIC X(3741) VALUE   SPACE.
      *
      *    ÎáÏÂ£¶Ç¯£¶·î°Ê¹ß       
      *ÉÂ±¡
      *(1)
               05  FILLER          PIC 9(09)   VALUE   190817610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190817710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190823610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190823710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190814610.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190869810.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190817810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190817910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190823810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190823910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190814710.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190869910.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190818010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190818110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£³¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£³¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190814810.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870010.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£³¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190818210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190818310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£´¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£´¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190814910.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870110.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£´¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190818410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190818510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£µ¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£µ¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815010.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870210.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£µ¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190818610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190818710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¶¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£¶¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815110.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870310.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¶¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190818810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190818910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£·¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190824910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£·¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815210.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870410.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£·¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190819010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190819110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¸¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£¸¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815310.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870510.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¸¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190819210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190819310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¹¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£¹¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815410.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870610.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£¹¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190819410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190819510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£°¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£°¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815510.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870710.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£°¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190819610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190819710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£±¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£±¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815610.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870810.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£±¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190819810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190819910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£²¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190825910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£²¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815710.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190870910.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£²¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190821210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190821310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¹¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£¹¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816410.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871610.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¹¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190821410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190821510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£°¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£°¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816510.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871710.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£°¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190821610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190821710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£±¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£±¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816610.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£±¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871810.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£±¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190823010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190823110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¸¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190829010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190829110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£¸¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190817310.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190872510.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¸¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190823210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190823310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¹¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190829210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190829310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£¹¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190817410.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¹¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190872610.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¹¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190823410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£³£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190823510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£³£°¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190829410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£³£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190829510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£³£°¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190817510.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£³£°¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190872710.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£³£°¡Ë".
      *(2)
               05  FILLER          PIC 9(09)   VALUE   190820010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190820110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£³¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£³¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815810.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871010.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£³¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190820210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190820310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£´¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£´¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190815910.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871110.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£´¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190820410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190820510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£µ¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£µ¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816010.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871210.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£µ¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190820610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190820710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¶¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£¶¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816110.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871310.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¶¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190820810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190820910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£·¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190826910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£·¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816210.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871410.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£·¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190821010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190821110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¸¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£±£¸¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816310.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¸¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871510.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£±£¸¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190821810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190821910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£²¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190827910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£²¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816710.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£²¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190871910.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£²¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190822010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190822110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£³¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828010.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828110.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£³¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816810.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£³¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190872010.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£³¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190822210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190822310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£´¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828210.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828310.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£´¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190816910.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£´¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190872110.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£´¡Ë".
      *
               05  FILLER          PIC X(2322) VALUE   SPACE. 
      *(3)
               05  FILLER          PIC 9(09)   VALUE   190822410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190822510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£µ¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828410.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828510.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£µ¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190817010.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£µ¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190872210.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£µ¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190822610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190822710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¶¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828610.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828710.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£¶¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190817110.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¶¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190872310.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£¶¡Ë".
      *
               05  FILLER          PIC 9(09)   VALUE   190822810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190822910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£·¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828810.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190828910.
               05  FILLER          PIC X(34)   VALUE
               "Æþ±¡ÎÁ£²¡ÊÆþ±¡ÎÁ£²£·¡Ë¡ÊÀ¸³è¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190817210.
               05  FILLER          PIC X(34)   VALUE
               "°ìÈÌÉÂÅï¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£·¡Ë".
               05  FILLER          PIC 9(09)   VALUE   190872410.
               05  FILLER          PIC X(34)   VALUE
               "ÆÃÄê°ìÈÌ¡¦Æþ±¡ÎÁ£±¡ÊÆþ±¡ÎÁ£²£·¡Ë".
      *
               05  FILLER          PIC X(3870) VALUE   SPACE. 
      *
           03  SUM-SRYCD-AREA-R        REDEFINES  SUM-SRYCD-AREA.
      *         1:Í­¾²¿ÇÎÅ½ê 2:ÎÅÍÜÉÂÅï
      *         3:ÎÅÍÜÉÂÅï¡ÊÎáÏÂ£¶Ç¯£µ£¶°Ê¹ß¡Ë   
                05  SUM-SRYCD-TABLE-OCC    OCCURS  3.
                    07  SUM-SRYCD-TABLE    OCCURS  3.
                       09  SUM-SRYCD-OCC   OCCURS  108.
                           11  SUM-SRYCD   PIC 9(09).
                           11  SUM-NAME    PIC X(34).
      *
           03  SUM-CNT-AREA.
               05  SUM-CNT-TABLE       OCCURS  3.
                   07  SUM-CNT-OCC     OCCURS  108.
                      09  SUM-CNT      PIC 9(03)   OCCURS  31.
                   07  SUM-CNT-G       PIC 9(03)   OCCURS  31.
                   07  SUM-CNT-T-G     PIC 9(05).
      *
           03  SUM-SRYCD-MAX           PIC 9(03)   VALUE   108.
      *
       01  WRK-PRT-AREA.
           03  WRK-PRT-HEAD-1.
               05  FILLER              PIC X(50)   VALUE   SPACE.
               05  WRK-PRT-SRYYM       PIC X(16)   VALUE   SPACE.
               05  FILLER              PIC X(18)   VALUE   
                   "Ê¬¡¡°åÎÅ¶èÊ¬½¸·×É½".
               05  FILLER              PIC X(50)   VALUE   SPACE.
               05  FILLER              PIC X(08)   VALUE   "ºîÀ®Æü¡§".
               05  WRK-PRT-SYSYMD      PIC X(09)   VALUE   SPACE.
      *         03  FILLER              PIC X(04)   VALUE   "  P.".
      *         03  WRK-PRT-PAGE        PIC 9.
      *
           03  WRK-PRT-BODY-1.
               05  WRK-PRT-TITLE       PIC X(34)   VALUE   SPACE.
               05  FILLER              PIC X(40)   VALUE
               "   1   2   3   4   5   6   7   8   9  10".
               05  FILLER              PIC X(40)   VALUE
               "  11  12  13  14  15  16  17  18  19  20".
               05  FILLER              PIC X(32)   VALUE   
               "  21  22  23  24  25  26  27  28".
               05  WRK-PRT-B1-29.
                   07  FILLER          PIC X(04)   VALUE   "  29".
                   07  WRK-PRT-B1-30.
                       09  FILLER      PIC X(04)   VALUE   "  30".
                       09  WRK-PRT-B1-31
                                       PIC X(04)   VALUE   "  31".
      * 
           03  WRK-PRT-BODY-2.
               05  WRK-PRT-NAME        PIC X(34)   VALUE   SPACE.
               05  WRK-PRT-OCC         OCCURS  31.
                   07  WRK-PRT-CNT     PIC ZZZ9.
      *
           03  WRK-PRT-BODY-3.
               05  FILLER              PIC X(54)   VALUE
               "------------------------------------------------------".
               05  FILLER              PIC X(54)   VALUE
               "------------------------------------------------------".
               05  FILLER              PIC X(38)   VALUE
                "--------------------------------------".
              05  WRK-PRT-B3-29.
                   07  FILLER          PIC X(04)   VALUE   "----".
                   07  WRK-PRT-B3-30.
                       09  FILLER      PIC X(04)   VALUE   "----".
                       09  WRK-PRT-B3-31
                                       PIC X(04)   VALUE   "----".
      *
       01  CONST-AREA.
           03  CONST-R0606             PIC X(06)   VALUE   "202406".
           03  CONST-LINE-MAX          PIC 9(02)   VALUE   55.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    °åÎÅµ¡´Ø¾ðÊó
           COPY    "CPSK1001.INC".
      *
      *    ´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊó
           COPY    "CPSK1009.INC".
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    °õºþ´ÉÍý
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    °õºþÍÑ¥Ç¡¼¥¿
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    ´µ¼Ô¾ðÊó
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    °õºþ£Ä£Â¹¹¿·¥µ¥Ö
           COPY    "CPORCSPRT.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ²èÌÌÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSGDAY.INC".
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    °ì»þ¥Õ¥¡¥¤¥ëÌ¾ÊÔ½¸
           COPY    "CPORCSGETTEMP.INC".
      *
      *   ´µ¼ÔÈÖ¹æÊÑ´¹¥µ¥Ö
           COPY    "CPORCSPTID.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
                                       SUM-CNT-AREA
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-SRYYM
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG034"          TO  JOB-PGID
           MOVE    "°åÎÅ¶èÊ¬½¸·×É½"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
       *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  RECEERR
      *
      *    ¥Ñ¥é¥á¥¿ÊÔ½¸½èÍý
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    WRK-PARA-SRYYM      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-SDAY (1:6)    TO  WRK-PARA-SRYYM
      *
               MOVE    WRK-PARA-SRYYM      TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2)
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG (1:16)  TO  WRK-PRT-SRYYM
      *        ¿ÇÎÅÇ¯·î¤ÎËöÆü
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI (7:2)
                                           TO  WRK-END-DD
               EVALUATE    WRK-END-DD-9
                   WHEN    28
                       MOVE    SPACE       TO  WRK-PRT-B3-29
                                               WRK-PRT-B1-29
                   WHEN    29
                       MOVE    SPACE       TO  WRK-PRT-B3-30
                                               WRK-PRT-B1-30
                   WHEN    30
                       MOVE    SPACE       TO  WRK-PRT-B3-31
                                               WRK-PRT-B1-31
               END-EVALUATE
           ELSE
               MOVE    "¿ÇÎÅÇ¯·î¤¬¥¨¥é¡¼¤Ç¤¹"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ºîÀ®Æü¼èÆÀ
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
          MOVE    LNK-DAY2-EDTYMD1 TO  WRK-PRT-SYSYMD
      *
      *    °åÎÅµ¡´Ø¾ðÊó¥»¥Ã¥È
           PERFORM 900-SYS1001-DBREAD-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    SYS-1001-HOSPSBT
               WHEN    "1"
                   IF      WRK-PARA-SRYYM  >=  CONST-R0606
                       MOVE    3               TO  IDXX
                   ELSE
                       MOVE    2               TO  IDXX
                   END-IF    
               WHEN    "2"
                   MOVE    1               TO  IDXX
           END-EVALUATE
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       3
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       SUM-SRYCD-MAX
                       OR      SUM-SRYCD (IDXX IDX IDY) =   SPACE
                   DISPLAY "NYUINRYO=" SUM-SRYCD (IDXX IDX IDY)  " "
                                       SUM-NAME  (IDXX IDX IDY) 
                   PERFORM 900-NYUINACCT-DBSELECT-SEC
      *
                   PERFORM         UNTIL   FLG-NYUINACCT  =   1
                     INITIALIZE                      ORCSPTIDAREA
                     MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                     MOVE    NACCT-PTID          TO  SPTID-PTID
                     CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                         SPA-AREA
      *              ¥Æ¥¹¥È´µ¼Ô¤ÏÂÐ¾Ý³°
                     INITIALIZE                      PTINF-REC
                     MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                     MOVE    NACCT-PTID          TO  PTINF-PTID
                     PERFORM 900-PTINF-READ-SEC
                     IF      PTINF-TSTPTNUMKBN   =   "1"
                       DISPLAY "NG PTNUM=" SPTID-PTNUM 
                               " TSTPTNUMKBN="  PTINF-TSTPTNUMKBN
                     ELSE
                       DISPLAY "IN PTNUM=" SPTID-PTNUM 
      *
                       ADD     1           TO  CNT-IN
                       PERFORM VARYING IDZ FROM    1   BY  1
                               UNTIL   IDZ >       31
                           IF      NACCT-DAY (IDZ) >   0
                               ADD     1           TO
                                               SUM-CNT     (IDX IDY IDZ)
                                               SUM-CNT-G   (IDX IDZ)
                                               SUM-CNT-T-G (IDX)
                           END-IF
                       END-PERFORM
                     END-IF
      *
                     PERFORM 900-NYUINACCT-DBFETCH-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                   MOVE    "key69"            TO  MCP-PATHNAME
                   PERFORM 900-DBCLOSECURSOR-SEC
                   DISPLAY " "    
               END-PERFORM
           END-PERFORM 
      *
      *    Ä¢É¼ÊÔ½¸½èÍý
           IF      IDXX            =   3
      *        ÎáÏÂ£¶Ç¯£··î°Ê¹ß¤ÎÎÅÍÜÉÂÅï¤Î¤È¤­     
               PERFORM 2002-PRINT-SEC
           ELSE
               PERFORM 2001-PRINT-SEC 
           END-IF     
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸½èÍý
      *****************************************************************
       2001-PRINT-SEC             SECTION.
      *
      *****IF      CNT-IN          =   ZERO
      *****   GO  TO  2001-PRINT-EXT
      *****END-IF
      *
           MOVE    ZERO            TO  CNT-LINE
      * 
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       3
      ***          OR      SUM-SRYCD (IDXX IDX 1)  =   SPACE
               IF      CNT-LINE        =   ZERO
                   ADD     1               TO  CNT-PAGE
      *
                   MOVE    SPACE           TO  HCMSL55
                   ADD     1               TO  CNT-LINE
                   MOVE    WRK-PRT-HEAD-1  TO  HCMSL55-MOJI (CNT-LINE)
               END-IF    
      *
               EVALUATE    IDX
                   WHEN    1
                       MOVE    "¡Ú°åÎÅ¶èÊ¬£³¡Û¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡"
                                               TO  WRK-PRT-TITLE
                   WHEN    2
                       MOVE    "¡Ú°åÎÅ¶èÊ¬£²¡Û¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡"
                                               TO  WRK-PRT-TITLE
                   WHEN    3
                       MOVE    "¡Ú°åÎÅ¶èÊ¬£±¡Û¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡"
                                               TO  WRK-PRT-TITLE
               END-EVALUATE
               ADD     3               TO  CNT-LINE
               MOVE    WRK-PRT-BODY-1  TO  HCMSL55-MOJI (CNT-LINE)
               ADD     1               TO  CNT-LINE
               MOVE    WRK-PRT-BODY-3  TO  HCMSL55-MOJI (CNT-LINE)
      *
               MOVE    SPACE           TO  WRK-PRT-BODY-2
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       SUM-SRYCD-MAX
                       OR      SUM-SRYCD (IDXX IDX IDY) =   SPACE
                   MOVE    SUM-NAME (IDXX IDX IDY)
                                           TO  WRK-PRT-NAME
                   INSPECT WRK-PRT-NAME    REPLACING   ALL "  "    
                                           BY  "¡¡"
                   PERFORM VARYING IDZ     FROM    1   BY  1
                           UNTIL   IDZ     >       WRK-END-DD-9
                       MOVE    SUM-CNT (IDX IDY IDZ)
                                               TO  WRK-Z4
                       MOVE    WRK-Z4          TO  WRK-PRT-CNT (IDZ)
                   END-PERFORM
                   ADD     1               TO  CNT-LINE
                   MOVE    WRK-PRT-BODY-2  TO  HCMSL55-MOJI (CNT-LINE)
              END-PERFORM
      *
              ADD     1                TO  CNT-LINE
              MOVE    WRK-PRT-BODY-3   TO  HCMSL55-MOJI (CNT-LINE)
              MOVE    SPACE            TO  WRK-PRT-BODY-2
              MOVE    SUM-CNT-T-G (IDX)
                                       TO  WRK-Z6
              STRING  "¡¡¡¡¡¡¡¡¡¡¹ç·×¡Ê"   DELIMITED   BY  SIZE
                      WRK-Z6               DELIMITED   BY  SIZE
                      "¡Ë¡¡¡¡¡¡¡¡¡¡¡¡"     DELIMITED   BY  SIZE
                                       INTO      WRK-PRT-NAME
               END-STRING                        
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   IDZ     >       WRK-END-DD-9
                   MOVE    SUM-CNT-G (IDX IDZ)
                                           TO  WRK-Z4
                   MOVE    WRK-Z4          TO  WRK-PRT-CNT (IDZ)
               END-PERFORM
               ADD     1               TO  CNT-LINE
               MOVE    WRK-PRT-BODY-2  TO  HCMSL55-MOJI (CNT-LINE)
      *
      *        °õºþ½èÍý
               IF      IDXX            =   2  
               AND     IDX             =   2  
                    PERFORM 20011-PRINT-OUT-SEC
                   MOVE    ZERO            TO  CNT-LINE
               END-IF    
           END-PERFORM 
      *
      *    °õºþ½èÍý
           PERFORM 20011-PRINT-OUT-SEC
            .
       2001-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    Ä¢É¼ÊÔ½¸½èÍý¡ÊÎáÏÂ£¶Ç¯£¶·î°Ê¹ß¤ÎÎÅÍÜÉÂÅï¤Î¤È¤­¡Ë
      *****************************************************************
       2002-PRINT-SEC             SECTION.
      *
           MOVE    CONST-LINE-MAX  TO  CNT-LINE
      * 
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       3
               MOVE    1               TO  FLG-HEAD-PRT    
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       SUM-SRYCD-MAX
                       OR      SUM-SRYCD (IDXX IDX IDY) =   SPACE
                   IF      CNT-LINE        >=  CONST-LINE-MAX
                       IF      CNT-PAGE        >   0
      *                    °õºþ½èÍý
                           PERFORM 20011-PRINT-OUT-SEC
                       END-IF    
                       MOVE    ZERO            TO  CNT-LINE
                       ADD     1               TO  CNT-PAGE
      *
                       MOVE    SPACE           TO  HCMSL55
                       ADD     1               TO  CNT-LINE
                       MOVE    WRK-PRT-HEAD-1  TO
                                               HCMSL55-MOJI (CNT-LINE)
                       MOVE    1               TO  FLG-HEAD-PRT    
                   END-IF
      *
                   IF      FLG-HEAD-PRT    =   1
                       EVALUATE    IDX
                           WHEN    1
                               MOVE
                                   "¡Ú°åÎÅ¶èÊ¬£³¡Û¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡"
                                                   TO  WRK-PRT-TITLE
                           WHEN    2
                               MOVE
                                   "¡Ú°åÎÅ¶èÊ¬£²¡Û¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡"
                                                   TO  WRK-PRT-TITLE
                           WHEN    3
                               MOVE
                                   "¡Ú°åÎÅ¶èÊ¬£±¡Û¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡"
                                                   TO  WRK-PRT-TITLE
                       END-EVALUATE
                       ADD     3               TO  CNT-LINE
                       MOVE    WRK-PRT-BODY-1  TO  HCMSL55-MOJI
                                                           (CNT-LINE)
                       ADD     1               TO  CNT-LINE
                       MOVE    WRK-PRT-BODY-3  TO  HCMSL55-MOJI
                                                           (CNT-LINE)
      *
                       MOVE    ZERO            TO  FLG-HEAD-PRT    
                   END-IF    
      *
                   MOVE    SPACE           TO  WRK-PRT-BODY-2
                   MOVE    SUM-NAME (IDXX IDX IDY)
                                           TO  WRK-PRT-NAME
                   INSPECT WRK-PRT-NAME    REPLACING   ALL "  "    
                                           BY  "¡¡"
                   PERFORM VARYING IDZ     FROM    1   BY  1
                           UNTIL   IDZ     >       WRK-END-DD-9
                       MOVE    SUM-CNT (IDX IDY IDZ)
                                               TO  WRK-Z4
                       MOVE    WRK-Z4          TO  WRK-PRT-CNT (IDZ)
                   END-PERFORM
                   ADD     1               TO  CNT-LINE
                   MOVE    WRK-PRT-BODY-2  TO  HCMSL55-MOJI (CNT-LINE)
              END-PERFORM
      *       ¹ç·×¹ÔÊÔ½¸
              ADD     1                TO  CNT-LINE
              MOVE    WRK-PRT-BODY-3   TO  HCMSL55-MOJI (CNT-LINE)
              MOVE    SPACE            TO  WRK-PRT-BODY-2
              MOVE    SUM-CNT-T-G (IDX)
                                       TO  WRK-Z6
              STRING  "¡¡¡¡¡¡¡¡¡¡¹ç·×¡Ê"   DELIMITED   BY  SIZE
                      WRK-Z6               DELIMITED   BY  SIZE
                      "¡Ë¡¡¡¡¡¡¡¡¡¡¡¡"     DELIMITED   BY  SIZE
                                       INTO      WRK-PRT-NAME
               END-STRING                        
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   IDZ     >       WRK-END-DD-9
                   MOVE    SUM-CNT-G (IDX IDZ)
                                           TO  WRK-Z4
                   MOVE    WRK-Z4          TO  WRK-PRT-CNT (IDZ)
               END-PERFORM
               ADD     1               TO  CNT-LINE
               MOVE    WRK-PRT-BODY-2  TO  HCMSL55-MOJI (CNT-LINE)
           END-PERFORM 
      *
      *    °õºþ½èÍý
           PERFORM 20011-PRINT-OUT-SEC
            .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ä¢É¼°õºþ½èÍý
      *****************************************************************
       20011-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    SPACE               TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"       TO  SPRT-PRTID
           MOVE    "°åÎÅ¶èÊ¬½¸·×É½"    TO  SPRT-TITLE
           MOVE    HCMSL55             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN     =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20011-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *****IF      CNT-IN          =   ZERO
      *****    MOVE    "½èÍýÂÐ¾Ý¤Î¥Ç¡¼¥¿¤Ï¤¢¤ê¤Þ¤»¤ó¤Ç¤·¤¿"
      *****                            TO  WRK-RECEERR
      *****    PERFORM 500-ERR-HENSYU-SEC   
      *****END-IF
      *
           DISPLAY "*** ORCBG034 IN "      CNT-IN
           DISPLAY "*** ORCBG034 END ***"
      *     
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    À¾ÎñÆüËÜ¸ìÊÑ´¹½èÍý
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "¡¡"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF      STS-RECEERR     =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷
      *****************************************************************
       900-SYS1001-DBREAD-SEC      SECTION.
      *
      *    °åÎÅµ¡´Ø¾ðÊó¼èÆÀ
           INITIALIZE                      SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
      *    ´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊó
           INITIALIZE                      SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD  (1:6)
           MOVE    "01"                TO  SYS-1009-STYUKYMD  (7:2)
           MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD  
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      * 
            IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "´µ¼ÔÈÖ¹æ¹½À®´ÉÍý¾ðÊó¤¬¼èÆÀ¤Ç¤­¤Þ¤»¤ó¤Ç¤·¤¿"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       900-SYS1001-DBREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷
      *****************************************************************
       900-NYUINACCT-DBSELECT-SEC      SECTION.
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
           MOVE    SUM-SRYCD (IDXX IDX IDY)
                                       TO  NACCT-SRYCDTOTAL
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key69"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¸¡º÷
      *****************************************************************
       900-NYUINACCT-DBFETCH-SEC      SECTION.
      *
           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
           MOVE    "key69"            TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¸¡º÷
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *  
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
       *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-DBFETCH-SEC
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ã£Ì£Ï£Ó£Å½èÍý
      *****************************************************************
       900-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCLOSECURSOR-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           MOVE    LOW-VALUE       TO  MCP-TABLE
           MOVE    LOW-VALUE       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
