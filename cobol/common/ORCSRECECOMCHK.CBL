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
       PROGRAM-ID.             ORCSRECECOMCHK.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : 
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Á¥§¥Ã¥¯¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  18/04/19    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  05.00.01    ORCAMO       21/04/09  ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ¡Ê¿ÇÎÅ
      *                                     Êó½·¾åÎ×»þÅª¼è°·¡ËÂÐ±þ
      *  05.01.01    ORCAMO       22/01/06  ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´É
      *                                     ÍýÎÁÂÐ±þ
      *  05.01.02    ORCAMO       22/01/27  ÆÃÊÌË¬Ìä´Ç¸î»Ø¼¨²Ã»»ÂÐ±þ
      *  05.01.03    ORCAMO       22/03/16  ÎáÏÂ£´Ç¯£´·î²þÀµÂÐ±þ
      *  05.01.04    ORCAMO       22/06/06  Àº¿À²Ê¥Ç¥¤¥±¥¢ÅùÁá´ü²Ã»»ÂÐ±þ
      *  05.01.05    ORCAMO       22/06/10  ¼«Æ°µ­ºÜ¼è¤ê»ß¤á(112,113)
      *  05.01.06    ORCAMO       22/11/09  ½é¿ÇÎÁ¥³¡¼¥ÉÄÉ²ÃÂÐ±þ
      *  05.02.01    ORCAMO       23/05/01  ÎáÏÂ£µÇ¯£µ·î²þÀµÂÐ±þ
      *  05.02.02    ORCAMO       24/06/06  ÎáÏÂ£¶Ç¯£¶·î²þÀµÂÐ±þ
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
       01  CHK-SRYCD-TABLE-AREA.
      *    (1)
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD1-IDX      PIC 9(03)   VALUE   1.
           03  FILLER              PIC X(09)   VALUE   "111011810".
           03  FILLER              PIC X(09)   VALUE   "830000024".
           03  FILLER              PIC X(09)   VALUE   "830100002".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡¦Ê¸½ñ¤Ë¤è¤ë¾Ò²ð¤¬¤Ê¤¤´µ¼Ô¡Ë
           03  CHK-SRYCD1-IDX1     PIC 9(03)   VALUE   1.
           03  FILLER              PIC X(09)   VALUE   "111012610".
           03  FILLER              PIC X(09)   VALUE   "830000024".
           03  FILLER              PIC X(09)   VALUE   "830100002".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD1-IDX2     PIC 9(03)   VALUE   1.
           03  FILLER              PIC X(09)   VALUE   "111012810".
           03  FILLER              PIC X(09)   VALUE   "830000024".
           03  FILLER              PIC X(09)   VALUE   "830100002".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD1-IDX3     PIC 9(03)   VALUE   1.
           03  FILLER              PIC X(09)   VALUE   "111014510".
           03  FILLER              PIC X(09)   VALUE   "830100002".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆü£²²ÊÌÜ¡¦Ãí£²¤«¤é£´¤Ëµ¬Äê¤¹¤ë¾ì¹ç¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD1-IDX4     PIC 9(03)   VALUE   1.
           03  FILLER              PIC X(09)   VALUE   "111014610".
           03  FILLER              PIC X(09)   VALUE   "830100002".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD1-IDX5     PIC 9(03)   VALUE   1.
           03  FILLER              PIC X(09)   VALUE   "111014710".
           03  FILLER              PIC X(09)   VALUE   "830100002".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           03  CHK-SRYCD1-IDX6     PIC 9(03)   VALUE   1.
           03  FILLER              PIC X(09)   VALUE   "101110040".
           03  FILLER              PIC X(09)   VALUE   "830000024".
           03  FILLER              PIC X(09)   VALUE   "830100002".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÊ¸½ñ¤Ë¤è¤ë¾Ò²ð¤¬¤Ê¤¤´µ¼Ô¡Ë¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           03  CHK-SRYCD1-IDX7     PIC 9(03)   VALUE   1.
           03  FILLER              PIC X(09)   VALUE   "101110060".
           03  FILLER              PIC X(09)   VALUE   "830100002".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (2)
      *    ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD2-IDX      PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "112015810".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD2-IDX1     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "112017010".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD2-IDX2     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "112015950".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÅÅÏÃÅùÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD2-IDX3     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "112017150".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë
           03  CHK-SRYCD2-IDX4     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "112023550".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÅÅÏÃÅùÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê£²²ÊÌÜ¡¦£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë
           03  CHK-SRYCD2-IDX5     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "112023850".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD2-IDX6     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "112025210".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD2-IDX7     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "112025310".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           03  CHK-SRYCD2-IDX8     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "101120040".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           03  CHK-SRYCD2-IDX9     PIC 9(03)   VALUE   2.
           03  FILLER              PIC X(09)   VALUE   "101120080".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100003".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (3)
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD3-IDX      PIC 9(03)   VALUE   3.
           03  FILLER              PIC X(09)   VALUE   "112016210".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100004".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡¦Ê¸½ñ¾Ò²ð¿½½Ð´µ¼Ô¡Ë
           03  CHK-SRYCD3-IDX1     PIC 9(03)   VALUE   3.
           03  FILLER              PIC X(09)   VALUE   "112016410".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100004".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD3-IDX2     PIC 9(03)   VALUE   3.
           03  FILLER              PIC X(09)   VALUE   "112017610".
           03  FILLER              PIC X(09)   VALUE   "830000051".
           03  FILLER              PIC X(09)   VALUE   "830100004".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD3-IDX3     PIC 9(03)   VALUE   3.
           03  FILLER              PIC X(09)   VALUE   "112025910".
           03  FILLER              PIC X(09)   VALUE   "830100004".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±ÆüÊ£¿ô²Ê£²²ÊÌÜ¡¦Ê¸½ñ¾Ò²ð¿½½Ð´µ¼Ô¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD3-IDX4     PIC 9(03)   VALUE   3.
           03  FILLER              PIC X(09)   VALUE   "112026010".
           03  FILLER              PIC X(09)   VALUE   "830100004".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD3-IDX5     PIC 9(03)   VALUE   3.
           03  FILLER              PIC X(09)   VALUE   "112026110".
           03  FILLER              PIC X(09)   VALUE   "830100004".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (4)
      *    ËýÀ­áÖÄË¼À´µ´ÉÍýÎÁ
           03  CHK-SRYCD4-IDX      PIC 9(03)   VALUE   4.
           03  FILLER              PIC X(09)   VALUE   "113006510".
           03  FILLER              PIC X(09)   VALUE   "850100059".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (5)
      *    ½ýÉÂ¼êÅö¶â°Õ¸«½ñ¸òÉÕÎÁ
           03  CHK-SRYCD5-IDX      PIC 9(03)   VALUE   5.
           03  FILLER              PIC X(09)   VALUE   "180000710".
           03  FILLER              PIC X(09)   VALUE   "840000011".
           03  FILLER              PIC X(09)   VALUE   "840000012".
           03  FILLER              PIC X(09)   VALUE   "840000511".
           03  FILLER              PIC X(09)   VALUE   "840000512".
           03  FILLER              PIC X(09)   VALUE   "850100089".
           03  FILLER              PIC X(135)  VALUE   SPACE.
      *    (6)
      *    ÎÅÍÜÈñÆ±°Õ½ñ¸òÉÕÎÁ
           03  CHK-SRYCD6-IDX      PIC 9(03)   VALUE   6.
           03  FILLER              PIC X(09)   VALUE   "113004310".
           03  FILLER              PIC X(09)   VALUE   "840000078".
           03  FILLER              PIC X(09)   VALUE   "840000578".
           03  FILLER              PIC X(09)   VALUE   "850100090".
           03  FILLER              PIC X(153)  VALUE   SPACE.
      *    (7)
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ£±¡Ê½é²ó¡Ë
           03  CHK-SRYCD7-IDX      PIC 9(03)   VALUE   7.
           03  FILLER              PIC X(09)   VALUE   "113008310".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100064".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ£±¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë¡ÊÂÐÌÌ¡Ë
           03  CHK-SRYCD7-IDX1     PIC 9(03)   VALUE   7.
           03  FILLER              PIC X(09)   VALUE   "113008410".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100064".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ£±¡Ê£µ²óÌÜ¡Ë
           03  CHK-SRYCD7-IDX2     PIC 9(03)   VALUE   7.
           03  FILLER              PIC X(09)   VALUE   "113008510".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100064".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ£±¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD7-IDX3     PIC 9(03)   VALUE   7.
           03  FILLER              PIC X(09)   VALUE   "113031610".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100064".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ£±¡Ê½é²ó¡Ë¡Ê¿ÇÎÅÊó½·¾åÎ×»þÅª¼è°·¡Ë
           03  CHK-SRYCD7-IDX4     PIC 9(03)   VALUE   7.
           03  FILLER              PIC X(09)   VALUE   "113033450".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100064".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ£±¡Ê£µ²óÌÜ¡Ë¡Ê¿ÇÎÅÊó½·¾åÎ×»þÅª¼è°·¡Ë
           03  CHK-SRYCD7-IDX5     PIC 9(03)   VALUE   7.
           03  FILLER              PIC X(09)   VALUE   "113033550".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100064".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (8)
      *    ½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¡Ê£±·îÌÜ¡Ë
           03  CHK-SRYCD8-IDX      PIC 9(03)   VALUE   8.
           03  FILLER              PIC X(09)   VALUE   "113008070".
           03  FILLER              PIC X(09)   VALUE   "840000098".
           03  FILLER              PIC X(09)   VALUE   "850100058".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ½ÅÅÙÓÃÂ©´µ¼Ô¼£ÎÅ´ÉÍý²Ã»»¡Ê£²·îÌÜ°Ê¹ß£¶·îÌÜ¤Þ¤Ç¡Ë
           03  CHK-SRYCD8-IDX1     PIC 9(03)   VALUE   8.
           03  FILLER              PIC X(09)   VALUE   "113008170".
           03  FILLER              PIC X(09)   VALUE   "840000098".
           03  FILLER              PIC X(09)   VALUE   "850100058".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (9)
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡Ë¡Ê·î¤Î£±²óÌÜ¡Ë
           03  CHK-SRYCD9-IDX      PIC 9(03)   VALUE   9.
           03  FILLER              PIC X(09)   VALUE   "113000810".
           03  FILLER              PIC X(09)   VALUE   "840000104".
           03  FILLER              PIC X(09)   VALUE   "840000604".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(153)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡Ë¡Ê½é²ó¡Ë
           03  CHK-SRYCD9-IDX1     PIC 9(03)   VALUE   9.
           03  FILLER              PIC X(09)   VALUE   "113702610".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡¦½é²ó¡Ë¡ÊÄÌ¿®µ¡¡Ë
           03  CHK-SRYCD9-IDX2     PIC 9(03)   VALUE   9.
           03  FILLER              PIC X(09)   VALUE   "113703210".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (10)
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡Ë¡Ê·î¤Î£²²óÌÜ¡Ë
           03  CHK-SRYCD10-IDX     PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113009910".
           03  FILLER              PIC X(09)   VALUE   "840000104".
           03  FILLER              PIC X(09)   VALUE   "840000604".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(153)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê¸øÇ§¿´Íý»Õ¡Ë
           03  CHK-SRYCD10-IDX1    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113029410".
           03  FILLER              PIC X(09)   VALUE   "840000104".
           03  FILLER              PIC X(09)   VALUE   "840000604".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(153)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡Ë¡Ê£±Ç¯°ÊÆâ¡¦·î¤Î£±²óÌÜ¡Ë
           03  CHK-SRYCD10-IDX2    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113702710".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡Ë¡Ê£±Ç¯°ÊÆâ¡¦·î¤Î£²²óÌÜ¡Ë
           03  CHK-SRYCD10-IDX3    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113702810".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡Ë¡Ê£²Ç¯°ÊÆâ¡¦·î¤Î£±²óÌÜ¡Ë
           03  CHK-SRYCD10-IDX4    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113702910".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡Ë¡Ê£²Ç¯°ÊÆâ¡¦·î¤Î£²²óÌÜ¡Ë
           03  CHK-SRYCD10-IDX5    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113703010".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡Ë¡Ê£´Ç¯°ÊÆâ¡Ë
           03  CHK-SRYCD10-IDX6    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113703110".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡¦£±Ç¯°ÊÆâ¡¦£±²óÌÜ¡Ë¡ÊÄÌ¿®µ¡¡Ë
           03  CHK-SRYCD10-IDX7    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113703310".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡¦£±Ç¯°ÊÆâ¡¦£²²óÌÜ¡Ë¡ÊÄÌ¿®µ¡¡Ë
           03  CHK-SRYCD10-IDX8    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113703410".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡¦£²Ç¯°ÊÆâ¡¦£±²óÌÜ¡Ë¡ÊÄÌ¿®µ¡¡Ë
           03  CHK-SRYCD10-IDX9    PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113703510".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡¦£²Ç¯°ÊÆâ¡¦£²²óÌÜ¡Ë¡ÊÄÌ¿®µ¡¡Ë
           03  CHK-SRYCD10-IDX10   PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113703610".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¾®»ùÆÃÄê¼À´µ¥«¥¦¥ó¥»¥ê¥ó¥°ÎÁ¡Ê°å»Õ¡¦£´Ç¯°ÊÆâ¡Ë¡ÊÄÌ¿®µ¡¡Ë
           03  CHK-SRYCD10-IDX11   PIC 9(03)   VALUE   10.
           03  FILLER              PIC X(09)   VALUE   "113703710".
           03  FILLER              PIC X(09)   VALUE   "850100051".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (11)
      *    Ç§ÃÎ¾ÉÀìÌç¿ÇÃÇ´ÉÍýÎÁ£²
           03  CHK-SRYCD11-IDX     PIC 9(03)   VALUE   11.
           03  FILLER              PIC X(09)   VALUE   "113014610".
           03  FILLER              PIC X(09)   VALUE   "850190004".
           03  FILLER              PIC X(09)   VALUE   "820190004".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (12)
      *    ¾®»ù±¿Æ°´ï¼À´µ»ØÆ³´ÉÍýÎÁ
           03  CHK-SRYCD12-IDX     PIC 9(03)   VALUE   12.
           03  FILLER              PIC X(09)   VALUE   "113024310".
           03  FILLER              PIC X(09)   VALUE   "850190002".
           03  FILLER              PIC X(09)   VALUE   "820190002".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (13)
      *    Ç§ÃÎ¾É¥µ¥Ý¡¼¥È»ØÆ³ÎÁ
           03  CHK-SRYCD13-IDX     PIC 9(03)   VALUE   13.
           03  FILLER              PIC X(09)   VALUE   "113028310".
           03  FILLER              PIC X(09)   VALUE   "850190005".
           03  FILLER              PIC X(09)   VALUE   "820190005".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (14)
      *    ÆÃÊÌ´ÉÍý»ØÆ³²Ã»»¡ÊÂà±¡»þ¶¦Æ±»ØÆ³ÎÁ£±¡Ë
           03  CHK-SRYCD14-IDX     PIC 9(03)   VALUE   14.
           03  FILLER              PIC X(09)   VALUE   "113014370".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (15)
      *    Âà±¡»þ¶¦Æ±»ØÆ³ÎÁ£±¡Ê£±°Ê³°¡Ë
           03  CHK-SRYCD15-IDX     PIC 9(03)   VALUE   15.
           03  FILLER              PIC X(09)   VALUE   "113008710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Âà±¡»þ¶¦Æ±»ØÆ³ÎÁ£±¡ÊºßÂðÎÅÍÜ»Ù±ç¿ÇÎÅ½ê¡Ë
           03  CHK-SRYCD15-IDX1    PIC 9(03)   VALUE   15.
           03  FILLER              PIC X(09)   VALUE   "113008610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (16)
      *    ³°Íè¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó¿ÇÎÅÎÁ£±
           03  CHK-SRYCD16-IDX     PIC 9(03)   VALUE   16.
           03  FILLER              PIC X(09)   VALUE   "113013910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ³°Íè¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó¿ÇÎÅÎÁ£²
           03  CHK-SRYCD16-IDX1    PIC 9(03)   VALUE   16.
           03  FILLER              PIC X(09)   VALUE   "113014010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ³°ÍèÊü¼ÍÀþ¾È¼Í¿ÇÎÅÎÁ
           03  CHK-SRYCD16-IDX2    PIC 9(03)   VALUE   16.
           03  FILLER              PIC X(09)   VALUE   "113014110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (17)
      *    ¿Í¹©Æâ¼ªµ¡´ïÄ´À°²Ã»»¡Ê£¶ºÐÌ¤Ëþ¡Ë
           03  CHK-SRYCD17-IDX     PIC 9(03)   VALUE   17.
           03  FILLER              PIC X(09)   VALUE   "113030670".
           03  FILLER              PIC X(09)   VALUE   "850190043".
           03  FILLER              PIC X(09)   VALUE   "820190043".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (18)
      *    ¿Í¹©Æâ¼ªµ¡´ïÄ´À°²Ã»»¡Ê£¶ºÐ°Ê¾å¡Ë
           03  CHK-SRYCD18-IDX     PIC 9(03)   VALUE   18.
           03  FILLER              PIC X(09)   VALUE   "113030770".
           03  FILLER              PIC X(09)   VALUE   "850190043".
           03  FILLER              PIC X(09)   VALUE   "820190043".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (19)
      *    ³°ÍèÇÓÇ¢¼«Î©»ØÆ³ÎÁ
           03  CHK-SRYCD19-IDX     PIC 9(03)   VALUE   19.
           03  FILLER              PIC X(09)   VALUE   "113032810".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "850100082".
           03  FILLER              PIC X(09)   VALUE   "842100030".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    (20)
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¥¤¡¦¿¢¹þ·¿½üºÙÆ°´ï°Ü¹Ô´ü²Ã»»´Þ¤à¡Ë
           03  CHK-SRYCD20-IDX     PIC 9(03)   VALUE   20.
           03  FILLER              PIC X(09)   VALUE   "113015130".
           03  FILLER              PIC X(09)   VALUE   "850100054".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (21)
      *    ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡Ê¥Ú¡¼¥¹¥á¡¼¥«¡¼¡Ë
           03  CHK-SRYCD21-IDX     PIC 9(03)   VALUE   21.
           03  FILLER              PIC X(09)   VALUE   "113030470".
           03  FILLER              PIC X(09)   VALUE   "850190001".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (22)
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¥Ú¡¼¥¹¥á¡¼¥«¡¼¡Ë
           03  CHK-SRYCD22-IDX     PIC 9(03)   VALUE   22.
           03  FILLER              PIC X(09)   VALUE   "113030210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (23)
      *    ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡Ê¿¢¹þ·¿½üºÙÆ°´ïÅù¡Ë
           03  CHK-SRYCD23-IDX     PIC 9(03)   VALUE   23.
           03  FILLER              PIC X(09)   VALUE   "113030570".
           03  FILLER              PIC X(09)   VALUE   "850190001".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (24)
      *    ¿´Â¡¥Ú¡¼¥¹¥á¡¼¥«¡¼»ØÆ³´ÉÍýÎÁ¡Ê¿¢¹þ·¿½üºÙÆ°´ïÅù¡Ë
           03  CHK-SRYCD24-IDX     PIC 9(03)   VALUE   24.
           03  FILLER              PIC X(09)   VALUE   "113030310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (25)
      *    ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡ÊºßÂð»ÀÁÇÎÅË¡»ØÆ³´ÉÍýÎÁ¡Ê¤½¤ÎÂ¾¡Ë¡Ë
           03  CHK-SRYCD25-IDX     PIC 9(03)   VALUE   25.
           03  FILLER              PIC X(09)   VALUE   "114045470".
           03  FILLER              PIC X(09)   VALUE   "850100140".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (26)
      *    ºßÂð»ÀÁÇÎÅË¡»ØÆ³´ÉÍýÎÁ¡Ê¤½¤ÎÂ¾¡Ë
           03  CHK-SRYCD26-IDX     PIC 9(03)   VALUE   26.
           03  FILLER              PIC X(09)   VALUE   "114003710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð»ÀÁÇÎÅË¡»ØÆ³´ÉÍýÎÁ¡Ê¤½¤ÎÂ¾¡Ë¡ÊÆÃÎã¡Ë
           03  CHK-SRYCD26-IDX1    PIC 9(03)   VALUE   26.
           03  FILLER              PIC X(09)   VALUE   "114055550".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (27)
      *    ±ó³Ö¥â¥Ë¥¿¥ê¥ó¥°²Ã»»¡ÊºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£²¡Ë
           03  CHK-SRYCD27-IDX     PIC 9(03)   VALUE   27.
           03  FILLER              PIC X(09)   VALUE   "114045670".
           03  FILLER              PIC X(09)   VALUE   "850100145".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (28)
      *    ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£²
           03  CHK-SRYCD28-IDX     PIC 9(03)   VALUE   28.
           03  FILLER              PIC X(09)   VALUE   "114040810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (29)
      *    ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ¡Ê°å»Õ¡¦´Ç¸î»Õ¤¬¿´ÍýÅªÉÔ°Â·Ú¸º¤Î¤¿¤áÌÌÀÜ¡Ë
           03  CHK-SRYCD29-IDX     PIC 9(03)   VALUE   29.
           03  FILLER              PIC X(09)   VALUE   "113015310".
           03  FILLER              PIC X(09)   VALUE   "850100060".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ¡Ê¿´ÍýÅªÉÔ°Â·Ú¸º¤Î¤¿¤áÌÌÀÜ¡¦¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD29-IDX1    PIC 9(03)   VALUE   29.
           03  FILLER              PIC X(09)   VALUE   "113035410".
           03  FILLER              PIC X(09)   VALUE   "850100060".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (30)
      *    ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ¡Ê°å»ÕÅù¤¬¹³°­À­¼ðáçºÞ¤ÎÉ¬Í×À­Åù¤òÊ¸½ñÀâÌÀ¡Ë
           03  CHK-SRYCD30-IDX     PIC 9(03)   VALUE   30.
           03  FILLER              PIC X(09)   VALUE   "113015410".
           03  FILLER              PIC X(09)   VALUE   "850100061".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¤¬¤ó´µ¼Ô»ØÆ³´ÉÍýÎÁ¡Ê¹³°­À­¼ðáçºÞ¤ÎÉ¬Í×À­Ê¸½ñÀâÌÀ¡¦¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD30-IDX1    PIC 9(03)   VALUE   30.
           03  FILLER              PIC X(09)   VALUE   "113035510".
           03  FILLER              PIC X(09)   VALUE   "850100061".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (31)
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê½é²ó¡Ë
           03  CHK-SRYCD31-IDX     PIC 9(03)   VALUE   31.
           03  FILLER              PIC X(09)   VALUE   "113024410".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "842100029".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (32)
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ¡Ê£²²óÌÜ¤«¤é£´²óÌÜ¤Þ¤Ç¡Ë
           03  CHK-SRYCD32-IDX     PIC 9(03)   VALUE   32.
           03  FILLER              PIC X(09)   VALUE   "113024510".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "842100029".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (33)
      *    Ë¬Ìä´Ç¸î»Ø¼¨ÎÁ
           03  CHK-SRYCD33-IDX     PIC 9(03)   VALUE   33.
           03  FILLER              PIC X(09)   VALUE   "114008010".
           03  FILLER              PIC X(09)   VALUE   "840000073".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (34)
      *    ºßÂð·ì±ÕÆ©ÀÏÉÑ²ó»ØÆ³´ÉÍý 
           03  CHK-SRYCD34-IDX     PIC 9(03)   VALUE   34.
           03  FILLER              PIC X(09)   VALUE   "114009410".
           03  FILLER              PIC X(09)   VALUE   "850100137".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (35)
      *    ±ý¿ÇÎÁ
           03  CHK-SRYCD35-IDX     PIC 9(03)   VALUE   35.
           03  FILLER              PIC X(09)   VALUE   "114000110".
           03  FILLER              PIC X(09)   VALUE   "840000039".
           03  FILLER              PIC X(09)   VALUE   "850100093".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ÆÃÊÌ±ý¿Ç
           03  CHK-SRYCD35-IDX1    PIC 9(03)   VALUE   35.
           03  FILLER              PIC X(09)   VALUE   "114001610".
           03  FILLER              PIC X(09)   VALUE   "840000039".
           03  FILLER              PIC X(09)   VALUE   "850100093".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (36)
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£±¡ÊÆ±°ì·úÊªµï½»¼Ô°Ê³°¡Ë
           03  CHK-SRYCD36-IDX     PIC 9(03)   VALUE   36.
           03  FILLER              PIC X(09)   VALUE   "114001110".
           03  FILLER              PIC X(09)   VALUE   "840000066".
           03  FILLER              PIC X(09)   VALUE   "850100097".
           03  FILLER              PIC X(09)   VALUE   "830100088".
           03  FILLER              PIC X(09)   VALUE   "850100094".
           03  FILLER              PIC X(09)   VALUE   "850100095".
           03  FILLER              PIC X(135)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£±¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë
           03  CHK-SRYCD36-IDX1    PIC 9(03)   VALUE   36.
           03  FILLER              PIC X(09)   VALUE   "114030310".
           03  FILLER              PIC X(09)   VALUE   "840000066".
           03  FILLER              PIC X(09)   VALUE   "850100097".
           03  FILLER              PIC X(09)   VALUE   "830100088".
           03  FILLER              PIC X(09)   VALUE   "850100094".
           03  FILLER              PIC X(09)   VALUE   "850100095".
           03  FILLER              PIC X(135)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£²¡ÊÆ±°ì·úÊªµï½»¼Ô°Ê³°¡Ë
           03  CHK-SRYCD36-IDX2    PIC 9(03)   VALUE   36.
           03  FILLER              PIC X(09)   VALUE   "114042110".
           03  FILLER              PIC X(09)   VALUE   "840000066".
           03  FILLER              PIC X(09)   VALUE   "850100097".
           03  FILLER              PIC X(09)   VALUE   "830100088".
           03  FILLER              PIC X(09)   VALUE   "850100094".
           03  FILLER              PIC X(09)   VALUE   "850100095".
           03  FILLER              PIC X(135)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£²¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë
           03  CHK-SRYCD36-IDX3    PIC 9(03)   VALUE   36.
           03  FILLER              PIC X(09)   VALUE   "114042210".
           03  FILLER              PIC X(09)   VALUE   "840000066".
           03  FILLER              PIC X(09)   VALUE   "850100097".
           03  FILLER              PIC X(09)   VALUE   "830100088".
           03  FILLER              PIC X(09)   VALUE   "850100094".
           03  FILLER              PIC X(09)   VALUE   "850100095".
           03  FILLER              PIC X(135)  VALUE   SPACE.
      *    (37)
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£²¡Ë¥¤
           03  CHK-SRYCD37-IDX     PIC 9(03)   VALUE   37.
           03  FILLER              PIC X(09)   VALUE   "114042810".
           03  FILLER              PIC X(09)   VALUE   "840000066".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£²¡Ë¥í¡ÊÂ¾¤ÎÊÝ¸±°åÎÅµ¡´Ø¤«¤é¾Ò²ð¤µ¤ì¤¿´µ¼Ô¡Ë
           03  CHK-SRYCD37-IDX1    PIC 9(03)   VALUE   37.
           03  FILLER              PIC X(09)   VALUE   "114046310".
           03  FILLER              PIC X(09)   VALUE   "840000066".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (38)
      *    ²ð¸î¿¦°÷ÅùÓ½áâµÛ°úÅù»Ø¼¨ÎÁ
           03  CHK-SRYCD38-IDX     PIC 9(03)   VALUE   38.
           03  FILLER              PIC X(09)   VALUE   "114020910".
           03  FILLER              PIC X(09)   VALUE   "850190006".
           03  FILLER              PIC X(09)   VALUE   "820190006".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (39)
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ¡Ê±ý¿Ç¡Ë
           03  CHK-SRYCD39-IDX     PIC 9(03)   VALUE   39.
           03  FILLER              PIC X(09)   VALUE   "114027610".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "850100129".
           03  FILLER              PIC X(09)   VALUE   "842100043".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ¡ÊË¬Ìä¿ÇÎÅ¡Ë¡ÊÆ±°ì·úÊªµï½»¼Ô°Ê³°¡Ë
           03  CHK-SRYCD39-IDX1    PIC 9(03)   VALUE   39.
           03  FILLER              PIC X(09)   VALUE   "114027710".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "850100129".
           03  FILLER              PIC X(09)   VALUE   "842100043".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    ºßÂð´µ¼Ô¶¦Æ±¿ÇÎÅÎÁ¡ÊË¬Ìä¿ÇÎÅ¡Ë¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë
           03  CHK-SRYCD39-IDX2    PIC 9(03)   VALUE   39.
           03  FILLER              PIC X(09)   VALUE   "114027810".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "850100129".
           03  FILLER              PIC X(09)   VALUE   "842100043".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    (40)
      *    ºßÂð´µ¼ÔË¬ÌäÌôºÞ´ÉÍý»ØÆ³ÎÁ¡ÊÃ±°ì·úÊª¿ÇÎÅ´µ¼Ô¤¬£±¿Í¡Ë
           03  CHK-SRYCD40-IDX     PIC 9(03)   VALUE   40.
           03  FILLER              PIC X(09)   VALUE   "114044710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬ÌäÌôºÞ´ÉÍý»ØÆ³ÎÁ¡ÊÃ±°ì·úÊª¿ÇÎÅ´µ¼Ô¤¬£²¿Í¤«¤é£¹¿Í¡Ë
           03  CHK-SRYCD40-IDX1    PIC 9(03)   VALUE   40.
           03  FILLER              PIC X(09)   VALUE   "114044810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬ÌäÌôºÞ´ÉÍý»ØÆ³ÎÁ¡ÊÃ±°ì·úÊª¿ÇÎÅ´µ¼Ô¤¬£±£°¿Í¤«¤é¡Ë
           03  CHK-SRYCD40-IDX2    PIC 9(03)   VALUE   40.
           03  FILLER              PIC X(09)   VALUE   "114044910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (41)
      *    °åÎÅ´Ñ»¡Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡Ê°å»Õ¡Ë
           03  CHK-SRYCD41-IDX     PIC 9(03)   VALUE   41.
           03  FILLER              PIC X(09)   VALUE   "188007010".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100244".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (42)
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡Ê°å»ÕµÚ¤Ó´Ç¸î»Õ¤¬¶¦Æ±¡Ë
           03  CHK-SRYCD42-IDX     PIC 9(03)   VALUE   42.
           03  FILLER              PIC X(09)   VALUE   "180047810".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100244".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (43)
      *    °åÎÅ´Ñ»¡Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡Ê°å»ÕµÚ¤Ó´Ç¸î»Õ¤¬¶¦Æ±¡Ë
           03  CHK-SRYCD43-IDX     PIC 9(03)   VALUE   43.
           03  FILLER              PIC X(09)   VALUE   "188010610".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100244".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (44)
      *    Ç§ÃÎÎÅË¡¡¦Ç§ÃÎ¹ÔÆ°ÎÅË¡¡Ê°å»Õ¡Ë
           03  CHK-SRYCD44-IDX     PIC 9(03)   VALUE   44.
           03  FILLER              PIC X(09)   VALUE   "180056410".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100244".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (45)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê½¸ÃæÅª»Ù±çÉ¬Í×¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CHK-SRYCD45-IDX     PIC 9(03)   VALUE   45.
           03  FILLER              PIC X(09)   VALUE   "180057210".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (46)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê½¸ÃæÅª»Ù±çÉ¬Í×¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CHK-SRYCD46-IDX     PIC 9(03)   VALUE   46.
           03  FILLER              PIC X(09)   VALUE   "180057310".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (47)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£²¡Ê½¸ÃæÅª»Ù±çÉ¬Í×¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CHK-SRYCD47-IDX     PIC 9(03)   VALUE   47.
           03  FILLER              PIC X(09)   VALUE   "180057810".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (48)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£²¡Ê½¸ÃæÅª»Ù±çÉ¬Í×¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CHK-SRYCD48-IDX     PIC 9(03)   VALUE   48.
           03  FILLER              PIC X(09)   VALUE   "180057910".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (49)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê½ÅÅÙÀº¿À¾ã³²¼Ô¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CHK-SRYCD49-IDX     PIC 9(03)   VALUE   49.
           03  FILLER              PIC X(09)   VALUE   "180057410".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (50)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê½ÅÅÙÀº¿À¾ã³²¼Ô¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CHK-SRYCD50-IDX     PIC 9(03)   VALUE   50.
           03  FILLER              PIC X(09)   VALUE   "180057510".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (51)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê¥¤µÚ¤Ó¥í°Ê³°¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CHK-SRYCD51-IDX     PIC 9(03)   VALUE   51.
           03  FILLER              PIC X(09)   VALUE   "180057610".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (52)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£±¡Ê¥¤µÚ¤Ó¥í°Ê³°¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CHK-SRYCD52-IDX     PIC 9(03)   VALUE   52.
           03  FILLER              PIC X(09)   VALUE   "180057710".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (53)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£²¡Ê½ÅÅÙÀº¿À¾ã³²¼Ô¡Ë¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CHK-SRYCD53-IDX     PIC 9(03)   VALUE   53.
           03  FILLER              PIC X(09)   VALUE   "180058010".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (54)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£²¡Ê½ÅÅÙÀº¿À¾ã³²¼Ô¡Ë¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CHK-SRYCD54-IDX     PIC 9(03)   VALUE   54.
           03  FILLER              PIC X(09)   VALUE   "180058110".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100261".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (55)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£³¡ÊÃ±°ì·úÊª£±¿Í¡Ë
           03  CHK-SRYCD55-IDX     PIC 9(03)   VALUE   55.
           03  FILLER              PIC X(09)   VALUE   "180064010".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100266".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (56)
      *    Àº¿À²ÊºßÂð´µ¼Ô»Ù±ç´ÉÍýÎÁ£³¡ÊÃ±°ì·úÊª£²¿Í°Ê¾å¡Ë
           03  CHK-SRYCD56-IDX     PIC 9(03)   VALUE   56.
           03  FILLER              PIC X(09)   VALUE   "180064110".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100266".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (57)
      *    ºßÂðÈ¾¸Ç·Á±ÉÍÜ·Ð´É±ÉÍÜË¡»ØÆ³´ÉÍýÎÁ
           03  CHK-SRYCD57-IDX     PIC 9(03)   VALUE   57.
           03  FILLER              PIC X(09)   VALUE   "114045510".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100142".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (58)
      *    ¿Í¹©¿ÕÂ¡¡Ê¤½¤ÎÂ¾¡Ë
           03  CHK-SRYCD58-IDX     PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140007710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    »ýÂ³´Ë½ù¼°·ì±ÕßÉ²á
           03  CHK-SRYCD58-IDX1    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140029850".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ·ìÞù¸ò´¹ÎÅË¡
           03  CHK-SRYCD58-IDX2    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140008210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ·ìµåÀ®Ê¬½üµîÎÅË¡
           03  CHK-SRYCD58-IDX3    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140039210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£±¡Ë¡Ê£´»þ´ÖÌ¤Ëþ¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX4    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140057810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£±¡Ë¡Ê£´»þ´Ö°Ê¾å£µ»þ´ÖÌ¤Ëþ¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX5    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140057910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£±¡Ë¡Ê£µ»þ´Ö°Ê¾å¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX6    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140058010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£²¡Ë¡Ê£´»þ´ÖÌ¤Ëþ¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX7    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140058110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£²¡Ë¡Ê£´»þ´Ö°Ê¾å£µ»þ´ÖÌ¤Ëþ¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX8    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140058210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£²¡Ë¡Ê£µ»þ´Ö°Ê¾å¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX9    PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140058310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£³¡Ë¡Ê£´»þ´ÖÌ¤Ëþ¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX10   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140058410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£³¡Ë¡Ê£´»þ´Ö°Ê¾å£µ»þ´ÖÌ¤Ëþ¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX11   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140058510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£³¡Ë¡Ê£µ»þ´Ö°Ê¾å¡Ë¡Êµ¬Äê´µ¼Ô¡Ë
           03  CHK-SRYCD58-IDX12   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140058610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£±¡Ë¡Ê£´»þ´ÖÌ¤Ëþ¡Ë¡Ê¥¤¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX13   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140060210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£±¡Ë¡Ê£´»þ´Ö°Ê¾å£µ»þ´ÖÌ¤Ëþ¡Ë¡Ê¥í¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX14   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140060310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£±¡Ë¡Ê£µ»þ´Ö°Ê¾å¡Ë¡Ê¥Ï¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX15   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140060410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£²¡Ë¡Ê£´»þ´ÖÌ¤Ëþ¡Ë¡Ê¥¤¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX16   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140060510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£²¡Ë¡Ê£´»þ´Ö°Ê¾å£µ»þ´ÖÌ¤Ëþ¡Ë¡Ê¥í¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX17   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140060610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£²¡Ë¡Ê£µ»þ´Ö°Ê¾å¡Ë¡Ê¥Ï¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX18   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140060710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£³¡Ë¡Ê£´»þ´ÖÌ¤Ëþ¡Ë¡Ê¥¤¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX19   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140060810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£³¡Ë¡Ê£´»þ´Ö°Ê¾å£µ»þ´ÖÌ¤Ëþ¡Ë¡Ê¥í¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX20   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140060910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿Í¹©¿ÕÂ¡¡ÊËýÀ­°Ý»ýÆ©ÀÏ£³¡Ë¡Ê£µ»þ´Ö°Ê¾å¡Ë¡Ê¥Ï¤ò½ü¤¯¡Ë
           03  CHK-SRYCD58-IDX21   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140061010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ·ìÞù¸ò´¹ÎÅË¡¡Ê£Ì£Ä£Ì¥¢¥Õ¥§¥ì¥·¥¹ÎÅË¡¡Ë
           03  CHK-SRYCD58-IDX22   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140063610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ·ìÞù¸ò´¹ÎÅË¡¡Ê°Ü¿¢¸å¹³ÂÎ´ØÏ¢·¿µñÀäÈ¿±þ¼£ÎÅ¡Ë
           03  CHK-SRYCD58-IDX23   PIC 9(03)   VALUE   58.
           03  FILLER              PIC X(09)   VALUE   "140063710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (59)
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡³°¡Ë¡Ê£±£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CHK-SRYCD59-IDX     PIC 9(03)   VALUE   59.
           03  FILLER              PIC X(09)   VALUE   "140054270".
           03  FILLER              PIC X(09)   VALUE   "850100270".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡³°¡Ë¡Ê£±£°£°£ã£í£²°Ê¾å¡Ë
           03  CHK-SRYCD59-IDX1    PIC 9(03)   VALUE   59.
           03  FILLER              PIC X(09)   VALUE   "140054370".
           03  FILLER              PIC X(09)   VALUE   "850100270".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡³°¡Ë¡Ê£²£°£°£ã£í£²°Ê¾å¡Ë
           03  CHK-SRYCD59-IDX2    PIC 9(03)   VALUE   59.
           03  FILLER              PIC X(09)   VALUE   "140054470".
           03  FILLER              PIC X(09)   VALUE   "850100270".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (60)
      *    ¼§µ¤¤Ë¤è¤ëç¯æùÅù»É·ãË¡
           03  CHK-SRYCD60-IDX     PIC 9(03)   VALUE   60.
           03  FILLER              PIC X(09)   VALUE   "140055110".
           03  FILLER              PIC X(09)   VALUE   "840000085".
           03  FILLER              PIC X(09)   VALUE   "850100286".
           03  FILLER              PIC X(09)   VALUE   "842100061".
           03  FILLER              PIC X(153)  VALUE   SPACE.
      *    (61)
      *    ¹âµ¤°µ»ÀÁÇ¼£ÎÅ¡Ê¸º°µ¾ÉËô¤Ï¶õµ¤ºÉÀò¡Ë
           03  CHK-SRYCD61-IDX     PIC 9(03)   VALUE   61.
           03  FILLER              PIC X(09)   VALUE   "140057510".
           03  FILLER              PIC X(09)   VALUE   "840000085".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "840000045".
           03  FILLER              PIC X(09)   VALUE   "850100272".
           03  FILLER              PIC X(09)   VALUE   "842100056".
           03  FILLER              PIC X(09)   VALUE   "850100273".
           03  FILLER              PIC X(126)  VALUE   SPACE.
      *    (62)
      *    ¹âµ¤°µ»ÀÁÇ¼£ÎÅ¡Ê¤½¤ÎÂ¾¡Ë
           03  CHK-SRYCD62-IDX     PIC 9(03)   VALUE   62.
           03  FILLER              PIC X(09)   VALUE   "140057610".
           03  FILLER              PIC X(09)   VALUE   "840000085".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "840000045".
           03  FILLER              PIC X(09)   VALUE   "850100272".
           03  FILLER              PIC X(09)   VALUE   "842100056".
           03  FILLER              PIC X(09)   VALUE   "850100273".
           03  FILLER              PIC X(126)  VALUE   SPACE.
      *    (63)
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDX     PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180027410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDX1    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180027510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDX2    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180028010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDX3    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180028110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDX4    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180027610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDX5    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180027710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD63-IDX6    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180030810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX7    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180044310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX8    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180044410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX9    PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180044510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX10   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180050330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX11   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180050430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX12   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180050530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX13   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180050630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX14   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180050830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX15   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180051030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDX16   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180027810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD63-IDX17   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180027910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDX18   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180032710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX19   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180045810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX20   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180045910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX21   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180046010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX22   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180052730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX23   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180052830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX24   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180052930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX25   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180053030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX26   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180053230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX27   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180053430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£¶ºÐÌ¤Ëþ¡Ë
           03  CHK-SRYCD63-IDX28   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180028210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£¶ºÐ°Ê¾å£±£¸ºÐÌ¤Ëþ¡Ë
           03  CHK-SRYCD63-IDX29   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180028310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¾ã³²»ù¡Ê¼Ô¡Ë¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±£¸ºÐ°Ê¾å¡Ë
           03  CHK-SRYCD63-IDX30   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180028410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDX31   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180044610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDX32   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180044710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD63-IDX33   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180044810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX34   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180044910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX35   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180045110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD63-IDX36   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180045310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX37   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180051530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX38   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180051630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX39   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180051730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX40   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180051830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX41   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180052030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX42   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180052230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¤¬¤ó´µ¼Ô¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ
           03  CHK-SRYCD63-IDX43   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180033110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX44   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180744210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX45   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180744310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX46   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180744410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê´Ç¸î»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX47   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180744510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê½¸ÃÄÎÅË¡¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX48   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180744610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX49   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180744710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX50   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180744810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX51   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180744910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê´Ç¸î»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX52   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê½¸ÃÄÎÅË¡¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX53   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      ***
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX54   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX55   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX56   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX57   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX58   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX59   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX60   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180745910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX61   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX62   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX63   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX64   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX65   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ë¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX66   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë
           03  CHK-SRYCD63-IDX67   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë
           03  CHK-SRYCD63-IDX68   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë
           03  CHK-SRYCD63-IDX69   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX70   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180746910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë
           03  CHK-SRYCD63-IDX71   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë
           03  CHK-SRYCD63-IDX72   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë
           03  CHK-SRYCD63-IDX73   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX74   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë
           03  CHK-SRYCD63-IDX75   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë
           03  CHK-SRYCD63-IDX76   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë
           03  CHK-SRYCD63-IDX77   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX78   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê£±¤«¤é£´°Ê³°¡Ë
           03  CHK-SRYCD63-IDX79   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX80   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180747930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX81   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX82   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX83   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX84   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX85   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX86   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX87   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX88   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX89   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX90   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180748930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX91   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê£±¤«¤é£´°Ê³°¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX92   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX93   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX94   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX95   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX96   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX97   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX98   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX99   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX100  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX101  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX102  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX103  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180749930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX104  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡ËÍ×²ð¸î¡¦£±¤«¤é£´°Ê³°¡¦¥ê¸º
           03  CHK-SRYCD63-IDX105  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      ***
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX106  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX107  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX108  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX109  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX110  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180750910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX111  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX112  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX113  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX114  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX115  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX116  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX117  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ë¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX118  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD63-IDX119  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD63-IDX120  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180751910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë
           03  CHK-SRYCD63-IDX121  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX122  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD63-IDX123  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD63-IDX124  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë
           03  CHK-SRYCD63-IDX125  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX126  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD63-IDX127  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD63-IDX128  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë
           03  CHK-SRYCD63-IDX129  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX130  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180752910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¥¤¤«¤é¥Ë°Ê³°¡Ë
           03  CHK-SRYCD63-IDX131  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX132  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX133  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX134  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX135  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX136  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX137  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX138  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX139  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ë°Ê³°¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX140  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX141  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180753930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX142  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX143  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX144  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX145  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX146  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX147  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX148  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX149  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX150  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180754930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX151  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX152  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX153  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX154  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX155  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX156  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¥¤¤«¤é¥Ë°Ê³°¡¦¥ê¸º
           03  CHK-SRYCD63-IDX157  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      ***
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX158  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX159  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX160  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180755910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX161  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX162  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX163  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX164  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX165  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX166  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ï°Ê³°¡Ë
           03  CHK-SRYCD63-IDX167  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD63-IDX168  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD63-IDX169  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX170  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD63-IDX171  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD63-IDX172  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX173  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD63-IDX174  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD63-IDX175  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD63-IDX176  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê£±¤«¤é£³°Ê³°¡Ë
           03  CHK-SRYCD63-IDX177  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX178  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX179  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX180  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180756930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX181  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX182  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX183  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX184  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX185  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX186  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ï°Ê³°¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX187  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180757630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX188  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX189  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX190  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180758930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX191  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX192  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX193  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX194  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX195  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD63-IDX196  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë£±¤«¤é£³°Ê³°¡¦¥ê¸º
           03  CHK-SRYCD63-IDX197  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      ***
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX198  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX199  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX200  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180759910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX201  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180760010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX202  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180760110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX203  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180760210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX204  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180760310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDX205  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "180760410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDXR1   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDXR2   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800240".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDXR3   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800340".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDXR4   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800350".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDXR5   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800250".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDXR6   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800260".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD63-IDXR7   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800270".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDXR8   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800280".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDXR9   PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800290".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD63-IDXR10  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101800330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD63-IDXR11  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101801110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD63-IDXR12  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101801120".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD63-IDXR13  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101801130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR14  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101801600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR15  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101801700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR16  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101801800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê´Ç¸î»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR17  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101801900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê½¸ÃÄÎÅË¡¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR18  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR19  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR20  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR21  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê´Ç¸î»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR22  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê½¸ÃÄÎÅË¡¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR23  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      ***
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR24  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR25  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR26  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR27  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101802900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR28  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR29  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR30  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR31  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR32  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR33  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR34  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR35  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¢¤«¤é¥¨¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR36  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      ***
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR37  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101803900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR38  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR39  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR40  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR41  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR42  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR43  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR44  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR45  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR46  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR47  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101804900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR48  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¢¤«¤é¥¨¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR49  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      ***
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR50  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR51  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR52  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR53  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR54  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR55  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR56  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR57  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101805900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR58  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¢¤«¤é¥¦¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR59  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      ***
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR60  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR61  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR62  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR63  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR64  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR65  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR66  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD63-IDXR67  PIC 9(03)   VALUE   63.
           03  FILLER              PIC X(09)   VALUE   "101806900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (64)
      *    °ÍÂ¸¾É½¸ÃÄÎÅË¡¡ÊÌôÊª°ÍÂ¸¾É¡Ë
           03  CHK-SRYCD64-IDX     PIC 9(03)   VALUE   64.
           03  FILLER              PIC X(09)   VALUE   "180047910".
           03  FILLER              PIC X(09)   VALUE   "840000076".
           03  FILLER              PIC X(09)   VALUE   "850100245".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    °åÎÅ´Ñ»¡°ÍÂ¸¾É½¸ÃÄÎÅË¡¡ÊÌôÊª°ÍÂ¸¾É¡Ë
           03  CHK-SRYCD64-IDX1    PIC 9(03)   VALUE   64.
           03  FILLER              PIC X(09)   VALUE   "188010710".
           03  FILLER              PIC X(09)   VALUE   "840000076".
           03  FILLER              PIC X(09)   VALUE   "850100245".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (65)
      *    ¼À´µÊÌÅùÀìÌç¥×¥í¥°¥é¥à²Ã»»¡ÊÀº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡¢¾®µ¬ÌÏ¤Ê¤â¤Î¡Ë
           03  CHK-SRYCD65-IDX     PIC 9(03)   VALUE   65.
           03  FILLER              PIC X(09)   VALUE   "180056570".
           03  FILLER              PIC X(09)   VALUE   "840000076".
           03  FILLER              PIC X(09)   VALUE   "850100249".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    ¼À´µÊÌÅùÀìÌç¥×¥í¥°¥é¥à²Ã»»¡Ê°åÎÅ´Ñ»¡Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡Ë
           03  CHK-SRYCD65-IDX1    PIC 9(03)   VALUE   65.
           03  FILLER              PIC X(09)   VALUE   "188011270".
           03  FILLER              PIC X(09)   VALUE   "840000076".
           03  FILLER              PIC X(09)   VALUE   "850100249".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (66)
      *    °ÍÂ¸¾É½¸ÃÄÎÅË¡¡Ê¥®¥ã¥ó¥Ö¥ë°ÍÂ¸¾É¡Ë
           03  CHK-SRYCD66-IDX     PIC 9(03)   VALUE   66.
           03  FILLER              PIC X(09)   VALUE   "180058710".
           03  FILLER              PIC X(09)   VALUE   "840000076".
           03  FILLER              PIC X(09)   VALUE   "850100245".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    °åÎÅ´Ñ»¡°ÍÂ¸¾É½¸ÃÄÎÅË¡¡Ê¥®¥ã¥ó¥Ö¥ë°ÍÂ¸¾É¡Ë
           03  CHK-SRYCD66-IDX1    PIC 9(03)   VALUE   66.
           03  FILLER              PIC X(09)   VALUE   "188011810".
           03  FILLER              PIC X(09)   VALUE   "840000076".
           03  FILLER              PIC X(09)   VALUE   "850100245".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (67)
      *    ·ÐÆ¬³¸¼§µ¤»É·ãÎÅË¡
           03  CHK-SRYCD67-IDX     PIC 9(03)   VALUE   67.
           03  FILLER              PIC X(09)   VALUE   "180058650".
           03  FILLER              PIC X(09)   VALUE   "840000076".
           03  FILLER              PIC X(09)   VALUE   "850100235".
           03  FILLER              PIC X(09)   VALUE   "850100236".
           03  FILLER              PIC X(153)  VALUE   SPACE.
      *    (68)
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD68-IDX     PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180027410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD68-IDX1    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180027510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX2    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180744210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX3    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180744310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX4    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180744410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê´Ç¸î»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX5    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180744510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê½¸ÃÄÎÅË¡¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX6    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180744610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX7    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180744710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX8    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180744810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX9    PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180744910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê´Ç¸î»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX10   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180745010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê½¸ÃÄÎÅË¡¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDX11   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "180745110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD68-IDXR1   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101800230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD68-IDXR2   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101800240".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR3   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101801600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR4   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101801700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR5   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101801800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê´Ç¸î»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR6   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101801900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê½¸ÃÄÎÅË¡¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR7   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101802000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR8   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101802100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR9   PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101802200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR10  PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101802300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê´Ç¸î»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR11  PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101802400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¿´Âç·ì´É¼À´µ¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê½¸ÃÄÎÅË¡¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD68-IDXR12  PIC 9(03)   VALUE   68.
           03  FILLER              PIC X(09)   VALUE   "101802500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (69)
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD69-IDX     PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180028010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD69-IDX1    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180028110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDX2    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180759710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDX3    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180759810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDX4    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180759910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDX5    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180760010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDX6    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180760110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDX7    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180760210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDX8    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180760310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDX9    PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "180760410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD69-IDXR1   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101800340".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD69-IDXR2   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101800350".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDXR3   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101806200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDXR4   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101806300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDXR5   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101806400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDXR6   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101806500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDXR7   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101806600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDXR8   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101806700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDXR9   PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101806800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD69-IDXR10  PIC 9(03)   VALUE   69.
           03  FILLER              PIC X(09)   VALUE   "101806900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (70)
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD70-IDX     PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180027610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD70-IDX1    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180027710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD70-IDX2    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180030810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD70-IDX3    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180044310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD70-IDX4    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180044410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD70-IDX5    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180044510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX6    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180050330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX7    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180050430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX8    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180050530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX9    PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180050630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX10   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180050830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX11   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180051030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX12   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180745310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX13   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180745410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX14   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180745510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX15   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180745610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX16   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180745710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX17   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180745810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX18   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180745910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX19   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX20   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX21   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX22   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX23   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ë¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDX24   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë
           03  CHK-SRYCD70-IDX25   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë
           03  CHK-SRYCD70-IDX26   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë
           03  CHK-SRYCD70-IDX27   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD70-IDX28   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180746910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë
           03  CHK-SRYCD70-IDX29   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë
           03  CHK-SRYCD70-IDX30   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë
           03  CHK-SRYCD70-IDX31   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD70-IDX32   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë
           03  CHK-SRYCD70-IDX33   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë
           03  CHK-SRYCD70-IDX34   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë
           03  CHK-SRYCD70-IDX35   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD70-IDX36   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê£±¤«¤é£´°Ê³°¡Ë
           03  CHK-SRYCD70-IDX37   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX38   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180747930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX39   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX40   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX41   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX42   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX43   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX44   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX45   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX46   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX47   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX48   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180748930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX49   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê£±¤«¤é£´°Ê³°¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX50   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX51   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX52   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX53   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX54   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX55   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX56   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180750030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX57   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX58   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180750130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX59   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX60   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180750230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX61   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180749930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD70-IDX62   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180750330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡ËÍ×²ð¸î¡¦£±¤«¤é£´°Ê³°¡¦¥ê¸º
           03  CHK-SRYCD70-IDX63   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "180750430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD70-IDXR1   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101800250".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD70-IDXR2   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101800260".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD70-IDXR3   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101800270".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR4   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101802600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR5   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101802700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR6   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101802800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR7   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101802900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR8   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR9   PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR10  PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR11  PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR12  PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR13  PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR14  PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR15  PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Ç¾·ì´É¼À´µÅù¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¢¤«¤é¥¨¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD70-IDXR16  PIC 9(03)   VALUE   70.
           03  FILLER              PIC X(09)   VALUE   "101803800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (71)
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD71-IDX     PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180027810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD71-IDX1    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180027910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD71-IDX2    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180032710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD71-IDX3    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180045810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD71-IDX4    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180045910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD71-IDX5    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180046010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX6    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180052730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX7    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180052830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX8    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180052930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX9    PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180053030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX10   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180053230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX11   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180053430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX12   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180755710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX13   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180755810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX14   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180755910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX15   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX16   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX17   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX18   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX19   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDX20   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ï°Ê³°¡Ë
           03  CHK-SRYCD71-IDX21   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD71-IDX22   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD71-IDX23   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD71-IDX24   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD71-IDX25   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD71-IDX26   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD71-IDX27   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD71-IDX28   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD71-IDX29   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD71-IDX30   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê£±¤«¤é£³°Ê³°¡Ë
           03  CHK-SRYCD71-IDX31   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX32   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX33   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX34   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180756930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX35   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX36   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX37   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX38   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX39   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX40   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ï°Ê³°¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX41   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180757630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX42   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX43   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX44   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180758930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX45   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180759030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX46   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180759130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX47   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180759230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX48   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180759330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX49   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180759430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD71-IDX50   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180759530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë£±¤«¤é£³°Ê³°¡¦¥ê¸º
           03  CHK-SRYCD71-IDX51   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "180759630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD71-IDXR1   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101800280".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD71-IDXR2   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101800290".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD71-IDXR3   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101800330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR4   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101805200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR5   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101805300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR6   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101805400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR7   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101805500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR8   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101805600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR9   PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101805700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR10  PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101805800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR11  PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101805900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR12  PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101806000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±¿Æ°´ï¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¢¤«¤é¥¦¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD71-IDXR13  PIC 9(03)   VALUE   71.
           03  FILLER              PIC X(09)   VALUE   "101806100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (72)
      *    ÀÝ¿©µ¡Ç½ÎÅË¡¡Ê£³£°Ê¬°Ê¾å¡Ë
           03  CHK-SRYCD72-IDX     PIC 9(03)   VALUE   72.
           03  FILLER              PIC X(09)   VALUE   "180016610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÀÝ¿©µ¡Ç½ÎÅË¡¡Ê£³£°Ê¬Ì¤Ëþ¡Ë
           03  CHK-SRYCD72-IDX1    PIC 9(03)   VALUE   72.
           03  FILLER              PIC X(09)   VALUE   "180054310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (73)
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD73-IDX     PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180044610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD73-IDX1    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180044710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD73-IDX2    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180044810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD73-IDX3    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180044910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD73-IDX4    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180045110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë
           03  CHK-SRYCD73-IDX5    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180045310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX6    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180051530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX7    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180051630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX8    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180051730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX9    PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180051830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX10   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180052030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡¦Æþ±¡¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX11   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180052230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX12   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180750510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX13   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180750610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX14   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180750710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX15   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180750810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX16   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180750910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX17   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX18   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX19   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX20   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX21   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX22   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX23   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ë¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDX24   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD73-IDX25   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD73-IDX26   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180751910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë
           03  CHK-SRYCD73-IDX27   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD73-IDX28   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD73-IDX29   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD73-IDX30   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë
           03  CHK-SRYCD73-IDX31   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD73-IDX32   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë
           03  CHK-SRYCD73-IDX33   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë
           03  CHK-SRYCD73-IDX34   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë
           03  CHK-SRYCD73-IDX35   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë
           03  CHK-SRYCD73-IDX36   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180752910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¥¤¤«¤é¥Ë°Ê³°¡Ë
           03  CHK-SRYCD73-IDX37   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX38   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX39   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX40   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX41   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX42   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX43   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX44   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX45   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¤¤«¤é¥Ë°Ê³°¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX46   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX47   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180753930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX48   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX49   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX50   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX51   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX52   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX53   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX54   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754730".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX55   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754830".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX56   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180754930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX57   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180755030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX58   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180755130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡ÊÍý³Ø¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX59   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180755230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Êºî¶È¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX60   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180755330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê¸À¸ì¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX61   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180755430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¡Ê°å»Õ¡Ë¡Ê¥ê¸º¡Ë
           03  CHK-SRYCD73-IDX62   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180755530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍ×²ð¸î¡Ë¥¤¤«¤é¥Ë°Ê³°¡¦¥ê¸º
           03  CHK-SRYCD73-IDX63   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "180755630".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD73-IDXR1   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101801110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD73-IDXR2   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101801120".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë
           03  CHK-SRYCD73-IDXR3   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101801130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *R6.6
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR4   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101803900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR5   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR6   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£±¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR7   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804200".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR8   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804300".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR9   PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804400".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR10  PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804500".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£²¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR11  PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804600".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡ÊÍý³ØÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR12  PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804700".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Êºî¶ÈÎÅË¡»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR13  PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804800".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¸À¸ìÄ°³Ð»Î¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR14  PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101804900".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê°å»Õ¤Ë¤è¤ë¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR15  PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101805000".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÇÑÍÑ¾É¸õ·²¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ê£³¡Ë¡Ê¥¢¤«¤é¥¨¤Þ¤Ç°Ê³°¤Î¾ì¹ç¡Ë
           03  CHK-SRYCD73-IDXR16  PIC 9(03)   VALUE   73.
           03  FILLER              PIC X(09)   VALUE   "101805100".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (74)
      *    ½é´ü²Ã»»¡Ê¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÎÁ¡Ë
           03  CHK-SRYCD74-IDX     PIC 9(03)   VALUE   74.
           03  FILLER              PIC X(09)   VALUE   "180033870".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (75)
      *    Áá´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó²Ã»»
           03  CHK-SRYCD75-IDX     PIC 9(03)   VALUE   75.
           03  FILLER              PIC X(09)   VALUE   "180030770".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (76)
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ¡Ê°ÂÁ´´ÉÍý¤òÍ×¤¹¤ë°åÌôÉÊÅêÍ¿´µ¼Ô¡Ë
           03  CHK-SRYCD76-IDX     PIC 9(03)   VALUE   76.
           03  FILLER              PIC X(09)   VALUE   "113010810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÌôºÞ´ÉÍý»ØÆ³ÎÁ¡Ê£±¤Î´µ¼Ô°Ê³°¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD76-IDX1    PIC 9(03)   VALUE   76.
           03  FILLER              PIC X(09)   VALUE   "120001610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (77)
      *    Âà±¡Á°Ë¬Ìä»ØÆ³ÎÁ
           03  CHK-SRYCD77-IDX     PIC 9(03)   VALUE   77.
           03  FILLER              PIC X(09)   VALUE   "190030310".
           03  FILLER              PIC X(09)   VALUE   "840000055".
           03  FILLER              PIC X(09)   VALUE   "850100084".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (78)
      *    ²ð¸î»Ù±çÅùÏ¢·È»ØÆ³ÎÁ
           03  CHK-SRYCD78-IDX     PIC 9(03)   VALUE   78.
           03  FILLER              PIC X(09)   VALUE   "113011710".
           03  FILLER              PIC X(09)   VALUE   "850100073".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (79)
      *    Âà±¡»þ¶¦Æ±»ØÆ³ÎÁ£²
           03  CHK-SRYCD79-IDX     PIC 9(03)   VALUE   79.
           03  FILLER              PIC X(09)   VALUE   "113008910".
           03  FILLER              PIC X(09)   VALUE   "840000110".
           03  FILLER              PIC X(09)   VALUE   "850100071".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (80)
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡¡Ë¡Ê£±£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CHK-SRYCD80-IDX     PIC 9(03)   VALUE   80.
           03  FILLER              PIC X(09)   VALUE   "140052170".
           03  FILLER              PIC X(09)   VALUE   "850100269".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡¡Ë¡Ê£±£°£°£ã£í£²°Ê¾å¡Ë
           03  CHK-SRYCD80-IDX1    PIC 9(03)   VALUE   80.
           03  FILLER              PIC X(09)   VALUE   "140052270".
           03  FILLER              PIC X(09)   VALUE   "850100269".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡¡Ë¡Ê£²£°£°£ã£í£²°Ê¾å¡Ë
           03  CHK-SRYCD80-IDX2    PIC 9(03)   VALUE   80.
           03  FILLER              PIC X(09)   VALUE   "140052370".
           03  FILLER              PIC X(09)   VALUE   "850100269".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (81)
      *    ¿´ÉÔÁ´¤ËÂÐ¤¹¤ë±óÀÖ³°Àþ²¹Ç®ÎÅË¡
           03  CHK-SRYCD81-IDX     PIC 9(03)   VALUE   81.
           03  FILLER              PIC X(09)   VALUE   "140061210".
           03  FILLER              PIC X(09)   VALUE   "840000076".
           03  FILLER              PIC X(09)   VALUE   "850100282".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (82)
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX     PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180039030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX1    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180036030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡ÊÁá´ü¡¦Æþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX2    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180039130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡ÊÁá´ü¡¦Æþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX3    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180039230".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX4    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180039330".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX5    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180036130".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡ÊÁá´ü¡¦Æþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX6    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180039430".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡ÊÁá´ü¡¦Æþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX7    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180039530".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡Ê£³Ç¯Ä¶¡¦½µ£³ÆüÄ¶¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX8    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180048930".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡Ê£³Ç¯Ä¶¡¦½µ£³ÆüÄ¶¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD82-IDX9    PIC 9(03)   VALUE   82.
           03  FILLER              PIC X(09)   VALUE   "180049030".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (83)
      *    Àº¿À²Ê³ÖÎ¥¼¼´ÉÍý²Ã»»
           03  CHK-SRYCD83-IDX     PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190107070".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»
           03  CHK-SRYCD83-IDX1    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190136810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ±ÉÍÜ¥µ¥Ý¡¼¥È¥Á¡¼¥à²Ã»»¡ÊÆÃÄêÃÏ°è¡Ë
           03  CHK-SRYCD83-IDX2    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190147110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ¸ÆµÛ¥±¥¢¥Á¡¼¥à²Ã»»
           03  CHK-SRYCD83-IDX3    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190137910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½Å¾É¼Ô²Ã»»£²¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë
           03  CHK-SRYCD83-IDX4    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190141370".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½Å¾É¼Ô²Ã»»£±¡ÊÀº¿ÀÎÅÍÜÉÂÅïÆþ±¡ÎÁ¡Ë
           03  CHK-SRYCD83-IDX5    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190151470".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½Å¾É¼Ô²Ã»»£±¡ÊÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ¡Ë
           03  CHK-SRYCD83-IDX6    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190195770".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½Å¾É¼Ô²Ã»»£²¡ÊÃÏ°è°Ü¹Ôµ¡Ç½¶¯²½ÉÂÅïÆþ±¡ÎÁ¡Ë
           03  CHK-SRYCD83-IDX7    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190195870".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Àº¿À²Ê¥ê¥¨¥¾¥ó¥Á¡¼¥à²Ã»»
           03  CHK-SRYCD83-IDX8    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190147010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»£±
           03  CHK-SRYCD83-IDX9    PIC 9(03)   VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190192110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÉÂÅïÌôºÞ¶ÈÌ³¼Â»Ü²Ã»»£²
           03  CHK-SRYCD83-IDX10    PIC 9(03)  VALUE   83.
           03  FILLER              PIC X(09)   VALUE   "190192210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (84)
      *    Ä¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐ°Ê¾å¡Ë
           03  CHK-SRYCD84-IDX     PIC 9(03)   VALUE   84.
           03  FILLER              PIC X(09)   VALUE   "190076570".
           03  FILLER              PIC X(09)   VALUE   "840000056".
           03  FILLER              PIC X(09)   VALUE   "840000556".
           03  FILLER              PIC X(09)   VALUE   "850100020".
           03  FILLER              PIC X(09)   VALUE   "850100021".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    (85)
      *    ½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐ°Ê¾å¡Ë
           03  CHK-SRYCD85-IDX     PIC 9(03)   VALUE   85.
           03  FILLER              PIC X(09)   VALUE   "190076670".
           03  FILLER              PIC X(09)   VALUE   "840000056".
           03  FILLER              PIC X(09)   VALUE   "840000556".
           03  FILLER              PIC X(09)   VALUE   "850100020".
           03  FILLER              PIC X(09)   VALUE   "850100021".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    (86)
      *    Ä¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐÌ¤Ëþ¡Ë
           03  CHK-SRYCD86-IDX     PIC 9(03)   VALUE   86.
           03  FILLER              PIC X(09)   VALUE   "190127510".
           03  FILLER              PIC X(09)   VALUE   "840000056".
           03  FILLER              PIC X(09)   VALUE   "840000556".
           03  FILLER              PIC X(09)   VALUE   "850100020".
           03  FILLER              PIC X(09)   VALUE   "850100021".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    (87)
      *    ½àÄ¶½Å¾É»ù¡Ê¼Ô¡ËÆþ±¡¿ÇÎÅ²Ã»»¡Ê£¶ºÐÌ¤Ëþ¡Ë
           03  CHK-SRYCD87-IDX     PIC 9(03)   VALUE   87.
           03  FILLER              PIC X(09)   VALUE   "190127610".
           03  FILLER              PIC X(09)   VALUE   "840000056".
           03  FILLER              PIC X(09)   VALUE   "840000556".
           03  FILLER              PIC X(09)   VALUE   "850100020".
           03  FILLER              PIC X(09)   VALUE   "850100021".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    (88)
      *    ÇÓÇ¢¼«Î©»Ù±ç²Ã»»
           03  CHK-SRYCD88-IDX     PIC 9(03)   VALUE   88.
           03  FILLER              PIC X(09)   VALUE   "190219010".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "840000086".
           03  FILLER              PIC X(09)   VALUE   "850100027".
           03  FILLER              PIC X(09)   VALUE   "842100022".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    (89)
      *    ½é¿ÇÎÁ»»Äê²Ê¡¨
           03  CHK-SRYCD89-IDX     PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "830000020".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD89-IDX1    PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "111011810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡¦Ê¸½ñ¤Ë¤è¤ë¾Ò²ð¤¬¤Ê¤¤´µ¼Ô¡Ë
           03  CHK-SRYCD89-IDX2    PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "111012610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD89-IDX3    PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "111012810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD89-IDX4    PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "111014510".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆü£²²ÊÌÜ¡¦Ãí£²¤«¤é£´¤Ëµ¬Äê¤¹¤ë¾ì¹ç¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD89-IDX5    PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "111014610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD89-IDX6    PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "111014710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ½é¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           03  CHK-SRYCD89-IDX7    PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "101110040".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ½é¿ÇÎÁ¡ÊÊ¸½ñ¤Ë¤è¤ë¾Ò²ð¤¬¤Ê¤¤´µ¼Ô¡Ë¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           03  CHK-SRYCD89-IDX8    PIC 9(03)   VALUE   89.
           03  FILLER              PIC X(09)   VALUE   "101110060".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (90)
      *    ºÆ¿ÇÎÁ»»Äê²Ê¡¨
           03  CHK-SRYCD90-IDX     PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "830000021".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD90-IDX1    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112015810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD90-IDX2    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112017010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD90-IDX3    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112015950".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÅÅÏÃÅùÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD90-IDX4    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112017150".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë
           03  CHK-SRYCD90-IDX5    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112023550".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÅÅÏÃÅùÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê£²²ÊÌÜ¡¦£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë
           03  CHK-SRYCD90-IDX6    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112023850".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë
           03  CHK-SRYCD90-IDX7    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112016210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡¦Ê¸½ñ¾Ò²ð¿½½Ð´µ¼Ô¡Ë
           03  CHK-SRYCD90-IDX8    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112016410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡¦Ê¸½ñ¾Ò²ð¿½½Ð´µ¼Ô¡Ë
           03  CHK-SRYCD90-IDX9    PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112017610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD90-IDX10   PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112025210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD90-IDX11   PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112025310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ¤Î£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD90-IDX12   PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112025910".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ³°Íè¿ÇÎÅÎÁ¡ÊÆ±ÆüÊ£¿ô²Ê£²²ÊÌÜ¡¦Ê¸½ñ¾Ò²ð¿½½Ð´µ¼Ô¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD90-IDX13   PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112026010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÄêÂÅ·ëÎ¨³°Íè¿ÇÎÅÎÁ¡ÊÆ±°ìÆüÊ£¿ô²Ê¼õ¿Ç»þ£²²ÊÌÜ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD90-IDX14   PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "112026110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           03  CHK-SRYCD90-IDX15   PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "101120040".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡ÊÆ±°ìÆüÊ£¿ô¿ÇÎÅ²Ê¼õ¿Ç¡Ë
           03  CHK-SRYCD90-IDX16   PIC 9(03)   VALUE   90.
           03  FILLER              PIC X(09)   VALUE   "101120080".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (91)
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ
           03  CHK-SRYCD91-IDX     PIC 9(03)   VALUE   91.
           03  FILLER              PIC X(09)   VALUE   "112007950".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÅÅÏÃÅùÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ
           03  CHK-SRYCD91-IDX1    PIC 9(03)   VALUE   91.
           03  FILLER              PIC X(09)   VALUE   "112016750".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡ÊÆÃÎã¡Ë
           03  CHK-SRYCD91-IDX2    PIC 9(03)   VALUE   91.
           03  FILLER              PIC X(09)   VALUE   "112026750".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ
           03  CHK-SRYCD91-IDX3    PIC 9(03)   VALUE   91.
           03  FILLER              PIC X(09)   VALUE   "101120050".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (92)
      *    ÅÅÏÃÅùºÆ¿ÇÎÁ¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë
           03  CHK-SRYCD92-IDX     PIC 9(03)   VALUE   92.
           03  FILLER              PIC X(09)   VALUE   "112023350".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÅÅÏÃÅùÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë
           03  CHK-SRYCD92-IDX1    PIC 9(03)   VALUE   92.
           03  FILLER              PIC X(09)   VALUE   "112023650".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (93)
      *    Æ±ÆüºÆ¿ÇÎÁ
           03  CHK-SRYCD93-IDX     PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112008350".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±Æü³°Íè¿ÇÎÅÎÁ
           03  CHK-SRYCD93-IDX1    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112011710".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±Æü³°Íè¿ÇÎÅÎÁ¡ÊÂ¾°åÎÅµ¡´Ø¤ØÊ¸½ñ¾Ò²ð¤Î¿½½Ð¤ò¹Ô¤Ã¤Æ¤¤¤ë´µ¼Ô¡Ë
           03  CHK-SRYCD93-IDX2    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112016550".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±ÆüÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ
           03  CHK-SRYCD93-IDX3    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112016850".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±ÆüÆÃÄêÂÅ·ëÎ¨³°Íè¿ÇÎÅÎÁ
           03  CHK-SRYCD93-IDX4    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112017450".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±ÆüºÆ¿ÇÎÁ¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD93-IDX5    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112024950".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±ÆüÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD93-IDX6    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112025150".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±Æü³°Íè¿ÇÎÅÎÁ¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD93-IDX7    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112025450".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±Æü³°Íè¿ÇÎÅÎÁ¡ÊÊ¸½ñ¾Ò²ð¿½½Ð´µ¼Ô¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD93-IDX8    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112025650".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±ÆüÆÃÄêÂÅ·ëÎ¨³°Íè¿ÇÎÅÎÁ¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD93-IDX9    PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "112025850".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    Æ±ÆüºÆ¿ÇÎÁ
           03  CHK-SRYCD93-IDX10   PIC 9(03)   VALUE   93.
           03  FILLER              PIC X(09)   VALUE   "101120060".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (94)
      *    Æ±ÆüÅÅÏÃÅùºÆ¿ÇÎÁ
           03  CHK-SRYCD94-IDX     PIC 9(03)   VALUE   94.
           03  FILLER              PIC X(09)   VALUE   "112008850".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±ÆüÅÅÏÃÅùÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ
           03  CHK-SRYCD94-IDX1    PIC 9(03)   VALUE   94.
           03  FILLER              PIC X(09)   VALUE   "112016950".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *Ï«ºÒÍÑ
      *    Æ±ÆüÅÅÏÃÅùºÆ¿ÇÎÁ
           03  CHK-SRYCD94-IDX2    PIC 9(03)   VALUE   94.
           03  FILLER              PIC X(09)   VALUE   "101120070".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (95)
      *    Æ±ÆüÅÅÏÃÅùºÆ¿ÇÎÁ¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë
           03  CHK-SRYCD95-IDX     PIC 9(03)   VALUE   95.
           03  FILLER              PIC X(09)   VALUE   "112023450".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    Æ±ÆüÅÅÏÃÅùÆÃÄêÂÅ·ëÎ¨ºÆ¿ÇÎÁ¡Ê£³£°Ç¯£³·î°ÊÁ°·ÑÂ³¡Ë
           03  CHK-SRYCD95-IDX1    PIC 9(03)   VALUE   95.
           03  FILLER              PIC X(09)   VALUE   "112023750".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (96)
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£±¡ÊÆ±°ì·úÊªµï½»¼Ô°Ê³°¡Ë
           03  CHK-SRYCD96-IDX     PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114001110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£±¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë
           03  CHK-SRYCD96-IDX1    PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114030310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£²¡ÊÆ±°ì·úÊªµï½»¼Ô°Ê³°¡Ë
           03  CHK-SRYCD96-IDX2    PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114042110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£²¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë
           03  CHK-SRYCD96-IDX3    PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114042210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£²¡Ë¥¤
           03  CHK-SRYCD96-IDX4    PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114042810".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£²¡Ë¥í¡ÊÂ¾¤ÎÊÝ¸±°åÎÅµ¡´Ø¤«¤é¾Ò²ð¤µ¤ì¤¿´µ¼Ô¡Ë
           03  CHK-SRYCD96-IDX5    PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114046310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£±¡ÊÆ±°ì·úÊªµï½»¼Ô°Ê³°¡Ë¡ÊÄþ¸º¡Ë
           03  CHK-SRYCD96-IDX6    PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114706310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£±¡Ë£±¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë¡ÊÄþ¸º¡Ë
           03  CHK-SRYCD96-IDX7    PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114706410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£²¡Ë¥¤¡ÊÄþ¸º¡Ë
           03  CHK-SRYCD96-IDX8    PIC 9(03)   VALUE   96.
           03  FILLER              PIC X(09)   VALUE   "114706610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (97)
      *    ¥ª¥ó¥é¥¤¥ó¿ÇÎÅÎÁ
           03  CHK-SRYCD97-IDX     PIC 9(03)   VALUE   97.
           03  FILLER              PIC X(09)   VALUE   "112023210".
           03  FILLER              PIC X(09)   VALUE   "850100001".
           03  FILLER              PIC X(09)   VALUE   "850100002".
           03  FILLER              PIC X(09)   VALUE   "850100003".
           03  FILLER              PIC X(09)   VALUE   "850100004".
           03  FILLER              PIC X(09)   VALUE   "850100005".
           03  FILLER              PIC X(09)   VALUE   "850100006".
           03  FILLER              PIC X(09)   VALUE   "850100007".
           03  FILLER              PIC X(09)   VALUE   "850100008".
           03  FILLER              PIC X(09)   VALUE   "850100009".
           03  FILLER              PIC X(09)   VALUE   "850100010".
           03  FILLER              PIC X(09)   VALUE   "850100011".
           03  FILLER              PIC X(81)   VALUE   SPACE.
      *    (98)
      *    ¥ª¥ó¥é¥¤¥óºßÂð´ÉÍýÎÁ¡Êºß°åÁí´É¡Ê·î£±²óË¬Ìä¿ÇÎÅ¡Ë¡Ë
           03  CHK-SRYCD98-IDX     PIC 9(03)   VALUE   98.
           03  FILLER              PIC X(09)   VALUE   "114043970".
           03  FILLER              PIC X(09)   VALUE   "850100110".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (99)
      *    Àº¿À²Ê¥ª¥ó¥é¥¤¥óºßÂð´ÉÍýÎÁ
           03  CHK-SRYCD99-IDX     PIC 9(03)   VALUE   99.
           03  FILLER              PIC X(09)   VALUE   "180058270".
           03  FILLER              PIC X(09)   VALUE   "850100267".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (100)
      *    ºßÂð´µ¼ÔË¬Ìä¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó»ØÆ³´ÉÍýÎÁ¡ÊÆ±°ì·úÊªµï½»¼Ô°Ê³°¡Ë
           03  CHK-SRYCD100-IDX    PIC 9(03)   VALUE   100.
           03  FILLER              PIC X(09)   VALUE   "114006410".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥ó»ØÆ³´ÉÍýÎÁ¡ÊÆ±°ì·úÊªµï½»¼Ô¡Ë
           03  CHK-SRYCD100-IDX1   PIC 9(03)   VALUE   100.
           03  FILLER              PIC X(09)   VALUE   "114015010".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (101)
      *    ±ý¿ÇÎÁ
           03  CHK-SRYCD101-IDX    PIC 9(03)   VALUE   101.
           03  FILLER              PIC X(09)   VALUE   "114000110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÊÌ±ý¿Ç
           03  CHK-SRYCD101-IDX1   PIC 9(03)   VALUE   101.
           03  FILLER              PIC X(09)   VALUE   "114001610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (102)
      *    Ç¾À­£Î£áÍøÇ¢¥Ú¥×¥Á¥É¡Ê£Â£Î£Ð¡Ë
           03  CHK-SRYCD102-IDX    PIC 9(03)   VALUE   102.
           03  FILLER              PIC X(09)   VALUE   "160162350".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (103)
      *    Ç¾À­£Î£áÍøÇ¢¥Ú¥×¥Á¥ÉÁ°¶îÂÎ£ÎÃ¼¥Õ¥é¥°¥á¥ó¥È¡Ê£Î£Ô¡Ý£ð£ò£ï£Â£Î£Ð¡Ë
           03  CHK-SRYCD103-IDX    PIC 9(03)   VALUE   103.
           03  FILLER              PIC X(09)   VALUE   "160181250".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (104)
      *    ¿´Ë¼À­£Î£áÍøÇ¢¥Ú¥×¥Á¥É¡Ê£Á£Î£Ð¡Ë
           03  CHK-SRYCD104-IDX    PIC 9(03)   VALUE   104.
           03  FILLER              PIC X(09)   VALUE   "160116310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (105)
      *    ÄÌ±¡¡¦ºßÂðÀº¿ÀÎÅË¡¡Ê£²£°ºÐÌ¤Ëþ¡Ë²Ã»»
           03  CHK-SRYCD105-IDX    PIC 9(03)   VALUE   105.
           03  FILLER              PIC X(09)   VALUE   "180020570".
           03  FILLER              PIC X(09)   VALUE   "850100238".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (106)
      *    »ùÆ¸»×½Õ´üÀº¿À²ÊÀìÌç´ÉÍý²Ã»»¡Ê£±£¶ºÐÌ¤Ëþ¡Ë
           03  CHK-SRYCD106-IDX    PIC 9(03)   VALUE   106.
           03  FILLER              PIC X(09)   VALUE   "180047270".
           03  FILLER              PIC X(09)   VALUE   "850100239".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (107)
      *    »ùÆ¸»×½Õ´üÀº¿À²ÊÀìÌç´ÉÍý²Ã»»¡Ê£²£°ºÐÌ¤Ëþ¡Ë
           03  CHK-SRYCD107-IDX    PIC 9(03)   VALUE   107.
           03  FILLER              PIC X(09)   VALUE   "180047370".
           03  FILLER              PIC X(09)   VALUE   "850100240".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (108)
      *    »þ´Ö³°¶ÛµÞ±¡Æâ¸¡ºº²Ã»»
           03  CHK-SRYCD108-IDX    PIC 9(03)   VALUE   108.
           03  FILLER              PIC X(09)   VALUE   "160000210".
           03  FILLER              PIC X(09)   VALUE   "820100129".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ³°Íè¿×Â®¸¡ÂÎ¸¡ºº²Ã»»
           03  CHK-SRYCD108-IDX1   PIC 9(03)   VALUE   108.
           03  FILLER              PIC X(09)   VALUE   "160177770".
           03  FILLER              PIC X(09)   VALUE   "820100129".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (109)
      *    »þ´Ö³°¶ÛµÞ±¡Æâ²èÁü¿ÇÃÇ²Ã»»
           03  CHK-SRYCD109-IDX    PIC 9(03)   VALUE   109.
           03  FILLER              PIC X(09)   VALUE   "170016010".
           03  FILLER              PIC X(09)   VALUE   "820100129".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (110)
      *    ±ý¿ÇÎÁ
           03  CHK-SRYCD110-IDX    PIC 9(03)   VALUE   110.
           03  FILLER              PIC X(09)   VALUE   "114000110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÊÌ±ý¿Ç
           03  CHK-SRYCD110-IDX1   PIC 9(03)   VALUE   110.
           03  FILLER              PIC X(09)   VALUE   "114001610".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (111)
      *    ¥Ë¥³¥Á¥ó°ÍÂ¸¾É´ÉÍýÎÁ£²
           03  CHK-SRYCD111-IDX    PIC 9(03)   VALUE   111.
           03  FILLER              PIC X(09)   VALUE   "113031710".
           03  FILLER              PIC X(09)   VALUE   "840000097".
           03  FILLER              PIC X(09)   VALUE   "850100064".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (112)
      *    »þ´Ö³°¶ÛµÞ±¡Æâ¸¡ºº²Ã»»
           03  CHK-SRYCD112-IDX    PIC 9(03)   VALUE   112.
           03  FILLER              PIC X(09)   VALUE   "160000210".
           03  FILLER              PIC X(09)   VALUE   "840000099".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (113)
      *    »þ´Ö³°¶ÛµÞ±¡Æâ²èÁü¿ÇÃÇ²Ã»»
           03  CHK-SRYCD113-IDX    PIC 9(03)   VALUE   113.
           03  FILLER              PIC X(09)   VALUE   "170016010".
           03  FILLER              PIC X(09)   VALUE   "840000099".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (114)
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX    PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180039030".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX1   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180036030".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡ÊÁá´ü¡¦Æþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX2   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180039130".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡ÊÁá´ü¡¦Æþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX3   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180039230".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX4   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180039330".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX5   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180036130".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡ÊÁá´ü¡¦Æþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX6   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180039430".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡ÊÁá´ü¡¦Æþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX7   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180039530".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡Ê£³Ç¯Ä¶¡¦½µ£³ÆüÄ¶¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX8   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180048930".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡Ê£³Ç¯Ä¶¡¦½µ£³ÆüÄ¶¡Ë¡ÊÆþ±¡Ãæ¤Î´µ¼Ô¡Ë
           03  CHK-SRYCD114-IDX9   PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180049030".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë
           03  CHK-SRYCD114-IDX10  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180028610".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë
           03  CHK-SRYCD114-IDX11  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180028710".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë
           03  CHK-SRYCD114-IDX12  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180007510".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë
           03  CHK-SRYCD114-IDX13  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180007610".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ê¾®µ¬ÌÏ¡Ë¡Ê£³Ç¯Ä¶¡¦½µ£³ÆüÄ¶¡Ë
           03  CHK-SRYCD114-IDX14  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180048030".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡ÊÂçµ¬ÌÏ¡Ë¡Ê£³Ç¯Ä¶¡¦½µ£³ÆüÄ¶¡Ë
           03  CHK-SRYCD114-IDX15  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180048130".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ê¥¤¥È¡¦¥±¥¢
           03  CHK-SRYCD114-IDX16  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180007810".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ê¥¤¥È¡¦¥±¥¢¡Ê£³Ç¯Ä¶¡¦½µ£³ÆüÄ¶¡Ë
           03  CHK-SRYCD114-IDX17  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180048430".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥Ê¥¤¥È¡¦¥±¥¢
           03  CHK-SRYCD114-IDX18  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180017210".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Àº¿À²Ê¥Ç¥¤¡¦¥Ê¥¤¥È¡¦¥±¥¢¡Ê£³Ç¯Ä¶¡¦½µ£³ÆüÄ¶¡Ë
           03  CHK-SRYCD114-IDX19  PIC 9(03)   VALUE   114.
           03  FILLER              PIC X(09)   VALUE   "180048530".
           03  FILLER              PIC X(09)   VALUE   "850100246".
           03  FILLER              PIC X(09)   VALUE   "820100745".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (115)
      *    ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£±
           03  CHK-SRYCD115-IDX    PIC 9(03)   VALUE   115.
           03  FILLER              PIC X(09)   VALUE   "114040710".
           03  FILLER              PIC X(09)   VALUE   "850100143".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (116)
      *    ºßÂð»ýÂ³ÍÛ°µ¸ÆµÛÎÅË¡»ØÆ³´ÉÍýÎÁ£²
           03  CHK-SRYCD116-IDX    PIC 9(03)   VALUE   116.
           03  FILLER              PIC X(09)   VALUE   "114040810".
           03  FILLER              PIC X(09)   VALUE   "850100143".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (117)
      *    ÆÃÊÌË¬Ìä´Ç¸î»Ø¼¨²Ã»»
           03  CHK-SRYCD117-IDX    PIC 9(03)   VALUE   117.
           03  FILLER              PIC X(09)   VALUE   "114008370".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÊÌË¬Ìä´Ç¸î»Ø¼¨²Ã»»¡Ê¿ÇÎÅÊó½·¾åÎ×»þÅª¼è°·¡Ë
           03  CHK-SRYCD117-IDX1   PIC 9(03)   VALUE   117.
           03  FILLER              PIC X(09)   VALUE   "114054050".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ÆÃÊÌË¬Ìä´Ç¸î»Ø¼¨²Ã»»¡ÊÆÃÎã¡Ë
           03  CHK-SRYCD117-IDX2   PIC 9(03)   VALUE   117.
           03  FILLER              PIC X(09)   VALUE   "114058050".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (118)
      *    °ìÈÌÉÔÇ¥¼£ÎÅ´ÉÍýÎÁ
           03  CHK-SRYCD118-IDX    PIC 9(03)   VALUE   118.
           03  FILLER              PIC X(09)   VALUE   "113036210".
           03  FILLER              PIC X(09)   VALUE   "850100402".
           03  FILLER              PIC X(09)   VALUE   "820190049".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (119)
      *    Æó¼¡À­¹üÀÞÍ½ËÉ·ÑÂ³´ÉÍýÎÁ£³
           03  CHK-SRYCD119-IDX    PIC 9(03)   VALUE   119.
           03  FILLER              PIC X(09)   VALUE   "113036710".
           03  FILLER              PIC X(09)   VALUE   "850100406".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (120)
      *    ¥¢¥ì¥ë¥®¡¼À­É¡±êÌÈ±ÖÎÅË¡¼£ÎÅ´ÉÍýÎÁ¡Ê£±·îÌÜ¡Ë
           03  CHK-SRYCD120-IDX    PIC 9(03)   VALUE   120.
           03  FILLER              PIC X(09)   VALUE   "113036810".
           03  FILLER              PIC X(09)   VALUE   "850100407".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¥¢¥ì¥ë¥®¡¼À­É¡±êÌÈ±ÖÎÅË¡¼£ÎÅ´ÉÍýÎÁ¡Ê£²·îÌÜ°Ê¹ß¡Ë
           03  CHK-SRYCD120-IDX1   PIC 9(03)   VALUE   120.
           03  FILLER              PIC X(09)   VALUE   "113036910".
           03  FILLER              PIC X(09)   VALUE   "850100407".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (121)
      *    ²¼»èÁÏ½ý½èÃÖ´ÉÍýÎÁ
           03  CHK-SRYCD121-IDX    PIC 9(03)   VALUE   121.
           03  FILLER              PIC X(09)   VALUE   "113037010".
           03  FILLER              PIC X(09)   VALUE   "850190198".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (122)
      *    ²¼»èÁÏ½ý½èÃÖ¡ÊÂ­ìæ¤Î¿¼¤¤ÄÙáç¡¦ìûÉô¤ÎÀõ¤¤ÄÙáç¡Ë
           03  CHK-SRYCD122-IDX    PIC 9(03)   VALUE   122.
           03  FILLER              PIC X(09)   VALUE   "140062210".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ²¼»èÁÏ½ý½èÃÖ¡ÊÂ­Éô¡Êìû¤ò½ü¤¯¡Ë¤ÎÀõ¤¤ÄÙáç¡Ë
           03  CHK-SRYCD122-IDX1   PIC 9(03)   VALUE   122.
           03  FILLER              PIC X(09)   VALUE   "140062110".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    ²¼»èÁÏ½ý½èÃÖ¡ÊÂ­Éô¡Êìû¤ò½ü¤¯¡Ë¤Î¿¼¤¤ÄÙáç¡¦ìûÉô¤Î¿¼¤¤ÄÙáç¡Ë
           03  CHK-SRYCD122-IDX2   PIC 9(03)   VALUE   122.
           03  FILLER              PIC X(09)   VALUE   "140062310".
           03  FILLER              PIC X(180)  VALUE   SPACE.
      *    (123)
      *    Áá´ü²Ã»»¡ÊÀº¿À²Ê¥·¥ç¡¼¥È¡¦¥±¥¢¡Ë
           03  CHK-SRYCD123-IDX    PIC 9(03)   VALUE   123.
           03  FILLER              PIC X(09)   VALUE   "180033370".
           03  FILLER              PIC X(09)   VALUE   "850100247".
           03  FILLER              PIC X(09)   VALUE   "850100248".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Áá´ü²Ã»»¡ÊÀº¿À²Ê¥Ç¥¤¡¦¥±¥¢¡Ë
           03  CHK-SRYCD123-IDX1   PIC 9(03)   VALUE   123.
           03  FILLER              PIC X(09)   VALUE   "180033470".
           03  FILLER              PIC X(09)   VALUE   "850100247".
           03  FILLER              PIC X(09)   VALUE   "850100248".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Áá´ü²Ã»»¡ÊÀº¿À²Ê¥Ê¥¤¥È¡¦¥±¥¢¡Ë
           03  CHK-SRYCD123-IDX2   PIC 9(03)   VALUE   123.
           03  FILLER              PIC X(09)   VALUE   "180036270".
           03  FILLER              PIC X(09)   VALUE   "850100247".
           03  FILLER              PIC X(09)   VALUE   "850100248".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    Áá´ü²Ã»»¡ÊÀº¿À²Ê¥Ç¥¤¡¦¥Ê¥¤¥È¡¦¥±¥¢¡Ë
           03  CHK-SRYCD123-IDX3   PIC 9(03)   VALUE   123.
           03  FILLER              PIC X(09)   VALUE   "180036370".
           03  FILLER              PIC X(09)   VALUE   "850100247".
           03  FILLER              PIC X(09)   VALUE   "850100248".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (124)
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ£²¡Ê½é²ó¡Ë
           03  CHK-SRYCD124-IDX    PIC 9(03)   VALUE   124.
           03  FILLER              PIC X(09)   VALUE   "113704010".
           03  FILLER              PIC X(09)   VALUE   "842100111".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (125)
      *    ÆýÁ£±ê½Å¾É²½Í½ËÉ¥±¥¢¡¦»ØÆ³ÎÁ£²¡Ê£²²óÌÜ¤«¤é£¸²óÌÜ¤Þ¤Ç¡Ë
           03  CHK-SRYCD125-IDX    PIC 9(03)   VALUE   125.
           03  FILLER              PIC X(09)   VALUE   "113703910".
           03  FILLER              PIC X(09)   VALUE   "842100111".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (126)
      *    ËýÀ­¿ÕÂ¡ÉÂÆ©ÀÏÍ½ËÉ»ØÆ³´ÉÍýÎÁ¡Ê£±Ç¯°ÊÆâ¡Ë
           03  CHK-SRYCD126-IDX    PIC 9(03)   VALUE   126.
           03  FILLER              PIC X(09)   VALUE   "113704110".
           03  FILLER              PIC X(09)   VALUE   "850190210".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ËýÀ­¿ÕÂ¡ÉÂÆ©ÀÏÍ½ËÉ»ØÆ³´ÉÍýÎÁ¡Ê£±Ç¯°ÊÆâ¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD126-IDX1   PIC 9(03)   VALUE   126.
           03  FILLER              PIC X(09)   VALUE   "113704210".
           03  FILLER              PIC X(09)   VALUE   "850190210".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ËýÀ­¿ÕÂ¡ÉÂÆ©ÀÏÍ½ËÉ»ØÆ³´ÉÍýÎÁ¡Ê£±Ç¯¤òÄ¶¤¨¤¿´ü´Ö¡Ë
           03  CHK-SRYCD126-IDX2   PIC 9(03)   VALUE   126.
           03  FILLER              PIC X(09)   VALUE   "113704310".
           03  FILLER              PIC X(09)   VALUE   "850190210".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ËýÀ­¿ÕÂ¡ÉÂÆ©ÀÏÍ½ËÉ»ØÆ³´ÉÍýÎÁ¡Ê£±Ç¯¤òÄ¶¤¨¤¿´ü´Ö¡Ë¡Ê¾ðÊóÄÌ¿®µ¡´ï¡Ë
           03  CHK-SRYCD126-IDX3   PIC 9(03)   VALUE   126.
           03  FILLER              PIC X(09)   VALUE   "113704410".
           03  FILLER              PIC X(09)   VALUE   "850190210".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (127)
      *    ±ó³ÖÏ¢·È¿ÇÎÅÎÁ¡Ê¿ÇÃÇ¤òÌÜÅª¡Ë
           03  CHK-SRYCD127-IDX    PIC 9(03)   VALUE   127.
           03  FILLER              PIC X(09)   VALUE   "113032110".
           03  FILLER              PIC X(09)   VALUE   "850100411".
           03  FILLER              PIC X(09)   VALUE   "820100846".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (128)
      *    ±ó³ÖÏ¢·È¿ÇÎÅÎÁ¡Ê¤½¤ÎÂ¾¡Ë
           03  CHK-SRYCD128-IDX    PIC 9(03)   VALUE   128.
           03  FILLER              PIC X(09)   VALUE   "113042910".
           03  FILLER              PIC X(09)   VALUE   "850100412".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (129)
      *    ¤³¤³¤í¤ÎÏ¢·È»ØÆ³ÎÁ¡Ê£±¡Ë
           03  CHK-SRYCD129-IDX    PIC 9(03)   VALUE   129.
           03  FILLER              PIC X(09)   VALUE   "113043010".
           03  FILLER              PIC X(09)   VALUE   "850100413".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (130)
      *    ¤³¤³¤í¤ÎÏ¢·È»ØÆ³ÎÁ¡Ê£²¡Ë
           03  CHK-SRYCD130-IDX    PIC 9(03)   VALUE   130.
           03  FILLER              PIC X(09)   VALUE   "113043110".
           03  FILLER              PIC X(09)   VALUE   "850100414".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (131)
      *    ¶ÛµÞË¬Ìä´Ç¸î²Ã»»¡ÊË¬Ìä´Ç¸î¡¦Ë¬Ìä´Ç¸î¡ÊÆ±°ì¡Ë·î£±£´ÆüÌÜ¤Þ¤Ç¡Ë
           03  CHK-SRYCD131-IDX    PIC 9(03)   VALUE   131.
           03  FILLER              PIC X(09)   VALUE   "114741010".
           03  FILLER              PIC X(09)   VALUE   "850190211".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ¶ÛµÞË¬Ìä´Ç¸î²Ã»»¡ÊË¬Ìä´Ç¸î¡¦Ë¬Ìä´Ç¸î¡ÊÆ±°ì¡Ë·î£±£µÆüÌÜ°Ê¹ß¡Ë
           03  CHK-SRYCD131-IDX1   PIC 9(03)   VALUE   131.
           03  FILLER              PIC X(09)   VALUE   "114741110".
           03  FILLER              PIC X(09)   VALUE   "850190211".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (132)
      *    ¼ê½ç½ñ²Ã»»¡ÊÀº¿À²ÊË¬Ìä´Ç¸î»Ø¼¨ÎÁ¡Ë
           03  CHK-SRYCD132-IDX    PIC 9(03)   VALUE   132.
           03  FILLER              PIC X(09)   VALUE   "180067770".
           03  FILLER              PIC X(09)   VALUE   "850190056".
           03  FILLER              PIC X(09)   VALUE   "820190052".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (133)
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£²¡Ë¥¤
           03  CHK-SRYCD133-IDX    PIC 9(03)   VALUE   133.
           03  FILLER              PIC X(09)   VALUE   "114042810".
           03  FILLER              PIC X(09)   VALUE   "850100417".
           03  FILLER              PIC X(09)   VALUE   "830100091".
           03  FILLER              PIC X(09)   VALUE   "850100102".
           03  FILLER              PIC X(09)   VALUE   "850100103".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    ºßÂð´µ¼ÔË¬Ìä¿ÇÎÅÎÁ¡Ê£²¡Ë¥í¡ÊÂ¾¤ÎÊÝ¸±°åÎÅµ¡´Ø¤«¤é¾Ò²ð¤µ¤ì¤¿´µ¼Ô¡Ë
           03  CHK-SRYCD133-IDX1   PIC 9(03)   VALUE   133.
           03  FILLER              PIC X(09)   VALUE   "114046310".
           03  FILLER              PIC X(09)   VALUE   "850100417".
           03  FILLER              PIC X(09)   VALUE   "830100091".
           03  FILLER              PIC X(09)   VALUE   "850100102".
           03  FILLER              PIC X(09)   VALUE   "850100103".
           03  FILLER              PIC X(144)  VALUE   SPACE.
      *    (134)
      *    Ç§ÃÎ¾ÉÀìÌç¿ÇÃÇ´ÉÍýÎÁ£²¡ÊÏ¢·È·¿¡Ë
           03  CHK-SRYCD134-IDX    PIC 9(03)   VALUE   134.
           03  FILLER              PIC X(09)   VALUE   "113042610".
           03  FILLER              PIC X(09)   VALUE   "850190004".
           03  FILLER              PIC X(09)   VALUE   "820190004".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (135)
      *    »ùÆ¸»×½Õ´üÀº¿À²ÊÀìÌç´ÉÍý²Ã»»¡Ê£±£¶ºÐÌ¤Ëþ¡Ë¡Ê¡Ê£±¡Ë°Ê³°¡Ë
           03  CHK-SRYCD135-IDX    PIC 9(03)   VALUE   135.
           03  FILLER              PIC X(09)   VALUE   "180067870".
           03  FILLER              PIC X(09)   VALUE   "850100239".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (136)
      *    °ÍÂ¸¾É½¸ÃÄÎÅË¡¡Ê¥¢¥ë¥³¡¼¥ë°ÍÂ¸¾É¡Ë
           03  CHK-SRYCD136-IDX    PIC 9(03)   VALUE   136.
           03  FILLER              PIC X(09)   VALUE   "180069870".
           03  FILLER              PIC X(09)   VALUE   "850100245".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    °åÎÅ´Ñ»¡°ÍÂ¸¾É½¸ÃÄÎÅË¡¡Ê¥¢¥ë¥³¡¼¥ë°ÍÂ¸¾É¡Ë
           03  CHK-SRYCD136-IDX1   PIC 9(03)   VALUE   136.
           03  FILLER              PIC X(09)   VALUE   "188018410".
           03  FILLER              PIC X(09)   VALUE   "850100245".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (137)
      *    ¼ê½ç½ñ²Ã»»¡ÊË¬Ìä´Ç¸î»Ø¼¨ÎÁ¡Ë
           03  CHK-SRYCD137-IDX    PIC 9(03)   VALUE   137.
           03  FILLER              PIC X(09)   VALUE   "114061070".
           03  FILLER              PIC X(09)   VALUE   "850100418".
           03  FILLER              PIC X(09)   VALUE   "820190052".
           03  FILLER              PIC X(162)  VALUE   SPACE.
      *    (138)
      *    ³°ÍèºßÂð¶¦Æ±»ØÆ³ÎÁ£±
           03  CHK-SRYCD138-IDX    PIC 9(03)   VALUE   138.
           03  FILLER              PIC X(09)   VALUE   "114061110".
           03  FILLER              PIC X(09)   VALUE   "850100490".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ³°ÍèºßÂð¶¦Æ±»ØÆ³ÎÁ£²
           03  CHK-SRYCD138-IDX1   PIC 9(03)   VALUE   138.
           03  FILLER              PIC X(09)   VALUE   "114061210".
           03  FILLER              PIC X(09)   VALUE   "850100490".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (139)
      *    ºßÂð¹³¶ÝÌôµÛÆþÎÅË¡ÍÑ¥Í¥Ö¥é¥¤¥¶²Ã»»¡Ê£±·îÌÜ¡Ë
           03  CHK-SRYCD139-IDX    PIC 9(03)   VALUE   139.
           03  FILLER              PIC X(09)   VALUE   "114051750".
           03  FILLER              PIC X(09)   VALUE   "820190053".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (140)
      *    ºßÂð¹³¶ÝÌôµÛÆþÎÅË¡ÍÑ¥Í¥Ö¥é¥¤¥¶²Ã»»¡Ê£²·îÌÜ°Ê¹ß¡Ë
           03  CHK-SRYCD140-IDX    PIC 9(03)   VALUE   140.
           03  FILLER              PIC X(09)   VALUE   "114051850".
           03  FILLER              PIC X(09)   VALUE   "850100419".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (141)
      *    Àº¿À²Ê¶ÛµÞË¬Ìä´Ç¸î²Ã»»¡Ê·î£±£´ÆüÌÜ¤Þ¤Ç¡Ë
           03  CHK-SRYCD141-IDX    PIC 9(03)   VALUE   141.
           03  FILLER              PIC X(09)   VALUE   "180761970".
           03  FILLER              PIC X(09)   VALUE   "850190234".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    Àº¿À²Ê¶ÛµÞË¬Ìä´Ç¸î²Ã»»¡Ê·î£±£µÆüÌÜ°Ê¹ß¡Ë
           03  CHK-SRYCD141-IDX1   PIC 9(03)   VALUE   141.
           03  FILLER              PIC X(09)   VALUE   "180762070".
           03  FILLER              PIC X(09)   VALUE   "850190234".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    (142)
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡¡Ë¡Ê£±£°£°£ã£í£²Ì¤Ëþ¡Ë
           03  CHK-SRYCD142-IDX    PIC 9(03)   VALUE   142.
           03  FILLER              PIC X(09)   VALUE   "140052170".
           03  FILLER              PIC X(09)   VALUE   "850100442".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡¡Ë¡Ê£±£°£°£ã£í£²°Ê¾å¡Ë
           03  CHK-SRYCD142-IDX1   PIC 9(03)   VALUE   142.
           03  FILLER              PIC X(09)   VALUE   "140052270".
           03  FILLER              PIC X(09)   VALUE   "850100442".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *    ½é²ó²Ã»»¡Ê¶É½ê±¢°µÊÄº¿½èÃÖ¡Ë¡ÊÆþ±¡¡Ë¡Ê£²£°£°£ã£í£²°Ê¾å¡Ë
           03  CHK-SRYCD142-IDX2   PIC 9(03)   VALUE   142.
           03  FILLER              PIC X(09)   VALUE   "140052370".
           03  FILLER              PIC X(09)   VALUE   "850100442".
           03  FILLER              PIC X(171)  VALUE   SPACE.
      *
       01  CHK-SRYCD-TABLE-AREA-R  REDEFINES   CHK-SRYCD-TABLE-AREA.
           03  CHK-SRYCD-TABLE.
               05  CHK-SRYCD-TABLE-O       OCCURS  874.
                   07  CHK-SRYCD-T-IDX PIC 9(03).
                   07  CHK-SRYCD-T-1   PIC X(09).
                   07  CHK-SRYCD-T-8-OCC.
                       09  CHK-SRYCD-T-8   PIC X(09)   OCCURS  20.
      *
       01  CHK-SRYCD-TABLE-MAX     PIC 9(04)   VALUE   874.
       01  CHK-SRYCD-T-8-MAX       PIC 9(03)   VALUE   20.
      *
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÍ­¸ú´ü´Ö
       01  CHK-SRYCD-YM-TABLE-AREA.
           03  CHK-SRYCD-YM-TBL        OCCURS  200.
               05  CHK-SRYCD-STYM-T
                                   PIC X(06).
               05  CHK-SRYCD-EDYM-T
                                   PIC X(06).
      *
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÂÐ¾ÝÆþ³°¶èÊ¬
      *    0:Æþ³° 1:Æþ±¡ 2:Æþ±¡³°
       01  CHK-SRYCD-NYUGAIKBN-TABLE-AREA.
           03  CHK-SRYCD-NYUGAIKBN-TBL.
               05  FILLER          PIC X(10)   VALUE   "0222002222".
               05  FILLER          PIC X(10)   VALUE   "2222220020".
               05  FILLER          PIC X(10)   VALUE   "2222000000".
               05  FILLER          PIC X(10)   VALUE   "2200222002".
               05  FILLER          PIC X(10)   VALUE   "2222222222".
               05  FILLER          PIC X(10)   VALUE   "2222220020".
               05  FILLER          PIC X(10)   VALUE   "0002020000".
               05  FILLER          PIC X(10)   VALUE   "0000011111".
               05  FILLER          PIC X(10)   VALUE   "1111111122".
               05  FILLER          PIC X(10)   VALUE   "2222222222".
               05  FILLER          PIC X(10)   VALUE   "0000222112".
               05  FILLER          PIC X(10)   VALUE   "2000000222".
               05  FILLER          PIC X(10)   VALUE   "2222222222".
               05  FILLER          PIC X(10)   VALUE   "2222220000".
               05  FILLER          PIC X(10)   VALUE   "21        ".
               05  FILLER          PIC X(50)   VALUE   SPACE.
           03  CHK-SRYCD-NYUGAIKBN-TBL-R
                                   REDEFINES   CHK-SRYCD-NYUGAIKBN-TBL.
               05  CHK-SRYCD-NYUGAIKBN-T
                                   PIC X(01)   OCCURS  200.
      *
       01  WRK-RECECOMCHK-TBL-OUT-MAX
                                   PIC 9(03)   VALUE   142.
      *
      *****************************************************************
      *    Ï¢ÍíÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSRECECOMCHK.INC".
      *
      *****************************************************************
      *
       PROCEDURE                   DIVISION    USING
                                   ORCSRECECOMCHKAREA.
       *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      * DISPLAY "LNK-RECECOMCHK-NYUGAIKBN =" LNK-RECECOMCHK-NYUGAIKBN
      * DISPLAY "LNK-RECECOMCHK-SRYCD     =" LNK-RECECOMCHK-SRYCD
      * DISPLAY "LNK-RECECOMCHK-SRYYM     =" LNK-RECECOMCHK-SRYYM
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
      *
           MOVE    ZERO            TO  LNK-RECECOMCHK-RC
           INITIALIZE                  ORCSRECECOMCHK-OUT-AREA
      *
      *    Æþ³°¶èÊ¬¥Á¥§¥Ã¥¯
           EVALUATE    LNK-RECECOMCHK-NYUGAIKBN
               WHEN    "1"
               WHEN    "2"
                   CONTINUE
               WHEN    OTHER
                   MOVE    1               TO  LNK-RECECOMCHK-RC
                   MOVE    1                   TO  FLG-END
                   GO  TO  100-INIT-EXT
           END-EVALUATE
      *
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥ÉÍ­¸ú´ü´ÖÀßÄê
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-RECECOMCHK-TBL-OUT-MAX
               EVALUATE    IDXX
                   WHEN    112
                   WHEN    113
                       MOVE    "202007"    TO  CHK-SRYCD-STYM-T (IDXX)
                   WHEN    114
                       MOVE    "202011"    TO  CHK-SRYCD-STYM-T (IDXX)
                   WHEN    115
                   WHEN    116
                       MOVE    "202202"    TO  CHK-SRYCD-STYM-T (IDXX)
                   WHEN    117  THRU  122
                   WHEN    127  THRU  130
                   WHEN    132  THRU  140
                   WHEN    142
                       MOVE    "202204"    TO  CHK-SRYCD-STYM-T (IDXX)
                   WHEN    123
                       MOVE    "202207"    TO  CHK-SRYCD-STYM-T (IDXX)
                   WHEN    124  THRU  126
                   WHEN    131
                   WHEN    141
                       MOVE    "202406"    TO  CHK-SRYCD-STYM-T (IDXX)
                   WHEN    OTHER
                       MOVE    "202004"    TO  CHK-SRYCD-STYM-T (IDXX)
               END-EVALUATE
               MOVE    "999999"            TO  CHK-SRYCD-EDYM-T (IDXX)
           END-PERFORM
      *
           MOVE        "202006"            TO  CHK-SRYCD-EDYM-T (77)
           MOVE        "202009"            TO  CHK-SRYCD-EDYM-T (80)
           MOVE        "202203"            TO  CHK-SRYCD-EDYM-T (97)
                                               CHK-SRYCD-EDYM-T (98)
                                               CHK-SRYCD-EDYM-T (99)
                                               CHK-SRYCD-EDYM-T (37)
           MOVE        "202205"            TO  CHK-SRYCD-EDYM-T (112)
                                               CHK-SRYCD-EDYM-T (113)
      *
           MOVE        "202405"            TO  CHK-SRYCD-EDYM-T (35)
                                               CHK-SRYCD-EDYM-T (36)
                                               CHK-SRYCD-EDYM-T (109)
                                               CHK-SRYCD-EDYM-T (128)
                                               CHK-SRYCD-EDYM-T (133)
      *
      *??
      *    PERFORM VARYING  IDXX    FROM    1   BY  1
      *             UNTIL   IDXX    >       CHK-SRYCD-TABLE-MAX
      *         MOVE CHK-SRYCD-T-IDX (IDXX) TO  IDYY
      *         display  IDXX "," CHK-SRYCD-T-IDX (IDXX) "," 
      *                         CHK-SRYCD-NYUGAIKBN-T (IDYY) ","
      *                         CHK-SRYCD-STYM-T (IDYY) ","
      *                         CHK-SRYCD-EDYM-T (IDYY) ","
      *                         CHK-SRYCD-T-1 (IDXX) ","
      *                         CHK-SRYCD-T-8  (IDXX 1) "," 
      *                         CHK-SRYCD-T-8  (IDXX 2) "," 
      *                         CHK-SRYCD-T-8  (IDXX 3) "," 
      *                         CHK-SRYCD-T-8  (IDXX 4) "," 
      *                         CHK-SRYCD-T-8  (IDXX 5) "," 
      *                         CHK-SRYCD-T-8  (IDXX 6) "," 
      *                         CHK-SRYCD-T-8  (IDXX 7) "," 
      *                         CHK-SRYCD-T-8  (IDXX 8) "," 
      *                         CHK-SRYCD-T-8  (IDXX 9) "," 
      *                         CHK-SRYCD-T-8  (IDXX 10) "," 
      *                         CHK-SRYCD-T-8  (IDXX 11) 
      *    END-PERFORM
      *??
           .
       100-INIT-EXT.
           EXIT
           .
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    LNK-RECECOMCHK-TBL-IN
                                   TO  LNK-RECECOMCHK-TBL-OUT
      *
      *    ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¥Á¥§¥Ã¥¯
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       CHK-SRYCD-TABLE-MAX
               MOVE    CHK-SRYCD-T-IDX (IDYY)
                                           TO  IDX
      *
               IF      LNK-RECECOMCHK-SRYCD
                                   =   CHK-SRYCD-T-1 (IDYY)
                   IF    ( CHK-SRYCD-NYUGAIKBN-T (IDX)
                                       =   "0"                      )
                   OR    ( CHK-SRYCD-NYUGAIKBN-T (IDX)
                                       =   LNK-RECECOMCHK-NYUGAIKBN )
                       IF    ( LNK-RECECOMCHK-SRYYM
                                           >=  CHK-SRYCD-STYM-T (IDX) )
                       AND   ( LNK-RECECOMCHK-SRYYM
                                           <=  CHK-SRYCD-EDYM-T (IDX) )
                           IF      LNK-RECECOMCHK-TBL-OUT-O(IDX)
                                               =   SPACE
                               MOVE    "1"         TO
                                       LNK-RECECOMCHK-TBL-OUT-O (IDX)
                           END-IF
                       END-IF
                   END-IF
               END-IF 
           END-PERFORM
      *
           IF      LNK-RECECOMCHK-SRYCD (1:1)
                                   =   "8"
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       CHK-SRYCD-TABLE-MAX
                   MOVE    CHK-SRYCD-T-IDX (IDYY)
                                           TO  IDX
      *
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL   IDZZ    >       CHK-SRYCD-T-8-MAX
                           OR      CHK-SRYCD-T-8 (IDYY IDZZ)
                                           =       SPACE
                       IF      LNK-RECECOMCHK-SRYCD
                                           =   CHK-SRYCD-T-8 (IDYY IDZZ)
                           IF    ( CHK-SRYCD-NYUGAIKBN-T (IDX)
                                           =   "0"                     )
                           OR    ( CHK-SRYCD-NYUGAIKBN-T (IDX)
                                           =   LNK-RECECOMCHK-NYUGAIKBN)
                               IF    ( LNK-RECECOMCHK-SRYYM
                                           >=  CHK-SRYCD-STYM-T (IDX) )
                               AND   ( LNK-RECECOMCHK-SRYYM
                                           <=  CHK-SRYCD-EDYM-T (IDX) )
                                   MOVE    "9"         TO
                                       LNK-RECECOMCHK-TBL-OUT-O (IDX)
                                   MOVE    CHK-SRYCD-T-8-MAX
                                                       TO  IDZZ
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    DISPLAY "LNK-RECECOMCHK RC          =" LNK-RECECOMCHK-RC
      *
           .
       300-END-EXT.
           EXIT. 
