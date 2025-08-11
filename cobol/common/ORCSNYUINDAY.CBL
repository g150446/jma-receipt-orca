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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSNYUINDAY.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡ÎÁÌ¤»»ÄêÆü¼èÆÀ
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/12/13    NACL¡Ý¾®Æ¦Âô  ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *Ê¿ÀÅ£²£°Ç¯£´·î²þÄêÂÐ±þ
      *  04.02.00    NACL-¾®Æ¦Âô  08/03/05  Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¤ÎÆþ±¡ÄÉ²Ã
      *  04.02.00    NACL-¾®Æ¦Âô  08/06/02  µßµÞÈÂÁ÷¿ÇÎÅÎÁ¤Î»»ÄêÆü¤òÈ½ÄêÄÉ²Ã
      *Ê¿À®£³£°Ç¯£´·î²þÄêÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  18/03/07  Ã»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ¤ÎÈ½ÄêÊÑ¹¹
      *
      *ÎáÏÂ£²Ç¯£´·î²þÀµÂÐ±þ
      *  05.00.00    NACL-¾®Æ¦Âô  20/03/10  Ã»´üÂÚºß£³ÄÉ²ÃÂÐ±þ
      *  05.00.00    ORCAMO       21/10/19  ¼«±¡¤ÎÆ±ÆüºÆÆþÂà±¡ÅÐÏ¿Æü¤Ë¤Ä¤¤¤Æ
      *                                     ÊÝ¸±ÁÈ¹ç¤»¤Î¼èÆÀ¤òÊÑ¹¹
      *  05.02.00    ORCAMO       23/12/27  Æþ±¡¤Î¿ÇÎÅ¹Ô°Ù¤ÇÆþÎÏ¤µ¤ì¤¿
      *                                     µßµÞÈÂÁ÷¿ÇÎÅÎÁ¤Î»»ÄêÆüÈ½Äê¤òºï½ü
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-TANKI2                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDX1                        PIC 9(04).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
           03  IDX-TK3                     PIC 9(02).
      *
      *    ¥³¥ó¥¹¥¿¥ó¥ÈÃÍ
       01  CONSTANT-AREA.
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²
           03   CONS-TANKI2                PIC X(09) VALUE "190076810".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   CONS-TANKI2-SEIKATSU       PIC X(09) VALUE "190125310".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
           03   CONS-TANKI3                PIC X(09) VALUE "190130410".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë
           03   CONS-TANKI3-SEIKATSU       PIC X(09) VALUE "190130510".
      *
           03   CONS-TANKI3-H26-AREA.
      *      Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼£±¡Ê·ÈÂÓÍÑÁõÃÖ»ÈÍÑ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "97".
             05   FILLER                   PIC X(09) VALUE "190177610".
      *      Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼£²¡ÊÂ¿ÅÀ´¶°µ¥»¥ó¥µ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "99".
             05   FILLER                   PIC X(09) VALUE "190177810".
      *      Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼£³¡Ê£±µÚ¤Ó£²°Ê³°¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9B".
             05   FILLER                   PIC X(09) VALUE "190178010".
      *      Ã»¼ê£³¡Ê¾®»ù¿©Êª¥¢¥ì¥ë¥®¡¼Éé²Ù¸¡ºº¡Ë
             05   FILLER                   PIC X(02) VALUE "9D".
             05   FILLER                   PIC X(09) VALUE "190178210".
      *      Ã»¼ê£³¡ÊÁ°Î©Á£¿ËÀ¸¸¡Ë¡¡Ë
             05   FILLER                   PIC X(02) VALUE "9F".
             05   FILLER                   PIC X(09) VALUE "190178410".
      *      Ã»¼ê£³¡Êæþ½­¾É¼ê½Ñ¡ÊÈéÉæÍ­ÌÓÉôÀÚ½ü½Ñ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9H".
             05   FILLER                   PIC X(09) VALUE "190178610".
      *      Ã»¼ê£³¡Ê´ØÀá¶À²¼¼êº¬´É³«Êü¼ê½Ñ¡Ë
             05   FILLER                   PIC X(02) VALUE "9J".
             05   FILLER                   PIC X(09) VALUE "190178810".
      *      Ã»¼ê£³¡Ê¶»¹Ð¶À²¼¸ò´¶¿À·ÐÀáÀÚ½ü½Ñ¡ÊÎ¾Â¦¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9L".
             05   FILLER                   PIC X(09) VALUE "190179010".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥ºÁÞÆþ¡¦¤½¤ÎÂ¾¤Î¤â¤Î¡Ë
             05   FILLER                   PIC X(02) VALUE "9N".
             05   FILLER                   PIC X(09) VALUE "190179210".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡Ê´ãÆâ¥ì¥ó¥ºÁÞÆþ¤·¤Ê¤¤¾ì¹ç¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9P".
             05   FILLER                   PIC X(09) VALUE "190179410".
      *      Ã»¼ê£³¡ÊÆýÁ£¼ðáçÅ¦½Ð½Ñ¡ÊÄ¹·Â£µ£ã£íÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9R".
             05   FILLER                   PIC X(09) VALUE "190179610".
      *      Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî¼ê½Ñ¡ÊÈ´µîÀÚ½ü½Ñ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9T".
             05   FILLER                   PIC X(09) VALUE "190179810".
      *      Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî¼ê½Ñ¡Ê¹Å²½ÎÅË¡¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9V".
             05   FILLER                   PIC X(09) VALUE "190180010".
      *      Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî¼ê½Ñ¡Ê¹â°Ì·ëå§½Ñ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9X".
             05   FILLER                   PIC X(09) VALUE "190180210".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£±£µºÐÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "9Z".
             05   FILLER                   PIC X(09) VALUE "190180410".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£±£µºÐ°Ê¾å¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "A1".
             05   FILLER                   PIC X(09) VALUE "190180610".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£±£µºÐÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "A3".
             05   FILLER                   PIC X(09) VALUE "190180810".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£±£µºÐ°Ê¾å¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "A5".
             05   FILLER                   PIC X(09) VALUE "190181010".
      *      Ã»¼ê£³¡ÊÆâ»ë¶ÀÅª·ëÄ²¥Ý¥ê¡¼¥×¡¦Ç´ËìÀÚ½ü½Ñ£²£ã£íÌ¤Ëþ¡Ë
             05   FILLER                   PIC X(02) VALUE "A7".
             05   FILLER                   PIC X(09) VALUE "190181210".
      *      Ã»¼ê£³¡ÊÆâ»ë¶ÀÅª·ëÄ²¥Ý¥ê¡¼¥×¡¦Ç´ËìÀÚ½ü½Ñ£²£ã£í°Ê¾å¡Ë
             05   FILLER                   PIC X(02) VALUE "A9".
             05   FILLER                   PIC X(09) VALUE "190181410".
      *      Ã»¼ê£³¡Ê¼¦³Ë¼ê½Ñ¡Ê¹Å²½ÎÅË¡¡Ê»ÍÃÊ³¬Ãí¼ÍË¡¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "AB".
             05   FILLER                   PIC X(09) VALUE "190181610".
      *      Ã»¼ê£³¡Ê»ÒµÜ·ÛÉô¡ÊçµÉô¡ËÀÚ½ü½Ñ¡Ë
             05   FILLER                   PIC X(02) VALUE "AD".
             05   FILLER                   PIC X(09) VALUE "190181810".
      *      Ã»¼ê£³¡Ê»ÒµÜ¶À²¼»ÒµÜ¶Ú¼ðÅ¦½Ð½Ñ¡Ë
             05   FILLER                   PIC X(02) VALUE "AF".
             05   FILLER                   PIC X(09) VALUE "190182010".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦¿å¾½ÂÎÇ¹³ÈÄ¥¥ê¥ó¥°¡¦Ë¥Ãå¤Ê¤·¡Ë
             05   FILLER                   PIC X(02) VALUE "AW".
             05   FILLER                   PIC X(09) VALUE "190182750".
      *      Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼£±¡Ê·ÈÂÓÍÑÁõÃÖ»ÈÍÑ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "98".
             05   FILLER                   PIC X(09) VALUE "190177710".
      *      Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼£²¡ÊÂ¿ÅÀ´¶°µ¥»¥ó¥µ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9A".
             05   FILLER                   PIC X(09) VALUE "190177910".
      *      Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼£³¡Ê£±µÚ¤Ó£²°Ê³°¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9C".
             05   FILLER                   PIC X(09) VALUE "190178110".
      *      Ã»¼ê£³¡Ê¾®»ù¿©Êª¥¢¥ì¥ë¥®¡¼Éé²Ù¸¡ºº¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9E".
             05   FILLER                   PIC X(09) VALUE "190178310".
      *      Ã»¼ê£³¡ÊÁ°Î©Á£¿ËÀ¸¸¡Ë¡¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9G".
             05   FILLER                   PIC X(09) VALUE "190178510".
      *      Ã»¼ê£³¡Êæþ½­¾É¼ê½Ñ¡ÊÈéÉæÍ­ÌÓÉôÀÚ½ü½Ñ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9I".
             05   FILLER                   PIC X(09) VALUE "190178710".
      *      Ã»¼ê£³¡Ê´ØÀá¶À²¼¼êº¬´É³«Êü¼ê½Ñ¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9K".
             05   FILLER                   PIC X(09) VALUE "190178910".
      *      Ã»¼ê£³¡Ê¶»¹Ð¶À²¼¸ò´¶¿À·ÐÀáÀÚ½ü½Ñ¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9M".
             05   FILLER                   PIC X(09) VALUE "190179110".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥ºÁÞÆþ¡¦¤½¤ÎÂ¾¤Î¤â¤Î¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9O".
             05   FILLER                   PIC X(09) VALUE "190179310".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡Ê´ãÆâ¥ì¥ó¥ºÁÞÆþ¤·¤Ê¤¤¾ì¹ç¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9Q".
             05   FILLER                   PIC X(09) VALUE "190179510".
      *      Ã»¼ê£³¡ÊÆýÁ£¼ðáçÅ¦½Ð½Ñ¡ÊÄ¹·Â£µ£ã£íÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9S".
             05   FILLER                   PIC X(09) VALUE "190179710".
      *      Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî¼ê½Ñ¡ÊÈ´µîÀÚ½ü½Ñ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9U".
             05   FILLER                   PIC X(09) VALUE "190179910".
      *      Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî¼ê½Ñ¡Ê¹Å²½ÎÅË¡¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9W".
             05   FILLER                   PIC X(09) VALUE "190180110".
      *      Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî¼ê½Ñ¡Ê¹â°Ì·ëå§½Ñ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "9Y".
             05   FILLER                   PIC X(09) VALUE "190180310".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£±£µºÐÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "A0".
             05   FILLER                   PIC X(09) VALUE "190180510".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£±£µºÐ°Ê¾å¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "A2".
             05   FILLER                   PIC X(09) VALUE "190180710".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£±£µºÐÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "A4".
             05   FILLER                   PIC X(09) VALUE "190180910".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£±£µºÐ°Ê¾å¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "A6".
             05   FILLER                   PIC X(09) VALUE "190181110".
      *      Ã»¼ê£³¡ÊÆâ»ë¶ÀÅª·ëÄ²¥Ý¥ê¡¼¥×¡¦Ç´ËìÀÚ½ü½Ñ£²£ã£íÌ¤Ëþ¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "A8".
             05   FILLER                   PIC X(09) VALUE "190181310".
      *      Ã»¼ê£³¡ÊÆâ»ë¶ÀÅª·ëÄ²¥Ý¥ê¡¼¥×¡¦Ç´ËìÀÚ½ü½Ñ£²£ã£í°Ê¾å¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "AA".
             05   FILLER                   PIC X(09) VALUE "190181510".
      *      Ã»¼ê£³¡Ê¼¦³Ë¼ê½Ñ¡Ê¹Å²½ÎÅË¡¡Ê»ÍÃÊ³¬Ãí¼ÍË¡¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "AC".
             05   FILLER                   PIC X(09) VALUE "190181710".
      *      Ã»¼ê£³¡Ê»ÒµÜ·ÛÉô¡ÊçµÉô¡ËÀÚ½ü½Ñ¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "AE".
             05   FILLER                   PIC X(09) VALUE "190181910".
      *      Ã»¼ê£³¡Ê»ÒµÜ¶À²¼»ÒµÜ¶Ú¼ðÅ¦½Ð½Ñ¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "AG".
             05   FILLER                   PIC X(09) VALUE "190182110".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦¿å¾½ÂÎÇ¹³ÈÄ¥¥ê¥ó¥°¡¦Ë¥Ãå¤Ê¤·¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "AX".
             05   FILLER                   PIC X(09) VALUE "190182850".
      *
           03  CONS-TANKI3-H26-AREA-R  REDEFINES  CONS-TANKI3-H26-AREA.
             05   CONS-TANKI3-H26-TBL      OCCURS  48.
                07   CONS-TANKI3-H26-KBNCD PIC X(02).
                07   CONS-TANKI3-H26-SRYCD PIC X(09).
      *
           03   CONS-TANKI3-H28-AREA.
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥ºÁÞÆþ¡¦¤½¤ÎÂ¾¡¦Î¾Â¦¡Ë
             05   FILLER                   PIC X(02) VALUE "AY".
             05   FILLER                   PIC X(09) VALUE "190195910".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥ºÁÞÆþ¤·¤Ê¤¤¡¦Î¾Â¦¡Ë
             05   FILLER                   PIC X(02) VALUE "B0".
             05   FILLER                   PIC X(09) VALUE "190196110".
      *      Ã»¼ê£³¡Ê·ÐÈéÅª¥·¥ã¥ó¥È³ÈÄ¥½Ñ¡¦·ìÀò½üµî½Ñ¡Ë
             05   FILLER                   PIC X(02) VALUE "B2".
             05   FILLER                   PIC X(09) VALUE "190196310".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£³ºÐÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "B4".
             05   FILLER                   PIC X(09) VALUE "190196510".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£³ºÐ°Ê¾å£¶ºÐÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "B6".
             05   FILLER                   PIC X(09) VALUE "190196610".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£¶ºÐ°Ê¾å£±£µºÐÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "B8".
             05   FILLER                   PIC X(09) VALUE "190196710".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£³ºÐÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "BA".
             05   FILLER                   PIC X(09) VALUE "190197110".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£³ºÐ°Ê¾å£¶ºÐÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "BC".
             05   FILLER                   PIC X(09) VALUE "190197210".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£¶ºÐ°Ê¾å£±£µºÐÌ¤Ëþ¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "BE".
             05   FILLER                   PIC X(09) VALUE "190197310".
      *      Ã»¼ê£³¡ÊÂÎ³°¾×·âÇÈ¿Õ¡¦Ç¢´É·ëÀÐÇËºÕ½Ñ¡Ë
             05   FILLER                   PIC X(02) VALUE "BG".
             05   FILLER                   PIC X(09) VALUE "190197710".
      *      Ã»¼ê£³¡Ê¥¬¥ó¥Þ¥Ê¥¤¥Õ¤Ë¤è¤ëÄê°ÌÊü¼ÍÀþ¼£ÎÅ¡Ë
             05   FILLER                   PIC X(02) VALUE "BI".
             05   FILLER                   PIC X(09) VALUE "190197910".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥ºÁÞÆþ¡¦¤½¤ÎÂ¾¡¦Î¾Â¦¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "AZ".
             05   FILLER                   PIC X(09) VALUE "190196010".
      *      Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥ºÁÞÆþ¤·¤Ê¤¤¡¦Î¾Â¦¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "B1".
             05   FILLER                   PIC X(09) VALUE "190196210".
      *      Ã»¼ê£³¡Ê·ÐÈéÅª¥·¥ã¥ó¥È³ÈÄ¥½Ñ¡¦·ìÀò½üµî½Ñ¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "B3".
             05   FILLER                   PIC X(09) VALUE "190196410".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£³ºÐÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "B5".
             05   FILLER                   PIC X(09) VALUE "190196810".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£³ºÐ°Ê¾å£¶ºÐÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "B7".
             05   FILLER                   PIC X(09) VALUE "190196910".
      *      Ã»¼ê£³¡ÊÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ê£¶ºÐ°Ê¾å£±£µºÐÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "B9".
             05   FILLER                   PIC X(09) VALUE "190197010".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£³ºÐÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "BB".
             05   FILLER                   PIC X(09) VALUE "190197410".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£³ºÐ°Ê¾å£¶ºÐÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "BD".
             05   FILLER                   PIC X(09) VALUE "190197510".
      *      Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼ÁÍ·Â¥Ø¥ë¥Ë¥¢¼ê½Ñ¡Ë¡Ê£¶ºÐ°Ê¾å£±£µºÐÌ¤Ëþ¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "BF".
             05   FILLER                   PIC X(09) VALUE "190197610".
      *      Ã»¼ê£³¡ÊÂÎ³°¾×·âÇÈ¿Õ¡¦Ç¢´É·ëÀÐÇËºÕ½Ñ¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "BH".
             05   FILLER                   PIC X(09) VALUE "190197810".
      *      Ã»¼ê£³¡Ê¥¬¥ó¥Þ¥Ê¥¤¥Õ¤Ë¤è¤ëÄê°ÌÊü¼ÍÀþ¼£ÎÅ¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "BJ".
             05   FILLER                   PIC X(09) VALUE "190198010".
      *
           03  CONS-TANKI3-H28-AREA-R  REDEFINES  CONS-TANKI3-H28-AREA.
             05   CONS-TANKI3-H28-TBL      OCCURS  22.
                07   CONS-TANKI3-H28-KBNCD PIC X(02).
                07   CONS-TANKI3-H28-SRYCD PIC X(09).
      *
           03   CONS-TANKI3-R02-AREA.
      *      Ã»¼ê£³¡Ê·ÐÈéÅª¥·¥ã¥ó¥È³ÈÄ¥½Ñ¡¦·ìÀò½üµî½Ñ¡Ê½é²ó¡Ë¡Ë
             05   FILLER                   PIC X(02) VALUE "CF".
             05   FILLER                   PIC X(09) VALUE "190220910".
      *      Ã»¼ê£³¡Ê·ÐÈéÅª¥·¥ã¥ó¥È³ÈÄ¥½Ñ¡¦·ìÀò½üµî½Ñ¡¦£³·î°ÊÆâ¡Ë
             05   FILLER                   PIC X(02) VALUE "CH".
             05   FILLER                   PIC X(09) VALUE "190221110".
      *      Ã»¼ê£³¡Ê·ÐÈéÅª¥·¥ã¥ó¥È³ÈÄ¥½Ñ¡¦·ìÀò½üµî½Ñ¡Ê½é²ó¡Ë¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "CG".
             05   FILLER                   PIC X(09) VALUE "190221010".
      *      Ã»¼ê£³¡Ê·ÐÈéÅª¥·¥ã¥ó¥È³ÈÄ¥½Ñ¡¦·ìÀò½üµî½Ñ¡¦£³·î°ÊÆâ¡Ë¡ÊÀ¸³è¡Ë
             05   FILLER                   PIC X(02) VALUE "CI".
             05   FILLER                   PIC X(09) VALUE "190221210".
      *
           03  CONS-TANKI3-R02-AREA-R  REDEFINES  CONS-TANKI3-R02-AREA.
             05   CONS-TANKI3-R02-TBL      OCCURS  4.
                07   CONS-TANKI3-R02-KBNCD PIC X(02).
                07   CONS-TANKI3-R02-SRYCD PIC X(09).
      *
           03   CONS-TANKI3-R04-AREA.
      *        Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼¡¦£±¡¢£²°Ê³°¡¦°ÂÁ´ÀºÅÙ´ÉÍý²¼¡Ë
               05  FILLER          PIC X(02)   VALUE "CS".
               05  FILLER          PIC X(09)   VALUE "190265610".
      *        Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼¡¦£±¡¢£²°Ê³°¡¦¤½¤ÎÂ¾¡Ë
               05  FILLER          PIC X(02)   VALUE "CU".
               05  FILLER          PIC X(09)   VALUE "190265810".
      *        Ã»¼ê£³¡Ê£Í£Ó£Ì£Ô¡Ë
               05  FILLER          PIC X(02)   VALUE "CW".
               05  FILLER          PIC X(09)   VALUE "190266010".
      *        Ã»¼ê£³¡ÊÆâÊ¬ÈçÉé²Ù»î¸³¡¦²¼¿âÂÎÁ°ÍÕÉé²Ù»î¸³¡Ê£Ç£È¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "CY".
               05  FILLER          PIC X(09)   VALUE "190266210".
      *        Ã»¼ê£³¡Ê·ÐÈéÅªÊü¼ÍÀþ¼£ÎÅÍÑ¶âÂ°¥Þ¡¼¥«¡¼Î±ÃÖ½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "D0".
               05  FILLER          PIC X(09)   VALUE "190266410".
      *        Ã»¼ê£³¡Ê»Í»è¡¦¶í´´ÆðÉô¼ðáçÅ¦½Ð½Ñ¡Ê¼ê¡Ë¡Ë 
               05  FILLER          PIC X(02)   VALUE "D2".
               05  FILLER          PIC X(09)   VALUE "190266610".
      *        Ã»¼ê£³¡Ê¹üÀÞ´Ñ·ìÅª¼ê½Ñ¡Ê¼ê½®¾õ¹ü¡Ë¡Ë 
               05  FILLER          PIC X(02)   VALUE "D4".
               05  FILLER          PIC X(09)   VALUE "190266810".
      *        Ã»¼ê£³¡Ê¹üÆâ°ÛÊª¡ÊÁÞÆþÊª¤ò´Þ¤à¡Ë½üµî½Ñ¡ÊÁ°ÏÓ¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "D6".
               05  FILLER          PIC X(09)   VALUE "190267010".
      *        Ã»¼ê£³¡Ê¹üÆâ°ÛÊª¡ÊÁÞÆþÊª¤ò´Þ¤à¡Ë½üµî½Ñ¡Êº¿¹ü¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "D8".
               05  FILLER          PIC X(09)   VALUE "190267210".
      *        Ã»¼ê£³¡Ê¹üÆâ°ÛÊª¡ÊÁÞÆþÊª¤ò´Þ¤à¡Ë½üµî½Ñ¡Ê¼ê¡Ë¡Ë 
               05  FILLER          PIC X(02)   VALUE "DA".
               05  FILLER          PIC X(09)   VALUE "190267410".
      *        Ã»¼ê£³¡Ê¥¬¥ó¥°¥ê¥ª¥óÅ¦½Ð½Ñ¡Ê¼ê¡Ë¡Ë 
               05  FILLER          PIC X(02)   VALUE "DC".
               05  FILLER          PIC X(09)   VALUE "190267610".
      *        Ã»¼ê£³¡ÊÎÞ´É¥Á¥å¡¼¥ÖÁÞÆþ½Ñ¡ÊÎÞÆ»Æâ»ë¶À¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "DE".
               05  FILLER          PIC X(09)   VALUE "190267810".
      *        Ã»¼ê£³¡Ê´ãâÛÆâÈ¿¾É¼ê½Ñ¡ÊÈéÉæÀÚ³«Ë¡¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "DG".
               05  FILLER          PIC X(09)   VALUE "190268010".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê´ãâÛµó¶ÚÁ°Å¾Ë¡¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "DI".
               05  FILLER          PIC X(09)   VALUE "190268210".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê¤½¤ÎÂ¾¤Î¤â¤Î¡Ë¡Ë 
               05  FILLER          PIC X(02)   VALUE "DK".
               05  FILLER          PIC X(09)   VALUE "190268410".
      *        Ã»¼ê£³¡ÊÍã¾õÊÒ¼ê½Ñ¡ÊÊÛ¤Î°Ü¿¢¤òÍ×¤¹¤ë¤â¤Î¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "DM".
               05  FILLER          PIC X(09)   VALUE "190268610".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡Ê¸åÅ¾Ë¡¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "DO".
               05  FILLER          PIC X(09)   VALUE "190268810".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡ÊÁ°Å¾Ë¡µÚ¤Ó¸åÅ¾Ë¡¤ÎÊ»»Ü¡Ë¡Ë  
               05  FILLER          PIC X(02)   VALUE "DQ".
               05  FILLER          PIC X(09)   VALUE "190269010".
      *        Ã»¼ê£³¡Ê¼£ÎÅÅª³ÑËìÀÚ½ü½Ñ¡¦¥¨¥­¥·¥Þ¥ì¡¼¥¶¡¼¡Ë
               05  FILLER          PIC X(02)   VALUE "DS".
               05  FILLER          PIC X(09)   VALUE "190269210".
      *        Ã»¼ê£³¡ÊÎÐÆâ¾ã¼ê½Ñ¡¦¿å¾½ÂÎºÆ·ú½ÑÊ»ÍÑ´ãÆâ¥É¥ì¡¼¥óÁÞÆþ¡Ë
               05  FILLER          PIC X(02)   VALUE "DU".
               05  FILLER          PIC X(09)   VALUE "190269410".
      *        Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥º¤òÁÞÆþ¤·¤Ê¤¤¡¦ÊÒÂ¦¡Ë 
               05  FILLER          PIC X(02)   VALUE "DW".
               05  FILLER          PIC X(09)   VALUE "190269610".
      *        Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥º¤òÁÞÆþ¤·¤Ê¤¤¡¦Î¾Â¦¡Ë 
               05  FILLER          PIC X(02)   VALUE "DY".
               05  FILLER          PIC X(09)   VALUE "190269810".
      *        Ã»¼ê£³¡Ê¸ÝËì·ÁÀ®¼ê½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "E0".
               05  FILLER          PIC X(09)   VALUE "190270010".
      *        Ã»¼ê£³¡ÊÉ¡¹ü¹üÀÞÀ°Éü¸ÇÄê½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "E2".
               05  FILLER          PIC X(09)   VALUE "190270210".
      *        Ã»¼ê£³¡Ê¹¢Æ¬¡¦À¼ÂÓ¥Ý¥ê¡¼¥×ÀÚ½ü½Ñ¡ÊÄ¾Ã£¹¢Æ¬¶À¡¢¥Õ¥¡¥¤¥Ð¡¼¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "E4".
               05  FILLER          PIC X(09)   VALUE "190270410".
      *        Ã»¼ê£³¡ÊÆýÁ£¼ðáçÅ¦½Ð½Ñ¡ÊÄ¾·Â£µ£ã£í°Ê¾å¡Ë  
               05  FILLER          PIC X(02)   VALUE "E6".
               05  FILLER          PIC X(09)   VALUE "190270610".
      *        Ã»¼ê£³¡ÊÂçÉúºßÀÅÌ®È´µî½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "E8".
               05  FILLER          PIC X(09)   VALUE "190270810".
      *        Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî·ì´ÉÆâ¾Æ¼Þ½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "EA".
               05  FILLER          PIC X(09)   VALUE "190271010".
      *        Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî·ì´ÉÆâºÉÀò½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "EC".
               05  FILLER          PIC X(09)   VALUE "190271210".
      *        Ã»¼ê£³¡ÊæêÌç¥Ý¥ê¡¼¥×ÀÚ½ü½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "EE".
               05  FILLER          PIC X(09)   VALUE "190271410".
      *        Ã»¼ê£³¡ÊæêÌçÀí·½¥³¥ó¥¸¥í¡¼¥àÀÚ½ü½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "EG".
               05  FILLER          PIC X(09)   VALUE "190271610".
       *        Ã»¼ê£³¡ÊÇ¢¼º¶Ø¼ê½Ñ¡Ê¥Ü¥Ä¥ê¥Ì¥¹ÆÇÁÇ¤Ë¤è¤ë¤â¤Î¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "EI".
               05  FILLER          PIC X(09)   VALUE "190271810".
      *        Ã»¼ê£³¡Ê¸²Èù¶À²¼Àºº÷ÀÅÌ®áî¼ê½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "EK".
               05  FILLER          PIC X(09)   VALUE "190272010".
      *        Ã»¼ê£³¡Ê»ÒµÜ¶À²¼Í­·ÔÇ´Ëì²¼¶Ú¼ðÀÚ½Ð¡¢¥Ý¥ê¡¼¥×¡¦ÅÅ²ò±Õ¡Ë 
               05  FILLER          PIC X(02)   VALUE "EM".
               05  FILLER          PIC X(09)   VALUE "190272210".
      *        Ã»¼ê£³¡Ê»ÒµÜ¶À²¼Í­·ÔÇ´Ëì²¼¶Ú¼ðÀÚ½Ð¡¢¥Ý¥ê¡¼¥×¡¦¤½¤ÎÂ¾¡Ë
               05  FILLER          PIC X(02)   VALUE "EO".
               05  FILLER          PIC X(09)   VALUE "190272410".
      *        Ã»¼ê£³¡Ê»ÒµÜ¶À²¼»ÒµÜ¶Ú¼ðÅ¦½Ð½Ñ¡ÊÅÅ²ò¼ÁÍÏ±ÕÍøÍÑ¡Ë¡Ë 
               05  FILLER          PIC X(02)   VALUE "EQ".
               05  FILLER          PIC X(09)   VALUE "190272610".
      *        Ã»¼ê£³¡Ê»ÒµÜ¶À²¼»ÒµÜ¶Ú¼ðÅ¦½Ð½Ñ¡Ê¤½¤ÎÂ¾¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "ES".
               05  FILLER          PIC X(09)   VALUE "190272810".
      *        Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼Íñ´É·ÁÀ®½Ñ¡Ë
               05  FILLER          PIC X(02)   VALUE "EU".
               05  FILLER          PIC X(09)   VALUE "190273010".
      *        Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼£±¡¢£²°Ê³°¡¦°ÂÁ´ÀºÅÙ´ÉÍý²¼À¸³è
               05  FILLER          PIC X(02)   VALUE "CT".
               05  FILLER          PIC X(09)   VALUE "190265710".
      *        Ã»¼ê£³¡Ê½ªÌë¿çÌ²¥Ý¥ê¥°¥é¥Õ¥£¡¼£±¡¢£²°Ê³°¡¦¤½¤ÎÂ¾¡¦À¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "CV".
               05  FILLER          PIC X(09)   VALUE "190265910".
      *        Ã»¼ê£³¡Ê£Í£Ó£Ì£Ô¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "CX".
               05  FILLER          PIC X(09)   VALUE "190266110".
      *        Ã»¼ê£³¡ÊÆâÊ¬ÈçÉé²Ù»î¸³¡¦²¼¿âÂÎÁ°ÍÕÉé²Ù»î¸³¡Ê£Ç£È¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "CZ".
               05  FILLER          PIC X(09)   VALUE "190266310".
      *        Ã»¼ê£³¡Ê·ÐÈéÅªÊü¼ÍÀþ¼£ÎÅÍÑ¶âÂ°¥Þ¡¼¥«¡¼Î±ÃÖ½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "D1".
               05  FILLER          PIC X(09)   VALUE "190266510".
      *        Ã»¼ê£³¡Ê»Í»è¡¦¶í´´ÆðÉô¼ðáçÅ¦½Ð½Ñ¡Ê¼ê¡Ë¡Ë¡ÊÀ¸³è¡Ë 
               05  FILLER          PIC X(02)   VALUE "D3".
               05  FILLER          PIC X(09)   VALUE "190266710".
      *        Ã»¼ê£³¡Ê¹üÀÞ´Ñ·ìÅª¼ê½Ñ¡Ê¼ê½®¾õ¹ü¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "D5".
               05  FILLER          PIC X(09)   VALUE "190266910".
      *        Ã»¼ê£³¡Ê¹üÆâ°ÛÊª¡ÊÁÞÆþÊª¤ò´Þ¤à¡Ë½üµî½Ñ¡ÊÁ°ÏÓ¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "D7".
               05  FILLER          PIC X(09)   VALUE "190267110".
      *        Ã»¼ê£³¡Ê¹üÆâ°ÛÊª¡ÊÁÞÆþÊª¤ò´Þ¤à¡Ë½üµî½Ñ¡Êº¿¹ü¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "D9".
               05  FILLER          PIC X(09)   VALUE "190267310".
      *        Ã»¼ê£³¡Ê¹üÆâ°ÛÊª¡ÊÁÞÆþÊª¤ò´Þ¤à¡Ë½üµî½Ñ¡Ê¼ê¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DB".
               05  FILLER          PIC X(09)   VALUE "190267510".
      *        Ã»¼ê£³¡Ê¥¬¥ó¥°¥ê¥ª¥óÅ¦½Ð½Ñ¡Ê¼ê¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DD".
               05  FILLER          PIC X(09)   VALUE "190267710".
      *        Ã»¼ê£³¡ÊÎÞ´É¥Á¥å¡¼¥ÖÁÞÆþ½Ñ¡ÊÎÞÆ»Æâ»ë¶À¡Ë¡Ë¡ÊÀ¸³è¡Ë 
               05  FILLER          PIC X(02)   VALUE "DF".
               05  FILLER          PIC X(09)   VALUE "190267910".
      *        Ã»¼ê£³¡Ê´ãâÛÆâÈ¿¾É¼ê½Ñ¡ÊÈéÉæÀÚ³«Ë¡¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DH".
               05  FILLER          PIC X(09)   VALUE "190268110".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê´ãâÛµó¶ÚÁ°Å¾Ë¡¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DJ".
               05  FILLER          PIC X(09)   VALUE "190268310".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê¤½¤ÎÂ¾¤Î¤â¤Î¡Ë¡Ë¡ÊÀ¸³è¡Ë 
               05  FILLER          PIC X(02)   VALUE "DL".
               05  FILLER          PIC X(09)   VALUE "190268510".
      *        Ã»¼ê£³¡ÊÍã¾õÊÒ¼ê½Ñ¡ÊÊÛ¤Î°Ü¿¢¤òÍ×¤¹¤ë¤â¤Î¡Ë¡Ë¡ÊÀ¸³è¡Ë 
               05  FILLER          PIC X(02)   VALUE "DN".
               05  FILLER          PIC X(09)   VALUE "190268710".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡Ê¸åÅ¾Ë¡¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DP".
               05  FILLER          PIC X(09)   VALUE "190268910".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡ÊÁ°Å¾Ë¡µÚ¤Ó¸åÅ¾Ë¡¤ÎÊ»»Ü¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DR".
               05  FILLER          PIC X(09)   VALUE "190269110".
      *        Ã»¼ê£³¡Ê¼£ÎÅÅª³ÑËìÀÚ½ü½Ñ¡¦¥¨¥­¥·¥Þ¥ì¡¼¥¶¡¼¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DT".
               05  FILLER          PIC X(09)   VALUE "190269310".
      *        Ã»¼ê£³¡ÊÎÐÆâ¾ã¼ê½Ñ¡¦¿å¾½ÂÎºÆ·ú½ÑÊ»ÍÑ´ãÆâ¥É¥ì¡¼¥óÁÞÆþ¡¦À¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DV".
               05  FILLER          PIC X(09)   VALUE "190269510".
      *        Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥º¤òÁÞÆþ¤·¤Ê¤¤¡¦ÊÒÂ¦¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DX".
               05  FILLER          PIC X(09)   VALUE "190269710".
      *        Ã»¼ê£³¡Ê¿å¾½ÂÎºÆ·ú½Ñ¡¦´ãÆâ¥ì¥ó¥º¤òÁÞÆþ¤·¤Ê¤¤¡¦Î¾Â¦¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "DZ".
               05  FILLER          PIC X(09)   VALUE "190269910".
      *        Ã»¼ê£³¡Ê¸ÝËì·ÁÀ®¼ê½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "E1".
               05  FILLER          PIC X(09)   VALUE "190270110".
      *        Ã»¼ê£³¡ÊÉ¡¹ü¹üÀÞÀ°Éü¸ÇÄê½Ñ¡Ë¡ÊÀ¸³è¡Ë  
               05  FILLER          PIC X(02)   VALUE "E3".
               05  FILLER          PIC X(09)   VALUE "190270310".
      *        Ã»¼ê£³¡Ê¹¢Æ¬¡¦À¼ÂÓ¥Ý¥ê¡¼¥×ÀÚ½ü¡ÊÄ¾Ã£¹¢Æ¬¶À¡¢¥Õ¥¡¥¤¥Ð¡¼À¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "E5".
               05  FILLER          PIC X(09)   VALUE "190270510".
      *        Ã»¼ê£³¡ÊÆýÁ£¼ðáçÅ¦½Ð½Ñ¡ÊÄ¾·Â£µ£ã£í°Ê¾å¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "E7".
               05  FILLER          PIC X(09)   VALUE "190270710".
      *        Ã»¼ê£³¡ÊÂçÉúºßÀÅÌ®È´µî½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "E9".
               05  FILLER          PIC X(09)   VALUE "190270910".
      *        Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî·ì´ÉÆâ¾Æ¼Þ½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "EB".
               05  FILLER          PIC X(09)   VALUE "190271110".
      *        Ã»¼ê£³¡Ê²¼»èÀÅÌ®áî·ì´ÉÆâºÉÀò½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "ED".
               05  FILLER          PIC X(09)   VALUE "190271310".
      *        Ã»¼ê£³¡ÊæêÌç¥Ý¥ê¡¼¥×ÀÚ½ü½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "EF".
               05  FILLER          PIC X(09)   VALUE "190271510".
      *        Ã»¼ê£³¡ÊæêÌçÀí·½¥³¥ó¥¸¥í¡¼¥àÀÚ½ü½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "EH".
               05  FILLER          PIC X(09)   VALUE "190271710".
      *        Ã»¼ê£³¡ÊÇ¢¼º¶Ø¼ê½Ñ¡Ê¥Ü¥Ä¥ê¥Ì¥¹ÆÇÁÇ¤Ë¤è¤ë¤â¤Î¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "EJ".
               05  FILLER          PIC X(09)   VALUE "190271910".
      *        Ã»¼ê£³¡Ê¸²Èù¶À²¼Àºº÷ÀÅÌ®áî¼ê½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "EL".
               05  FILLER          PIC X(09)   VALUE "190272110".
      *        Ã»Ã»¼ê£³¡Ê»ÒµÜ¶À²¼Í­·ÔÇ´Ëì²¼¶Ú¼ðÀÚ½Ð¡¢¥Ý¥ê¡¼¥×¡¦ÅÅ²ò±Õ¡¦À¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "EN".
               05  FILLER          PIC X(09)   VALUE "190272310".
      *        Ã»¼ê£³¡Ê»ÒµÜ¶À²¼Í­·ÔÇ´Ëì²¼¶Ú¼ðÀÚ½Ð¡¢¥Ý¥ê¡¼¥×¡¦¤½¤ÎÂ¾¡¦À¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "EP".
               05  FILLER          PIC X(09)   VALUE "190272510".
      *        Ã»¼ê£³¡Ê»ÒµÜ¶À²¼»ÒµÜ¶Ú¼ðÅ¦½Ð½Ñ¡ÊÅÅ²ò¼ÁÍÏ±ÕÍøÍÑ¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "ER".
               05  FILLER          PIC X(09)   VALUE "190272710".
      *        Ã»¼ê£³¡Ê»ÒµÜ¶À²¼»ÒµÜ¶Ú¼ðÅ¦½Ð½Ñ¡Ê¤½¤ÎÂ¾¡Ë¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "ET".
               05  FILLER          PIC X(09)   VALUE "190272910".
      *        Ã»¼ê£³¡ÊÊ¢¹Ð¶À²¼Íñ´É·ÁÀ®½Ñ¡Ë¡ÊÀ¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "EV".
               05  FILLER          PIC X(09)   VALUE "190273110".
      *
           03  CONS-TANKI3-R04-AREA-R  REDEFINES  CONS-TANKI3-R04-AREA.
             05   CONS-TANKI3-R04-TBL      OCCURS  76.
                07   CONS-TANKI3-R04-KBNCD PIC X(02).
                07   CONS-TANKI3-R04-SRYCD PIC X(09).
      *
           03   CONS-TANKI3-R06-AREA.
      *        Ã»¼ê£³¡ÊÎÞ´É¥Á¥å¡¼¥ÖÁÞÆþ½Ñ¡ÊÎÞÆ»Æâ»ë¶À¡Ë¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "G5".
               05  FILLER          PIC X(09)   VALUE "190873210".
      *        Ã»¼ê£³¡ÊÎÞ´É¥Á¥å¡¼¥ÖÁÞÆþ½Ñ¡ÊÎÞÆ»Æâ»ë¶À¡Ë¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "G7".
               05  FILLER          PIC X(09)   VALUE "190873410".
      *        Ã»¼ê£³¡Ê´ãâÛÆâÈ¿¾É¼ê½Ñ¡ÊÈéÉæÀÚ³«Ë¡¡Ë¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "G9".
               05  FILLER          PIC X(09)   VALUE "190873610".
      *        Ã»¼ê£³¡Ê´ãâÛÆâÈ¿¾É¼ê½Ñ¡ÊÈéÉæÀÚ³«Ë¡¡Ë¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GB".
               05  FILLER          PIC X(09)   VALUE "190873810".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê´ãâÛµó¶ÚÁ°Å¾Ë¡¡Ë¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GD".
               05  FILLER          PIC X(09)   VALUE "190874010".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê´ãâÛµó¶ÚÁ°Å¾Ë¡¡Ë¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GF".
               05  FILLER          PIC X(09)   VALUE "190874210".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê¤½¤ÎÂ¾¤Î¤â¤Î¡Ë¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GH".
               05  FILLER          PIC X(09)   VALUE "190874410".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê¤½¤ÎÂ¾¤Î¤â¤Î¡Ë¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GJ".
               05  FILLER          PIC X(09)   VALUE "190874610".
      *        Ã»¼ê£³¡ÊÍã¾õÊÒ¼ê½Ñ¡ÊÊÛ¤Î°Ü¿¢¤òÍ×¤¹¤ë¤â¤Î¡Ë¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GL".
               05  FILLER          PIC X(09)   VALUE "190874810".
      *        Ã»¼ê£³¡ÊÍã¾õÊÒ¼ê½Ñ¡ÊÊÛ¤Î°Ü¿¢¤òÍ×¤¹¤ë¤â¤Î¡Ë¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GN".
               05  FILLER          PIC X(09)   VALUE "190875010".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡Ê¸åÅ¾Ë¡¡Ë¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GP".
               05  FILLER          PIC X(09)   VALUE "190875210".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡Ê¸åÅ¾Ë¡¡Ë¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GR".
               05  FILLER          PIC X(09)   VALUE "190875410".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡ÊÁ°Å¾Ë¡µÚ¤Ó¸åÅ¾Ë¡¤ÎÊ»»Ü¡Ë¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GT".
               05  FILLER          PIC X(09)   VALUE "190875610".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡ÊÁ°Å¾Ë¡µÚ¤Ó¸åÅ¾Ë¡¤ÎÊ»»Ü¡Ë¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GV".
               05  FILLER          PIC X(09)   VALUE "190875810".
      *        Ã»¼ê£³¡Ê¼£ÎÅÅª³ÑËìÀÚ½ü½Ñ¡¦¥¨¥­¥·¥Þ¥ì¡¼¥¶¡¼¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GX".
               05  FILLER          PIC X(09)   VALUE "190876010".
      *        Ã»¼ê£³¡Ê¼£ÎÅÅª³ÑËìÀÚ½ü½Ñ¡¦¥¨¥­¥·¥Þ¥ì¡¼¥¶¡¼¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "GZ".
               05  FILLER          PIC X(09)   VALUE "190876210".
      *        Ã»¼ê£³¡ÊÎÐÆâ¾ã¼ê½Ñ¡¦¿å¾½ÂÎºÆ·ú½ÑÊ»ÍÑ´ãÆâ¥É¥ì¡¼¥óÁÞÆþ½Ñ¡ÊÊÒÂ¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "H1".
               05  FILLER          PIC X(09)   VALUE "190876410".
      *        Ã»¼ê£³¡ÊÎÐÆâ¾ã¼ê½Ñ¡¦¿å¾½ÂÎºÆ·ú½ÑÊ»ÍÑ´ãÆâ¥É¥ì¡¼¥óÁÞÆþ½Ñ¡ÊÎ¾Â¦¡Ë¡Ë
               05  FILLER          PIC X(02)   VALUE "H3".
               05  FILLER          PIC X(09)   VALUE "190876610".
      *
      *        Ã»¼ê£³¡ÊÎÞ´É¥Á¥å¡¼¥ÖÁÞÆþ½Ñ¡ÊÎÞÆ»Æâ»ë¶À¡Ë¡ÊÊÒÂ¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "G6".
               05  FILLER          PIC X(09)   VALUE "190873310".
      *        Ã»¼ê£³¡ÊÎÞ´É¥Á¥å¡¼¥ÖÁÞÆþ½Ñ¡ÊÎÞÆ»Æâ»ë¶À¡Ë¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "G8".
               05  FILLER          PIC X(09)   VALUE "190873510".
      *        Ã»¼ê£³¡Ê´ãâÛÆâÈ¿¾É¼ê½Ñ¡ÊÈéÉæÀÚ³«Ë¡¡Ë¡ÊÊÒÂ¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GA".
               05  FILLER          PIC X(09)   VALUE "190873710".
      *        Ã»¼ê£³¡Ê´ãâÛÆâÈ¿¾É¼ê½Ñ¡ÊÈéÉæÀÚ³«Ë¡¡Ë¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GC".
               05  FILLER          PIC X(09)   VALUE "190873910".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê´ãâÛµó¶ÚÁ°Å¾Ë¡¡Ë¡ÊÊÒÂ¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GE".
               05  FILLER          PIC X(09)   VALUE "190874110".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê´ãâÛµó¶ÚÁ°Å¾Ë¡¡Ë¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GG".
               05  FILLER          PIC X(09)   VALUE "190874310".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê¤½¤ÎÂ¾¤Î¤â¤Î¡Ë¡ÊÊÒÂ¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GI".
               05  FILLER          PIC X(09)   VALUE "190874510".
      *        Ã»¼ê£³¡Ê´ãâÛ²¼¿â¾É¼ê½Ñ¡Ê¤½¤ÎÂ¾¤Î¤â¤Î¡Ë¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GK".
               05  FILLER          PIC X(09)   VALUE "190874710".
      *        Ã»¼ê£³¡ÊÍã¾õÊÒ¼ê½Ñ¡ÊÊÛ¤Î°Ü¿¢¤òÍ×¤¹¤ë¤â¤Î¡Ë¡ÊÊÒÂ¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GM".
               05  FILLER          PIC X(09)   VALUE "190874910".
      *        Ã»¼ê£³¡ÊÍã¾õÊÒ¼ê½Ñ¡ÊÊÛ¤Î°Ü¿¢¤òÍ×¤¹¤ë¤â¤Î¡Ë¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GO".
               05  FILLER          PIC X(09)   VALUE "190875110".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡Ê¸åÅ¾Ë¡¡Ë¡ÊÊÒÂ¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GQ".
               05  FILLER          PIC X(09)   VALUE "190875310".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡Ê¸åÅ¾Ë¡¡Ë¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GS".
               05  FILLER          PIC X(09)   VALUE "190875510".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡ÊÁ°Å¾Ë¡µÚ¤Ó¸åÅ¾Ë¡¤ÎÊ»»Ü¡Ë¡ÊÊÒÂ¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GU".
               05  FILLER          PIC X(09)   VALUE "190875710".
      *        Ã»¼ê£³¡Ê¼Ð»ë¼ê½Ñ¡ÊÁ°Å¾Ë¡µÚ¤Ó¸åÅ¾Ë¡¤ÎÊ»»Ü¡Ë¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GW".
               05  FILLER          PIC X(09)   VALUE "190875910".
      *        Ã»¼ê£³¡Ê¼£ÎÅÅª³ÑËìÀÚ½ü½Ñ¡¦¥¨¥­¥·¥Þ¥ì¡¼¥¶¡¼¡ÊÊÒÂ¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "GY".
               05  FILLER          PIC X(09)   VALUE "190876110".
      *        Ã»¼ê£³¡Ê¼£ÎÅÅª³ÑËìÀÚ½ü½Ñ¡¦¥¨¥­¥·¥Þ¥ì¡¼¥¶¡¼¡ÊÎ¾Â¦¡Ë¡Ë¡ÊÀ¸³èÎÅÍÜ¡Ë
               05  FILLER          PIC X(02)   VALUE "H0".
               05  FILLER          PIC X(09)   VALUE "190876310".
      *        Ã»¼ê£³¡ÊÎÐÆâ¾ã¼ê½Ñ¡¦¿å¾½ÂÎºÆ·ú½ÑÊ»ÍÑ´ãÆâ¥É¥ì¡¼¥óÁÞÆþ¡¦ÊÒ¡¦À¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "H2".
               05  FILLER          PIC X(09)   VALUE "190876510".
      *        Ã»¼ê£³¡ÊÎÐÆâ¾ã¼ê½Ñ¡¦¿å¾½ÂÎºÆ·ú½ÑÊ»ÍÑ´ãÆâ¥É¥ì¡¼¥óÁÞÆþ¡¦Î¾¡¦À¸³è¡Ë
               05  FILLER          PIC X(02)   VALUE "H4".
               05  FILLER          PIC X(09)   VALUE "190876710".
           03  CONS-TANKI3-R06-AREA-R  REDEFINES  CONS-TANKI3-R06-AREA.
             05   CONS-TANKI3-R06-TBL      OCCURS  36.
                07   CONS-TANKI3-R06-KBNCD PIC X(02).
                07   CONS-TANKI3-R06-SRYCD PIC X(09).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
      *
           03  WRK-GAIHAKU-SRYCD           PIC X(09).
      *
      *    Æ±ÆüºÆÆþ±¡Æü¤ÎÍâÆü¼èÆÀÍÑ
           03  WRK-DOUJITSU-YMD            PIC X(08).
           03  WRK-DOUJITSU-YMDR    REDEFINES   WRK-DOUJITSU-YMD.
               05  WRK-DOUJITSU-YYMM       PIC 9(06).
               05  WRK-DOUJITSU-DD         PIC 9(02).
      *
      *    Æ±ÆüºÆÆþ±¡Æü¤ÎÍâÆü¼èÆÀÍÑ
           03  WRK-YOKUJITSU-YMD          PIC 9(08).
           03  WRK-YOKUJITSU-YMDR   REDEFINES   WRK-YOKUJITSU-YMD.
               05  WRK-YOKUJITSU-YYMM      PIC 9(06).
               05  WRK-YOKUJITSU-DD        PIC 9(02).
      *
      *    ·îËöÆü¼èÆÀÍÑ
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *    Ã»´üÂÚºß°Ê³°¤ÇÆþ±¡ÎÁ¤ò»»Äê¤·¤Æ¤¤¤ëÆü
           03  WRK-TANKI-IGAI              PIC X(01)  OCCURS 31.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    Æþ±¡¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡Ý
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡Ý
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ÊÝ¸±ÁÈ¤ß¹ç¤ï¤»
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¡Ý
           COPY    "CPTENSU.INC".
      *
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ¥¹¥ÑÎÎ°è
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSNYUINDAY-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-NYUINDAY-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-NYUINDAY-RC
      *
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-NYUINDAY-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    ¼ç½èÍý
           IF      LNK-NYUINDAY-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *     PERFORM    VARYING IDX   FROM   1   BY  1
      *                UNTIL IDX   >   31
      *       DISPLAY "IDX = " IDX " LNK-NYUINDAY-KBN = "
      *                 LNK-NYUINDAY-KBN(IDX)
      *               " LNK-NYUINDAY-TANKI2-KBN = " 
      *                LNK-NYUINDAY-TANKI2-KBN(IDX)
      *     END-PERFORM
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ¥Ñ¥é¥á¡¼¥¿¥Á¥§¥Ã¥¯
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    »»ÄêÇ¯·î¥Á¥§¥Ã¥¯
           IF     (LNK-NYUINDAY-SANTEIYM  =   SPACE)      OR
                  (LNK-NYUINDAY-SANTEIYM  <=   "200603")
               MOVE    1                TO  LNK-NYUINDAY-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼¸¡º÷
           INITIALIZE                       PTINF-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTINF-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF              NOT =   ZERO
               MOVE    2                  TO  LNK-NYUINDAY-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    Æþ±¡Ãæ¤Î¥Á¥§¥Ã¥¯¡ÊÆþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷¡Ë
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  NACCT-PTID
           MOVE    LNK-NYUINDAY-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "5"                    TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT          NOT =   ZERO
               MOVE    3                  TO  LNK-NYUINDAY-RC
           ELSE
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE    "key17"            TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT         NOT =   ZERO
                    MOVE    3             TO  LNK-NYUINDAY-RC
               END-IF
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key17"                TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¼èÆÀ
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
      *    ·îËöÆü¤ò¼èÆÀ
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    LNK-NYUINDAY-SANTEIYM
                                          TO  LNK-DAY7-YM
           CALL    "ORCSDAY"          USING   STS-AREA-DAY
                                              LNK-DAY7-AREA
           IF      STS-DAY-RC1            =   ZERO
               MOVE    LNK-DAY7-KOYOMI    TO  WRK-GETSUMATSU-YMD
           END-IF
      *
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ°Ê³°¤ÇÆþ±¡ÎÁ¤Î¥«¥ì¥ó¥À¡¼¤¬Î©¤Ã¤Æ¤¤¤ëÆü¤ò
      *    ÆÃÄê¤¹¤ë
           INITIALIZE                       NYUINACCT-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  NACCT-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  NACCT-PTID
           MOVE    LNK-NYUINDAY-SANTEIYM  TO  NACCT-SRYYM
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key36"                TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT          =   ZERO
               MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
               MOVE    "key36"            TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               PERFORM  UNTIL  ( FLG-NYUINACCT    =   1 )
                  PERFORM    210-NYUINACT-GET-SEC
      *           ÅÀ¿ô¥Þ¥¹¥¿¤è¤êÃ»´üÂÚºß°Ê³°¤Ç¤¢¤ë¤³¤È¤ÎÈ½Äê
                  INITIALIZE                  TNS-TENSU-REC
                  MOVE    LNK-NYUINDAY-HOSPNUM
                                          TO  TNS-HOSPNUM
                  MOVE    NSRY-SRYCD(1)   TO  TNS-SRYCD
                  MOVE    LNK-NYUINDAY-SANTEIYM
                                          TO  TNS-YUKOSTYMD(1:6)
                  MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
                  MOVE    TNS-YUKOSTYMD   TO  TNS-YUKOEDYMD
                  PERFORM   910-TENSU-READ-SEC
                  IF    (TNS-CDKBN-KBN     =   "A")   AND
                        (TNS-CDKBN-KBNNUM  =   400)   AND
                        (TNS-CDKBN-KOUBAN  =   2  OR 3)
                      CONTINUE
                  ELSE
                      PERFORM    VARYING IDX   FROM   1   BY  1
                        UNTIL IDX   >   31
                            IF  NACCT-DAY(IDX)  NOT =   ZERO
                                MOVE   "1"      TO   WRK-TANKI-IGAI(IDX)
                            END-IF
                      END-PERFORM
                  END-IF
                  MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                  MOVE    "key36"            TO  MCP-PATHNAME
                  PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"        TO  MCP-TABLE
           MOVE    "key36"                TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
           MOVE    "21"                   TO  PTNYUINRRK-TOKUTEI-NYUIN
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key49"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key49"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
           MOVE    "50"                   TO  PTNYUINRRK-TOKUTEI-NYUIN
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key49"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key49"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
           MOVE    "52"                   TO  PTNYUINRRK-TOKUTEI-NYUIN
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key49"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key49"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
           MOVE    "53"                   TO  PTNYUINRRK-TOKUTEI-NYUIN
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key49"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key49"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Ê¿À®£²£¶Ç¯²þÄê¤ÎÄÉ²ÃÊ¬
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ°Ê³°¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           IF       LNK-NYUINDAY-SANTEIYM   >=   "201404"
            PERFORM     VARYING   IDX-TK3  FROM   1  BY  1
             UNTIL     IDX-TK3   >   24
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-H26-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
            PERFORM     VARYING   IDX-TK3  FROM   25  BY  1
             UNTIL     IDX-TK3   >   48
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-H26-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
           END-IF
      *
      *    Ê¿À®£²£¸Ç¯²þÄê¤ÎÄÉ²ÃÊ¬
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ°Ê³°¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           IF       LNK-NYUINDAY-SANTEIYM   >=   "201604"
            PERFORM     VARYING   IDX-TK3  FROM   1  BY  1
             UNTIL     IDX-TK3   >   11
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-H28-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
            PERFORM     VARYING   IDX-TK3  FROM   12  BY  1
             UNTIL     IDX-TK3   >   22
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-H28-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
           END-IF
      *
      *    ÎáÏÂ£²Ç¯²þÄê¤ÎÄÉ²ÃÊ¬
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ°Ê³°¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           IF       LNK-NYUINDAY-SANTEIYM   >=   "202004"
            PERFORM     VARYING   IDX-TK3  FROM   1  BY  1
             UNTIL     IDX-TK3   >   2
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-R02-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
            PERFORM     VARYING   IDX-TK3  FROM   3  BY  1
             UNTIL     IDX-TK3   >   4
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-R02-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
           END-IF
      *
      *    ÎáÏÂ£´Ç¯²þÄê¤ÎÄÉ²ÃÊ¬
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ°Ê³°¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           IF       LNK-NYUINDAY-SANTEIYM   >=   "202204"
            PERFORM     VARYING   IDX-TK3  FROM   1  BY  1
             UNTIL     IDX-TK3   >   38
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-R04-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
            PERFORM     VARYING   IDX-TK3  FROM   39  BY  1
             UNTIL     IDX-TK3   >   76
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-R04-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
           END-IF
      *
      *    ÎáÏÂ£¶Ç¯²þÄê¤ÎÄÉ²ÃÊ¬
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ°Ê³°¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
           IF       LNK-NYUINDAY-SANTEIYM   >=   "202406"
            PERFORM     VARYING   IDX-TK3  FROM   1  BY  1
             UNTIL     IDX-TK3   >   18
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-R06-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
      *
      *    Æþ±¡ÍúÎò¤è¤êÃ»´üÂÚºß¼ê½ÑÅù´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ¡Ë¤Î»»ÄêÍ­Ìµ¤òÆÃÄê
            PERFORM     VARYING   IDX-TK3  FROM   19  BY  1
             UNTIL     IDX-TK3   >   36
             INITIALIZE                     PTNYUINRRK-REC
             MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
             MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
             MOVE    "3"                    TO  PTNYUINRRK-TOKUTEI-KBN
             MOVE    CONS-TANKI3-R06-KBNCD(IDX-TK3)
                                            TO  PTNYUINRRK-TOKUTEI-NYUIN
             MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
             MOVE    "key49"                TO  MCP-PATHNAME
             PERFORM 910-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key49"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                   ((PTNYUINRRK-TENSTUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                                   OR
                    (PTNYUINRRK-TENSTUYMD(1:6) = "999999"))
                      MOVE     ZERO       TO     IDX-ST
                                                 IDX-ED
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          <  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                            =  LNK-NYUINDAY-SANTEIYM)
                          MOVE  1                          TO  IDX-ST
                          MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                      END-IF
                      IF  (PTNYUINRRK-TENNYUYMD(1:6)
                                          =  LNK-NYUINDAY-SANTEIYM) AND
                          (PTNYUINRRK-TENSTUYMD(1:6)
                                          >  LNK-NYUINDAY-SANTEIYM)
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                          MOVE  99                         TO  IDX-ED
                      END-IF
                      IF      PTNYUINRRK-TENSTUYMD(1:6) = "999999"
                           MOVE   99     TO    IDX-ED
                      END-IF
                      PERFORM    VARYING   IDX   FROM   1   BY   1
                               UNTIL     IDX   >   WRK-GETSUMATSU-DD
                         IF    (IDX-ST    <=    IDX)   AND
                               (IDX-ED    >=    IDX)
                            MOVE  "1"  TO  LNK-NYUINDAY-TANKI-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI2-KBN(IDX)
                            MOVE  "2"  TO  LNK-NYUINDAY-TANKI3-KBN(IDX)
                         END-IF
                      END-PERFORM
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key49"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
             END-IF
      *      ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
             MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
             MOVE    "key49"                 TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
            END-PERFORM
      *
           END-IF
      *
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²¡ÊÀ¸³èÎÅÍÜ´Þ¤à¡ËËô¤ÏÃ»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³¡ÊÀ¸³èÎÅÍÜ´Þ¤à¡Ë
      *    ¤Ë³ºÅö¤¹¤ëÆþ±¡ÎÁ¤òÈ½Äê¤·¡¢Æþ±¡²ñ·×¤Ç»»ÄêºÑ¤ß¤Î¾ì¹ç¤ÏÊÖµÑÃÍ¤ò¥¯¥ê¥¢¤¹¤ë
           INITIALIZE                          NYUINACCT-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID       TO  NACCT-PTID
           MOVE    LNK-NYUINDAY-SANTEIYM   TO  NACCT-SRYYM
           MOVE    "1"                     TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT           =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM   910-NYUINACCT-READ-SEC
               PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
                   IF    NACCT-SRYCDTOTAL     =    99999916
                       CONTINUE
                   ELSE
                       PERFORM    210-NYUINACT-GET-SEC
                       IF  NSRY-SRYCD(1)  = CONS-TANKI2     OR
                                            CONS-TANKI2-SEIKATSU OR
                                            CONS-TANKI3     OR
                                            CONS-TANKI3-SEIKATSU OR
                                            CONS-TANKI3-H26-SRYCD(1) OR
                                            CONS-TANKI3-H26-SRYCD(2) OR
                                            CONS-TANKI3-H26-SRYCD(3) OR
                                            CONS-TANKI3-H26-SRYCD(4) OR
                                            CONS-TANKI3-H26-SRYCD(5) OR
                                            CONS-TANKI3-H26-SRYCD(6) OR
                                            CONS-TANKI3-H26-SRYCD(7) OR
                                            CONS-TANKI3-H26-SRYCD(8) OR
                                            CONS-TANKI3-H26-SRYCD(9) OR
                                            CONS-TANKI3-H26-SRYCD(10) OR
                                            CONS-TANKI3-H26-SRYCD(11) OR
                                            CONS-TANKI3-H26-SRYCD(12) OR
                                            CONS-TANKI3-H26-SRYCD(13) OR
                                            CONS-TANKI3-H26-SRYCD(14) OR
                                            CONS-TANKI3-H26-SRYCD(15) OR
                                            CONS-TANKI3-H26-SRYCD(16) OR
                                            CONS-TANKI3-H26-SRYCD(17) OR
                                            CONS-TANKI3-H26-SRYCD(18) OR
                                            CONS-TANKI3-H26-SRYCD(19) OR
                                            CONS-TANKI3-H26-SRYCD(20) OR
                                            CONS-TANKI3-H26-SRYCD(21) OR
                                            CONS-TANKI3-H26-SRYCD(22) OR
                                            CONS-TANKI3-H26-SRYCD(23) OR
                                            CONS-TANKI3-H26-SRYCD(24) OR
                                            CONS-TANKI3-H26-SRYCD(25) OR
                                            CONS-TANKI3-H26-SRYCD(26) OR
                                            CONS-TANKI3-H26-SRYCD(27) OR
                                            CONS-TANKI3-H26-SRYCD(28) OR
                                            CONS-TANKI3-H26-SRYCD(29) OR
                                            CONS-TANKI3-H26-SRYCD(30) OR
                                            CONS-TANKI3-H26-SRYCD(31) OR
                                            CONS-TANKI3-H26-SRYCD(32) OR
                                            CONS-TANKI3-H26-SRYCD(33) OR
                                            CONS-TANKI3-H26-SRYCD(34) OR
                                            CONS-TANKI3-H26-SRYCD(35) OR
                                            CONS-TANKI3-H26-SRYCD(36) OR
                                            CONS-TANKI3-H26-SRYCD(37) OR
                                            CONS-TANKI3-H26-SRYCD(38) OR
                                            CONS-TANKI3-H26-SRYCD(39) OR
                                            CONS-TANKI3-H26-SRYCD(40) OR
                                            CONS-TANKI3-H26-SRYCD(41) OR
                                            CONS-TANKI3-H26-SRYCD(42) OR
                                            CONS-TANKI3-H26-SRYCD(43) OR
                                            CONS-TANKI3-H26-SRYCD(44) OR
                                            CONS-TANKI3-H26-SRYCD(45) OR
                                            CONS-TANKI3-H26-SRYCD(46) OR
                                            CONS-TANKI3-H26-SRYCD(47) OR
                                            CONS-TANKI3-H26-SRYCD(48) OR
                                            CONS-TANKI3-H28-SRYCD(1)  OR
                                            CONS-TANKI3-H28-SRYCD(2)  OR
                                            CONS-TANKI3-H28-SRYCD(3)  OR
                                            CONS-TANKI3-H28-SRYCD(4)  OR
                                            CONS-TANKI3-H28-SRYCD(5)  OR
                                            CONS-TANKI3-H28-SRYCD(6)  OR
                                            CONS-TANKI3-H28-SRYCD(7)  OR
                                            CONS-TANKI3-H28-SRYCD(8)  OR
                                            CONS-TANKI3-H28-SRYCD(9)  OR
                                            CONS-TANKI3-H28-SRYCD(10) OR
                                            CONS-TANKI3-H28-SRYCD(11) OR
                                            CONS-TANKI3-H28-SRYCD(12) OR
                                            CONS-TANKI3-H28-SRYCD(13) OR
                                            CONS-TANKI3-H28-SRYCD(14) OR
                                            CONS-TANKI3-H28-SRYCD(15) OR
                                            CONS-TANKI3-H28-SRYCD(16) OR
                                            CONS-TANKI3-H28-SRYCD(17) OR
                                            CONS-TANKI3-H28-SRYCD(18) OR
                                            CONS-TANKI3-H28-SRYCD(19) OR
                                            CONS-TANKI3-H28-SRYCD(20) OR
                                            CONS-TANKI3-H28-SRYCD(21) OR
                                            CONS-TANKI3-H28-SRYCD(22) OR
                                            CONS-TANKI3-R02-SRYCD(1) OR
                                            CONS-TANKI3-R02-SRYCD(2) OR
                                            CONS-TANKI3-R02-SRYCD(3) OR
                                            CONS-TANKI3-R02-SRYCD(4) OR
                                            CONS-TANKI3-R04-SRYCD(1) OR
                                            CONS-TANKI3-R04-SRYCD(2) OR
                                            CONS-TANKI3-R04-SRYCD(3) OR
                                            CONS-TANKI3-R04-SRYCD(4) OR
                                            CONS-TANKI3-R04-SRYCD(5) OR
                                            CONS-TANKI3-R04-SRYCD(6) OR
                                            CONS-TANKI3-R04-SRYCD(7) OR
                                            CONS-TANKI3-R04-SRYCD(8) OR
                                            CONS-TANKI3-R04-SRYCD(9) OR
                                            CONS-TANKI3-R04-SRYCD(10) OR
                                            CONS-TANKI3-R04-SRYCD(11) OR
                                            CONS-TANKI3-R04-SRYCD(12) OR
                                            CONS-TANKI3-R04-SRYCD(13) OR
                                            CONS-TANKI3-R04-SRYCD(14) OR
                                            CONS-TANKI3-R04-SRYCD(15) OR
                                            CONS-TANKI3-R04-SRYCD(16) OR
                                            CONS-TANKI3-R04-SRYCD(17) OR
                                            CONS-TANKI3-R04-SRYCD(18) OR
                                            CONS-TANKI3-R04-SRYCD(19) OR
                                            CONS-TANKI3-R04-SRYCD(20) OR
                                            CONS-TANKI3-R04-SRYCD(21) OR
                                            CONS-TANKI3-R04-SRYCD(22) OR
                                            CONS-TANKI3-R04-SRYCD(23) OR
                                            CONS-TANKI3-R04-SRYCD(24) OR
                                            CONS-TANKI3-R04-SRYCD(25) OR
                                            CONS-TANKI3-R04-SRYCD(26) OR
                                            CONS-TANKI3-R04-SRYCD(27) OR
                                            CONS-TANKI3-R04-SRYCD(28) OR
                                            CONS-TANKI3-R04-SRYCD(29) OR
                                            CONS-TANKI3-R04-SRYCD(30) OR
                                            CONS-TANKI3-R04-SRYCD(31) OR
                                            CONS-TANKI3-R04-SRYCD(32) OR
                                            CONS-TANKI3-R04-SRYCD(33) OR
                                            CONS-TANKI3-R04-SRYCD(34) OR
                                            CONS-TANKI3-R04-SRYCD(35) OR
                                            CONS-TANKI3-R04-SRYCD(36) OR
                                            CONS-TANKI3-R04-SRYCD(37) OR
                                            CONS-TANKI3-R04-SRYCD(38) OR
                                            CONS-TANKI3-R04-SRYCD(39) OR
                                            CONS-TANKI3-R04-SRYCD(40) OR
                                            CONS-TANKI3-R04-SRYCD(41) OR
                                            CONS-TANKI3-R04-SRYCD(42) OR
                                            CONS-TANKI3-R04-SRYCD(43) OR
                                            CONS-TANKI3-R04-SRYCD(44) OR
                                            CONS-TANKI3-R04-SRYCD(45) OR
                                            CONS-TANKI3-R04-SRYCD(46) OR
                                            CONS-TANKI3-R04-SRYCD(47) OR
                                            CONS-TANKI3-R04-SRYCD(48) OR
                                            CONS-TANKI3-R04-SRYCD(49) OR
                                            CONS-TANKI3-R04-SRYCD(50) OR
                                            CONS-TANKI3-R04-SRYCD(51) OR
                                            CONS-TANKI3-R04-SRYCD(52) OR
                                            CONS-TANKI3-R04-SRYCD(53) OR
                                            CONS-TANKI3-R04-SRYCD(54) OR
                                            CONS-TANKI3-R04-SRYCD(55) OR
                                            CONS-TANKI3-R04-SRYCD(56) OR
                                            CONS-TANKI3-R04-SRYCD(57) OR
                                            CONS-TANKI3-R04-SRYCD(58) OR
                                            CONS-TANKI3-R04-SRYCD(59) OR
                                            CONS-TANKI3-R04-SRYCD(60) OR
                                            CONS-TANKI3-R04-SRYCD(61) OR
                                            CONS-TANKI3-R04-SRYCD(62) OR
                                            CONS-TANKI3-R04-SRYCD(63) OR
                                            CONS-TANKI3-R04-SRYCD(64) OR
                                            CONS-TANKI3-R04-SRYCD(65) OR
                                            CONS-TANKI3-R04-SRYCD(66) OR
                                            CONS-TANKI3-R04-SRYCD(67) OR
                                            CONS-TANKI3-R04-SRYCD(68) OR
                                            CONS-TANKI3-R04-SRYCD(69) OR
                                            CONS-TANKI3-R04-SRYCD(70) OR
                                            CONS-TANKI3-R04-SRYCD(71) OR
                                            CONS-TANKI3-R04-SRYCD(72) OR
                                            CONS-TANKI3-R04-SRYCD(73) OR
                                            CONS-TANKI3-R04-SRYCD(74) OR
                                            CONS-TANKI3-R04-SRYCD(75) OR
                                            CONS-TANKI3-R04-SRYCD(76) OR
                                            CONS-TANKI3-R06-SRYCD(1) OR
                                            CONS-TANKI3-R06-SRYCD(2) OR
                                            CONS-TANKI3-R06-SRYCD(3) OR
                                            CONS-TANKI3-R06-SRYCD(4) OR
                                            CONS-TANKI3-R06-SRYCD(5) OR
                                            CONS-TANKI3-R06-SRYCD(6) OR
                                            CONS-TANKI3-R06-SRYCD(7) OR
                                            CONS-TANKI3-R06-SRYCD(8) OR
                                            CONS-TANKI3-R06-SRYCD(9) OR
                                            CONS-TANKI3-R06-SRYCD(10) OR
                                            CONS-TANKI3-R06-SRYCD(11) OR
                                            CONS-TANKI3-R06-SRYCD(12) OR
                                            CONS-TANKI3-R06-SRYCD(13) OR
                                            CONS-TANKI3-R06-SRYCD(14) OR
                                            CONS-TANKI3-R06-SRYCD(15) OR
                                            CONS-TANKI3-R06-SRYCD(16) OR
                                            CONS-TANKI3-R06-SRYCD(17) OR
                                            CONS-TANKI3-R06-SRYCD(18) OR
                                            CONS-TANKI3-R06-SRYCD(19) OR
                                            CONS-TANKI3-R06-SRYCD(20) OR
                                            CONS-TANKI3-R06-SRYCD(21) OR
                                            CONS-TANKI3-R06-SRYCD(22) OR
                                            CONS-TANKI3-R06-SRYCD(23) OR
                                            CONS-TANKI3-R06-SRYCD(24) OR
                                            CONS-TANKI3-R06-SRYCD(25) OR
                                            CONS-TANKI3-R06-SRYCD(26) OR
                                            CONS-TANKI3-R06-SRYCD(27) OR
                                            CONS-TANKI3-R06-SRYCD(28) OR
                                            CONS-TANKI3-R06-SRYCD(29) OR
                                            CONS-TANKI3-R06-SRYCD(30) OR
                                            CONS-TANKI3-R06-SRYCD(31) OR
                                            CONS-TANKI3-R06-SRYCD(32) OR
                                            CONS-TANKI3-R06-SRYCD(33) OR
                                            CONS-TANKI3-R06-SRYCD(34) OR
                                            CONS-TANKI3-R06-SRYCD(35) OR
                                            CONS-TANKI3-R06-SRYCD(36)
                          PERFORM    VARYING   IDX   FROM   1   BY   1
                                  UNTIL     IDX   >   WRK-GETSUMATSU-DD
                            IF     NACCT-DAY(IDX)  NOT =  ZERO
                               MOVE  SPACE TO LNK-NYUINDAY-TANKI2-KBN
                                                                 (IDX)
                            END-IF
                          END-PERFORM
                       END-IF
                   END-IF
                   MOVE    "tbl_nyuinacct"      TO  MCP-TABLE
                   MOVE    "key17"              TO  MCP-PATHNAME
                   PERFORM 910-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    Æþ±¡²ñ·×¥¯¥í¡¼¥º
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡¶èÊ¬¤Î¥»¥Ã¥È
           PERFORM    VARYING   IDX   FROM   1   BY   1
                 UNTIL     IDX   >   31
               IF   LNK-NYUINDAY-TANKI2-KBN(IDX)  NOT =   SPACE
                   MOVE   "1"     TO   LNK-NYUINDAY-KBN(IDX)
               END-IF
           END-PERFORM
      *
      *    Æþ±¡¶èÊ¬¤Î¥¯¥ê¥¢
           PERFORM    VARYING   IDX   FROM   1   BY   1
                 UNTIL     IDX   >   31
               IF   WRK-TANKI-IGAI(IDX)  NOT =   SPACE
                   MOVE   SPACE     TO   LNK-NYUINDAY-KBN(IDX)
                   MOVE   SPACE     TO   LNK-NYUINDAY-TANKI-KBN(IDX)
                   MOVE   SPACE     TO   LNK-NYUINDAY-TANKI2-KBN(IDX)
                   MOVE   SPACE     TO   LNK-NYUINDAY-TANKI3-KBN(IDX)
               END-IF
           END-PERFORM
      *
      *    Æþ±¡Ãæ¤ÎµßµÞÈÂÁ÷¿ÇÎÅÎÁ¡ÊÆþ±¡¤Î¿ÇÎÅ¹Ô°Ù¡Ë»»ÄêÆü¤ò
      *    ¿©»ö»»Äê²ÄÇ½¤È¤¹¤ë
      *    INITIALIZE                          SRYACT-REC
      *    MOVE    LNK-NYUINDAY-HOSPNUM    TO  SRY-HOSPNUM
      *    MOVE    "1"                     TO  SRY-NYUGAIKBN
      *    MOVE    LNK-NYUINDAY-PTID       TO  SRY-PTID
      *    MOVE    LNK-NYUINDAY-SANTEIYM   TO  SRY-SRYYM
      *    MOVE    "114003010"             TO  SRY-SRYCD(1)
      *                                        SRY-SRYCD(2)
      *                                        SRY-SRYCD(3)
      *                                        SRY-SRYCD(4)
      *                                        SRY-SRYCD(5)
      *    MOVE    "tbl_sryact"            TO  MCP-TABLE
      *    MOVE    "key7"                  TO  MCP-PATHNAME
      *    PERFORM 910-SRYACT-SELECT-SEC
      *    IF      FLG-SRYACT             =   ZERO
      *        MOVE    "tbl_sryact"        TO  MCP-TABLE
      *        MOVE    "key7"              TO  MCP-PATHNAME
      *        PERFORM   910-SRYACT-READ-SEC
      *        PERFORM   UNTIL   (FLG-SRYACT  =   1)
      *            INITIALIZE                  SRYACCT-REC
      *            MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
      *            MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
      *            MOVE    SRY-PTID        TO  ACCT-PTID
      *            MOVE    SRY-SRYKA       TO  ACCT-SRYKA
      *            MOVE    SRY-SRYYM       TO  ACCT-SRYYM
      *            MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
      *            MOVE    "tbl_sryacct"   TO  MCP-TABLE
      *            MOVE    "key5"          TO  MCP-PATHNAME
      *            PERFORM   910-SRYACCT-SELECT-SEC
      *            MOVE    "tbl_sryacct"   TO  MCP-TABLE
      *            MOVE    "key5"          TO  MCP-PATHNAME
      *            PERFORM   910-SRYACCT-READ-SEC
      *            IF    FLG-SRYACCT       =   0
      *                PERFORM    VARYING   IDX   FROM   1   BY   1
      *                  UNTIL     IDX   >   31
      *                    IF   ACCT-DAY(1 IDX)   NOT =   ZERO
      *                      MOVE   "1"    TO   LNK-NYUINDAY-KBN(IDX)
      *                                         LNK-NYUINDAY-KYUKYU-KBN
      *                                                           (IDX)
      *                    END-IF
      *                END-PERFORM
      *            END-IF
      *
      *            ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
      *            MOVE    "tbl_sryacct"   TO  MCP-TABLE
      *            MOVE    "key5"          TO  MCP-PATHNAME
      *            PERFORM   900-CLOSE-SEC
      *
      *            MOVE    "tbl_sryact"    TO  MCP-TABLE
      *            MOVE    "key7"          TO  MCP-PATHNAME
      *            PERFORM 910-SRYACT-READ-SEC
      *        END-PERFORM
      *    END-IF
      *
      *    ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
      *    MOVE    "tbl_sryact"            TO  MCP-TABLE
      *    MOVE    "key7"                  TO  MCP-PATHNAME
      *    PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡Ãæ¤ÎµßµÞÈÂÁ÷¿ÇÎÅÎÁ¡Ê³°Íè¤Î¿ÇÎÅ¹Ô°Ù¡Ë»»ÄêÆü¤ò
      *    ¿©»ö»»Äê²ÄÇ½¤È¤¹¤ë
           INITIALIZE                          SRYACT-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    "2"                     TO  SRY-NYUGAIKBN
           MOVE    LNK-NYUINDAY-PTID       TO  SRY-PTID
           MOVE    LNK-NYUINDAY-SANTEIYM   TO  SRY-SRYYM
           MOVE    "114003010"             TO  SRY-SRYCD(1)
                                               SRY-SRYCD(2)
                                               SRY-SRYCD(3)
                                               SRY-SRYCD(4)
                                               SRY-SRYCD(5)
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM 910-SRYACT-SELECT-SEC
           IF      FLG-SRYACT             =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM   910-SRYACT-READ-SEC
               PERFORM   UNTIL   (FLG-SRYACT  =   1)
                   INITIALIZE                  SRYACCT-REC
                   MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
                   MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                   MOVE    SRY-PTID        TO  ACCT-PTID
                   MOVE    SRY-SRYKA       TO  ACCT-SRYKA
                   MOVE    SRY-SRYYM       TO  ACCT-SRYYM
                   MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-SELECT-SEC
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   910-SRYACCT-READ-SEC
                   IF    FLG-SRYACCT       =   0
                       PERFORM    VARYING   IDX   FROM   1   BY   1
                         UNTIL     IDX   >   31
                           IF   ACCT-DAY(1 IDX)   NOT =   ZERO
                             MOVE   "1"    TO   LNK-NYUINDAY-KBN(IDX)
                                                LNK-NYUINDAY-KYUKYU-KBN
                                                                  (IDX)
                           END-IF
                       END-PERFORM
                   END-IF
      *
      *            ¿ÇÎÅ²ñ·×¥¯¥í¡¼¥º
                   MOVE    "tbl_sryacct"   TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
      *
                   MOVE    "tbl_sryact"    TO  MCP-TABLE
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 910-SRYACT-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ¿ÇÎÅ¹Ô°Ù¥¯¥í¡¼¥º
           MOVE    "tbl_sryact"            TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    Æþ±¡ÍúÎò¤è¤êÆ±ÆüºÆÆþ±¡¤òÆÃÄê
           INITIALIZE                     PTNYUINRRK-REC
           MOVE    LNK-NYUINDAY-HOSPNUM   TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID      TO  PTNYUINRRK-PTID
           MOVE    "tbl_ptnyuinrrk"       TO  MCP-TABLE
           MOVE    "key33"                TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK         =   ZERO
               MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
               MOVE    "key33"            TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL   (FLG-PTNYUINRRK   =   1)
                 IF (PTNYUINRRK-TENNYUYMD(1:6) = LNK-NYUINDAY-SANTEIYM)
                      MOVE     ZERO       TO     IDX-ST
                      IF  PTNYUINRRK-DOUJITSUKBN   =   "1"
      *
                          MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
      *
                          MOVE  "1"       TO  LNK-NYUINDAY-DOUJITSU-KBN
                                                               (IDX-ST)
      *
      *                   Æ±ÆüºÆÆþ±¡Æü¡ÊÆþ±¡Ê¬¡Ë¤Î¿©»ö³«»Ï»þ´Ö¶èÊ¬
                          MOVE  PTNYUINRRK-TIMEKBN-IN
                                     TO  LNK-NYUINDAY-DOUJITSU-SKJKBN
                                                               (IDX-ST)
      *
      *                   Æ±ÆüºÆÆþ±¡Æü¡ÊÂà±¡Ê¬¡Ë¤Î¿©»ö½ªÎ»»þ´Ö¶èÊ¬
                          EVALUATE   PTNYUINRRK-TIMEKBN-IN
                              WHEN   "0"
                                  MOVE  "3"   TO
                                         LNK-NYUINDAY-DOUJITSU-T-SKJKBN
                                                               (IDX-ST)
                              WHEN   "1"
                                  MOVE  "0"   TO
                                         LNK-NYUINDAY-DOUJITSU-T-SKJKBN
                                                               (IDX-ST)
                              WHEN   "2"
                                  MOVE  "1"   TO
                                         LNK-NYUINDAY-DOUJITSU-T-SKJKBN
                                                               (IDX-ST)
                              WHEN   "3"
                                  MOVE  "2"   TO
                                         LNK-NYUINDAY-DOUJITSU-T-SKJKBN
                                                               (IDX-ST)
                          END-EVALUATE
      *
      *
      *                   Æ±ÆüºÆÆþ±¡Æü¤ÎÊÝ¸±ÁÈ¹ç¤»¤òÆþ±¡²ñ·×¤«¤é
      *                   ºÆ¼èÆÀ¤¹¤ë
                          MOVE    PTNYUINRRK-TENNYUYMD
                                           TO  WRK-DOUJITSU-YMD
                          INITIALIZE           NYUINACCT-REC
                           MOVE    LNK-NYUINDAY-HOSPNUM
                                           TO  NACCT-HOSPNUM
                           MOVE    LNK-NYUINDAY-PTID
                                           TO  NACCT-PTID
                           MOVE    WRK-DOUJITSU-YYMM
                                           TO  NACCT-SRYYM
                           MOVE    "5"     TO  NACCT-ZAISKBKBN
                           MOVE    "tbl_nyuinacct"  TO  MCP-TABLE
                           MOVE    "key17"          TO  MCP-PATHNAME
                           PERFORM 910-NYUINACCT-SELECT-SEC
                           IF      FLG-NYUINACCT   =   ZERO
                               MOVE    "tbl_nyuinacct"
                                                    TO  MCP-TABLE
                               MOVE    "key17"      TO  MCP-PATHNAME
                               PERFORM 910-NYUINACCT-READ-SEC
                               IF   FLG-NYUINACCT    =   ZERO
                                     MOVE   NACCT-DAY (WRK-DOUJITSU-DD)
                                                 TO  
                                        LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI
                                                           (IDX-ST)
                               END-IF
                           END-IF
      *
      *                    Æþ±¡²ñ·×¥¯¥í¡¼¥º
                           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                           MOVE    "key17"            TO  MCP-PATHNAME
                           PERFORM   900-CLOSE-SEC
      *
      *                    Æ±ÆüÂà±¡Ê¬¤ÎÊÝ¸±ÁÈ¹ç¤»ÊÝ¸±¼ïÎà¤ò¼èÆÀ¤¹¤ë
      *                    ¡Ê£±¡§¼ÒÊÝ¡¡£²¡§¹ñÊÝ¡¡£³¡§¸å´ü¹âÎð¡¡£´¡§Ï«ºÒ
      *                      £µ¡§¼«Çå¡¡£¶¡§¸ø³²¡¡£·¡§¼«Èñ¡¡£¸¡§¼£¸³¡Ë
                           IF  LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI (IDX-ST)
                                                      NOT = ZERO
                               PERFORM   210-HKNSYURUI-GET1-SEC
                           END-IF
      *
      *                   Æ±ÆüºÆÆþ±¡Æü¡ÊÆþ±¡Ê¬¡Ë¤ÎÊÝ¸±ÁÈ¹ç¤»ÈÖ¹æ
                          MOVE  PTNYUINRRK-HKNCOMBINUM
                                     TO  LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                               (IDX-ST)
      *
      *                   Æ±ÆüºÆÆþ±¡ÆüÍâÆü¤ÎÊÝ¸±ÁÈ¹ç¤»¤òÆþ±¡²ñ·×¤«¤é
      *                   ºÆ¼èÆÀ¤¹¤ë
                          MOVE    ZERO    TO  WRK-YOKUJITSU-YMD
                          INITIALIZE          LNK-DAY6-AREA
                          MOVE    "61"    TO  LNK-DAY6-IRAI
                          MOVE    PTNYUINRRK-TENNYUYMD
                                          TO  LNK-DAY6-KIJUN
                          MOVE    "1"     TO  LNK-DAY6-ZOGENPTN
                          MOVE    1       TO  LNK-DAY6-ZOGEN
                          CALL    "ORCSDAY"   USING     STS-AREA-DAY
                                                        LNK-DAY6-AREA
                          MOVE    LNK-DAY6-KEISAN
                                          TO  WRK-YOKUJITSU-YMD
      *                   Æþ±¡²ñ·×¡ÊÊÝ¸±ÁÈ¹ç¤»¤ÎºÞ¤ò¸¡º÷¡Ë
                          INITIALIZE           NYUINACCT-REC
                           MOVE    LNK-NYUINDAY-HOSPNUM
                                           TO  NACCT-HOSPNUM
                           MOVE    LNK-NYUINDAY-PTID
                                           TO  NACCT-PTID
                           MOVE    WRK-YOKUJITSU-YYMM
                                           TO  NACCT-SRYYM
                           MOVE    "5"     TO  NACCT-ZAISKBKBN
                           MOVE    "tbl_nyuinacct"  TO  MCP-TABLE
                           MOVE    "key17"          TO  MCP-PATHNAME
                           PERFORM 910-NYUINACCT-SELECT-SEC
                           IF      FLG-NYUINACCT   =   ZERO
                               MOVE    "tbl_nyuinacct"
                                                    TO  MCP-TABLE
                               MOVE    "key17"      TO  MCP-PATHNAME
                               PERFORM 910-NYUINACCT-READ-SEC
                               IF   FLG-NYUINACCT    =   ZERO
                                 IF  NACCT-DAY(WRK-YOKUJITSU-DD)
                                                 NOT = ZERO
                                   MOVE   NACCT-DAY(WRK-YOKUJITSU-DD)
                                                    TO
                                       LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                             (IDX-ST)
                                 END-IF                        
                               END-IF
                           END-IF
      *
      *                    Æþ±¡²ñ·×¥¯¥í¡¼¥º
                           MOVE    "tbl_nyuinacct"    TO  MCP-TABLE
                           MOVE    "key17"            TO  MCP-PATHNAME
                           PERFORM   900-CLOSE-SEC
      *
      *                    Æ±ÆüºÆÆþ±¡Ê¬¤ÎÊÝ¸±ÁÈ¹ç¤»ÊÝ¸±¼ïÎà¤ò¼èÆÀ¤¹¤ë
      *                    ¡Ê£±¡§¼ÒÊÝ¡¡£²¡§¹ñÊÝ¡¡£³¡§¸å´ü¹âÎð¡¡£´¡§Ï«ºÒ
      *                      £µ¡§¼«Çå¡¡£¶¡§¸ø³²¡¡£·¡§¼«Èñ¡¡£¸¡§¼£¸³¡Ë
                           IF  LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX-ST)
                                                      NOT = ZERO
                               PERFORM   210-HKNSYURUI-GET2-SEC
                           END-IF
      *
      *                    ÊÝ¸±ÊÑÆ°¶èÊ¬¤ò¥»¥Ã¥È
                           IF  (LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                                                              =  "1" OR
                                                                 "2" OR
                                                                 "3")
                                               AND
                               (LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                                                               = "1" OR
                                                                 "2" OR
                                                                 "3")
                              MOVE  "1"   TO
                                 LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX-ST)
                           END-IF
                           IF  (LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                                                              =  "1" OR
                                                                 "2" OR
                                                                 "3")
                                               AND
                               (LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                                                               = "4" OR
                                                                 "5" OR
                                                                 "6" OR
                                                                 "7" OR
                                                                 "8")
                              MOVE  "2"   TO
                                 LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX-ST)
                           END-IF
                           IF  (LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                                                              =  "4" OR
                                                                 "5" OR
                                                                 "6" OR
                                                                 "7" OR
                                                                 "8")
                                               AND
                               (LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                                                               = "1" OR
                                                                 "2" OR
                                                                 "3")
                              MOVE  "3"   TO
                                 LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX-ST)
                           END-IF
                           IF  (LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                                                              =  "4" OR
                                                                 "5" OR
                                                                 "6" OR
                                                                 "7" OR
                                                                 "8")
                                               AND
                               (LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                                                               = "4" OR
                                                                 "5" OR
                                                                 "6" OR
                                                                 "7" OR
                                                                 "8")
                              MOVE  "4"   TO
                                 LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX-ST)
                           END-IF
      *
                      END-IF
                 END-IF
                 MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                 MOVE    "key33"              TO  MCP-PATHNAME
                 PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *
      *    ´µ¼ÔÆþ±¡ÍúÎò¥¯¥í¡¼¥º
           MOVE    "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE    "key33"                 TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      **     PERFORM   VARYING  IDX    FROM  1  BY  1 
      **       UNTIL   IDX   >   31
      **         DISPLAY "IDX = " IDX
      **         DISPLAY "LNK-NYUINDAY-KBN = " LNK-NYUINDAY-KBN(IDX)
      **         DISPLAY "LNK-NYUINDAY-TANKI2-KBN = "
      **                  LNK-NYUINDAY-TANKI2-KBN(IDX)
      **       DISPLAY "DOUJITSU = " LNK-NYUINDAY-DOUJITSU-KBN(IDX)
      **       DISPLAY "HKNHEN   = " LNK-NYUINDAY-DOUJITSU-HKNHENKBN(IDX)
      **       DISPLAY "TAI-SKJ  = " LNK-NYUINDAY-DOUJITSU-T-SKJKBN(IDX)
      **       DISPLAY "TAI-HKN  = " LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI(IDX)
      **       DISPLAY "TAI-HKNSYU=" LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX)
      **       DISPLAY "NYU-SKJ  = " LNK-NYUINDAY-DOUJITSU-SKJKBN(IDX)
      **       DISPLAY "NYU-HKN  = " LNK-NYUINDAY-DOUJITSU-HKNCOMBI(IDX)
      **       DISPLAY "NYU-HKNSYU=" LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX)
      **     END-PERFORM
      **     DISPLAY   "WRK-YOKUJITSU-YMD = " WRK-YOKUJITSU-YMD
      *
      *     IF       LNK-NYUINDAY-TBL   =    SPACE
      *         MOVE     4              TO   LNK-NYUINDAY-RC
      *     END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¼èÆÀ
      *****************************************************************
       210-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    Æþ±¡¹Ô°Ù¥¯¥í¡¼¥º
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       210-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æ±ÆüºÆÆþ±¡¡ÊÂà±¡Ê¬¡Ë¤ÎÊÝ¸±¼ïÎà¼èÆÀ
      *****************************************************************
       210-HKNSYURUI-GET1-SEC            SECTION.
      *
           MOVE    "1"      TO   LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID       TO  COMB-PTID
           MOVE    LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI (IDX-ST)
                                           TO  COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
               IF   FLG-HKNCOMBI           =   ZERO
      *            ¹ñÊÝ
                   IF   COMB-HKNNUM        =   "060"  OR
                                               "067"  OR
                                               "068"  OR
                                               "069"
                       MOVE    "2"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            ¸å´ü¹âÎð¼Ô
                   IF   COMB-HKNNUM        =   "039"  OR
                                               "040"
                       MOVE    "3"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            Ï«ºÒÊÝ¸±
                   IF   COMB-HKNNUM        =   "971"
                       MOVE    "4"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            ¼«ÇåÀÕÊÝ¸±
                   IF   COMB-HKNNUM        =   "973"
                       MOVE    "5"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            ¸ø³²ÊÝ¸±
                   IF   COMB-HKNNUM        =   "975"
                       MOVE    "6"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            ¼«Èñ
                   IF   COMB-HKNNUM(1:2)   =   "98"
                       MOVE    "7"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
      *            ¼£¸³
                   IF   COMB-HKNNUM(1:2)   =   "90"  OR
                                               "91"
                       MOVE    "8"         TO
                                 LNK-NYUINDAY-DOUJITSU-T-HKNSYU(IDX-ST)
                   END-IF
               END-IF
      *        ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           ELSE
      *        ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           END-IF
      *
           .
       210-HKNSYURUI-GET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æ±ÆüºÆÆþ±¡¡ÊÆþ±¡Ê¬¡Ë¤ÎÊÝ¸±¼ïÎà¼èÆÀ
      *****************************************************************
       210-HKNSYURUI-GET2-SEC            SECTION.
      *
           MOVE    "1"      TO   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-NYUINDAY-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    LNK-NYUINDAY-PTID       TO  COMB-PTID
           MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX-ST)
                                           TO  COMB-HKNCOMBINUM
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-HKNCOMBI-READ-SEC
               IF   FLG-HKNCOMBI           =   ZERO
      *            ¹ñÊÝ
                   IF   COMB-HKNNUM        =   "060"  OR
                                               "067"  OR
                                               "068"  OR
                                               "069"
                       MOVE    "2"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            ¸å´ü¹âÎð¼Ô
                   IF   COMB-HKNNUM        =   "039"  OR
                                               "040"
                       MOVE    "3"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            Ï«ºÒÊÝ¸±
                   IF   COMB-HKNNUM        =   "971"
                       MOVE    "4"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            ¼«ÇåÀÕÊÝ¸±
                   IF   COMB-HKNNUM        =   "973"
                       MOVE    "5"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            ¸ø³²ÊÝ¸±
                   IF   COMB-HKNNUM        =   "975"
                       MOVE    "6"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            ¼«Èñ
                   IF   COMB-HKNNUM(1:2)   =   "98"
                       MOVE    "7"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
      *            ¼£¸³
                   IF   COMB-HKNNUM(1:2)   =   "90"  OR
                                               "91"
                       MOVE    "8"         TO
                                   LNK-NYUINDAY-DOUJITSU-HKNSYU(IDX-ST)
                   END-IF
               END-IF
      *        ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           ELSE
      *        ÊÝ¸±ÁÈ¹ç¤»¥¯¥í¡¼¥º
               MOVE    "tbl_hkncombi"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
           END-IF
      *
           .
       210-HKNSYURUI-GET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SRYACCT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SRYACCT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  SRYACCT-REC
               MOVE    ZERO               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                     SRYACCT-REC
               MOVE    1                  TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-SRYACT-SELECT-SEC        SECTION.
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SRYACT
           ELSE
               MOVE    1                  TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿ÇÎÅ¹Ô°Ù¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-SRYACT-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       910-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÝ¸±ÁÈ¹ç¤»¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  TNS-TENSU-REC
                   MOVE    ZERO        TO  FLG-TENSU
               ELSE
                   MOVE    1           TO  FLG-TENSU
                   INITIALIZE              TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Ó£Å£Ì£Å£Ã£Ô½èÍý
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â£Æ£Å£Ô£Ã£È½èÍý
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
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
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-EXT.
           EXIT.
