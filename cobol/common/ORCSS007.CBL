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
       PROGRAM-ID.             ORCSS007.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : ¶¦ÄÌ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : Æþ±¡´ðËÜÎÁÅÀ¿ô¥Á¥§¥Ã¥¯
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  07/08/21    NACL¡ÝÂÀÅÄ    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  04.07.00    NACL-¾®Æ¦Âô  12/07/20  À¸³èÎÅÍÜ¤ÎÆþ±¡ÎÁÈ½ÄêÄÉ²Ã
      *  05.00.00    NACL-¾®Æ¦Âô  20/09/17  ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¤Î·Ð²áÁ¼ÃÖ
      *                                     ½ªÎ»¤ËÈ¼¤¦ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ
      *                                     ¤ÎÄÉ²Ã
      *  05.01.00    ORCAMO       22/11/17  À¸³èÎÅÍÜ¤ÎÆþ±¡ÎÁÈ½ÄêÄÉ²Ã
      *  05.02.00    ORCAMO       24/11/12  À¸³èÎÅÍÜ¤ÎÆþ±¡ÎÁÈ½ÄêÄÉ²Ã
      *****************************************************************
      *
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-PTNYUINRRK                  PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-NYUINACT                    PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-UNMATCH                     PIC 9(01).
           03  FLG-RYOYO-CHK                   PIC 9(01).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                             PIC 9(05).
           03  IDX0                            PIC 9(05).
           03  IDX1                            PIC 9(05).
           03  IDX-NSRY                        PIC 9(05).
           03  IDX-ST                          PIC 9(05).
           03  IDX-ED                          PIC 9(05).
           03  IDX-SEI                         PIC 9(05).
           03  IDX-KSN                         PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-ZAITEN                      PIC 9(06).
           03  WRK-SRYYMD                      PIC X(08).
           03  WRK-SRYCD                       PIC X(09).
           03  WRK-HYOKETU-TEN                 PIC 9(06).
      *    ·îËöÆü»»ÄêÎÎ°è
           03  WRK-MATSUYMD.
               05  WRK-MATSUYY                 PIC X(04).
               05  WRK-MATSUMM                 PIC X(02).
               05  WRK-MATSUDD                 PIC X(02).
      *
           03  WRK-RYOYO-TABLE.
               05  WRK-RYOYO-TBL               PIC X(01)  OCCURS 31.
           03  WRK-HKNCOMBI-TABLE.
               05  WRK-HKNCOMBI-TBL            PIC X(04)  OCCURS 31.
           03  WRK-HKNCOMBI-TBL9               PIC 9(04).
           03  WRK-HKNCOMBI-TBLX               REDEFINES
                                               WRK-HKNCOMBI-TBL9
                                               PIC X(04).
           03  WRK-RSI-TABLE.
               05  WRK-RSI-TBL                 PIC X(01)  OCCURS 31.
      *
      *    À¸³èÎÅÍÜÈ½ÄêÍÑ
       01  TBL-H24-SEIKATSU-AREA.
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±
           03   FILLER                     PIC X(09) VALUE "190150110".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190719510".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²
           03   FILLER                     PIC X(09) VALUE "190141610".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190719710".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£³
           03   FILLER                     PIC X(09) VALUE "190141810".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£³¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190719910".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£²
           03   FILLER                     PIC X(09) VALUE "190076810".
      *    Ã»´üÂÚºß¼ê½Ñ´ðËÜÎÁ£³
           03   FILLER                     PIC X(09) VALUE "190130410".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Á
           03   FILLER                     PIC X(09) VALUE "190122010".
      *10  Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Á¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190711410".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Â
           03   FILLER                     PIC X(09) VALUE "190122110".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Â¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190711610".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ã
           03   FILLER                     PIC X(09) VALUE "190122210".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ã¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190711810".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ä
           03   FILLER                     PIC X(09) VALUE "190122310".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Ä¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190712010".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Å
           03   FILLER                     PIC X(09) VALUE "190122410".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ£Å¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190712210".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Á
           03   FILLER                     PIC X(09) VALUE "190155210".
      *20  ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Á¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190155310".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Â
           03   FILLER                     PIC X(09) VALUE "190155610".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Â¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190155710".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ã
           03   FILLER                     PIC X(09) VALUE "190156010".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ã¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156110".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ä
           03   FILLER                     PIC X(09) VALUE "190156410".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Ä¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156510".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Å
           03   FILLER                     PIC X(09) VALUE "190156810".
      *    ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡£Å¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190156910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë
           03   FILLER                     PIC X(09) VALUE "190121310".
      *30  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190700910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë
           03   FILLER                     PIC X(09) VALUE "190121410".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë
           03   FILLER                     PIC X(09) VALUE "190121510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë
           03   FILLER                     PIC X(09) VALUE "190121610".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë
           03   FILLER                     PIC X(09) VALUE "190121710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë
           03   FILLER                     PIC X(09) VALUE "190131610".
      *40  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190701910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë
           03   FILLER                     PIC X(09) VALUE "190131810".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë
           03   FILLER                     PIC X(09) VALUE "190132010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë
           03   FILLER                     PIC X(09) VALUE "190132210".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë
           03   FILLER                     PIC X(09) VALUE "190132410".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë
           03   FILLER                     PIC X(09) VALUE "190132610".
      *50  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190702910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë
           03   FILLER                     PIC X(09) VALUE "190132810".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë
           03   FILLER                     PIC X(09) VALUE "190133010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë
           03   FILLER                     PIC X(09) VALUE "190133210".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133410".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë
           03   FILLER                     PIC X(09) VALUE "190133610".
      *60  ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190703910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë
           03   FILLER                     PIC X(09) VALUE "190133810".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190704110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë
           03   FILLER                     PIC X(09) VALUE "190134010".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190704310".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Á¡Ë
           03   FILLER                     PIC X(09) VALUE "190157210".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Á¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157310".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Â¡Ë
           03   FILLER                     PIC X(09) VALUE "190157610".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Â¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190157710".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ã¡Ë
           03   FILLER                     PIC X(09) VALUE "190158010".
      *70  ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ã¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190158110".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ä¡Ë
           03   FILLER                     PIC X(09) VALUE "190158410".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ä¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190158510".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Å¡Ë
           03   FILLER                     PIC X(09) VALUE "190158810".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Å¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190158910".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Æ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159210".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Æ¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159310".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ç¡Ë
           03   FILLER                     PIC X(09) VALUE "190159610".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£Ç¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190159710".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£È¡Ë
           03   FILLER                     PIC X(09) VALUE "190160010".
      *80  ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£È¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190160110".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£É¡Ë
           03   FILLER                     PIC X(09) VALUE "190160410".
      *    ÆÃÄê°ìÈÌÉÂÅï¡¦ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£±¡Ê£É¡Ë¡Ê±É´É·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190160510".
      *
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190122510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190121810".
      *
       01  TBL-H24-SEIKATSU-R     REDEFINES  TBL-H24-SEIKATSU-AREA.
           03   TBL-H24-SEISRYCD           PIC X(09)  OCCURS  84.
      *
      *
      *    À¸³èÎÅÍÜÈ½ÄêÍÑ¡ÊÊ¿À®£²£¶Ç¯ÄÉ²ÃÊ¬¡Ë
       01  TBL-H26-SEIKATSU-AREA.
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±
           03   FILLER                     PIC X(09) VALUE "190175310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190176110".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²
           03   FILLER                     PIC X(09) VALUE "190175710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190176510".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±
           03   FILLER                     PIC X(09) VALUE "190175510".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190176310".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²
           03   FILLER                     PIC X(09) VALUE "190175910".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190176710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190166910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÌë¶Ð»þ´ÖÄ¶²á¸º»»¡Ë
           03   FILLER                     PIC X(09) VALUE "190167710".
      *
       01  TBL-H26-SEIKATSU-R     REDEFINES  TBL-H26-SEIKATSU-AREA.
           03   TBL-H26-SEISRYCD           PIC X(09)  OCCURS  17.
      *
      *    À¸³èÎÅÍÜÈ½ÄêÍÑ¡ÊÊ¿À®£²£¸Ç¯ÄÉ²ÃÊ¬¡Ë
       01  TBL-H28-SEIKATSU-AREA.
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190185510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Á¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190183710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190185710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Â¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190183910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190185910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ã¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190184110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190186110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ä¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190184310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190186310".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Å¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190184510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190186510".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Æ¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190184710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190186710".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£Ç¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190184910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190186910".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£È¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190185110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡ÊÌë¶Ð»þ´ÖÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190187110".
      *    ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ£²¡ÊÆþ±¡´ðËÜÎÁ£É¡Ë¡Ê´Ç¸î¿¦°÷¿ôÅù·Ð²áÁ¼ÃÖ¡Ë
           03   FILLER                     PIC X(09) VALUE "190185310".
      *
       01  TBL-H28-SEIKATSU-R     REDEFINES  TBL-H28-SEIKATSU-AREA.
           03   TBL-H28-SEISRYCD           PIC X(09)  OCCURS  18.
      *
      *    À¸³èÎÅÍÜÈ½ÄêÍÑ¡ÊÊ¿À®£³£°Ç¯ÄÉ²ÃÊ¬¡Ë
       01  TBL-H30-SEIKATSU-AREA.
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±
           03   FILLER                     PIC X(09) VALUE "190209110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±
           03   FILLER                     PIC X(09) VALUE "190209310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²
           03   FILLER                     PIC X(09) VALUE "190209510".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²
           03   FILLER                     PIC X(09) VALUE "190209710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£³
           03   FILLER                     PIC X(09) VALUE "190209910".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£³
           03   FILLER                     PIC X(09) VALUE "190210110".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´
           03   FILLER                     PIC X(09) VALUE "190210310".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´
           03   FILLER                     PIC X(09) VALUE "190210510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190210710".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190210910".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190211110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190211310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£³¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190211510".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£³¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190211710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190211910".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190212110".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£±
           03   FILLER                     PIC X(09) VALUE "190207910".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£²
           03   FILLER                     PIC X(09) VALUE "190208110".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£³
           03   FILLER                     PIC X(09) VALUE "190208310".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£´
           03   FILLER                     PIC X(09) VALUE "190208510".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£µ
           03   FILLER                     PIC X(09) VALUE "190208710".
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£¶
           03   FILLER                     PIC X(09) VALUE "190208910".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Á¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190215110".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Â¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190215310".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Ã¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190215510".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Ä¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190215710".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Å¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190215910".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Æ¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190216110".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Ç¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190216310".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£È¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190216510".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£É¡Ë¡Ê·Ð²áÁ¼ÃÖ£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190216710".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Á¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190202510".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Â¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190202710".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Ã¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190202910".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Ä¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190203110".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Å¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190203310".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Æ¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190203510".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£Ç¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190203710".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£È¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190203910".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£É¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190204110".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£É¡Ë¡Ê·Ð²áÁ¼ÃÖ£²¡¦ÄÌÂ§£¸¡Ë
           03   FILLER                     PIC X(09) VALUE "190204310".
      *
       01  TBL-H30-SEIKATSU-R     REDEFINES  TBL-H30-SEIKATSU-AREA.
           03   TBL-H30-SEISRYCD           PIC X(09)  OCCURS  41.
      *
      *    À¸³èÎÅÍÜÈ½ÄêÍÑ¡ÊÎáÏÂ£²Ç¯£±£°·îÄÉ²ÃÊ¬¡Ë
       01  TBL-R02-SEIKATSU-AREA.
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ£±
           03   FILLER                     PIC X(09) VALUE "190209310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ£²
           03   FILLER                     PIC X(09) VALUE "190231310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ£³
           03   FILLER                     PIC X(09) VALUE "190231510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ£´
           03   FILLER                     PIC X(09) VALUE "190231710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ£±¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190231910".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ£²¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190232110".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ£³¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190232310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆÃÊÌÆþ±¡ÎÁ£´¡ÊÆÃÄêÃÏ°è¡Ë
           03   FILLER                     PIC X(09) VALUE "190232510".
      *
       01  TBL-R02-SEIKATSU-R     REDEFINES  TBL-R02-SEIKATSU-AREA.
           03   TBL-R02-SEISRYCD           PIC X(09)  OCCURS  8.
      *
      *    À¸³èÎÅÍÜÈ½ÄêÍÑ¡ÊÎáÏÂ£´Ç¯£´·îÄÉ²ÃÊ¬¡Ë
       01  TBL-R04-SEIKATSU-AREA.
      *    ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ£µ
           03   FILLER                     PIC X(09) VALUE "190249510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÎÅÍÜÉÂ¾²¡Ë
           03   FILLER                     PIC X(09) VALUE "190249710".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÎÅÍÜÉÂ¾²¡Ë
           03   FILLER                     PIC X(09) VALUE "190250110".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡Ë
           03   FILLER                     PIC X(09) VALUE "190250510".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡Ë
           03   FILLER                     PIC X(09) VALUE "190252110".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£³¡ÊÎÅÍÜÉÂ¾²¡Ë
           03   FILLER                     PIC X(09) VALUE "190252910".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£³¡ÊÎÅÍÜÉÂ¾²¡Ë
           03   FILLER                     PIC X(09) VALUE "190253310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡Ë
           03   FILLER                     PIC X(09) VALUE "190253710".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡Ë
           03   FILLER                     PIC X(09) VALUE "190255310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÃí£¹¡Ë
           03   FILLER                     PIC X(09) VALUE "190256110".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÃí£¹¡Ë
           03   FILLER                     PIC X(09) VALUE "190256910".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£¹¡Ë
           03   FILLER                     PIC X(09) VALUE "190250710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£¹¡Ë
           03   FILLER                     PIC X(09) VALUE "190253910".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£³¡ÊÃí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190257710".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£³¡ÊÃí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190257910".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÃí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190258110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´¡ÊÃí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190258510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£³¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190253110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£³¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190253510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190254110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190255510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÃí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190258910".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÃí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190259310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÃí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190259710".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´¡ÊÃí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190259910".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190250910".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190252310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190254310".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190255710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÃí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190260110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÃí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190260310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÃí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190260510".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÃí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190260710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£±¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190249910".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£±¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190250310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190251110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190252510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£¹¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190251310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£¹¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190251510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£±¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190251710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£¹¡¦Ãí£±£±¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190251910".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£±¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190252710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£¹¡¦Ãí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190254510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£¹¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190254710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£°¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190254910".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£¹¡¦Ãí£±£°¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190255110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´¡ÊÎÅÍÜÉÂ¾²¡¦Ãí£±£°¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190255910".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÃí£¹¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190256310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÃí£¹¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190256510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÃí£¹¡¦Ãí£±£±¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190256710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÃí£¹¡¦Ãí£±£°¡Ë
           03   FILLER                     PIC X(09) VALUE "190257110".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÃí£¹¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190257310".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÃí£¹¡¦Ãí£±£°¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190257510".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£´¡ÊÃí£±£°¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190258310".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£´¡ÊÃí£±£°¡¦Ãí£±£±¡Ë
           03   FILLER                     PIC X(09) VALUE "190258710".
      *    ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ£²¡ÊÃí£±£±¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190259110".
      *    ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ£²¡ÊÃí£±£±¡¦Ãí£±£²¡Ë
           03   FILLER                     PIC X(09) VALUE "190259510".
      *
       01  TBL-R04-SEIKATSU-R     REDEFINES  TBL-R04-SEIKATSU-AREA.
           03   TBL-R04-SEISRYCD           PIC X(09)  OCCURS  57.
      *
      *    ÎáÏÂ£¶Ç¯£¶·î²þÄêÀ¸³èÎÅÍÜÈ½ÄêÍÑ
       01  TBL-R06-SEIKATSU-AREA.
      *01  ²óÉü´ü¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ
           03   FILLER                     PIC X(09) VALUE "190207910".
           03   FILLER                     PIC X(09) VALUE "190208110".
           03   FILLER                     PIC X(09) VALUE "190208310".
           03   FILLER                     PIC X(09) VALUE "190208510".
           03   FILLER                     PIC X(09) VALUE "190249510".
      *    Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ
           03   FILLER                     PIC X(09) VALUE "190122010".
           03   FILLER                     PIC X(09) VALUE "190122110".
           03   FILLER                     PIC X(09) VALUE "190122210".
           03   FILLER                     PIC X(09) VALUE "190122310".
           03   FILLER                     PIC X(09) VALUE "190122410".
      *10  ÎÅÍÜÉÂ¾²°Ê³°¡¦Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡
           03   FILLER                     PIC X(09) VALUE "190155210".
           03   FILLER                     PIC X(09) VALUE "190155610".
           03   FILLER                     PIC X(09) VALUE "190155610".
           03   FILLER                     PIC X(09) VALUE "190155610".
           03   FILLER                     PIC X(09) VALUE "190155610".
      *    ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£±¡Ê£³£°·ï¡Ë
           03   FILLER                     PIC X(09) VALUE "190817610".
           03   FILLER                     PIC X(09) VALUE "190817810".
           03   FILLER                     PIC X(09) VALUE "190818010".
           03   FILLER                     PIC X(09) VALUE "190818210".
           03   FILLER                     PIC X(09) VALUE "190818410".
           03   FILLER                     PIC X(09) VALUE "190818610".
           03   FILLER                     PIC X(09) VALUE "190818810".
           03   FILLER                     PIC X(09) VALUE "190819010".
           03   FILLER                     PIC X(09) VALUE "190819210".
           03   FILLER                     PIC X(09) VALUE "190819410".
           03   FILLER                     PIC X(09) VALUE "190819610".
           03   FILLER                     PIC X(09) VALUE "190819810".
           03   FILLER                     PIC X(09) VALUE "190820010".
           03   FILLER                     PIC X(09) VALUE "190820210".
           03   FILLER                     PIC X(09) VALUE "190820410".
           03   FILLER                     PIC X(09) VALUE "190820610".
           03   FILLER                     PIC X(09) VALUE "190820810".
           03   FILLER                     PIC X(09) VALUE "190821010".
           03   FILLER                     PIC X(09) VALUE "190821210".
           03   FILLER                     PIC X(09) VALUE "190821410".
           03   FILLER                     PIC X(09) VALUE "190821610".
           03   FILLER                     PIC X(09) VALUE "190821810".
           03   FILLER                     PIC X(09) VALUE "190822010".
           03   FILLER                     PIC X(09) VALUE "190822210".
           03   FILLER                     PIC X(09) VALUE "190822410".
           03   FILLER                     PIC X(09) VALUE "190822610".
           03   FILLER                     PIC X(09) VALUE "190822810".
           03   FILLER                     PIC X(09) VALUE "190823010".
           03   FILLER                     PIC X(09) VALUE "190823210".
           03   FILLER                     PIC X(09) VALUE "190823410".
      *64  ÎÅÍÜÉÂÅïÆþ±¡ÎÁ£²¡Ê£³£°·ï¡Ë
           03   FILLER                     PIC X(09) VALUE "190823610".
           03   FILLER                     PIC X(09) VALUE "190823810".
           03   FILLER                     PIC X(09) VALUE "190824010".
           03   FILLER                     PIC X(09) VALUE "190824210".
           03   FILLER                     PIC X(09) VALUE "190824410".
           03   FILLER                     PIC X(09) VALUE "190824610".
           03   FILLER                     PIC X(09) VALUE "190824810".
           03   FILLER                     PIC X(09) VALUE "190825010".
           03   FILLER                     PIC X(09) VALUE "190825210".
           03   FILLER                     PIC X(09) VALUE "190825410".
           03   FILLER                     PIC X(09) VALUE "190825610".
           03   FILLER                     PIC X(09) VALUE "190825810".
           03   FILLER                     PIC X(09) VALUE "190826010".
           03   FILLER                     PIC X(09) VALUE "190826210".
           03   FILLER                     PIC X(09) VALUE "190826410".
           03   FILLER                     PIC X(09) VALUE "190826610".
           03   FILLER                     PIC X(09) VALUE "190826810".
           03   FILLER                     PIC X(09) VALUE "190827010".
           03   FILLER                     PIC X(09) VALUE "190827210".
           03   FILLER                     PIC X(09) VALUE "190827410".
           03   FILLER                     PIC X(09) VALUE "190827610".
           03   FILLER                     PIC X(09) VALUE "190827810".
           03   FILLER                     PIC X(09) VALUE "190828010".
           03   FILLER                     PIC X(09) VALUE "190828210".
           03   FILLER                     PIC X(09) VALUE "190828410".
           03   FILLER                     PIC X(09) VALUE "190828610".
           03   FILLER                     PIC X(09) VALUE "190828810".
           03   FILLER                     PIC X(09) VALUE "190829010".
           03   FILLER                     PIC X(09) VALUE "190829210".
           03   FILLER                     PIC X(09) VALUE "190829410".
      *77  Ã»¼ê£³¡Ê£·£³·ï¡Ë
           03   FILLER                     PIC X(09) VALUE "190178210".
           03   FILLER                     PIC X(09) VALUE "190178410".
           03   FILLER                     PIC X(09) VALUE "190178810".
           03   FILLER                     PIC X(09) VALUE "190179010".
           03   FILLER                     PIC X(09) VALUE "190179210".
           03   FILLER                     PIC X(09) VALUE "190179610".
           03   FILLER                     PIC X(09) VALUE "190179810".
           03   FILLER                     PIC X(09) VALUE "190180010".
           03   FILLER                     PIC X(09) VALUE "190180210".
           03   FILLER                     PIC X(09) VALUE "190180610".
           03   FILLER                     PIC X(09) VALUE "190181010".
           03   FILLER                     PIC X(09) VALUE "190181210".
           03   FILLER                     PIC X(09) VALUE "190181410".
           03   FILLER                     PIC X(09) VALUE "190181610".
           03   FILLER                     PIC X(09) VALUE "190181810".
           03   FILLER                     PIC X(09) VALUE "190195910".
           03   FILLER                     PIC X(09) VALUE "190196510".
           03   FILLER                     PIC X(09) VALUE "190196610".
           03   FILLER                     PIC X(09) VALUE "190196710".
           03   FILLER                     PIC X(09) VALUE "190197110".
           03   FILLER                     PIC X(09) VALUE "190197210".
           03   FILLER                     PIC X(09) VALUE "190197310".
           03   FILLER                     PIC X(09) VALUE "190197710".
           03   FILLER                     PIC X(09) VALUE "190197910".
           03   FILLER                     PIC X(09) VALUE "190220910".
           03   FILLER                     PIC X(09) VALUE "190221110".
           03   FILLER                     PIC X(09) VALUE "190265610".
           03   FILLER                     PIC X(09) VALUE "190265810".
           03   FILLER                     PIC X(09) VALUE "190266010".
           03   FILLER                     PIC X(09) VALUE "190266210".
           03   FILLER                     PIC X(09) VALUE "190266410".
           03   FILLER                     PIC X(09) VALUE "190266610".
           03   FILLER                     PIC X(09) VALUE "190266810".
           03   FILLER                     PIC X(09) VALUE "190267010".
           03   FILLER                     PIC X(09) VALUE "190267210".
           03   FILLER                     PIC X(09) VALUE "190267410".
           03   FILLER                     PIC X(09) VALUE "190267610".
           03   FILLER                     PIC X(09) VALUE "190269610".
           03   FILLER                     PIC X(09) VALUE "190269810".
           03   FILLER                     PIC X(09) VALUE "190270010".
           03   FILLER                     PIC X(09) VALUE "190270210".
           03   FILLER                     PIC X(09) VALUE "190270410".
           03   FILLER                     PIC X(09) VALUE "190270610".
           03   FILLER                     PIC X(09) VALUE "190270810".
           03   FILLER                     PIC X(09) VALUE "190271010".
           03   FILLER                     PIC X(09) VALUE "190271210".
           03   FILLER                     PIC X(09) VALUE "190271410".
           03   FILLER                     PIC X(09) VALUE "190271610".
           03   FILLER                     PIC X(09) VALUE "190271810".
           03   FILLER                     PIC X(09) VALUE "190272010".
           03   FILLER                     PIC X(09) VALUE "190272210".
           03   FILLER                     PIC X(09) VALUE "190272410".
           03   FILLER                     PIC X(09) VALUE "190272610".
           03   FILLER                     PIC X(09) VALUE "190272810".
           03   FILLER                     PIC X(09) VALUE "190273010".
           03   FILLER                     PIC X(09) VALUE "190873210".
           03   FILLER                     PIC X(09) VALUE "190873410".
           03   FILLER                     PIC X(09) VALUE "190873610".
           03   FILLER                     PIC X(09) VALUE "190873810".
           03   FILLER                     PIC X(09) VALUE "190874010".
           03   FILLER                     PIC X(09) VALUE "190874210".
           03   FILLER                     PIC X(09) VALUE "190874410".
           03   FILLER                     PIC X(09) VALUE "190874610".
           03   FILLER                     PIC X(09) VALUE "190874810".
           03   FILLER                     PIC X(09) VALUE "190875010".
           03   FILLER                     PIC X(09) VALUE "190875210".
           03   FILLER                     PIC X(09) VALUE "190875410".
           03   FILLER                     PIC X(09) VALUE "190875610".
           03   FILLER                     PIC X(09) VALUE "190875810".
           03   FILLER                     PIC X(09) VALUE "190876010".
           03   FILLER                     PIC X(09) VALUE "190876210".
           03   FILLER                     PIC X(09) VALUE "190876410".
           03   FILLER                     PIC X(09) VALUE "190876610".
      *150 ÃÏ°èÊñ³ç¥±¥¢ÉÂÅïÆþ±¡ÎÁ¡¢ÃÏ°èÊñ³ç¥±¥¢Æþ±¡°åÎÅ´ÉÍýÎÁ¡Ê£±£´£´·ï¡Ë
           03   FILLER                     PIC X(09) VALUE "190839310".
           03   FILLER                     PIC X(09) VALUE "190867210".
           03   FILLER                     PIC X(09) VALUE "190855310".
           03   FILLER                     PIC X(09) VALUE "190842910".
           03   FILLER                     PIC X(09) VALUE "190843310".
           03   FILLER                     PIC X(09) VALUE "190839510".
           03   FILLER                     PIC X(09) VALUE "190867410".
           03   FILLER                     PIC X(09) VALUE "190855510".
           03   FILLER                     PIC X(09) VALUE "190843110".
           03   FILLER                     PIC X(09) VALUE "190843510".
           03   FILLER                     PIC X(09) VALUE "190840110".
           03   FILLER                     PIC X(09) VALUE "190865210".
           03   FILLER                     PIC X(09) VALUE "190865610".
           03   FILLER                     PIC X(09) VALUE "190868010".
           03   FILLER                     PIC X(09) VALUE "190856110".
           03   FILLER                     PIC X(09) VALUE "190846910".
           03   FILLER                     PIC X(09) VALUE "190847310".
           03   FILLER                     PIC X(09) VALUE "190847710".
           03   FILLER                     PIC X(09) VALUE "190840310".
           03   FILLER                     PIC X(09) VALUE "190865410".
           03   FILLER                     PIC X(09) VALUE "190865810".
           03   FILLER                     PIC X(09) VALUE "190868210".
           03   FILLER                     PIC X(09) VALUE "190856310".
           03   FILLER                     PIC X(09) VALUE "190847110".
           03   FILLER                     PIC X(09) VALUE "190847510".
           03   FILLER                     PIC X(09) VALUE "190847910".
           03   FILLER                     PIC X(09) VALUE "190848110".
           03   FILLER                     PIC X(09) VALUE "190848310".
           03   FILLER                     PIC X(09) VALUE "190840910".
           03   FILLER                     PIC X(09) VALUE "190862410".
           03   FILLER                     PIC X(09) VALUE "190856910".
           03   FILLER                     PIC X(09) VALUE "190849310".
           03   FILLER                     PIC X(09) VALUE "190849710".
           03   FILLER                     PIC X(09) VALUE "190841110".
           03   FILLER                     PIC X(09) VALUE "190862610".
           03   FILLER                     PIC X(09) VALUE "190857110".
           03   FILLER                     PIC X(09) VALUE "190849510".
           03   FILLER                     PIC X(09) VALUE "190849910".
           03   FILLER                     PIC X(09) VALUE "190841710".
           03   FILLER                     PIC X(09) VALUE "190863610".
           03   FILLER                     PIC X(09) VALUE "190864010".
           03   FILLER                     PIC X(09) VALUE "190866410".
           03   FILLER                     PIC X(09) VALUE "190857710".
           03   FILLER                     PIC X(09) VALUE "190853310".
           03   FILLER                     PIC X(09) VALUE "190853710".
           03   FILLER                     PIC X(09) VALUE "190854110".
           03   FILLER                     PIC X(09) VALUE "190841910".
           03   FILLER                     PIC X(09) VALUE "190863810".
           03   FILLER                     PIC X(09) VALUE "190864210".
           03   FILLER                     PIC X(09) VALUE "190866610".
           03   FILLER                     PIC X(09) VALUE "190857910".
           03   FILLER                     PIC X(09) VALUE "190853510".
           03   FILLER                     PIC X(09) VALUE "190853910".
           03   FILLER                     PIC X(09) VALUE "190854310".
           03   FILLER                     PIC X(09) VALUE "190854510".
           03   FILLER                     PIC X(09) VALUE "190854710".
           03   FILLER                     PIC X(09) VALUE "190838910".
           03   FILLER                     PIC X(09) VALUE "190866810".
           03   FILLER                     PIC X(09) VALUE "190854910".
           03   FILLER                     PIC X(09) VALUE "190842110".
           03   FILLER                     PIC X(09) VALUE "190842510".
           03   FILLER                     PIC X(09) VALUE "190839110".
           03   FILLER                     PIC X(09) VALUE "190867010".
           03   FILLER                     PIC X(09) VALUE "190855110".
           03   FILLER                     PIC X(09) VALUE "190842310".
           03   FILLER                     PIC X(09) VALUE "190842710".
           03   FILLER                     PIC X(09) VALUE "190839710".
           03   FILLER                     PIC X(09) VALUE "190860010".
           03   FILLER                     PIC X(09) VALUE "190859610".
           03   FILLER                     PIC X(09) VALUE "190858810".
           03   FILLER                     PIC X(09) VALUE "190859210".
           03   FILLER                     PIC X(09) VALUE "190864410".
           03   FILLER                     PIC X(09) VALUE "190864810".
           03   FILLER                     PIC X(09) VALUE "190867610".
           03   FILLER                     PIC X(09) VALUE "190855710".
           03   FILLER                     PIC X(09) VALUE "190845310".
           03   FILLER                     PIC X(09) VALUE "190845710".
           03   FILLER                     PIC X(09) VALUE "190846110".
           03   FILLER                     PIC X(09) VALUE "190843710".
           03   FILLER                     PIC X(09) VALUE "190844110".
           03   FILLER                     PIC X(09) VALUE "190844510".
           03   FILLER                     PIC X(09) VALUE "190844910".
           03   FILLER                     PIC X(09) VALUE "190839910".
           03   FILLER                     PIC X(09) VALUE "190860210".
           03   FILLER                     PIC X(09) VALUE "190859410".
           03   FILLER                     PIC X(09) VALUE "190859810".
           03   FILLER                     PIC X(09) VALUE "190859010".
           03   FILLER                     PIC X(09) VALUE "190864610".
           03   FILLER                     PIC X(09) VALUE "190865010".
           03   FILLER                     PIC X(09) VALUE "190867810".
           03   FILLER                     PIC X(09) VALUE "190855910".
           03   FILLER                     PIC X(09) VALUE "190845510".
           03   FILLER                     PIC X(09) VALUE "190845910".
           03   FILLER                     PIC X(09) VALUE "190846310".
           03   FILLER                     PIC X(09) VALUE "190843910".
           03   FILLER                     PIC X(09) VALUE "190844310".
           03   FILLER                     PIC X(09) VALUE "190844710".
           03   FILLER                     PIC X(09) VALUE "190845110".
           03   FILLER                     PIC X(09) VALUE "190846510".
           03   FILLER                     PIC X(09) VALUE "190846710".
           03   FILLER                     PIC X(09) VALUE "190840510".
           03   FILLER                     PIC X(09) VALUE "190862010".
           03   FILLER                     PIC X(09) VALUE "190856510".
           03   FILLER                     PIC X(09) VALUE "190848510".
           03   FILLER                     PIC X(09) VALUE "190848910".
           03   FILLER                     PIC X(09) VALUE "190840710".
           03   FILLER                     PIC X(09) VALUE "190862210".
           03   FILLER                     PIC X(09) VALUE "190856710".
           03   FILLER                     PIC X(09) VALUE "190848710".
           03   FILLER                     PIC X(09) VALUE "190849110".
           03   FILLER                     PIC X(09) VALUE "190841310".
           03   FILLER                     PIC X(09) VALUE "190861610".
           03   FILLER                     PIC X(09) VALUE "190861210".
           03   FILLER                     PIC X(09) VALUE "190860410".
           03   FILLER                     PIC X(09) VALUE "190860810".
           03   FILLER                     PIC X(09) VALUE "190862810".
           03   FILLER                     PIC X(09) VALUE "190863210".
           03   FILLER                     PIC X(09) VALUE "190866010".
           03   FILLER                     PIC X(09) VALUE "190857310".
           03   FILLER                     PIC X(09) VALUE "190851710".
           03   FILLER                     PIC X(09) VALUE "190852110".
           03   FILLER                     PIC X(09) VALUE "190852510".
           03   FILLER                     PIC X(09) VALUE "190850110".
           03   FILLER                     PIC X(09) VALUE "190850510".
           03   FILLER                     PIC X(09) VALUE "190850910".
           03   FILLER                     PIC X(09) VALUE "190851310".
           03   FILLER                     PIC X(09) VALUE "190841510".
           03   FILLER                     PIC X(09) VALUE "190861810".
           03   FILLER                     PIC X(09) VALUE "190861010".
           03   FILLER                     PIC X(09) VALUE "190861410".
           03   FILLER                     PIC X(09) VALUE "190860610".
           03   FILLER                     PIC X(09) VALUE "190863010".
           03   FILLER                     PIC X(09) VALUE "190863410".
           03   FILLER                     PIC X(09) VALUE "190866210".
           03   FILLER                     PIC X(09) VALUE "190857510".
           03   FILLER                     PIC X(09) VALUE "190851910".
           03   FILLER                     PIC X(09) VALUE "190852310".
           03   FILLER                     PIC X(09) VALUE "190852710".
           03   FILLER                     PIC X(09) VALUE "190850310".
           03   FILLER                     PIC X(09) VALUE "190850710".
           03   FILLER                     PIC X(09) VALUE "190851110".
           03   FILLER                     PIC X(09) VALUE "190851510".
           03   FILLER                     PIC X(09) VALUE "190852910".
           03   FILLER                     PIC X(09) VALUE "190853110".
      *294 ÆÃÄêµ¡Ç½ÉÂ±¡¥ê¥Ï¥Ó¥ê¥Æ¡¼¥·¥ç¥óÉÂÅïÆþ±¡ÎÁ
           03   FILLER                     PIC X(09) VALUE "190265310".
      *295 Í­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190122510".
      *296 ÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡ÊÆÃÊÌÆþ±¡´ðËÜÎÁ¡Ë
           03   FILLER                     PIC X(09) VALUE "190121810".
      *
       01  TBL-R06-SEIKATSU-R     REDEFINES  TBL-R06-SEIKATSU-AREA.
           03   TBL-R06-SEISRYCD           PIC X(09)  OCCURS  295.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ¥·¥¹¥Æ¥à´ÉÍý¥Þ¥¹¥¿
           COPY    "CPSYSKANRI.INC".
      *    ¡ÊÉÂÅï¾ðÊó¡Ë
           COPY    "CPSK5001.INC".
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
           COPY    "CPTENSU.INC".
      *
      *    ´µ¼Ô¥Þ¥¹¥¿¡Ý
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    ÊÝ¸±»»ÄêÍÑÇ¯Îð¡¦³ä¹ç·×»»¥µ¥Ö
           COPY    "CPORCSAGECHK.INC".
      *
      *   ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSS007.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           SS007-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SS007-OT
      *
      *    ½é´ü½èÍý
           PERFORM 100-INIT-SEC
      *
      *    ¼ç½èÍý
           PERFORM 200-MAIN-SEC
      *
      *    ½ªÎ»½èÍý
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    SS007-SRYYM     TO  WRK-SRYYMD (1:6)
           MOVE    "01"            TO  WRK-SRYYMD (7:2)
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    ÎÅÍÜÉÂ¾²Æþ±¡¥Á¥§¥Ã¥¯
           IF      SS007-SRYYM     >=  "201204"
              PERFORM 2001-RYOYO-MAECHK-SEC
           END-IF
      *
           MOVE    ZERO            TO  FLG-UNMATCH
      *
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
                    OR   ( FLG-UNMATCH     NOT =   ZERO )
      *
               IF    ( NACCT-ZAIKAISU      =   ZERO )
                OR   ( NACCT-SRYCDTOTAL    =   099999916 )
                   CONTINUE
               ELSE
                   PERFORM 2001-NYUINACT-CHK-SEC
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY17-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    ¿©Æ²²Ã»»»»Äê²ÄÇ½ÉÂÅï¤ÎÆþ±¡ÂÐ¾ÝÆü¼èÆÀ
           PERFORM   2002-SHOKUDOU-CHK-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÎÅÍÜÉÂ¾²Æþ±¡¥Á¥§¥Ã¥¯
      *****************************************************************
       2001-RYOYO-MAECHK-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-RYOYO-CHK
      *
      *    ´µ¼ÔÇ¯Îð·×»»
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    WRK-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"            USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
      **     DISPLAY "ORCSS007 AGECHK-NENREI = " AGECHK-NENREI
      *
      *    £¶£µºÐ°Ê¾å¤Î´µ¼Ô¤Ë¤Ä¤¤¤Æ´µ¼ÔÆþ±¡ÍúÎò¤ò¸¡º÷¤·¤ÆÎÅÍÜÉÂ¾²¤Ë
      *    Æþ±¡¤·¤Æ¤¤¤ë³ºÅöÆü¤òµá¤á¤ë¡ÊÏ«ºÒ¡¢¼«Çå¡¢¸øÌ³ºÒ³²¤Ï½ü¤¯¡Ë
           IF     AGECHK-NENREI        >=  65
             INITIALIZE                    PTNYUINRRK-REC
             MOVE    SPA-HOSPNUM       TO  PTNYUINRRK-HOSPNUM
             MOVE    SPA-PTID          TO  PTNYUINRRK-PTID
      *      Å¾ÆþÆü¥»¥Ã¥È¡Ê·îËöÆü¡Ë
             MOVE    SS007-SRYYM       TO  PTNYUINRRK-TENNYUYMD(1:6)
             INITIALIZE                    STS-AREA-DAY
             INITIALIZE                    LNK-DAY7-AREA
             MOVE    "71"              TO  LNK-DAY7-IRAI
             MOVE    SS007-SRYYM       TO  LNK-DAY7-YM
             CALL    "ORCSDAY"   USING     STS-AREA-DAY
                                           LNK-DAY7-AREA
             MOVE    LNK-DAY7-KOYOMI   TO  WRK-MATSUYMD
             MOVE    WRK-MATSUDD       TO  PTNYUINRRK-TENNYUYMD(7:2)
      *      Å¾½ÐÆü¥»¥Ã¥È¡Ê£±Æü¡Ë
             MOVE    SS007-SRYYM       TO  PTNYUINRRK-TENSTUYMD(1:6)
             MOVE    "01"              TO  PTNYUINRRK-TENSTUYMD(7:2)
      *
             MOVE    "tbl_ptnyuinrrk"  TO  MCP-TABLE
             MOVE    "key44"           TO  MCP-PATHNAME
             PERFORM 900-PTNYUINRRK-SELECT-SEC
             IF      FLG-PTNYUINRRK      =   ZERO
               MOVE  "tbl_ptnyuinrrk"  TO  MCP-TABLE
               MOVE  "key44"           TO  MCP-PATHNAME
               PERFORM 900-PTNYUINRRK-READ-SEC
               IF  FLG-PTNYUINRRK      =   ZERO
                   PERFORM UNTIL  FLG-PTNYUINRRK  =  1
      *                ÉÂÅï¼ïÊÌ¤Î¼èÆÀ
                       PERFORM    20011-BTUSEL-SEC
      *
                       IF     PTNYUINRRK-TENNYUYMD(1:6) <  SS007-SRYYM
                           MOVE     1  TO  IDX-ST
                       END-IF
                       IF     PTNYUINRRK-TENNYUYMD(1:6) =  SS007-SRYYM
                           MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                       END-IF
                       IF     PTNYUINRRK-TENSTUYMD(1:6) >  SS007-SRYYM
                           MOVE     WRK-MATSUDD  TO  IDX-ED
                       END-IF
                       IF     PTNYUINRRK-TENSTUYMD(1:6) =  SS007-SRYYM
                           MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                       END-IF
      **                 DISPLAY   "---------------------"
      **                 DISPLAY   "IDX-ST = " IDX-ST
      **                 DISPLAY   "IDX-ED = " IDX-ED
                       PERFORM   VARYING   IDX   FROM  IDX-ST  BY  1
                                 UNTIL    (IDX   >     IDX-ED)
                           IF   (SYS-5001-BTU-SBT   =    "04")   OR
                                (SYS-5001-BTU-SBT   =    "09")   OR
                               ((SYS-5001-BTU-SBT   =    "08")  AND
                                (SYS-5001-CRYOYOKBN =    "1" ))
                              MOVE   "1"     TO    WRK-RYOYO-TBL(IDX)
                           ELSE
                              MOVE   SPACE   TO    WRK-RYOYO-TBL(IDX)
                           END-IF
                       END-PERFORM
      **                 DISPLAY  "----5----0----5----0----5----0-"
      **                 DISPLAY  WRK-RYOYO-TABLE
                       MOVE  "tbl_ptnyuinrrk"  TO  MCP-TABLE
                       MOVE  "key44"           TO  MCP-PATHNAME
                       PERFORM 900-PTNYUINRRK-READ-SEC
                   END-PERFORM
               END-IF
             END-IF
      *
             MOVE    "tbl_ptnyuinrrk"  TO  MCP-TABLE
             MOVE    "key44"           TO  MCP-PATHNAME
             PERFORM  910-DBCLOSECURSOR-SEC
      *
      *      Ï«ºÒ¡¢¼«Çå¡¢¸øÌ³ºÒ³²¤Ç¤ÎÆþ±¡ÆüÈ½Äê
             INITIALIZE                    NYUINACCT-REC
             MOVE    SPA-HOSPNUM       TO  NACCT-HOSPNUM
             MOVE    SPA-PTID          TO  NACCT-PTID
             MOVE    SS007-SRYYM       TO  NACCT-SRYYM
             MOVE    "5"               TO  NACCT-ZAISKBKBN
             MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
             MOVE    "key17"           TO  MCP-PATHNAME
             PERFORM 900-NYUINACCT-SELECT-SEC
             IF      FLG-NYUINACCT     =   ZERO
                MOVE  "tbl_nyuinacct"  TO  MCP-TABLE
                MOVE  "key17"          TO  MCP-PATHNAME
                PERFORM 900-NYUINACCT-READ-SEC
                IF   FLG-NYUINACCT     =   ZERO
                    PERFORM   VARYING  IDX   FROM   1   BY   1
                       UNTIL  IDX      >     31
                          MOVE   NACCT-DAY (IDX)
                                       TO  WRK-HKNCOMBI-TBL9
                          MOVE   WRK-HKNCOMBI-TBLX
                                       TO  WRK-HKNCOMBI-TBL(IDX)
                    END-PERFORM
                END-IF
             END-IF
      *
             PERFORM   VARYING  IDX   FROM  1  BY  1
               UNTIL   IDX   >  31
                 IF   WRK-HKNCOMBI-TBL(IDX)  NOT =  "0000"
                    IF   IDX    =    1
      *               ÊÝ¸±¶èÊ¬¼èÆÀ
                      INITIALIZE                     ORCSAGECHKAREA
                      MOVE    SPA-HOSPNUM        TO  AGECHK-HOSPNUM
                      MOVE    SPA-PTID           TO  AGECHK-PTID
                      MOVE    WRK-SRYYMD         TO  AGECHK-SRYYMD
                      MOVE    PTINF-BIRTHDAY     TO  AGECHK-BIRTHDAY
                      MOVE    WRK-HKNCOMBI-TBL(IDX)
                                                 TO  AGECHK-HKNCOMBINUM
                      CALL    "ORCSAGECHK"           USING
                                                     ORCSAGECHKAREA
                                                     SPA-AREA
      **                DISPLAY  "AGECHK-RC 1 = " AGECHK-RC
                      IF   AGECHK-HKNKBN   =  1  OR  2  OR  4  OR  5
                          MOVE   "1"             TO  WRK-RSI-TBL(IDX)
                      END-IF
                    ELSE
                      MOVE    ZERO      TO   IDX1
                      COMPUTE   IDX1    =    IDX   -  1
                      IF WRK-HKNCOMBI-TBL(IDX1) = WRK-HKNCOMBI-TBL(IDX)
                          MOVE  WRK-RSI-TBL(IDX1)  TO  WRK-RSI-TBL(IDX)
                      ELSE
      *                 ÊÝ¸±¶èÊ¬¼èÆÀ
                        INITIALIZE                     ORCSAGECHKAREA
                        MOVE    SPA-HOSPNUM        TO  AGECHK-HOSPNUM
                        MOVE    SPA-PTID           TO  AGECHK-PTID
                        MOVE    WRK-SRYYMD         TO  AGECHK-SRYYMD
                        MOVE    PTINF-BIRTHDAY     TO  AGECHK-BIRTHDAY
                        MOVE    WRK-HKNCOMBI-TBL(IDX)
                                                 TO  AGECHK-HKNCOMBINUM
                        CALL    "ORCSAGECHK"           USING
                                                       ORCSAGECHKAREA
                                                       SPA-AREA
      **                  DISPLAY  "AGECHK-RC 2 = " AGECHK-RC
                        IF   AGECHK-HKNKBN   =  1  OR  2  OR  4  OR  5
                            MOVE   "1"             TO  WRK-RSI-TBL(IDX)
                        END-IF
                      END-IF
                    END-IF
                 END-IF
             END-PERFORM
      *
             PERFORM    VARYING   IDX    FROM   1   BY  1
               UNTIL    IDX    >    31
      **           DISPLAY "IDX = " IDX
      **           DISPLAY " WRK-RSI-TBL(IDX) = " WRK-RSI-TBL(IDX)
             END-PERFORM
      *      Æþ±¡²ñ·×¥¯¥í¡¼¥º
             MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
             MOVE    "key17"           TO  MCP-PATHNAME
             PERFORM  910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           IF    WRK-RYOYO-TABLE      NOT = SPACE
               MOVE     1             TO    FLG-RYOYO-CHK
           END-IF
           .
       2001-RYOYO-MAECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÉÂÅï¾ðÊó¼èÆÀ
      *****************************************************************
       20011-BTUSEL-SEC             SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC    =    ZERO
               MOVE   MCPDATA-REC      TO  SYS-5001-REC
           ELSE
               INITIALIZE                  SYS-5001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-BTUSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       2001-NYUINACT-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  WRK-ZAITEN
                                       WRK-HYOKETU-TEN
      *
           PERFORM 900-NYUINACT-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACT    NOT =   ZERO )
      *
               PERFORM VARYING IDX-NSRY    FROM    1   BY  1
                       UNTIL ( IDX-NSRY    >   5 )
      *
                   IF    ( NSRY-SRYCD (IDX-NSRY)   NOT =   SPACE )
                       MOVE    NSRY-SRYCD (IDX-NSRY)
                                   TO  WRK-SRYCD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       EVALUATE    TNS-TENSIKIBETU
                       WHEN    3
                           COMPUTE WRK-ZAITEN
                               =   WRK-ZAITEN  +   TNS-TEN
                       WHEN    8
                           COMPUTE WRK-ZAITEN
                                =   WRK-ZAITEN  + ( TNS-TEN *   -1 )
                       WHEN    6
                           MOVE    TNS-TEN     TO  WRK-HYOKETU-TEN
                       END-EVALUATE
                   END-IF
      *
      *            À¸³èÎÅÍÜ¤È¤Ê¤ë´µ¼Ô¤¬¡¢À¸³èÎÅÍÜ°Ê³°¤ÎÆþ±¡ÎÁ¤ò»»Äê
      *            ¤·¤Æ¤¤¤¿¾ì¹ç¤Î¥Á¥§¥Ã¥¯
                   IF     (SS007-SRYYM     >=  "201204")   AND
                          (SS007-SRYYM     <=  "202405")
                     IF   (IDX-NSRY     =     1)    AND
                        ( NSRY-SRYCD (IDX-NSRY)   NOT =   SPACE )
                       PERFORM    VARYING   IDX-SEI   FROM  1  BY  1
                         UNTIL   (IDX-SEI    >   84)
                           IF  NSRY-SRYCD (IDX-NSRY)
                                       =  TBL-H24-SEISRYCD(IDX-SEI)
                             PERFORM   VARYING  IDX FROM  1  BY  1
                                UNTIL    (IDX   >     31)
                                  IF   (NACCT-DAY(IDX)      =   1 ) AND
                                       (WRK-RYOYO-TBL(IDX)  =  "1") AND
                                       (WRK-RSI-TBL(IDX)    =  SPACE)
                                           MOVE    1    TO  SS007-RC2
                                  END-IF
                             END-PERFORM
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
                   IF     (SS007-SRYYM     >=  "201404")   AND
                          (SS007-SRYYM     <=  "202405")
                     IF   (IDX-NSRY     =     1)    AND
                        ( NSRY-SRYCD (IDX-NSRY)   NOT =   SPACE )
                       PERFORM    VARYING   IDX-SEI   FROM  1  BY  1
                         UNTIL   (IDX-SEI    >   17)
                           IF  NSRY-SRYCD (IDX-NSRY)
                                       =  TBL-H26-SEISRYCD(IDX-SEI)
                             PERFORM   VARYING  IDX FROM  1  BY  1
                                UNTIL    (IDX   >     31)
                                  IF   (NACCT-DAY(IDX)      =   1 ) AND
                                       (WRK-RYOYO-TBL(IDX)  =  "1") AND
                                       (WRK-RSI-TBL(IDX)    =  SPACE)
                                           MOVE    1    TO  SS007-RC2
                                  END-IF
                             END-PERFORM
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
                   IF     (SS007-SRYYM     >=  "201604")   AND
                          (SS007-SRYYM     <=  "202405")
                     IF   (IDX-NSRY     =     1)    AND
                        ( NSRY-SRYCD (IDX-NSRY)   NOT =   SPACE )
                       PERFORM    VARYING   IDX-SEI   FROM  1  BY  1
                         UNTIL   (IDX-SEI    >   18)
                           IF  NSRY-SRYCD (IDX-NSRY)
                                       =  TBL-H28-SEISRYCD(IDX-SEI)
                             PERFORM   VARYING  IDX FROM  1  BY  1
                                UNTIL    (IDX   >     31)
                                  IF   (NACCT-DAY(IDX)      =   1 ) AND
                                       (WRK-RYOYO-TBL(IDX)  =  "1") AND
                                       (WRK-RSI-TBL(IDX)    =  SPACE)
                                           MOVE    1    TO  SS007-RC2
                                  END-IF
                             END-PERFORM
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
                   IF     (SS007-SRYYM     >=  "201804")   AND
                          (SS007-SRYYM     <=  "202405")
                     IF   (IDX-NSRY     =     1)    AND
                        ( NSRY-SRYCD (IDX-NSRY)   NOT =   SPACE )
                       PERFORM    VARYING   IDX-SEI   FROM  1  BY  1
                         UNTIL   (IDX-SEI    >   41)
                           IF  NSRY-SRYCD (IDX-NSRY)
                                       =  TBL-H30-SEISRYCD(IDX-SEI)
                             PERFORM   VARYING  IDX FROM  1  BY  1
                                UNTIL    (IDX   >     31)
                                  IF   (NACCT-DAY(IDX)      =   1 ) AND
                                       (WRK-RYOYO-TBL(IDX)  =  "1") AND
                                       (WRK-RSI-TBL(IDX)    =  SPACE)
                                           MOVE    1    TO  SS007-RC2
                                  END-IF
                             END-PERFORM
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
                   IF     (SS007-SRYYM     >=  "202010")   AND
                          (SS007-SRYYM     <=  "202405")
                     IF   (IDX-NSRY     =     1)    AND
                        ( NSRY-SRYCD (IDX-NSRY)   NOT =   SPACE )
                       PERFORM    VARYING   IDX-SEI   FROM  1  BY  1
                         UNTIL   (IDX-SEI    >   8)
                           IF  NSRY-SRYCD (IDX-NSRY)
                                       =  TBL-R02-SEISRYCD(IDX-SEI)
                             PERFORM   VARYING  IDX FROM  1  BY  1
                                UNTIL    (IDX   >     31)
                                  IF   (NACCT-DAY(IDX)      =   1 ) AND
                                       (WRK-RYOYO-TBL(IDX)  =  "1") AND
                                       (WRK-RSI-TBL(IDX)    =  SPACE)
                                           MOVE    1    TO  SS007-RC2
                                  END-IF
                             END-PERFORM
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
                   IF     (SS007-SRYYM     >=  "202204")   AND
                          (SS007-SRYYM     <=  "202405")
                     IF   (IDX-NSRY     =     1)    AND
                        ( NSRY-SRYCD (IDX-NSRY)   NOT =   SPACE )
                       PERFORM    VARYING   IDX-SEI   FROM  1  BY  1
                         UNTIL   (IDX-SEI    >   57)
                           IF  NSRY-SRYCD (IDX-NSRY)
                                       =  TBL-R04-SEISRYCD(IDX-SEI)
                             PERFORM   VARYING  IDX FROM  1  BY  1
                                UNTIL    (IDX   >     31)
                                  IF   (NACCT-DAY(IDX)      =   1 ) AND
                                       (WRK-RYOYO-TBL(IDX)  =  "1") AND
                                       (WRK-RSI-TBL(IDX)    =  SPACE)
                                           MOVE    1    TO  SS007-RC2
                                  END-IF
                             END-PERFORM
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
                   IF     (SS007-SRYYM     >=  "202406")
                     IF   (IDX-NSRY     =     1)    AND
                        ( NSRY-SRYCD (IDX-NSRY)   NOT =   SPACE )
                       PERFORM    VARYING   IDX-SEI   FROM  1  BY  1
                         UNTIL   (IDX-SEI    >   295)
                           IF  NSRY-SRYCD (IDX-NSRY)
                                       =  TBL-R06-SEISRYCD(IDX-SEI)
                             PERFORM   VARYING  IDX FROM  1  BY  1
                                UNTIL    (IDX   >     31)
                                  IF   (NACCT-DAY(IDX)      =   1 ) AND
                                       (WRK-RYOYO-TBL(IDX)  =  "1") AND
                                       (WRK-RSI-TBL(IDX)    =  SPACE)
                                           MOVE    1    TO  SS007-RC2
                                  END-IF
                             END-PERFORM
                           END-IF
                       END-PERFORM
                     END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACT-KEY9-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-HYOKETU-TEN NOT =   ZERO )
               COMPUTE WRK-ZAITEN
                   =   WRK-ZAITEN
                   -   WRK-ZAITEN * ( WRK-HYOKETU-TEN / 100)
                   +  0.5
           END-IF
      *
      **     DISPLAY "WRK-ZAITEN = " WRK-ZAITEN
      *
           IF    ( NACCT-ZAITEN    NOT =   WRK-ZAITEN )
               MOVE    1               TO  FLG-UNMATCH
           END-IF
      *
           .
       2001-NYUINACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¿©Æ²²Ã»»»»Äê²ÄÇ½ÉÂÅï¤ÎÆþ±¡ÂÐ¾ÝÆü¼èÆÀ
      *****************************************************************
       2002-SHOKUDOU-CHK-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-RYOYO-CHK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *    Å¾ÆþÆü¥»¥Ã¥È¡Ê·îËöÆü¡Ë
           MOVE    SS007-SRYYM         TO  PTNYUINRRK-TENNYUYMD(1:6)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SS007-SRYYM         TO  LNK-DAY7-YM
           CALL    "ORCSDAY"   USING       STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-MATSUYMD
           MOVE    WRK-MATSUDD         TO  PTNYUINRRK-TENNYUYMD(7:2)
      *    Å¾½ÐÆü¥»¥Ã¥È¡Ê£±Æü¡Ë
           MOVE    SS007-SRYYM         TO  PTNYUINRRK-TENSTUYMD(1:6)
           MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:2)
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 900-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK      =   ZERO
             MOVE  "tbl_ptnyuinrrk"    TO  MCP-TABLE
             MOVE  "key44"             TO  MCP-PATHNAME
             PERFORM 900-PTNYUINRRK-READ-SEC
             IF  FLG-PTNYUINRRK        =   ZERO
                 PERFORM UNTIL  FLG-PTNYUINRRK  =  1
      *              ÉÂÅï¼ïÊÌ¤Î¼èÆÀ
                     PERFORM    20011-BTUSEL-SEC
      *
                     IF     PTNYUINRRK-TENNYUYMD(1:6) <  SS007-SRYYM
                         MOVE     1  TO  IDX-ST
                     END-IF
                     IF     PTNYUINRRK-TENNYUYMD(1:6) =  SS007-SRYYM
                         MOVE  PTNYUINRRK-TENNYUYMD(7:2)  TO  IDX-ST
                     END-IF
                     IF     PTNYUINRRK-TENSTUYMD(1:6) >  SS007-SRYYM
                         MOVE     WRK-MATSUDD  TO  IDX-ED
                     END-IF
                     IF     PTNYUINRRK-TENSTUYMD(1:6) =  SS007-SRYYM
                         MOVE  PTNYUINRRK-TENSTUYMD(7:2)  TO  IDX-ED
                     END-IF
      **               DISPLAY   "---------------------"
      **               DISPLAY   "IDX-ST = " IDX-ST
      **               DISPLAY   "IDX-ED = " IDX-ED
                     PERFORM   VARYING   IDX   FROM  IDX-ST  BY  1
                               UNTIL    (IDX   >     IDX-ED)
                         IF  SYS-5001-BTU-SYOKUDOKBN    =   '2'
                             MOVE   "1"     TO  SS007-RC3(IDX)
                         ELSE
                             MOVE   SPACE   TO  SS007-RC3(IDX)
                         END-IF
                     END-PERFORM
      **               DISPLAY  "----5----0----5----0----5----0-"
      **               DISPLAY  SS007-RC3-TBL
                     MOVE  "tbl_ptnyuinrrk"  TO  MCP-TABLE
                     MOVE  "key44"           TO  MCP-PATHNAME
                     PERFORM 900-PTNYUINRRK-READ-SEC
                 END-PERFORM
             END-IF
           END-IF
      *
             MOVE    "tbl_ptnyuinrrk"  TO  MCP-TABLE
             MOVE    "key44"           TO  MCP-PATHNAME
             PERFORM  910-DBCLOSECURSOR-SEC
      *
      *
           .
       2002-SHOKUDOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    FLG-UNMATCH     TO  SS007-RC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼Ô¥Þ¥¹¥¿¡¼ÆÉ¹þ¡Ê¼ç¥­¡¼¡Ë
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF  MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   MOVE    1           TO  FLG-PTINF
                   INITIALIZE              PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    SPA-PTID        TO  NACCT-PTID
           MOVE    SS007-SRYYM     TO  NACCT-SRYYM
           MOVE    "1"             TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC   TO   MCPDATA-REC
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               MOVE    1           TO  FLG-NYUINACCT
               INITIALIZE              NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY24-SEL-EXT.
            EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×FETCH½èÍý
      *****************************************************************
       900-NYUINACCT-KEY17-FET-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               MOVE    1           TO  FLG-NYUINACCT
               INITIALIZE              NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY17-FET-EXT.
            EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼¸¡º÷
      *****************************************************************
       900-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   911-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    Æþ±¡²ñ·×¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  NYUINACCT-REC
               MOVE    ZERO               TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                     NYUINACCT-REC
               MOVE    1                  TO  FLG-NYUINACCT
           END-IF
      *
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACT-KEY9-SEL-SEC       SECTION.
      *
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¸¡º÷½èÍý
           MOVE    ZERO                TO  FLG-NYUINACT
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACT
               INITIALIZE                  NYUINACT-REC
           END-IF
      *
           .
       900-NYUINACT-KEY9-SEL-EXT.
            EXIT.
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°ÙFETCHÍý
      *****************************************************************
       900-NYUINACT-KEY9-FET-SEC       SECTION.
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACT
               INITIALIZE                  NYUINACT-REC
           END-IF
      *
           .
       900-NYUINACT-KEY9-FET-EXT.
            EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎò¸¡º÷
      *****************************************************************
       900-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC         TO  MCPDATA-REC
           PERFORM   911-DBSELECT-SEC
           IF      MCP-RC                 =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ´µ¼ÔÆþ±¡ÍúÎòÆÉ¹þ
      *****************************************************************
       900-PTNYUINRRK-READ-SEC        SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC                 =   ZERO
               MOVE    MCPDATA-REC        TO  PTNYUINRRK-REC
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                     PTNYUINRRK-REC
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤â¹Ô¤¦)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤Ï¹Ô¤ï¤Ê¤¤)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥º½èÍý
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
