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
       PROGRAM-ID.             ORCHCN68S01.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : Æþ±¡²ñ·×¾È²ñ
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : °åÎÅ¶èÊ¬¡¦£Á£Ä£ÌÉ¾²ÁÉ¼ÊÔ½¸¥µ¥Ö
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  13/09/17    NACL-ÂÀÅÄ     ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô        ÆüÉÕ      ÆâÍÆ
      *  05.02.01    ORCAMO       24/05/07  ÎáÏÂ£¶Ç¯ÅÙ²þÀµÂÐ±þ
      *  05.02.02    ORCAMO       24/08/07  ÎáÏÂ£¶Ç¯ÅÙ°åÎÅ¶èÊ¬¥³¥á¥ó¥ÈÆÉ¤ßÂØ¤¨
      *  05.02.03    ORCAMO       24/10/21  ÎáÏÂ£¶Ç¯ÅÙ°åÎÅ¶èÊ¬¥³¥á¥ó¥ÈÆÉ
      *                                     ¤ßÂØ¤¨¤Î¥¹¥â¥ó¤ÎºÇ¿·¤Î¥³¡¼¥É¤òÀßÄê  
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-RYOCOM          PIC 9(01).
      *
      *    ¥¤¥ó¥Ç¥Ã¥¯¥¹ÎÎ°è
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-DAY             PIC 9(05).
           03  IDX-LST             PIC 9(05).
           03  IDX-SRY             PIC 9(05).
           03  IDX-MED             PIC 9(05).
           03  IDXK                PIC 9(05).
           03  IDXM                PIC 9(05).
           03  IDXR                PIC 9(05).
           03  IDXX                PIC 9(05).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-RYOYO-DAY-G.
               05  WRK-RYOYO-DAY           PIC S9(02)
                                           OCCURS  31.
           03  WRK-RYOYO-G.
               05  WRK-RYOYO-CD            PIC X(09)
                                           OCCURS  31.
           03  WRK-RYOYO-KHN-G.
               05  WRK-RYOYO-KHN           PIC 9(02)
                                           OCCURS  31.
           03  WRK-TNS-SRYCD               PIC X(09).
           03  WRK-TNS-KJNYMD              PIC X(08).
           03  WRK-SRYYM                   PIC X(06).
           03  WRK-ZAINUM-G.
               05  WRK-ZAINUM              PIC 9(08).
           03  WRK-NUM-ZEN                 PIC X(02).
           03  WRK-NUM-HAN                 PIC X(01).
      *
      *    CPMEDKBN.INC ¤«¤éÀßÄê 
       01  MEDINF-AREA.
           03  MEDINF-MAX                  PIC 9(05).
           03  MEDINF-OCC                  OCCURS       100.
               09  MEDINF-MEDKBN           PIC 9(01).
               09  MEDINF-MEDKBN1          PIC X(01).
               09  MEDINF-DSPKBN           PIC X(01).
               09  MEDINF-LIMIT            PIC 9(03).
               09  MEDINF-ALLKBN           PIC 9(01).
               09  MEDINF-ZAINUM           PIC 9(04).
      *********09  MEDINF-NUM              PIC 9(02).
               09  MEDINF-SRYCD            PIC X(09).
               09  MEDINF-ID               PIC X(02).
               09  MEDINF-NAME             PIC X(100).
      *
       01  CONST-AREA.
           03  CONST-R060601           PIC X(08)   VALUE   "20240601".
           03  CONST-H300401           PIC X(08)   VALUE   "20180401".
           03  CONST-H240401           PIC X(08)   VALUE   "20120401".
           03  CONST-H220401           PIC X(08)   VALUE   "20100401".
           03  CONST-H200401           PIC X(08)   VALUE   "20080401".
           03  CONST-NSRYACT-MAX       PIC 9(05)   VALUE 5.
           03  CONST-RYOLIST-MAX       PIC 9(04)   VALUE   100.
      *    ÎáÏÂ£¶Ç¯ÅÙ°åÎÅ¶èÊ¬¥³¥á¥ó¥ÈÆÉ¤ßÂØ¤¨¤Î¤¿¤á¡¢ºÇ¿·¤Î¥³¡¼¥É¤òÀßÄê                       
           03  CONST-R0606-SUMON-SRYCD PIC X(09)    VALUE  "820000257".
      *
           03  CONST-ADLKBN-G.
               05  CONST-ADLTEN-MAX        PIC 9(03).
               05  CONST-ADLKBN-OCC    OCCURS  4.
                   07  CONST-ADLKBN-ALLKBN PIC 9(01).
                   07  CONST-ADLKBN-ZAINUM PIC 9(04).
                   07  CONST-ADLKBN-ID     PIC X(02).
                   07  CONST-ADLKBN-NAME   PIC X(100).
               05  CONST-ADLKBN-MAX        PIC 9(03).
      *
           COPY    "CPRYOYOCD.INC".
           COPY    "CPMEDKBN.INC".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK5014.INC".
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
           COPY    "CPTENSU.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑ¡¡ÎÎ°è
      *****************************************************************
      *
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSS011.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *
      *****************************************************************
      *    Ï¢Íí¡¡ÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHCN68S01.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   HCN68S01-AREA
                                   .
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ½é´ü¡¡½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  MEDINF-AREA
      *
           IF    ( HCN68S01-RANGE      =   SPACE )
               MOVE    "0"             TO  HCN68S01-RANGE
           END-IF
      *
           MOVE    6                   TO  CONST-ADLTEN-MAX
           MOVE    4                   TO  CONST-ADLKBN-MAX
      *
           MOVE    0                   TO  CONST-ADLKBN-ALLKBN (1)
           MOVE    1001                TO  CONST-ADLKBN-ZAINUM (1)
           MOVE    "a"                 TO  CONST-ADLKBN-ID     (1)
           MOVE    "£á¡¡¥Ù¥Ã¥É¤Î²ÄÆ°À­"
                                       TO  CONST-ADLKBN-NAME   (1)
      *
           MOVE    0                   TO  CONST-ADLKBN-ALLKBN (2)
           MOVE    1002                TO  CONST-ADLKBN-ZAINUM (2)
           MOVE    "b"                 TO  CONST-ADLKBN-ID     (2)
           MOVE    "£â¡¡°Ü¾è"          TO  CONST-ADLKBN-NAME   (2)
      *
           MOVE    0                   TO  CONST-ADLKBN-ALLKBN (3)
           MOVE    1003                TO  CONST-ADLKBN-ZAINUM (3)
           MOVE    "c"                 TO  CONST-ADLKBN-ID     (3)
           MOVE    "£ã¡¡¿©»ö"          TO  CONST-ADLKBN-NAME   (3)
      *
           MOVE    0                   TO  CONST-ADLKBN-ALLKBN (4)
           MOVE    1004                TO  CONST-ADLKBN-ZAINUM (4)
           MOVE    "d"                 TO  CONST-ADLKBN-ID     (4)
           MOVE    "£ä¡¡¥È¥¤¥ì¤Î»ÈÍÑ"  TO  CONST-ADLKBN-NAME   (4)
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç¡¡¡¡½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 210-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÊÔ½¸½èÍý
      *****************************************************************
       210-EDIT-SEC                SECTION.
      *
           PERFORM 2101-EDIT-NYUIN-DAY-SEC
      *
           PERFORM 2101-EDIT-RYOYO-DAY-SEC
      *
           PERFORM 2101-EDIT-RYOLST-SEC
      *
           IF    ( HCN68S01-RYOYO-FLG  =   1 )
               PERFORM 2101-EDIT-PTSTS-SEC
           END-IF
      *
           .
       210-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡´ü´ÖÊÔ½¸½èÍý
      *****************************************************************
       2101-EDIT-NYUIN-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           IF    ( FLG-NYUINACCT   =   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   MOVE    NACCT-DAY (IDX-DAY)
                                   TO  HCN68S01-COMB-DAY  (IDX-DAY)
      *
                   IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                       MOVE    1   TO  HCN68S01-NYUIN-DAY (IDX-DAY)
                       COMPUTE HCN68S01-NYUIN-NISSU
                           =   HCN68S01-NYUIN-NISSU    +   1
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       2101-EDIT-NYUIN-DAY-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ·¿Æþ±¡´ðËÜÎÁ»»Äê´ü´ÖÊÔ½¸½èÍý
      *****************************************************************
       2101-EDIT-RYOYO-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY35-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               INITIALIZE      WRK-RYOYO-DAY-G
      *
               PERFORM 21011-EDIT-RYOYO-DAY-SEC
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   IF    ( WRK-RYOYO-DAY (IDX-DAY) NOT =   ZERO )
                       MOVE    WRK-RYOYO-DAY (IDX-DAY)
                                       TO  HCN68S01-RYOYO-DAY (IDX-DAY)
                   END-IF
      *
                   IF    ( WRK-RYOYO-CD  (IDX-DAY) NOT =   SPACE )
                       MOVE    WRK-RYOYO-CD  (IDX-DAY)
                                       TO  HCN68S01-RYOYO-CD  (IDX-DAY)
                       MOVE    WRK-RYOYO-KHN (IDX-DAY)
                                       TO  HCN68S01-RYOYO-KHN (IDX-DAY)
                       MOVE    1       TO  HCN68S01-RYOYO-FLG
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACCT-KEY35-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2101-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ·¿Æþ±¡´ðËÜÎÁ»»Äê´ü´ÖÊÔ½¸½èÍý
      *****************************************************************
       21011-EDIT-RYOYO-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACT-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACT    NOT =   ZERO )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRY FROM    1   BY  1
                       UNTIL ( IDX-SRY >   CONST-NSRYACT-MAX )
                        OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
                   PERFORM 210111-RYOYOCD-CHK-SEC
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
           .
       21011-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ·¿Æþ±¡´ðËÜÎÁ¿ÇÎÅ¹Ô°Ù¥³¡¼¥É¥Á¥§¥Ã¥¯½èÍý
      *****************************************************************
       210111-RYOYOCD-CHK-SEC          SECTION.
      *
           IF    ( HCN68S01-SRYYMD     >=  CONST-R060601 )
            AND  ( SPA-HOSPSBT         =   1             )
             PERFORM VARYING IDXK    FROM    1   BY  1
                     UNTIL ( IDXK    >   CONST-R06-RYOYOCD-KHN-MAX )
                      OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
               PERFORM VARYING IDXR    FROM    1   BY  1
                 UNTIL ( IDXR    >   CONST-R06-RYOYOCD-CD-MAX  )
                  OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
                   IF    ( NSRY-SRYCD (IDX-SRY)    =   CONST-R06-RYOYOCD
                                                           (IDXK IDXR))
      *
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                               MOVE    IDXR
                                           TO  WRK-RYOYO-DAY (IDX-DAY)
                               MOVE    CONST-R06-RYOYOCD  (IDXK IDXR)
                                           TO  WRK-RYOYO-CD  (IDX-DAY)
                               MOVE    IDXK
                                           TO  WRK-RYOYO-KHN(IDX-DAY)
                           END-IF
      *
                       END-PERFORM
      *
                   END-IF
      *
               END-PERFORM
      *
             END-PERFORM
           END-IF  
      *
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   CONST-RYOYOCD-KHN-MAX )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
               PERFORM VARYING IDXR    FROM    1   BY  1
                   UNTIL ( IDXR    >   CONST-RYOYOCD-CD-MAX  )
                    OR   ( WRK-RYOYO-DAY-G NOT =   ZERO )
      *
                   IF    ( NSRY-SRYCD (IDX-SRY)    =   CONST-RYOYOCD
                                                            (IDXK IDXR))
      *
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                               MOVE    IDXR
                                           TO  WRK-RYOYO-DAY (IDX-DAY)
                               MOVE    CONST-RYOYOCD  (IDXK IDXR)
                                           TO  WRK-RYOYO-CD  (IDX-DAY)
                               MOVE    IDXK
                                           TO  WRK-RYOYO-KHN(IDX-DAY)
                           END-IF
      *
                       END-PERFORM
      *
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           IF    ( WRK-RYOYO-DAY-G         =   ZERO )
               MOVE    NSRY-SRYCD (IDX-SRY)    TO  WRK-TNS-SRYCD
               MOVE    HCN68S01-SRYYMD        TO  WRK-TNS-KJNYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( TNS-DATAKBN     =       1 )
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
      *
                       IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                           MOVE    -1      TO  WRK-RYOYO-DAY (IDX-DAY)
                       END-IF
      *
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       210111-RYOYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    £Á£Ä£Ì¶èÊ¬¡¦°åÎÅ¶èÊ¬¥ê¥¹¥ÈÊÔ½¸½èÍý
      *****************************************************************
       2101-EDIT-RYOLST-SEC            SECTION.
      *
           INITIALIZE                  HCN68S01-RYOLIST-AREA
      *
           PERFORM 21011-EDIT-MEDINF-SEC
      *
           MOVE    ZERO            TO  IDX-LST
      *
           PERFORM VARYING IDX0        FROM    1   BY  1
                   UNTIL ( IDX0    >   MEDINF-MAX )
                    OR   ( IDX-LST >=  CONST-RYOLIST-MAX )
      *
               MOVE    HCN68S01-SRYYM  TO  WRK-SRYYM
               MOVE    MEDINF-ZAINUM (IDX0)
                                       TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
                   COMPUTE IDX-LST =   IDX-LST +   1
      *
                   PERFORM 21011-EDIT-RYOLST-MEDKBN-SEC
      *
                   MOVE    RYOCOM-MONTH
                                   TO  HCN68S01-TDAYALL (IDX-LST)
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
      *
                       EVALUATE    TRUE
                       WHEN  ( HCN68S01-RANGE      =   "1" )
                         AND ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                       WHEN  ( HCN68S01-RANGE      =   "0" )
                         AND ( HCN68S01-NYUIN-DAY (IDX-DAY)
                                                   >   ZERO )
                           MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  HCN68S01-TDAY (IDX-LST IDX-DAY)
                       END-EVALUATE
      *
                   END-PERFORM
      *
               ELSE
                    COMPUTE IDX-LST =   IDX-LST +   1
                    PERFORM 21011-EDIT-RYOLST-MEDKBN-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0        FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-ADLKBN-MAX )
                    OR   ( IDX-LST >=  CONST-RYOLIST-MAX )
      *
               COMPUTE IDX-LST =   IDX-LST +   1
      *
               IF    ( IDX0    =   1 )
                   MOVE    IDX-LST TO  HCN68S01-RYOLIST-ADL-STIDX
               END-IF
      *
               MOVE    "1"         TO  HCN68S01-TZAISKBKBN
                                                   (IDX-LST)
      *
               MOVE    CONST-ADLKBN-ALLKBN (IDX0)
                                   TO  HCN68S01-TALLKBN (IDX-LST)
               MOVE    CONST-ADLKBN-ZAINUM (IDX0)
                                   TO  HCN68S01-TZAINUM (IDX-LST)
               MOVE    CONST-ADLKBN-ID     (IDX0)
                                   TO  HCN68S01-TID     (IDX-LST)
               MOVE    CONST-ADLKBN-NAME   (IDX0)
                                   TO  HCN68S01-TNAME   (IDX-LST)
      *
               MOVE    HCN68S01-SRYYM  TO  WRK-SRYYM
               MOVE    HCN68S01-TZAINUM (IDX-LST)
                                   TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
                   IF    ( RYOCOM-MONTH    >   ZERO )
                    AND  ( RYOCOM-DAY-TBL2 =   ZERO )
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           EVALUATE    TRUE
                           WHEN  ( HCN68S01-RANGE      =   "1" )
                             AND ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                           WHEN  ( HCN68S01-RANGE      =   "0" )
                             AND ( HCN68S01-NYUIN-DAY (IDX-DAY)
                                                   >   ZERO )
                               MOVE    RYOCOM-MONTH
                                   TO  HCN68S01-TDAY (IDX-LST IDX-DAY)
                           END-EVALUATE
      *
                       END-PERFORM
                   ELSE
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
      *
                           EVALUATE    TRUE
                           WHEN  ( HCN68S01-RANGE      =   "1" )
                             AND ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                           WHEN  ( HCN68S01-RANGE      =   "0" )
                             AND ( HCN68S01-NYUIN-DAY (IDX-DAY)
                                                   >   ZERO )
                               MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  HCN68S01-TDAY (IDX-LST IDX-DAY)
                           END-EVALUATE
      *
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( HCN68S01-SRYYMD >=  CONST-H300401 )
               PERFORM 21011-EDIT-RYOLST-DESIGNR-SEC
           END-IF
      *
           MOVE    IDX-LST         TO  HCN68S01-RYOLIST-MAX
      *
           .
       2101-EDIT-RYOLST-EXT.
           EXIT.
      *****************************************************************
      *    £Á£Ä£Ì¶èÊ¬¡¦°åÎÅ¶èÊ¬¥ê¥¹¥ÈÊÔ½¸½èÍý ¡Ê°åÎÅ¶èÊ¬ÊÔ½¸¡Ë
      *****************************************************************
       21011-EDIT-RYOLST-MEDKBN-SEC    SECTION.
      *
           MOVE    "2"             TO  HCN68S01-TZAISKBKBN  (IDX-LST)
           MOVE    MEDINF-MEDKBN (IDX0)
                                   TO  HCN68S01-TMEDKBN (IDX-LST)
           MOVE    MEDINF-MEDKBN1 (IDX0)
                                   TO  HCN68S01-TMEDKBN1 (IDX-LST)
           MOVE    MEDINF-DSPKBN  (IDX0)
                                   TO  HCN68S01-TDSPKBN (IDX-LST)
           MOVE    MEDINF-LIMIT  (IDX0)
                                   TO  HCN68S01-TLIMIT  (IDX-LST)
           MOVE    MEDINF-ALLKBN (IDX0)
                                   TO  HCN68S01-TALLKBN (IDX-LST)
           MOVE    MEDINF-ZAINUM (IDX0)
                                   TO  HCN68S01-TZAINUM (IDX-LST)
           MOVE    MEDINF-SRYCD  (IDX0)
                                   TO  HCN68S01-TSRYCD  (IDX-LST)
           IF   HCN68S01-SRYYM    >=  "202406"
             EVALUATE   MEDINF-SRYCD  (IDX0)
      *      ¡Ê£±¡Ë
             WHEN   "820000060"
                MOVE  "820000247"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²¡Ë
             WHEN   "820000248"
                MOVE  "820000248"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³¡Ë
             WHEN   "820000079"
                MOVE  "820000249"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£´¡Ë
             WHEN   "820000076"
                MOVE  "820000250"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£µ¡Ë
             WHEN   "820000077"
                MOVE  "820000251"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¶¡Ë
             WHEN   "820000078"
                MOVE  "820000252"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£·¡Ë
             WHEN   "820000080"
                MOVE  "820000253"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¸¡Ë
             WHEN   "820000083"
                MOVE  "820000254"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¹¡Ë
             WHEN   "820000087"
                MOVE  "820000255"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£°¡Ë
             WHEN   "820000090"
                MOVE  "820000256"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£±¡Ë
             WHEN   "820000057"
                MOVE  "820000257"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£²¡Ë
             WHEN   "820000066"
                MOVE  "820000258"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£³¡Ë
             WHEN   "820000058"
                MOVE  "820000259"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£´¡Ë¡ÊÎÅÍÜÉÂÅïÆþ±¡´ðËÜÎÁ¡Ë
             WHEN   "820000260"
                MOVE  "820000260"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£´¡Ë¡ÊÍ­¾²¿ÇÎÅ½êÎÅÍÜÉÂ¾²Æþ±¡´ðËÜÎÁ¡Ë
             WHEN   "820000294"
                MOVE  "820000294"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£µ¡Ë
             WHEN   "820000061"
                MOVE  "820000261"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£¶¡Ë
             WHEN   "820000062"
                MOVE  "820000262"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£·¡Ë
             WHEN   "820000063"
                MOVE  "820000263"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£¸¡Ë
             WHEN   "820000064"
                MOVE  "820000264"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£±£¹¡Ë
             WHEN   "820000065"
                MOVE  "820000265"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£°¡Ë
             WHEN   "820000067"
                MOVE  "820000266"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£±¡Ë
             WHEN   "820000068"
                MOVE  "820000267"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£²¡Ë
             WHEN   "820000069"
                MOVE  "820000268"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£³¡Ë
             WHEN   "820000070"
                MOVE  "820000269"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£´¡Ë
             WHEN   "820000071"
                MOVE  "820000270"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£µ¡Ë
             WHEN   "820000072"
                MOVE  "820000271"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£¶¡Ë
             WHEN   "820000073"
                MOVE  "820000272"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£·¡Ë
             WHEN   "820000092"
                MOVE  "820000273"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£¸¡Ë
             WHEN   "820000093"
                MOVE  "820000296"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£²£¹¡Ë
             WHEN   "820000074"
                MOVE  "820000274"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£°¡Ë
             WHEN   "820000085"
                MOVE  "820000275"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£±¡Ë
             WHEN   "820000241"
                MOVE  "820000297"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£²¡Ë
             WHEN   "820000276"
                MOVE  "820000276"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£³¡Ë
             WHEN   "820000086"
                MOVE  "820000277"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£´¡Ë
             WHEN   "820000075"
                MOVE  "820000278"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£µ¡Ë
             WHEN   "820000081"
                MOVE  "820000279"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£¶¡Ë
             WHEN   "820000082"
                MOVE  "820000280"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£·¡Ë
             WHEN   "820000084"
                MOVE  "820000281"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£¸¡Ë
             WHEN   "820000088"
                MOVE  "820000282"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£³£¹¡Ë
             WHEN   "820000089"
                MOVE  "820000283"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£´£°¡Ë
             WHEN   "820000091"
                MOVE  "820000284"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£´£±¡Ë
             WHEN   "820000232"
                MOVE  "820000285"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¸£±¡Ë
             WHEN   "201804001"
                MOVE  "820000287"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¸£²¡Ë
             WHEN   "201804002"
                MOVE  "820000288"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¸£³¡Ë
             WHEN   "201804003"
                MOVE  "820000295"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¸£´¡Ë
             WHEN   "201804004"
                MOVE  "820000289"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¸£µ¡Ë
             WHEN   "201804005"
                MOVE  "820000290"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¸£¶¡Ë
             WHEN   "201804006"
                MOVE  "820000291"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¸£·¡Ë
             WHEN   "820000243"
                MOVE  "820000292"  TO  HCN68S01-TSRYCD  (IDX-LST)
      *      ¡Ê£¹£±¡Ë
             WHEN   "820000159"
                MOVE  "820000293"  TO  HCN68S01-TSRYCD  (IDX-LST)
             END-EVALUATE
           END-IF
           MOVE    MEDINF-NAME   (IDX0)
                                   TO  HCN68S01-TNAME   (IDX-LST)
           MOVE    MEDINF-ID     (IDX0)
                                   TO  HCN68S01-TID     (IDX-LST)
      *
           .
       21011-EDIT-RYOLST-MEDKBN-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅ¶èÊ¬¾ðÊóÊÔ½¸½èÍý
      *****************************************************************
       21011-EDIT-MEDINF-SEC           SECTION.
      *
           EVALUATE    TRUE
               WHEN  ( HCN68S01-SRYYMD  >=  CONST-R060601 )
               IF      SPA-HOSPSBT     =   "1"
                   MOVE    1               TO  IDXX
               ELSE
                   MOVE    2               TO  IDXX
               END-IF              
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDR06-MAX )
                       OR    ( MEDR06-STYMD (IDXX IDX0)
                                       =   SPACE )
      *
                   IF    ( MEDR06-STYMD (IDXX IDX0) <= HCN68S01-SRYYMD )
                     AND ( MEDR06-EDYMD (IDXX IDX0) >= HCN68S01-SRYYMD )
                       COMPUTE MEDINF-MAX  =   MEDINF-MAX  +   1
                       MOVE    MEDINF-MAX  TO  IDXM
                       MOVE  MEDR06-MEDKBN (IDXX IDX0)
                                           TO  MEDINF-MEDKBN (IDXM)
                       MOVE  MEDR06-MEDKBN1(IDXX IDX0)
                                           TO  MEDINF-MEDKBN1(IDXM)
                       MOVE  MEDR06-DSPKBN (IDXX IDX0)
                                           TO  MEDINF-DSPKBN (IDXM)
                       MOVE  MEDR06-LIMIT (IDXX IDX0)
                                           TO  MEDINF-LIMIT  (IDXM)
                       MOVE  MEDR06-ALLKBN (IDXX IDX0)
                                           TO  MEDINF-ALLKBN (IDXM)
                       MOVE  MEDR06-ZAINUM (IDXX IDX0)
                                           TO  MEDINF-ZAINUM (IDXM)
                       MOVE  MEDR06-CDL    (IDXX IDX0)
                                           TO  MEDINF-SRYCD  (IDXM)
                       MOVE  MEDR06-MEIL   (IDXX IDX0)
                                           TO MEDINF-NAME (IDXM)
                       MOVE  MEDR06-ID     (IDXX IDX0)
                                           TO MEDINF-ID (IDXM)
      ***              MOVE  FUNCTION NUMVAL (MEDR06-ID (IDXX IDX0))
      ***                                  TO  MEDINF-NUM    (IDXM)
                   END-IF
               END-PERFORM
      *
               WHEN  ( HCN68S01-SRYYMD  >=  CONST-H200401 )
               MOVE    ZERO            TO  MEDINF-MAX
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDH20-MAX )
      *
                   IF    ( MEDH20-STYMD  (IDX0) <= HCN68S01-SRYYMD )
                     AND ( MEDH20-EDYMD  (IDX0) >= HCN68S01-SRYYMD )
      *
                       COMPUTE MEDINF-MAX  =   MEDINF-MAX  +   1
                       MOVE    MEDINF-MAX  TO  IDXM
      *
                       MOVE  MEDH20-MEDKBN (IDX0)
                                           TO  MEDINF-MEDKBN (IDXM)
                       MOVE  MEDH20-LIMIT (IDX0)
                                           TO  MEDINF-LIMIT  (IDXM)
                       MOVE  MEDH20-ALLKBN(IDX0)
                                           TO  MEDINF-ALLKBN (IDXM)
                       MOVE  MEDH20-ZAINUM(IDX0)
                                           TO  MEDINF-ZAINUM (IDXM)
                       MOVE  MEDH20-SRYCDL (IDX0)
                                           TO  MEDINF-SRYCD  (IDXM)
                       MOVE  MEDH20-SRYCDL (IDX0)
                                             TO WRK-TNS-SRYCD
                       MOVE  HCN68S01-SRYYMD TO WRK-TNS-KJNYMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
      * »ÃÄêÂÐ±þ
      *                MOVE    TNS-NAME    TO MEDINF-NAME (IDXM)
      *                IF    ( MEDINF-NAME (IDXM)  =   SPACE )
                           MOVE    MEDH20-MEIL (IDX0)
                                           TO MEDINF-NAME (IDXM)
      *                END-IF
                       MOVE    MEDH20-ID   (IDX0)
                                           TO MEDINF-ID (IDXM)
                   END-IF
               END-PERFORM
           END-EVALUATE
      *
           .
       21011-EDIT-MEDINF-EXT.
           EXIT.
      *****************************************************************
      *    £Á£Ä£Ì¶èÊ¬¡¦°åÎÅ¶èÊ¬¥ê¥¹¥ÈÊÔ½¸½èÍý ¡Ê£Ä£Å£Ó£É£Ç£Î£ÒÊÔ½¸¡Ë
      *****************************************************************
       21011-EDIT-RYOLST-DESIGNR-SEC    SECTION.
      *
           COMPUTE IDX-LST =   IDX-LST +   1
      *
           MOVE    IDX-LST TO  HCN68S01-RYOLIST-DESIGNR-STIDX
      *
           MOVE    "A"             TO  HCN68S01-TZAISKBKBN  (IDX-LST)
      *
           MOVE    DESIGNR-ALLKBN  TO  HCN68S01-TALLKBN (IDX-LST)
           MOVE    DESIGNR-ZAINUM  TO  HCN68S01-TZAINUM (IDX-LST)
           MOVE    DESIGNR-SRYCD   TO  HCN68S01-TSRYCD  (IDX-LST)
           MOVE    DESIGNR-ID      TO  HCN68S01-TID     (IDX-LST)
           MOVE    DESIGNR-MEI     TO  HCN68S01-TNAME   (IDX-LST)
      *
           MOVE    HCN68S01-SRYYM  TO  WRK-SRYYM
           MOVE    DESIGNR-ZAINUM  TO  WRK-ZAINUM
           PERFORM 900-RYOCOM-KEY-SEL-SEC
           IF    ( FLG-RYOCOM  =   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                       MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  HCN68S01-TDAY (IDX-LST IDX-DAY)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    HCN68S01-SRYYM  TO  WRK-SRYYM
           MOVE    DESIGNR-ZAINUM-EVAL
                                   TO  WRK-ZAINUM
           PERFORM 900-RYOCOM-KEY-SEL-SEC
           IF    ( FLG-RYOCOM  =   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                               NOT =   SPACE )
                       MOVE    RYOCOM-DAY (IDX-DAY)
                               TO  HCN68S01-DESIGNR-EVAL-DAY (IDX-DAY)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       21011-EDIT-RYOLST-DESIGNR-EXT.
           EXIT.
      *****************************************************************
      *    ´µ¼Ô¾õÂÖÊÔ½¸½èÍý
      *****************************************************************
       2101-EDIT-PTSTS-SEC            SECTION.
      *
           PERFORM 21011-EDIT-MEDADL-SEC
      *
           INITIALIZE                  SS011-AREA
           MOVE    HCN68S01-PTID   TO  SS011-PTID
           MOVE    HCN68S01-SRYYMD TO  SS011-SRYYM
           MOVE    1               TO  SS011-DAYFLG
           MOVE    1               TO  SS011-MEDADLFLG
           MOVE    HCN68S01-MEDKBN-SUMONFLG
                                   TO  SS011-SUMONFLG
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( HCN68S01-RYOYO-DAY (IDX-DAY) >  ZERO )
                   MOVE    HCN68S01-RYOYO-DAY (IDX-DAY)
                                   TO  SS011-DAY (IDX-DAY)
                   MOVE    HCN68S01-RYOYO-KHN (IDX-DAY)
                                   TO  SS011-KHN (IDX-DAY)
                   IF    ( HCN68S01-SRYYMD  >=  CONST-R060601 )
                   AND   ( SPA-HOSPSBT     =   "1"           )
      *                ¼À´µ¡¦¾õÂÖ¤Ë·¸¤ë°åÎÅ¶èÊ¬
                       EVALUATE    TRUE
                       WHEN  ( HCN68S01-MEDKBN-DAYALL (1)        =  1 )
                       WHEN  ( HCN68S01-MEDKBN-DAY (1 IDX-DAY)   =  1 )
                           MOVE    3   TO  SS011-MEDKBN (IDX-DAY)
                       WHEN  ( HCN68S01-MEDKBN-DAYALL (2)        =  1 )
                       WHEN  ( HCN68S01-MEDKBN-DAY (2 IDX-DAY)   =  1 )
                           MOVE    2   TO  SS011-MEDKBN (IDX-DAY)
                       WHEN    OTHER
                           MOVE    1   TO  SS011-MEDKBN (IDX-DAY)
                       END-EVALUATE
      *                ½èÃÖÅù¤Ë·¸¤ë°åÎÅ¶èÊ¬                 
                       EVALUATE    TRUE
                       WHEN  ( HCN68S01-MEDKBN-DAYALL (3)      =    1 )
                       WHEN  ( HCN68S01-MEDKBN-DAY (3 IDX-DAY)   =  1 )
                           MOVE    3   TO  SS011-MEDKBN1 (IDX-DAY)
                       WHEN  ( HCN68S01-MEDKBN-DAYALL (4)        =  1 )
                       WHEN  ( HCN68S01-MEDKBN-DAY (4 IDX-DAY)   =  1 )
                           MOVE    2   TO  SS011-MEDKBN1 (IDX-DAY)
                       WHEN    OTHER
                           MOVE    1   TO  SS011-MEDKBN1 (IDX-DAY)
                       END-EVALUATE
                   ELSE    
                       EVALUATE    TRUE
                       WHEN  (HCN68S01-MEDKBN-DAYALL (1)        =  1 )
                       WHEN  (HCN68S01-MEDKBN-DAY (1 IDX-DAY)   =  1 )
                           MOVE    3   TO  SS011-MEDKBN (IDX-DAY)
                       WHEN  (HCN68S01-MEDKBN-DAYALL (2)        =  1 )
                       WHEN  (HCN68S01-MEDKBN-DAY (2 IDX-DAY)   =  1 )
                           MOVE    2   TO  SS011-MEDKBN (IDX-DAY)
                       WHEN    OTHER
                           MOVE    1   TO  SS011-MEDKBN (IDX-DAY)
                       END-EVALUATE
                   END-IF
               END-IF    
      *
               COMPUTE SS011-ADLTEN (IDX-DAY)
                    =  HCN68S01-ADLTEN-DAY (IDX-DAY)
           END-PERFORM
      *
           CALL    "ORCSS011"  USING   SS011-AREA
                                       SPA-AREA
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                   MOVE    SS011-DAY  (IDX-DAY)
                                TO  HCN68S01-PTSTS-DAY (IDX-DAY)
                   MOVE    SS011-DAY2  (IDX-DAY)
                                TO  HCN68S01-PTSTS-DAY2(IDX-DAY)
                   MOVE    SS011-DAY3  (IDX-DAY)
                                TO  HCN68S01-PTSTS-DAY3(IDX-DAY)
                   MOVE    SS011-SRYCD (IDX-DAY)
                                TO  HCN68S01-PTSTS-SRYCD (IDX-DAY)
               END-IF
      *
           END-PERFORM
      *
           .
       2101-EDIT-PTSTS-EXT.
           EXIT.
      *****************************************************************
      *    °åÎÅ¶èÊ¬¡¢£Á£Ä£ÌÅÀ¿ôÊÔ½¸½èÍý
      *****************************************************************
       21011-EDIT-MEDADL-SEC               SECTION.
      *
           INITIALIZE                  HCN68S01-MEDKBN-DAY-AREA
                                       HCN68S01-ADLTEN-DAY-AREA
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   HCN68S01-RYOLIST-MAX )
      *
               IF    ( HCN68S01-TZAISKBKBN (IDX-LST)    =   "1" )
                   IF    ( HCN68S01-TDAYALL (IDX-LST)   >   ZERO )
                    AND  ( HCN68S01-TDAY-G  (IDX-LST)   =   ZERO )
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
                           IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                               COMPUTE HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   =   HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   +   HCN68S01-TDAYALL (IDX-LST)
                           END-IF
                       END-PERFORM
                   ELSE
                       PERFORM VARYING IDX-DAY FROM    1   BY  1
                               UNTIL ( IDX-DAY >   31 )
                           IF    ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                               COMPUTE HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   =   HCN68S01-ADLTEN-DAY (IDX-DAY)
                                   +   HCN68S01-TDAY  (IDX-LST IDX-DAY)
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
               MOVE    ZERO            TO  IDX-MED
      *        1:°åÎÅ¶èÊ¬£³ 2:°åÎÅ¶èÊ¬£²
      *        R0606°Ê¹ß¤ÎÎÅÍÜÉÂÅï  
      *        1:¼À´µ¡¦¾õÂÖ¤Ë·¸¤ë°åÎÅ¶èÊ¬£³
      *        2:¼À´µ¡¦¾õÂÖ¤Ë·¸¤ë°åÎÅ¶èÊ¬£²
      *        3:½èÃÖÅù¤Ë·¸¤ë°åÎÅ¶èÊ¬£³
      *        4:½èÃÖÅù¤Ë·¸¤ë°åÎÅ¶èÊ¬£²
               IF    ( HCN68S01-TZAISKBKBN (IDX-LST)   =   "2" )
                   IF    ( HCN68S01-TMEDKBN(IDX-LST)   =   3 )
                       MOVE    1       TO  IDX-MED
                   END-IF
                   IF    ( HCN68S01-TMEDKBN(IDX-LST)   =   2 )
                       MOVE    2       TO  IDX-MED
                   END-IF
                   IF    ( HCN68S01-SRYYMD >=  CONST-R060601 )
                   AND   ( SPA-HOSPSBT     =   "1"           )
      *                1:¼À´µ¡¦¾õÂÖ¤Ë·¸¤ë¡¡2:½èÃÖÅù¤Ë·¸¤ë¡ÊÎÅÍÜÉÂÅï¤Î¤ß¡Ë
                       IF    ( HCN68S01-TMEDKBN (IDX-LST)   =   3 )
                       AND   ( HCN68S01-TMEDKBN1(IDX-LST)   =   2 )
                           MOVE    3       TO  IDX-MED
                       END-IF
                       IF    ( HCN68S01-TMEDKBN(IDX-LST)    =   2 )
                       AND   ( HCN68S01-TMEDKBN1(IDX-LST)   =   2 )
                           MOVE    4       TO  IDX-MED
                       END-IF
                   END-IF    
                   IF    ( HCN68S01-SRYYMD >=  CONST-R060601 )
                   AND   ( SPA-HOSPSBT     =   "1"           )
                        IF    ( HCN68S01-TSRYCD (IDX-LST) 
                                           =   CONST-R0606-SUMON-SRYCD )
                       AND   ( HCN68S01-TDAYALL(IDX-LST)
                                           >   ZERO                    )
                           MOVE    1       TO  HCN68S01-MEDKBN-SUMONFLG
                       END-IF
                   END-IF        
               END-IF
      *
               IF    ( IDX-MED >       ZERO )
      *
                   IF    ( HCN68S01-TDAYALL(IDX-LST)    >   ZERO )
                    AND  ( HCN68S01-TDAY-G (IDX-LST)    =   ZERO )
                       MOVE    HCN68S01-TDAYALL(IDX-LST)
                                   TO HCN68S01-MEDKBN-DAYALL (IDX-MED)
      *                ¥¹¥â¥ó¤ò½ü¤¯¼À´µ¡¦¾õÂÖ¤Ë·¸¤ë°åÎÅ¶èÊ¬£³
                       IF    (  IDX-MED     =   1                      ) 
                       AND   ( HCN68S01-TSRYCD (IDX-LST) 
                                       NOT =   CONST-R0606-SUMON-SRYCD )
                           MOVE    HCN68S01-TDAYALL(IDX-LST)
                                       TO HCN68S01-MEDKBN-DAYALL (5)
                       END-IF
                   END-IF
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( HCN68S01-TDAY   (IDX-LST IDX-DAY)
                                           >   ZERO )
                        AND  ( HCN68S01-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                           MOVE    HCN68S01-TDAY   (IDX-LST IDX-DAY)
                               TO  HCN68S01-MEDKBN-DAY (IDX-MED IDX-DAY)
      *                    ¥¹¥â¥ó¤ò½ü¤¯¼À´µ¡¦¾õÂÖ¤Ë·¸¤ë°åÎÅ¶èÊ¬£³
                           IF    (  IDX-MED     =   1                  )
                           AND   ( HCN68S01-TSRYCD (IDX-LST) 
                                       NOT =   CONST-R0606-SUMON-SRYCD )
                             MOVE    HCN68S01-TDAY   (IDX-LST IDX-DAY)
                               TO  HCN68S01-MEDKBN-DAY (5 IDX-DAY)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       21011-EDIT-MEDADL-EXT.
           EXIT.
      *****************************************************************
      *    ½ªÎ»¡¡¡¡½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Þ¥·¥óÆüÉÕ¼èÆÀ¥µ¥Ö
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ¿ô»úÈ¾³Ñ½èÍý
      *****************************************************************
       800-NUM-HANKAKU-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-NUM-HAN
           EVALUATE    WRK-NUM-ZEN
           WHEN    "£°"
               MOVE    "0"         TO  WRK-NUM-HAN
           WHEN    "£±"
               MOVE    "1"         TO  WRK-NUM-HAN
           WHEN    "£²"
               MOVE    "2"         TO  WRK-NUM-HAN
           WHEN    "£³"
               MOVE    "3"         TO  WRK-NUM-HAN
           WHEN    "£´"
               MOVE    "4"         TO  WRK-NUM-HAN
           WHEN    "£µ"
               MOVE    "5"         TO  WRK-NUM-HAN
           WHEN    "£¶"
               MOVE    "6"         TO  WRK-NUM-HAN
           WHEN    "£·"
               MOVE    "7"         TO  WRK-NUM-HAN
           WHEN    "£¸"
               MOVE    "8"         TO  WRK-NUM-HAN
           WHEN    "£¹"
               MOVE    "9"         TO  WRK-NUM-HAN
           END-EVALUATE
      *
           .
      *
       800-NUM-HANKAKU-EXT.
           EXIT.
      *****************************************************************
      *    ¥·¥¹¥Æ¥à´ÉÍý¸¡º÷½èÍý
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE  NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    HCN68S01-PTID       TO  NACCT-PTID
           MOVE    HCN68S01-SRYYM      TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACCT-KEY35-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    HCN68S01-PTID       TO  NACCT-PTID
           MOVE    HCN68S01-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
      *
           .
       900-NYUINACCT-KEY35-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡²ñ·×FETCH½èÍý
      *****************************************************************
       900-NYUINACCT-KEY35-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  NYUINACCT-REC
           ELSE
               INITIALIZE              NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
      *
           .
       900-NYUINACCT-KEY35-FET-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°Ù¸¡º÷½èÍý
      *****************************************************************
       900-NYUINACT-KEY9-SEL-SEC       SECTION.
      *
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
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Æþ±¡¿ÇÎÅ¹Ô°ÙFETCH½èÍý
      *****************************************************************
       900-NYUINACT-KEY9-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¸¡º÷½èÍý(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    WRK-TNS-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               INITIALIZE              TNS-TENSU-REC
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿¥¯¥í¡¼¥º
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ÎÅÍÜ¥ì¥»¥×¥È¥³¥á¥ó¥È¥Þ¥¹¥¿¸¡º÷½èÍý
      *****************************************************************
       900-RYOCOM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    HCN68S01-PTID   TO  RYOCOM-PTID
           MOVE    WRK-SRYYM       TO  RYOCOM-SRYYM
           MOVE    WRK-ZAINUM      TO  RYOCOM-ZAINUM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-RYOCOM-KEY-SEL-EXT.
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
      *    £Ä£Â¸¡º÷½èÍý¡ÊFHETCH¤ò¹Ô¤ï¤Ê¤¤)
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
