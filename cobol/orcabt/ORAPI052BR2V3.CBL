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
       PROGRAM-ID.             ORAPI052BR2V3.
      *****************************************************************
      *  ¥·¥¹¥Æ¥àÌ¾        : £Ï£Ò£Ã£Á
      *  ¥µ¥Ö¥·¥¹¥Æ¥àÌ¾    : APIÏ¢·ÈÍÑ¥â¥¸¥å¡¼¥ë
      *  ¥³¥ó¥Ý¡¼¥Í¥ó¥ÈÌ¾  : ÅÀ¿ô¥Þ¥¹¥¿ÊÖµÑ
      *  ´ÉÍý¼Ô            : 
      *  ºîÀ®ÆüÉÕ    ºî¶È¼Ô        µ­½Ò
      *  17/06/30    NACL¡ÝÆ£¸¶    ¿·µ¬ºîÀ®
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  05.00.01    NACL-Æ£¸¶    18/09/05  £Ã£Ó£ÖÂÐ±þ
      *                                     °­À­¼ðáçÉÂÍýÁÈ¿¥É¸ËÜ²Ã»»¤ÎÄÉ²Ã
      *                                     Ãê½Ð¾ò·ï¤ÎÄÉ²Ã
      *  05.00.02    NACL-Æ£¸¶    19/02/05  £Ã£Ó£Ö¥Ç¡¼¥¿¤ÎºÇ¸å¤Î¥Ç¡¼¥¿¤Î
      *                                     ÊÔ½¸ÊýË¡¤Î½¤Àµ
      *  05.00.03    NACL-Æ£¸¶    20/03/25  Ê¿À®£³£²Ç¯£´·î²þÀµÂÐ±þ
      *                                     ÁÏ³°¸ÇÄê´ï²Ã»»
      *                                     Ä¶²»ÇÈÀÚºï´ï²Ã»»
      *                                     º¸¿´¼ªÊÄº¿½ÑÊ»»Ü¶èÊ¬¤ÎÄÉ²Ã
      *  05.01.01    ORCAMO       22/12/02  ¥Þ¥¹¥¿°ì³ç¼èÆÀ¤Î¹âÂ®²½ÂÐ±þ
      *  05.02.01    ORCAMO       23/06/29  ³°Íè´¶À÷¾ÉÂÐºö¸þ¾å²Ã»»Åù
      *                                     ¼ªÉ¡°ö¹¢²ÊÆýÍÄ»ù½èÃÖ²Ã»»
      *                                     ¼ªÉ¡°ö¹¢²Ê¾®»ù¹³¶ÝÌôÅ¬Àµ»ÈÍÑ»Ù±ç²Ã»»
      *                                     ÀÚ³«ÁÏ¶É½ê±¢°µÊÄº¿½èÃÖµ¡´ï²Ã»»
      *                                     ¤ÎÄÉ²Ã
      *  05.02.02    ORCAMO       24/05/02  ÎáÏÂ£¶Ç¯ÅÙ²þÀµÂÐ±þ
      *                                     CDKBN-KOUBAN¤ò£²·å¤«¤é£³·å¤ËÊÑ¹¹
      *                                     ´Ç¸î¿¦°÷½è¶ø²þÁ±É¾²ÁÎÁÅù
      *                                     ³°Íè¡¦ºßÂð¥Ù¡¼¥¹¥¢¥Ã¥×É¾²ÁÎÁ¡Ê£±¡Ë
      *                                     ³°Íè¡¦ºßÂð¥Ù¡¼¥¹¥¢¥Ã¥×É¾²ÁÎÁ¡Ê£²¡Ë
      *                                     ºÆÀ½Â¤Ã±²ó»ÈÍÑ°åÎÅµ¡´ï»ÈÍÑ²Ã»»
      *                                     Ìô²Á´ð½à¼ýºÜÇ¯·îÆü
      *                                     °ìÈÌÌ¾¥³¡¼¥É
      *                                     °ìÈÌÌ¾½èÊý²Ã»»ÂÐ¾Ý¶èÊ¬
      *                                     ¹³£È£É£ÖÌô¶èÊ¬
      *                                     ¤ÎÄÉ²Ã
      *  05.02.03    ORCAMO       24/09/02  Ä¹´ü¼ýºÜÉÊ´ØÏ¢
      *                                     ÁªÄêÎÅÍÜ¶èÊ¬¤ÎÄÉ²Ã
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    £Ã£Ó£Ö¥Õ¥¡¥¤¥ë
           SELECT  CSV-FILE        ASSIGN  CSVPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    £Ã£Ó£Ö¥Õ¥¡¥¤¥ë
       FD  CSV-FILE.
       01  CSV-REC                 PIC X(3100).
      *
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ë
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //CSVPARA//.
      *
      *    °ì»þ¥Õ¥¡¥¤¥ëÂÐ±þ
           COPY    "CPRECEERR.INC".
      *
      *    ¥¹¥ÑÎÎ°è
       01  STS-AREA.
           03  STS-CSV             PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    ¥Õ¥é¥°ÎÎ°è
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
      *    ¥«¥¦¥ó¥ÈÎÎ°è
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(10).
           03  CNT-OUT1            PIC 9(10).
           03  CNT-XMLCOUNT        PIC 9(06).
      *
      *    Åº»úÎÎ°è
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDW2                PIC 9(04).
      *
      *    ¥·¥¹¥Æ¥àÎÎ°è
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    ¥Ñ¥é¥á¥¿ÎÎ°è
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC X(02).
           03  WRK-PARA-TERMID     PIC X(64).
           03  WRK-PARA-BASE-YMD   PIC X(08).
           03  WRK-PARA-DATA-FORMAT
                                   PIC X(03).
           03  WRK-PARA-EFFECTIVE-MODE
                                   PIC X(05).
           03  WRK-PARA-ERRFILE    PIC X(64).
           03  WRK-PARA-SYORI      PIC X(01).
      *
      *    °ì»þÎÎ°è
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC X(04).
               05  WRK-SMM         PIC X(02).
               05  WRK-SDD         PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *
           03  WRK-THMS.
               05  WRK-THH         PIC X(02).
               05  WRK-TMM         PIC X(02).
               05  WRK-TSS         PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH     PIC X(02).
               05  WRK-HEN-T1      PIC X(01).
               05  WRK-HEN-TMM     PIC X(02).
               05  WRK-HEN-T2      PIC X(01).
               05  WRK-HEN-TSS     PIC X(02).
      *
      *     ±¦¤Å¤áÊÔ½¸ÍÑ
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    ½ÐÎÏ¥Ç¡¼¥¿ÊÔ½¸ÍÑ
           03  WRK-REC                 PIC X(3100).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
               05  WRK-MOJI-CHK        PIC 9(01).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).        
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    ¥¨¥é¡¼¥Õ¥¡¥¤¥ëÌ¾¾ÎÎÎ°è 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
       01  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
      *
      *    FULLNAMEÆâ¤ÎBASENAME³«»Ï°ÌÃÖ
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    ÅÀ¿ô¥Ç¡¼¥¿(1330 byte)
       01  TENSULST-REC.
           03  TENSULST-OCC          OCCURS   1000.
      *1-20
               05  TENSULST-SRYCD              PIC X(09).
               05  TENSULST-YUKOSTYMD          PIC X(10).
               05  TENSULST-YUKOEDYMD          PIC X(10).
               05  TENSULST-SRYKBN             PIC X(02).
               05  TENSULST-SRYSYUKBN          PIC X(03).
               05  TENSULST-YUKOKETA           PIC 9(02).
               05  TENSULST-NAME               PIC X(200).
               05  TENSULST-KANAYUKOKETA       PIC 9(02).
               05  TENSULST-KANANAME           PIC X(200).
               05  TENSULST-FORMALYUKOKETA     PIC 9(03).
               05  TENSULST-FORMALNAME         PIC X(400).
               05  TENSULST-TENSIKIBETU        PIC 9(01).
               05  TENSULST-TEN                PIC 999999999.99.
               05  TENSULST-TANICD             PIC X(03).
               05  TENSULST-TANIMOJISU         PIC 9(01).
               05  TENSULST-TANINAME           PIC X(24).
               05  TENSULST-DATAKBN            PIC 9(01).
               05  TENSULST-HKNTEKKBN          PIC 9(01).
               05  TENSULST-NYUGAITEKKBN       PIC 9(01).
               05  TENSULST-ROUTEKKBN          PIC 9(01).
      *21-40
               05  TENSULST-HOSPSRYKBN         PIC 9(01).
               05  TENSULST-OSINKBN            PIC 9(01).
               05  TENSULST-HOUKNSKBN          PIC 9(02).
               05  TENSULST-BYOKANRENKBN       PIC 9(02).
               05  TENSULST-SDOKNRYO           PIC 9(02).
               05  TENSULST-JITUDAY            PIC 9(01).
               05  TENSULST-DAYCNT             PIC 9(01).
               05  TENSULST-YKZKNRKBN          PIC 9(01).
               05  TENSULST-KZMCOMPSIKIBETU    PIC 9(01).
               05  TENSULST-KZMKGN             PIC 9(08).
               05  TENSULST-KZMJGN             PIC 9(08).
               05  TENSULST-KZM                PIC 9(08).
               05  TENSULST-KZMTEN             PIC 9999999.99.
               05  TENSULST-KZMERR             PIC 9(01).
               05  TENSULST-JGNCNT             PIC 9(03).
               05  TENSULST-JGNCNT1D           PIC 9(03).
               05  TENSULST-JGNCNT1W           PIC 9(03).
               05  TENSULST-JGNCNTETCM         PIC 9(02).
               05  TENSULST-JGNCNTETC          PIC 9(03).
               05  TENSULST-JGNCNTERR          PIC 9(01).
      *41-60
               05  TENSULST-CHUKSNCD           PIC X(04).
               05  TENSULST-CHUKSNTUBAN        PIC X(01).
               05  TENSULST-TSUSOKUAGE         PIC 9(01).
               05  TENSULST-KGNAGE             PIC X(02).
               05  TENSULST-JGNAGE             PIC X(02).
               05  TENSULST-TIMEKSNKBN         PIC 9(01).
               05  TENSULST-KIJUNTEIGENKBN     PIC 9(01).
               05  TENSULST-KIJUNTEIGENCD      PIC 9(04).
               05  TENSULST-LASERKBN           PIC 9(01).
               05  TENSULST-CHPMESUKSN         PIC 9(01).
               05  TENSULST-MICMESUKSN         PIC 9(01).
               05  TENSULST-DONORKBN           PIC 9(01).
               05  TENSULST-KNSJISKBN          PIC 9(01).
               05  TENSULST-KNSJISGRPKBN       PIC 9(02).
               05  TENSULST-TEIGENKBN          PIC 9(01).
               05  TENSULST-SEKIZUISOKUTEI     PIC 9(01).
               05  TENSULST-KEIBUJYUTU         PIC 9(01).
               05  TENSULST-AUTOHOUGO          PIC 9(01).
               05  TENSULST-GAIKANRIKBN        PIC 9(01).
               05  TENSULST-TUSOKUGAIKBN       PIC 9(01).
      *61-91
               05  TENSULST-HOUKATUTEIGENKBN   PIC 9(03).
               05  TENSULST-CHPNAISIKSN        PIC 9(01).
               05  TENSULST-AUTOFUNGO          PIC 9(01).
               05  TENSULST-CHIIKIKSN          PIC 9(01).
               05  TENSULST-BYOSYOKBN          PIC 9(01).
               05  TENSULST-CHPGYOKOSOTIKSN    PIC 9(01).
               05  TENSULST-SHORTSTAYOPE       PIC 9(01).
               05  TENSULST-BUIKBN             PIC 9(01).
               05  TENSULST-SANTEIRRKKBN       PIC 9(01).
               05  TENSULST-SSTKIJUNCDTBL      OCCURS 10.
                   07  TENSULST-SSTKIJUNCD         PIC 9(04).
               05  TENSULST-AGEKSNTBL          OCCURS 4.
                   07  TENSULST-AGEKSNKGN          PIC X(02).
                   07  TENSULST-AGEKSNJGN          PIC X(02).
                   07  TENSULST-AGEKSNSRYCD        PIC X(09).
      *92-110
               05  TENSULST-KENTAICOMMENT      PIC 9(01).
               05  TENSULST-NYUKHNKBN          PIC 9(02).
               05  TENSULST-NYUKHNKSNKBN       PIC 9(03).
               05  TENSULST-KANGOKSN           PIC 9(02).
               05  TENSULST-OLDTENSKB          PIC 9(01).
               05  TENSULST-OLDTEN             PIC 999999999.99.
               05  TENSULST-MADOKUKBN          PIC 9(01).
               05  TENSULST-SINKEIHAKAIKBN     PIC 9(01).
               05  TENSULST-SEIBUTUGAKUKBN     PIC 9(01).
               05  TENSULST-ZOEIZAIKBN         PIC 9(01).
               05  TENSULST-CSYYOURYO          PIC 9(05).
               05  TENSULST-YKZKBN             PIC 9(01).
               05  TENSULST-ZAIGATAKBN         PIC 9(01).
               05  TENSULST-KOUHATUKBN         PIC 9(01).
               05  TENSULST-LONGTOYOKBN        PIC 9(01).
               05  TENSULST-MEIUSESKB          PIC 9(01).
               05  TENSULST-TOKUKIZAIAGEKSNKBN PIC 9(01).
               05  TENSULST-SANSOKBN           PIC 9(01).
               05  TENSULST-TOKUKIZAISBT1      PIC 9(01).
      *111-130
               05  TENSULST-TOKUKIZAISBT2      PIC 9(01).
               05  TENSULST-JGNTEN             PIC 9(07).
               05  TENSULST-GAITENTTLKBN       PIC 9(03).
               05  TENSULST-NYUTENTTLKBN       PIC 9(03).
               05  TENSULST-CDKBN-KBN          PIC X(01).
               05  TENSULST-CDKBN-SYO          PIC 9(01).
               05  TENSULST-CDKBN-BU           PIC 9(02).
               05  TENSULST-CDKBN-KBNNUM       PIC 9(03).
               05  TENSULST-CDKBN-KBNNUM-EDA   PIC 9(02).
               05  TENSULST-CDKBN-KOUBAN       PIC 9(03).
               05  TENSULST-KOKUCHI-KBN        PIC X(01).
               05  TENSULST-KOKUCHI-SYO        PIC 9(01).
               05  TENSULST-KOKUCHI-BU         PIC 9(02).
               05  TENSULST-KOKUCHI-KBNNUM     PIC 9(03).
               05  TENSULST-KOKUCHI-KBNNUM-EDA PIC 9(02).
               05  TENSULST-KOKUCHI-KOUBAN     PIC 9(02).
               05  TENSULST-KOKUJISKBKBN1      PIC 9(01).
               05  TENSULST-KOKUJISKBKBN2      PIC 9(01).
               05  TENSULST-NAMECHGKBN         PIC 9(01).
               05  TENSULST-KANANAMECHGKBN     PIC 9(01).
      *131-157
               05  TENSULST-IDOKANREN          PIC X(09).
               05  TENSULST-KOUHYOJYUNNUM      PIC 9(09).
               05  TENSULST-YAKKAKJNCD         PIC X(12).
               05  TENSULST-SYUSAISKB          PIC 9(01).
               05  TENSULST-SYOMEIKANREN       PIC X(09).
               05  TENSULST-CHGYMD             PIC X(10).
               05  TENSULST-HAISIYMD           PIC X(10).
               05  TENSULST-KEIKASOCHIYMD      PIC X(10).
      ******** 05  TENSULST-TERMID             PIC X(16).
      ******** 05  TENSULST-OPID               PIC X(16).
               05  TENSULST-CREYMD             PIC X(10).
               05  TENSULST-UPYMD              PIC X(10).
               05  TENSULST-UPHMS              PIC X(08).
      *********05  TENSULST-HOSPNUM            PIC 9(02).
      *********05  TENSULST-MASTER-CLASS       PIC 9(01).
               05  TENSULST-GAZOOPESUP         PIC 9(01).
               05  TENSULST-IRYOKANSATUKBN     PIC 9(01).
               05  TENSULST-MASUISKBKBN        PIC 9(01).
               05  TENSULST-FUKUBIKUNAIKBN     PIC 9(01).
               05  TENSULST-FUKUBIKUKOTUKBN    PIC 9(01).
               05  TENSULST-TIMEMASUIKBN       PIC 9(01).
               05  TENSULST-DPCKBN             PIC 9(01).
               05  TENSULST-HISINSYUMONITORKSN PIC 9(01).
               05  TENSULST-TOUKETUHOZONKSN    PIC 9(01).
               05  TENSULST-KUBUNBANGOU        PIC X(30).
               05  TENSULST-ROSAIKBN           PIC 9(01).
               05  TENSULST-SISIKSN            PIC 9(01).
               05  TENSULST-AKUSEIBYORIKSN     PIC 9(01).
               05  TENSULST-SOGAIKOTEIKSN      PIC 9(01).
               05  TENSULST-CHPSESSAKUKSN      PIC 9(01).
               05  TENSULST-SASINJIHEISIKBN    PIC 9(01).
      *158-171         
               05  TENSULST-GAIKANSENSYOKSN    PIC 9(01).
               05  TENSULST-JIBINYUYOJIKSN     PIC 9(01).
               05  TENSULST-JIBISYONIKOKINKSN  PIC 9(01).
               05  TENSULST-SEKKAISOUKIKIKSN   PIC 9(01).
               05  TENSULST-KANGOHYOKA         PIC X(01).
               05  TENSULST-BASEUPHYOKA1       PIC X(01).
               05  TENSULST-BASEUPHYOKA2       PIC X(01).
               05  TENSULST-RSUDKSN            PIC 9(03).
               05  TENSULST-YAKKAKJNYMD        PIC X(08).
               05  TENSULST-GECODE             PIC X(12).
               05  TENSULST-IPNSHOHOKSN        PIC X(01).
               05  TENSULST-HIVKBN             PIC X(01).
               05  TENSULST-CYOKISYUSAI        PIC X(09).
               05  TENSULST-CYOKISENTEIKBN     PIC 9(01).
      *
       01  TITLE-01.
      *1-20
           03  FILLER                 PIC X(14)    VALUE
               "¿ÇÎÅ¹Ô°Ù¥³¡¼¥É".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "Í­¸ú³«»ÏÇ¯·îÆü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "Í­¸ú½ªÎ»Ç¯·îÆü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "¿ÇÎÅ¹Ô°Ù¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "¿ÇÎÅ¼ïÊÌ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "´Á»úÍ­¸ú·å¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "´Á»úÌ¾¾Î".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "¥«¥ÊÍ­¸ú·å¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "¥«¥ÊÌ¾¾Î".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "Àµ¼°Ì¾¾ÎÍ­¸ú·å¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "Àµ¼°Ì¾¾Î".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "ÅÀ¿ô¼±ÊÌ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(04)    VALUE
               "ÅÀ¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "Ã±°Ì¥³¡¼¥É".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "Ã±°ÌÊ¸»ú¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "Ã±°ÌÌ¾¾Î".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "¥Ç¡¼¥¿¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "ÊÝ¸±Å¬ÍÑ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "Æþ³°Å¬ÍÑ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "¸å´ü¹âÎð¼Ô°åÎÅÅ¬ÍÑ¶èÊ¬".
      *21-40
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "ÉÂ±¡¿ÇÎÅ½ê¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "±ý¿Ç¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "Êñ³çÂÐ¾Ý¸¡ºº".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "½ýÉÂÌ¾´ØÏ¢¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "»ØÆ³´ÉÍýÎÁ¡¢°å³Ø´ÉÍýÎÁ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(06)    VALUE
               "¼ÂÆü¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "Æü¿ô²ó¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "°åÌôÉÊ´ØÏ¢¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "¤­¤¶¤ßÃÍ·×»»¼±ÊÌ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "¤­¤¶¤ßÃÍ²¼¸ÂÃÍ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "¤­¤¶¤ßÃÍ¾å¸ÂÃÍ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "¤­¤¶¤ßÃÍ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "¤­¤¶¤ßÅÀ¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "¤­¤¶¤ßÃÍ¾å²¼¸Â¥¨¥é¡¼½èÍý".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "¾å¸Â²ó¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "¾å¸Â²ó¿ô£±Æü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "¾å¸Â²ó¿ô£±½µ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "¾å¸Â²ó¿ôÂ¾·î¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "¾å¸Â²ó¿ôÂ¾".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "¾å¸Â²ó¿ô¥¨¥é¡¼½èÍý".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *41-60
           03  FILLER                 PIC X(12)    VALUE
               "Ãí²Ã»»¥³¡¼¥É".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "Ãí²Ã»»ÄÌÈÖ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "ÄÌÂ§Ç¯Îð".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "¾å²¼¸ÂÇ¯Îð²¼¸ÂÇ¯Îð".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "¾å²¼¸ÂÇ¯Îð¾å¸ÂÇ¯Îð".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "»þ´Ö²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "´ð½àÉÔÅ¬¹çÄþ¸º¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(26)    VALUE
               "´ð½àÉÔÅ¬¹çÄþ¸ºÂÐ¾Ý»ÜÀß¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "½èÃÖÆýÍÄ»ù²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "¶ËÄã½ÐÀ¸ÂÎ½Å»ù²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "Æþ±¡´ðËÜÎÁÅù¸º»»ÂÐ¾Ý¼±ÊÌ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "¥É¥Ê¡¼Ê¬½¸·×¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "¸¡ººÅù¼Â»ÜÈ½ÃÇ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(26)    VALUE
               "¸¡ººÅù¼Â»ÜÈ½ÃÇ¥°¥ë¡¼¥×¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "Äþ¸ºÂÐ¾Ý¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(26)    VALUE
               "ÀÔ¿ñÍ¶È¯ÅÅ°ÌÂ¬ÄêÅù²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "ðôÉô³ÔÀ¶½ÑÊ»»Ü²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "¼«Æ°Ë¥¹ç´ï²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "³°Íè´ÉÍý²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(44)    VALUE
               "ÄÌÂ§²Ã»»ÂÐ¾Ý³°¶èÊ¬¡¢ÄÌÂ§²Ã»»½êÄêÅÀ¿ôÂÐ¾Ý¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *61-91
           03  FILLER                 PIC X(12)    VALUE
               "Êñ³çÄþ¸º¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "Ä¶²»ÇÈÆâ»ë¶À²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "¼«Æ°Ê­¹ç´ï²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "ÃÏ°è²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "ÉÂ¾²¿ô¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(28)    VALUE
               "Ä¶²»ÇÈ¶Å¸ÇÀÚ³«ÁõÃÖÅù²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "Ã»´üÂÚºß¼ê½Ñ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "Éô°Ì¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "»»ÄêÍúÎò¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£±".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£²".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£³".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£´".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£µ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£¶".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£·".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£¸".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£¹".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "»ÜÀß´ð½à¥³¡¼¥É£±£°".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ç¯Îð²Ã»»²¼¸ÂÇ¯Îð£±".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ç¯Îð²Ã»»¾å¸ÂÇ¯Îð£±".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "Ç¯Îð²Ã»»Ãí²Ã»»¿ÇÎÅ¹Ô°Ù¥³¡¼¥É£±".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ç¯Îð²Ã»»²¼¸ÂÇ¯Îð£²".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ç¯Îð²Ã»»¾å¸ÂÇ¯Îð£²".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "Ç¯Îð²Ã»»Ãí²Ã»»¿ÇÎÅ¹Ô°Ù¥³¡¼¥É£²".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ç¯Îð²Ã»»²¼¸ÂÇ¯Îð£³".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ç¯Îð²Ã»»¾å¸ÂÇ¯Îð£³".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "Ç¯Îð²Ã»»Ãí²Ã»»¿ÇÎÅ¹Ô°Ù¥³¡¼¥É£³".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ç¯Îð²Ã»»²¼¸ÂÇ¯Îð£´".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ç¯Îð²Ã»»¾å¸ÂÇ¯Îð£´".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "Ç¯Îð²Ã»»Ãí²Ã»»¿ÇÎÅ¹Ô°Ù¥³¡¼¥É£´".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *92-110
           03  FILLER                 PIC X(16)    VALUE
               "¸¡ÂÎ¸¡ºº¥³¥á¥ó¥È".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "Æþ±¡´ðËÜÎÁ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Æþ±¡´ðËÜÎÁ²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "´Ç¸î²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "µìÅÀ¿ô¼±ÊÌ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(06)    VALUE
               "µìÅÀ¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "ËãÆÇ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "¿À·ÐÇË²õºÞ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "À¸Êª³ØÅªÀ½ºÞ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "Â¤±ÆºÞ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "Ãí¼ÍÍÆÎÌ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "ÌôºÞ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "ºÞ·¿¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "¸åÈ¯°åÌôÉÊ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "Ä¹´üÅêÍ¿¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "Ì¾¾Î»ÈÍÑ¼±ÊÌ".
      *******         ¡Ö¼¾ÉÛÌô¶èÊ¬¡×¡ÖÁªÂò¼°¥³¥á¥ó¥È¼±ÊÌ¡×
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "ÆÃÄê´ïºàÇ¯Îð²Ã»»¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "»ÀÁÇÅù¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "ÆÃÄê´ïºà¼ïÊÌ£±".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *111-130
           03  FILLER                 PIC X(24)    VALUE
               "ÆÃÄê´ïºà¼ïÊÌ£²¡¢¾å¸Â²Á³Ê".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "¾å¸ÂÅÀ¿ô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "ÅÀ¿ôÍó½¸·×Àè¼±ÊÌ¡Ê³°Íè¡Ë".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "ÅÀ¿ôÍó½¸·×Àè¼±ÊÌ¡ÊÆþ±¡¡Ë".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "¥³¡¼¥ÉÉ½ÍÑ¶èÊ¬¡Ý¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "¥³¡¼¥ÉÉ½ÍÑ¶èÊ¬¡Ý¾Ï".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "¥³¡¼¥ÉÉ½ÍÑ¶èÊ¬¡ÝÉô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "¥³¡¼¥ÉÉ½ÍÑ¶èÊ¬¡Ý¶èÊ¬ÈÖ¹æ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "¥³¡¼¥ÉÉ½ÍÑ¶èÊ¬¡Ý¶èÊ¬ÈÖ¹æ¡Ý»ÞÈÖ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "¥³¡¼¥ÉÉ½ÍÑ¶èÊ¬¡Ý¹àÈÖ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "¹ðÃÎ¡¦ÄÌÃÎ´ØÏ¢ÈÖ¹æ¡Ý¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "¹ðÃÎ¡¦ÄÌÃÎ´ØÏ¢ÈÖ¹æ¡Ý¾Ï".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "¹ðÃÎ¡¦ÄÌÃÎ´ØÏ¢ÈÖ¹æ¡ÝÉô".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(28)    VALUE
               "¹ðÃÎ¡¦ÄÌÃÎ´ØÏ¢ÈÖ¹æ¡Ý¶èÊ¬ÈÖ¹æ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(34)    VALUE
               "¹ðÃÎ¡¦ÄÌÃÎ´ØÏ¢ÈÖ¹æ¡Ý¶èÊ¬ÈÖ¹æ¡Ý»ÞÈÖ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "¹ðÃÎ¡¦ÄÌÃÎ´ØÏ¢ÈÖ¹æ¡Ý¹àÈÖ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "¹ð¼¨Åù¼±ÊÌ¶èÊ¬¡Ê£±¡Ë".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "¹ð¼¨Åù¼±ÊÌ¶èÊ¬¡Ê£²¡Ë".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "´Á»úÌ¾¾ÎÊÑ¹¹¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "¥«¥ÊÌ¾¾ÎÊÑ¹¹¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *131-171
           03  FILLER                 PIC X(08)    VALUE
               "°ÛÆ°´ØÏ¢".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "¸øÉ½½ç½øÈÖ¹æ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "Ìô²Á´ð½à¥³¡¼¥É".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "¼ýºÜÊý¼°Åù¼±ÊÌ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "¾¦ÉÊÌ¾Åù´ØÏ¢".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "ÊÑ¹¹Ç¯·îÆü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "ÇÑ»ßÇ¯·îÆü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "·Ð²áÁ¼ÃÖÇ¯·îÆü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "ºîÀ®Ç¯·îÆü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "¹¹¿·Ç¯·îÆü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "¹¹¿·»þ´Ö".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "²èÁüÅù¼ê½Ñ»Ù±ç²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "°åÎÅ´Ñ»¡Ë¡ÂÐ¾Ý¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "Ëã¿ì¼±ÊÌ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "ÉûÉ¡¹Ð¼ê½ÑÍÑÆâ»ë¶À²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(34)    VALUE
               "ÉûÉ¡¹Ð¼ê½ÑÍÑ¹üÆðÉôÁÈ¿¥ÀÚ½üµ¡´ï²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ä¹»þ´ÖËã¿ì´ÉÍý²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "£Ä£Ð£ÃÅ¬ÍÑ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(32)    VALUE
               "Èó¿¯½±Åª·ì¹ÔÆ°ÂÖ¥â¥Ë¥¿¥ê¥ó¥°²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "Åà·ëÊÝÂ¸Æ±¼ïÁÈ¿¥²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "ÅÀ¿ôÉ½¶èÊ¬ÈÖ¹æ".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "Ï«ºÒ»»ÄêÉÔ²Ä¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "»Í»è²Ã»»¶èÊ¬¡ÊÏ«ºÒ¡Ë".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "°­À­¼ðáçÉÂÍýÁÈ¿¥É¸ËÜ²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "ÁÏ³°¸ÇÄê´ï²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "Ä¶²»ÇÈÀÚºï´ï²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "º¸¿´¼ªÊÄº¿½ÑÊ»»Ü¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "³°Íè´¶À÷¾ÉÂÐºö¸þ¾å²Ã»»Åù".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "¼ªÉ¡°ö¹¢²ÊÆýÍÄ»ù½èÃÖ²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(36)    VALUE
               "¼ªÉ¡°ö¹¢²Ê¾®»ù¹³¶ÝÌôÅ¬Àµ»ÈÍÑ»Ù±ç²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "ÀÚ³«ÁÏ¶É½ê±¢°µÊÄº¿½èÃÖµ¡´ï²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "´Ç¸î¿¦°÷½è¶ø²þÁ±É¾²ÁÎÁÅù".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(34)    VALUE
               "³°Íè¡¦ºßÂð¥Ù¡¼¥¹¥¢¥Ã¥×É¾²ÁÎÁ¡Ê£±¡Ë".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(34)    VALUE
               "³°Íè¡¦ºßÂð¥Ù¡¼¥¹¥¢¥Ã¥×É¾²ÁÎÁ¡Ê£²¡Ë".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "ºÆÀ½Â¤Ã±²ó»ÈÍÑ°åÎÅµ¡´ï»ÈÍÑ²Ã»»".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "Ìô²Á´ð½à¼ýºÜÇ¯·îÆü".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "°ìÈÌÌ¾¥³¡¼¥É".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "°ìÈÌÌ¾½èÊý²Ã»»ÂÐ¾Ý¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "¹³£È£É£ÖÌô¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "Ä¹´ü¼ýºÜÉÊ´ØÏ¢".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "ÁªÄêÎÅÍÜ¶èÊ¬".
           03  FILLER                 PIC X(01)    VALUE   X"0D".
      *
       01  WRK-CONS-AREA.
      *    ¶èÀÚ¤êÊ¸»ú
           03  WRK-KUGIRI             PIC X(01)    VALUE   ",".
      *    ²þ¹Ô¥³¡¼¥É
           03  WRK-KAIGYO             PIC X(01)    VALUE   X"0D".
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥ì¥¤¥¢¥¦¥È
      *****************************************************************
      *
      *    ÅÀ¿ô
           COPY    "CPTENSU.INC".
      *
      *    ¥¸¥ç¥Ö´ÉÍý¥Þ¥¹¥¿
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    ¥µ¥Ö¥×¥íÍÑÎÎ°è
      *****************************************************************
      *
      *    È¾³Ñ¥Á¥§¥Ã¥¯¥µ¥Ö
           COPY    "CPORCSKANACHK.INC".
      *
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ¥µ¥Ö
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ÆüÉÕÊÑ´¹¥µ¥Ö
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    °ì»þ¥Õ¥¡¥¤¥ëÌ¾¼èÆÀ
           COPY    "CPORCSGETTEMP.INC".
      *
      *    £Ä£Â¸¡º÷
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "MONBLOB".
           COPY    "XMLDATA.INC".
           COPY    "ORCA-BLOB".
      *
           COPY    "CPORCSAPIFILE.INC".
      *
      *****************************************************************
      *    Ï¢ÍíÎÎ°è
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    ½é´ü½èÍý
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
                                       CNT-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
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
                                               WRK-PARA-BASE-YMD
                                               WRK-PARA-DATA-FORMAT
                                               WRK-PARA-EFFECTIVE-MODE
                                               WRK-PARA-SYORI
           END-UNSTRING
      *
           DISPLAY "PARA-BASE-YMD      =" WRK-PARA-BASE-YMD
           DISPLAY "PARA-DATA-FORMAT   =" WRK-PARA-DATA-FORMAT
           DISPLAY "PARA-EFFECTIVE-MODE=" WRK-PARA-EFFECTIVE-MODE
           DISPLAY "PARA-SYORI-KBN     =" WRK-PARA-SYORI
           DISPLAY "LNK-PRTKANRI-SKYYMD=" LNK-PRTKANRI-SKYYMD
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "medication_masterlst.csv"
                                       TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  CSVPARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý³«»Ï½èÍý
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORAPI052BR2V3"     TO  JOB-PGID
           MOVE    "ÅÀ¿ô¥Þ¥¹¥¿ÊÖµÑ"    TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           IF      WRK-PARA-DATA-FORMAT
                                       =   "csv"
               OPEN    OUTPUT      CSV-FILE
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¼ç½èÍý
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *      
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           EVALUATE    WRK-PARA-SYORI
               WHEN    "1"  
                   MOVE    WRK-PARA-BASE-YMD   TO  TNS-UPYMD
                   MOVE    "key35"             TO  WRK-MCP-PATHNAME
               WHEN    "2"
                   MOVE    WRK-PARA-BASE-YMD   TO  TNS-YUKOEDYMD
                   MOVE    "key39"             TO  WRK-MCP-PATHNAME
           END-EVALUATE
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-N-SEC
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
      *    ÅÀ¿ô¥Þ¥¹¥¿ÊÔ½¸½èÍý
           PERFORM     UNTIL   FLG-TENSU  =   1
               MOVE    ZERO                TO  CNT-OUT1
      *
               MOVE    SPACE               TO  TENSULST-REC
      *
               PERFORM         UNTIL   FLG-TENSU   =   1
                               OR      CNT-OUT1    >=  1000
                   PERFORM 2001-TENSULST-HENSYU-SEC
               END-PERFORM
      *
               IF      WRK-PARA-DATA-FORMAT
                                       =   "csv"
                   PERFORM 2002-TENSULST-CSV-SEC
               ELSE
                   ADD     1               TO  CNT-XMLCOUNT
                   PERFORM 910-XMLEXPORT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿ÊÔ½¸½èÍý
      *****************************************************************
       2001-TENSULST-HENSYU-SEC          SECTION.
      *
           IF    ( WRK-PARA-DATA-FORMAT
                                       =   "csv" )
           AND   ( CNT-OUT             =   ZERO  )
               MOVE    TITLE-01            TO  CSV-REC
               WRITE   CSV-REC
           END-IF
      *
           ADD     1                   TO  CNT-OUT1
                                           CNT-OUT
           MOVE    CNT-OUT1            TO  IDX
      *
           INITIALIZE                      TENSULST-OCC (IDX)
      *
           MOVE    TNS-SRYCD           TO  TENSULST-SRYCD          (IDX)
           MOVE    TNS-YUKOSTYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-YUKOSTYMD      (IDX)
           MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-YUKOEDYMD      (IDX)
           MOVE    TNS-SRYKBN          TO  TENSULST-SRYKBN         (IDX)
           MOVE    TNS-SRYSYUKBN       TO  TENSULST-SRYSYUKBN      (IDX)
           MOVE    TNS-YUKOKETA        TO  TENSULST-YUKOKETA       (IDX)
           MOVE    TNS-NAME            TO  TENSULST-NAME           (IDX)
           MOVE    TNS-KANAYUKOKETA    TO  TENSULST-KANAYUKOKETA   (IDX)
           MOVE    TNS-KANANAME        TO  TENSULST-KANANAME       (IDX)
           MOVE    TNS-FORMALYUKOKETA  TO  TENSULST-FORMALYUKOKETA (IDX)
           MOVE    TNS-FORMALNAME      TO  TENSULST-FORMALNAME     (IDX)
           MOVE    TNS-TENSIKIBETU     TO  TENSULST-TENSIKIBETU    (IDX)
           MOVE    TNS-TEN             TO  TENSULST-TEN            (IDX)
           MOVE    TNS-TANICD          TO  TENSULST-TANICD         (IDX)
           MOVE    TNS-TANIMOJISU      TO  TENSULST-TANIMOJISU     (IDX)
           MOVE    TNS-TANINAME        TO  TENSULST-TANINAME       (IDX)
           MOVE    TNS-DATAKBN         TO  TENSULST-DATAKBN        (IDX)
           MOVE    TNS-HKNTEKKBN       TO  TENSULST-HKNTEKKBN      (IDX)
           MOVE    TNS-NYUGAITEKKBN    TO  TENSULST-NYUGAITEKKBN   (IDX)
           MOVE    TNS-ROUTEKKBN       TO  TENSULST-ROUTEKKBN      (IDX)
           MOVE    TNS-HOSPSRYKBN      TO  TENSULST-HOSPSRYKBN     (IDX)
           MOVE    TNS-OSINKBN         TO  TENSULST-OSINKBN        (IDX)
           MOVE    TNS-HOUKNSKBN       TO  TENSULST-HOUKNSKBN      (IDX)
           MOVE    TNS-BYOKANRENKBN    TO  TENSULST-BYOKANRENKBN   (IDX)
           MOVE    TNS-SDOKNRYO        TO  TENSULST-SDOKNRYO       (IDX)
           MOVE    TNS-JITUDAY         TO  TENSULST-JITUDAY        (IDX)
           MOVE    TNS-DAYCNT          TO  TENSULST-DAYCNT         (IDX)
           MOVE    TNS-YKZKNRKBN       TO  TENSULST-YKZKNRKBN      (IDX)
           MOVE    TNS-KZMCOMPSIKIBETU TO  TENSULST-KZMCOMPSIKIBETU(IDX)
           MOVE    TNS-KZMKGN          TO  TENSULST-KZMKGN         (IDX)
           MOVE    TNS-KZMJGN          TO  TENSULST-KZMJGN         (IDX)
           MOVE    TNS-KZM             TO  TENSULST-KZM            (IDX)
           MOVE    TNS-KZMTEN          TO  TENSULST-KZMTEN         (IDX)
           MOVE    TNS-KZMERR          TO  TENSULST-KZMERR         (IDX)
           MOVE    TNS-JGNCNT          TO  TENSULST-JGNCNT         (IDX)
           MOVE    TNS-JGNCNT1D        TO  TENSULST-JGNCNT1D       (IDX)
           MOVE    TNS-JGNCNT1W        TO  TENSULST-JGNCNT1W       (IDX)
           MOVE    TNS-JGNCNTETCM      TO  TENSULST-JGNCNTETCM     (IDX)
           MOVE    TNS-JGNCNTETC       TO  TENSULST-JGNCNTETC      (IDX)
           MOVE    TNS-JGNCNTERR       TO  TENSULST-JGNCNTERR      (IDX)
           MOVE    TNS-CHUKSNCD        TO  TENSULST-CHUKSNCD       (IDX)
           MOVE    TNS-CHUKSNTUBAN     TO  TENSULST-CHUKSNTUBAN    (IDX)
           MOVE    TNS-TSUSOKUAGE      TO  TENSULST-TSUSOKUAGE     (IDX)
           MOVE    TNS-KGNAGE          TO  TENSULST-KGNAGE         (IDX)
           MOVE    TNS-JGNAGE          TO  TENSULST-JGNAGE         (IDX)
           MOVE    TNS-TIMEKSNKBN      TO  TENSULST-TIMEKSNKBN     (IDX)
           MOVE    TNS-KIJUNTEIGENKBN  TO  TENSULST-KIJUNTEIGENKBN (IDX)
           MOVE    TNS-KIJUNTEIGENCD   TO  TENSULST-KIJUNTEIGENCD  (IDX)
           MOVE    TNS-LASERKBN        TO  TENSULST-LASERKBN       (IDX)
           MOVE    TNS-CHPMESUKSN      TO  TENSULST-CHPMESUKSN     (IDX)
           MOVE    TNS-MICMESUKSN      TO  TENSULST-MICMESUKSN     (IDX)
           MOVE    TNS-DONORKBN        TO  TENSULST-DONORKBN       (IDX)
           MOVE    TNS-KNSJISKBN       TO  TENSULST-KNSJISKBN      (IDX)
           MOVE    TNS-KNSJISGRPKBN    TO  TENSULST-KNSJISGRPKBN   (IDX)
           MOVE    TNS-TEIGENKBN       TO  TENSULST-TEIGENKBN      (IDX)
           MOVE    TNS-SEKIZUISOKUTEI  TO  TENSULST-SEKIZUISOKUTEI (IDX)
           MOVE    TNS-KEIBUJYUTU      TO  TENSULST-KEIBUJYUTU     (IDX)
           MOVE    TNS-AUTOHOUGO       TO  TENSULST-AUTOHOUGO      (IDX)
           MOVE    TNS-GAIKANRIKBN     TO  TENSULST-GAIKANRIKBN    (IDX)
           MOVE    TNS-TUSOKUGAIKBN    TO  TENSULST-TUSOKUGAIKBN   (IDX)
           MOVE    TNS-HOUKATUTEIGENKBN
                                       TO  TENSULST-HOUKATUTEIGENKBN
                                                                   (IDX)
           MOVE    TNS-CHPNAISIKSN     TO  TENSULST-CHPNAISIKSN    (IDX)
           MOVE    TNS-AUTOFUNGO       TO  TENSULST-AUTOFUNGO      (IDX)
           MOVE    TNS-CHIIKIKSN       TO  TENSULST-CHIIKIKSN      (IDX)
           MOVE    TNS-BYOSYOKBN       TO  TENSULST-BYOSYOKBN      (IDX)
           MOVE    TNS-CHPGYOKOSOTIKSN TO  TENSULST-CHPGYOKOSOTIKSN(IDX)
           MOVE    TNS-SHORTSTAYOPE    TO  TENSULST-SHORTSTAYOPE   (IDX)
           MOVE    TNS-BUIKBN          TO  TENSULST-BUIKBN         (IDX)
           MOVE    TNS-SANTEIRRKKBN    TO  TENSULST-SANTEIRRKKBN   (IDX)
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       10
               MOVE    TNS-SSTKIJUNCD (IDY)
                                       TO  TENSULST-SSTKIJUNCD (IDX IDY)
           END-PERFORM
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
               MOVE    TNS-AGEKSNKGN (IDY)
                                       TO  TENSULST-AGEKSNKGN  (IDX IDY)
               MOVE    TNS-AGEKSNJGN (IDY)
                                       TO  TENSULST-AGEKSNJGN  (IDX IDY)
               MOVE    TNS-AGEKSNSRYCD (IDY)
                                       TO  TENSULST-AGEKSNSRYCD(IDX IDY)
           END-PERFORM
           MOVE    TNS-KENTAICOMMENT   TO  TENSULST-KENTAICOMMENT  (IDX)
           MOVE    TNS-NYUKHNKBN       TO  TENSULST-NYUKHNKBN      (IDX)
           MOVE    TNS-NYUKHNKSNKBN    TO  TENSULST-NYUKHNKSNKBN   (IDX)
           MOVE    TNS-KANGOKSN        TO  TENSULST-KANGOKSN       (IDX)
           MOVE    TNS-OLDTENSKB       TO  TENSULST-OLDTENSKB      (IDX)
           MOVE    TNS-OLDTEN          TO  TENSULST-OLDTEN         (IDX)
           MOVE    TNS-MADOKUKBN       TO  TENSULST-MADOKUKBN      (IDX)
           MOVE    TNS-SINKEIHAKAIKBN  TO  TENSULST-SINKEIHAKAIKBN (IDX)
           MOVE    TNS-SEIBUTUGAKUKBN  TO  TENSULST-SEIBUTUGAKUKBN (IDX)
           MOVE    TNS-ZOEIZAIKBN      TO  TENSULST-ZOEIZAIKBN     (IDX)
           MOVE    TNS-CSYYOURYO       TO  TENSULST-CSYYOURYO      (IDX)
           MOVE    TNS-YKZKBN          TO  TENSULST-YKZKBN         (IDX)
           MOVE    TNS-ZAIGATAKBN      TO  TENSULST-ZAIGATAKBN     (IDX)
           MOVE    TNS-KOUHATUKBN      TO  TENSULST-KOUHATUKBN     (IDX)
           MOVE    TNS-LONGTOYOKBN     TO  TENSULST-LONGTOYOKBN    (IDX)
           MOVE    TNS-MEIUSESKB       TO  TENSULST-MEIUSESKB      (IDX)
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  TENSULST-TOKUKIZAIAGEKSNKBN
                                                                   (IDX)
           MOVE    TNS-SANSOKBN        TO  TENSULST-SANSOKBN       (IDX)
           MOVE    TNS-TOKUKIZAISBT1   TO  TENSULST-TOKUKIZAISBT1  (IDX)
           MOVE    TNS-TOKUKIZAISBT2   TO  TENSULST-TOKUKIZAISBT2  (IDX)
           MOVE    TNS-JGNTEN          TO  TENSULST-JGNTEN         (IDX)
           MOVE    TNS-GAITENTTLKBN    TO  TENSULST-GAITENTTLKBN   (IDX)
           MOVE    TNS-NYUTENTTLKBN    TO  TENSULST-NYUTENTTLKBN   (IDX)
           MOVE    TNS-CDKBN-KBN       TO  TENSULST-CDKBN-KBN      (IDX)
           MOVE    TNS-CDKBN-SYO       TO  TENSULST-CDKBN-SYO      (IDX)
           MOVE    TNS-CDKBN-BU        TO  TENSULST-CDKBN-BU       (IDX)
           MOVE    TNS-CDKBN-KBNNUM    TO  TENSULST-CDKBN-KBNNUM   (IDX)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  TENSULST-CDKBN-KBNNUM-EDA
                                                                   (IDX)
           MOVE    TNS-CDKBN-KOUBAN    TO  TENSULST-CDKBN-KOUBAN   (IDX)
           MOVE    TNS-KOKUCHI-KBN     TO  TENSULST-KOKUCHI-KBN    (IDX)
           MOVE    TNS-KOKUCHI-SYO     TO  TENSULST-KOKUCHI-SYO    (IDX)
           MOVE    TNS-KOKUCHI-BU      TO  TENSULST-KOKUCHI-BU     (IDX)
           MOVE    TNS-KOKUCHI-KBNNUM  TO  TENSULST-KOKUCHI-KBNNUM (IDX)
           MOVE    TNS-KOKUCHI-KBNNUM-EDA
                                       TO  TENSULST-KOKUCHI-KBNNUM-EDA
                                                                   (IDX)
           MOVE    TNS-KOKUCHI-KOUBAN  TO  TENSULST-KOKUCHI-KOUBAN (IDX)
           MOVE    TNS-KOKUJISKBKBN1   TO  TENSULST-KOKUJISKBKBN1  (IDX)
           MOVE    TNS-KOKUJISKBKBN2   TO  TENSULST-KOKUJISKBKBN2  (IDX)
           MOVE    TNS-NAMECHGKBN      TO  TENSULST-NAMECHGKBN     (IDX)
           MOVE    TNS-KANANAMECHGKBN  TO  TENSULST-KANANAMECHGKBN (IDX)
           MOVE    TNS-IDOKANREN       TO  TENSULST-IDOKANREN      (IDX)
           MOVE    TNS-KOUHYOJYUNNUM   TO  TENSULST-KOUHYOJYUNNUM  (IDX)
           MOVE    TNS-YAKKAKJNCD      TO  TENSULST-YAKKAKJNCD     (IDX)
           MOVE    TNS-SYUSAISKB       TO  TENSULST-SYUSAISKB      (IDX)
           MOVE    TNS-SYOMEIKANREN    TO  TENSULST-SYOMEIKANREN   (IDX)
           MOVE    TNS-CHGYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-CHGYMD         (IDX)
           MOVE    TNS-HAISIYMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-HAISIYMD       (IDX)
           MOVE    TNS-KEIKASOCHIYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-KEIKASOCHIYMD  (IDX)
      ***  MOVE    TNS-TERMID          TO  TENSULST-TERMID         (IDX)
      ***  MOVE    TNS-OPID            TO  TENSULST-OPID           (IDX)
           MOVE    TNS-CREYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-CREYMD         (IDX)
           MOVE    TNS-UPYMD           TO  WRK-SYMD
           MOVE    TNS-UPHMS           TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-UPYMD          (IDX)
           MOVE    WRK-HEN-TIME        TO  TENSULST-UPHMS          (IDX)
      *****MOVE    TNS-MASTER-CLASS    TO  TENSULST-MASTER-CLASS   (IDX)
           MOVE    TNS-GAZOOPESUP      TO  TENSULST-GAZOOPESUP     (IDX)
           MOVE    TNS-IRYOKANSATUKBN  TO  TENSULST-IRYOKANSATUKBN (IDX)
           MOVE    TNS-MASUISKBKBN     TO  TENSULST-MASUISKBKBN    (IDX)
           MOVE    TNS-FUKUBIKUNAIKBN  TO  TENSULST-FUKUBIKUNAIKBN (IDX)
           MOVE    TNS-FUKUBIKUKOTUKBN TO  TENSULST-FUKUBIKUKOTUKBN(IDX)
           MOVE    TNS-TIMEMASUIKBN    TO  TENSULST-TIMEMASUIKBN   (IDX)
           MOVE    TNS-DPCKBN          TO  TENSULST-DPCKBN         (IDX)
           MOVE    TNS-HISINSYUMONITORKSN
                                       TO  TENSULST-HISINSYUMONITORKSN
                                                                   (IDX)
           MOVE    TNS-TOUKETUHOZONKSN TO  TENSULST-TOUKETUHOZONKSN(IDX)
           MOVE    TNS-KUBUNBANGOU     TO  TENSULST-KUBUNBANGOU    (IDX)
           MOVE    TNS-ROSAIKBN        TO  TENSULST-ROSAIKBN       (IDX)
           MOVE    TNS-SISIKSN         TO  TENSULST-SISIKSN        (IDX)
           MOVE    TNS-AKUSEIBYORIKSN  TO  TENSULST-AKUSEIBYORIKSN (IDX)
           MOVE    TNS-SOGAIKOTEIKSN   TO  TENSULST-SOGAIKOTEIKSN  (IDX)
           MOVE    TNS-CHPSESSAKUKSN   TO  TENSULST-CHPSESSAKUKSN  (IDX)
           MOVE    TNS-SASINJIHEISIKBN TO  TENSULST-SASINJIHEISIKBN(IDX)
           MOVE    TNS-GAIKANSENSYOKSN TO  TENSULST-GAIKANSENSYOKSN(IDX)
           MOVE    TNS-JIBINYUYOJIKSN  TO  TENSULST-JIBINYUYOJIKSN (IDX)
           MOVE    TNS-JIBISYONIKOKINKSN
                                       TO  TENSULST-JIBISYONIKOKINKSN
                                                                   (IDX)
           MOVE    TNS-SEKKAISOUKIKIKSN
                                       TO  TENSULST-SEKKAISOUKIKIKSN
                                                                   (IDX)
           MOVE    TNS-KANGOHYOKA      TO  TENSULST-KANGOHYOKA     (IDX)
           MOVE    TNS-BASEUPHYOKA1    TO  TENSULST-BASEUPHYOKA1   (IDX)
           MOVE    TNS-BASEUPHYOKA2    TO  TENSULST-BASEUPHYOKA2   (IDX)
           MOVE    TNS-RSUDKSN         TO  TENSULST-RSUDKSN        (IDX)
           MOVE    TNS-YAKKAKJNYMD     TO  TENSULST-YAKKAKJNYMD    (IDX)
           MOVE    TNS-GECODE          TO  TENSULST-GECODE         (IDX)
           MOVE    TNS-IPNSHOHOKSN     TO  TENSULST-IPNSHOHOKSN    (IDX)
           MOVE    TNS-HIVKBN          TO  TENSULST-HIVKBN         (IDX)
           MOVE    TNS-CYOKISYUSAI     TO  TENSULST-CYOKISYUSAI    (IDX)
           MOVE    TNS-CYOKISENTEIKBN  TO  TENSULST-CYOKISENTEIKBN (IDX)
      *

           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-TENSU-READ-N-SEC
           .
       2001-TENSULST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿£Ã£Ó£ÖÊÔ½¸½èÍý
      *****************************************************************
       2002-TENSULST-CSV-SEC       SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       CNT-OUT1
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
      *
               MOVE    TENSULST-SRYCD      (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YUKOSTYMD  (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YUKOEDYMD  (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SRYKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SRYSYUKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YUKOKETA   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NAME       (IDX)
                                           TO  WRK-DATA
               MOVE    200                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANAYUKOKETA   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANANAME   (IDX)
                                           TO  WRK-DATA
               MOVE    200                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-FORMALYUKOKETA (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-FORMALNAME (IDX)
                                           TO  WRK-DATA
               MOVE    400                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TENSIKIBETU    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TEN        (IDX)
                                           TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TANICD     (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TANIMOJISU (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TANINAME   (IDX)
                                           TO  WRK-DATA
               MOVE    24                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-DATAKBN    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HKNTEKKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NYUGAITEKKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-ROUTEKKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *21-40
               MOVE    TENSULST-HOSPSRYKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-OSINKBN    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HOUKNSKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-BYOKANRENKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SDOKNRYO   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JITUDAY    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-DAYCNT     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YKZKNRKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMCOMPSIKIBETU     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMKGN     (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMJGN     (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZM        (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMTEN     (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMERR     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNT     (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNT1D   (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNT1W   (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNTETCM (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNTETC  (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNTERR  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *41-60
               MOVE    TENSULST-CHUKSNCD   (IDX)
                                           TO  WRK-DATA
               MOVE    4                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHUKSNTUBAN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TSUSOKUAGE (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KGNAGE     (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNAGE     (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TIMEKSNKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KIJUNTEIGENKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KIJUNTEIGENCD       (IDX)
                                           TO  WRK-DATA
               MOVE    4                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-LASERKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHPMESUKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-MICMESUKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-DONORKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KNSJISKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KNSJISGRPKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TEIGENKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SEKIZUISOKUTEI      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KEIBUJYUTU (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-AUTOHOUGO  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-GAIKANRIKBN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TUSOKUGAIKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *61-91
               MOVE    TENSULST-HOUKATUTEIGENKBN    (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHPNAISIKSN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-AUTOFUNGO  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHIIKIKSN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-BYOSYOKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHPGYOKOSOTIKSN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SHORTSTAYOPE        (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-BUIKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SANTEIRRKKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL    IDY       >   10
                   MOVE    TENSULST-SSTKIJUNCD     (IDX IDY)
                                               TO  WRK-DATA
                   MOVE    4                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
               END-PERFORM
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL    IDY       >   4
                   MOVE    TENSULST-AGEKSNKGN      (IDX IDY)
                                               TO  WRK-DATA
                   MOVE    2                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
                   MOVE    TENSULST-AGEKSNJGN      (IDX IDY)
                                               TO  WRK-DATA
                   MOVE    2                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
                   MOVE    TENSULST-AGEKSNSRYCD    (IDX IDY)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
               END-PERFORM
      *92-110
               MOVE    TENSULST-KENTAICOMMENT   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NYUKHNKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NYUKHNKSNKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANGOKSN   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-OLDTENSKB  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-OLDTEN     (IDX)
                                           TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-MADOKUKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SINKEIHAKAIKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SEIBUTUGAKUKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-ZOEIZAIKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CSYYOURYO  (IDX)
                                           TO  WRK-DATA
               MOVE    5                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YKZKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-ZAIGATAKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOUHATUKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-LONGTOYOKBN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-MEIUSESKB  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TOKUKIZAIAGEKSNKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SANSOKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TOKUKIZAISBT1       (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *111-130
               MOVE    TENSULST-TOKUKIZAISBT2       (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNTEN     (IDX)
                                           TO  WRK-DATA
               MOVE    7                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-GAITENTTLKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NYUTENTTLKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-KBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-SYO  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-BU   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-KBNNUM        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-KBNNUM-EDA    (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-KOUBAN        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-KBN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-SYO(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-BU (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-KBNNUM      (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-KBNNUM-EDA  (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-KOUBAN      (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUJISKBKBN1       (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUJISKBKBN2       (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NAMECHGKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANANAMECHGKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *131-157
               MOVE    TENSULST-IDOKANREN  (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOUHYOJYUNNUM       (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YAKKAKJNCD (IDX)
                                           TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SYUSAISKB  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SYOMEIKANREN        (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHGYMD     (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HAISIYMD   (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KEIKASOCHIYMD       (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CREYMD     (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-UPYMD      (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-UPHMS      (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-GAZOOPESUP (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-IRYOKANSATUKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-MASUISKBKBN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-FUKUBIKUNAIKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-FUKUBIKUKOTUKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TIMEMASUIKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-DPCKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HISINSYUMONITORKSN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TOUKETUHOZONKSN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KUBUNBANGOU(IDX)
                                           TO  WRK-DATA
               MOVE    30                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-ROSAIKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SISIKSN    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-AKUSEIBYORIKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SOGAIKOTEIKSN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHPSESSAKUKSN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SASINJIHEISIKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-GAIKANSENSYOKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JIBINYUYOJIKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JIBISYONIKOKINKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SEKKAISOUKIKIKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANGOHYOKA (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-BASEUPHYOKA1 (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-BASEUPHYOKA2 (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-RSUDKSN (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YAKKAKJNYMD (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-GECODE (IDX)
                                           TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-IPNSHOHOKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HIVKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CYOKISYUSAI (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CYOKISENTEIKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KAIGYO                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
      *
               MOVE    WRK-REC             TO  CSV-REC
               WRITE   CSV-REC
      *
               IF      STS-CSV             =   "00"
                   CONTINUE
               ELSE
                   CALL "coblog" USING   "file write err " CSVPARA
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    CSVPARA             TO  WRK-ERR-FILE-NM
                   MOVE    STS-CSV             TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
           END-PERFORM
           .
       2002-TENSULST-CSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT ½èÍý
      *****************************************************************
       910-XMLEXPORT-SEC           SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE   TENSULST-REC     TO  XMLDATA-DATA
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_medication_masterlst"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "medication_masterlst"
                                   TO  XMLDATA-RECORDNAME
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           INITIALIZE                   ORCA-BLOB
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    XMLDATA-OBJECT  TO  ORCA-BLOB-OBJECT
           INITIALIZE                  SGETTEMP-AREA
           STRING  "medication_masterlst"
                                   DELIMITED   BY  SIZE
                   CNT-XMLCOUNT    DELIMITED   BY  SIZE
                   ".xml"          DELIMITED   BY  SIZE
                                   INTO        SGETTEMP-BASENAMES  (1)
           END-STRING
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ORCA-BLOB-FILE
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       ORCA-BLOB
           .
       910-XMLEXPORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÆüÉÕÊÔ½¸½èÍý
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   SPACE   OR  "00000000"
               MOVE    "1900"              TO  WRK-HEN-YY
               MOVE    "01"                TO  WRK-HEN-MM
               MOVE    "01"                TO  WRK-HEN-DD
           ELSE
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
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           IF      WRK-THMS (1:3)      =   SPACE   OR  "000"
               MOVE    "00"                TO  WRK-HEN-THH
                                               WRK-HEN-TMM
                                               WRK-HEN-TSS
           ELSE
               MOVE    WRK-THH             TO  WRK-HEN-THH
               MOVE    WRK-TMM             TO  WRK-HEN-TMM
               MOVE    WRK-TSS             TO  WRK-HEN-TSS
           END-IF
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
      *    ½ªÎ»½èÍý
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "½èÍýÂÐ¾Ý¤Î¥Ç¡¼¥¿¤¬¤¢¤ê¤Þ¤»¤ó¤Ç¤·¤¿"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
             IF      WRK-PARA-DATA-FORMAT
                                       =   "csv"
                   CONTINUE
             ELSE
               INITIALIZE                 ORCSAPIFILEAREA
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-IN-DIRNAME
               MOVE    "medication_masterlst"
                                       TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               move    "2"             TO  SAPIFILE-IN-KBN
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    "medication_masterlst.xml"
                                       TO  SAPIFILE-OT-FILENAME
               CALL    "ORCSAPIFILE"   USING   ORCSAPIFILEAREA
      *
               IF      SAPIFILE-RC     =   0
                   CONTINUE
               ELSE
                   STRING   "£ø£í£ì¥Õ¥¡¥¤¥ëºîÀ®¥¨¥é¡¼ RC="
                                           DELIMITED   BY  SIZE
                            SAPIFILE-RC    DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
             END-IF
           END-IF
      *
           IF      WRK-PARA-DATA-FORMAT
                                       =   "csv"
               CLOSE           CSV-FILE
           END-IF
      *
      *    ¥¹¥Æ¥Ã¥×´ÉÍý½ªÎ»½èÍý
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORAPI052BR2V3 OUT:" CNT-OUT
           DISPLAY "*** ORAPI052BR2V3 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥¨¥é¡¼½ÐÎÏ½èÍý
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ¥¸¥ç¥Ö´ÉÍý³«»Ï½èÍý
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Õ¥¡¥¤¥ë¥¨¥é¡¼½èÍý
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "¥Õ¥¡¥¤¥ë£Ï£Ð£Å£Î¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "¥Õ¥¡¥¤¥ëÆÉ¤ß¹þ¤ß¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "¥Õ¥¡¥¤¥ë½ñ¤­¹þ¤ß¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "¥Õ¥¡¥¤¥ë¹¹¿·¥¨¥é¡¼¡¡FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORAPI052BR2V3 "        DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ½ÐÎÏ¥ì¥³¡¼¥É½èÍý¡ÊÊ¸»ú¡Ë
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE
               IF      WRK-MOJI-CHK    =   1
      *            ¸å¤í¤ÎÁ´³Ñ¶õÇò¤òºï½ü¤¹¤ë
                   INITIALIZE                      ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    "1"                 TO  KANACHK-SYORIKBN
                   MOVE    WRK-NAGASA          TO  KANACHK-MAX-CNT
                   MOVE    WRK-DATA            TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC          =   ZERO )
                   AND   ( KANACHK-MAX         >   ZERO )
                       MOVE    SPACE               TO  WRK-DATA
                       MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                                   TO  WRK-DATA
                   END-IF
               END-IF
      *
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA (1:IDW2)
                                               TO  WRK-REC
                       ELSE
                           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                                   WRK-DATA(1:IDW2)          DELIMITED
                                                             BY  SIZE
                                   INTO        WRK-REC
                           END-STRING
                       END-IF
                       ADD     IDW2                TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ¥¸¥ç¥Ö´ÉÍý£Ä£ÂÀ©¸æ½èÍý
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ÅÀ¿ô¥Þ¥¹¥¿¡¼ÆÉ¹þ
      *****************************************************************
       900-TENSU-READ-N-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               IF      WRK-PARA-SYORI      =   "1"
                   EVALUATE    WRK-PARA-EFFECTIVE-MODE
                       WHEN    "True"
                           IF      LNK-PRTKANRI-SKYYMD >   TNS-YUKOEDYMD
                               GO  TO  900-TENSU-READ-N-SEC
                           END-IF
                       WHEN    "False"
                           IF      LNK-PRTKANRI-SKYYMD <=  TNS-YUKOEDYMD
                               GO  TO  900-TENSU-READ-N-SEC
                           END-IF
                   END-EVALUATE
               END-IF    
           ELSE
              MOVE    1               TO  FLG-TENSU
           END-IF
           .
       900-TENSU-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ¥Æ¡¼¥Ö¥ë¸¡º÷½èÍý
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    £Ä£ÂÆÉ¤ß¹þ¤ß½èÍý
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¥¯¥í¡¼¥¹½èÍý
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    £Ä£Â¡¡¥ª¡¼¥×¥ó½èÍý
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
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
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
