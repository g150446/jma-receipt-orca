      ******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGI47.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : ＡＤＬ区分・医療区分入力
      *  管理者            : 
      *  05/03/09    NACL-太田    新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    ORCAMO       21/07/02  200-ENTRYに
      *                                     3101-EDIT-CONST-ADLKBN-SEC
      *                                     追加
      *  05.02.01    ORCAMO       24/05/07  令和６年度改正対応
      *  05.02.02    ORCAMO       24/06/20  令和６年６月以降の診療所の
      *                                     評価票の発行は不可とする
      ******************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計カード共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-I47.
           03  SPA-I47-AREA.
               05  SPA-I47-ADLINPUTKBN             PIC X(01).
               05  SPA-I47-GETUMATU.
                   07  SPA-I47-GETUMATU-YM         PIC X(06).
                   07  SPA-I47-GETUMATU-DD         PIC 9(02).
               05  SPA-I47-ZENGETUMATU.
                   07  SPA-I47-ZENGETUMATU-YM      PIC X(06).
                   07  SPA-I47-ZENGETUMATU-DD      PIC 9(02).
               05  SPA-I47-NYUIN-DAY-G.
                   07  SPA-I47-NYUIN-DAY           PIC 9(01)
                                                   OCCURS  31.
               05  SPA-I47-RYOYO-FLG               PIC 9(01).
               05  SPA-I47-RYOYO-DAY-G.
                   07  SPA-I47-RYOYO-DAY-OCC       OCCURS  31.
                       09  SPA-I47-RYOYO-DAY       PIC S9(02).
                       09  SPA-I47-RYOYO-CD        PIC X(09).
                       09  SPA-I47-RYOYO-KHN       PIC 9(02).
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-I47-CHG-FLG                 PIC 9(01).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SELNUM              PIC X(50).
                   07  SPA-GMN-MEISYO              PIC X(200).
                   07  SPA-GMN-GRPINPUT            PIC X(50).
                   07  SPA-GMN-DAY-G.
                       09  SPA-GMN-DAYALL          PIC X(02).
                       09  SPA-GMN-DAY             PIC X(02)
                                                   OCCURS  31.
               05  SPA-GMN-RYOLIST-AREA.
                   07  SPA-GMN-RYOLIST-MAX         PIC 9(05).
                   07  SPA-GMN-RYOLIST-SEL         PIC 9(05).
                   07  SPA-GMN-RYOLIST-OCC         OCCURS  100.
                       09  SPA-GMN-TNUM            PIC 9(03).
                       09  SPA-GMN-TNAME           PIC X(100).
                       09  SPA-GMN-TDAY-G.
                           11  SPA-GMN-TDAYALL     PIC 9(02).
                           11  SPA-GMN-TDAY        PIC 9(02)
                                                   OCCURS  31.
               05  SPA-NAI-RYOLIST-AREA.
                   07  SPA-NAI-RYOLIST-ADL-STIDX   PIC 9(03).
                   07  SPA-NAI-RYOLIST-DESIGNR-STIDX
                                                   PIC 9(03).
                   07  SPA-NAI-RYOLIST-OCC         OCCURS  100.
                       09  SPA-NAI-TZAISKBKBN      PIC X(01).
                       09  SPA-NAI-TMEDKBN         PIC 9(01).
                       09  SPA-NAI-TMEDKBN1        PIC X(01).
                       09  SPA-NAI-TDSPKBN         PIC X(01).
                       09  SPA-NAI-TLIMIT          PIC 9(03).
                       09  SPA-NAI-TALLKBN         PIC 9(01).
                       09  SPA-NAI-TZAINUM         PIC 9(04).
                       09  SPA-NAI-TSRYCD          PIC X(09).
                   07  SPA-NAI-DESIGNR-EVAL-DAY-G.
                       09  SPA-NAI-DESIGNR-EVAL-DAY     PIC 9(1)
                                                   OCCURS  31.
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-INPUT-MODE          PIC 9(1).
                   07  SPA-NAI-MUL-MAIN            PIC 9(1).
                   07  SPA-NAI-MUL-SUB             PIC 9(1).
                   07  SPA-NAI-SELNUM-FLG          PIC 9(1)
                                                   OCCURS  100.
                   07  SPA-NAI-DAY-G.
                       09  SPA-NAI-DAYALL          PIC 9(02).
                       09  SPA-NAI-DAY             PIC 9(02)
                                                   OCCURS  31.
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-LIMIT-ERR               PIC 9(01).
           03  FLG-RYOCOM                  PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-MEDKBN3                 PIC 9(01).
           03  FLG-MEDKBN2                 PIC 9(01).
           03  FLG-MEDKBN3LMT              PIC 9(01).
           03  FLG-MEDKBN2LMT              PIC 9(01).
           03  FLG-ADLKBN                  PIC 9(01).
           03  FLG-MEDCOM                  PIC 9(01).
           03  FLG-MEDMORE                 PIC 9(01).
           03  FLG-TARGET                  PIC 9(01).
           03  FLG-RYOLIST-ROW             PIC 9(01).
           03  FLG-DIFFER                  PIC 9(01).
           03  FLG-EDIT                    PIC 9(01).
           03  FLG-DESIGNR                 PIC 9(01).
           03  FLG-SHOW-ZERO               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-MAX                     PIC 9(05).
           03  CNT-DAY                     PIC 9(05).
           03  CNT-NISSU                   PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-DAY                     PIC 9(05).
           03  IDX-MUL                     PIC 9(05).
           03  IDX-PNT                     PIC 9(05).
           03  IDX-SEL                     PIC 9(05).
           03  IDX-SRY                     PIC 9(05).
           03  IDX-RYO                     PIC 9(05).
           03  IDX-MED                     PIC 9(05).
           03  IDX-ADL                     PIC 9(05).
           03  IDXA                        PIC 9(05).
           03  IDXM                        PIC 9(05).
           03  IDXK                        PIC 9(05).
           03  IDXR                        PIC 9(05).
           03  IDXZ                        PIC 9(05).
           03  IDXX                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-IIDCD                   PIC X(04).
           03  WRK-I4ID-FLG                PIC X(02).
           03  WRK-SYMD                    PIC X(10).
           03  WRK-SRYYM                   PIC X(06).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-MAE-INPUT               PIC X(100).
           03  WRK-OUT-INPUT               PIC X(100).
           03  WRK-TNS-SRYCD               PIC X(09).
           03  WRK-TNS-KJNYMD              PIC X(08).
           03  WRK-I4IDMSG                 PIC X(80). 
           03  WRK-ZZ                      PIC ZZ.
           03  WRK-Z9                      PIC Z9.
           03  WRK-ZZ9                     PIC ZZ9.
           03  WRK-ZZZ-G.
               05  WRK-ZZZ                 PIC ZZZ.
           03  WRK-STR-TMP                 PIC X(100).
           03  WRK-SELNUM-G.
               05  WRK-SELNUM-MUL          PIC X(50).
               05  WRK-INPUT-MODE          PIC 9(01).
               05  WRK-SELNUM-X            PIC X(50).
               05  WRK-SELNUM              PIC 9(03).
               05  WRK-SELNUM-FLG          PIC 9(01)
                                           OCCURS  100.
               05  WRK-SELNUM-IDX          PIC 9(03).
           03  WRK-GRPINPUT-G.
               05  WRK-GRPINPUT-X          PIC X(50).
               05  WRK-GRPINPUT            PIC 9(03).
               05  WRK-GRPINPUT-MAX        PIC 9(05).
               05  WRK-GRPINPUT-OCC        OCCURS  20.
                   07  WRK-GRPINPUT-TENSU  PIC 9(02).
                   07  WRK-GRPINPUT-DAYALL PIC 9(01).
                   07  WRK-GRPINPUT-DAY    PIC 9(01)
                                           OCCURS  31.
           03  WRK-MULTI-AREA.
               05  WRK-MUL-NUM             PIC X(10)
                                           OCCURS 20.
               05  WRK-MUL-SYMBOL          PIC X(10).
               05  WRK-MUL-TENSU-X         PIC X(10).
               05  WRK-MUL-STNUM-X         PIC X(10).
               05  WRK-MUL-EDNUM-X         PIC X(10).
               05  WRK-MUL-TENSU           PIC 9(05).
               05  WRK-MUL-STNUM           PIC 9(05).
               05  WRK-MUL-EDNUM           PIC 9(05).
           03  WRK-MAX                     PIC 9(05).
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-EDIDX                   PIC 9(05).
           03  WRK-WIDGET-DAY-G.
               05  WRK-WIDGET-DAY-LBL      PIC X(03).
               05  WRK-WIDGET-DAY          PIC 9(02).
           03  WRK-DAY-G.
               05  WRK-GMN-DAY             PIC X(02).
               05  WRK-DAY                 PIC 9(02).
           03  WRK-TENSU-G.
               05  WRK-TENSU-X             PIC X(10).
               05  WRK-TENSU               PIC 9(02).
           03  WRK-RYOYO-DAY-G.
               05  WRK-RYOYO-DAY           PIC S9(02)
                                           OCCURS  31.
           03  WRK-RYOYO-CD-G.
               05  WRK-RYOYO-CD            PIC X(09)
                                           OCCURS  31.
           03  WRK-RYOYO-KHN-G.
               05  WRK-RYOYO-KHN           PIC 9(02)
                                           OCCURS  31.
           03  WRK-ZAINUM-G.
               05  WRK-ZAINUM              PIC 9(08).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-ERRMSG                  PIC X(100).
           03  WRK-ADLTEN                  PIC 9(02).
      *
       01  MEDKBN-DAY-AREA.
      *    1:医療区分３ 2:医療区分２
      *    R0606以降の療養病棟  
      *    1:疾患・状態に係る医療区分３  2:疾患・状態に係る医療区分２
      *    3:処置等に係る医療区分３      4:処置等に係る医療区分２
           03  MEDKBN-OCC                  OCCURS 4.
               05  MEDKBN-DAYALL           PIC 9(01).
               05  MEDKBN-DAY-G.
                   07  MEDKBN-DAY          PIC 9(01)
                                           OCCURS  31.
      *    1:スモンの設定あり（R0606以降の療養病棟）
           03  MEDKBN-SUMONFLG             PIC 9(01).
       *
       01  ADLTEN-DAY-AREA.
           03  ADLTEN-DAYALL               PIC 9(02).
           03  ADLTEN-DAY-G.
               05  ADLTEN-DAY              PIC 9(02)
                                           OCCURS  31.
      *
       01  WRK-ROW-AREA.
           03  WRK-NYULIST-ROW             PIC S9(9).
           03  WRK-RYOLIST-ROW             PIC S9(9).
      *
      *    CPMEDKBN.INC から設定 
       01  MEDINF-AREA.
           03  MEDINF-MAX                  PIC 9(05).
           03  MEDINF-OCC                  OCCURS       100.
               09  MEDINF-MEDKBN           PIC 9(01).
               09  MEDINF-MEDKBN1          PIC X(01).
               09  MEDINF-DSPKBN           PIC X(01).
               09  MEDINF-LIMIT            PIC 9(03).
               09  MEDINF-ALLKBN           PIC 9(01).
               09  MEDINF-ZAINUM           PIC 9(04).
               09  MEDINF-NUM              PIC 9(02).
               09  MEDINF-SRYCD            PIC X(09).
      *
       01  CONST-AREA.
      *
           03  CONST-R060601           PIC X(08)   VALUE   "20240601".
           03  CONST-H300401           PIC X(08)   VALUE   "20180401".
           03  CONST-H280401           PIC X(08)   VALUE   "20160401".
           03  CONST-H240401           PIC X(08)   VALUE   "20120401".
           03  CONST-H220401           PIC X(08)   VALUE   "20100401".
           03  CONST-H200401           PIC X(08)   VALUE   "20080401".
      *
           03  CONST-NSRYACT-MAX       PIC 9(05)     VALUE 5.
      *
           03  CONST-RYOLIST-MAX       PIC 9(04)   VALUE   100.
      *
           03  CONST-DESIGNRTEN-MAX    PIC 9(03)   VALUE   66.
      *                                 
           03  CONST-R0606-SUMON-SRYCD PIC X(09)    VALUE  "820000057".
      *
           03  CONST-ADLKBN-G.
               05  CONST-ADLKBN-BASE-IDX   PIC 9(03).
               05  CONST-ADLTEN-MAX        PIC 9(03).
               05  CONST-ADLKBN-OCC    OCCURS  4.
                   07  CONST-ADLKBN-ALLKBN PIC 9(01).
                   07  CONST-ADLKBN-ZAINUM PIC 9(04).
                   07  CONST-ADLKBN-NAME   PIC X(100).
               05  CONST-ADLKBN-MAX        PIC 9(03).
      *
           03  CONST-MEDKBN-G.
               05  CONST-MEDKBN-VAL.
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2001.
                   07  FILLER          PIC X(09)   VALUE "820000057".
                   07  FILLER          PIC X(02)   VALUE "01".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2002.
                   07  FILLER          PIC X(09)   VALUE "820000058".
                   07  FILLER          PIC X(02)   VALUE "02".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2003.
                   07  FILLER          PIC X(09)   VALUE "820000059".
                   07  FILLER          PIC X(02)   VALUE "03".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2004.
                   07  FILLER          PIC X(09)   VALUE "820000060".
                   07  FILLER          PIC X(02)   VALUE "04".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2005.
                   07  FILLER          PIC X(09)   VALUE "820000061".
                   07  FILLER          PIC X(02)   VALUE "05".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2006.
                   07  FILLER          PIC X(09)   VALUE "820000062".
                   07  FILLER          PIC X(02)   VALUE "06".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2007.
                   07  FILLER          PIC X(09)   VALUE "820000063".
                   07  FILLER          PIC X(02)   VALUE "07".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2008.
                   07  FILLER          PIC X(09)   VALUE "820000064".
                   07  FILLER          PIC X(02)   VALUE "08".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2009.
                   07  FILLER          PIC X(09)   VALUE "820000065".
                   07  FILLER          PIC X(02)   VALUE "09".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2010.
                   07  FILLER          PIC X(09)   VALUE "820000066".
                   07  FILLER          PIC X(02)   VALUE "10".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2011.
                   07  FILLER          PIC X(09)   VALUE "820000067".
                   07  FILLER          PIC X(02)   VALUE "11".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2012.
                   07  FILLER          PIC X(09)   VALUE "820000068".
                   07  FILLER          PIC X(02)   VALUE "12".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2013.
                   07  FILLER          PIC X(09)   VALUE "820000069".
                   07  FILLER          PIC X(02)   VALUE "13".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2014.
                   07  FILLER          PIC X(09)   VALUE "820000070".
                   07  FILLER          PIC X(02)   VALUE "14".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2015.
                   07  FILLER          PIC X(09)   VALUE "820000071".
                   07  FILLER          PIC X(02)   VALUE "15".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2016.
                   07  FILLER          PIC X(09)   VALUE "820000072".
                   07  FILLER          PIC X(02)   VALUE "16".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2017.
                   07  FILLER          PIC X(09)   VALUE "820000073".
                   07  FILLER          PIC X(02)   VALUE "17".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2018.
                   07  FILLER          PIC X(09)   VALUE "820000074".
                   07  FILLER          PIC X(02)   VALUE "18".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2019.
                   07  FILLER          PIC X(09)   VALUE "820000075".
                   07  FILLER          PIC X(02)   VALUE "19".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 14.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2020.
                   07  FILLER          PIC X(09)   VALUE "820000076".
                   07  FILLER          PIC X(02)   VALUE "20".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 30.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2021.
                   07  FILLER          PIC X(09)   VALUE "820000077".
                   07  FILLER          PIC X(02)   VALUE "21".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2022.
                   07  FILLER          PIC X(09)   VALUE "820000078".
                   07  FILLER          PIC X(02)   VALUE "22".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2023.
                   07  FILLER          PIC X(09)   VALUE "820000079".
                   07  FILLER          PIC X(02)   VALUE "23".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 3.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2024.
                   07  FILLER          PIC X(09)   VALUE "820000080".
                   07  FILLER          PIC X(02)   VALUE "24".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2025.
                   07  FILLER          PIC X(09)   VALUE "820000081".
                   07  FILLER          PIC X(02)   VALUE "25".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2026.
                   07  FILLER          PIC X(09)   VALUE "820000082".
                   07  FILLER          PIC X(02)   VALUE "26".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2027.
                   07  FILLER          PIC X(09)   VALUE "820000083".
                   07  FILLER          PIC X(02)   VALUE "27".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2028.
                   07  FILLER          PIC X(09)   VALUE "820000084".
                   07  FILLER          PIC X(02)   VALUE "28".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2029.
                   07  FILLER          PIC X(09)   VALUE "820000085".
                   07  FILLER          PIC X(02)   VALUE "29".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2030.
                   07  FILLER          PIC X(09)   VALUE "820000086".
                   07  FILLER          PIC X(02)   VALUE "30".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2031.
                   07  FILLER          PIC X(09)   VALUE "820000087".
                   07  FILLER          PIC X(02)   VALUE "31".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2032.
                   07  FILLER          PIC X(09)   VALUE "820000088".
                   07  FILLER          PIC X(02)   VALUE "32".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2033.
                   07  FILLER          PIC X(09)   VALUE "820000089".
                   07  FILLER          PIC X(02)   VALUE "33".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 3.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2034.
                   07  FILLER          PIC X(09)   VALUE "820000090".
                   07  FILLER          PIC X(02)   VALUE "34".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2035.
                   07  FILLER          PIC X(09)   VALUE "820000091".
                   07  FILLER          PIC X(02)   VALUE "35".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2036.
                   07  FILLER          PIC X(09)   VALUE "820000092".
                   07  FILLER          PIC X(02)   VALUE "36".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2037.
                   07  FILLER          PIC X(09)   VALUE "820000093".
                   07  FILLER          PIC X(02)   VALUE "37".
      *
               05  CONST-MEDKBN-R REDEFINES CONST-MEDKBN-VAL.
                   07  CONST-MEDKBN-OCC    OCCURS  37.
                       09  CONST-MEDKBN-MEDKBN PIC 9(01).
                       09  CONST-MEDKBN-LIMIT  PIC 9(03).
                       09  CONST-MEDKBN-ALLKBN PIC 9(01).
                       09  CONST-MEDKBN-ZAINUM PIC 9(04).
                       09  CONST-MEDKBN-SRYCD  PIC X(09).
                       09  CONST-MEDKBN-ID     PIC X(02).
               05  CONST-MEDKBN-MAX        PIC 9(03)   VALUE   37.
      *
      *
           03  CONST-MEDH20-G.
               05  CONST-MEDH20-VAL.
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2004.
                   07  FILLER          PIC X(09)   VALUE "820000060".
                   07  FILLER          PIC X(02)   VALUE "01".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 14.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2020.
                   07  FILLER          PIC X(09)   VALUE "820000076".
                   07  FILLER          PIC X(02)   VALUE "02".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 30.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2021.
                   07  FILLER          PIC X(09)   VALUE "820000077".
                   07  FILLER          PIC X(02)   VALUE "03".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2022.
                   07  FILLER          PIC X(09)   VALUE "820000078".
                   07  FILLER          PIC X(02)   VALUE "04".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2023.
                   07  FILLER          PIC X(09)   VALUE "820000079".
                   07  FILLER          PIC X(02)   VALUE "05".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 3.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2024.
                   07  FILLER          PIC X(09)   VALUE "820000080".
                   07  FILLER          PIC X(02)   VALUE "06".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2027.
                   07  FILLER          PIC X(09)   VALUE "820000083".
                   07  FILLER          PIC X(02)   VALUE "07".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2031.
                   07  FILLER          PIC X(09)   VALUE "820000087".
                   07  FILLER          PIC X(02)   VALUE "08".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 3.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2034.
                   07  FILLER          PIC X(09)   VALUE "820000090".
                   07  FILLER          PIC X(02)   VALUE "09".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2001.
                   07  FILLER          PIC X(09)   VALUE "820000057".
                   07  FILLER          PIC X(02)   VALUE "10".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2010.
                   07  FILLER          PIC X(09)   VALUE "820000066".
                   07  FILLER          PIC X(02)   VALUE "11".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2002.
                   07  FILLER          PIC X(09)   VALUE "820000058".
                   07  FILLER          PIC X(02)   VALUE "12".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2003.
                   07  FILLER          PIC X(09)   VALUE "820000059".
                   07  FILLER          PIC X(02)   VALUE "13".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2005.
                   07  FILLER          PIC X(09)   VALUE "820000061".
                   07  FILLER          PIC X(02)   VALUE "14".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2006.
                   07  FILLER          PIC X(09)   VALUE "820000062".
                   07  FILLER          PIC X(02)   VALUE "15".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2007.
                   07  FILLER          PIC X(09)   VALUE "820000063".
                   07  FILLER          PIC X(02)   VALUE "16".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2008.
                   07  FILLER          PIC X(09)   VALUE "820000064".
                   07  FILLER          PIC X(02)   VALUE "17".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2009.
                   07  FILLER          PIC X(09)   VALUE "820000065".
                   07  FILLER          PIC X(02)   VALUE "18".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2011.
                   07  FILLER          PIC X(09)   VALUE "820000067".
                   07  FILLER          PIC X(02)   VALUE "19".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2012.
                   07  FILLER          PIC X(09)   VALUE "820000068".
                   07  FILLER          PIC X(02)   VALUE "20".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2013.
                   07  FILLER          PIC X(09)   VALUE "820000069".
                   07  FILLER          PIC X(02)   VALUE "21".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2014.
                   07  FILLER          PIC X(09)   VALUE "820000070".
                   07  FILLER          PIC X(02)   VALUE "22".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2015.
                   07  FILLER          PIC X(09)   VALUE "820000071".
                   07  FILLER          PIC X(02)   VALUE "23".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2016.
                   07  FILLER          PIC X(09)   VALUE "820000072".
                   07  FILLER          PIC X(02)   VALUE "24".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2017.
                   07  FILLER          PIC X(09)   VALUE "820000073".
                   07  FILLER          PIC X(02)   VALUE "25".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2030.
                   07  FILLER          PIC X(09)   VALUE "820000086".
                   07  FILLER          PIC X(02)   VALUE "26".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2036.
                   07  FILLER          PIC X(09)   VALUE "820000092".
                   07  FILLER          PIC X(02)   VALUE "27".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2037.
                   07  FILLER          PIC X(09)   VALUE "820000093".
                   07  FILLER          PIC X(02)   VALUE "28".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2018.
                   07  FILLER          PIC X(09)   VALUE "820000074".
                   07  FILLER          PIC X(02)   VALUE "29".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2019.
                   07  FILLER          PIC X(09)   VALUE "820000075".
                   07  FILLER          PIC X(02)   VALUE "30".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2025.
                   07  FILLER          PIC X(09)   VALUE "820000081".
                   07  FILLER          PIC X(02)   VALUE "31".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2026.
                   07  FILLER          PIC X(09)   VALUE "820000082".
                   07  FILLER          PIC X(02)   VALUE "32".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2028.
                   07  FILLER          PIC X(09)   VALUE "820000084".
                   07  FILLER          PIC X(02)   VALUE "33".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2029.
                   07  FILLER          PIC X(09)   VALUE "820000085".
                   07  FILLER          PIC X(02)   VALUE "34".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2032.
                   07  FILLER          PIC X(09)   VALUE "820000088".
                   07  FILLER          PIC X(02)   VALUE "35".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2033.
                   07  FILLER          PIC X(09)   VALUE "820000089".
                   07  FILLER          PIC X(02)   VALUE "36".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2035.
                   07  FILLER          PIC X(09)   VALUE "820000091".
                   07  FILLER          PIC X(02)   VALUE "37".
      *
               05  CONST-MEDH20-R REDEFINES CONST-MEDH20-VAL.
                   07  CONST-MEDH20-OCC    OCCURS  37.
                       09  CONST-MEDH20-MEDH20 PIC 9(01).
                       09  CONST-MEDH20-LIMIT  PIC 9(03).
                       09  CONST-MEDH20-ALLKBN PIC 9(01).
                       09  CONST-MEDH20-ZAINUM PIC 9(04).
                       09  CONST-MEDH20-SRYCD  PIC X(09).
                       09  CONST-MEDH20-ID     PIC X(02).
               05  CONST-MEDH20-MAX        PIC 9(03)   VALUE   37.
      *
           03  CONST-MEDH22-G.
               05  CONST-MEDH22-VAL.
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2004.
                   07  FILLER          PIC X(09)   VALUE "820000060".
                   07  FILLER          PIC X(02)   VALUE "01".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 14.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2020.
                   07  FILLER          PIC X(09)   VALUE "820000076".
                   07  FILLER          PIC X(02)   VALUE "02".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 30.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2021.
                   07  FILLER          PIC X(09)   VALUE "820000077".
                   07  FILLER          PIC X(02)   VALUE "03".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2022.
                   07  FILLER          PIC X(09)   VALUE "820000078".
                   07  FILLER          PIC X(02)   VALUE "04".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2023.
                   07  FILLER          PIC X(09)   VALUE "820000079".
                   07  FILLER          PIC X(02)   VALUE "05".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 3.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2024.
                   07  FILLER          PIC X(09)   VALUE "820000080".
                   07  FILLER          PIC X(02)   VALUE "06".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2027.
                   07  FILLER          PIC X(09)   VALUE "820000083".
                   07  FILLER          PIC X(02)   VALUE "07".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2031.
                   07  FILLER          PIC X(09)   VALUE "820000087".
                   07  FILLER          PIC X(02)   VALUE "08".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 3.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2034.
                   07  FILLER          PIC X(09)   VALUE "820000090".
                   07  FILLER          PIC X(02)   VALUE "09".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2001.
                   07  FILLER          PIC X(09)   VALUE "820000057".
                   07  FILLER          PIC X(02)   VALUE "10".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2010.
                   07  FILLER          PIC X(09)   VALUE "820000066".
                   07  FILLER          PIC X(02)   VALUE "11".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2002.
                   07  FILLER          PIC X(09)   VALUE "820000058".
                   07  FILLER          PIC X(02)   VALUE "12".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2003.
                   07  FILLER          PIC X(09)   VALUE "820000059".
                   07  FILLER          PIC X(02)   VALUE "13".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2005.
                   07  FILLER          PIC X(09)   VALUE "820000061".
                   07  FILLER          PIC X(02)   VALUE "14".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2006.
                   07  FILLER          PIC X(09)   VALUE "820000062".
                   07  FILLER          PIC X(02)   VALUE "15".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2007.
                   07  FILLER          PIC X(09)   VALUE "820000063".
                   07  FILLER          PIC X(02)   VALUE "16".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2008.
                   07  FILLER          PIC X(09)   VALUE "820000064".
                   07  FILLER          PIC X(02)   VALUE "17".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2009.
                   07  FILLER          PIC X(09)   VALUE "820000065".
                   07  FILLER          PIC X(02)   VALUE "18".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2011.
                   07  FILLER          PIC X(09)   VALUE "820000067".
                   07  FILLER          PIC X(02)   VALUE "19".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2012.
                   07  FILLER          PIC X(09)   VALUE "820000068".
                   07  FILLER          PIC X(02)   VALUE "20".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2013.
                   07  FILLER          PIC X(09)   VALUE "820000069".
                   07  FILLER          PIC X(02)   VALUE "21".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2014.
                   07  FILLER          PIC X(09)   VALUE "820000070".
                   07  FILLER          PIC X(02)   VALUE "22".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2015.
                   07  FILLER          PIC X(09)   VALUE "820000071".
                   07  FILLER          PIC X(02)   VALUE "23".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2016.
                   07  FILLER          PIC X(09)   VALUE "820000072".
                   07  FILLER          PIC X(02)   VALUE "24".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2017.
                   07  FILLER          PIC X(09)   VALUE "820000073".
                   07  FILLER          PIC X(02)   VALUE "25".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2030.
                   07  FILLER          PIC X(09)   VALUE "820000086".
                   07  FILLER          PIC X(02)   VALUE "26".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2036.
                   07  FILLER          PIC X(09)   VALUE "820000092".
                   07  FILLER          PIC X(02)   VALUE "27".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2037.
                   07  FILLER          PIC X(09)   VALUE "820000093".
                   07  FILLER          PIC X(02)   VALUE "28".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2018.
                   07  FILLER          PIC X(09)   VALUE "820000074".
                   07  FILLER          PIC X(02)   VALUE "29".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2019.
                   07  FILLER          PIC X(09)   VALUE "820000075".
                   07  FILLER          PIC X(02)   VALUE "30".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2025.
                   07  FILLER          PIC X(09)   VALUE "820000081".
                   07  FILLER          PIC X(02)   VALUE "31".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2026.
                   07  FILLER          PIC X(09)   VALUE "820000082".
                   07  FILLER          PIC X(02)   VALUE "32".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2028.
                   07  FILLER          PIC X(09)   VALUE "820000084".
                   07  FILLER          PIC X(02)   VALUE "33".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2029.
                   07  FILLER          PIC X(09)   VALUE "820000085".
                   07  FILLER          PIC X(02)   VALUE "34".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2032.
                   07  FILLER          PIC X(09)   VALUE "820000088".
                   07  FILLER          PIC X(02)   VALUE "35".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2033.
                   07  FILLER          PIC X(09)   VALUE "820000089".
                   07  FILLER          PIC X(02)   VALUE "36".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2035.
                   07  FILLER          PIC X(09)   VALUE "820000091".
                   07  FILLER          PIC X(02)   VALUE "37".
      *
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2091.
                   07  FILLER          PIC X(09)   VALUE "820000158".
                   07  FILLER          PIC X(02)   VALUE "91".
      *
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2092.
                   07  FILLER          PIC X(09)   VALUE "820000159".
                   07  FILLER          PIC X(02)   VALUE "92".
      *
               05  CONST-MEDH22-R REDEFINES CONST-MEDH22-VAL.
                   07  CONST-MEDH22-OCC     OCCURS  39.
                       09  CONST-MEDH22-MEDH22 PIC 9(01).
                       09  CONST-MEDH22-LIMIT  PIC 9(03).
                       09  CONST-MEDH22-ALLKBN PIC 9(01).
                       09  CONST-MEDH22-ZAINUM PIC 9(04).
                       09  CONST-MEDH22-SRYCD  PIC X(09).
                       09  CONST-MEDH22-ID     PIC X(02).
               05  CONST-MEDH22-MAX        PIC 9(03)   VALUE   39.
      *
           03  CONST-MEDH24-G.
               05  CONST-MEDH24-VAL.
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2004.
                   07  FILLER          PIC X(09)   VALUE "820000060".
                   07  FILLER          PIC X(02)   VALUE "01".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 14.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2020.
                   07  FILLER          PIC X(09)   VALUE "820000076".
                   07  FILLER          PIC X(02)   VALUE "02".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 30.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2021.
                   07  FILLER          PIC X(09)   VALUE "820000077".
                   07  FILLER          PIC X(02)   VALUE "03".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2022.
                   07  FILLER          PIC X(09)   VALUE "820000078".
                   07  FILLER          PIC X(02)   VALUE "04".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2023.
                   07  FILLER          PIC X(09)   VALUE "820000079".
                   07  FILLER          PIC X(02)   VALUE "05".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 3.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2024.
                   07  FILLER          PIC X(09)   VALUE "820000080".
                   07  FILLER          PIC X(02)   VALUE "06".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2027.
                   07  FILLER          PIC X(09)   VALUE "820000083".
                   07  FILLER          PIC X(02)   VALUE "07".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 7.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2031.
                   07  FILLER          PIC X(09)   VALUE "820000087".
                   07  FILLER          PIC X(02)   VALUE "08".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 3.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2034.
                   07  FILLER          PIC X(09)   VALUE "820000090".
                   07  FILLER          PIC X(02)   VALUE "09".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2001.
                   07  FILLER          PIC X(09)   VALUE "820000057".
                   07  FILLER          PIC X(02)   VALUE "10".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2010.
                   07  FILLER          PIC X(09)   VALUE "820000066".
                   07  FILLER          PIC X(02)   VALUE "11".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2002.
                   07  FILLER          PIC X(09)   VALUE "820000058".
                   07  FILLER          PIC X(02)   VALUE "12".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2003.
                   07  FILLER          PIC X(09)   VALUE "820000059".
                   07  FILLER          PIC X(02)   VALUE "13".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2005.
                   07  FILLER          PIC X(09)   VALUE "820000061".
                   07  FILLER          PIC X(02)   VALUE "14".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2006.
                   07  FILLER          PIC X(09)   VALUE "820000062".
                   07  FILLER          PIC X(02)   VALUE "15".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2007.
                   07  FILLER          PIC X(09)   VALUE "820000063".
                   07  FILLER          PIC X(02)   VALUE "16".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2008.
                   07  FILLER          PIC X(09)   VALUE "820000064".
                   07  FILLER          PIC X(02)   VALUE "17".
      *
                   07  FILLER          PIC 9(01)   VALUE 3.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2009.
                   07  FILLER          PIC X(09)   VALUE "820000065".
                   07  FILLER          PIC X(02)   VALUE "18".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2011.
                   07  FILLER          PIC X(09)   VALUE "820000067".
                   07  FILLER          PIC X(02)   VALUE "19".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2012.
                   07  FILLER          PIC X(09)   VALUE "820000068".
                   07  FILLER          PIC X(02)   VALUE "20".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2013.
                   07  FILLER          PIC X(09)   VALUE "820000069".
                   07  FILLER          PIC X(02)   VALUE "21".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2014.
                   07  FILLER          PIC X(09)   VALUE "820000070".
                   07  FILLER          PIC X(02)   VALUE "22".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2015.
                   07  FILLER          PIC X(09)   VALUE "820000071".
                   07  FILLER          PIC X(02)   VALUE "23".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2016.
                   07  FILLER          PIC X(09)   VALUE "820000072".
                   07  FILLER          PIC X(02)   VALUE "24".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2017.
                   07  FILLER          PIC X(09)   VALUE "820000073".
                   07  FILLER          PIC X(02)   VALUE "25".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2030.
                   07  FILLER          PIC X(09)   VALUE "820000086".
                   07  FILLER          PIC X(02)   VALUE "26".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2036.
                   07  FILLER          PIC X(09)   VALUE "820000092".
                   07  FILLER          PIC X(02)   VALUE "27".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 1.
                   07  FILLER          PIC 9(04)   VALUE 2037.
                   07  FILLER          PIC X(09)   VALUE "820000093".
                   07  FILLER          PIC X(02)   VALUE "28".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2018.
                   07  FILLER          PIC X(09)   VALUE "820000074".
                   07  FILLER          PIC X(02)   VALUE "29".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2019.
                   07  FILLER          PIC X(09)   VALUE "820000075".
                   07  FILLER          PIC X(02)   VALUE "30".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2025.
                   07  FILLER          PIC X(09)   VALUE "820000081".
                   07  FILLER          PIC X(02)   VALUE "31".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2026.
                   07  FILLER          PIC X(09)   VALUE "820000082".
                   07  FILLER          PIC X(02)   VALUE "32".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2028.
                   07  FILLER          PIC X(09)   VALUE "820000084".
                   07  FILLER          PIC X(02)   VALUE "33".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2029.
                   07  FILLER          PIC X(09)   VALUE "820000085".
                   07  FILLER          PIC X(02)   VALUE "34".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2032.
                   07  FILLER          PIC X(09)   VALUE "820000088".
                   07  FILLER          PIC X(02)   VALUE "35".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2033.
                   07  FILLER          PIC X(09)   VALUE "820000089".
                   07  FILLER          PIC X(02)   VALUE "36".
      *
                   07  FILLER          PIC 9(01)   VALUE 2.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2035.
                   07  FILLER          PIC X(09)   VALUE "820000091".
                   07  FILLER          PIC X(02)   VALUE "37".
      *
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(03)   VALUE 0.
                   07  FILLER          PIC 9(01)   VALUE 0.
                   07  FILLER          PIC 9(04)   VALUE 2092.
                   07  FILLER          PIC X(09)   VALUE "820000159".
                   07  FILLER          PIC X(02)   VALUE "91".
      *
               05  CONST-MEDH24-R REDEFINES CONST-MEDH24-VAL.
                   07  CONST-MEDH24-OCC     OCCURS  38.
                       09  CONST-MEDH24-MEDH24 PIC 9(01).
                       09  CONST-MEDH24-LIMIT  PIC 9(03).
                       09  CONST-MEDH24-ALLKBN PIC 9(01).
                       09  CONST-MEDH24-ZAINUM PIC 9(04).
                       09  CONST-MEDH24-SRYCD  PIC X(09).
                       09  CONST-MEDH24-ID     PIC X(02).
               05  CONST-MEDH24-MAX        PIC 9(03)   VALUE   38.
      *
           COPY    "CPRYOYOCD.INC".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                               //SPA-// BY //WKSPA-//.
      *
      *
           COPY  "CPMEDKBN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5111.INC".
      *
           COPY    "CPSK5014.INC".
      *
           COPY    "CPSK5015.INC".
      *
           COPY    "CPTENSU.INC".
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCSS005.INC".
           COPY    "CPORCSS010.INC".
           COPY    "CPORCSS011.INC".
      *
      *    医療区分・ＡＤＬ評価票
           COPY    "CPORCHCN68.INC".
           COPY    "CPONPRTDATA.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA32SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 LINKAREA
                                                 SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-I4KYOUTU
           MOVE    SPA-FREE        TO  SPA-I47
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-I4KYOUTU    TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I47         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "I4ERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  MCP-PUTTYPE
           MOVE    "I47"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "I4ID1"
           WHEN    "I4ID2"
               PERFORM 330-I4ID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           MOVE    SPA-AREA        TO  WKSPA-AREA
           MOVE    SPA-GMN-PTID    TO  WKSPA-PTID
      *
           INITIALIZE                  SS005-AREA
           MOVE    SPA-I40-SRYYMD(1:6)
                                   TO  SS005-SRYYM
           CALL    "ORCSS005"      USING   SS005-AREA
                                           WKSPA-AREA
      *
           INITIALIZE                  SPA-I47
      *
           INITIALIZE                  SYS-5015-REC
           MOVE    "5015"          TO  SYS-5015-KANRICD
           MOVE    "*"             TO  SYS-5015-KBNCD
           MOVE    SPA-I40-SRYYMD (1:6)
                                   TO  SYS-5015-STYUKYMD
           MOVE    "01"            TO  SYS-5015-STYUKYMD (7:2)
           MOVE    SYS-5015-STYUKYMD
                                   TO  SYS-5015-EDYUKYMD
           MOVE    SYS-5015-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5015-REC
      *
           IF    ( SYS-5015-ADLINPUTKBN    =   SPACE )
               MOVE    "1"         TO  SPA-I47-ADLINPUTKBN
           ELSE
               MOVE    SYS-5015-ADLINPUTKBN
                                   TO  SPA-I47-ADLINPUTKBN
           END-IF
      *
           IF    ( SPA-I40-SRYYMD  >=   CONST-H220401 )
               MOVE    "1"         TO  SPA-I47-ADLINPUTKBN
           END-IF
      *
           PERFORM 3101-EDIT-CONST-ADLKBN-SEC
      *
           MOVE    SPA-I40-SRYYMD  TO  WRK-SYMD
           PERFORM 800-GETUMATU-SEC
           MOVE    WRK-SYMD        TO  SPA-I47-GETUMATU
      *
           MOVE    SPA-I40-SRYYMD  TO  WRK-SYMD
           MOVE    "01"            TO  WRK-SYMD(7:2)
           MOVE    "1"             TO  WRK-ZOGENPTN
           MOVE    -1              TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD        TO  SPA-I47-ZENGETUMATU
      *
           PERFORM 3101-EDIT-NYUIN-DAY-SEC
      *
           PERFORM 3101-EDIT-RYOYO-DAY-SEC
      *
           PERFORM 3101-EDIT-RYOLST-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    ADL区分固定値設定処理
      *****************************************************************
       3101-EDIT-CONST-ADLKBN-SEC      SECTION.
      *
           INITIALIZE                      CONST-ADLKBN-G
      *
           MOVE    95                      TO  CONST-ADLKBN-BASE-IDX
      *
           IF    ( SPA-I47-ADLINPUTKBN     =   "1" )
      *
               MOVE    6                   TO  CONST-ADLTEN-MAX
               MOVE    4                   TO  CONST-ADLKBN-MAX
      *
               MOVE    1                   TO  CONST-ADLKBN-ALLKBN (1)
               MOVE    1001                TO  CONST-ADLKBN-ZAINUM (1)
               MOVE    "　ａ　ベッドの可動性"
                                           TO  CONST-ADLKBN-NAME   (1)
      *
               MOVE    1                   TO  CONST-ADLKBN-ALLKBN (2)
               MOVE    1002                TO  CONST-ADLKBN-ZAINUM (2)
               MOVE    "　ｂ　移乗"        TO  CONST-ADLKBN-NAME   (2)
      *
               MOVE    1                   TO  CONST-ADLKBN-ALLKBN (3)
               MOVE    1003                TO  CONST-ADLKBN-ZAINUM (3)
               MOVE    "　ｃ　食事"        TO  CONST-ADLKBN-NAME   (3)
      *
               MOVE    1                   TO  CONST-ADLKBN-ALLKBN (4)
               MOVE    1004                TO  CONST-ADLKBN-ZAINUM (4)
               MOVE    "　ｄ　トイレの使用" TO CONST-ADLKBN-NAME   (4)
           ELSE
      *
               MOVE    24                  TO  CONST-ADLTEN-MAX
               MOVE    1                   TO  CONST-ADLKBN-MAX
      *
               MOVE    1                   TO  CONST-ADLKBN-ALLKBN (1)
               MOVE    3000                TO  CONST-ADLKBN-ZAINUM (1)
               MOVE     "ＡＤＬ点数"       TO  CONST-ADLKBN-NAME   (1)
           END-IF
      *
           .
       3101-EDIT-CONST-ADLKBN-EXT.
           EXIT.
      *****************************************************************
      *    入院期間編集処理
      *****************************************************************
       3101-EDIT-NYUIN-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY19-SEL-SEC
           IF    ( FLG-NYUINACCT   =   ZERO )
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( NACCT-DAY (IDX-DAY) >   ZERO )
                       MOVE    1   TO  SPA-I47-NYUIN-DAY (IDX-DAY)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       3101-EDIT-NYUIN-DAY-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料算定期間編集処理
      *****************************************************************
       3101-EDIT-RYOYO-DAY-SEC         SECTION.
      *
           PERFORM 900-NYUINACCT-KEY35-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               INITIALIZE      WRK-RYOYO-DAY-G
                               WRK-RYOYO-CD-G
                               WRK-RYOYO-KHN-G
      *
               PERFORM 31011-EDIT-RYOYO-DAY-SEC
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   IF    ( WRK-RYOYO-DAY (IDX-DAY) NOT =   ZERO )
                       MOVE    WRK-RYOYO-DAY (IDX-DAY)
                                       TO  SPA-I47-RYOYO-DAY (IDX-DAY)
                   END-IF
      *
                   IF    ( WRK-RYOYO-CD  (IDX-DAY) NOT =   SPACE )
                       MOVE    WRK-RYOYO-CD  (IDX-DAY)
                                       TO  SPA-I47-RYOYO-CD  (IDX-DAY)
                       MOVE    WRK-RYOYO-KHN (IDX-DAY)
                                       TO  SPA-I47-RYOYO-KHN (IDX-DAY)
                       MOVE    1       TO  SPA-I47-RYOYO-FLG
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
       3101-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料算定期間編集処理
      *****************************************************************
       31011-EDIT-RYOYO-DAY-SEC         SECTION.
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
                   PERFORM VARYING IDXZ    FROM    1   BY  1
                           UNTIL ( IDXZ    >   CONST-EXRYOYO-MAX )
                       IF    ( NSRY-SRYCD (IDX-SRY)
                                   =   CONST-EXRYOYO-UNDEFINED (IDXZ))
                           MOVE    CONST-EXRYOYO-DEFINED (IDXZ)
                                           TO   NSRY-SRYCD (IDX-SRY)
                           MOVE    CONST-EXRYOYO-MAX
                                           TO   IDXZ
                       END-IF
                   END-PERFORM
      *
                   PERFORM 310111-RYOYOCD-CHK-SEC
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
       31011-EDIT-RYOYO-DAY-EXT.
           EXIT.
      *****************************************************************
      *    療養型入院基本料診療行為コードチェック処理
      *****************************************************************
       310111-RYOYOCD-CHK-SEC          SECTION.
      *
           IF    ( SPA-I40-SRYYMD      >=  CONST-R060601 )
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
               MOVE    SPA-I40-SRYYMD
                                               TO  WRK-TNS-KJNYMD
               MOVE    "01"        TO  WRK-TNS-KJNYMD (7:2)
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
       310111-RYOYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分・医療区分リスト編集処理
      *****************************************************************
       3101-EDIT-RYOLST-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-RYOLIST-AREA
                                       SPA-NAI-RYOLIST-AREA
      *
           PERFORM 31011-EDIT-MEDINF-SEC
      *
           MOVE    ZERO            TO  IDX-LST
      *
           PERFORM VARYING IDX0        FROM    1   BY  1
                   UNTIL ( IDX0    >   MEDINF-MAX )
                    OR   ( IDX-LST >=  CONST-RYOLIST-MAX )
      *
               MOVE    SPA-I40-SRYYMD (1:6)
                                       TO  WRK-SRYYM
               MOVE    MEDINF-ZAINUM (IDX0)
                                       TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
      *
                   PERFORM 31011-EDIT-RYOLST-MEDKBN-SEC
      *
                   MOVE    RYOCOM-MONTH
                                   TO  SPA-GMN-TDAYALL (IDX-LST)
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  SPA-GMN-TDAY (IDX-LST IDX-DAY)
                   END-PERFORM
      *
               ELSE
      *
                   MOVE    ZERO            TO  FLG-EDIT
      *
                   INITIALIZE                  SYS-5014-REC
                   MOVE    "5014"          TO  SYS-5014-KANRICD
                   MOVE    MEDINF-ZAINUM (IDX0)
                                           TO  WRK-ZAINUM
                   MOVE    WRK-ZAINUM-G    TO  SYS-5014-KBNCD
                   MOVE    SPA-I40-SRYYMD (1:6)
                                           TO  SYS-5014-STYUKYMD
                   MOVE    "01"            TO  SYS-5014-STYUKYMD (7:2)
                   MOVE    SYS-5014-STYUKYMD
                                           TO  SYS-5014-EDYUKYMD
                   MOVE    SYS-5014-REC    TO  SYSKANRI-REC
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
      *
                       MOVE    SYSKANRI-REC    TO  SYS-5014-REC
                       IF    ( SYS-5014-DISPKBN    =   "1" )
                           MOVE    1       TO  FLG-EDIT
                       END-IF
      *
                   ELSE
                           MOVE    1       TO  FLG-EDIT
                   END-IF
      *
                   IF    ( SPA-I40-SRYYMD  >=  CONST-H300401 )
                       IF    ( MEDINF-NUM (IDX0)
                                            =  4 OR 6 OR 8
                                           OR 12 OR 16 OR 39 )
                           MOVE    1       TO  FLG-EDIT
                       END-IF
                   END-IF
      *
                   IF    ( FLG-EDIT    =   1 )
                       PERFORM 31011-EDIT-RYOLST-MEDKBN-SEC
                   END-IF
      *
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
                   MOVE    IDX-LST TO  SPA-NAI-RYOLIST-ADL-STIDX
               END-IF
      *
               MOVE    "1"         TO  SPA-NAI-TZAISKBKBN
                                                   (IDX-LST)
      *
               MOVE    CONST-ADLKBN-ALLKBN (IDX0)
                                   TO  SPA-NAI-TALLKBN (IDX-LST)
               MOVE    CONST-ADLKBN-ZAINUM (IDX0)
                                   TO  SPA-NAI-TZAINUM (IDX-LST)
               MOVE    CONST-ADLKBN-NAME   (IDX0)
                                   TO  SPA-GMN-TNAME   (IDX-LST)
      *
               COMPUTE SPA-GMN-TNUM (IDX-LST)
                   =   CONST-ADLKBN-BASE-IDX
                   +   IDX0
      *
               MOVE    SPA-I40-SRYYMD (1:6)
                                   TO  WRK-SRYYM
               MOVE    SPA-NAI-TZAINUM (IDX-LST)
                                   TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
      *
                   MOVE    RYOCOM-MONTH
                                   TO  SPA-GMN-TDAYALL (IDX-LST)
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  SPA-GMN-TDAY (IDX-LST IDX-DAY)
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-I40-SRYYMD  >=  CONST-H300401 )
               COMPUTE IDX-LST =   IDX-LST +   1
               MOVE    IDX-LST TO  SPA-NAI-RYOLIST-DESIGNR-STIDX
      *
               MOVE    "A"         TO  SPA-NAI-TZAISKBKBN
                                                   (IDX-LST)
      *
               MOVE    DESIGNR-ALLKBN
                                   TO  SPA-NAI-TALLKBN (IDX-LST)
               MOVE    DESIGNR-ZAINUM
                                   TO  SPA-NAI-TZAINUM (IDX-LST)
               MOVE    DESIGNR-SRYCD
                                   TO  SPA-NAI-TSRYCD  (IDX-LST)
               MOVE    DESIGNR-MEI
                                   TO  SPA-GMN-TNAME   (IDX-LST)
      *
               COMPUTE SPA-GMN-TNUM (IDX-LST)
                   =   101
      *
               MOVE    SPA-I40-SRYYMD (1:6)
                                   TO  WRK-SRYYM
               MOVE    SPA-NAI-TZAINUM (IDX-LST)
                                   TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  SPA-GMN-TDAY (IDX-LST IDX-DAY)
                   END-PERFORM
               END-IF
      *
               MOVE    SPA-I40-SRYYMD (1:6)
                                   TO  WRK-SRYYM
               MOVE    DESIGNR-ZAINUM-EVAL
                                   TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       MOVE    RYOCOM-DAY (IDX-DAY)
                                   TO  SPA-NAI-DESIGNR-EVAL-DAY(IDX-DAY)
                   END-PERFORM
               END-IF
      *
           END-IF
      *
           MOVE    IDX-LST         TO  SPA-GMN-RYOLIST-MAX
      *
           .
       3101-EDIT-RYOLST-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分・医療区分リスト編集処理 （医療区分編集）
      *****************************************************************
       31011-EDIT-RYOLST-MEDKBN-SEC    SECTION.
      *
           COMPUTE IDX-LST =   IDX-LST +   1
      *
           MOVE    "2"         TO  SPA-NAI-TZAISKBKBN  (IDX-LST)
           MOVE    MEDINF-MEDKBN (IDX0)
                                   TO  SPA-NAI-TMEDKBN (IDX-LST)
           MOVE    MEDINF-MEDKBN1 (IDX0)
                                   TO  SPA-NAI-TMEDKBN1 (IDX-LST)
           MOVE    MEDINF-DSPKBN  (IDX0)
                                   TO  SPA-NAI-TDSPKBN (IDX-LST)
           MOVE    MEDINF-LIMIT  (IDX0)
                                   TO  SPA-NAI-TLIMIT  (IDX-LST)
           MOVE    MEDINF-ALLKBN (IDX0)
                                   TO  SPA-NAI-TALLKBN (IDX-LST)
           MOVE    MEDINF-ZAINUM (IDX0)
                                   TO  SPA-NAI-TZAINUM (IDX-LST)
           MOVE    MEDINF-SRYCD  (IDX0)
                                   TO  SPA-NAI-TSRYCD  (IDX-LST)
           MOVE    MEDINF-NUM    (IDX0)
                                   TO  SPA-GMN-TNUM    (IDX-LST)
           MOVE    SPACE           TO  SPA-GMN-TNAME   (IDX-LST)
      *
      *
           EVALUATE    TRUE
           WHEN  ( SPA-I40-SRYYMD  >=  CONST-R060601 )
               IF      SPA-HOSPSBT     =   1
                   MOVE    1               TO  IDXX
               ELSE
                   MOVE    2               TO  IDXX
               END-IF              
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL ( IDX2 >   MEDR06-MAX )
                        OR   ( MEDR06-STYMD (IDXX IDX2)
                                           =   SPACE )
                        OR   ( SPA-GMN-TNAME   (IDX-LST)
                                       NOT =   SPACE )
                   IF    ( MEDR06-STYMD  (IDXX IDX2)
                                      <=   SPA-I40-SRYYMD )
                     AND ( MEDR06-EDYMD  (IDXX IDX2)
                                      >=   SPA-I40-SRYYMD )
                     AND ( MEDR06-ZAINUM (IDXX IDX2)
                                       =   SPA-NAI-TZAINUM  (IDX-LST))
                       MOVE    MEDR06-MEIL (IDXX IDX2)
                                       TO  SPA-GMN-TNAME   (IDX-LST)
                   END-IF
      *
               END-PERFORM
           WHEN  ( SPA-I40-SRYYMD  >=  CONST-H200401 )
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL ( IDX2 >   MEDH20-MAX )
                        OR   ( SPA-GMN-TNAME   (IDX-LST)
                                       NOT =   SPACE )
                   IF    ( MEDH20-STYMD  (IDX2)
                                      <=   SPA-I40-SRYYMD )
                     AND ( MEDH20-EDYMD  (IDX2)
                                      >=   SPA-I40-SRYYMD )
                     AND ( MEDH20-ZAINUM (IDX2)
                                       =   SPA-NAI-TZAINUM  (IDX-LST))
                       MOVE    MEDH20-MEIL (IDX2)
                                       TO  SPA-GMN-TNAME   (IDX-LST)
                   END-IF
      *
               END-PERFORM
           WHEN    OTHER
               MOVE    MEDINF-SRYCD  (IDX0)
                                   TO  WRK-TNS-SRYCD
               MOVE    SPA-I40-SRYYMD  TO  WRK-TNS-KJNYMD
               MOVE    "01"        TO  WRK-TNS-KJNYMD (7:2)
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU   =   ZERO )
                   MOVE    TNS-NAME    TO  SPA-GMN-TNAME   (IDX-LST)
               END-IF
           END-EVALUATE
      *
           IF    ( SPA-GMN-TNAME   (IDX-LST)   =   SPACE )
               MOVE    MEDINF-SRYCD  (IDX0)
                                   TO  SPA-GMN-TNAME   (IDX-LST)
           END-IF
      *
           .
       31011-EDIT-RYOLST-MEDKBN-EXT.
           EXIT.
      *****************************************************************
      *    医療区分情報編集処理
      *****************************************************************
       31011-EDIT-MEDINF-SEC           SECTION.
      *
           INITIALIZE                  MEDINF-AREA
           MOVE    ZERO            TO  IDXM
      *
           EVALUATE    TRUE
           WHEN      ( SPA-I40-SRYYMD  >=  CONST-R060601 )
               IF      SPA-HOSPSBT     =   1
                   MOVE    1               TO  IDXX
               ELSE
                   MOVE    2               TO  IDXX
               END-IF              
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDR06-MAX )
                       OR    ( MEDR06-STYMD (IDXX IDX0)
                                       =   SPACE )
      *
                   IF    ( SPA-I40-SRYYMD  >=
                                          MEDR06-STYMD (IDXX IDX0) )
                    AND  ( SPA-I40-SRYYMD  <=
                                          MEDR06-EDYMD (IDXX IDX0) )
                       COMPUTE IDXM  = IDXM    +   1
                       MOVE  IDXM          TO  MEDINF-MAX
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
                       MOVE  MEDR06-CDL (IDXX IDX0)
                                           TO  MEDINF-SRYCD  (IDXM)
                       MOVE  FUNCTION NUMVAL (MEDR06-ID (IDXX IDX0))
                                           TO  MEDINF-NUM    (IDXM)
                   END-IF
      *
               END-PERFORM
           WHEN      ( SPA-I40-SRYYMD  >=  CONST-H280401 )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDH20-MAX )
      *
                   IF    ( SPA-I40-SRYYMD  >=  MEDH20-STYMD (IDX0) )
                    AND  ( SPA-I40-SRYYMD  <=  MEDH20-EDYMD (IDX0) )
                       COMPUTE IDXM  = IDXM    +   1
                       MOVE  IDXM          TO  MEDINF-MAX
                       MOVE  MEDH20-MEDKBN (IDX0)
                                           TO  MEDINF-MEDKBN (IDXM)
                       MOVE  MEDH20-LIMIT (IDX0)
                                           TO  MEDINF-LIMIT  (IDXM)
                       MOVE  MEDH20-ALLKBN (IDX0)
                                           TO  MEDINF-ALLKBN (IDXM)
                       MOVE  MEDH20-ZAINUM (IDX0)
                                           TO  MEDINF-ZAINUM (IDXM)
                       MOVE  MEDH20-CDL (IDX0)
                                           TO  MEDINF-SRYCD  (IDXM)
                       MOVE  FUNCTION NUMVAL (MEDH20-ID (IDX0))
                                           TO  MEDINF-NUM    (IDXM)
                   END-IF
      *
               END-PERFORM
           WHEN      ( SPA-I40-SRYYMD  >=  CONST-H240401 )
               MOVE    CONST-MEDH24-MAX    TO  MEDINF-MAX
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDINF-MAX )
                   MOVE  CONST-MEDH24-MEDH24(IDX0)
                                           TO  MEDINF-MEDKBN (IDX0)
                   MOVE  CONST-MEDH24-LIMIT (IDX0)
                                           TO  MEDINF-LIMIT  (IDX0)
                   MOVE  CONST-MEDH24-ALLKBN(IDX0)
                                           TO  MEDINF-ALLKBN (IDX0)
                   MOVE  CONST-MEDH24-ZAINUM(IDX0)
                                           TO  MEDINF-ZAINUM (IDX0)
                   MOVE  CONST-MEDH24-SRYCD (IDX0)
                                           TO  MEDINF-SRYCD  (IDX0)
                   MOVE  FUNCTION NUMVAL (CONST-MEDH24-ID (IDX0))
                                           TO  MEDINF-NUM    (IDX0)
               END-PERFORM
           WHEN      ( SPA-I40-SRYYMD  >=  CONST-H220401 )
               MOVE    CONST-MEDH22-MAX    TO  MEDINF-MAX
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDINF-MAX )
                   MOVE  CONST-MEDH22-MEDH22(IDX0)
                                           TO  MEDINF-MEDKBN (IDX0)
                   MOVE  CONST-MEDH22-LIMIT (IDX0)
                                           TO  MEDINF-LIMIT  (IDX0)
                   MOVE  CONST-MEDH22-ALLKBN(IDX0)
                                           TO  MEDINF-ALLKBN (IDX0)
                   MOVE  CONST-MEDH22-ZAINUM(IDX0)
                                           TO  MEDINF-ZAINUM (IDX0)
                   MOVE  CONST-MEDH22-SRYCD (IDX0)
                                           TO  MEDINF-SRYCD  (IDX0)
                   MOVE  FUNCTION NUMVAL (CONST-MEDH22-ID (IDX0))
                                           TO  MEDINF-NUM    (IDX0)
               END-PERFORM
           WHEN      ( SPA-I40-SRYYMD  >=  CONST-H200401 )
               MOVE    CONST-MEDH20-MAX    TO  MEDINF-MAX
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDINF-MAX )
                   MOVE  CONST-MEDH20-MEDH20(IDX0)
                                           TO  MEDINF-MEDKBN (IDX0)
                   MOVE  CONST-MEDH20-LIMIT (IDX0)
                                           TO  MEDINF-LIMIT  (IDX0)
                   MOVE  CONST-MEDH20-ALLKBN(IDX0)
                                           TO  MEDINF-ALLKBN (IDX0)
                   MOVE  CONST-MEDH20-ZAINUM(IDX0)
                                           TO  MEDINF-ZAINUM (IDX0)
                   MOVE  CONST-MEDH20-SRYCD (IDX0)
                                           TO  MEDINF-SRYCD  (IDX0)
                   MOVE  FUNCTION NUMVAL (CONST-MEDH20-ID (IDX0))
                                           TO  MEDINF-NUM   (IDX0)
               END-PERFORM
           WHEN    OTHER
               MOVE    CONST-MEDKBN-MAX    TO  MEDINF-MAX
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDINF-MAX )
                   MOVE  CONST-MEDKBN-MEDKBN(IDX0)
                                           TO  MEDINF-MEDKBN (IDX0)
                   MOVE  CONST-MEDKBN-LIMIT (IDX0)
                                           TO  MEDINF-LIMIT  (IDX0)
                   MOVE  CONST-MEDKBN-ALLKBN(IDX0)
                                           TO  MEDINF-ALLKBN (IDX0)
                   MOVE  CONST-MEDKBN-ZAINUM(IDX0)
                                           TO  MEDINF-ZAINUM (IDX0)
                   MOVE  CONST-MEDKBN-SRYCD (IDX0)
                                           TO  MEDINF-SRYCD  (IDX0)
                   MOVE  FUNCTION NUMVAL (CONST-MEDKBN-ID (IDX0))
                                           TO  MEDINF-NUM    (IDX0)
               END-PERFORM
           END-EVALUATE
      *
           .
       31011-EDIT-MEDINF-EXT.
           EXIT.
      *****************************************************************
      *    確認画面から戻り時の処理
      *****************************************************************
       330-I4ID1-SET-SEC               SECTION.
      *
           MOVE    SPA-IIDCD       TO  WRK-IIDCD
           MOVE    SPA-I4ID-FLG    TO  WRK-I4ID-FLG
      *
           MOVE    SPACE           TO  SPA-IIDCD
           MOVE    SPACE           TO  SPA-I4ID-FLG
      *
           IF    ( WRK-I4ID-FLG    =   "OK" OR "O2" )
               EVALUATE    WRK-IIDCD
               WHEN    "2001"
                   PERFORM 2121-TOUROKU-SEC
               WHEN    "2002"
                   PERFORM 2111-TOUROKU-SEC
               WHEN    "2003"
                   IF    ( WRK-I4ID-FLG    =   "OK" )
                       PERFORM 2111-TOUROKU-SEC
                   ELSE
                       PERFORM 21211-TOUROKU-SEC
                   END-IF
                   IF    ( SPA-ERRCD       =   SPACE )
                       PERFORM 2101-HYOKAHYO-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
           .
      *
       330-I4ID1-SET-EXT.
           .
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO      WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"     TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
      *    剤変更
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 4101-ZAIHENKOU-SEC
      *    評価票発行
           WHEN    "CLICKED"       ALSO    "B10"
               PERFORM 210-HYOKAHYO-SEC
      *    評価票発行（頭紙）
           WHEN    "CLICKED"       ALSO    "B10S"
               PERFORM 2102-HYOKAHYO-SEC
      *    入院料変更
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 211-TOUROKU-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 212-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           PERFORM   3101-EDIT-CONST-ADLKBN-SEC
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "GRPINPUT"
               PERFORM 4100-GRPINPUT-CHK-SEC
           WHEN        ANY         ALSO    "NYULIST"
               PERFORM 4101-NYULIST-SEL-SEC
           WHEN        ANY         ALSO    "RYOLIST"
               PERFORM 4101-RYOLIST-SEL-SEC
           WHEN    OTHER
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
       
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I41"               TO  SPA-SAKIPG
           MOVE    "I47"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           INITIALIZE                  SPA-GMN-AREA
                                       SPA-NAIB-AREA
           MOVE    ZERO            TO  SPA-GMN-RYOLIST-SEL
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    評価票発行処理
      *****************************************************************
       210-HYOKAHYO-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SELNUM  NOT =  SPACE )
                   MOVE    "1001"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *     関連チェックは保留（印刷して内容を確認する場合を考慮）
      *     IF    ( SPA-ERRCD       =   SPACE )
      *         PERFORM 410-KANREN-CHK-SEC
      *     END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 2101-PTSTS-CHK-SEC
      *
               IF    ( SPA-I47-CHG-FLG =   ZERO )
                AND  ( FLG-DIFFER      =   ZERO )
                   PERFORM 2101-HYOKAHYO-SEC
               ELSE
                   MOVE    "2003"      TO  SPA-IIDCD
               END-IF
           END-IF
      *
           .
       210-HYOKAHYO-EXT.
           EXIT.
      *****************************************************************
      *    患者状態チェック処理
      *****************************************************************
       2101-PTSTS-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-DIFFER
      *
           PERFORM 50011-EDIT-MEDADL-SEC
      *
           PERFORM 900-ORCSS011-CALL-SEC
           CALL    "ORCSS011"  USING   SS011-AREA
                                       SPA-AREA
      *     
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( SS011-DAY (IDX-DAY)
                                   NOT = SS011-DAY2 (IDX-DAY))
                   MOVE    1       TO  FLG-DIFFER
               END-IF
           END-PERFORM
      *
           .
       2101-PTSTS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養病棟入院基本料と医療区分、ＡＤＬ点数妥当性チェックサブ処理
      *****************************************************************
       900-ORCSS011-CALL-SEC               SECTION.
      *
           INITIALIZE          SS011-AREA
           MOVE    SPA-GMN-PTID    TO  SS011-PTID
           MOVE    SPA-I40-SRYYMD (1:6)
                                   TO  SS011-SRYYM
           MOVE    1               TO  SS011-DAYFLG
           MOVE    1               TO  SS011-MEDADLFLG
           MOVE    MEDKBN-SUMONFLG TO  SS011-SUMONFLG
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( SPA-I47-RYOYO-DAY (IDX-DAY) >  ZERO )
                   MOVE    SPA-I47-RYOYO-DAY (IDX-DAY)
                               TO  SS011-DAY (IDX-DAY)
                   MOVE    SPA-I47-RYOYO-KHN (IDX-DAY)
                               TO  SS011-KHN (IDX-DAY)
                   IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
                   AND   ( SPA-HOSPSBT     =   1             )
      *                疾患・状態に係る医療区分
                       EVALUATE    TRUE
                       WHEN  ( MEDKBN-DAYALL (1)        =  1 )
                       WHEN  ( MEDKBN-DAY (1 IDX-DAY)   =  1 )
                           MOVE    3   TO  SS011-MEDKBN (IDX-DAY)
                       WHEN  ( MEDKBN-DAYALL (2)        =  1 )
                       WHEN  ( MEDKBN-DAY (2 IDX-DAY)   =  1 )
                           MOVE    2   TO  SS011-MEDKBN (IDX-DAY)
                       WHEN    OTHER
                           MOVE    1   TO  SS011-MEDKBN (IDX-DAY)
                       END-EVALUATE
      *                処置等に係る医療区分                 
                       EVALUATE    TRUE
                       WHEN  ( MEDKBN-DAYALL (3)      =    1 )
                       WHEN  ( MEDKBN-DAY (3 IDX-DAY)   =  1 )
                           MOVE    3   TO  SS011-MEDKBN1 (IDX-DAY)
                       WHEN  ( MEDKBN-DAYALL (4)        =  1 )
                       WHEN  ( MEDKBN-DAY (4 IDX-DAY)   =  1 )
                           MOVE    2   TO  SS011-MEDKBN1 (IDX-DAY)
                       WHEN    OTHER
                           MOVE    1   TO  SS011-MEDKBN1 (IDX-DAY)
                       END-EVALUATE
                   ELSE    
                       EVALUATE    TRUE
                       WHEN  ( MEDKBN-DAYALL (1)        =  1 )
                       WHEN  ( MEDKBN-DAY (1 IDX-DAY)   =  1 )
                           MOVE    3   TO  SS011-MEDKBN (IDX-DAY)
                       WHEN  ( MEDKBN-DAYALL (2)        =  1 )
                       WHEN  ( MEDKBN-DAY (2 IDX-DAY)   =  1 )
                           MOVE    2   TO  SS011-MEDKBN (IDX-DAY)
                       WHEN    OTHER
                           MOVE    1   TO  SS011-MEDKBN (IDX-DAY)
                       END-EVALUATE
                   END-IF    
               END-IF
      *
               COMPUTE SS011-ADLTEN (IDX-DAY)
                   =   ADLTEN-DAY   (IDX-DAY)   +   ADLTEN-DAYALL
           END-PERFORM
      *
           .
       900-ORCSS011-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    評価票発行処理
      *****************************************************************
       2101-HYOKAHYO-SEC                 SECTION.
      *
           INITIALIZE                    ORCHCN68AREA
           MOVE    "1"             TO    ORCHCN68-PRTKBN
      *
           PERFORM 21011-HYOKAHYO-SEC
      *
           .
       2101-HYOKAHYO-EXT.
           EXIT.
      *****************************************************************
      *    評価票発行処理
      *****************************************************************
       21011-HYOKAHYO-SEC                 SECTION.
      *
           INITIALIZE                      ONPRTDATA
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
             OR  ( SPA-PRTCONF         =   "1" )
               INITIALIZE                  ORCSPRTCTRLAREA
               CALL    "ORCSPRTCTRL"       USING
                                           SPA-AREA
                                           ORCSPRTCTRLAREA
                                           MCPAREA
           END-IF
      *
           MOVE    SPA-SYSYMD      TO    SPA-SRYYMD
           MOVE    SPA-I40-SRYYMD (1:6)
                                   TO    ORCHCN68-SRYYM
           MOVE    SPA-GMN-PTID    TO    ORCHCN68-PTID
           MOVE    SPA-GMN-PTNUM   TO    ORCHCN68-PTNUM
           MOVE    ORCHCN68AREA    TO    ONPRTDATA-DATA
           MOVE    "01"            TO    SPA-PRT-FLG
           CALL    "ORCHCN68"      USING
                                   SPA-AREA
                                   ONPRTDATA
           MOVE    SPACE           TO    SPA-PRT-FLG
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO            TO  SPA-CLIENT-TEMP-FLG
      *
           .
       21011-HYOKAHYO-EXT.
           EXIT.
      *****************************************************************
      *    評価票発行処理
      *****************************************************************
       2102-HYOKAHYO-SEC                 SECTION.
      *
           INITIALIZE                    ORCHCN68AREA
           MOVE    "0"             TO    ORCHCN68-PRTKBN
      *
           PERFORM 21011-HYOKAHYO-SEC
      *
           .
       2102-HYOKAHYO-EXT.
           EXIT.
      *****************************************************************
      *    登録　処理
      *****************************************************************
       211-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SELNUM  NOT =  SPACE )
                   MOVE    "1001"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 410-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "2002"      TO  SPA-IIDCD
           END-IF
      *
           .
       211-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録　処理
      *****************************************************************
       2111-TOUROKU-SEC                SECTION.
      *
           PERFORM 21211-TOUROKU-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 21111-TOUROKU-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 310-SPASET-SEC
           END-IF
      *
           .
       2111-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録　処理
      *****************************************************************
       21111-TOUROKU-SEC               SECTION.
      *
      *
           PERFORM 50011-EDIT-MEDADL-SEC
      *
           PERFORM 900-ORCSS011-CALL-SEC
           MOVE    1                   TO  SS011-NYUUPDFLG
           CALL    "ORCSS011"      USING   SS011-AREA
                                           SPA-AREA
           IF    ( SS011-RC        NOT =   ZERO )
               MOVE    "4003"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               INITIALIZE                  SS010-AREA
               MOVE    SPA-GMN-PTID        TO  SS010-PTID
               MOVE    SPA-I40-SRYYMD (1:6)
                                           TO  SS010-SRYYM
               CALL    "ORCSS010"      USING   SS010-AREA
                                               SPA-AREA
               IF    ( SS010-RC        NOT =   ZERO )
                   MOVE    "4004"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       21111-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録　処理
      *****************************************************************
       212-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-SELNUM  NOT =  SPACE )
                   MOVE    "1001"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 410-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "2001"      TO  SPA-IIDCD
           END-IF
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録　処理
      *****************************************************************
       2121-TOUROKU-SEC                SECTION.
      *
           PERFORM 21211-TOUROKU-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE   "JOIN"       TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       2121-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録　処理
      *****************************************************************
       21211-TOUROKU-SEC               SECTION.
      *
           PERFORM 900-RYOCOM-DEL3-SEC
           IF    ( FLG-RYOCOM  NOT =   ZERO )
               MOVE    "4001"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-RYOLIST-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-TDAY-G (IDX-LST)    NOT =   ZERO )
                OR   ( SPA-NAI-TZAISKBKBN (IDX-LST)    =   "1" )
                   PERFORM 21211-RYOCOM-INS-SEC
               END-IF
      *
               IF    ( SPA-NAI-DESIGNR-EVAL-DAY-G  NOT =   ZERO )
                AND  ( SPA-NAI-TZAISKBKBN (IDX-LST)    =   "A" )
                   PERFORM 21212-RYOCOM-INS-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       21211-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ作成処理
      *****************************************************************
       21211-RYOCOM-INS-SEC            SECTION.
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  RYOCOM-PTID
           MOVE    SPA-I40-SRYYMD (1:6)
                                   TO  RYOCOM-SRYYM
           MOVE    SPA-NAI-TZAINUM (IDX-LST)
                                   TO  RYOCOM-ZAINUM
           MOVE    SPA-NAI-TZAISKBKBN (IDX-LST)
                                   TO  RYOCOM-ZAISKBKBN
           MOVE    SPA-NAI-TSRYCD (IDX-LST)
                                   TO  RYOCOM-SRYCD
           MOVE    SPA-GMN-TDAYALL (IDX-LST)
                                   TO  RYOCOM-MONTH
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               MOVE    SPA-GMN-TDAY (IDX-LST IDX-DAY)
                                   TO  RYOCOM-DAY (IDX-DAY)
           END-PERFORM
      *
           MOVE    SMCNDATE-YMD    TO  RYOCOM-CREYMD
                                       RYOCOM-UPYMD
           MOVE    SMCNDATE-HMS    TO  RYOCOM-UPHMS
      *
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4002"      TO  SPA-ERRCD
           END-IF
      *
           .
       21211-RYOCOM-INS-EXT.
           EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ作成処理
      *****************************************************************
       21212-RYOCOM-INS-SEC            SECTION.
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  RYOCOM-PTID
           MOVE    SPA-I40-SRYYMD (1:6)
                                   TO  RYOCOM-SRYYM
           MOVE    DESIGNR-ZAINUM-EVAL
                                   TO  RYOCOM-ZAINUM
           MOVE    SPA-NAI-TZAISKBKBN (IDX-LST)
                                   TO  RYOCOM-ZAISKBKBN
           MOVE    DESIGNR-SRYCD-EVAL
                                   TO  RYOCOM-SRYCD
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               MOVE    SPA-NAI-DESIGNR-EVAL-DAY (IDX-DAY)
                                   TO  RYOCOM-DAY (IDX-DAY)
           END-PERFORM
      *
           MOVE    SMCNDATE-YMD    TO  RYOCOM-CREYMD
                                       RYOCOM-UPYMD
           MOVE    SMCNDATE-HMS    TO  RYOCOM-UPHMS
      *
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4005"      TO  SPA-ERRCD
           END-IF
      *
           .
       21212-RYOCOM-INS-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    I47-DAYALL          TO  SPA-GMN-DAYALL
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               MOVE    I47-DAY (IDX-DAY)
                                       TO  SPA-GMN-DAY (IDX-DAY)
      *
           END-PERFORM
      *
           PERFORM 3101-EDIT-CONST-ADLKBN-SEC
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-GRPINPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-DAY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NISSU-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日別チェック処理
      *****************************************************************
       4101-DAY-CHK-SEC                SECTION.
      *
           INITIALIZE                  WRK-DAY-G
                                       SPA-NAI-DAY-G
      *
           MOVE    SPA-GMN-DAYALL  TO  WRK-TENSU-X
           PERFORM 4101-TENSU-LIMIT-CHK-SEC
           IF    ( FLG-LIMIT-ERR   =   ZERO )
               MOVE    WRK-TENSU   TO  SPA-NAI-DAYALL
           ELSE
               MOVE    "0006"      TO  SPA-ERRCD
               COMPUTE SPA-GMN-CUR =   100
               GO  TO  4101-DAY-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( IDX-DAY     >   SPA-I47-GETUMATU-DD )
                   MOVE    ZERO    TO  SPA-NAI-DAY (IDX-DAY)
               ELSE
                   IF    ( SPA-GMN-DAY (IDX-DAY)   NOT =   SPACE )
                    AND  ( SPA-I47-NYUIN-DAY (IDX-DAY) =   ZERO )
      *
                       MOVE    "0007"      TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR =   IDX-DAY     +   100
                   ELSE
      *
                       MOVE    SPA-GMN-DAY (IDX-DAY)
                                               TO  WRK-TENSU-X
                       PERFORM 4101-TENSU-LIMIT-CHK-SEC
                       IF    ( FLG-LIMIT-ERR   =   ZERO )
                           MOVE    WRK-TENSU   TO  SPA-NAI-DAY (IDX-DAY)
                       ELSE
                           MOVE    "0006"      TO  SPA-ERRCD
                           COMPUTE SPA-GMN-CUR =   IDX-DAY     +   100
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       4101-DAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-SELNUM-G
      *
           IF    ( SPA-GMN-SELNUM  =   SPACE )
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELNUM      TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-SELNUM
      *
           IF    ( SPA-GMN-SELNUM (1:1)    =   "." )
               MOVE    SPA-GMN-SELNUM (2:)
                                       TO  WRK-SELNUM-MUL
           ELSE
               MOVE    SPA-GMN-SELNUM  TO  WRK-SELNUM-MUL
           END-IF
      *
           PERFORM 41011-MULTI-SEL-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    1           TO  SPA-GMN-CUR
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号範囲チェック処理
      *****************************************************************
       4101-SELNUM-LIMIT-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-SELNUM
                                       WRK-SELNUM-IDX
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-SELNUM-X    TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
            AND  ( SNUM-MINKBN     =   SPACE )
            AND  ( SNUM-SYOKBN     =   SPACE ) 
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-RYOLIST-MAX )
                        OR   ( WRK-SELNUM  NOT =   ZERO )
                   IF    ( SPA-GMN-TNUM (IDX0) =   SNUM-OUTNUM )
                       MOVE    SNUM-OUTNUM
                                       TO  WRK-SELNUM
                       MOVE    IDX0    TO  WRK-SELNUM-IDX
                   END-IF
               END-PERFORM
               IF    ( WRK-SELNUM   =  ZERO )
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
      *
           ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    1       TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-SELNUM-LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括修正チェック処理
      *****************************************************************
       4101-GRPINPUT-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-GRPINPUT-G
      *
           MOVE    ZERO            TO  WRK-MAX
      *
           IF    ( SPA-GMN-GRPINPUT    =   SPACE )
               GO  TO  4101-GRPINPUT-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-GRPINPUT    TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-GRPINPUT
      *
      *
           INITIALIZE                  WRK-MULTI-AREA
      *
           UNSTRING    SPA-GMN-GRPINPUT    DELIMITED   BY  ","
                                                       OR  "."
           INTO        WRK-MUL-NUM (1)
                       WRK-MUL-NUM (2)
                       WRK-MUL-NUM (3)
                       WRK-MUL-NUM (4)
                       WRK-MUL-NUM (5)
                       WRK-MUL-NUM (6)
                       WRK-MUL-NUM (7)
                       WRK-MUL-NUM (8)
                       WRK-MUL-NUM (9)
                       WRK-MUL-NUM (10)
                       WRK-MUL-NUM (11)
                       WRK-MUL-NUM (12)
                       WRK-MUL-NUM (13)
                       WRK-MUL-NUM (14)
                       WRK-MUL-NUM (15)
                       WRK-MUL-NUM (16)
                       WRK-MUL-NUM (17)
                       WRK-MUL-NUM (18)
                       WRK-MUL-NUM (19)
                       WRK-MUL-NUM (20)
           ON  OVERFLOW
               MOVE    "0004"      TO  SPA-ERRCD  
               MOVE    2           TO  SPA-GMN-CUR
               GO  TO  4101-GRPINPUT-CHK-EXT
           END-UNSTRING
      *
           INITIALIZE  WRK-MUL-TENSU-X
      *
           PERFORM VARYING IDX-MUL     FROM    1   BY  1
                   UNTIL ( IDX-MUL     >       20 )
      *
               IF    ( WRK-MUL-NUM (IDX-MUL)   =   SPACE )
                   CONTINUE
               ELSE
      *
                   INITIALIZE  WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                               WRK-MUL-TENSU
                               WRK-MUL-STNUM
                               WRK-MUL-EDNUM
      *
                   MOVE        1       TO  IDX-PNT
      *
                   IF    ( IDX-MUL     =   1 )
                       UNSTRING    WRK-MUL-NUM (IDX-MUL)
                                                   DELIMITED   BY  "/"
                       INTO        WRK-MUL-TENSU-X
                       WITH    POINTER IDX-PNT
                       END-UNSTRING
      *
                       IF    ( IDX-PNT     =   1 )
                           MOVE    2               TO  SPA-GMN-CUR
                           MOVE    "0005"          TO  SPA-ERRCD
                           GO  TO  4101-GRPINPUT-CHK-EXT
                       END-IF
      *
                       EVALUATE    TRUE
                       WHEN  ( WRK-MUL-TENSU-X     =   SPACE )
                           MOVE    "1"             TO  WRK-MUL-TENSU-X
                                                       WRK-MUL-SYMBOL
                       WHEN  ( WRK-MUL-TENSU-X     =   "-" )
                           MOVE    "0"             TO  WRK-MUL-TENSU-X
                           MOVE    "-"             TO  WRK-MUL-SYMBOL
                       WHEN    OTHER
                           MOVE    WRK-MUL-TENSU-X TO  WRK-MUL-SYMBOL
                       END-EVALUATE
                   END-IF
      *
                   UNSTRING    WRK-MUL-NUM (IDX-MUL) DELIMITED   BY  "-"
                   INTO        WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                   WITH    POINTER IDX-PNT
                   END-UNSTRING
      *
                   IF    ( WRK-MUL-STNUM-X     =   SPACE )
                    AND  ( WRK-MUL-EDNUM-X     =   SPACE )
                       IF    ( WRK-MUL-TENSU-X =   SPACE )
                           MOVE    "0"         TO  WRK-MUL-STNUM-X
                                                   WRK-MUL-EDNUM-X
                       ELSE
                           MOVE    2           TO  SPA-GMN-CUR
                           MOVE    "0005"      TO  SPA-ERRCD
                           GO  TO  4101-GRPINPUT-CHK-EXT
                       END-IF
                   ELSE
                       EVALUATE    TRUE
                       WHEN  ( WRK-MUL-EDNUM-X     =   SPACE )
                           MOVE    WRK-MUL-STNUM-X
                                           TO  WRK-MUL-EDNUM-X
                       WHEN  ( WRK-MUL-STNUM-X     =   SPACE )
                           MOVE    2           TO  SPA-GMN-CUR
                           MOVE    "0005"      TO  SPA-ERRCD
                           GO  TO  4101-GRPINPUT-CHK-EXT
                       END-EVALUATE
                   END-IF
      *
                   MOVE    WRK-MUL-TENSU-X TO  WRK-TENSU-X
                   PERFORM 4101-TENSU-LIMIT-CHK-SEC
                   IF    ( FLG-LIMIT-ERR    NOT =   ZERO )
                       MOVE    2           TO  SPA-GMN-CUR
                       MOVE    "0005"  TO  SPA-ERRCD
                       GO  TO  4101-GRPINPUT-CHK-EXT
                   END-IF
                   MOVE    WRK-TENSU   TO  WRK-MUL-TENSU
      *
                   MOVE    WRK-MUL-STNUM-X TO  WRK-GRPINPUT-X
                   PERFORM 4101-DAY-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                           GO  TO  4101-GRPINPUT-CHK-EXT
                   END-IF
                   MOVE    WRK-GRPINPUT    TO  WRK-MUL-STNUM
      *
                   MOVE    WRK-MUL-EDNUM-X TO  WRK-GRPINPUT-X
                   PERFORM 4101-DAY-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                           GO  TO  4101-GRPINPUT-CHK-EXT
                   END-IF
                   MOVE    WRK-GRPINPUT    TO  WRK-MUL-EDNUM
      *
                   COMPUTE WRK-MAX =   WRK-MAX    +   1
      *
                   MOVE    WRK-MUL-TENSU   TO      WRK-GRPINPUT-TENSU
                                                          (WRK-MAX)
      *
                   PERFORM VARYING IDX-DAY FROM    WRK-MUL-STNUM   BY  1
                           UNTIL ( IDX-DAY >       WRK-MUL-EDNUM )
      *
                       IF    ( IDX-DAY     =   ZERO )
                           MOVE    1       TO      WRK-GRPINPUT-DAYALL
                                                               (WRK-MAX)
                       ELSE
                           MOVE    1       TO      WRK-GRPINPUT-DAY
                                                       (WRK-MAX IDX-DAY)
                       END-IF
      *
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-MAX         TO  WRK-GRPINPUT-MAX
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-GRPINPUT-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( WRK-GRPINPUT-DAYALL (IDX0)  =   1 )
                AND  ( WRK-GRPINPUT-TENSU(IDX0)    >   ZERO )
                AND  ( SPA-I47-NYUIN-DAY-G         =   ZERO )
                    MOVE    "0007"     TO  SPA-ERRCD
                    MOVE    2          TO  SPA-GMN-CUR
               END-IF
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( WRK-GRPINPUT-DAY (IDX0 IDX-DAY)  =   1 )
                    AND  ( WRK-GRPINPUT-TENSU(IDX0)         >   ZERO )
                    AND  ( SPA-I47-NYUIN-DAY( IDX-DAY)      =   ZERO )
                        MOVE    "0007"     TO  SPA-ERRCD
                        MOVE    2          TO  SPA-GMN-CUR
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           .
       4101-GRPINPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数範囲チェック処理
      *****************************************************************
       4101-TENSU-LIMIT-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-LIMIT-ERR
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-TENSU-X     TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
            AND  ( SNUM-MINKBN     =   SPACE )
            AND  ( SNUM-SYOKBN     =   SPACE )
               MOVE    SNUM-OUTNUM TO  WRK-TENSU
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-INPUT-MODE  =   2 )
               WHEN  ( SPA-NAI-INPUT-MODE  =   1 )
                 AND ( SPA-GMN-RYOLIST-SEL >   ZERO )
                 AND ( SPA-NAI-TZAISKBKBN (SPA-GMN-RYOLIST-SEL)
                                       NOT =   "A" )
                   IF    ( WRK-TENSU  >=  ZERO )
                    AND  ( WRK-TENSU  <=  CONST-ADLTEN-MAX )
                       CONTINUE
                   ELSE
                       MOVE    1       TO  FLG-LIMIT-ERR
                   END-IF
               WHEN  ( SPA-NAI-INPUT-MODE  =   1 )
                 AND ( SPA-GMN-RYOLIST-SEL >   ZERO )
                 AND ( SPA-NAI-TZAISKBKBN (SPA-GMN-RYOLIST-SEL)
                                       =   "A" )
                   IF    ( WRK-TENSU  >=  ZERO )
                    AND  ( WRK-TENSU  <=  CONST-DESIGNRTEN-MAX )
                       CONTINUE
                   ELSE
                       MOVE    1       TO  FLG-LIMIT-ERR
                   END-IF
               END-EVALUATE
           ELSE
                   MOVE    1       TO  FLG-LIMIT-ERR
           END-IF
      *
           .
       4101-TENSU-LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付範囲チェック処理
      *****************************************************************
       4101-DAY-LIMIT-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-GRPINPUT-X  TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
            AND  ( SNUM-MINKBN     =   SPACE )
            AND  ( SNUM-SYOKBN     =   SPACE ) 
      *
               IF    ( SNUM-OUTNUM =   99 )
                   MOVE    SPA-I47-GETUMATU-DD
                                   TO  WRK-GRPINPUT
               ELSE
                   MOVE    SNUM-OUTNUM
                                   TO  WRK-GRPINPUT
               END-IF
      *
               IF    ( WRK-GRPINPUT    >=  ZERO )
                AND  ( WRK-GRPINPUT    <=  SPA-I47-GETUMATU-DD )
                   CONTINUE
               ELSE
                   MOVE    2           TO  SPA-GMN-CUR
                   MOVE    "0005"  TO  SPA-ERRCD
               END-IF
      *
           ELSE
                   MOVE    2           TO  SPA-GMN-CUR
                   MOVE    "0005"  TO  SPA-ERRCD
           END-IF
      *
           .
       4101-DAY-LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数行選択チェック処理
      *****************************************************************
       41011-MULTI-SEL-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-MULTI-AREA
      *
           MOVE    ZERO            TO  FLG-DESIGNR
           MOVE    ZERO            TO  WRK-INPUT-MODE
      *
      *    入力がないとき
           IF    ( WRK-SELNUM-MUL  =   SPACE )
               MOVE    1           TO  SPA-GMN-CUR
               MOVE    "0001"      TO  SPA-ERRCD
               GO  TO  41011-MULTI-SEL-CHK-EXT
           END-IF
      *
           UNSTRING    WRK-SELNUM-MUL  DELIMITED   BY  ","
                                                   OR  "."
           INTO        WRK-MUL-NUM (1)
                       WRK-MUL-NUM (2)
                       WRK-MUL-NUM (3)
                       WRK-MUL-NUM (4)
                       WRK-MUL-NUM (5)
                       WRK-MUL-NUM (6)
                       WRK-MUL-NUM (7)
                       WRK-MUL-NUM (8)
                       WRK-MUL-NUM (9)
                       WRK-MUL-NUM (10)
                       WRK-MUL-NUM (11)
                       WRK-MUL-NUM (12)
                       WRK-MUL-NUM (13)
                       WRK-MUL-NUM (14)
                       WRK-MUL-NUM (15)
                       WRK-MUL-NUM (16)
                       WRK-MUL-NUM (17)
                       WRK-MUL-NUM (18)
                       WRK-MUL-NUM (19)
                       WRK-MUL-NUM (20)
           ON  OVERFLOW
               MOVE    1           TO  SPA-GMN-CUR
               MOVE    "0002"      TO  SPA-ERRCD  
               GO  TO  41011-MULTI-SEL-CHK-EXT
           END-UNSTRING
      *
           PERFORM VARYING IDX-MUL    FROM    1   BY  1
                   UNTIL ( IDX-MUL    >       20 )
      *
               IF    ( WRK-MUL-NUM (IDX-MUL)  =   SPACE )
                   CONTINUE
               ELSE
      *
                   INITIALIZE  WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                               WRK-MUL-STNUM
                               WRK-MUL-EDNUM
      *
                   UNSTRING    WRK-MUL-NUM (IDX-MUL) DELIMITED   BY  "-"
                   INTO        WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                   END-UNSTRING
      *
                   IF    ( WRK-MUL-STNUM-X     =   SPACE )
                    AND  ( WRK-MUL-EDNUM-X     =   SPACE )
                       MOVE    1           TO  SPA-GMN-CUR
                       MOVE    "0001"      TO  SPA-ERRCD
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   ELSE
                       EVALUATE    TRUE
                       WHEN  ( WRK-MUL-EDNUM-X     =   SPACE )
                           MOVE    WRK-MUL-STNUM-X
                                           TO  WRK-MUL-EDNUM-X
                       WHEN  ( WRK-MUL-STNUM-X     =   SPACE )
                           MOVE    WRK-MUL-EDNUM-X
                                           TO  WRK-MUL-STNUM-X
                       END-EVALUATE
                   END-IF
      *
                   MOVE    WRK-MUL-STNUM-X     TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM-IDX  TO  WRK-MUL-STNUM
      *
                   MOVE    WRK-MUL-EDNUM-X TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM-IDX  TO  WRK-MUL-EDNUM
      *
                   PERFORM VARYING IDX1    FROM    WRK-MUL-STNUM   BY  1
                           UNTIL ( IDX1    >       WRK-MUL-EDNUM )
                       IF    ( IDX1        >=      1 )
                        AND  ( IDX1        <=      SPA-GMN-RYOLIST-MAX )
      *                     IF    ( SPA-NAI-TZAINUM (IDX1)  =   2039 )
      *                         CONTINUE
      *                     ELSE
                            MOVE    1       TO  WRK-SELNUM-FLG (IDX1)
                            IF    ( SPA-NAI-TZAISKBKBN (IDX1)  =   "A" )
                               MOVE    1    TO  FLG-DESIGNR
                            END-IF
      *                     END-IF
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  CNT-DAY
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-RYOLIST-MAX )
      *
               IF    ( WRK-SELNUM-FLG (IDX1)   =   1 )
                   COMPUTE CNT-DAY =   CNT-DAY +   1
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-DAY         >   1 )
               MOVE    2           TO  WRK-INPUT-MODE
           ELSE
               MOVE    1           TO  WRK-INPUT-MODE
           END-IF
      *
           IF    ( WRK-INPUT-MODE  =   2 )
            AND  ( FLG-DESIGNR     =   1 )
               MOVE    "0008"      TO  SPA-ERRCD
           END-IF
      *
           .
       41011-MULTI-SEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       410-KANREN-CHK-SEC              SECTION.
      *
      *
           .
       410-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           MOVE    ZERO            TO  SPA-NAI-INPUT-MODE
      *
           MOVE    I47-SELNUM      TO  SPA-GMN-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    WRK-INPUT-MODE  TO  SPA-NAI-INPUT-MODE
      *
           EVALUATE    SPA-NAI-INPUT-MODE
      *
           WHEN    1
               MOVE    WRK-SELNUM-IDX
                                   TO  SPA-GMN-RYOLIST-SEL
      *
               PERFORM 41011-EDIT-DAYAREA-SEC
      *
           WHEN    2
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-RYOLIST-MAX )
                   MOVE    WRK-SELNUM-FLG (IDX0)
                                   TO  SPA-NAI-SELNUM-FLG (IDX0)
               END-PERFORM
           END-EVALUATE
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括入力チェック処理
      *****************************************************************
       4100-GRPINPUT-CHK-SEC             SECTION.
      *
           MOVE    I47-GRPINPUT    TO  SPA-GMN-GRPINPUT
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-GRPINPUT-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-SELNUM  =   SPACE )
               MOVE    "0003"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-CUR
               GO  TO  4100-GRPINPUT-CHK-EXT
           END-IF
      *
           PERFORM 4101-GRPINPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-GRPINPUT-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-GRPINPUT-MAX )
      *
               IF    ( WRK-GRPINPUT-DAYALL (IDX0) =   1 )
                   MOVE    WRK-GRPINPUT-TENSU (IDX0)
                                       TO  SPA-NAI-DAYALL
                   IF    ( SPA-NAI-INPUT-MODE     =   1 )
                       MOVE    WRK-GRPINPUT-TENSU (IDX0)
                                       TO  WRK-ZZ
                       MOVE    WRK-ZZ  TO  SPA-GMN-DAYALL
                   ELSE
                       MOVE    WRK-GRPINPUT-TENSU (IDX0)
                                       TO  WRK-Z9
                       MOVE    WRK-Z9  TO  SPA-GMN-DAYALL
                   END-IF
               END-IF
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   IF    ( WRK-GRPINPUT-DAY (IDX0 IDX-DAY)  =   1 )
                       MOVE    WRK-GRPINPUT-TENSU (IDX0)
                                       TO  SPA-NAI-DAY (IDX-DAY)
      *
                       MOVE    ZERO    TO  FLG-SHOW-ZERO
      *
                       EVALUATE    TRUE
                       WHEN  ( SPA-NAI-INPUT-MODE  =   2 )
                       WHEN  ( SPA-GMN-RYOLIST-SEL >   ZERO )
                         AND ( SPA-NAI-TZAISKBKBN (SPA-GMN-RYOLIST-SEL)
                                                   =   "A" )
                           IF    ( WRK-MUL-TENSU       =   ZERO )
                            AND  ( WRK-MUL-SYMBOL  NOT =   "-" )
                               MOVE    1           TO  FLG-SHOW-ZERO
                           END-IF
                       END-EVALUATE
      *
                       IF    ( FLG-SHOW-ZERO       =   1 )
                           MOVE    WRK-GRPINPUT-TENSU (IDX0)
                                           TO  WRK-Z9
                           MOVE    WRK-Z9  TO  SPA-GMN-DAY (IDX-DAY)
      *
                       ELSE
                           MOVE    WRK-GRPINPUT-TENSU (IDX0)
                                           TO  WRK-ZZ
                           MOVE    WRK-ZZ  TO  SPA-GMN-DAY (IDX-DAY)
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-GMN-GRPINPUT
      *
           .
       4100-GRPINPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コラムリスト選択処理
      *****************************************************************
       4101-NYULIST-SEL-SEC            SECTION.
      *
           IF    ( SPA-I47-ADLINPUTKBN     =   "1" )
               CONTINUE
           ELSE
               IF    ( I47-NYULIST-SELECT (3)  =   "T" )
      *
                   PERFORM 220-CLEAR-SEC
      *
                   MOVE    2           TO  FLG-RYOLIST-ROW
                   MOVE    1           TO  SPA-NAI-INPUT-MODE
                   MOVE    SPA-NAI-RYOLIST-ADL-STIDX
                                       TO  SPA-GMN-RYOLIST-SEL
                   PERFORM 41011-EDIT-DAYAREA-SEC
               END-IF
           END-IF
      *
           .
       4101-NYULIST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    コラムリスト選択処理
      *****************************************************************
       4101-RYOLIST-SEL-SEC            SECTION.
      *
           PERFORM 220-CLEAR-SEC
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   I47-RYOLIST-COUNT )
                    OR   ( SPA-GMN-RYOLIST-SEL >   ZERO )
      *
               IF    ( I47-RYOLIST-SELECT (IDX-LST)    =   "T" )
      *
                   IF    ( I47-TNUM (IDX-LST)  =   SPACE )
                       MOVE    I47-RYOLIST-COUNT
                                       TO  IDX-LST
                   ELSE
                       MOVE    1       TO  SPA-NAI-INPUT-MODE
                       PERFORM VARYING IDXA    FROM    1   BY  1
                               UNTIL ( IDXA    >   SPA-GMN-RYOLIST-MAX )
                           IF    ( SPA-GMN-TNUM (IDXA)
                               =   FUNCTION NUMVAL (I47-TNUM (IDX-LST)))
                               MOVE    IDXA    TO  SPA-GMN-RYOLIST-SEL
                               MOVE    1       TO  SPA-NAI-INPUT-MODE
                               PERFORM 41011-EDIT-DAYAREA-SEC
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       4101-RYOLIST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    日付エリア編集処理
      *****************************************************************
       41011-EDIT-DAYAREA-SEC          SECTION.
      *
           MOVE    SPA-GMN-RYOLIST-SEL
                                   TO  IDX-SEL
      *
           MOVE    SPACE           TO  SPA-GMN-MEISYO
      *
           EVALUATE    SPA-NAI-TZAISKBKBN(IDX-SEL)
           WHEN    "1"
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-ADLKBN-MAX )
                        OR   ( SPA-GMN-MEISYO
                                   NOT =   SPACE )
                   IF    ( SPA-NAI-TZAINUM (IDX-SEL)
                                       =   CONST-ADLKBN-ZAINUM (IDX0))
                       MOVE    CONST-ADLKBN-NAME (IDX0)
                                   TO  SPA-GMN-MEISYO
                   END-IF
               END-PERFORM
           WHEN    "2"
               IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
                   IF      SPA-HOSPSBT     =   1
                       MOVE    1               TO  IDXX
                   ELSE
                       MOVE    2               TO  IDXX
                   END-IF              
                   PERFORM VARYING IDX2 FROM    1   BY  1
                           UNTIL ( IDX2 >   MEDR06-MAX )
                           OR   ( MEDR06-STYMD (IDXX IDX2)
                                               =   SPACE )
                            OR   ( SPA-GMN-MEISYO
                                           NOT =   SPACE )
                       IF    ( MEDR06-STYMD  (IDXX IDX2)
                                      <=   SPA-I40-SRYYMD )
                         AND ( MEDR06-EDYMD  (IDXX IDX2)
                                      >=   SPA-I40-SRYYMD )
                         AND ( MEDR06-ZAINUM (IDXX IDX2)
                                       =   SPA-NAI-TZAINUM  (IDX-SEL))
                           MOVE    MEDR06-MEIL (IDXX IDX2)
                                       TO  SPA-GMN-MEISYO
                       END-IF
      *
                   END-PERFORM
               ELSE
                 IF    ( SPA-I40-SRYYMD  >=  CONST-H200401 )
                   PERFORM VARYING IDX2 FROM    1   BY  1
                           UNTIL ( IDX2 >   MEDH20-MAX )
                            OR   ( SPA-GMN-MEISYO
                                           NOT =   SPACE )
                       IF    ( MEDH20-STYMD  (IDX2)
                                      <=   SPA-I40-SRYYMD )
                         AND ( MEDH20-EDYMD  (IDX2)
                                      >=   SPA-I40-SRYYMD )
                         AND ( MEDH20-ZAINUM (IDX2)
                                       =   SPA-NAI-TZAINUM  (IDX-SEL))
                           MOVE    MEDH20-MEIL (IDX2)
                                       TO  SPA-GMN-MEISYO
                       END-IF
      *
                   END-PERFORM
                 ELSE
                   MOVE    SPA-NAI-TSRYCD (IDX-SEL)
                                   TO  WRK-TNS-SRYCD
                   MOVE    SPA-I40-SRYYMD
                                   TO  WRK-TNS-KJNYMD
                   MOVE    "01"    TO  WRK-TNS-KJNYMD (7:2)
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-NAME TO  SPA-GMN-MEISYO
                 END-IF
               END-IF  
           WHEN    "A"
               MOVE    DESIGNR-MEI  TO  SPA-GMN-MEISYO
           END-EVALUATE
      *
           MOVE    SPA-GMN-TNUM (IDX-SEL)
                                   TO  WRK-ZZZ
           MOVE    FUNCTION TRIM(WRK-ZZZ-G)
                                   TO  SPA-GMN-SELNUM
      *
           MOVE    SPA-GMN-TDAYALL (IDX-SEL)
                                   TO  WRK-ZZ
           MOVE    WRK-ZZ          TO  SPA-GMN-DAYALL
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               IF    ( SPA-NAI-TZAISKBKBN(IDX-SEL) = "A" )
                   IF    ( SPA-NAI-DESIGNR-EVAL-DAY (IDX-DAY) > ZERO )
                       MOVE    SPA-GMN-TDAY  (IDX-SEL IDX-DAY)
                                       TO  WRK-Z9
                       MOVE    WRK-Z9  TO  SPA-GMN-DAY (IDX-DAY)
                   END-IF
               ELSE
                   MOVE    SPA-GMN-TDAY  (IDX-SEL IDX-DAY)
                                       TO  WRK-ZZ
                   MOVE    WRK-ZZ      TO  SPA-GMN-DAY (IDX-DAY)
               END-IF
      *
           END-PERFORM
      *
           MOVE    2               TO  SPA-GMN-CUR
      *
           .
       41011-EDIT-DAYAREA-EXT.
           EXIT.
      *****************************************************************
      *    剤変更処理
      *****************************************************************
       4101-ZAIHENKOU-SEC              SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4101-ZAIHENKOU-EXT
           END-IF
      *
           IF    ( SPA-GMN-RYOLIST-SEL >=  1)
            AND  ( SPA-GMN-RYOLIST-SEL <=  SPA-GMN-RYOLIST-MAX )
               MOVE    1               TO  SPA-NAI-SELNUM-FLG
                                             (SPA-GMN-RYOLIST-SEL)
           END-IF
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-RYOLIST-MAX )
      *
               IF    (SPA-NAI-SELNUM-FLG (IDX-LST)  =  1 )
                   PERFORM 41011-ZAIHENKOU-SEC
                   MOVE    1           TO  SPA-I47-CHG-FLG
               END-IF
      *
           END-PERFORM
      *
           PERFORM 220-CLEAR-SEC
      *
           .
       4101-ZAIHENKOU-EXT.
           EXIT.
      *****************************************************************
      *    日数制限チェック処理
      *****************************************************************
       4101-NISSU-CHK-SEC              SECTION.
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-RYOLIST-MAX )
      *
               IF    ( SPA-NAI-SELNUM-FLG (IDX-LST)  =  1 )
                OR   ( IDX-LST             =   SPA-GMN-RYOLIST-SEL )
                   IF    ( SPA-NAI-TZAINUM (IDX-LST)   =    2004
                                                       OR   2020
                                                       OR   2022
                                                       OR   2027
                                                       OR   2031 )
      *
                       MOVE    ZERO    TO  CNT-NISSU
      *
                       MOVE    SPA-I47-ZENGETUMATU-YM
                                           TO  WRK-SRYYM
                       MOVE    SPA-NAI-TZAINUM (IDX-LST)
                                           TO  WRK-ZAINUM
                       PERFORM 900-RYOCOM-KEY-SEL-SEC
                       IF    ( FLG-RYOCOM  =   ZERO )
                           COMPUTE WRK-STIDX
                               =   SPA-I47-ZENGETUMATU-DD
                           MOVE    ZERO    TO  WRK-EDIDX
                           IF    ( SPA-NAI-TZAINUM (IDX-LST)
                                                       =   2020 )
                               COMPUTE WRK-EDIDX
                                   =   SPA-I47-ZENGETUMATU-DD
                                   -   13
                           ELSE
                               COMPUTE WRK-EDIDX
                                   =   SPA-I47-ZENGETUMATU-DD
                                   -   6
                           END-IF
                           PERFORM VARYING IDX-DAY
                                   FROM    WRK-STIDX   BY  -1
                                   UNTIL ( IDX-DAY     <   WRK-EDIDX )
                                    OR   ( RYOCOM-DAY (IDX-DAY)
                                                       =   ZERO )
      *
                               COMPUTE CNT-NISSU   =   CNT-NISSU   +   1
      *
                           END-PERFORM
                       END-IF
      *
                       PERFORM 41011-NISSU-CHK-SEC
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       4101-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数制限チェック処理
      *****************************************************************
       41011-NISSU-CHK-SEC             SECTION.
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
                    OR   ( SPA-ERRCD   NOT = SPACE )
      *
               IF    ( SPA-GMN-DAY (IDX-DAY)   = SPACE )
                   IF    ( SPA-NAI-INPUT-MODE  =   1 )
                       MOVE    ZERO    TO  CNT-NISSU
                   ELSE
                       IF    ( SPA-GMN-TDAY (IDX-LST IDX-DAY)
                                               =   ZERO )
                           MOVE    ZERO    TO  CNT-NISSU
                       ELSE
                           COMPUTE CNT-NISSU
                               =   CNT-NISSU   +   1
      *
                       END-IF
                   END-IF
               ELSE
                   IF    ( FUNCTION NUMVAL (SPA-GMN-DAY (IDX-DAY))
                                       >   ZERO )
                       COMPUTE CNT-NISSU   =   CNT-NISSU   +   1
                   END-IF
               END-IF
      *
               IF    ( SPA-NAI-TZAINUM (IDX-LST)   =   2020 )
                   IF    ( CNT-NISSU   >   14 )
                       IF    ( SPA-NAI-INPUT-MODE  =   1 )
                           MOVE    "0101"  TO  SPA-ERRCD
                       ELSE
                           MOVE    "0102"  TO  SPA-ERRCD
                           MOVE    SPA-GMN-TNAME (IDX-LST)
                                           TO WRK-ERRMSG
                       END-IF
                   END-IF
               ELSE
                   IF    ( CNT-NISSU   >   7 )
                       IF    ( SPA-NAI-INPUT-MODE  =   1 )
                           MOVE    "0103"  TO  SPA-ERRCD
                       ELSE
                           MOVE    "0104"  TO  SPA-ERRCD
                           MOVE    SPA-GMN-TNAME (IDX-LST)
                                           TO WRK-ERRMSG
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   COMPUTE SPA-GMN-CUR =   IDX-DAY +   100
               END-IF
      *
           END-PERFORM
      *
           .
       41011-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    剤変更処理
      *****************************************************************
       41011-ZAIHENKOU-SEC             SECTION.
      *
           IF    ( SPA-NAI-TZAISKBKBN(IDX-LST) =   "A" )
               MOVE    ZERO            TO  SPA-NAI-DESIGNR-EVAL-DAY-G
           END-IF
      *
           IF    ( SPA-NAI-TALLKBN (IDX-LST)   =   1 )
      *
               MOVE    ZERO                TO  WRK-DAY
      *
               IF    ( SPA-NAI-INPUT-MODE  =   1 )
                   IF    ( SPA-NAI-DAYALL      >   ZERO )
                       IF    ( SPA-NAI-TZAISKBKBN (IDX-LST)    =   "1" )
                           MOVE    SPA-NAI-DAYALL  TO  WRK-DAY
                       ELSE
                           MOVE    1       TO  WRK-DAY
                       END-IF
                   END-IF
      *
                   MOVE    WRK-DAY         TO  SPA-GMN-TDAYALL (IDX-LST)
               ELSE
                   IF    ( SPA-GMN-DAYALL  NOT =   SPACE )
                       IF    ( SPA-NAI-DAYALL      =   ZERO )
                               MOVE    ZERO            TO  WRK-DAY
                       ELSE
                           IF    ( SPA-NAI-TZAISKBKBN (IDX-LST)
                                                   =   "1" )
                               MOVE    SPA-NAI-DAYALL  TO  WRK-DAY
                           ELSE
                               MOVE    1               TO  WRK-DAY
                           END-IF
                       END-IF
      *
                       MOVE    WRK-DAY     TO  SPA-GMN-TDAYALL (IDX-LST)
      *
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SPA-NAI-TALLKBN (IDX-LST)   =   ZERO )
            OR  (( SPA-NAI-TZAISKBKBN(IDX-LST) =   "1" )
            AND  ( SPA-NAI-TALLKBN (IDX-LST)   =   1 )
            AND  ( SPA-NAI-DAYALL              =   ZERO ))
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   MOVE    ZERO            TO  WRK-DAY
      *
                   IF    ( SPA-NAI-INPUT-MODE  =   1 )
                       EVALUATE    SPA-NAI-TZAISKBKBN(IDX-LST)
                       WHEN    "1"
                           IF    ( SPA-NAI-DAY (IDX-DAY) > ZERO )
                               MOVE    SPA-NAI-DAY (IDX-DAY)
                                           TO  WRK-DAY
                           END-IF
                       WHEN    "A"
                           IF    ( SPA-GMN-DAY (IDX-DAY)
                                           NOT = SPACE )
                               MOVE    SPA-NAI-DAY (IDX-DAY)
                                           TO  WRK-DAY
                               MOVE    1   TO  SPA-NAI-DESIGNR-EVAL-DAY
                                                         (IDX-DAY)
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-NAI-DAY (IDX-DAY) > ZERO )
                               MOVE    1   TO  WRK-DAY
                           END-IF
                       END-EVALUATE
      *
                       MOVE    WRK-DAY         TO  SPA-GMN-TDAY
                                                   (IDX-LST IDX-DAY)
                   ELSE
                       IF    ( SPA-GMN-DAY (IDX-DAY)
                                               NOT =   SPACE )
                           IF    ( SPA-NAI-DAY (IDX-DAY)   =   ZERO)
                               MOVE    ZERO    TO  WRK-DAY
                           ELSE
                               IF    ( SPA-NAI-TZAISKBKBN(IDX-LST)
                                                           =   "1" )
                                   MOVE    SPA-NAI-DAY (IDX-DAY)
                                               TO  WRK-DAY
                               ELSE
                                   MOVE    1   TO  WRK-DAY
                               END-IF
                           END-IF
      *
                           MOVE    WRK-DAY     TO  SPA-GMN-TDAY
                                                   (IDX-LST IDX-DAY)
      *
                       END-IF
                   END-IF
      *
               END-PERFORM
           ELSE
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   MOVE    ZERO            TO  SPA-GMN-TDAY
                                                   (IDX-LST IDX-DAY)
               END-PERFORM
           END-IF
      *
           
      *
           .
       41011-ZAIHENKOU-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-IIDCD       NOT =   SPACE )
               EVALUATE    SPA-IIDCD
               WHEN    "2003"
                   PERFORM 521-I4IDSET-SEC
               WHEN    OTHER
                   PERFORM 520-I4IDSET-SEC
               END-EVALUATE
      *
               GO  TO  500-SET-SCREEN-EXT
      *
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I47    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    I47-RYOLIST-ROW TO  WRK-RYOLIST-ROW
           MOVE    I47-NYULIST-ROW TO  WRK-NYULIST-ROW
      *
           MOVE    SPACE               TO  I47
           INITIALIZE                      I47
      *
           MOVE    ZERO                TO  FLG-MEDKBN3
                                           FLG-MEDKBN3LMT
                                           FLG-MEDKBN2
                                           FLG-MEDKBN2LMT
                                           FLG-ADLKBN
                                           FLG-MEDCOM
                                           FLG-MEDMORE
      *
           MOVE    SPA-GMN-PTNUM       TO  I47-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  I47-KANANAME
           MOVE    SPA-GMN-NAME        TO  I47-NAME
           MOVE    SPA-GMN-SEX         TO  I47-SEX
           MOVE    SPA-I40-SRYYMDW(1:6)    TO  I47-SRYYM
           MOVE    SPA-GMN-BIRTHDAYW   TO  I47-BIRTHDAY
           MOVE    SPA-GMN-NYUINBI     TO  I47-NYUINYMD
           MOVE    SPA-GMN-TAINBI      TO  I47-TAIINYMD
      *
           MOVE    SPA-GMN-SELNUM      TO  I47-SELNUM
           MOVE    SPA-GMN-MEISYO      TO  I47-MEISYO
           MOVE    SPA-GMN-GRPINPUT    TO  I47-GRPINPUT
      *
           MOVE    SPA-GMN-DAYALL      TO  I47-DAYALL
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
      *
               MOVE    SPA-GMN-DAY (IDX-DAY)
                                       TO  I47-DAY (IDX-DAY)
      *
           END-PERFORM
      *
           MOVE    I47-GRPINPUT        TO  I47-GRPINPUT
      *
           MOVE    ZERO            TO  IDX-LST
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-RYOLIST-MAX )
                    OR   ( IDX-LST >=  CONST-RYOLIST-MAX )
      *
               COMPUTE IDX-LST =   IDX-LST +   1
      *
               EVALUATE    TRUE
               WHEN  ( SPA-I40-SRYYMD  >=  CONST-R060601 )
      *
                   EVALUATE    TRUE
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   3 )
                    AND  ( SPA-NAI-TLIMIT  (IDX0)  >   0 )
                    AND  ( FLG-MEDKBN3LMT          =   0 )
                       IF    ( FLG-MEDKBN3LMT      =   0 )
                        AND  ( FLG-MEDKBN2LMT      =   0 )
                           MOVE    "Ｉ　算定期間に限りがある区分"
                                   TO  I47-TNAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN3LMT
                       IF      SPA-NAI-TDSPKBN (IDX0)  =   "1" OR  "9"
                           EVALUATE    SPA-NAI-TMEDKBN1 (IDX0)
                               WHEN    "0"
                                   MOVE    "【医療区分３】"
                                               TO  I47-TNAME(IDX-LST)
                               WHEN    "1"
                                   MOVE
                                       "【疾患・状態に係る医療区分３】"
                                               TO  I47-TNAME(IDX-LST)
                               WHEN    "2"
                                   MOVE    "【処置等に係る医療区分３】"
                                               TO  I47-TNAME(IDX-LST)
                           END-EVALUATE                    
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF    
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   2 )
                    AND  ( SPA-NAI-TLIMIT  (IDX0)  >   0 )
                    AND  ( FLG-MEDKBN2LMT          =   0 )
                       IF    ( FLG-MEDKBN3LMT      =   0 )
                        AND  ( FLG-MEDKBN2LMT      =   0 )
                           MOVE    "Ｉ　算定期間に限りがある区分"
                                   TO  I47-TNAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN2LMT
                       IF      SPA-NAI-TDSPKBN (IDX0)  =   "1" OR  "9"
                           EVALUATE    SPA-NAI-TMEDKBN1 (IDX0)
                               WHEN    "0"
                                   MOVE    "【医療区分２】"
                                               TO  I47-TNAME(IDX-LST)
                               WHEN    "1"
                                   MOVE
                                       "【疾患・状態に係る医療区分２】"
                                               TO  I47-TNAME(IDX-LST)
                               WHEN    "2"
                                   MOVE    "【処置等に係る医療区分２】"
                                               TO  I47-TNAME(IDX-LST)
                           END-EVALUATE                    
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF    
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   3 )
                    AND  ( SPA-NAI-TLIMIT  (IDX0)  =   0 )
                    AND  ( FLG-MEDKBN3             =   0 )
                       IF    ( FLG-MEDKBN3         =   0 )
                        AND  ( FLG-MEDKBN2         =   0 )
                           MOVE    "ＩＩ算定期間に限りがない区分"
                                   TO  I47-TNAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1           TO  FLG-MEDKBN3
                       IF      SPA-NAI-TDSPKBN (IDX0)  =   "1" OR  "9"
                           EVALUATE    SPA-NAI-TMEDKBN1 (IDX0)
                               WHEN    "0"
                                   MOVE    "【医療区分３】"
                                               TO  I47-TNAME(IDX-LST)
                               WHEN    "1"
                                   MOVE
                                       "【疾患・状態に係る医療区分３】"
                                               TO  I47-TNAME(IDX-LST)
                               WHEN    "2"
                                   MOVE    "【処置等に係る医療区分３】"
                                               TO  I47-TNAME(IDX-LST)
                           END-EVALUATE                    
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF    
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   2 )
                    AND  ( SPA-NAI-TLIMIT  (IDX0)  =   0 )
                    AND  ( FLG-MEDKBN2             =   0 )
                       IF    ( FLG-MEDKBN3         =   0 )
                        AND  ( FLG-MEDKBN2         =   0 )
                           MOVE    "ＩＩ算定期間に限りがない区分"
                                   TO  I47-TNAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN2
                       IF      SPA-NAI-TDSPKBN (IDX0)  =   "1" OR  "9"
                           EVALUATE    SPA-NAI-TMEDKBN1 (IDX0)
                               WHEN    "0"
                                   MOVE    "【医療区分２】"
                                               TO  I47-TNAME(IDX-LST)
                               WHEN    "1"
                                   MOVE
                                       "【疾患・状態に係る医療区分２】"
                                               TO  I47-TNAME(IDX-LST)
                               WHEN    "2"
                                   MOVE    "【処置等に係る医療区分２】"
                                               TO  I47-TNAME(IDX-LST)
                           END-EVALUATE                    
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF    
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   3   )
                    AND  ( SPA-NAI-TDSPKBN (IDX0)  =   "1" OR  "9" )
                       EVALUATE    SPA-NAI-TMEDKBN1 (IDX0)
                           WHEN    "1"
                               MOVE    "【疾患・状態に係る医療区分３】"
                                           TO  I47-TNAME(IDX-LST)
                           WHEN    "2"
                               MOVE    "【処置等に係る医療区分３】"
                                           TO  I47-TNAME(IDX-LST)
                        END-EVALUATE                    
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   2 )
                    AND  ( SPA-NAI-TDSPKBN (IDX0)  =   "1" OR  "9" )
                        EVALUATE    SPA-NAI-TMEDKBN1 (IDX0)
                           WHEN    "1"
                               MOVE    "【疾患・状態に係る医療区分２】"
                                           TO  I47-TNAME(IDX-LST)
                           WHEN    "2"
                               MOVE    "【処置等に係る医療区分２】"
                                           TO  I47-TNAME(IDX-LST)
                       END-EVALUATE                    
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "2" )
                    AND  ( SPA-NAI-TSRYCD     (IDX0)  =   "820000158" )
                    AND  ( FLG-MEDCOM                 =   0   )
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "2" )
                    AND  ( SPA-I40-SRYYMD        >=   CONST-H240401 )
                    AND  ( SPA-NAI-TSRYCD     (IDX0)  =   "820000159" )
                    AND  ( FLG-MEDCOM                 =   0   )
                       MOVE    1       TO  FLG-MEDCOM
                       MOVE    "――"
                                       TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "2" )
                    AND  ( SPA-I40-SRYYMD            >=   CONST-H300401)
                    AND  ( SPA-NAI-TZAINUM (IDX0)    >=   2080 )
                    AND  ( SPA-NAI-TZAINUM (IDX0)    <=   2089 )
                    AND  ( FLG-MEDMORE                =   0   )
                       MOVE    1       TO  FLG-MEDMORE
                       MOVE    "――"  TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "1" )
                    AND  ( FLG-ADLKBN                 =   0 )
                    AND  ( SPA-I47-ADLINPUTKBN        =   "1" )
                       MOVE    1       TO  FLG-ADLKBN
                       MOVE    "ＡＤＬ区分"
                                       TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN    OTHER
                       PERFORM 5001-GMNHEN-SEC
                   END-EVALUATE
               WHEN  ( SPA-I40-SRYYMD  >=  CONST-H200401 )
      *
                   EVALUATE    TRUE
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   3 )
                    AND  ( SPA-NAI-TLIMIT  (IDX0)  >   0 )
                    AND  ( FLG-MEDKBN3LMT          =   0 )
                       IF    ( FLG-MEDKBN3LMT      =   0 )
                        AND  ( FLG-MEDKBN2LMT      =   0 )
                           MOVE    "Ｉ　算定期間に限りがある区分"
                                   TO  I47-TNAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN3LMT
                       MOVE    "医療区分３"
                                   TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   2 )
                    AND  ( SPA-NAI-TLIMIT  (IDX0)  >   0 )
                    AND  ( FLG-MEDKBN2LMT          =   0 )
                       IF    ( FLG-MEDKBN3LMT      =   0 )
                        AND  ( FLG-MEDKBN2LMT      =   0 )
                           MOVE    "Ｉ　算定期間に限りがある区分"
                                   TO  I47-TNAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN2LMT
                       MOVE    "医療区分２"
                                       TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   3 )
                    AND  ( SPA-NAI-TLIMIT  (IDX0)  =   0 )
                    AND  ( FLG-MEDKBN3             =   0 )
                       IF    ( FLG-MEDKBN3         =   0 )
                        AND  ( FLG-MEDKBN2         =   0 )
                           MOVE    "ＩＩ算定期間に限りがない区分"
                                   TO  I47-TNAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN3
                       MOVE    "医療区分３"
                                   TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   2 )
                    AND  ( SPA-NAI-TLIMIT  (IDX0)  =   0 )
                    AND  ( FLG-MEDKBN2             =   0 )
                       IF    ( FLG-MEDKBN3         =   0 )
                        AND  ( FLG-MEDKBN2         =   0 )
                           MOVE    "ＩＩ算定期間に限りがない区分"
                                   TO  I47-TNAME(IDX-LST)
                           COMPUTE IDX-LST =   IDX-LST +   1
                       END-IF
                       MOVE    1       TO  FLG-MEDKBN2
                       MOVE    "医療区分２"
                                       TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "2" )
                    AND  ( SPA-NAI-TSRYCD     (IDX0)  =   "820000158" )
                    AND  ( FLG-MEDCOM                 =   0   )
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "2" )
                    AND  ( SPA-I40-SRYYMD        >=   CONST-H240401 )
                    AND  ( SPA-NAI-TSRYCD     (IDX0)  =   "820000159" )
                    AND  ( FLG-MEDCOM                 =   0   )
                       MOVE    1       TO  FLG-MEDCOM
                       MOVE    "――"
                                       TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "2" )
                    AND  ( SPA-I40-SRYYMD            >=   CONST-H300401)
                    AND  ( SPA-NAI-TZAINUM (IDX0)    >=   2080 )
                    AND  ( SPA-NAI-TZAINUM (IDX0)    <=   2089 )
                    AND  ( FLG-MEDMORE                =   0   )
                       MOVE    1       TO  FLG-MEDMORE
                       MOVE    "――"  TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "1" )
                    AND  ( FLG-ADLKBN                 =   0 )
                    AND  ( SPA-I47-ADLINPUTKBN        =   "1" )
                       MOVE    1       TO  FLG-ADLKBN
                       MOVE    "ＡＤＬ区分"
                                       TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN    OTHER
                       PERFORM 5001-GMNHEN-SEC
                   END-EVALUATE
               WHEN    OTHER
      *
                   EVALUATE    TRUE
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   3 )
                    AND  ( FLG-MEDKBN3             =   0 )
                       MOVE    1       TO  FLG-MEDKBN3
                       MOVE    "医療区分３"
                                   TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TMEDKBN (IDX0)  =   2 )
                    AND  ( FLG-MEDKBN2             =   0 )
                       MOVE    1       TO  FLG-MEDKBN2
                       MOVE    "医療区分２"
                                       TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN  ( SPA-NAI-TZAISKBKBN (IDX0)  =   "1" )
                    AND  ( FLG-ADLKBN                 =   0 )
                    AND  ( SPA-I47-ADLINPUTKBN        =   "1" )
                       MOVE    1       TO  FLG-ADLKBN
                       MOVE    "ＡＤＬ区分"
                                       TO  I47-TNAME(IDX-LST)
                       COMPUTE IDX-LST =   IDX-LST +   1
                       PERFORM 5001-GMNHEN-SEC
                   WHEN    OTHER
                       PERFORM 5001-GMNHEN-SEC
                   END-EVALUATE
               END-EVALUATE
      *
           END-PERFORM
      *
           MOVE    IDX-LST         TO  I47-RYOLIST-COUNT
      *
           EVALUATE    FLG-RYOLIST-ROW
           WHEN    2
               MOVE    I47-RYOLIST-COUNT  TO  I47-RYOLIST-ROW
               MOVE    1                  TO  I47-RYOLIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-RYOLIST-ROW    TO  I47-RYOLIST-ROW
           END-EVALUATE
      *
           MOVE    WRK-NYULIST-ROW     TO  I47-NYULIST-ROW
      *
           PERFORM 5001-GMNHEN-NYULIST-SEC
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       5001-GMNHEN-SEC                 SECTION.
      *
           IF    ( IDX-LST >=  CONST-RYOLIST-MAX )
               MOVE    CONST-RYOLIST-MAX
                                   TO  IDX-LST
           ELSE
               MOVE    SPA-GMN-TNUM (IDX0)
                                    TO  WRK-ZZZ
               MOVE    WRK-ZZZ-G    TO  I47-TNUM (IDX-LST)
      *
               IF    ( SPA-NAI-INPUT-MODE  =   2 )
                   IF    ( SPA-NAI-SELNUM-FLG (IDX0)   =   "1" )
                       MOVE    "*"     TO  I47-TMARK (IDX-LST)
                   ELSE
                       MOVE    SPACE   TO  I47-TMARK (IDX-LST)
                   END-IF
               END-IF
      *
               MOVE    SPA-GMN-TNAME (IDX0)
                                       TO  I47-TNAME(IDX-LST)
      *
               MOVE    SPA-NAI-TLIMIT(IDX0)
                                       TO  WRK-ZZ
               MOVE    WRK-ZZ          TO  I47-TDAYLMT (IDX-LST)
      *
               IF    ( SPA-NAI-TZAISKBKBN (IDX0)   =   "2" OR "A")
                AND  ( SPA-NAI-TALLKBN (IDX0)      =   ZERO )
                       MOVE    "--"    TO  I47-TDAYALL (IDX-LST)
               ELSE
                   MOVE    SPA-GMN-TDAYALL (IDX0)
                                       TO  WRK-ZZ
                   MOVE    WRK-ZZ      TO  I47-TDAYALL (IDX-LST)
               END-IF
      *
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( SPA-NAI-TZAISKBKBN (IDX0)   =   "2" )
                    AND  ( SPA-NAI-TALLKBN (IDX0)      =   1 )
                       MOVE    "--"        TO  I47-TDAY(IDX-LST IDX-DAY)
                   ELSE
                       IF    ( SPA-NAI-TZAISKBKBN (IDX0)   =   "A")
                           IF    ( SPA-NAI-DESIGNR-EVAL-DAY (IDX-DAY)
                                                       >   ZERO )
                               MOVE    SPA-GMN-TDAY (IDX0 IDX-DAY)
                                           TO  WRK-Z9
                               MOVE    WRK-Z9
                                           TO  I47-TDAY(IDX-LST IDX-DAY)
                           END-IF
                       ELSE
                           MOVE    SPA-GMN-TDAY (IDX0 IDX-DAY)
                                           TO  WRK-ZZ
                           MOVE    WRK-ZZ  TO  I47-TDAY(IDX-LST IDX-DAY)
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF    ( SPA-GMN-RYOLIST-SEL =   IDX0 )
                   MOVE    "T"         TO  I47-RYOLIST-SELECT (IDX-LST)
               ELSE
                   MOVE    "F"         TO  I47-RYOLIST-SELECT (IDX-LST)
               END-IF
           END-IF
      *
           .
       5001-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理 （入院情報リスト）
      *****************************************************************
       5001-GMNHEN-NYULIST-SEC         SECTION.
      *
           MOVE    1                   TO  IDXZ
           MOVE    IDXZ                TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  I47-TNL-NUM (1)
           MOVE    "入院料"            TO  I47-TNL-NAME  (1)
      *
           ADD     1                   TO  IDXZ
           MOVE    IDXZ                TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  I47-TNL-NUM (2)
           IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
           AND   ( SPA-HOSPSBT     =   1             )
               MOVE    "処置等に係る医療区分"
                                           TO  I47-TNL-NAME  (2)
           ELSE
               MOVE    "医療区分"          TO  I47-TNL-NAME  (2)
           END-IF     
      *
           IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
           AND   ( SPA-HOSPSBT     =   1             )
               ADD     1                   TO  IDXZ
               MOVE    IDXZ                TO  WRK-ZZ
               MOVE    WRK-ZZ              TO  I47-TNL-NUM   (IDXZ)
               MOVE    "疾患・状態に係る医療区分"
                                           TO  I47-TNL-NAME  (IDXZ)
           END-IF    
      *                                
           ADD     1                   TO  IDXZ
           IF    ( SPA-I47-ADLINPUTKBN =   "1" )
               MOVE    IDXZ                TO  WRK-ZZ
               MOVE    WRK-ZZ              TO  I47-TNL-NUM   (IDXZ)
           ELSE
               MOVE    SPA-NAI-RYOLIST-ADL-STIDX
                                       TO  WRK-ZZ
               MOVE    WRK-ZZ          TO  I47-TNL-NUM (IDXZ)
               IF    ( SPA-NAI-INPUT-MODE  =   1 )
                   IF    ( SPA-GMN-RYOLIST-SEL
                                       =   SPA-NAI-RYOLIST-ADL-STIDX )
                       MOVE    "T"     TO  I47-NYULIST-SELECT (IDXZ)
                   END-IF
               ELSE
                   IF    ( SPA-NAI-SELNUM-FLG(SPA-NAI-RYOLIST-ADL-STIDX)
                                           =   "1" )
                       MOVE    "*"     TO  I47-TNL-MARK (IDXZ)
                   END-IF
               END-IF
           END-IF
           MOVE    "ＡＤＬ点数"        TO  I47-TNL-NAME  (IDXZ)
      *
           MOVE    IDXZ                TO  I47-NYULIST-COUNT
      *
           PERFORM 50011-GMNHEN-NYULIST-NYU-SEC
      *
           IF    ( SPA-I47-RYOYO-FLG   =   1 )
      *
               PERFORM 50011-EDIT-MEDADL-SEC
      *
               PERFORM 50011-GMNHEN-NYULIST-MED-SEC
               PERFORM 50011-GMNHEN-NYULIST-ADL-SEC
      *
               PERFORM 900-ORCSS011-CALL-SEC
               CALL    "ORCSS011"  USING   SS011-AREA
                                           SPA-AREA
               MOVE    "black" TO   I47-LBLINFO-STYLE
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( SS011-DAY (IDX-DAY)
                                       = SS011-DAY2 (IDX-DAY))
                       MOVE    "black" TO   I47-NYULIST-NDAY-STYLE
                                                             (IDX-DAY)
                   ELSE
                       MOVE    "red"   TO   I47-NYULIST-NDAY-STYLE
                                                             (IDX-DAY)
                     IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
                     AND   ( SPA-HOSPSBT     =   1             )
                       EVALUATE    SS011-DAY2 (IDX-DAY)
                       WHEN    1
                          MOVE    " 1" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    2
                          MOVE    " 2" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    3
                          MOVE    " 3" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    4
                          MOVE    " 4" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    5
                          MOVE    " 5" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    6
                          MOVE    " 6" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    7
                          MOVE    " 7" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    8
                          MOVE    " 8" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    9
                          MOVE    " 9" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    10
                          MOVE    "10" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    11
                          MOVE    "11" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    12
                          MOVE    "12" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    13
                          MOVE    "13" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    14
                          MOVE    "14" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    15
                          MOVE    "15" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    16
                          MOVE    "16" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    17
                          MOVE    "17" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    18
                          MOVE    "18" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    19
                          MOVE    "19" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    20
                          MOVE    "20" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    21
                          MOVE    "21" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    22
                          MOVE    "22" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    23
                          MOVE    "23" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    24
                          MOVE    "24" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    25
                          MOVE    "25" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    26
                          MOVE    "26" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    27
                          MOVE    "27" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    28
                          MOVE    "28" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    29
                          MOVE    "29" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       WHEN    30
                          MOVE    "30" TO  I47-NYULIST-NDAY    (IDX-DAY)
                       END-EVALUATE
                     ELSE             
                       EVALUATE    SS011-DAY2 (IDX-DAY)
                       WHEN    1
                          MOVE    "Ａ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       WHEN    2
                          MOVE    "Ｂ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       WHEN    3
                          MOVE    "Ｃ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       WHEN    4
                          MOVE    "Ｄ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       WHEN    5
                          MOVE    "Ｅ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       WHEN    6
                          MOVE    "Ｆ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       WHEN    7
                          MOVE    "Ｇ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       WHEN    8
                          MOVE    "Ｈ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       WHEN    9
                          MOVE    "Ｉ" TO  I47-NYULIST-NDAY
                                                   (IDX-DAY)
                       END-EVALUATE
                     END-IF  
                       MOVE    "red"   TO   I47-LBLINFO-STYLE
                       STRING  "入院料と医療区分・ＡＤＬ区分の評価が"
                                            DELIMITED  BY  SIZE
                               "一致しない日があります"
                                            DELIMITED  BY  SIZE
                       INTO    I47-LBLINFO
                       END-STRING
                   END-IF
               END-PERFORM
      *
           END-IF
      *
           .
       5001-GMNHEN-NYULIST-EXT.
           EXIT.
      *****************************************************************
      *    医療区分、ＡＤＬ点数編集処理
      *****************************************************************
       50011-EDIT-MEDADL-SEC               SECTION.
      *
           INITIALIZE                  MEDKBN-DAY-AREA
                                       ADLTEN-DAY-AREA
      *
           PERFORM VARYING IDX-LST     FROM    1   BY  1
                   UNTIL ( IDX-LST     >   SPA-GMN-RYOLIST-MAX )
      *
               IF    ( SPA-NAI-TZAISKBKBN (IDX-LST)    =   "1" )
                   COMPUTE ADLTEN-DAYALL
                       =   ADLTEN-DAYALL
                       +   SPA-GMN-TDAYALL (IDX-LST)
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( SPA-I47-RYOYO-CD (IDX-DAY)
                                           NOT =  SPACE )
                           COMPUTE ADLTEN-DAY (IDX-DAY)
                               =   ADLTEN-DAY (IDX-DAY)
                               +   SPA-GMN-TDAY  (IDX-LST IDX-DAY)
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    ZERO            TO  IDX-MED
      *        1:医療区分３ 2:医療区分２
      *        R0606以降の療養病棟  
      *        1:疾患・状態に係る医療区分３
      *        2:疾患・状態に係る医療区分２
      *        3:処置等に係る医療区分３
      *        4:処置等に係る医療区分２
               IF    ( SPA-NAI-TZAISKBKBN (IDX-LST)    =   "2" )
      *            2:医療区分２ 3:医療区分３         
                   IF    ( SPA-NAI-TMEDKBN(IDX-LST)    =   3 )
                       MOVE    1       TO  IDX-MED
                   END-IF
                   IF    ( SPA-NAI-TMEDKBN(IDX-LST)    =   2 )
                       MOVE    2       TO  IDX-MED
                   END-IF
                   IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
                   AND   ( SPA-HOSPSBT     =   1             )
      *                1:疾患・状態に係る　2:処置等に係る（療養病棟のみ）
                       IF    ( SPA-NAI-TMEDKBN (IDX-LST)   =   3 )
                       AND   ( SPA-NAI-TMEDKBN1(IDX-LST)   =   2 )
                           MOVE    3       TO  IDX-MED
                       END-IF
                       IF    ( SPA-NAI-TMEDKBN(IDX-LST)    =   2 )
                       AND   ( SPA-NAI-TMEDKBN1(IDX-LST)   =   2 )
                           MOVE    4       TO  IDX-MED
                       END-IF
                   END-IF    
                   IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
                   AND   ( SPA-HOSPSBT     =   1             )
                        IF    ( SPA-NAI-TSRYCD (IDX-LST) 
                                           =   CONST-R0606-SUMON-SRYCD )
                       AND   ( SPA-GMN-TDAYALL(IDX-LST)
                                           >   ZERO                    )
                           MOVE    1           TO  MEDKBN-SUMONFLG
                       END-IF
                   END-IF
               END-IF    
      *
               IF    ( IDX-MED >       ZERO )
      *
                   IF    ( SPA-GMN-TDAYALL(IDX-LST)    >   ZERO )
                       MOVE      SPA-GMN-TDAYALL(IDX-LST)
                                       TO  MEDKBN-DAYALL (IDX-MED)
                   END-IF
      *
                   PERFORM VARYING IDX-DAY FROM    1   BY  1
                           UNTIL ( IDX-DAY >   31 )
                       IF    ( SPA-I47-RYOYO-CD (IDX-DAY)
                                       NOT =  SPACE )
                        AND  ( SPA-GMN-TDAY   (IDX-LST IDX-DAY)
                                           >   ZERO )
                           MOVE    SPA-GMN-TDAY   (IDX-LST IDX-DAY)
                                   TO  MEDKBN-DAY (IDX-MED IDX-DAY)
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       50011-EDIT-MEDADL-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理 （入院情報リスト:入院料）
      *****************************************************************
       50011-GMNHEN-NYULIST-NYU-SEC         SECTION.
      *
           PERFORM VARYING IDX-DAY     FROM    1   BY  1
                   UNTIL ( IDX-DAY     >   31 )
      *       
                   MOVE    IDX-DAY     TO   WRK-Z9
                   MOVE    WRK-Z9      TO   I47-NYULIST-NDAY (IDX-DAY)
      *
             IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
             AND   ( SPA-HOSPSBT     =   1             )
              IF    ( SPA-I47-RYOYO-KHN (IDX-DAY) = 7 OR 8)
               MOVE    "特"            TO  I47-TNL-DAY(1 IDX-DAY)
              ELSE
               EVALUATE    SPA-I47-RYOYO-DAY (IDX-DAY)
               WHEN    1
                   MOVE    " 1"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    2
                   MOVE    " 2"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    3
                   MOVE    " 3"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    4
                   MOVE    " 4"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    5
                   MOVE    " 5"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    6
                   MOVE    " 6"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    7
                   MOVE    " 7"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    8
                   MOVE    " 8"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    9
                   MOVE    " 9"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   10
                   MOVE    "10"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   11
                   MOVE    "11"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   12
                   MOVE    "12"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   13
                   MOVE    "13"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   14
                   MOVE    "14"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   15
                   MOVE    "15"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   16
                   MOVE    "16"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   17
                   MOVE    "17"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   18
                   MOVE    "18"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   19
                   MOVE    "19"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   20
                   MOVE    "20"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   21
                   MOVE    "21"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   22
                   MOVE    "22"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   23
                   MOVE    "23"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   24
                   MOVE    "24"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   25
                   MOVE    "25"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   26
                   MOVE    "26"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   27
                   MOVE    "27"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   28
                   MOVE    "28"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   29
                   MOVE    "29"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN   30
                   MOVE    "30"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    -1
                   MOVE    "・"        TO  I47-TNL-DAY(1 IDX-DAY)
               END-EVALUATE
              END-IF
             ELSE
              IF    ( SPA-I47-RYOYO-KHN (IDX-DAY) = 5 OR 6 OR 9 OR 10 )
               MOVE    "特"            TO  I47-TNL-DAY(1 IDX-DAY)
              ELSE
               EVALUATE    SPA-I47-RYOYO-DAY (IDX-DAY)
               WHEN    1
                   MOVE    "Ａ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    2
                   MOVE    "Ｂ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    3
                   MOVE    "Ｃ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    4
                   MOVE    "Ｄ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    5
                   MOVE    "Ｅ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    6
                   MOVE    "Ｆ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    7
                   MOVE    "Ｇ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    8
                   MOVE    "Ｈ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    9
                   MOVE    "Ｉ"        TO  I47-TNL-DAY(1 IDX-DAY)
               WHEN    -1
                   MOVE    "・"        TO  I47-TNL-DAY(1 IDX-DAY)
               END-EVALUATE
              END-IF
             END-IF 
           END-PERFORM
      *
           .
       50011-GMNHEN-NYULIST-NYU-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理 （入院情報リスト:医療区分）
      *****************************************************************
       50011-GMNHEN-NYULIST-MED-SEC         SECTION.
      *
      *    MEDKBN-DAYALL MEDKBN-DAY-G
      *    1:医療区分３ 2:医療区分２
      *    R0606以降の療養病棟  
      *    1:疾患・状態に係る医療区分３
      *    2:疾患・状態に係る医療区分２
      *    3:処置等に係る医療区分３
      *    4:処置等に係る医療区分２
           IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
           AND   ( SPA-HOSPSBT     =   1             )
             EVALUATE    TRUE
             WHEN      ( MEDKBN-DAYALL (1)   =    1 )
               MOVE    "３"            TO  I47-TNL-DAYALL(3)
             WHEN      ( MEDKBN-DAY-G  (1)   =    ZERO )
                 AND   ( MEDKBN-DAYALL (2)   =    1    )
               MOVE    "２"            TO  I47-TNL-DAYALL(3)
             WHEN      ( MEDKBN-DAY-G  (1)   =    ZERO )
                 AND   ( MEDKBN-DAY-G  (2)   =    ZERO )
               MOVE    1               TO  WRK-ZZ
               MOVE    WRK-ZZ          TO  I47-TNL-DAYALL(3)
             WHEN    OTHER
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( SPA-I47-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
                       EVALUATE    TRUE
                       WHEN  ( MEDKBN-DAY (1 IDX-DAY)   =  1 )
                           MOVE    "３"    TO  I47-TNL-DAY (3 IDX-DAY)
                       WHEN  ( MEDKBN-DAYALL (2)        =  1 )
                       WHEN  ( MEDKBN-DAY (2 IDX-DAY)   =  1 )
                           MOVE    "２"    TO  I47-TNL-DAY (3 IDX-DAY)
                       WHEN    OTHER
                           MOVE    1       TO  WRK-ZZ
                           MOVE    WRK-ZZ  TO  I47-TNL-DAY (3 IDX-DAY)
                       END-EVALUATE
      *
                   END-IF
               END-PERFORM
             END-EVALUATE
      *
             EVALUATE    TRUE
             WHEN      ( MEDKBN-DAYALL (3)   =    1 )
               MOVE    "３"            TO  I47-TNL-DAYALL(2)
             WHEN      ( MEDKBN-DAY-G  (3)   =    ZERO )
                 AND   ( MEDKBN-DAYALL (4)   =    1    )
               MOVE    "２"            TO  I47-TNL-DAYALL(2)
             WHEN      ( MEDKBN-DAY-G  (3)   =    ZERO )
                 AND   ( MEDKBN-DAY-G  (4)   =    ZERO )
               MOVE    1               TO  WRK-ZZ
               MOVE    WRK-ZZ          TO  I47-TNL-DAYALL(2)
             WHEN    OTHER
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( SPA-I47-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
      *
                       EVALUATE    TRUE
                       WHEN  ( MEDKBN-DAY (3 IDX-DAY)   =  1 )
                           MOVE    "３"    TO  I47-TNL-DAY (2 IDX-DAY)
                       WHEN  ( MEDKBN-DAYALL (4)        =  1 )
                       WHEN  ( MEDKBN-DAY (4 IDX-DAY)   =  1 )
                           MOVE    "２"    TO  I47-TNL-DAY (2 IDX-DAY)
                       WHEN    OTHER
                           MOVE    1       TO  WRK-ZZ
                           MOVE    WRK-ZZ  TO  I47-TNL-DAY (2 IDX-DAY)
                       END-EVALUATE
      *
                   END-IF
               END-PERFORM
             END-EVALUATE
           ELSE
             EVALUATE    TRUE
             WHEN      ( MEDKBN-DAYALL (1)   =    1 )
               MOVE    "３"            TO  I47-TNL-DAYALL(2)
             WHEN      ( MEDKBN-DAY-G  (1)   =    ZERO )
                 AND   ( MEDKBN-DAYALL (2)   =    1    )
               MOVE    "２"            TO  I47-TNL-DAYALL(2)
             WHEN      ( MEDKBN-DAY-G  (1)   =    ZERO )
                 AND   ( MEDKBN-DAY-G  (2)   =    ZERO )
               MOVE    1               TO  WRK-ZZ
               MOVE    WRK-ZZ          TO  I47-TNL-DAYALL(2)
             WHEN    OTHER
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
                   IF    ( SPA-I47-RYOYO-CD (IDX-DAY)
                                       NOT =   SPACE )
      *
                       EVALUATE    TRUE
                       WHEN  ( MEDKBN-DAY (1 IDX-DAY)   =  1 )
                           MOVE    "３"    TO  I47-TNL-DAY (2 IDX-DAY)
                       WHEN  ( MEDKBN-DAYALL (2)        =  1 )
                       WHEN  ( MEDKBN-DAY (2 IDX-DAY)   =  1 )
                           MOVE    "２"    TO  I47-TNL-DAY (2 IDX-DAY)
                       WHEN    OTHER
                           MOVE    1       TO  WRK-ZZ
                           MOVE    WRK-ZZ  TO  I47-TNL-DAY (2 IDX-DAY)
                       END-EVALUATE
      *
                   END-IF
               END-PERFORM
             END-EVALUATE
           END-IF    
      *
           .
       50011-GMNHEN-NYULIST-MED-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理 （入院情報リスト:ＡＤＬ区分）
      *****************************************************************
       50011-GMNHEN-NYULIST-ADL-SEC         SECTION.
      *
           IF    ( ADLTEN-DAYALL       >    ZERO )
            AND  ( ADLTEN-DAY-G        =    ZERO )
               MOVE    ADLTEN-DAYALL   TO   WRK-ZZ
               IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
               AND   ( SPA-HOSPSBT     =   1             )
                   MOVE    WRK-ZZ          TO   I47-TNL-DAYALL(4)
               ELSE  
                   MOVE    WRK-ZZ          TO   I47-TNL-DAYALL(3)
               END-IF    
           ELSE
               PERFORM VARYING IDX-DAY FROM 1  BY  1
                       UNTIL ( IDX-DAY >    31 )
      *
                   IF    ( SPA-I47-RYOYO-CD (IDX-DAY)  NOT =  SPACE )
                       COMPUTE WRK-ADLTEN
                           =   ADLTEN-DAY (IDX-DAY)   +   ADLTEN-DAYALL
      *
                       MOVE    WRK-ADLTEN
                                           TO   WRK-ZZ
                       IF    ( SPA-I40-SRYYMD  >=  CONST-R060601 )
                       AND   ( SPA-HOSPSBT     =   1             )
                           MOVE    WRK-ZZ  TO   I47-TNL-DAY (4 IDX-DAY)
                       ELSE  
                           MOVE    WRK-ZZ  TO   I47-TNL-DAY (3 IDX-DAY)
                       END-IF    
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       50011-GMNHEN-NYULIST-ADL-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  I47-B10-STATE
                                           I47-B10S-STATE
                                           I47-DAY-AREA-STATE
      *
      **     IF    ( SPA-I40-SRYYMD      >=  CONST-R060601 )
      *     AND   ( SPA-HOSPSBT         =   2             ) 
      *         MOVE    WIDGET-INSENSITIVE
      *                                 TO  I47-B10-STATE
      *                                     I47-B10S-STATE
      *     ELSE
               IF    ( SPA-I40-SRYYMD      >   CONST-H240401 )
                   CONTINUE
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  I47-B10-STATE
                                               I47-B10S-STATE
               END-IF
      *     END-IF    
      *
           MOVE    "black"             TO  I47-LBLINFO2-STYLE
           IF    ( SPA-NAI-INPUT-MODE  =   1 )
               MOVE    SPA-GMN-RYOLIST-SEL
                                       TO  IDX-LST
      *         IF    ( SPA-NAI-TZAINUM (IDX-LST)   =   2039 )
      *             MOVE    WIDGET-INSENSITIVE
      *                                 TO  I47-DAY-AREA-STATE
      *             MOVE    "blue"      TO  I47-LBLINFO2-STYLE
      *             MOVE    "他の入力項目の内容より自動で設定します。"
      *                                 TO  I47-LBLINFO2
      *         END-IF
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5011-INPUT-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    002
               MOVE    "GRPINPUT"      TO  MCP-WIDGET
           WHEN    100
               MOVE    "DAYALL"        TO  MCP-WIDGET
           WHEN    101     THRU    131
               MOVE    "DAY"           TO  WRK-WIDGET-DAY-LBL
               COMPUTE WRK-WIDGET-DAY
                   =   SPA-GMN-CUR     -   100
               MOVE    WRK-WIDGET-DAY-G
                                       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "GRPINPUT"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "DAYALL"
               MOVE    101             TO  SPA-GMN-CUR
           WHEN    "DAY01"         THRU    "DAY31"
               IF    ( WRK-MCP-WIDGET (7:2)
                                       >=  SPA-I47-GETUMATU (7:2))
                   MOVE    208         TO  SPA-GMN-CUR
               ELSE
                   MOVE    WRK-MCP-WIDGET  TO  WRK-WIDGET-DAY-G
                   COMPUTE SPA-GMN-CUR
                       =   WRK-WIDGET-DAY
                       +   101
               END-IF
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5011-INPUT-CUR-SKIP-SEC         SECTION.
      *
      *
           .
      *
       5011-INPUT-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "指定された番号が多すぎます"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "指定された期間が多すぎます"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "一括修正の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "日別の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "入院期間外です"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               STRING  "ＤＥＳＧＩＮ−Ｒ合計点と"
                                           DELIMITED   BY  SIZE
                       "他の項目は同時に選択できません"
                                           DELIMITED   BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0101"
               MOVE    "連続して１４日を越えて入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "0102"
               STRING  "連続して１４日を越えて入力されています"
                                           DELIMITED   BY  SIZE
                       "【"                DELIMITED   BY  SIZE
                       WRK-ERRMSG(1:20)    DELIMITED   BY  SIZE
                       "】"                DELIMITED   BY  SIZE
                       INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0103"
               MOVE    "連続して７日を越えて入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "0104"
               STRING  "連続して７日を越えて入力されています"
                                           DELIMITED   BY  SIZE
                       "【"                DELIMITED   BY  SIZE
                       WRK-ERRMSG(1:20)    DELIMITED   BY  SIZE
                       "】"                DELIMITED   BY  SIZE
                       INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1001"
               MOVE    "剤が修正中です。変更確定かクリアをしてください"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4005"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I4ERR
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I47"               TO  SPA-MOTOPG
           MOVE    "I4ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I4ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-I4IDSET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-I4IDMSG
      *
           EVALUATE    SPA-IIDCD
           WHEN    "2001"
               MOVE    "医療区分・ＡＤＬ区分を登録します"
                                       TO  WRK-I4IDMSG
           WHEN    "2002"
             MOVE "医療区分・ＡＤＬ区分を登録後、入院料の変更を行います"
                                       TO  WRK-I4IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I4ID1
           INITIALIZE                      I4ID1
           MOVE    SPA-IIDCD           TO  I4ID1-ID1CODE
           MOVE    WRK-I4IDMSG         TO  I4ID1-ID1MSG
      *
           MOVE    "戻る"              TO  I4ID1-B01
           MOVE    "ＯＫ"              TO  I4ID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I47"               TO  SPA-MOTOPG
           MOVE    "I4ID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I4ID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-I4IDSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       521-I4IDSET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-I4IDMSG
      *
           EVALUATE    SPA-IIDCD
           WHEN    "2003"
               MOVE    "入院料変更処理後、評価票の発行を行います"
                                       TO  WRK-I4IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I4ID2
           INITIALIZE                      I4ID2
           MOVE    SPA-IIDCD           TO  I4ID2-ID1CODE
           MOVE    WRK-I4IDMSG         TO  I4ID2-ID1MSG
      *
           MOVE    "F1:戻る"           TO  I4ID2-B01
           MOVE    "F11:入院料の変更を行わずに発行"
                                       TO  I4ID2-B11
           MOVE    "F12:ＯＫ"          TO  I4ID2-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "I47"               TO  SPA-MOTOPG
           MOVE    "I4ID2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I4ID2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       521-I4IDSET-EXT.
           EXIT.
      *****************************************************************
      *    カナチェック処理
      *****************************************************************
       800-KANA-CHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT
           END-IF
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    医療区分、ＡＤＬ点数妥当性チェック
      *****************************************************************
       800-ORCSS011-SEC                SECTION.
      *
      *
           .
       800-ORCSS011-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
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
      *    療養レセプトコメントマスタ削除処理
      *****************************************************************
       900-RYOCOM-DEL3-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  RYOCOM-PTID
           MOVE    SPA-I40-SRYYMD (1:6)
                                   TO  RYOCOM-SRYYM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "del3"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1           TO  FLG-RYOCOM
           END-IF
      *
           .
       900-RYOCOM-DEL3-EXT.
            EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ検索処理
      *****************************************************************
       900-RYOCOM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-GMN-PTID    TO  RYOCOM-PTID
           MOVE    WRK-SRYYM
                                   TO  RYOCOM-SRYYM
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
      *    点数マスタ検索処理(KEY)
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
      *    点数マスタクローズ
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY19-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE  NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    SPA-I40-SRYYMD(1:6) TO  NACCT-SRYYM
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
           .
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY35-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    SPA-I40-SRYYMD(1:6) TO  NACCT-SRYYM
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
      *    入院会計FETCH処理
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
      *    入院診療行為検索処理
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
      *    入院診療行為FETCH処理
      *****************************************************************
       900-NYUINACT-KEY9-FET-SEC       SECTION.
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
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC      SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
