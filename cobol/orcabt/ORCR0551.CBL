      *******************************************************************
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
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR0551.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      コメント編集（労災・自賠責）（レセ電用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/02/12    NACL−門脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-門脇    19/03/28  電話再診コメント内容修正
      *  05.00.02    NACL-門脇    19/04/03  自動記載コメント改元対応
      *  05.00.03    NACL-門脇    19/06/24  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      *  05.00.04    NACL-門脇    19/08/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）修正
      *  05.00.05    NACL-門脇    20/03/30  令和２年４月改正対応
      *  05.00.06    NACL-門脇    20/05/07  令和２年４月改正対応
      *                                     画像診断コメント対応
      *  05.00.07    NACL-門脇    20/06/05  画像診断コメント対応修正
      *  05.00.08    NACL-門脇    20/07/01  選択式コメント対応（自動記載）
      *  05.00.09    NACL-門脇    20/09/03  選択式コメント対応（自動記載）
      *  05.00.10    NACL-門脇    20/10/09  選択式コメント対応（自動記載）
      *  05.00.11    NACL-門脇    20/11/27  湿布薬コメント編集に関する
      *                                     チェック条件追加
      *
      *  05.01.01    NMED-藤原    20/10/28  アフターケアレセ電作成対応
      *  05.00.12    ORCAMO       21/04/09  ニコチン依存症管理料（診療報酬上臨時的取扱）対応
      *  05.00.13    ORCAMO       21/08/12  【包括診療】画像診断の
      *                                     選択式コメント編集修正
      *  05.01.00    ORCAMO       22/01/06  在宅持続陽圧呼吸療法指導管
      *                                     理料対応
      *  05.01.01    ORCAMO       22/01/27  特別訪問看護指示加算対応
      *  05.01.02    ORCAMO       22/03/31  令和４年４月改正対応
      *  05.01.03    ORCAMO       22/05/11  新たな選択式コメント対応
      *                                     （４桁目が"6"）
      *  05.01.04    ORCAMO       22/05/17  新たな部位コメント追加
      *  05.01.05    ORCAMO       22/06/06  精神科デイケア等早期加算対応
      *  05.01.06    ORCAMO       22/06/21  廃用症候群リハの初期加算の
      *                                     疾患名コメントコード変更対応
      *  05.01.07    ORCAMO       22/07/11  院外投薬選択式コメント対応
      *  05.01.08    ORCAMO       22/08/16  院外投薬コメント対応修正
      *  05.01.09    ORCAMO       22/11/11  関連コメント対応
      *                                     （医薬品、器材）
      *  05.01.10    ORCAMO       22/11/14  初診料コード追加対応
      *  05.01.11    ORCAMO       22/11/15  （検選）等のコメント編集修正
      *  05.02.01    ORCAMO       23/05/01  令和５年５月改正対応
      *  05.02.02    ORCAMO       23/06/02  薬剤料逓減の剤まとめ修正
      *  05.02.03    ORCAMO       24/06/06  令和６年６月改正対応
      *  05.02.04    ORCAMO       24/09/12  令和６年１０月改正対応
      *  05.02.05    ORCAMO       24/10/29  令和６年１０月改正対応
      *  05.02.06    ORCAMO       25/03/10  剤点が０点の薬剤・器材を含む
      *                                     剤についての自動記載修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
      *
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
      *
           SELECT  RECE441-FILE    ASSIGN  RECE441PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE441-KEY
                                   FILE    STATUS  IS  STS-RECE441.
      *
           SELECT  RECE451-FILE    ASSIGN  RECE451PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE451-KEY
                                   FILE    STATUS  IS  STS-RECE451.
      *ワーク
           SELECT  RECE449-FILE    ASSIGN  RECE449PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE449-KEY
                                   FILE    STATUS  IS  STS-RECE449.
      *
           SELECT  RECE459-FILE    ASSIGN  RECE459PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE459-KEY
                                   FILE    STATUS  IS  STS-RECE459.
      *
           SELECT  RECE455-FILE    ASSIGN  RECE455PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE455-KEY
                                   FILE    STATUS  IS  STS-RECE455.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC.
           COPY    "CPRCF044.INC".
      *
       FD  RECE45-FILE.
       01  RECE45-REC.
           COPY    "CPRCF045.INC".
      *
       FD  RECE441-FILE.
       01  RECE441-REC. 
           COPY    "CPRCF0441.INC".
      *
       FD  RECE451-FILE.
       01  RECE451-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE451//.
      *ワーク
       FD  RECE449-FILE.
       01  RECE449-REC. 
           COPY    "CPRCF0441.INC" REPLACING   //RECE441//
                                   BY          //RECE449//.
      *
       FD  RECE459-FILE.
       01  RECE459-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //RECE459//.
      *
       FD  RECE455-FILE.
       01  RECE455-REC. 
           COPY    "CPRCF0455.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE441//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE451//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE449//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE459//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE455//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE41          PIC X(02).
           03  STS-RECE44          PIC X(02).
           03  STS-RECE441         PIC X(02).
           03  STS-RECE45          PIC X(02).
           03  STS-RECE451         PIC X(02).
           03  STS-RECE449         PIC X(02).
           03  STS-RECE459         PIC X(02).
           03  STS-RECE455         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE44          PIC 9(01).
           03  FLG-RECE449         PIC 9(01).
           03  FLG-RECE441         PIC 9(01).
           03  FLG-RECE45          PIC 9(01).
           03  FLG-RECE459         PIC 9(01).
           03  FLG-RECE451         PIC 9(01).
           03  FLG-WRKRECE45       PIC 9(01).
           03  FLG-SHIDOU-COMMENT  PIC 9(01).
           03  FLG-HINKAI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT  PIC 9(01).
           03  FLG-KYUSEI-COMMENT2 PIC 9(01).
           03  FLG-COMMENT         PIC 9(02).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-FIRST           PIC 9(01).
           03  FLG-YAKUTEI         PIC 9(01).
           03  FLG-YAKUTEI2        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-KYUSEI-OK       PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-NIKOCHINTOU-COMMENT
                                   PIC 9(01).
           03  FLG-NIKOCHINTOU-COMMENT2
                                   PIC 9(01).
           03  FLG-RIHA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-OSIN-HENSYU     PIC 9(01).
           03  FLG-ZAIHOU-HENSYU   PIC 9(01).
           03  FLG-COMOKIKAE       PIC 9(01).
           03  FLG-COMOKIKAE2      PIC 9(01).
           03  FLG-SYOUNI-COMMENT  PIC 9(01).
           03  FLG-SYOUNI-COMMENT2 PIC 9(01).
           03  FLG-SYOUNI-COMMENT3 PIC 9(01).
           03  FLG-COMARI          PIC 9(01).
           03  FLG-COMARI2         PIC 9(01).
           03  FLG-SANTEIPLUS      PIC 9(01).
           03  FLG-SINPESU-COMMENT PIC 9(01).
           03  FLG-SINPESUR-COMMENT
                                   PIC 9(01).
           03  FLG-SINPESUR-COMMENT2
                                   PIC 9(01).
           03  FLG-SANTEIPLUS-HANTEI
                                   PIC 9(01).
           03  FLG-TAKYO-HENSYU    PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SETCOM          PIC 9(01).
           03  FLG-SETCOM2         PIC 9(01).
           03  FLG-MEISKY-OKIKAE   PIC 9(01).
           03  FLG-SETCOM-OKIKAE   PIC 9(01).
           03  FLG-GAIKAN          PIC 9(01).
           03  FLG-ZAIKYO-COMMENT  PIC 9(01).
           03  FLG-CHECK-MAE       PIC 9(01).
           03  FLG-CHECK-ATO       PIC 9(01).
           03  FLG-RETRY           PIC 9(01).
           03  FLG-ZAIKYO-OK       PIC 9(01).
           03  FLG-ZAIKYOCOMARI    PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-SIPPU-OK        PIC 9(01).
           03  FLG-GET             PIC 9(01).
           03  FLG-RKISAI          PIC 9(01).
           03  FLG-ETC             PIC 9(01).
           03  FLG-SENTAKU-OKIKAE  PIC 9(01).
           03  FLG-SENTAKU-OKIKAE2 PIC 9(01).
           03  FLG-JINNAI-COMMENT  PIC 9(01).
           03  FLG-GAZO            PIC 9(01).
           03  FLG-SENTAKU-SYUGI   PIC 9(01).
           03  FLG-YOUATU-COMMENT  PIC 9(01).
           03  FLG-COM-HENSYU      PIC 9(01).
           03  FLG-COM-HENSYU2     PIC 9(01).
           03  FLG-SOUKAN-HENSYU   PIC 9(01).
           03  FLG-KOUYAK-COMMENT  PIC 9(01).
           03  FLG-TEJHOU-COMMENT  PIC 9(01).
           03  FLG-HANTEI          PIC 9(01).
           03  FLG-YAKUTEI-COMARI  PIC 9(01).
           03  FLG-YAKUTEI-NG      PIC 9(01).
           03  FLG-YAKUTEI-NASHI   PIC 9(01).
           03  FLG-YAKUTEI-CHECK   PIC 9(01).
           03  FLG-MANJIN-COMMENT  PIC 9(01).
           03  FLG-MANJIN-COMMENT2 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE44          PIC 9(06).
           03  CNT-RECE45          PIC 9(06).
           03  CNT-RECE449         PIC 9(06).
           03  CNT-RECE459         PIC 9(06).
           03  CNT-RECE455         PIC 9(06).
           03  CNT-RECE44-RE       PIC 9(06).
           03  CNT-RECE45-RE       PIC 9(06).
           03  CNT-RIHA            PIC 9(06).
           03  CNT-JUSHIN          PIC 9(06).
           03  CNT-SANTEIKA        PIC 9(06).
           03  CNT-DOUJITU         PIC 9(06).
           03  CNT-DENWA           PIC 9(06).
           03  CNT-DOUDEN          PIC 9(06).
           03  CNT-DENWA2          PIC 9(06).
           03  CNT-DOUDEN2         PIC 9(06).
           03  CNT-KENSA           PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(04).
           03  IDX9                PIC 9(04).
           03  IDX10               PIC 9(04).
           03  IDX11               PIC 9(04).
           03  IDX12               PIC 9(04).
           03  IDX13               PIC 9(04).
           03  IDX15               PIC 9(01).
           03  IDX16               PIC 9(02).
           03  IDX17               PIC 9(04).
           03  IDX18               PIC 9(04).
           03  IDX19               PIC 9(01).
           03  IDX20               PIC 9(02).
           03  IDX21               PIC 9(04).
           03  IDX26               PIC 9(02).
           03  IDX27               PIC 9(02).
           03  IDX28               PIC 9(02).
           03  IDX29               PIC 9(02).
           03  IDX30               PIC 9(02).
           03  IDX33               PIC 9(03).
           03  IDX34               PIC 9(03).
           03  IDX35               PIC 9(03).
           03  IDX36               PIC 9(03).
           03  IDX37               PIC 9(03).
           03  IDX40               PIC 9(02).
           03  IDX41               PIC 9(02).
           03  IDX42               PIC 9(02).
           03  IDX43               PIC 9(02).
           03  IDX45               PIC 9(03).
           03  IDX46               PIC 9(03).
           03  IDX48               PIC 9(02).
           03  IDX49               PIC 9(02).
           03  IDX50               PIC 9(02).
           03  IDX51               PIC 9(03).
           03  IDX52               PIC 9(03).
           03  IDX53               PIC 9(03).
           03  IDX54               PIC 9(03).
           03  IDX55               PIC 9(04).
           03  IDX56               PIC 9(03).
           03  IDX57               PIC 9(03).
           03  IDX58               PIC 9(03).
           03  IDX59               PIC 9(03).
           03  IDX60               PIC 9(03).
           03  IDY                 PIC 9(04).
           03  IDY11               PIC 9(03).
           03  IDY12               PIC 9(03).
           03  IDY13               PIC 9(03).
           03  IDZ                 PIC 9(04).
           03  DAY-IDX             PIC 9(04).
           03  DAY-IDX2            PIC 9(02).
           03  DAY-IDX3            PIC 9(01).
           03  DAY-IDX4            PIC 9(03).
           03  IDX-RECE45          PIC 9(04).
           03  IDV                 PIC 9(04).
           03  IDV2                PIC 9(04).
           03  IDV3                PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDC1                PIC 9(02).
           03  IDC2                PIC 9(02).
           03  IDC3                PIC 9(02).
           03  IDC4                PIC 9(02).
           03  RIHA-IDX            PIC 9(02).
           03  RIHA-IDX2           PIC 9(02).
           03  RIHA-ST             PIC 9(02).
           03  RIHA-ST2            PIC 9(02).
           03  RIHA-ED             PIC 9(02).
           03  RIHA-ED2            PIC 9(02).
           03  IDX-JIDO            PIC 9(04).
      *     
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-RECEKBN            PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN            PIC  X(01).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-JYUNBAN         PIC 9(04).
           03  WRK-GYOSUU          PIC 9(04).
           03  WRK-GYOSUU2         PIC 9(04).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-CNT-MEISAI      PIC 9(04).
           03  WRK-KETA            PIC 9(04).
           03  WRK-KETA-TAIHI      PIC 9(04).
           03  WRK-KETA2           PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-FSANTEIYMD      PIC 9(08).
           03  WRK-AFTER2          PIC 9(08).
           03  WRK-AFTER180        PIC 9(08).
           03  WRK-TOSEKIYMD       PIC 9(08).
           03  WRK-KYUSEIYMD       PIC 9(08).
           03  WRK-HINKAIYMD       PIC 9(08).
           03  WRK-DAY             PIC 9(02).
      *
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD2          PIC X(09).
           03  WRK-NIKOCHINTOU-YMD PIC X(08).
           03  WRK-NIKOCHINTOU-YMD2 PIC X(08).
      *
           03  WRK-HENSYU-KAISU    PIC X(02).
           03  WRK-HENSYU-KAISU2   PIC X(04).
           03  WRK-HENSYU-KAISU3   PIC X(03).
           03  WRK-HENSYU-KAISU4   PIC X(06).
      *
           03  WRK-HENSYU-KASANRITU   PIC X(01).
           03  WRK-HENSYU-KASANRITU2  PIC X(02).
      *
           03  WRK-RUIKEITOTAL     PIC 9(13)V9(05).
           03  WRK-RUIKEITOTAL2    PIC 9(14).
      *
           03  TBL-SANTEI-DAY.
               05  TB-SANTEI-DAY   PIC 9(02)   OCCURS  31.
      *
      *    該当診療月分
           03  TBL-RIHASANTEI-DAY.
               05  TB-RIHASANTEI-DAY PIC 9(01) OCCURS  31.
      *
           03  TBL-RIHASANTEI-DAY-END.
               05  TB-RIHASANTEI-DAY-END
                                     PIC 9(01) OCCURS  31.
           03  TBL-RIHASANTEI-DAY-END2.
               05  TB-RIHASANTEI-DAY-END2
                                     PIC 9(01) OCCURS  31.
           03  TBL-RIHASANTEI-DAY-END3.
               05  TB-RIHASANTEI-DAY-END3
                                     PIC 9(01) OCCURS  31.
      *
      *    直近分
           03  WRK-RIHASANTEI-YMD    PIC X(08).
      *
      *    直近分退避用
           03  WRK-RIHASANTEI-YMD2   PIC X(08).
           03  WRK-RIHASANTEI-YMD3   PIC X(08).
      *
      *    前月以前の直近終了日
           03  WRK-RIHASANTEI-END    PIC X(08).
      *
      *    ２００６年４月１日に算定日があるか
           03  WRK-RIHASANTEI41-FLG  PIC 9(01).
      *
      *    リハビリ開始日指定チェック用
           03  WRK-CHECK-RIHASANTEI-AREA.
               05  WRK-CHECK-RIHASANTEI-YMD     PIC X(08).
               05  WRK-CHECK-RIHASANTEI-YMD3    PIC X(08).
               05  WRK-CHECK-RIHASANTEI-DAY-OCC.
                 07  WRK-CHECK-RIHASANTEI-DAY   PIC 9(01)  OCCURS  31.
      *
           03  WRK-HKNCOMBINUMX      PIC X(04).
           03  WRK-HKNCOMBINUM9      REDEFINES  WRK-HKNCOMBINUMX
                                     PIC 9(04).
      *
           03  WRK-SRYYMDX.
               05  WRK-SRYYM         PIC 9(06).
               05  WRK-SRYDD         PIC 9(02).
      *
           03  WRK-KEISAN            PIC 9(09).
           03  WRK-KEISAN2           PIC 9(03).
           03  WRK-KEISAN3           PIC 9(09).
           03  WRK-KEISAN4           PIC 9(09).
           03  WRK-KEISAN-TAIHI      PIC X(09).
      *
           03  WRK-KYUSEI-NYUGAIKBN  PIC X(01).
           03  WRK-KYUSEI-SRYKA      PIC X(02).
           03  WRK-KYUSEI-ZAINUM     PIC 9(08).
      *
           03  WRK-ZAIKYO-NYUGAIKBN  PIC X(01).
           03  WRK-ZAIKYO-SRYKA      PIC X(02).
           03  WRK-ZAIKYO-ZAINUM     PIC 9(08).
      *
           03  WRK-HENYMD            PIC X(08).
           03  WRK-TAIHI-XX          PIC X(02).
      *
           03  WRK-OKIKAEFLG         PIC X(01).
      *
           03  WRK-RIHAIGAKU-SRYCD           PIC X(09).
           03  WRK-RIHAIGAKU-SRYCD2          PIC X(09).
           03  WRK-RIHAIGAKU-FSANTEIYMD      PIC X(08).
           03  WRK-RIHAIGAKU-SANTEI-AREA.
               05  WRK-RIHAIGAKU-SANTEI-FLG  PIC X(01).
               05  WRK-RIHAIGAKU-SANTEI-DAY  PIC 9(03)
                                             OCCURS 31.
      *
           03  WRK-SYOUNI-FSANTEIYMD         PIC X(08).
           03  WRK-SYOUNI-FSANTEIYMD2        PIC X(08).
           03  WRK-SYOUNI-FSANTEIYMD3        PIC X(08).
      *
           03  WRK-MCP-TABLE         PIC X(64).
           03  WRK-MCP-PATHNAME      PIC X(64).
      *
           03  WRK-SINPESU-SRYCD             PIC X(09).
           03  WRK-SINPESU-YMD               PIC X(08).
           03  WRK-SINPESUR-YMD              PIC X(08).
           03  WRK-SINPESUR-YMD2             PIC X(08).
           03  WRK-ZAIKYO-YMD                PIC X(08).
           03  WRK-ZAIKYO-YMD2               PIC X(08).
           03  WRK-NINRYO-FSANTEIYMD         PIC X(08).
      *
           03  WRK-BUNKAKUKETA               PIC 9(02).
           03  WRK-BUNKAKU-N                 PIC X(10).
           03  WRK-SRYKAISU-X                PIC ZZ9.
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                PIC X(03).
               05  WRK-CHAR-N                PIC X(06).
               05  WRK-CHAR-N-CNT            PIC 9(01).
               05  WRK-CHAR-IDX              PIC 9(01).
      *
           03  WRK-HEN-RENNUM                PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM          PIC 9(01).
           03  WRK-HEN-SEQNO                 PIC 9(02).
      *
           03  WRK-HEN-RENNUM-X              PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-X        PIC 9(01).
           03  WRK-HEN-SEQNO-X               PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J              PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J        PIC 9(01).
           03  WRK-HEN-SEQNO-J               PIC 9(02).
      *
           03  WRK-HEN-RENNUM-J2             PIC 9(02).
           03  WRK-HEN-SHORI-RENNUM-J2       PIC 9(01).
           03  WRK-HEN-SEQNO-J2              PIC 9(02).
      *
           03  WRK-TAIHI-SRYYMD.
               05  WRK-TAIHI-SRYYM           PIC 9(06).
               05  WRK-TAIHI-SRYD            PIC 9(02).
      *
           03  WRK-TENSU-SRYCD               PIC X(09).
           03  WRK-TENSU-SRYYM               PIC X(06).
      *
           03  WRK-ZAIKAISUX                 PIC Z9.
           03  WRK-TNSKAI                    PIC X(10).
      *
           03  WRK-GANSIDO-YMD-AREA.
               05  WRK-GANSIDO-YMD           PIC X(08)
                                             OCCURS 10.
           03  WRK-GANSIDO-DATA              PIC X(08).
      *
           03  WRK-BOKOHO-YMD2-AREA.
               05  WRK-BOKOHO-YMD2           PIC X(08)
                                             OCCURS 30.
      *
           03  WRK-ZAIKYO-YMD9               PIC X(08).
           03  WRK-ZAIKYO-KAISU              PIC 9(02).
      *
           03  WRK-SIPPU-ZAICNT              PIC 9(08).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1              PIC ZZZZZ.
               05  WRK-SURYO-Z2              PIC X(01).
               05  WRK-SURYO-Z3              PIC ZZZZZ.
           03  WRK-SURYOST                   PIC 9(02).
           03  WRK-SURYOKETA                 PIC 9(02).
           03  WRK-SURYOKETA1                PIC 9(02).
           03  WRK-SURYOKETA2                PIC 9(02).
           03  WRK-SURYO-N.
               05  WRK-SURYO-N1              PIC X(10).
               05  WRK-SURYO-N2              PIC X(02).
               05  WRK-SURYO-N3              PIC X(10).
           03  WRK-SRYSURYOW                 PIC X(24).
           03  WRK-SURYO                     PIC 9(05)V9(05).
           03  WRK-SURYO-R  REDEFINES  WRK-SURYO.
               05  WRK-SURYO-S1              PIC 9(05).
               05  WRK-SURYO-S2              PIC 9(05).
           03  WRK-CHAR2-HENKAN-AREA.
               05  WRK-CHAR2-X               PIC X(10).
               05  WRK-CHAR2-N               PIC X(20).
               05  WRK-CHAR2-CNT             PIC 9(02).
               05  WRK-CHAR2-N-CNT           PIC 9(02).
           03  WRK-CHAR2-IDX                 PIC 9(02).
      *
           03  WRK-INGAISIPPU-NAME           PIC X(300).
           03  WRK-INGAIKAISU-NAME           PIC X(300).
      *
           03  WRK-ZAIKAISU-X.
               05  WRK-ZAIKAISU-Z            PIC ZZZZ.
           03  WRK-ZAIKAISU-N                PIC X(08).
           03  WRK-KEI-N                     PIC X(02).
      *
           03  WRK-CHECK-SRYCD               PIC X(09).
      *
           03  WRK-SJIYMD                    PIC X(08).
      *
           03  WRK-ONLINE-COMCD              PIC X(09).
           03  WRK-ONLINE-FSANTEIYM          PIC X(08).
      *
           03  WRK-JINNAI-YMD                PIC X(08).
      *
           03  WRK-GAIJIR-FSANTEIYM          PIC X(08).
           03  WRK-GAIJIR-KAISU              PIC 9(02).
      *
           03  WRK-RCW-KBN                   PIC X(01).
           03  WRK-RCW-SRYCD                 PIC X(09).
           03  WRK-RCW-SRYYMD                PIC X(08).
      *
           03  WRK-ZENYYMM                   PIC 9(06).
           03  WRK-OSIN-SRYCD                PIC X(09).
      *
           03  WRK-YOUATU-FSANTEIYMD         PIC X(08).
      *
           03  WRK-KOUYAK-FSANTEIYMD         PIC X(08).
      *
           03  WRK-TEJHOU-YMD                PIC X(08).
      *
           03  WRK-INDEX-AREA.
               05  WRK-INDEX                 PIC 9(03)   OCCURS   5.
           03  WRK-INDEX2-AREA.
               05  WRK-INDEX2                PIC 9(03)   OCCURS  25.
      *
           03  WRK-NYUSEN-SRYCD              PIC X(09).
      *
           03  WRK-MANJIN-FSANTEIYMD         PIC X(08).
           03  WRK-MANJIN-FSANTEIYMD2        PIC X(08).
      *
           03  WRK-RKISAICD                  PIC X(09).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"    REPLACING  //RECEDAT1PARA//
                                       BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
       01  WRK-RECE44-REC. 
           COPY    "CPRCF044.INC"  REPLACING   //RECE44//
                                   BY          //WRK-RECE44//.
      *
       01  WRK-RECE45-REC. 
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45//.
      *
       01  WRK-RECE45XX-REC.
         02 WRK-RECE45XX-TBL       OCCURS 100.
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45XX//.
      *
       01  WRK-RECE45YY-REC.
         02 WRK-RECE45YY-TBL       OCCURS 100.
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45YY//.
      *
       01  WRK-RECE45ZZ-REC.
         02 WRK-RECE45ZZ-TBL       OCCURS 100.
           COPY    "CPRCF045.INC"  REPLACING   //RECE45//
                                   BY          //WRK-RECE45ZZ//.
      *
       01  WRK-FREE-AREA.
           03  WRK-FREEINF                     OCCURS 49.         
               05  WK-FREE-YUKOKETA          PIC  9(03).
               05  WK-FREE-NAME              PIC  X(300).
      *
       01  WRK-FREE-AREA2.                                              
           03  WRK-FREEINF2                    OCCURS 50.               
               05  WK-FREE-SRYCD2            PIC  X(09).
               05  WK-FREE-YUKOKETA2         PIC  9(03).                
               05  WK-FREE-NAME2             PIC  X(300).               
               05  WK-FREE-INPUTCOMENT2      PIC  X(140).
               05  WK-FREE-MEISKYFLG2        PIC  X(01).
               05  WK-FREE-SETCOMATFLG2      PIC  9(03).
               05  WK-FREE-RECEDENFLG2       PIC  X(01).
      *
       01  WRK-FREE-AREA3.                                              
           03  WRK-FREEINF3.
               05  WK-FREE-NAME3             PIC  X(1000).              
               05  WK-FREE-NAME3-TAIHI       PIC  X(1000).              
      *
       01  WRK-JIDO-SETCOM-AREA.
           03  WRK-JIDO-SETCOMATFLG          PIC  9(03).
           03  WRK-JIDO-SETCOMFLG            PIC  X(01).
           03  WRK-JIDO-JYOUGEFLG            PIC  9(01).
           03  WRK-JIDO-RECEDENFLG           PIC  X(01).
      *
       01  WRK-RECECOM-AREA.                                              
           03  WRK-RECECOM-KBN               PIC  X(01).
           03  WRK-RECECOM-SRYCD             PIC  X(09).
           03  WRK-RECECOM-YMD               PIC  X(08).
           03  WRK-RECECOM-COMMENT           PIC  X(100).
           03  WRK-RECECOM-NUM               PIC  S9(05)V9(06).
           03  WRK-RECECOM-SRYYMD            PIC  X(08).
      *
      *    日付全角変換領域
       01  TBL-DAY-AREA.
           03  FILLER              PIC X(04)  VALUE "　１".
           03  FILLER              PIC X(04)  VALUE "　２".
           03  FILLER              PIC X(04)  VALUE "　３".
           03  FILLER              PIC X(04)  VALUE "　４".
           03  FILLER              PIC X(04)  VALUE "　５".
           03  FILLER              PIC X(04)  VALUE "　６".
           03  FILLER              PIC X(04)  VALUE "　７".
           03  FILLER              PIC X(04)  VALUE "　８".
           03  FILLER              PIC X(04)  VALUE "　９".
           03  FILLER              PIC X(04)  VALUE "１０".
           03  FILLER              PIC X(04)  VALUE "１１".
           03  FILLER              PIC X(04)  VALUE "１２".
           03  FILLER              PIC X(04)  VALUE "１３".
           03  FILLER              PIC X(04)  VALUE "１４".
           03  FILLER              PIC X(04)  VALUE "１５".
           03  FILLER              PIC X(04)  VALUE "１６".
           03  FILLER              PIC X(04)  VALUE "１７".
           03  FILLER              PIC X(04)  VALUE "１８".
           03  FILLER              PIC X(04)  VALUE "１９".
           03  FILLER              PIC X(04)  VALUE "２０".
           03  FILLER              PIC X(04)  VALUE "２１".
           03  FILLER              PIC X(04)  VALUE "２２".
           03  FILLER              PIC X(04)  VALUE "２３".
           03  FILLER              PIC X(04)  VALUE "２４".
           03  FILLER              PIC X(04)  VALUE "２５".
           03  FILLER              PIC X(04)  VALUE "２６".
           03  FILLER              PIC X(04)  VALUE "２７".
           03  FILLER              PIC X(04)  VALUE "２８".
           03  FILLER              PIC X(04)  VALUE "２９".
           03  FILLER              PIC X(04)  VALUE "３０".
           03  FILLER              PIC X(04)  VALUE "３１".
       01  TBL-DAY-AREA-R          REDEFINES   TBL-DAY-AREA.
           03  TBL-DAY             PIC X(04)   OCCURS   31.
      *
       01  MAX-MEISAISU            PIC 9(07).
      *
      *    固定値
       01  CONST-AREA.
      *    慢性とう痛疾患
           03  CONS-MANSEI          PIC X(09) VALUE "113006510".
      *    在宅血液透析指導管理料
           03  CONS-TOSEKI          PIC X(09) VALUE "114009310".
      *    脳血管疾患急性発症日
           03  CONS-KYUSEI          PIC X(09) VALUE "099800101".
      *    ニコチン依存症管理料１（初回）
           03  CONS-NIKOCHIN        PIC X(09) VALUE "113008310".
      *    ニコチン依存症管理料２
           03  CONS-NIKOCHIN2       PIC X(09) VALUE "113031710".
      *    ニコチン依存症管理料１（初回）（診療報酬上臨時的取扱）
           03  CONS-NIKOCHIN3       PIC X(09) VALUE "113033450".
      *    重度喘息患者治療管理加算（１月目）
           03  CONS-ZENSOKU         PIC X(09) VALUE "113008070".
      *    心大リハ開始日
           03  CONS-SINDAI          PIC X(09) VALUE "099800111".
      *    呼吸器リハ開始日
           03  CONS-KOKYUKI         PIC X(09) VALUE "099800141".
      *    脳血管疾患等リハ開始日
           03  CONS-NOUKE           PIC X(09) VALUE "099800121".
      *    運動器リハ開始日
           03  CONS-UNDOKI          PIC X(09) VALUE "099800131".
      *    難病患者リハ開始日
           03  CONS-NANBYO          PIC X(09) VALUE "099800161".
      *    障害児（者）リハ開始日
           03  CONS-SYOGAI          PIC X(09) VALUE "099800171".
      *    摂食療法開始日
           03  CONS-SESSYOKU        PIC X(09) VALUE "099800151".
      *    廃用症候群リハ開始日
           03  CONS-HAIYO           PIC X(09) VALUE "099800191".
      *
      *    心大リハ終了日
           03  CONS-SINDAI-E        PIC X(09) VALUE "099800112".
      *    呼吸器リハ終了日
           03  CONS-KOKYUKI-E       PIC X(09) VALUE "099800142".
      *    脳血管疾患等リハ終了日
           03  CONS-NOUKE-E         PIC X(09) VALUE "099800122".
      *    運動器リハ終了日
           03  CONS-UNDOKI-E        PIC X(09) VALUE "099800132".
      *    難病患者リハ終了日
           03  CONS-NANBYO-E        PIC X(09) VALUE "099800162".
      *    障害児（者）リハ終了日
           03  CONS-SYOGAI-E        PIC X(09) VALUE "099800172".
      *    摂食療法終了日
           03  CONS-SESSYOKU-E      PIC X(09) VALUE "099800152".
      *    廃用症候群リハ開始日
           03  CONS-HAIYO-E         PIC X(09) VALUE "099800192".
      *
      *    心大リハ加算開始日
           03  CONS-SINDAI-K        PIC X(09) VALUE "099800211".
      *    呼吸器リハ加算開始日
           03  CONS-KOKYUKI-K       PIC X(09) VALUE "099800241".
      *    廃用症候群リハ加算開始日
           03  CONS-HAIYO-K         PIC X(09) VALUE "099800291".
      *
      *    心大血管疾患リハビリテーション医学管理料（１）
           03  CONS-SINDAI-IGAKU1   PIC X(09) VALUE "180029910".
      *    心大血管疾患リハビリテーション医学管理料（２）
           03  CONS-SINDAI-IGAKU2   PIC X(09) VALUE "180030010".
      *    呼吸器リハビリテーション医学管理料（１）
           03  CONS-KOKYUKI-IGAKU1  PIC X(09) VALUE "180030510".
      *    呼吸器リハビリテーション医学管理料（２）
           03  CONS-KOKYUKI-IGAKU2  PIC X(09) VALUE "180030610".
      *    脳血管疾患等リハビリテーション医学管理料（１）
           03  CONS-NOUKE-IGAKU1    PIC X(09) VALUE "180030110".
      *    脳血管疾患等リハビリテーション医学管理料（２）
           03  CONS-NOUKE-IGAKU2    PIC X(09) VALUE "180030210".
      *    運動器リハビリテーション医学管理料（１）
           03  CONS-UNDOKI-IGAKU1   PIC X(09) VALUE "180030310".
      *    運動器リハビリテーション医学管理料（２）
           03  CONS-UNDOKI-IGAKU2   PIC X(09) VALUE "180030410".
      *
      *    心大血管疾患リハ実施
           03  CONS-SINDAI-DUMMY    PIC X(09) VALUE "099800311".
      *    呼吸器リハ実施
           03  CONS-KOKYUKI-DUMMY   PIC X(09) VALUE "099800341".
      *    脳血管疾患リハ実施
           03  CONS-NOUKE-DUMMY     PIC X(09) VALUE "099800321".
      *    運動器リハ実施
           03  CONS-UNDOKI-DUMMY    PIC X(09) VALUE "099800331".
      *
      *    小児特定疾患カウンセリング料（１回目）
           03  CONS-SYOUNI          PIC X(09) VALUE "113000810".
      *    小児特定疾患カウンセリング料（医師）（初回）
           03  CONS-SYOUNI2         PIC X(09) VALUE "113702610".
      *    小児特定疾患カウンセリング料（医師・初回）（通信機）
           03  CONS-SYOUNI3         PIC X(09) VALUE "113703210".
      *
      *    心臓ペースメーカー指導管理料（遠隔モニタリング）
           03  CONS-SINPESU         PIC X(09) VALUE "113001510".
      *    遠隔モニタリング加算
           03  CONS-SINPESU2        PIC X(09) VALUE "113018070".
      *
      *    認知療法・認知行動療法
           03  CONS-NINRYO          PIC X(09) VALUE "180033210".
      *    認知療法・認知行動療法（精神科救急体制の精神保健指定医）
           03  CONS-NINRYO2         PIC X(09) VALUE "180035910".
      *    医療観察認知療法・認知行動療法（精神科救急体制の精神保健指定医）
           03  CONS-NINRYO4         PIC X(09) VALUE "188007010".
      *    認知療法・認知行動療法（精神保健指定医と看護師が共同で行う場合）
           03  CONS-NINRYO5         PIC X(09) VALUE "180047810".
      *    医療観察認知療法・認知行動療法（精神保健指定医と看護師が共同）
           03  CONS-NINRYO7         PIC X(09) VALUE "188010610".
      *    認知療法・認知行動療法（医師）
           03  CONS-NINRYO8         PIC X(09) VALUE "180056410".
      *
      *    認知症専門診断管理料２
           03  CONS-NINSEN          PIC X(09) VALUE "113014610".
      *    小児運動器疾患指導管理料
           03  CONS-SYOUNDO         PIC X(09) VALUE "113024310".
      *    療養・就労両立支援指導料
           03  CONS-RYOSYU          PIC X(09) VALUE "113027910".
      *    認知症サポート指導料
           03  CONS-NINSAPO         PIC X(09) VALUE "113028310".
      *    一般不妊治療管理料
           03  CONS-FUNIN           PIC X(09) VALUE "113036210".
      *    認知症専門診断管理料２（連携型）
           03  CONS-NINSEN2         PIC X(09) VALUE "113042610".
      *    遠隔連携診療料（診断を目的）
           03  CONS-ENREN1          PIC X(09) VALUE "113032110".
      *    手順書加算（精神科訪問看護指示料）
           03  CONS-TEJSEI          PIC X(09) VALUE "180067770".
      *
      *    介護職員等喀痰吸引等指示料
           03  CONS-KAISYOKU        PIC X(09) VALUE "114020910".
      *
      *    救急医療管理加算
           03  CONS-KYUKYU          PIC X(09) VALUE "101110030".
           03  CONS-KYUKYU2         PIC X(09) VALUE "101800890".
      *
      *    心臓ペースメーカー指導管理料（着用型自動除細動器）
           03  CONS-SINPESUR1       PIC X(09) VALUE "113015010".
      *    心臓ペースメーカー指導管理料（ロ・植込型除細動器移行期加算含む）
           03  CONS-SINPESUR2       PIC X(09) VALUE "113015130".
      *    心臓ペースメーカー指導管理料（イ以外）
           03  CONS-SINPESUR3       PIC X(09) VALUE "113001610".
      *    心臓ペースメーカー指導管理料（ペースメーカー）
           03  CONS-SINPESUR4       PIC X(09) VALUE "113030210".
      *    心臓ペースメーカー指導管理料（植込型除細動器等）
           03  CONS-SINPESUR5       PIC X(09) VALUE "113030310".
      *
      *    がん患者指導管理料（心理的不安軽減のため面接）
           03  CONS-GANSIDO1        PIC X(09) VALUE "113015310".
      *    がん患者指導管理料（抗悪性腫瘍剤の必要等文書説明）
           03  CONS-GANSIDO2        PIC X(09) VALUE "113015410".
      *    がん患者指導管理料（心理的不安軽減のため面接・情報通信機器）
           03  CONS-GANSIDO3        PIC X(09) VALUE "113035410".
      *    がん患者指導管理料（抗悪性腫瘍剤の必要性文書説明・情報通信機器）
           03  CONS-GANSIDO4        PIC X(09) VALUE "113035510".
      *
      *    在宅患者共同診療料開始日
           03  CONS-ZAIKYO          PIC X(09) VALUE "099140004".
      *
      *    磁気による膀胱等刺激法開始日
           03  CONS-BOKOHO1         PIC X(09) VALUE "099400001".
      *    磁気による膀胱等刺激法
           03  CONS-BOKOHO2         PIC X(09) VALUE "140055110".
      *
      *    通院・在宅精神療法開始日
           03  CONS-TUINZAI         PIC X(09) VALUE "099830102".
      *
      *    依存症集団療法開始日（薬物依存症）
           03  CONS-IZONSYO         PIC X(09) VALUE "099830103".
      *    依存症集団療法開始日（ギャンブル依存症）
           03  CONS-IZONSYO2        PIC X(09) VALUE "099830105".
      *    依存症集団療法開始日（アルコール依存症）
           03  CONS-IZONSYO3        PIC X(09) VALUE "099830109".
      *
      *    精神科ＳＣ疾患別等専門プログラム加算開始日
           03  CONS-SCPRO           PIC X(09) VALUE "099830104".
      *
      *    経頭蓋磁気刺激療法開始日
           03  CONS-JIKSGK          PIC X(09) VALUE "099830106".
      *    経頭蓋磁気刺激療法終了日
           03  CONS-JIKSGK-E        PIC X(09) VALUE "099830107".
      *
      *    精神科重症患者早期集中支援管理料１（単一建物１人）
           03  CONS-SEIJYU          PIC X(09) VALUE "180042110".
      *    精神科重症患者早期集中支援管理料２（単一建物１人）
           03  CONS-SEIJYU2         PIC X(09) VALUE "180042410".
      *    精神科重症患者早期集中支援管理料１（単一建物２人以上）
           03  CONS-SEIJYU3         PIC X(09) VALUE "180048710".
      *    精神科重症患者早期集中支援管理料２（単一建物２人以上）
           03  CONS-SEIJYU4         PIC X(09) VALUE "180048810".
      *
      *    精神科在宅患者支援管理料１（集中的支援必要）（単一建物１人）
           03  CONS-SEIZAI          PIC X(09) VALUE "180057210".
      *    精神科在宅患者支援管理料１（集中的支援必要）（単一建物２人以上）
           03  CONS-SEIZAI2         PIC X(09) VALUE "180057310".
      *    精神科在宅患者支援管理料１（重度精神障害者）（単一建物１人）
           03  CONS-SEIZAI3         PIC X(09) VALUE "180057410".
      *    精神科在宅患者支援管理料１（重度精神障害者）（単一建物２人以上）
           03  CONS-SEIZAI4         PIC X(09) VALUE "180057510".
      *    精神科在宅患者支援管理料１（イ及びロ以外）（単一建物１人）
           03  CONS-SEIZAI5         PIC X(09) VALUE "180057610".
      *    精神科在宅患者支援管理料１（イ及びロ以外）（単一建物２人以上）
           03  CONS-SEIZAI6         PIC X(09) VALUE "180057710".
      *    精神科在宅患者支援管理料２（集中的支援必要）（単一建物１人）
           03  CONS-SEIZAI7         PIC X(09) VALUE "180057810".
      *    精神科在宅患者支援管理料２（集中的支援必要）（単一建物２人以上）
           03  CONS-SEIZAI8         PIC X(09) VALUE "180057910".
      *    精神科在宅患者支援管理料２（重度精神障害者）（単一建物１人）
           03  CONS-SEIZAI9         PIC X(09) VALUE "180058010".
      *    精神科在宅患者支援管理料２（重度精神障害者）（単一建物２人以上）
           03  CONS-SEIZAI10        PIC X(09) VALUE "180058110".
      *    精神科在宅患者支援管理料３（単一建物１人）
           03  CONS-SEIZAI11        PIC X(09) VALUE "180064010".
      *    精神科在宅患者支援管理料３（単一建物２人以上）
           03  CONS-SEIZAI12        PIC X(09) VALUE "180064110".
      *
      *    在宅半固形栄養経管栄養法指導管理料
           03  CONS-ZAIHANKO        PIC X(09) VALUE "114045510".
      *
      *    乳腺炎重症化予防ケア・指導料（初回）
           03  CONS-NYUSEN          PIC X(09) VALUE "113024410".
      *    乳腺炎重症化予防ケア・指導料（２回目から４回目まで）
           03  CONS-NYUSEN2         PIC X(09) VALUE "113024510".
      *
      *    乳腺炎重症化予防ケア・指導料２（初回）
           03  CONS-NYUSEN3         PIC X(09) VALUE "113704010".
      *    乳腺炎重症化予防ケア・指導料２（２回目から８回目まで）
           03  CONS-NYUSEN4         PIC X(09) VALUE "113703910".
      *
      *    在宅酸素療法指導管理料（その他）
           03  CONS-ZAISAN          PIC X(09) VALUE "114003710".
      *    在宅酸素療法指導管理料（その他）（特例）
           03  CONS-ZAISAN2         PIC X(09) VALUE "114055550".
      *    在宅持続陽圧呼吸療法指導管理料１
           03  CONS-YOUATU1         PIC X(09) VALUE "114040710".
      *    在宅持続陽圧呼吸療法指導管理料２
           03  CONS-YOUATU2         PIC X(09) VALUE "114040810".
      *    在宅持続陽圧呼吸療法指導管理料
           03  CONS-YOUATU3         PIC X(09) VALUE "114009710".
      *
      *    高気圧酸素治療（減圧症又は空気塞栓）
           03  CONS-KIATU           PIC X(09) VALUE "140057510".
      *    高気圧酸素治療（その他のもの）
           03  CONS-KIATU2          PIC X(09) VALUE "140057610".
      *    高気圧酸素治療開始日
           03  CONS-KIATU3          PIC X(09) VALUE "099400002".
      *
      *    人工内耳機器調整加算（６歳未満）
           03  CONS-JINNAI1         PIC X(09) VALUE "113030670".
      *    人工内耳機器調整加算（６歳以上）
           03  CONS-JINNAI2         PIC X(09) VALUE "113030770".
      *
      *    往診料
           03  CONS-OSIN1           PIC X(09) VALUE "114000110".
      *    特別往診
           03  CONS-OSIN2           PIC X(09) VALUE "114001610".
      *
      *    精神科デイ・ケア等開始日
           03  CONS-SEIKEA          PIC X(09) VALUE "099830108".
      *
      *    二次性骨折予防継続管理料３
           03  CONS-NIJISEI         PIC X(09) VALUE "113036710".
      *    アレルギー性鼻炎免疫療法治療管理料（１月目）
           03  CONS-AREBI           PIC X(09) VALUE "113036810".
      *    遠隔連携診療料（その他）
           03  CONS-ENREN2          PIC X(09) VALUE "113042910".
      *    こころの連携指導料（１）
           03  CONS-COCO1           PIC X(09) VALUE "113043010".
      *    こころの連携指導料（２）
           03  CONS-COCO2           PIC X(09) VALUE "113043110".
      *    在宅抗菌薬吸入療法用ネブライザ加算（１月目）
           03  CONS-KOUYAK          PIC X(09) VALUE "114051750".
      *    手順書加算（訪問看護指示料）
           03  CONS-TEJHOU          PIC X(09) VALUE "114061070".
      *
      *    慢性腎臓病透析予防指導管理料（１年以内）
           03  CONS-MANJIN          PIC X(09) VALUE "113704110".
      *    慢性腎臓病透析予防指導管理料（１年以内）（情報通信機器）
           03  CONS-MANJIN2         PIC X(09) VALUE "113704210".
      *
      *    在宅患者共同診療料テーブル
       01  TBL-ZAIKYO-CHECK-AREA.
           03  FILLER          PIC X(09)   VALUE    "114027610".
           03  FILLER          PIC X(09)   VALUE    "114027710".
           03  FILLER          PIC X(09)   VALUE    "114027810".
           03  FILLER          PIC X(09)   VALUE    "114027910".
      *
       01  TBL-ZAIKYO-CHECK-R  REDEFINES   TBL-ZAIKYO-CHECK-AREA.
           03  TBL-ZAIKYO-CHECK     PIC X(09)   OCCURS    4.
      *
       01  TBL-ZAIKYO-CHECK-MAX     PIC 9(02)   VALUE     4.
      *
      *    複数診療科受診の退避テーブル
       01  TBL-JUSHIN-INF.
           03  TBL-JUSHIN-INF2      OCCURS   100.
               05  TBL-JUSHIN-SRYKA       PIC X(02).
               05  TBL-JUSHIN-SRYKANAME   PIC X(80).
               05  TBL-JUSHIN-ARI         PIC X(01).
               05  TBL-JUSHIN-DAY         PIC X(01)   OCCURS  31.
      *
       01  TBL-HASSYO-INF.
           03  TBL-HASSYO-KAITATU-COMMENT   PIC 9(01).
           03  TBL-HASSYO-SINDAI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-COMMENT   PIC 9(01).
           03  TBL-HASSYO-NOUKE-COMMENT     PIC 9(01).
           03  TBL-HASSYO-UNDOKI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-NANBYO-COMMENT    PIC 9(01).
           03  TBL-HASSYO-SYOGAI-COMMENT    PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-COMMENT  PIC 9(01).
           03  TBL-HASSYO-HAIYO-COMMENT     PIC 9(01).
      *
           03  TBL-HASSYO-SINDAI-DAY-T.
               05  TBL-HASSYO-SINDAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-SINDAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-KOKYUKI-DAY-T.
               05  TBL-HASSYO-KOKYUKI-DAY-OCC     OCCURS  31.
                   07  TBL-HASSYO-KOKYUKI-SANTEI  PIC 9(01).
           03  TBL-HASSYO-NOUKE-DAY-T.
               05  TBL-HASSYO-NOUKE-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-NOUKE-SANTEI    PIC 9(01).
           03  TBL-HASSYO-UNDOKI-DAY-T.
               05  TBL-HASSYO-UNDOKI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-UNDOKI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-NANBYO-DAY-T.
               05  TBL-HASSYO-NANBYO-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-NANBYO-SANTEI   PIC 9(01).
           03  TBL-HASSYO-SYOGAI-DAY-T.
               05  TBL-HASSYO-SYOGAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO-SYOGAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO-SESSYOKU-DAY-T.
               05  TBL-HASSYO-SESSYOKU-DAY-OCC    OCCURS  31.
                   07  TBL-HASSYO-SESSYOKU-SANTEI PIC 9(01).
           03  TBL-HASSYO-HAIYO-DAY-T.
               05  TBL-HASSYO-HAIYO-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-HAIYO-SANTEI    PIC 9(01).
           03  TBL-HASSYO-TAIHI-DAY-T.
               05  TBL-HASSYO-TAIHI-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO-TAIHI-SANTEI    PIC 9(01).
      *
           03  TBL-HASSYO-TAIHIYMD1-DAY-T.
               05  TBL-HASSYO-TAIHIYMD1-FLG       PIC 9(01).
               05  TBL-HASSYO-TAIHIYMD1-DAY-OCC1.
                   07  TBL-HASSYO-TAIHIYMD11      PIC X(08) OCCURS  2.
               05  TBL-HASSYO-TAIHIYMD1-DAY-OCC2.
                   07  TBL-HASSYO-TAIHIYMD12      PIC 9(01) OCCURS 31.
      *
      *****（リハ加算用）
       01  TBL-HASSYO9-INF.
           03  TBL-HASSYO9-SINDAI-COMMENT    PIC 9(01).
           03  TBL-HASSYO9-KOKYUKI-COMMENT   PIC 9(01).
           03  TBL-HASSYO9-HAIYO-COMMENT     PIC 9(01).
      *
           03  TBL-HASSYO9-SINDAI-DAY-T.
               05  TBL-HASSYO9-SINDAI-DAY-OCC      OCCURS  31.
                   07  TBL-HASSYO9-SINDAI-SANTEI   PIC 9(01).
           03  TBL-HASSYO9-KOKYUKI-DAY-T.
               05  TBL-HASSYO9-KOKYUKI-DAY-OCC     OCCURS  31.
                   07  TBL-HASSYO9-KOKYUKI-SANTEI  PIC 9(01).
           03  TBL-HASSYO9-HAIYO-DAY-T.
               05  TBL-HASSYO9-HAIYO-DAY-OCC       OCCURS  31.
                   07  TBL-HASSYO9-HAIYO-SANTEI    PIC 9(01).
      *****
      *
       01  TBL-ZAISIEN-INF.
           03  TBL-ZAISIEN-COMMENT          PIC 9(01).
      *
       01  TBL-HOMONCOM-NINZU-INF.
           03  TBL-HOMONCOM-SETFLG          PIC 9(01).
           03  TBL-HOMONCOM-NINZU           PIC 9(03)  OCCURS  31.
      *
      *    酸素（金額・数量）情報
       01  TBL-SANSO-INF.
           03  TBL-SANSO-INF2       OCCURS    10.
               05  TBL-SANSO-TEN          PIC 9(09)V9(02).
               05  TBL-SANSO-SRYSURYO     PIC 9(05)V9(05).
           03  TBL-SANSO-INF3.
               05  TBL-KASAN-SRYSURYO     PIC 9(05)V9(05).
      *
      *    薬剤逓減情報
       01  TBL-YAKUTEI-INF.
           03  TBL-YT1-OCC                      OCCURS 100.
             05  TBL-YT1-KEY.
               07  TBL-YT1-SRYKBN               PIC X(02).
               07  TBL-YT1-JIHOKBN              PIC X(01).
      ***      07  TBL-YT1-SRYKA                PIC X(02).
             05  TBL-YT1-RECE44-REC             PIC X(2000).
             05  TBL-YT1-HIKAKU.
               07  TBL-YT1-ZAITEN               PIC 9(08).
               07  TBL-YT1-SRYCDTOTAL           PIC 9(14).
               07  TBL-YT1-SURYOUTOTAL          PIC 9(07)V9(05).
               07  TBL-YT1-RUIKEITOTAL          PIC 9(13)V9(05).
               07  TBL-YT1-RUIKEITOTAL2         PIC 9(13)V9(05).
               07  TBL-YT1-RUIKEITOTAL3         PIC 9(13)V9(05).
               07  TBL-YT1-RUIKEITOTAL4         PIC 9(14).
               07  TBL-YT1-MEISAISU             PIC 9(04).
             05  TBL-YT1-RECE44-DAY-AREA.
               07  TBL-YT1-OCC2                 OCCURS  31.
                 09  TBL-YT1-OCC3               OCCURS   9.
                   11  TBL-YT1-KAISU            PIC 9(03).
             05  TBL-YT1-COMNAME-AREA.
               07  TBL-YT1-OCC4                 OCCURS   5.
                   11  TBL-YT1-COMCD            PIC X(09).
                   11  TBL-YT1-COMNAME          PIC X(300).
             05  TBL-YT1-FLG                    PIC 9(01).
      *
      *    並び替え用
       01  WRK-YAKUTEI-DATA                     PIC X(4492).
      *
       01  WRK-TAIHI-YT1-KEY                    PIC X(03).
       01  WRK-YAKUTEI-ZAINUM                   PIC 9(08).
       01  WRK-YAKUTEI-ZAINUM2                  PIC 9(08).
      *
      *    薬剤逓減情報（キー単位の編集テーブル）
       01  TBL-YAKUTEI-INF2.
           03  TBL-YT2-DAY                    OCCURS      31.
             05  TBL-YT2X-KAIT                  OCCURS     9.
               06  TBL-YT2X1-OCC                  OCCURS  25.
                 07  TBL-YT2X1-RECE44-REC         PIC X(2000).
                 07  TBL-YT2X1-KAISU              PIC 9(03).
                 07  TBL-YT2X1-DAY-AREA.
                   09  TBL-YT2X1-OCC2               OCCURS  31.
                       11  TBL-YT2X1-DAY            PIC 9(03).
                 07  TBL-YT2X1-COMNAME-AREA.
                   09  TBL-YT2X1-OCC4               OCCURS   5.
                       11  TBL-YT2X1-COMCD          PIC X(09).
                       11  TBL-YT2X1-COMNAME        PIC X(300).
               06  TBL-YT2X2-OCC                  OCCURS   3.
                 07  TBL-YT2X2-RECE44-REC         PIC X(2000).
                 07  TBL-YT2X2-KAISU              PIC 9(03).
                 07  TBL-YT2X2-DAY-AREA.
                   09  TBL-YT2X2-OCC2               OCCURS  31.
                       11  TBL-YT2X2-DAY            PIC 9(03).
             05  TBL-YT2Y-KAIT                  OCCURS     9.
               06  TBL-YT2Y1-OCC                  OCCURS  26.
                 07  TBL-YT2Y1-HIKAKU.
                   09  TBL-YT2Y1-ZAITEN           PIC 9(08).
                   09  TBL-YT2Y1-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT2Y1-SURYOUTOTAL      PIC 9(07)V9(05).
                   09  TBL-YT2Y1-RUIKEITOTAL      PIC 9(13)V9(05).
                   09  TBL-YT2Y1-RUIKEITOTAL2     PIC 9(13)V9(05).
                   09  TBL-YT2Y1-RUIKEITOTAL3     PIC 9(13)V9(05).
                   09  TBL-YT2Y1-RUIKEITOTAL4     PIC 9(14).
                   09  TBL-YT2Y1-MEISAISU         PIC 9(04).
               06  TBL-YT2Y2-OCC                  OCCURS   4.
                 07  TBL-YT2Y2-HIKAKU.
                   09  TBL-YT2Y2-ZAITEN           PIC 9(08).
                   09  TBL-YT2Y2-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT2Y2-SURYOUTOTAL      PIC 9(07)V9(05).
                   09  TBL-YT2Y2-RUIKEITOTAL      PIC 9(13)V9(05).
                   09  TBL-YT2Y2-RUIKEITOTAL2     PIC 9(13)V9(05).
                   09  TBL-YT2Y2-RUIKEITOTAL3     PIC 9(13)V9(05).
                   09  TBL-YT2Y2-RUIKEITOTAL4     PIC 9(14).
                   09  TBL-YT2Y2-MEISAISU         PIC 9(04).
      *
      *    薬剤逓減情報（まとめ用の編集テーブル）
       01  TBL-YAKUTEI-INF3.
           03  TBL-YT3-XXX                    OCCURS     100.
             05  TBL-YT3X-KAIT.
               06  TBL-YT3X1-OCC                  OCCURS  25.
                 07  TBL-YT3X1-RECE44-REC         PIC X(2000).
                 07  TBL-YT3X1-KAISU              PIC 9(03).
                 07  TBL-YT3X1-DAY-AREA.
                   09  TBL-YT3X1-OCC2               OCCURS  31.
                       11  TBL-YT3X1-DAY            PIC 9(03).
                 07  TBL-YT3X1-COMNAME-AREA.
                   09  TBL-YT3X1-OCC4               OCCURS   5.
                       11  TBL-YT3X1-COMCD          PIC X(09).
                       11  TBL-YT3X1-COMNAME        PIC X(300).
               06  TBL-YT3X2-OCC                  OCCURS   3.
                 07  TBL-YT3X2-RECE44-REC         PIC X(2000).
                 07  TBL-YT3X2-KAISU              PIC 9(03).
                 07  TBL-YT3X2-DAY-AREA.
                   09  TBL-YT3X2-OCC2               OCCURS  31.
                       11  TBL-YT3X2-DAY            PIC 9(03).
             05  TBL-YT3Y-KAIT.
               06  TBL-YT3Y1-OCC                  OCCURS  26.
                 07  TBL-YT3Y1-HIKAKU.
                   09  TBL-YT3Y1-ZAITEN           PIC 9(08).
                   09  TBL-YT3Y1-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT3Y1-SURYOUTOTAL      PIC 9(07)V9(05).
                   09  TBL-YT3Y1-RUIKEITOTAL      PIC 9(13)V9(05).
                   09  TBL-YT3Y1-RUIKEITOTAL2     PIC 9(13)V9(05).
                   09  TBL-YT3Y1-RUIKEITOTAL3     PIC 9(13)V9(05).
                   09  TBL-YT3Y1-RUIKEITOTAL4     PIC 9(14).
                   09  TBL-YT3Y1-MEISAISU         PIC 9(04).
               06  TBL-YT3Y2-OCC                  OCCURS   4.
                 07  TBL-YT3Y2-HIKAKU.
                   09  TBL-YT3Y2-ZAITEN           PIC 9(08).
                   09  TBL-YT3Y2-SRYCDTOTAL       PIC 9(14).
                   09  TBL-YT3Y2-SURYOUTOTAL      PIC 9(07)V9(05).
                   09  TBL-YT3Y2-RUIKEITOTAL      PIC 9(13)V9(05).
                   09  TBL-YT3Y2-RUIKEITOTAL2     PIC 9(13)V9(05).
                   09  TBL-YT3Y2-RUIKEITOTAL3     PIC 9(13)V9(05).
                   09  TBL-YT3Y2-RUIKEITOTAL4     PIC 9(14).
                   09  TBL-YT3Y2-MEISAISU         PIC 9(04).
      *
      *    画像診断の部位コード編集用
       01  TBL-GAZO-INF.
           03  TBL-GAZO-INF2                      OCCURS 10.
             05  TBL-GAZO-CHEKFLG                 PIC 9(02).
      *
      *    在医総管、施医総管の選択式コメント編集用
       01  TBL-ZAIISO-INF.
           03  TBL-ZAIISO-INF2                    OCCURS 31.
             05  TBL-ZAIISO-FLG                   PIC 9(01).
      *
      *    下肢創傷処置管理料の選択式コメント編集用
       01  TBL-SOUKAN-INF.
           03  TBL-SOUKAN-INF2                    OCCURS 31.
             05  TBL-SOUKAN-FLG                   PIC 9(01).
      *
      *    酸素 
           COPY    "CMSANSOTBL.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    算定履歴付加
       01  SANTEIPLUS-REC.
           COPY    "CPSANTEIPLUS.INC".
      *
      *    レセプト記載
       01  RKISAI-REC.
           COPY    "CPRECEKISAI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    オンライン診療料等に関わる算定履歴チェックサブ
           COPY    "CPORCSRECESRCHK.INC".
      *
      *    レセプト自動記載コメント編集サブ
           COPY    "CPORCSRECECOMMENT.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END  =  1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           PERFORM 120-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCR0551"      TO  JOB-PGID
           MOVE    "コメント編集（労災・自賠責）（レセ電用）" 
                                   TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *    
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    "RECE44E"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
      *
           MOVE    "RECE45C"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    "RECE44F"           TO  RECE441PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE441PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE441PARA-HOSPNUM
      *
           MOVE    "RECE45F"           TO  RECE451PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE451PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE451PARA-HOSPNUM
      *
           MOVE    "RECE44G"           TO  RECE449PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE449PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE449PARA-HOSPNUM
      *
           MOVE    "RECE45G"           TO  RECE459PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE459PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE459PARA-HOSPNUM
      *
           MOVE    "RECE456"           TO  RECE455PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE455PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE455PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE41PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE44PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE45PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE441PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    RECE451PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           MOVE    RECE449PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (7)
           MOVE    RECE459PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (8)
           MOVE    RECE455PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (9)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE41PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE44PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE45PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE441PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE451PARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE449PARA
           MOVE    SGETTEMP-FULLNAMES (8)
                                       TO  RECE459PARA
           MOVE    SGETTEMP-FULLNAMES (9)
                                       TO  RECE455PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    INPUT               RECE41-FILE
           IF      STS-RECE41          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE41PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE41          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE44-FILE
           IF      STS-RECE44          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE44PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE44          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE45-FILE
           IF      STS-RECE45          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE45PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE45          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE441-FILE
           CLOSE                       RECE441-FILE
      *
           OPEN    OUTPUT              RECE451-FILE
           CLOSE                       RECE451-FILE
      *
           OPEN    OUTPUT              RECE449-FILE
           CLOSE                       RECE449-FILE
      *
           OPEN    OUTPUT              RECE459-FILE
           CLOSE                       RECE459-FILE
      *
           OPEN    OUTPUT              RECE455-FILE
           CLOSE                       RECE455-FILE
      *
           OPEN    I-O                 RECE441-FILE
           OPEN    I-O                 RECE449-FILE
      *
           OPEN    I-O                 RECE451-FILE
           OPEN    I-O                 RECE459-FILE
      *
           OPEN    OUTPUT              RECE455-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE       ZERO             TO  CNT-SANTEIKA
                                           CNT-DOUJITU
                                           CNT-DENWA
                                           CNT-DOUDEN
                                           CNT-DENWA2
                                           CNT-DOUDEN2
      *
           INITIALIZE                      TBL-HASSYO-INF
           INITIALIZE                      TBL-HASSYO9-INF
           INITIALIZE                      TBL-ZAISIEN-INF
           INITIALIZE                      TBL-HOMONCOM-NINZU-INF
      *
           INITIALIZE                      TBL-YAKUTEI-INF
      *
           INITIALIZE                      WRK-JIDO-SETCOM-AREA
      *
           INITIALIZE                      TBL-ZAIISO-INF
           INITIALIZE                      TBL-SOUKAN-INF
      *
           MOVE       ZERO             TO  FLG-OSIN-HENSYU
           MOVE       ZERO             TO  FLG-ZAIHOU-HENSYU
           MOVE       ZERO             TO  FLG-TAKYO-HENSYU
           MOVE       ZERO             TO  FLG-SOUKAN-HENSYU
      *
           MOVE       ZERO             TO  IDX6
      *
           MOVE       ZERO             TO  IDX33
           MOVE       ZERO             TO  IDX34
           MOVE       ZERO             TO  IDX56
      *
           MOVE  ZERO        TO    CNT-KENSA
           IF   RECE41-KENSA-BNPFLG     =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
           IF   RECE41-KENSA-NTPROFLG   =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
           IF   RECE41-KENSA-ANPFLG     =   1
                 ADD    1    TO    CNT-KENSA
           END-IF
      *
           IF   RECE41-COMCNTFLG   =   "1"
      *
             PERFORM 800-RECE41-SET44-SEC
             MOVE    ZERO                TO  RECE44-ZAINUM
      *
             START   RECE44-FILE
                 KEY IS    >=   RECE44-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE44
                 NOT INVALID
                     PERFORM 990-RECE44-NEXT-SEC
             END-START
             PERFORM
               UNTIL  ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                  EVALUATE      RECE44-SRYKBN
                      WHEN      "11"
      *               （初診料算定科：のカウント）
                         IF  RECE44-SYOSANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
                      WHEN      "12"
      *               （再診料算定科：のカウント）
                         IF  RECE44-SAISANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
      *               （電話再診のカウント）
                         IF  RECE44-DENWASAI-COMMENT     =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DENWA
                         END-IF
      *               （同日再診のカウント）
                         IF  RECE44-DOUJITUSAI-COMMENT   =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUJITU
                         END-IF
      *               （同日電話再診のカウント）
                         IF  RECE44-DOUDENSAI-COMMENT    =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUDEN
                         END-IF
      *               （電話再診（３０年３月以前継続）のカウント）
                         IF  RECE44-DENWASAI2-COMMENT    =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DENWA2
                         END-IF
      *               （同日電話再診（３０年３月以前継続）のカウント）
                         IF  RECE44-DOUDENSAI2-COMMENT   =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUDEN2
                         END-IF
                      WHEN      "13"
      *               （退院時共同指導料特別管理指導加算に関する編集）
                         IF  RECE44-FLG-TAKYO-COMMENT    =  "1"
                            MOVE  1           TO  FLG-TAKYO-HENSYU
                         END-IF
                      WHEN      "14"
      *               （在支援に関する診療のカウント）
                         IF  RECE44-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                            MOVE  RECE44-REC  TO  WRK-RECE44-REC
                            EVALUATE  RECE44-FLG-ZAISIEN-COMMENT
                              WHEN      "1"
                                MOVE  1  TO  TBL-ZAISIEN-COMMENT
                              WHEN      "2"
                                MOVE  2  TO  TBL-ZAISIEN-COMMENT
                            END-EVALUATE
                         END-IF
      *               （往診に関する編集）
                         IF  RECE44-FLG-OSIN-COMMENT     =  "2"
                            MOVE  1           TO  FLG-OSIN-HENSYU
                            IF  RECE41-SRYYM  >= "202010"
                              PERFORM  VARYING  IDX1  FROM  1  BY  1
                                        UNTIL   IDX1    >  31
                                IF  RECE44-KAISU(1 IDX1)  NOT = ZERO
                                  MOVE  1  TO  TBL-ZAIISO-FLG(IDX1)
                                END-IF
                              END-PERFORM
                            END-IF
                         END-IF
      *               （在宅患者訪問診療料に関する編集）
                         IF  RECE44-FLG-ZAIHOU-COMMENT   =  "3"
                            MOVE  1           TO  FLG-ZAIHOU-HENSYU
                            IF  RECE41-SRYYM  >= "202010"
                              PERFORM  VARYING  IDX1  FROM  1  BY  1
                                        UNTIL   IDX1    >  31
                                IF  RECE44-KAISU(1 IDX1)  NOT = ZERO
                                  MOVE  1  TO  TBL-ZAIISO-FLG(IDX1)
                                END-IF
                              END-PERFORM
                            END-IF
                         END-IF
                      WHEN      "40"
      *               （介達牽引あり）
                         IF  RECE44-KAITATU-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KAITATU-COMMENT
                         END-IF
      *               （下肢創傷処置管理料に関する編集）
                         IF  RECE44-FLG-SOUSYO-COMMENT =  "1"
                           MOVE    1   TO  FLG-SOUKAN-HENSYU
                           PERFORM  VARYING  IDX1  FROM  1  BY  1
                                     UNTIL   IDX1    >  31
                             IF  RECE44-KAISU(1 IDX1)  NOT = ZERO
                               MOVE  1   TO  TBL-SOUKAN-FLG(IDX1)
                             END-IF
                           END-PERFORM
                         END-IF
                      WHEN      "80"
      *               （リハビリあり）
                         IF  RECE44-SINDAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SINDAI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-SINDAI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-KOKYUKI-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KOKYUKI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-KOKYUKI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-NOUKE-COMMENT        =  "1"
                            MOVE   1   TO  TBL-HASSYO-NOUKE-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-NOUKE-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-UNDOKI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-UNDOKI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-UNDOKI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-NANBYO-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-NANBYO-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-NANBYO-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-SYOGAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SYOGAI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-SYOGAI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-SESSYOKU-COMMENT     =  "1"
                            MOVE   1   TO  TBL-HASSYO-SESSYOKU-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-SESSYOKU-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-HAIYO-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-HAIYO-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-HAIYO-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
      *
                         IF  RECE44-SINDAI9-COMMENT       =  "1"
                         OR  RECE44-SINDAI9X-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO9-SINDAI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO9-SINDAI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-KOKYUKI9-COMMENT      =  "1"
                         OR  RECE44-KOKYUKI9X-COMMENT     =  "1"
                            MOVE   1   TO  TBL-HASSYO9-KOKYUKI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO9-KOKYUKI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-HAIYO9-COMMENT       =  "1"
                         OR  RECE44-HAIYO9X-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO9-HAIYO-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO9-HAIYO-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                      WHEN      "99"
      *                  （訪問診療にかかる記録書コメントに関する編集）
                         IF  RECE44-HOMONCOMFLG          =  "2"
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE  1  TO  TBL-HOMONCOM-SETFLG
                              ADD  RECE44-KAISU(1 IDX1)  TO
                                           TBL-HOMONCOM-NINZU(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                  END-EVALUATE
               ELSE                                                     
                  EVALUATE      RECE44-SRYKBN  
                      WHEN      "11"
      *               （初診料算定科：のカウント）
                         IF  RECE44-SYOSANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
                      WHEN      "12"
      *               （再診料算定科：のカウント）
                         IF  RECE44-SAISANTEIKA-COMMENT  =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-SANTEIKA
                         END-IF
      *               （電話再診のカウント）
                         IF  RECE44-DENWASAI-COMMENT     =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DENWA
                         END-IF
      *               （同日再診のカウント）
                         IF  RECE44-DOUJITUSAI-COMMENT   =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUJITU
                         END-IF
      *               （同日電話再診のカウント）
                         IF  RECE44-DOUDENSAI-COMMENT    =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUDEN
                         END-IF
      *               （電話再診（３０年３月以前継続）のカウント）
                         IF  RECE44-DENWASAI2-COMMENT    =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DENWA2
                         END-IF
      *               （同日電話再診（３０年３月以前継続）のカウント）
                         IF  RECE44-DOUDENSAI2-COMMENT   =  "1"
                            ADD    RECE44-ZAIKAISU   TO   CNT-DOUDEN2
                         END-IF
                      WHEN      "13"
      *               （退院時共同指導料特別管理指導加算に関する編集）
                         IF  RECE44-FLG-TAKYO-COMMENT    =  "1"
                            MOVE  1           TO  FLG-TAKYO-HENSYU
                         END-IF
                      WHEN      "14"
      *               （在支援に関する診療のカウント）
                         IF  RECE44-FLG-ZAISIEN-COMMENT   =  "1" OR "2"
                            MOVE  RECE44-REC  TO  WRK-RECE44-REC
                            EVALUATE  RECE44-FLG-ZAISIEN-COMMENT
                              WHEN      "1"
                                MOVE  1  TO  TBL-ZAISIEN-COMMENT
                              WHEN      "2"
                                MOVE  2  TO  TBL-ZAISIEN-COMMENT
                            END-EVALUATE
                         END-IF
      *               （往診に関する編集）
                         IF  RECE44-FLG-OSIN-COMMENT     =  "2"
                            MOVE  1           TO  FLG-OSIN-HENSYU
                            IF  RECE41-SRYYM  >= "202010"
                              PERFORM  VARYING  IDX1  FROM  1  BY  1
                                        UNTIL   IDX1    >  31
                                IF  RECE44-KAISU(1 IDX1)  NOT = ZERO
                                  MOVE  1  TO  TBL-ZAIISO-FLG(IDX1)
                                END-IF
                              END-PERFORM
                            END-IF
                         END-IF
      *               （在宅患者訪問診療料に関する編集）
                         IF  RECE44-FLG-ZAIHOU-COMMENT   =  "3"
                            MOVE  1           TO  FLG-ZAIHOU-HENSYU
                            IF  RECE41-SRYYM  >= "202010"
                              PERFORM  VARYING  IDX1  FROM  1  BY  1
                                        UNTIL   IDX1    >  31
                                IF  RECE44-KAISU(1 IDX1)  NOT = ZERO
                                  MOVE  1  TO  TBL-ZAIISO-FLG(IDX1)
                                END-IF
                              END-PERFORM
                            END-IF
                         END-IF
                      WHEN      "40"
                      WHEN      "80"
      *               （介達牽引あり）
                         IF  RECE44-KAITATU-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KAITATU-COMMENT
                         END-IF
      *               （下肢創傷処置管理料に関する編集）
                         IF  RECE44-FLG-SOUSYO-COMMENT =  "1"
                           MOVE    1   TO  FLG-SOUKAN-HENSYU
                           PERFORM  VARYING  IDX1  FROM  1  BY  1
                                     UNTIL   IDX1    >  31
                             IF  RECE44-KAISU(1 IDX1)  NOT = ZERO
                               MOVE  1   TO  TBL-SOUKAN-FLG(IDX1)
                             END-IF
                           END-PERFORM
                         END-IF
      *               （リハビリあり）
                         IF  RECE44-SINDAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SINDAI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-SINDAI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-KOKYUKI-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO-KOKYUKI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-KOKYUKI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-NOUKE-COMMENT        =  "1"
                            MOVE   1   TO  TBL-HASSYO-NOUKE-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-NOUKE-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-UNDOKI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-UNDOKI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-UNDOKI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-NANBYO-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-NANBYO-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-NANBYO-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-SYOGAI-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-SYOGAI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-SYOGAI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-SESSYOKU-COMMENT     =  "1"
                            MOVE   1   TO  TBL-HASSYO-SESSYOKU-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-SESSYOKU-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-HAIYO-COMMENT       =  "1"
                            MOVE   1   TO  TBL-HASSYO-HAIYO-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO-HAIYO-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
      *
                         IF  RECE44-SINDAI9-COMMENT       =  "1"
                         OR  RECE44-SINDAI9X-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO9-SINDAI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO9-SINDAI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-KOKYUKI9-COMMENT      =  "1"
                         OR  RECE44-KOKYUKI9X-COMMENT     =  "1"
                            MOVE   1   TO  TBL-HASSYO9-KOKYUKI-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO9-KOKYUKI-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                         IF  RECE44-HAIYO9-COMMENT       =  "1"
                         OR  RECE44-HAIYO9X-COMMENT      =  "1"
                            MOVE   1   TO  TBL-HASSYO9-HAIYO-COMMENT
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE 1 TO TBL-HASSYO9-HAIYO-SANTEI(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                      WHEN      "99"
      *                  （訪問診療にかかる記録書コメントに関する編集）
                         IF  RECE44-HOMONCOMFLG          =  "2"
                            PERFORM  VARYING  IDX1  FROM  1  BY  1
                                      UNTIL   IDX1   >   31
                             IF   RECE44-KAISU(1 IDX1)  NOT = ZERO
                              MOVE  1  TO  TBL-HOMONCOM-SETFLG
                              ADD  RECE44-KAISU(1 IDX1)  TO
                                           TBL-HOMONCOM-NINZU(IDX1)
                             END-IF
                            END-PERFORM
                         END-IF
                  END-EVALUATE                                          
               END-IF                                                   
      *
               PERFORM 990-RECE44-NEXT-SEC
      *
             END-PERFORM
      *
           END-IF
      *
           INITIALIZE    WRK-JIDO-SETCOM-AREA
      *
      *    発症日のコメント
           IF   RECE41-SRYYM      >=    "200604"
               PERFORM   2001-HASSYO-HANTEI-SEC
           ELSE
               MOVE      ZERO            TO   FLG-COMOKIKAE
               MOVE      CONS-KYUSEI     TO   WRK-SRYCD
               PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      *
      *    複数診療科受診
      *
           IF   CNT-SANTEIKA   >  ZERO
      *
             MOVE       ZERO             TO  CNT-JUSHIN
             INITIALIZE                      TBL-JUSHIN-INF
      *      診療科の取得
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE41-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"    TO  MCP-TABLE
               MOVE    "key2"            TO  MCP-PATHNAME
               PERFORM   900-SYSKANRI-READ-SEC
               PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX             >     100)
                            OR    (FLG-SYSKANRI    =       1)
                   MOVE    SYS-1005-KBNCD  TO TBL-JUSHIN-SRYKA(IDX)
                   MOVE    SYS-1005-SRYKANAME   
                                           TO TBL-JUSHIN-SRYKANAME(IDX)
      *            受診履歴テーブルから診療科毎の受診日を検索セット
                   PERFORM   2001-JUSHIN-KENSAKU-SEC
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM  900-SYSKANRI-READ-SEC
               END-PERFORM
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
      *      複数の診療科受診があるか判定
             MOVE       ZERO       TO   IDZ  
             INITIALIZE            WRK-FREE-AREA
      *
             PERFORM   VARYING   IDX    FROM   1   BY   1
                        UNTIL    IDX      >  100
               IF   TBL-JUSHIN-ARI (IDX)  =  "1"
                   ADD       1     TO   CNT-JUSHIN
                   PERFORM  2001-FREE-COMMENT-SEC
               END-IF
             END-PERFORM
      *      複数診療科受診のコメント編集
             IF    CNT-JUSHIN  >   1
                    PERFORM   2001-JUSHIN-COMMENT-SEC
             END-IF
      *
           END-IF
      *
      *    電話再診・同日複数再診
      *
           IF   CNT-DENWA     >   ZERO
           OR   CNT-DOUJITU   >   ZERO
           OR   CNT-DOUDEN    >   ZERO
           OR   CNT-DENWA2    >   ZERO
           OR   CNT-DOUDEN2   >   ZERO
                  PERFORM    2001-DENWA-DOUJITU-COMMENT-SEC
           END-IF
      *
      *    自賠責従来様式
           IF   RECE41-HKNKBN     =   "4"
           AND  WRK-PARA-RECEKBN  =   "5"
             EVALUATE  RECE41-JIBAIKBN
      *        加算率
               WHEN     "1"
                  PERFORM    2001-KASANRITU-COMMENT-SEC
               WHEN     "2"
      *           薬剤等点単価
                  IF    RECE41-SYUTENTANKA  =  RECE41-YKZTENTANKA
                     CONTINUE
                  ELSE
                     PERFORM    2001-YKZTENTANKA-COMMENT-SEC
                  END-IF
      *           乳幼児・幼児のコメント
                  IF   RECE41-NYUYOJIFLG   NOT =   SPACE
      *             誕生月で、誕生日が１日以外の日である場合
                    IF  (RECE41-BIRTHDAY(5:2)      =  RECE41-SRYYM(5:2))
                    AND (RECE41-BIRTHDAY(7:2)  NOT =  "01"             )
                    AND (RECE41-AGE                =   5               )
                    AND (RECE41-SRYYM             >=  "200604"         )
                          PERFORM   2001-NYUYOJI-COMMENT-SEC
                    END-IF
                  END-IF
             END-EVALUATE
           END-IF
      *
      *    第三者行為
           IF   RECE41-HKNKBN     =   "6"
      *           乳幼児・幼児のコメント
                  IF   RECE41-NYUYOJIFLG   NOT =   SPACE
      *             誕生月で、誕生日が１日以外の日である場合
                    IF  (RECE41-BIRTHDAY(5:2)      =  RECE41-SRYYM(5:2))
                    AND (RECE41-BIRTHDAY(7:2)  NOT =  "01"             )
                    AND (RECE41-AGE                =   5               )
                    AND (RECE41-SRYYM             >=  "200604"         )
                          PERFORM   2001-NYUYOJI-COMMENT-SEC
                    END-IF
                  END-IF
           END-IF
      *
      *    在支援コメント
           IF     TBL-ZAISIEN-COMMENT   NOT =  ZERO
               PERFORM     2001-ZAISIEN-COMMENT-SEC
           END-IF
      *
      *    妊婦のコメント
           IF     RECE41-NINPUFLG   =   1
               PERFORM     2001-NINPU-COMMENT-SEC
           END-IF
      *
      *+++++++++++++++++++++++++++++++++++++++++++++++++
      *    外来管理加算（特)のコメント（レセ電用）
           IF     RECE41-GAITOKU-PRTKBN1    =  "1"
               PERFORM     2001-GAITOK-COMMENT-SEC
           END-IF
      *+++++++++++++++++++++++++++++++++++++++++++++++++
      *
      *    パラメータ０４ファイルより編集
           PERFORM 800-RECE41-SET44-SEC
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 990-RECE44-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE44           =   1                   )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD     )
      *
               INITIALIZE    WRK-JIDO-SETCOM-AREA
      *
               IF   RECE44-JIHOKBN  NOT =  SPACE
                INITIALIZE  WRK-JIDO-SETCOM-AREA
                PERFORM     20011-HOKATU-COMMENT-SEC
               ELSE
                IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                 EVALUATE      RECE44-SRYKBN                   
                   WHEN      "11"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYODOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "12"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SAIDOU-HANTEI-SEC
                      END-IF
      *               診区１２・１３・１４・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "13"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SHIDOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NIKOCHINTOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOUNI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYUEIYO-HANTEI-SEC
                      END-IF
      *               診区１３・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TAKYOKSN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GAIRIHA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINNAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GAIJIR-HANTEI-SEC
                      END-IF
      *               診区１３・１４共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESUR-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GANSIDO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUSEN2-HANTEI-SEC
                      END-IF
      *               診区１２・１３・１４・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SOUKAN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-MANJIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "14"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUSIJI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUSKSN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TEJHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HINKAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-OSIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KAISYOKU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-YOUATU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIKYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIRIHA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIYAK-HANTEI-SEC
                      END-IF
      *               診区１３・１４共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESUR-HANTEI-SEC
                      END-IF
      *               診区１４・８０と共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINRYO-HANTEI-SEC
                      END-IF
      *               診区１２・１３・１４・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIISO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GAIZAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KOUYAK-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KINHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        IF RECE44-TOYAKUSENCOMFLG   =   "1"
                           PERFORM 2001-INGAITOYAKU-CHECK-SEC
                        END-IF
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "21"
                   WHEN      "22"
                   WHEN      "23"
                      IF   RECE44-COMCDFLG   =   "3"
                        PERFORM    2001-YAKUTEI-HENSYU-SEC
                      ELSE
                        IF  (RECE44-SIPPUFLG        = "1"     )
                        AND (RECE41-SRYYM          <= "202409")
                           PERFORM 2001-INGAISIPPU-CHECK-SEC
                        END-IF
                        IF ((RECE44-SIPPUFLG        = "1"     ) AND
                            (RECE44-SIPPUFLG2       = "1"     ) AND
                            (RECE41-SRYYM          >= "202410"))
                        OR ( RECE44-TOYAKUSENCOMFLG = "1"      )
                           PERFORM 2001-INGAITOYAKU-CHECK-SEC
                        END-IF
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "33"
                      PERFORM      2001-HOUTEN-HANTEI-SEC
                   WHEN      "40"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINZOUETC-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KYOKUIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-BOKOHO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KIATU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "50"
                      PERFORM      2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "54"
                      PERFORM      2001-MASUI-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENKSN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "70"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-CTMRI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KINGAZ-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE44-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE44-RIHABIRI-COMMENT      =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                          ELSE
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHAIGAKU-COMMENT-SEC
                          END-IF
                        END-IF
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RIHASEN-HANTEI-SEC
                      END-IF
      *               診区１４・８０と共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINRYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KYUKYU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TUINZAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-IZONSYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SIPPUCOM-HANTEI-SEC
                      END-IF
      *               診区１２・１３・１４・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SEIKEA-HANTEI-SEC
                      END-IF
      *               診区１３・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SEIKIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "99"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOMONCOM-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN   OTHER
                      PERFORM      2099-RECE44-TENSOU-SEC
                 END-EVALUATE
                ELSE                                                     
                 EVALUATE      RECE44-SRYKBN                            
                   WHEN      "11"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYODOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "12"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SAIDOU-HANTEI-SEC
                      END-IF
      *               診区１２・１３・１４・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "13"                                       
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SHIDOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOBYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RYOYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NIKOCHINTOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYOUNI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SYUEIYO-HANTEI-SEC
                      END-IF
      *               診区１３・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TAKYOKSN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GAIRIHA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINNAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GAIJIR-HANTEI-SEC
                      END-IF
      *               診区１３・１４共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESUR-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GANSIDO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NYUSEN2-HANTEI-SEC
                      END-IF
      *               診区１２・１３・１４・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SOUKAN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-MANJIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "14"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUSIJI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOUSKSN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TEJHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HINKAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-OSIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KAISYOKU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-YOUATU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIKYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIRIHA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIYAK-HANTEI-SEC
                      END-IF
      *               診区１３・１４共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SINPESUR-HANTEI-SEC
                      END-IF
      *               診区１４・８０と共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINRYO-HANTEI-SEC
                      END-IF
      *               診区１２・１３・１４・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ZAIISO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-GAIZAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KOUYAK-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KINHOU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        IF RECE44-TOYAKUSENCOMFLG   =   "1"
                           PERFORM 2001-INGAITOYAKU-CHECK-SEC
                        END-IF
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "21"
                   WHEN      "22"
                   WHEN      "23"
                      IF   RECE44-COMCDFLG   =   "3"
                        PERFORM    2001-YAKUTEI-HENSYU-SEC
                      ELSE
                        IF  (RECE44-SIPPUFLG        = "1"     )
                        AND (RECE41-SRYYM          <= "202409")
                           PERFORM 2001-INGAISIPPU-CHECK-SEC
                        END-IF
                        IF ((RECE44-SIPPUFLG        = "1"     ) AND
                            (RECE44-SIPPUFLG2       = "1"     ) AND
                            (RECE41-SRYYM          >= "202410"))
                        OR ( RECE44-TOYAKUSENCOMFLG = "1"      )
                           PERFORM 2001-INGAITOYAKU-CHECK-SEC
                        END-IF
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "33"
                      PERFORM      2001-HOUTEN-HANTEI-SEC
                   WHEN      "50"                                       
                      PERFORM      2001-SHUJUTSU-HANTEI-SEC
                   WHEN      "54"
                      PERFORM      2001-MASUI-HANTEI-SEC
                   WHEN      "60"
                   WHEN      "64"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENSA-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KENKSN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "70"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-CTMRI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KINGAZ-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "40"          
                   WHEN      "80"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        IF    RECE44-RIHABIRI-COMMENT  NOT = SPACE
                          IF  RECE44-RIHABIRI-COMMENT      =  "1"
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHA-COMMENT-SEC
                          ELSE
                            MOVE      99   TO   FLG-COMMENT
                            PERFORM   2001-RIHAIGAKU-COMMENT-SEC
                          END-IF
                        END-IF
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-RIHASEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-JINZOUETC-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KYOKUIN-HANTEI-SEC
                      END-IF
      *               診区１４・８０と共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINRYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KYUKYU-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-BOKOHO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-TUINZAI-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-IZONSYO-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SIPPUCOM-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-KIATU-HANTEI-SEC
                      END-IF
      *               診区１２・１３・１４・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-ONLINE-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SEIKEA-HANTEI-SEC
                      END-IF
      *               診区１３・８０共用分
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-NINSEN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-SEIKIN-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN      "99"
                      MOVE   ZERO   TO  FLG-COMMENT
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2001-HOMONCOM-HANTEI-SEC
                      END-IF
                      IF   FLG-COMMENT  =  ZERO
                        PERFORM    2099-RECE44-TENSOU-SEC
                      END-IF
                   WHEN   OTHER                                         
                      PERFORM      2099-RECE44-TENSOU-SEC
                 END-EVALUATE                                           
                END-IF
               END-IF                                                   
      *
               PERFORM 990-RECE44-NEXT-SEC
      *
           END-PERFORM
      *
      *****内服薬剤料逓減に関する編集２****
           PERFORM     240-YAKUTEI-HENSYU2-SEC
      *
      *****差込編集２**********************
           PERFORM     250-SASHIKOMI2-SEC
      *
      *****選択式コメント編集**************
           IF   RECE41-SRYYM  >=  "201810"
             PERFORM   270-SENTAKUCOM-HENSYU-SEC
           END-IF
      *
      *****関係コメント編集****************
           PERFORM     260-SETCOM-HENSYU-SEC
      *
           PERFORM 990-RECE41-READ-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴テーブルから診療科毎の受診日を検索セット
      *****************************************************************
       2001-JUSHIN-KENSAKU-SEC             SECTION.
      *
      *    受診歴の検索
           INITIALIZE                      JYURRK-REC
           MOVE    RECE41-HOSPNUM      TO  JYURRK-HOSPNUM
           MOVE    RECE41-PTID         TO  JYURRK-PTID
           MOVE    RECE41-NYUGAIKBN    TO  JYURRK-NYUGAIKBN
           MOVE    TBL-JUSHIN-SRYKA(IDX)
                                       TO  JYURRK-SRYKA
           MOVE    RECE41-SRYYM        TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    RECE41-SRYYM        TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"    TO  MCP-TABLE
               MOVE    "key24"         TO  MCP-PATHNAME
               PERFORM   900-JYURRK-READ-SEC
               PERFORM   UNTIL   FLG-JYURRK   =    1
                  IF  ((RECE41-HKNKBN  =  "1" OR "2" OR "3" OR "5") AND
                       (JYURRK-HKNKBN  =  "1" OR "5"              ))
                  OR  ((RECE41-HKNKBN  =  "4"                     ) AND
                       (JYURRK-HKNKBN  =  "2" OR "4"              ))
                  OR  ((RECE41-HKNKBN  =  "6"                     ) AND
                       (JYURRK-HKNKBN  =  "7"                     ))
                      MOVE   JYURRK-HKNCOMBINUM
                                       TO  WRK-HKNCOMBINUMX
                      IF  WRK-HKNCOMBINUM9  =  RECE41-HKNCOMBI
                          IF  RECE41-HKNKBN   NOT =  "3"
                               MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                               IF   RECE41-JNISSUTBL(WRK-SRYDD) = SPACE
                                  CONTINUE
                               ELSE
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                               END-IF
                          ELSE
                            IF  JYURRK-SRYYMD  =  RECE41-TEKSTYMD
                               MOVE   JYURRK-SRYYMD   TO   WRK-SRYYMDX
                               IF   RECE41-JNISSUTBL(WRK-SRYDD) = SPACE
                                  CONTINUE
                               ELSE
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-ARI (IDX)
                                  MOVE   "1"     TO    
                                        TBL-JUSHIN-DAY (IDX WRK-SRYDD)
                               END-IF
                            END-IF
                          END-IF
                      END-IF      
                  END-IF
      *
                  MOVE    "tbl_jyurrk"    TO  MCP-TABLE
                  MOVE    "key24"         TO  MCP-PATHNAME
                  PERFORM   900-JYURRK-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-JUSHIN-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数診療科受診の科・日付の編集
      *****************************************************************
       2001-FREE-COMMENT-SEC          SECTION.
      *
           ADD      1                      TO  IDZ
           MOVE     ZERO                   TO  WRK-KETA
                                               WRK-KETA-TAIHI
                                               WRK-KETA2
      *
           IF  TBL-JUSHIN-SRYKANAME(IDX)  NOT =  SPACE
             INITIALIZE                      ORCSKANACHKAREA
             MOVE    "2"                     TO  KANACHK-SYORI 
             MOVE  TBL-JUSHIN-SRYKANAME(IDX) TO  KANACHK-MAE-INPUT
             CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
             IF      KANACHK-RC      =   ZERO
                 COMPUTE WRK-KETA          =   KANACHK-MAX
                 COMPUTE WRK-KETA-TAIHI    =   KANACHK-MAX
                 COMPUTE WRK-KETA2         =   KANACHK-MAX  /  2
                 MOVE    WRK-KETA2   TO  WK-FREE-YUKOKETA(IDZ)
                 MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                   TO  WK-FREE-NAME(IDZ)(1:KANACHK-MAX)
             END-IF
           END-IF
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDY    FROM   1   BY   1
                          UNTIL   (IDY             >      31)
      *
             IF  TBL-JUSHIN-DAY(IDX IDY) =  "1"
                 IF  FLG-FIRST  =  ZERO
                    IF  IDY     <  10 
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "　"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "日"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                    ELSE
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "　"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)     
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "日"
                           TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  IDY     <  10 
                       IF    WK-FREE-YUKOKETA(IDZ)  =  25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA(IDZ)  =  24 OR 25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
                       IF    WK-FREE-YUKOKETA(IDZ)  =  25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA(IDZ)  =  23 OR 24 OR 25
                          PERFORM  20011-FREE-COMMENT-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
             END-IF
      *
           END-PERFORM
           .
       2001-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数診療科受診の科・日付の編集（改行）
      *****************************************************************
       20011-FREE-COMMENT-KAIGYO-SEC          SECTION.
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA(IDZ)
           MOVE    ALL "　"        TO  WK-FREE-NAME(IDZ)(1:WRK-KETA)
      *
           .
       20011-FREE-COMMENT-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数診療科受診のコメント編集
      *****************************************************************
       2001-JUSHIN-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           ADD     1            TO  IDX-RECE45
           MOVE    "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE    ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
           MOVE    7            TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
           MOVE    "複数診療科受診"
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
           MOVE    "1"          TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   49    )
                         OR      ( WK-FREE-NAME(IDX) =   SPACE )
                ADD    1        TO  IDX-RECE45
                MOVE   "810000001"
                                TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO     TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA(IDX)
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME(IDX)
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   "1"      TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM 
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                PERFORM 800-RECE41-SET459-SEC
      *         MOVE    ZERO                TO  RECE459-ZAINUM
                MOVE    99999999            TO  RECE459-ZAINUM
                MOVE    "99"                TO  RECE459-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                         UNTIL   (IDX       >    5)   OR
                                 (FLG-WRKRECE45   =   1)
                   ADD         1            TO  IDX-RECE45
                   IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                   OR (IDX-RECE45                       >  100 )
                       MOVE    1            TO  FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF  
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                   PERFORM 800-RECE459-WRITE-SEC
                END-IF
           END-PERFORM     
      *
      *
           PERFORM 800-RECE41-SET449-SEC
      *    MOVE    ZERO                TO  RECE449-ZAINUM
           MOVE    99999999            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           .
       2001-JUSHIN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電話再診・同日複数再診のコメント編集
      *****************************************************************
       2001-DENWA-DOUJITU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999991            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
      *
           IF    CNT-DENWA     >   ZERO
              ADD     1                TO  RECE449-MEISAISU
           END-IF
           IF    CNT-DOUJITU   >   ZERO
              ADD     1                TO  RECE449-MEISAISU
           END-IF
           IF    CNT-DOUDEN    >   ZERO
              ADD     1                TO  RECE449-MEISAISU
           END-IF
           IF    CNT-DENWA2    >   ZERO
              ADD     1                TO  RECE449-MEISAISU
           END-IF
           IF    CNT-DOUDEN2   >   ZERO
              ADD     1                TO  RECE449-MEISAISU
           END-IF
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      ***
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999991            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
           MOVE    ZERO                TO  IDX40
      *    電話再診のコメントの挿入
           IF    CNT-DENWA     >   ZERO
             ADD     1                 TO  IDX40
             MOVE    "810000001"       TO  WRK-RECE45XX-SRYCD(1 1)
             MOVE    ZERO              TO  WRK-RECE45XX-SRYSURYO(1 1)
             MOVE    CNT-DENWA         TO  DAY-IDX2
             PERFORM    20011-KAISU-HENSYU-SEC
             IF   RECE41-SRYYM  >=  "201904"
               MOVE  10                TO  WRK-RECE45XX-YUKOKETA(1 1)
               MOVE  "電話等再診（　　回）"
                                       TO  WRK-RECE45XX-NAME(1 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(1 1)(13:4)
             ELSE
               MOVE  9                 TO  WRK-RECE45XX-YUKOKETA(1 1)
               MOVE  "電話再診（　　回）"
                                       TO  WRK-RECE45XX-NAME(1 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(1 1)(11:4)
             END-IF
             MOVE    "1"               TO  WRK-RECE45XX-SIKFLG(1 1)
             MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                       TO  RECE459-SRYACT-TBL(IDX40)
           END-IF
      *    同日複数再診のコメントの挿入
           IF    CNT-DOUJITU   >   ZERO
             ADD     1                 TO  IDX40
             IF   RECE41-SRYYM      >=    "201004"
               MOVE  "810000001"       TO  WRK-RECE45XX-SRYCD(2 1)
               MOVE  ZERO              TO  WRK-RECE45XX-SRYSURYO(2 1)
               MOVE  9                 TO  WRK-RECE45XX-YUKOKETA(2 1)
               MOVE  "同日再診（　　回）"
                                       TO  WRK-RECE45XX-NAME(2 1)
      *
               MOVE  CNT-DOUJITU       TO  DAY-IDX2
               PERFORM    20011-KAISU-HENSYU-SEC
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(2 1)(11:4)
      *
               MOVE  "1"               TO  WRK-RECE45XX-SIKFLG(2 1)
               MOVE  WRK-RECE45XX-SRYACT-TBL(2 1)
                                       TO  RECE459-SRYACT-TBL(IDX40)
             ELSE
               MOVE  "810000001"       TO  WRK-RECE45XX-SRYCD(2 1)
               MOVE  ZERO              TO  WRK-RECE45XX-SRYSURYO(2 1)
               MOVE  11                TO  WRK-RECE45XX-YUKOKETA(2 1)
               MOVE  "同日複数再診（　　回）"
                                       TO  WRK-RECE45XX-NAME(2 1)
      *
               MOVE  CNT-DOUJITU       TO  DAY-IDX2
               PERFORM    20011-KAISU-HENSYU-SEC
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(2 1)(15:4)
      *
               MOVE  "1"               TO  WRK-RECE45XX-SIKFLG(2 1)
               MOVE  WRK-RECE45XX-SRYACT-TBL(2 1)
                                       TO  RECE459-SRYACT-TBL(IDX40)
             END-IF
           END-IF
      *    同日電話再診のコメントの挿入
           IF    CNT-DOUDEN    >   ZERO
             ADD     1                 TO  IDX40
             MOVE    "810000001"       TO  WRK-RECE45XX-SRYCD(3 1)
             MOVE    ZERO              TO  WRK-RECE45XX-SRYSURYO(3 1)
             MOVE    CNT-DOUDEN        TO  DAY-IDX2
             PERFORM    20011-KAISU-HENSYU-SEC
             IF   RECE41-SRYYM  >=  "201904"
               MOVE  12                TO  WRK-RECE45XX-YUKOKETA(3 1)
               MOVE  "同日電話等再診（　　回）"
                                       TO  WRK-RECE45XX-NAME(3 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(3 1)(17:4)
             ELSE
               MOVE  11                TO  WRK-RECE45XX-YUKOKETA(3 1)
               MOVE  "同日電話再診（　　回）"
                                       TO  WRK-RECE45XX-NAME(3 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(3 1)(15:4)
             END-IF
             MOVE    "1"               TO  WRK-RECE45XX-SIKFLG(3 1)
             MOVE    WRK-RECE45XX-SRYACT-TBL(3 1)
                                       TO  RECE459-SRYACT-TBL(IDX40)
           END-IF
      *    電話再診（３０年３月以前継続）のコメントの挿入
           IF    CNT-DENWA2    >   ZERO
             ADD     1                 TO  IDX40
             MOVE    "810000001"       TO  WRK-RECE45XX-SRYCD(4 1)
             MOVE    ZERO              TO  WRK-RECE45XX-SRYSURYO(4 1)
             MOVE    CNT-DENWA2        TO  DAY-IDX2
             PERFORM    20011-KAISU-HENSYU-SEC
             IF   RECE41-SRYYM  >=  "201904"
               MOVE  21                TO  WRK-RECE45XX-YUKOKETA(4 1)
               MOVE  "電話等再診（３０年３月以前継続）（　　回）"
                                       TO  WRK-RECE45XX-NAME(4 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(4 1)(35:4)
             ELSE
               MOVE  20                TO  WRK-RECE45XX-YUKOKETA(4 1)
               MOVE  "電話再診（３０年３月以前継続）（　　回）"
                                       TO  WRK-RECE45XX-NAME(4 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(4 1)(33:4)
             END-IF
             MOVE    "1"               TO  WRK-RECE45XX-SIKFLG(4 1)
             MOVE    WRK-RECE45XX-SRYACT-TBL(4 1)
                                       TO  RECE459-SRYACT-TBL(IDX40)
           END-IF
      *    同日電話再診（３０年３月以前継続）のコメントの挿入
           IF    CNT-DOUDEN2   >   ZERO
             ADD     1                 TO  IDX40
             MOVE    "810000001"       TO  WRK-RECE45XX-SRYCD(5 1)
             MOVE    ZERO              TO  WRK-RECE45XX-SRYSURYO(5 1)
             MOVE    CNT-DOUDEN2       TO  DAY-IDX2
             PERFORM    20011-KAISU-HENSYU-SEC
             IF   RECE41-SRYYM  >=  "201904"
               MOVE  23                TO  WRK-RECE45XX-YUKOKETA(5 1)
               MOVE  "同日電話等再診（３０年３月以前継続）（　　回）"
                                       TO  WRK-RECE45XX-NAME(5 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(5 1)(39:4)
             ELSE
               MOVE  22                TO  WRK-RECE45XX-YUKOKETA(5 1)
               MOVE  "同日電話再診（３０年３月以前継続）（　　回）"
                                       TO  WRK-RECE45XX-NAME(5 1)
               MOVE  WRK-HENSYU-KAISU2 TO  WRK-RECE45XX-NAME(5 1)(37:4)
             END-IF
             MOVE    "1"               TO  WRK-RECE45XX-SIKFLG(5 1)
             MOVE    WRK-RECE45XX-SRYACT-TBL(5 1)
                                       TO  RECE459-SRYACT-TBL(IDX40)
           END-IF
      *
           PERFORM 800-RECE459-WRITE-SEC
      *
           .
       2001-DENWA-DOUJITU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集
      *****************************************************************
       20011-KAISU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KAISU
           MOVE    SPACE                TO  WRK-HENSYU-KAISU2
      *
           MOVE    DAY-IDX2             TO  WRK-HENSYU-KAISU
           IF      WRK-HENSYU-KAISU(1:1) =  "0"
               MOVE      SPACE          TO  WRK-HENSYU-KAISU(1:1)
           END-IF
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KAISU     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WRK-HENSYU-KAISU2
           END-IF
      *
           .
       20011-KAISU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集２
      *****************************************************************
       20011-KAISU-HENSYU2-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KAISU3
           MOVE    SPACE                TO  WRK-HENSYU-KAISU4
      *
           MOVE    DAY-IDX4             TO  WRK-HENSYU-KAISU3
           IF      WRK-HENSYU-KAISU3(1:1) =  "0"
               MOVE      SPACE          TO  WRK-HENSYU-KAISU3(1:1)
           END-IF
           IF      WRK-HENSYU-KAISU3(2:1) =  "0"
               MOVE      SPACE          TO  WRK-HENSYU-KAISU3(2:1)
           END-IF
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KAISU3    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:6)
                                        TO  WRK-HENSYU-KAISU4
           END-IF
      *
           .
       20011-KAISU-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    妊婦のコメント編集
      *****************************************************************
       2001-NINPU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999960            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           MOVE    1                   TO  RECE449-MEISAISU
           PERFORM 800-RECE449-WRITE-SEC
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999960            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *    「妊婦」
           MOVE    "820100348"         TO  WRK-TENSU-SRYCD
           MOVE    RECE41-SRYYM        TO  WRK-TENSU-SRYYM
           PERFORM 900-TENSU-TBL-SEC
      *
           MOVE    TNS-SRYCD           TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    TNS-YUKOKETA        TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    TNS-NAME            TO  WRK-RECE45XX-NAME(1 1)
                                           WRK-RECE45XX-INPUTCOMENT(1 1)
           MOVE    "1"                 TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                       TO  RECE459-SRYACT-TBL(1)
           PERFORM 800-RECE459-WRITE-SEC
      *
           .
       2001-NINPU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日があるか判定
      *****************************************************************
       2001-HASSYO-HANTEI-SEC          SECTION.
      *
           IF   TBL-HASSYO-KAITATU-COMMENT   =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-KYUSEI      TO   WRK-SRYCD
                PERFORM   2001-KYUSEIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SINDAI-COMMENT    =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-SINDAI      TO   WRK-SRYCD
                MOVE      CONS-SINDAI-E    TO   WRK-SRYCD2
                MOVE      TBL-HASSYO-SINDAI-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-SINDAI-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-KOKYUKI-COMMENT   =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-KOKYUKI     TO   WRK-SRYCD
                MOVE      CONS-KOKYUKI-E   TO   WRK-SRYCD2
                MOVE      TBL-HASSYO-KOKYUKI-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-KOKYUKI-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-NOUKE-COMMENT     =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-NOUKE       TO   WRK-SRYCD
                MOVE      CONS-NOUKE-E     TO   WRK-SRYCD2
                MOVE      TBL-HASSYO-NOUKE-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-NOUKE-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-UNDOKI-COMMENT    =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-UNDOKI      TO   WRK-SRYCD
                MOVE      CONS-UNDOKI-E    TO   WRK-SRYCD2
                MOVE      TBL-HASSYO-UNDOKI-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-UNDOKI-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-NANBYO-COMMENT    =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-NANBYO      TO   WRK-SRYCD
                MOVE      CONS-NANBYO-E    TO   WRK-SRYCD2
                MOVE      TBL-HASSYO-NANBYO-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-NANBYO-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SYOGAI-COMMENT    =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-SYOGAI      TO   WRK-SRYCD
                MOVE      CONS-SYOGAI-E    TO   WRK-SRYCD2
                MOVE      TBL-HASSYO-SYOGAI-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-SYOGAI-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-SESSYOKU-COMMENT  =  1
                MOVE      ZERO             TO   FLG-COMOKIKAE
                MOVE      CONS-SESSYOKU    TO   WRK-SRYCD
                MOVE      CONS-SESSYOKU-E  TO   WRK-SRYCD2
                MOVE      TBL-HASSYO-SESSYOKU-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-SESSYOKU-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO-HAIYO-COMMENT    =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-HAIYO       TO   WRK-SRYCD
                MOVE      CONS-HAIYO-E     TO   WRK-SRYCD2
                MOVE      TBL-HASSYO-HAIYO-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-HAIYO-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      *####
           IF   TBL-HASSYO9-SINDAI-COMMENT    =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-SINDAI-K    TO   WRK-SRYCD
                MOVE      "999999999"      TO   WRK-SRYCD2
                MOVE      TBL-HASSYO9-SINDAI-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-SINDAI9-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO9-KOKYUKI-COMMENT   =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-KOKYUKI-K   TO   WRK-SRYCD
                MOVE      "999999999"      TO   WRK-SRYCD2
                MOVE      TBL-HASSYO9-KOKYUKI-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-KOKYUKI9-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      ***
           IF   TBL-HASSYO9-HAIYO-COMMENT    =  1
                MOVE      1                TO   FLG-COMOKIKAE
                MOVE      CONS-HAIYO-K     TO   WRK-SRYCD
                MOVE      "999999999"      TO   WRK-SRYCD2
                MOVE      TBL-HASSYO9-HAIYO-DAY-T
                                           TO   TBL-HASSYO-TAIHI-DAY-T
                MOVE      RECE41-RIHA2-HAIYO9-AREA
                                       TO   TBL-HASSYO-TAIHIYMD1-DAY-T
                PERFORM   20011-RIHABIRIHASSYO-HANTEI-SEC
           END-IF
      *
           .
       2001-HASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日があるか判定
      *    脳血管疾患急性発症日の１８０日以内か判定
      *****************************************************************
       2001-KYUSEIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     WRK-KYUSEIYMD
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"   TO  MCP-TABLE
                MOVE    "key3"         TO  MCP-PATHNAME
                PERFORM  900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                 ( FLG-KYUSEI-COMMENT =   1 )
      *
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3               <    1 )
                               OR    ( FLG-KYUSEI-COMMENT =    1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                         MOVE   SANTEI-SRYYM   TO  WRK-FSANTEIYMD(1:6)
                         MOVE   IDX3           TO  WRK-FSANTEIYMD(7:2)
      *
                         INITIALIZE                  STS-AREA-DAY
                         INITIALIZE                  LNK-DAY6-AREA
                         MOVE    "61"            TO  LNK-DAY6-IRAI
                         MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                         MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                         MOVE     179            TO  LNK-DAY6-ZOGEN
                         CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                         LNK-DAY6-AREA
                         MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER180
      *
                         IF   WRK-FSANTEIYMD(1:6) <=  RECE41-SRYYM
                         AND  RECE41-SRYYM        <=  WRK-AFTER180(1:6)
                               MOVE    1    TO   FLG-KYUSEI-COMMENT
                               MOVE  WRK-FSANTEIYMD
                                            TO   WRK-KYUSEIYMD
                         END-IF
      *
                     END-IF
      *
                   END-PERFORM
      *
                   MOVE    "tbl_santei"   TO  MCP-TABLE
                   MOVE    "key3"         TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-KYUSEI-COMMENT    =      1
               PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
               IF   RECE41-SRYYM      >=    "200604"
                   MOVE       WRK-KYUSEIYMD(7:2)  TO  IDX16
                   IF   RECE41-SRYYM >= "201905"
                     PERFORM  20011-HASSYO-COM-201905-SEC
                   ELSE
                     PERFORM  20011-HASSYO-COMMENT-SEC
                   END-IF
               ELSE
                   PERFORM    20011-KYUSEIHASSYO-COMMENT-SEC
               END-IF
           END-IF
      *
           .
       2001-KYUSEIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日等の検索
      *****************************************************************
       20011-KYUSEIHASSYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-KYUSEI-OK
           MOVE    SPACE                 TO  WRK-KYUSEI-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-KYUSEI-SRYKA
           MOVE    ZERO                  TO  WRK-KYUSEI-ZAINUM
           MOVE    ZERO                  TO  FLG-COMOKIKAE2
           MOVE    ZERO                  TO  FLG-COMARI
           MOVE    ZERO                  TO  FLG-COMARI2
      *
           MOVE    ZERO                  TO  IDZ
           INITIALIZE                        WRK-FREE-AREA
           INITIALIZE                        WRK-FREE-AREA2
      *++++
           PERFORM   VARYING   IDX15    FROM   2   BY  -1
                      UNTIL   (IDX15          <    1)
                       OR     (FLG-KYUSEI-OK  =    1)
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE41-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    IDX15                 TO  SRY-NYUGAIKBN
           MOVE    RECE41-PTID           TO  SRY-PTID
           MOVE    WRK-KYUSEIYMD(1:6)    TO  SRY-SRYYM
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (1)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (2)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (3)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (4)
           MOVE    WRK-SRYCD             TO  SRY-SRYCD (5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key7"                TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_sryact"      TO  MCP-TABLE
               MOVE    "key7"            TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-KYUSEI-OK  =   1
                 INITIALIZE                     SRYACCT-REC
                 MOVE    SRY-HOSPNUM      TO    ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN    TO    ACCT-NYUGAIKBN
                 MOVE    SRY-PTID         TO    ACCT-PTID
                 MOVE    SRY-SRYKA        TO    ACCT-SRYKA
                 MOVE    SRY-SRYYM        TO    ACCT-SRYYM
                 MOVE    SRY-ZAINUM       TO    ACCT-ZAINUM
                 MOVE    SRYACCT-REC      TO    MCPDATA-REC
                 MOVE    "tbl_sryacct"    TO    MCP-TABLE
                 MOVE    "key31"          TO    MCP-PATHNAME
                 PERFORM  900-DBSELECT-SEC
                 IF     MCP-RC            =  ZERO
                    MOVE    "tbl_sryacct" TO    MCP-TABLE
                    MOVE    "key31"       TO    MCP-PATHNAME
                    PERFORM  900-DBFETCH-SEC
                    IF      MCP-RC        =       ZERO
                        MOVE    MCPDATA-REC  TO  SRYACCT-REC
      *
                        MOVE    WRK-KYUSEIYMD(7:2)  TO  DAY-IDX2
                        IF  ( ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO )
                        AND ( ACCT-HKNCOMBI  =  RECE41-HKNCOMBI )
                             MOVE   1   TO  FLG-KYUSEI-OK
                             MOVE   ACCT-NYUGAIKBN
                                        TO  WRK-KYUSEI-NYUGAIKBN
                             MOVE   ACCT-SRYKA
                                        TO  WRK-KYUSEI-SRYKA
                             MOVE   ACCT-ZAINUM
                                        TO  WRK-KYUSEI-ZAINUM
                        END-IF
                    END-IF
                 END-IF
                 MOVE    "tbl_sryacct"    TO   MCP-TABLE
                 MOVE    "key31"          TO   MCP-PATHNAME
                 PERFORM  900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"     TO   MCP-TABLE
                 MOVE    "key7"           TO   MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           END-PERFORM
      *++++
           IF      WRK-KYUSEI-ZAINUM    NOT =    ZERO
              INITIALIZE                           SRYACT-REC
              MOVE    RECE41-HOSPNUM         TO    SRY-HOSPNUM
              MOVE    WRK-KYUSEI-NYUGAIKBN   TO    SRY-NYUGAIKBN
              MOVE    RECE41-PTID            TO    SRY-PTID
              MOVE    WRK-KYUSEI-SRYKA       TO    SRY-SRYKA
              MOVE    WRK-KYUSEIYMD(1:6)     TO    SRY-SRYYM
              MOVE    WRK-KYUSEI-ZAINUM      TO    SRY-ZAINUM
              MOVE    SRYACT-REC             TO    MCPDATA-REC
              MOVE    "tbl_sryact"           TO    MCP-TABLE
              MOVE    "key2"                 TO    MCP-PATHNAME
              PERFORM  900-DBSELECT-SEC
              IF     MCP-RC            =  ZERO
                 MOVE    "tbl_sryact"        TO    MCP-TABLE
                 MOVE    "key2"              TO    MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL   FLG-SRYACT     =   1
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )  OR
                                    (SRY-SRYCD(IDX12)  =   SPACE)
                    IF   RECE41-SRYYM >= "202008"
                      PERFORM  200111-KYUSEIHASSYO-KSCOM1-SEC
                    ELSE
                      IF  ((SRY-SRYCD(IDX12)(1:1)     =   "8"  )  OR
                           (SRY-SRYCD(IDX12)(1:3)     =   "008"))
                      AND ( SRY-SRYCD(IDX12)(1:4) NOT =
                                              "0085" AND "0086" )
                       IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
                         INITIALIZE                      PTCOM-REC
                         MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
                         MOVE    SRY-PTID            TO  PTCOM-PTID
                         MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
                         MOVE    SRY-SRYCD(IDX12)    TO  PTCOM-SRYCD
                         MOVE    SRY-INPUTNUM(IDX12) TO  PTCOM-RENNUM
                         PERFORM 900-PTCOM-READ-SEC
                         ADD     1     TO    IDZ
                         MOVE    PTCOM-INPUTCOMENT
                                                TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       ELSE
                         MOVE    SRY-SRYCD(IDX12) TO  WRK-TENSU-SRYCD
                         MOVE    SRY-SRYYM        TO  WRK-TENSU-SRYYM
                         PERFORM      900-TENSU-TBL-SEC
                         ADD     1           TO  IDZ
                         MOVE    TNS-NAME    TO  WK-FREE-NAME(IDZ)
                         PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                    UNTIL    IDX13    <    1
                           IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                             COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                      IDX13    /    2
                             MOVE     1     TO      IDX13
                           END-IF
                         END-PERFORM
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          EVALUATE  SRY-SRYCD(IDX12)
                            WHEN     "840000042"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    1    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000101"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    2    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                            WHEN     "840000058"
                              IF   FLG-COMOKIKAE2 = ZERO
                                 MOVE    3    TO  FLG-COMOKIKAE2
                                 MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                                 MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                                 COMPUTE  IDZ  =  IDZ - 1
                              END-IF
                          END-EVALUATE
                       END-IF
                       IF   FLG-COMOKIKAE  =   1
                          IF  SRY-SRYCD(IDX12) = "840000042" OR
                                                 "840000101" OR
                                                 "840000058"
                            MOVE    1    TO   FLG-COMARI2
                          ELSE
                            MOVE    1    TO   FLG-COMARI
                          END-IF
                       ELSE
                            MOVE    1    TO   FLG-COMARI
                       END-IF
                      END-IF
                    END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"       TO  MCP-TABLE
                   MOVE    "key2"             TO  MCP-PATHNAME
                   PERFORM  900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_sryact"            TO  MCP-TABLE
              MOVE    "key2"                  TO  MCP-PATHNAME
              PERFORM  900-CLOSE-SEC
           END-IF
      *++++
           IF      FLG-COMARI    =    ZERO
              INITIALIZE                      SANTEIPLUS-REC
              MOVE    RECE41-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
              MOVE    RECE41-PTID         TO  SANTEIPLUS-PTID
              MOVE    WRK-KYUSEIYMD(1:6)  TO  SANTEIPLUS-SRYYM
              MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
              MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
              MOVE    ZERO                TO  SANTEIPLUS-SRYKA
              MOVE    RECE41-HKNCOMBI     TO  SANTEIPLUS-HKNCOMBINUM
              MOVE    WRK-KYUSEIYMD(7:2)  TO  SANTEIPLUS-DAYKEY
              MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
              MOVE    "tbl_santeiplus"    TO  MCP-TABLE
              MOVE    "key2"              TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC               =       ZERO
                 MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                 MOVE    "key2"               TO  MCP-PATHNAME
                 PERFORM  900-SANTEIPLUS-READ-SEC
                 PERFORM  UNTIL   FLG-SANTEIPLUS     =   1
      *           リハコメントの終了日と診療月をチェック
      *           （履歴番号＞０のデータ）
                  IF     SANTEIPLUS-RENNUM   NOT =  ZERO
                   IF  ( SANTEIPLUS-ENDYMD       =  SPACE        )
                   OR  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                        (SANTEIPLUS-ENDYMD(1:6) >=  RECE41-SRYYM))
                    IF   RECE41-SRYYM >= "202008"
                      PERFORM  200111-KYUSEIHASSYO-KSCOM2-SEC
                    ELSE
                     IF  SANTEIPLUS-COMMENT  NOT =  SPACE
                      ADD     1                 TO  IDZ
                      MOVE  SANTEIPLUS-COMMENT  TO  WK-FREE-NAME(IDZ)
                      PERFORM   VARYING   IDX13   FROM   300   BY  -1
                                 UNTIL    IDX13    <     1
                        IF  WK-FREE-NAME(IDZ)(IDX13:1)  NOT =  SPACE
                          COMPUTE  WK-FREE-YUKOKETA(IDZ)  =
                                   IDX13    /    2
                          MOVE     1     TO      IDX13
                        END-IF
                      END-PERFORM
                     END-IF
                     IF   FLG-COMOKIKAE  =    1
                     AND  FLG-COMARI2    =   ZERO
                      IF   SANTEIPLUS-COMMENT(1:8)  = "手術　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    1    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:12) = "急性増悪　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    2    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                      IF   SANTEIPLUS-COMMENT(1:8) = "診断　日"
                        IF   FLG-COMOKIKAE2 = ZERO
                           MOVE    3    TO  FLG-COMOKIKAE2
                           MOVE  SPACE  TO  WK-FREE-NAME(IDZ)
                           MOVE  ZERO   TO  WK-FREE-YUKOKETA(IDZ)
                           COMPUTE  IDZ  =  IDZ - 1
                        END-IF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                  END-IF
                  MOVE    "tbl_santeiplus"     TO  MCP-TABLE
                  MOVE    "key2"               TO  MCP-PATHNAME
                  PERFORM  900-SANTEIPLUS-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_santeiplus"     TO  MCP-TABLE
              MOVE    "key2"               TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-KYUSEIHASSYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント検索
      *****************************************************************
       200111-KYUSEIHASSYO-KSCOM1-SEC          SECTION.
      *
           IF  (SRY-SRYCD(IDX12) = "810000001" OR "840000042"
                                OR "840000101" OR "840000058")
           OR  (SRY-SRYCD(IDX12)(1:4) = "0082" OR "0083"     )
           OR  (SRY-SRYCD(IDX12)(1:4) = "8301"               )
      *
             MOVE    SRY-SRYCD(IDX12)  TO  WRK-TENSU-SRYCD
             MOVE    SRY-SRYYM         TO  WRK-TENSU-SRYYM
             PERFORM      900-TENSU-TBL-SEC
      *
             IF   SRY-INPUTNUM(IDX12)   NOT =   ZERO
               INITIALIZE                      PTCOM-REC
               MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
               MOVE    SRY-PTID            TO  PTCOM-PTID
               MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
               MOVE    SRY-SRYCD(IDX12)    TO  PTCOM-SRYCD
               MOVE    SRY-INPUTNUM(IDX12) TO  PTCOM-RENNUM
               PERFORM 900-PTCOM-READ-SEC
               ADD     1     TO    IDZ
               MOVE    "999999999"         TO  WK-FREE-SRYCD2(IDZ)
               MOVE    PTCOM-INPUTCOMENT   TO  WK-FREE-NAME2(IDZ)
                                               WK-FREE-INPUTCOMENT2(IDZ)
      *
               IF  (SRY-SRYCD(IDX12)(1:4) = "8301")
      *********OR  (SRY-SRYCD(IDX12)(1:4) = "0083")
                 INITIALIZE                ORCSKANACHKAREA
                 MOVE  "2"             TO  KANACHK-SYORI 
                 MOVE  TNS-NAME        TO  KANACHK-MAE-INPUT
                 CALL  "ORCSKANACHK"   USING  ORCSKANACHKAREA
                 IF    ( KANACHK-RC      =   ZERO )
                 AND   ( KANACHK-MAX     >   ZERO )
                   COMPUTE IDX51 = KANACHK-MAX + 1
                   MOVE  PTCOM-INPUTCOMENT(IDX51:)
                                       TO  WK-FREE-INPUTCOMENT2(IDZ)
                 ELSE
                   MOVE  PTCOM-INPUTCOMENT
                                       TO  WK-FREE-INPUTCOMENT2(IDZ)
                 END-IF
      ***********IF   SRY-SRYCD(IDX12)(1:4) = "8301"
      ***********  MOVE  SRY-SRYCD(IDX12)  TO  WK-FREE-SRYCD2(IDZ)
      ***********END-IF
               END-IF
      *
               IF   SRY-SRYCD(IDX12)(1:4) = "8301"
                 IF      PTCOM-INPUTSTRING  =  SPACE
                   IF  TNS-FORMALNAME  =  SPACE
                     CONTINUE
                   ELSE
                     COMPUTE IDX52 = TNS-YUKOKETA * 2 + 1
                     INITIALIZE              ORCSKANACHKAREA
                     MOVE    "2"         TO  KANACHK-SYORI
                     STRING  TNS-FORMALNAME
                                     DELIMITED  BY  SPACE
                             PTCOM-INPUTCOMENT(IDX52:)
                                     DELIMITED  BY  SPACE
                                     INTO    KANACHK-MAE-INPUT
                     END-STRING
                     CALL    "ORCSKANACHK"   USING  ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                       MOVE  KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                         TO  WK-FREE-NAME2(IDZ)
                     END-IF
                   END-IF
                 ELSE
                   IF  TNS-FORMALNAME  =  SPACE
                     INITIALIZE              ORCSKANACHKAREA
                     MOVE    "2"         TO  KANACHK-SYORI
                     STRING  TNS-NAME
                                     DELIMITED  BY  SPACE
                             PTCOM-INPUTSTRING
                                     DELIMITED  BY  SPACE
                                     INTO    KANACHK-MAE-INPUT
                     END-STRING
                     CALL    "ORCSKANACHK"   USING  ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                       MOVE  KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                         TO  WK-FREE-NAME2(IDZ)
                     END-IF
                   ELSE
                     INITIALIZE              ORCSKANACHKAREA
                     MOVE    "2"         TO  KANACHK-SYORI
                     STRING  TNS-FORMALNAME
                                     DELIMITED  BY  SPACE
                             PTCOM-INPUTSTRING
                                     DELIMITED  BY  SPACE
                                     INTO    KANACHK-MAE-INPUT
                     END-STRING
                     CALL    "ORCSKANACHK"   USING  ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                       MOVE  KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                         TO  WK-FREE-NAME2(IDZ)
                     END-IF
                   END-IF
                   MOVE    PTCOM-INPUTSTRING
                                         TO  WK-FREE-INPUTCOMENT2(IDZ)
                 END-IF
               END-IF
             ELSE
               ADD     1           TO  IDZ
               MOVE    "999999999" TO  WK-FREE-SRYCD2(IDZ)
               MOVE    TNS-NAME    TO  WK-FREE-NAME2(IDZ)
                                       WK-FREE-INPUTCOMENT2(IDZ)
             END-IF
             PERFORM   VARYING   IDX13   FROM   300   BY  -1
                        UNTIL    IDX13    <    1
               IF  WK-FREE-NAME2(IDZ)(IDX13:1)  NOT =  SPACE
                 COMPUTE  WK-FREE-YUKOKETA2(IDZ) = IDX13  /  2
                 MOVE     1      TO  IDX13
               END-IF
             END-PERFORM
      *
             IF   SRY-SRYCD(IDX12) = "840000042" OR "840000101"
                                                 OR "840000058"
               MOVE  SPACE  TO  WK-FREE-SRYCD2(IDZ)
               MOVE  SPACE  TO  WK-FREE-NAME2(IDZ)
               MOVE  SPACE  TO  WK-FREE-INPUTCOMENT2(IDZ)
               MOVE  ZERO   TO  WK-FREE-YUKOKETA2(IDZ)
               COMPUTE  IDZ  =  IDZ - 1
               EVALUATE  SRY-SRYCD(IDX12)
                 WHEN     "840000042"
                   IF   FLG-COMOKIKAE  =   1
                   AND  FLG-COMOKIKAE2 =  ZERO
                     MOVE   1    TO   FLG-COMOKIKAE2
                   END-IF
                 WHEN     "840000101"
                   IF   FLG-COMOKIKAE  =   1
                   AND  FLG-COMOKIKAE2 =  ZERO
                     MOVE   2    TO   FLG-COMOKIKAE2
                   END-IF
                 WHEN     "840000058"
                   IF   FLG-COMOKIKAE  =   1
                   AND  FLG-COMOKIKAE2 =  ZERO
                     MOVE   3    TO   FLG-COMOKIKAE2
                   END-IF
               END-EVALUATE
             END-IF
             IF   FLG-COMOKIKAE  =   1
               IF   SRY-SRYCD(IDX12) = "840000042" OR "840000101"
                                                   OR "840000058"
                     MOVE   1    TO   FLG-COMARI2
               ELSE
                     MOVE   1    TO   FLG-COMARI
               END-IF
             ELSE
               IF   SRY-SRYCD(IDX12) = "840000042" OR "840000101"
                                                   OR "840000058"
                     CONTINUE
               ELSE
                     MOVE   1    TO   FLG-COMARI
               END-IF
             END-IF
           END-IF
      *
           .
       200111-KYUSEIHASSYO-KSCOM1-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント検索
      *****************************************************************
       200111-KYUSEIHASSYO-KSCOM2-SEC          SECTION.
      *
           IF   SANTEIPLUS-COMMENT  NOT =  SPACE
             ADD     1           TO  IDZ
             MOVE    "999999999" TO  WK-FREE-SRYCD2(IDZ)
             MOVE    SANTEIPLUS-COMMENT
                                 TO  WK-FREE-NAME2(IDZ)
                                     WK-FREE-INPUTCOMENT2(IDZ)
             PERFORM   VARYING   IDX13   FROM   300   BY  -1
                        UNTIL    IDX13    <    1
               IF  WK-FREE-NAME2(IDZ)(IDX13:1)  NOT =  SPACE
                 COMPUTE  WK-FREE-YUKOKETA2(IDZ) = IDX13  /  2
                 MOVE     1      TO  IDX13
               END-IF
             END-PERFORM
           END-IF
      *
           IF   SANTEIPLUS-COMMENT(1:8)  = "手術　日"
             MOVE  SPACE  TO  WK-FREE-SRYCD2(IDZ)
             MOVE  SPACE  TO  WK-FREE-NAME2(IDZ)
             MOVE  SPACE  TO  WK-FREE-INPUTCOMENT2(IDZ)
             MOVE  ZERO   TO  WK-FREE-YUKOKETA2(IDZ)
             COMPUTE  IDZ  =  IDZ - 1
             IF   FLG-COMOKIKAE  =   1
             AND  FLG-COMARI2    =  ZERO
             AND  FLG-COMOKIKAE2 =  ZERO
               MOVE   1   TO   FLG-COMOKIKAE2
             END-IF
           END-IF
           IF   SANTEIPLUS-COMMENT(1:12) = "急性増悪　日"
             MOVE  SPACE  TO  WK-FREE-SRYCD2(IDZ)
             MOVE  SPACE  TO  WK-FREE-NAME2(IDZ)
             MOVE  SPACE  TO  WK-FREE-INPUTCOMENT2(IDZ)
             MOVE  ZERO   TO  WK-FREE-YUKOKETA2(IDZ)
             COMPUTE  IDZ  =  IDZ - 1
             IF   FLG-COMOKIKAE  =   1
             AND  FLG-COMARI2    =  ZERO
             AND  FLG-COMOKIKAE2 =  ZERO
               MOVE   2   TO   FLG-COMOKIKAE2
             END-IF
           END-IF
           IF   SANTEIPLUS-COMMENT(1:8) = "診断　日"
             MOVE  SPACE  TO  WK-FREE-SRYCD2(IDZ)
             MOVE  SPACE  TO  WK-FREE-NAME2(IDZ)
             MOVE  SPACE  TO  WK-FREE-INPUTCOMENT2(IDZ)
             MOVE  ZERO   TO  WK-FREE-YUKOKETA2(IDZ)
             COMPUTE  IDZ  =  IDZ - 1
             IF   FLG-COMOKIKAE  =   1
             AND  FLG-COMARI2    =  ZERO
             AND  FLG-COMOKIKAE2 =  ZERO
               MOVE   3   TO   FLG-COMOKIKAE2
             END-IF
           END-IF
      *
           .
       200111-KYUSEIHASSYO-KSCOM2-EXT.
           EXIT.
      *
      *****************************************************************
      *    脳血管疾患急性発症日のコメント編集
      *****************************************************************
       20011-KYUSEIHASSYO-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO         TO  IDX-RECE45
           MOVE    SPACE        TO  WRK-RECE45XX-REC
           INITIALIZE               WRK-RECE45XX-REC
      *
           ADD     1            TO  IDX-RECE45
           MOVE    "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE    ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
           MOVE    9            TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
           MOVE    "発症　　　月　　日"
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
           MOVE    WRK-KYUSEIYMD(5:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(7:4)
           MOVE    WRK-KYUSEIYMD(7:2)   TO  DAY-IDX2
           MOVE    TBL-DAY(DAY-IDX2)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(13:4)
           MOVE    "1"        TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           PERFORM   VARYING   IDX12   FROM    1   BY  1
                      UNTIL  ( IDX12               >   49    )
                        OR   ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1        TO  IDX-RECE45
                MOVE   "810000001"
                                TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO     TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME(IDX12)
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   "1"      TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                PERFORM 800-RECE41-SET459-SEC
                MOVE    99999996            TO  RECE459-ZAINUM
                MOVE    "99"                TO  RECE459-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE45   =    1)
                   ADD         1            TO  IDX-RECE45
                   IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                   OR (IDX-RECE45                       >  100 )
                       MOVE    1            TO  FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                    PERFORM 800-RECE459-WRITE-SEC
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999996            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           .
       20011-KYUSEIHASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集
      *****************************************************************
       20011-HASSYO-COMMENT-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO         TO  IDX-RECE45
           MOVE    SPACE        TO  WRK-RECE45XX-REC
           INITIALIZE               WRK-RECE45XX-REC
      *
           ADD     1            TO  IDX-RECE45
           MOVE    "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE    ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
      *
           MOVE    SPACE        TO  WRK-OKIKAEFLG
      *
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    19   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "脳血管疾患急性発症　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    21   TO  IDC1
               MOVE    27   TO  IDC2
               MOVE    33   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               COMPUTE WRK-ZAINUM    =    99980000
                                     +   (IDX16 + 31)
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    23   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "心大血管疾患リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980200 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980200 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980200
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE  "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE  "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE  "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980400 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980400 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980400
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    21  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "脳血管疾患等リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    21  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "脳血管疾患等リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    23  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE
                       "脳血管疾患等リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    21  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "脳血管疾患等リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980600 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980600 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980600
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   MOVE    20  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "廃用症候群リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE
                       "廃用症候群リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980800 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980800 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980800
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE  "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE  "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE  "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981000 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981000 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981000
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    18  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "運動器リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    18  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "運動器リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    20  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "運動器リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    18  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "運動器リハ：診断　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    19  TO  IDC1
                   MOVE    25  TO  IDC2
                   MOVE    31  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981200 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981200 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981200
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-KOKYUKI
               MOVE    20   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "呼吸器リハ：治療開始　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    23   TO  IDC1
               MOVE    29   TO  IDC2
               MOVE    35   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981400 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981400 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981400
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE  "早期・初期リハ加算：発症　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE  "早期・初期リハ加算：手術　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE  "早期・初期リハ加算：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981600 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981600 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981600
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-SESSYOKU
               MOVE    21   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "摂食機能療法：治療開始　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981800 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981800 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981800
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    19  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "難病患者リハ：発症　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    19  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "難病患者リハ：手術　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    21  TO  IDC1
                   MOVE    27  TO  IDC2
                   MOVE    33  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    21  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE    "難病患者リハ：急性増悪　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982000 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982000 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982000
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-SYOGAI
               MOVE    21   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE    "障害児（者）リハ：発症　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982200 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982200 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982200
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           MOVE    "1"        TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           PERFORM   VARYING   IDX12   FROM    1   BY  1
                      UNTIL  ( IDX12               >   49    )
                        OR   ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1        TO  IDX-RECE45
                MOVE   "810000001"
                                TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO     TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME(IDX12)
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   "1"      TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                PERFORM 800-RECE41-SET459-SEC
                MOVE    WRK-ZAINUM          TO  RECE459-ZAINUM
                MOVE    WRK-SRYKBN          TO  RECE459-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE45   =    1)
                   ADD         1            TO  IDX-RECE45
                   IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                   OR (IDX-RECE45                       >  100 )
                       MOVE    1            TO  FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                    PERFORM 800-RECE459-WRITE-SEC
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    WRK-ZAINUM          TO  RECE449-ZAINUM
           MOVE    WRK-SRYKBN          TO  RECE449-SRYKBN
           MOVE    WRK-OKIKAEFLG       TO  RECE449-OKIKAEFLG
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           .
       20011-HASSYO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集
      *****************************************************************
       20011-HASSYO-COM-201905-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO         TO  IDX-RECE45
           MOVE    SPACE        TO  WRK-RECE45XX-REC
           INITIALIZE               WRK-RECE45XX-REC
      *
           ADD     1            TO  IDX-RECE45
           MOVE    "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE    ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
      *
           MOVE    SPACE        TO  WRK-OKIKAEFLG
      *
           EVALUATE      WRK-SRYCD
             WHEN       CONS-KYUSEI
               MOVE    21   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "脳血管疾患急性発症　　　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    25   TO  IDC1
               MOVE    31   TO  IDC2
               MOVE    37   TO  IDC3
               MOVE    21   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               COMPUTE WRK-ZAINUM    =    99980000
                                     +   (IDX16 + 31)
               MOVE    "40" TO  WRK-SRYKBN
             WHEN       CONS-SINDAI
               MOVE    25   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "心大血管疾患リハ：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    33   TO  IDC1
               MOVE    39   TO  IDC2
               MOVE    45   TO  IDC3
               MOVE    29   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980200 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980200 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980200
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980400 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980400 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980400
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-NOUKE
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    23  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "脳血管疾患等リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    23  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "脳血管疾患等リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    25  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "脳血管疾患等リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    33  TO  IDC1
                   MOVE    39  TO  IDC2
                   MOVE    45  TO  IDC3
                   MOVE    29  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    23  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "脳血管疾患等リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980600 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980600 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980600
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-HAIYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       1
                 WHEN       3
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "廃用症候群リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   MOVE    23  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "廃用症候群リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980800 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980800 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980800
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-HAIYO-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
           MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981000 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981000 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981000
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-UNDOKI
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                   MOVE    20  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "運動器リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    20  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "運動器リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    22  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "運動器リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    27  TO  IDC1
                   MOVE    33  TO  IDC2
                   MOVE    39  TO  IDC3
                   MOVE    23  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       3
                   MOVE    20  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "運動器リハ：診断　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    23  TO  IDC1
                   MOVE    29  TO  IDC2
                   MOVE    35  TO  IDC3
                   MOVE    19  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981200 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981200 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981200
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-KOKYUKI
               MOVE    22   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "呼吸器リハ：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    27   TO  IDC1
               MOVE    33   TO  IDC2
               MOVE    39   TO  IDC3
               MOVE    23   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981400 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981400 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981400
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                 MOVE "早期・初期リハ加算：発症　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    24  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                 MOVE "早期・初期リハ加算：手術　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    31  TO  IDC1
                   MOVE    37  TO  IDC2
                   MOVE    43  TO  IDC3
                   MOVE    27  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    26  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
             MOVE "早期・初期リハ加算：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    35  TO  IDC1
                   MOVE    41  TO  IDC2
                   MOVE    47  TO  IDC3
                   MOVE    31  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981600 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981600 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981600
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-SESSYOKU
               MOVE    23   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "摂食機能療法：治療開始　　　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               MOVE    25   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981800 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981800 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981800
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-NANBYO
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   MOVE    21  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "難病患者リハ：発症　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   MOVE    21  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       1
                   MOVE    21  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "難病患者リハ：手術　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    25  TO  IDC1
                   MOVE    31  TO  IDC2
                   MOVE    37  TO  IDC3
                   MOVE    21  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
                 WHEN       2
                   MOVE    23  TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE "難病患者リハ：急性増悪　　　　　年　　月　　日"
                               TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE    29  TO  IDC1
                   MOVE    35  TO  IDC2
                   MOVE    41  TO  IDC3
                   MOVE    25  TO  IDC4
                   PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982000 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982000 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982000
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
             WHEN       CONS-SYOGAI
               MOVE    23   TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "障害児（者）リハ：発症　　　　　年　　月　　日"
                            TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE    29   TO  IDC1
               MOVE    35   TO  IDC2
               MOVE    41   TO  IDC3
               MOVE    25   TO  IDC4
               PERFORM 200111-HASSYO-COMMENT-DATE-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99982200 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99982200 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99982200
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
               IF   RECE41-HKNKBN    NOT  =  "4" AND "6"
                    MOVE    "80" TO  WRK-SRYKBN
               ELSE
                 EVALUATE  WRK-PARA-JIBAIPRTKBN
                  WHEN     "0"
                    MOVE    "40" TO  WRK-SRYKBN
                    MOVE    "1"  TO  WRK-OKIKAEFLG
                  WHEN     "1"
                    MOVE    "80" TO  WRK-SRYKBN
                 END-EVALUATE
               END-IF
           END-EVALUATE
      *
           MOVE    "1"        TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           PERFORM   VARYING   IDX12   FROM    1   BY  1
                      UNTIL  ( IDX12               >   49    )
                        OR   ( WK-FREE-NAME(IDX12) =   SPACE )
                ADD    1        TO  IDX-RECE45
                MOVE   "810000001"
                                TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO     TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA(IDX12)
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME(IDX12)
                                TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   "1"      TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-PERFORM
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                PERFORM 800-RECE41-SET459-SEC
                MOVE    WRK-ZAINUM          TO  RECE459-ZAINUM
                MOVE    WRK-SRYKBN          TO  RECE459-SRYKBN
                ADD     1                   TO  WRK-RENNUM
                MOVE    WRK-RENNUM          TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE45   =    1)
                   ADD         1            TO  IDX-RECE45
                   IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                   OR (IDX-RECE45                       >  100 )
                       MOVE    1            TO  FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL(IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                    PERFORM 800-RECE459-WRITE-SEC
                END-IF
           END-PERFORM
      *
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    WRK-ZAINUM          TO  RECE449-ZAINUM
           MOVE    WRK-SRYKBN          TO  RECE449-SRYKBN
           MOVE    WRK-OKIKAEFLG       TO  RECE449-OKIKAEFLG
      *
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           .
       20011-HASSYO-COM-201905-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集（年月日）
      *****************************************************************
       200111-HASSYO-COMMENT-DATE-SEC          SECTION.
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-KYUSEIYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                          LNK-DAY2-AREA
           IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(IDC1:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(IDC2:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                       TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(IDC3:4)
              END-IF
              IF   RECE41-SRYYM >= "201905"
                 MOVE    LNK-DAY2-EDTYMD2-G
                       TO  WRK-RECE45XX-NAME(IDX-RECE45 1)(IDC4:4)
              END-IF
           END-IF
      *
           .
       200111-HASSYO-COMMENT-DATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集
      *  ＃202008選択式コメント対応
      *****************************************************************
       20011-HASSYO-COM-202008-SEC          SECTION.
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO        TO  IDX-RECE45
           MOVE    SPACE       TO  WRK-RECE45XX-REC
           INITIALIZE              WRK-RECE45XX-REC
      *
           MOVE    SPACE       TO  WRK-OKIKAEFLG
      *
           MOVE    ZERO        TO  FLG-COM-HENSYU
           MOVE    ZERO        TO  FLG-COM-HENSYU2
           EVALUATE      WRK-SRYCD
             WHEN       CONS-SINDAI
               MOVE   68            TO  IDX-JIDO
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
               MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
               PERFORM  200111-RECECOMMENT-HENSYU2-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980200 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980200 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980200
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
             WHEN       CONS-SINDAI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF   RECE41-RSKSN-SYOKI-SIN  =  1
                     MOVE   74            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-SIN  =  1
                     MOVE   75            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                 WHEN       1
                   IF   RECE41-RSKSN-SYOKI-SIN  =  1
                     MOVE   574           TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-SIN  =  1
                     MOVE   575           TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                 WHEN       2
                   IF   RECE41-RSKSN-SYOKI-SIN  =  1
                     MOVE   1074          TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-SIN  =  1
                     MOVE   1075          TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980400 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980400 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980400
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
             WHEN       CONS-NOUKE
               IF  RECE41-SRYYM >= "202010"
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     IF  (RECE41-SRYYM   >= "202204")
                     AND (FLG-COMOKIKAE2  =  3      )
                       MOVE 2070      TO  IDX-JIDO
                     ELSE
                       MOVE   70      TO  IDX-JIDO
                     END-IF
                   WHEN       1
                     MOVE   1070      TO  IDX-JIDO
                   WHEN       2
                     MOVE   1570      TO  IDX-JIDO
                 END-EVALUATE
               ELSE
                     MOVE     70      TO  IDX-JIDO
               END-IF
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
               MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
               PERFORM  200111-RECECOMMENT-HENSYU2-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980600 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980600 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980600
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
             WHEN       CONS-HAIYO
               MOVE   73            TO  IDX-JIDO
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
               MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
               PERFORM  200111-RECECOMMENT-HENSYU2-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99980800 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99980800 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99980800
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
             WHEN       CONS-HAIYO-K
               MOVE     1   TO  FLG-COM-HENSYU
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF   RECE41-RSKSN-SYOKI-HAI  =  1
                     MOVE   74            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                     MOVE     1           TO  FLG-COM-HENSYU2
                     PERFORM  200111-HASSYO-COM-202008X-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-HAI  =  1
                     MOVE   75            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                     MOVE     2           TO  FLG-COM-HENSYU2
                     PERFORM  200111-HASSYO-COM-202008X-SEC
                   END-IF
                 WHEN       1
                   IF   RECE41-RSKSN-SYOKI-HAI  =  1
                     MOVE   574           TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                     MOVE     1           TO  FLG-COM-HENSYU2
                     PERFORM  200111-HASSYO-COM-202008X-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-HAI  =  1
                     MOVE   575           TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                     MOVE     2           TO  FLG-COM-HENSYU2
                     PERFORM  200111-HASSYO-COM-202008X-SEC
                   END-IF
                 WHEN       2
                   IF   RECE41-RSKSN-SYOKI-HAI  =  1
                     MOVE   1074          TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                     MOVE     1           TO  FLG-COM-HENSYU2
                     PERFORM  200111-HASSYO-COM-202008X-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-HAI  =  1
                     MOVE   1075          TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                     MOVE     2           TO  FLG-COM-HENSYU2
                     PERFORM  200111-HASSYO-COM-202008X-SEC
                   END-IF
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981000 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981000 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981000
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
             WHEN       CONS-UNDOKI
               IF  RECE41-SRYYM >= "202010"
                 EVALUATE   FLG-COMOKIKAE2
                   WHEN       0
                   WHEN       3
                     IF  (RECE41-SRYYM   >= "202204")
                     AND (FLG-COMOKIKAE2  =  3      )
                       MOVE 2071      TO  IDX-JIDO
                     ELSE
                       MOVE   71      TO  IDX-JIDO
                     END-IF
                   WHEN       1
                     MOVE   1071      TO  IDX-JIDO
                   WHEN       2
                     MOVE   1571      TO  IDX-JIDO
                 END-EVALUATE
               ELSE
                     MOVE     71      TO  IDX-JIDO
               END-IF
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
               MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
               PERFORM  200111-RECECOMMENT-HENSYU2-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981200 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981200 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981200
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
             WHEN       CONS-KOKYUKI
               MOVE   69            TO  IDX-JIDO
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
               MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
               PERFORM  200111-RECECOMMENT-HENSYU2-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981400 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981400 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981400
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
             WHEN       CONS-KOKYUKI-K
               EVALUATE   FLG-COMOKIKAE2
                 WHEN       0
                 WHEN       3
                   IF   RECE41-RSKSN-SYOKI-KOK  =  1
                     MOVE   74            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-KOK  =  1
                     MOVE   75            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                 WHEN       1
                   IF   RECE41-RSKSN-SYOKI-KOK  =  1
                     MOVE   574           TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-KOK  =  1
                     MOVE   575           TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                 WHEN       2
                   IF   RECE41-RSKSN-SYOKI-KOK  =  1
                     MOVE   1074          TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
                   IF   RECE41-RSKSN-SOUKI-KOK  =  1
                     MOVE   1075          TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU2-SEC
                   END-IF
               END-EVALUATE
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981600 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981600 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981600
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
             WHEN       CONS-SESSYOKU
               MOVE   72            TO  IDX-JIDO
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
               MOVE   WRK-KYUSEIYMD TO  WRK-RECECOM-YMD
               PERFORM  200111-RECECOMMENT-HENSYU2-SEC
               EVALUATE    FLG-RIHA
                 WHEN        ZERO
                   COMPUTE WRK-ZAINUM    =  99981800 + 1
                 WHEN        1
                   COMPUTE WRK-ZAINUM    =  99981800 + 10 + 1
                 WHEN        2
                   COMPUTE WRK-ZAINUM    =  99981800
                                         + (IDX16 + (1 * 31))
               END-EVALUATE
           END-EVALUATE
      *
      *    疾患名編集
           IF  FLG-COM-HENSYU = ZERO
             PERFORM 200111-HASSYO-COM-202008X-SEC
           END-IF
      *
           IF   RECE41-HKNKBN  NOT =  "4" AND "6"
                 MOVE    "80"  TO  WRK-SRYKBN
           ELSE
             EVALUATE  WRK-PARA-JIBAIPRTKBN
               WHEN     "0"
                 MOVE    "40"  TO  WRK-SRYKBN
                 MOVE    "1"   TO  WRK-OKIKAEFLG
               WHEN     "1"
               WHEN     "2"
                 MOVE    "80"  TO  WRK-SRYKBN
             END-EVALUATE
           END-IF
      *
      *    科・日付の編集を含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
                PERFORM 800-RECE41-SET459-SEC
                MOVE    WRK-ZAINUM           TO  RECE459-ZAINUM
                MOVE    WRK-SRYKBN           TO  RECE459-SRYKBN
                ADD     1                    TO  WRK-RENNUM
                MOVE    WRK-RENNUM           TO  RECE459-RENNUM
                PERFORM  VARYING   IDX   FROM   1   BY   1
                          UNTIL   (IDX             >    5)   OR
                                  (FLG-WRKRECE45   =    1)
                   ADD         1             TO    IDX-RECE45
                   IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                   OR (IDX-RECE45                       >  100 )
                       MOVE    1             TO    FLG-WRKRECE45
                   ELSE
                       MOVE  WRK-RECE45XX-SRYACT-TBL (IDX-RECE45 1)
                                        TO   RECE459-SRYACT-TBL (IDX)
                       ADD   1          TO   WRK-CNT-MEISAI
                   END-IF
                END-PERFORM
                IF   RECE459-SRYCD(1)  NOT =  SPACE
                    PERFORM 800-RECE459-WRITE-SEC
                END-IF
           END-PERFORM
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    WRK-ZAINUM          TO  RECE449-ZAINUM
           MOVE    WRK-SRYKBN          TO  RECE449-SRYKBN
           MOVE    WRK-OKIKAEFLG       TO  RECE449-OKIKAEFLG
           MOVE    WRK-CNT-MEISAI      TO  RECE449-MEISAISU
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           .
       20011-HASSYO-COM-202008-EXT.
           EXIT.
      *
      *****************************************************************
      *    発症日のコメント編集（疾患名編集）
      *  ＃202008選択式コメント対応
      *****************************************************************
       200111-HASSYO-COM-202008X-SEC          SECTION.
      *
           PERFORM  VARYING  IDX12  FROM  1  BY  1
                     UNTIL  (IDX12                >   50  )
                       OR   (WK-FREE-NAME2(IDX12) =  SPACE)
               IF  WK-FREE-SRYCD2(IDX12) = "999999999"
                 MOVE    ZERO      TO  IDX-JIDO
                 EVALUATE  WRK-SRYCD
                   WHEN     CONS-SINDAI
                      MOVE    568  TO  IDX-JIDO
                   WHEN     CONS-NOUKE
                      MOVE    570  TO  IDX-JIDO
                   WHEN     CONS-HAIYO
                      MOVE    573  TO  IDX-JIDO
                   WHEN     CONS-UNDOKI
                      MOVE    571  TO  IDX-JIDO
                   WHEN     CONS-KOKYUKI
                      MOVE    569  TO  IDX-JIDO
                   WHEN     CONS-SESSYOKU
                      MOVE    572  TO  IDX-JIDO
                   WHEN     CONS-HAIYO-K
                     IF  RECE41-SRYYM >= "202204"
                      IF   FLG-COM-HENSYU2  =  1
                       MOVE  1574  TO  IDX-JIDO
                      END-IF
                      IF   FLG-COM-HENSYU2  =  2
                       MOVE  1575  TO  IDX-JIDO
                      END-IF
                     END-IF
                 END-EVALUATE
                 PERFORM  200111-RECECOMMENT-IDXGET-SEC
                 MOVE   WK-FREE-INPUTCOMENT2(IDX12)
                                   TO  WRK-RECECOM-COMMENT
      *
                 INITIALIZE                  ORCSRECECOMMENTAREA
                 MOVE  WRK-RECECOM-AREA   TO ORCSRECECOMMENT-IN-AREA
                 CALL  "ORCSRECECOMMENT"  USING  ORCSRECECOMMENTAREA
                                                 SPA-AREA
                 IF    LNK-RECECOMMENT-RC  =  ZERO
                   ADD  1     TO  IDX-RECE45
                   MOVE LNK-RECECOMMENT-SRYCD
                              TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                   MOVE ZERO  TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                   MOVE LNK-RECECOMMENT-YUKOKETA
                              TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                   MOVE LNK-RECECOMMENT-NAME
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                   MOVE LNK-RECECOMMENT-INPUTCOMMENT
                              TO  WRK-RECE45XX-INPUTCOMENT(IDX-RECE45 1)
                   MOVE "1"   TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
                 END-IF
               ELSE
                 ADD    1     TO  IDX-RECE45
                 MOVE   WK-FREE-SRYCD2(IDX12)
                              TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                 MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                 MOVE   WK-FREE-YUKOKETA2(IDX12)
                              TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                 MOVE   WK-FREE-NAME2(IDX12)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                 MOVE   WK-FREE-INPUTCOMENT2(IDX12)
                              TO  WRK-RECE45XX-INPUTCOMENT(IDX-RECE45 1)
                 MOVE   "1"   TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
               END-IF
           END-PERFORM
      *
           .
       200111-HASSYO-COM-202008X-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ発症日があるか判定
      *****************************************************************
       20011-RIHABIRIHASSYO-HANTEI-SEC          SECTION.
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     SPACE    TO     WRK-RIHASANTEI-END
           INITIALIZE               TBL-RIHASANTEI-DAY-END
           INITIALIZE               TBL-RIHASANTEI-DAY-END2
           INITIALIZE               TBL-RIHASANTEI-DAY-END3
      *
      *    リハ終了日
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD2          TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"   TO  MCP-TABLE
                MOVE    "key3"         TO  MCP-PATHNAME
                PERFORM  900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                  IF   SANTEI-SRYYM  =  RECE41-SRYYM
                   PERFORM   VARYING   IDX3   FROM   1  BY   1
                              UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE    1   TO   TB-RIHASANTEI-DAY-END(IDX3)
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM   VARYING   IDX3   FROM  31  BY  -1
                              UNTIL  ( IDX3                <   1 )
                                OR   ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                        MOVE  SANTEI-SRYYM  TO  WRK-RIHASANTEI-END(1:6)
                        MOVE  IDX3          TO  WRK-RIHASANTEI-END(7:2)
      *
                        MOVE  1             TO  FLG-KYUSEI-COMMENT2
      *
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"   TO  MCP-TABLE
                 MOVE    "key3"         TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      ********
      *
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT
           MOVE     ZERO     TO     FLG-KYUSEI-COMMENT2
           MOVE     ZERO     TO     WRK-KYUSEIYMD
           INITIALIZE               TBL-RIHASANTEI-DAY
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD2
           MOVE     SPACE    TO     WRK-RIHASANTEI-YMD3
           MOVE     ZERO     TO     WRK-RIHASANTEI41-FLG
      *
      *    リハ開始日
      *
      *    当月の先頭のリハビリ算定日
           MOVE     ZERO           TO  RIHA-ST2
           PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                     UNTIL   RIHA-IDX    >   31
             IF     TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO
               MOVE     RIHA-IDX   TO  RIHA-ST2
               MOVE     31         TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *
                MOVE    "tbl_santei"   TO  MCP-TABLE
                MOVE    "key3"         TO  MCP-PATHNAME
                PERFORM  900-SANTEI-READ-SEC
      *
                PERFORM   UNTIL  ( FLG-SANTEI          =  1 ) OR
                                 ( FLG-KYUSEI-COMMENT2 =  1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                  IF   SANTEI-SRYYM  =  RECE41-SRYYM
                   PERFORM  VARYING   IDX3   FROM   1  BY  1
                             UNTIL    IDX3    >    31
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
      *
                         MOVE   1     TO  TB-RIHASANTEI-DAY(IDX3)
      *
                         MOVE   1     TO  FLG-KYUSEI-COMMENT
      *
                         IF    (SANTEI-SRYYM = "200604")
                         AND   (IDX3         =  1      )
                            MOVE    1    TO   WRK-RIHASANTEI41-FLG
                         END-IF
      **>                IF    (SANTEI-SRYYM  NOT = "200604")
      **>                AND   (IDX3              =  1      )
      **>                   MOVE   1     TO   FLG-KYUSEI-COMMENT2
      **>                END-IF
      *
                     END-IF
      *
                   END-PERFORM
                  ELSE
                   PERFORM  VARYING   IDX3   FROM  31  BY  -1
                             UNTIL   ( IDX3                <   1 )
                               OR    ( FLG-KYUSEI-COMMENT2 =   1 )
      *
                     IF   SANTEI-DAY(IDX3)  NOT =  ZERO
                       PERFORM  200111-RIHA-SANTEIPLUS-SEC
                     END-IF
      *
                     IF  (SANTEI-DAY(IDX3)  NOT =  ZERO)
                     AND (FLG-SANTEIPLUS-HANTEI =  ZERO)
                       MOVE  SANTEI-SRYYM    TO WRK-RIHASANTEI-YMD2(1:6)
                       MOVE  IDX3            TO WRK-RIHASANTEI-YMD2(7:2)
                       IF   (WRK-RIHASANTEI-YMD   = SPACE     )
                       OR   (WRK-RIHASANTEI-YMD   = "20060401")
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           MOVE  WRK-RIHASANTEI-YMD2
                                             TO WRK-RIHASANTEI-YMD
                           MOVE  1           TO FLG-KYUSEI-COMMENT
                           IF  (SANTEIPLUS-ENDYMD   NOT =  SPACE       )
                           AND (SANTEIPLUS-ENDYMD(1:6)  =  RECE41-SRYYM)
                           AND (IDX43                  >=  RIHA-ST2    )
                             MOVE   1   TO TB-RIHASANTEI-DAY-END3(IDX43)
                           END-IF
                         END-IF
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           IF   (WRK-RIHASANTEI41-FLG =  1        )
                           AND  (WRK-RIHASANTEI-YMD2  < "20060401")
                             MOVE  ZERO      TO TB-RIHASANTEI-DAY(1)
                           END-IF
                         END-IF
                       ELSE
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           MOVE  WRK-RIHASANTEI-YMD2
                                             TO WRK-RIHASANTEI-YMD3
                         END-IF
                         MOVE     1          TO FLG-KYUSEI-COMMENT2
                         IF  (WRK-RIHASANTEI-END  = SPACE              )
                         OR  (WRK-RIHASANTEI-END  < WRK-RIHASANTEI-YMD2)
                           IF    WRK-RIHASANTEI-YMD2  = "20060401"
                             MOVE  ZERO      TO FLG-KYUSEI-COMMENT2
                           END-IF
                         END-IF
                       END-IF
                     END-IF
      *
                   END-PERFORM
                  END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"   TO  MCP-TABLE
                 MOVE    "key3"         TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
      *
                END-PERFORM
      *
           END-IF
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      ***
      *
      *    当月先頭のリハビリ算定日より前に当月のリハビリ終了日があれば
      *    前月以前の直近のリハビリ開始日をクリアする
      *    及び、
      *    当月のリハビリ終了日より前の当月のリハビリ開始日をクリアする
           IF    RIHA-ST2            NOT = ZERO
             PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                       UNTIL   RIHA-IDX    >   31
               IF    (TB-RIHASANTEI-DAY-END(RIHA-IDX) NOT = ZERO)
               AND   (RIHA-ST2  >  RIHA-IDX                     )
                 MOVE   SPACE      TO  WRK-RIHASANTEI-YMD
                 MOVE   SPACE      TO  WRK-RIHASANTEI-YMD3
                 PERFORM  VARYING  RIHA-IDX2  FROM  1  BY  1
                           UNTIL   RIHA-IDX2    >  31
                   IF    (TB-RIHASANTEI-DAY(RIHA-IDX2)  NOT = ZERO)
                   AND   (RIHA-IDX  >= RIHA-IDX2                  )
                     MOVE  ZERO    TO  TB-RIHASANTEI-DAY(RIHA-IDX2)
                   END-IF
                 END-PERFORM
               END-IF
             END-PERFORM
           END-IF
      *
      *    リハビリ開始日を退避
           INITIALIZE                     WRK-CHECK-RIHASANTEI-AREA
           MOVE  TBL-RIHASANTEI-DAY   TO  WRK-CHECK-RIHASANTEI-DAY-OCC
           MOVE  WRK-RIHASANTEI-YMD   TO  WRK-CHECK-RIHASANTEI-YMD
           MOVE  WRK-RIHASANTEI-YMD3  TO  WRK-CHECK-RIHASANTEI-YMD3
      *
      *    当月のリハビリ開始日より後に当月のリハビリ算定日がなければ
      *    当月のリハビリ開始日をクリアする
           MOVE     ZERO       TO  FLG-OK
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY(RIHA-IDX)  NOT = ZERO
               PERFORM   200111-RIHA-SANTEIPLUS2-SEC
               MOVE      ZERO  TO  FLG-OK
               PERFORM  VARYING  RIHA-IDX2  FROM  1  BY  1
                         UNTIL   RIHA-IDX2    >  31
                 IF    (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX2) NOT = ZERO)
                 AND   (RIHA-IDX  <=  RIHA-IDX2                      )
                 AND   (IDX43     >=  RIHA-IDX2                      )
                   MOVE  ZERO  TO  TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX2)
                   MOVE  1     TO  FLG-OK
                 END-IF
               END-PERFORM
               IF   FLG-OK  =  ZERO
                 MOVE    ZERO  TO  TB-RIHASANTEI-DAY(RIHA-IDX)
               ELSE
                 IF  IDX43  NOT =  99
                   MOVE  1     TO  TB-RIHASANTEI-DAY-END2(IDX43)
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
      *    当月の先頭のリハビリ開始日
           MOVE     ZERO           TO  RIHA-ST
           PERFORM  VARYING  RIHA-IDX  FROM   1  BY   1
                     UNTIL   RIHA-IDX    >   31
             IF     TB-RIHASANTEI-DAY(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ST
               MOVE   31           TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    当月のリハビリ開始日に当月の終了日の入力（算定履歴で入力）
      *    があり、その中で一番月末に近い日
           MOVE     99             TO  RIHA-ED
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY-END2(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ED
               MOVE   1            TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    直近のリハビリ開始日に入力した当月の終了日（算定履歴で入力）
           MOVE     99             TO  RIHA-ED2
           PERFORM  VARYING  RIHA-IDX  FROM  31  BY  -1
                     UNTIL   RIHA-IDX    <    1
             IF     TB-RIHASANTEI-DAY-END3(RIHA-IDX)  NOT = ZERO
               MOVE   RIHA-IDX     TO  RIHA-ED2
               MOVE   1            TO  RIHA-IDX
             END-IF
           END-PERFORM
      *
      *    当月先頭のリハビリ開始日より前に当月のリハビリ算定日がなければ
      *    前月以前の直近のリハビリ開始日をクリアする
           MOVE     ZERO           TO  FLG-OK
           IF    WRK-RIHASANTEI-YMD  NOT = SPACE
             IF  RIHA-ST             NOT = ZERO
               PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                         UNTIL   RIHA-IDX    >  31
                 IF ((TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO) AND
                     (RIHA-ST   >  RIHA-IDX                       ))
                 OR ((TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO) AND
                     (RIHA-ED   <  RIHA-IDX                       ))
                   MOVE   1        TO  FLG-OK
                 END-IF
               END-PERFORM
               IF    FLG-OK  =  ZERO
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD3
               ELSE
                 MOVE     ZERO     TO  FLG-OK
                 PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                           UNTIL   RIHA-IDX    >  31
                   IF  (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO)
                   AND (RIHA-ED2  <  RIHA-IDX                       )
                     MOVE   1      TO  FLG-OK
                   END-IF
                 END-PERFORM
                 IF    FLG-OK  =  ZERO
                   MOVE     SPACE  TO  WRK-RIHASANTEI-YMD3
                 END-IF
               END-IF
             ELSE
               PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                         UNTIL   RIHA-IDX    >  31
                 IF  (TBL-HASSYO-TAIHI-SANTEI(RIHA-IDX) NOT = ZERO)
                 AND (RIHA-ED2  <  RIHA-IDX                       )
                   MOVE   1        TO  FLG-OK
                 END-IF
               END-PERFORM
               IF    FLG-OK  =  ZERO
                 MOVE     SPACE    TO  WRK-RIHASANTEI-YMD3
               END-IF
             END-IF
           END-IF
      *
           IF   (TBL-HASSYO-TAIHIYMD1-FLG  =  1)
           AND  (FLG-KYUSEI-COMMENT        =  1)
             INITIALIZE                     TBL-RIHASANTEI-DAY
             MOVE     SPACE            TO   WRK-RIHASANTEI-YMD
             MOVE     SPACE            TO   WRK-RIHASANTEI-YMD3
      *
             IF  (TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE                )
             AND (TBL-HASSYO-TAIHIYMD11(1) = WRK-CHECK-RIHASANTEI-YMD
                                          OR WRK-CHECK-RIHASANTEI-YMD3)
                 CONTINUE
             ELSE
                 MOVE  SPACE  TO  TBL-HASSYO-TAIHIYMD11(1)
             END-IF
             IF  (TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE                )
             AND (TBL-HASSYO-TAIHIYMD11(2) = WRK-CHECK-RIHASANTEI-YMD
                                          OR WRK-CHECK-RIHASANTEI-YMD3)
                 CONTINUE
             ELSE
                 MOVE  SPACE  TO  TBL-HASSYO-TAIHIYMD11(2)
             END-IF
      *
             IF  (TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE)
             AND (TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE)
               IF TBL-HASSYO-TAIHIYMD11(2) > TBL-HASSYO-TAIHIYMD11(1)
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD3
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD
               ELSE
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD3
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD
               END-IF
             ELSE
               IF TBL-HASSYO-TAIHIYMD11(1) NOT = SPACE
                 MOVE TBL-HASSYO-TAIHIYMD11(1) TO WRK-RIHASANTEI-YMD
               END-IF
               IF TBL-HASSYO-TAIHIYMD11(2) NOT = SPACE
                 MOVE TBL-HASSYO-TAIHIYMD11(2) TO WRK-RIHASANTEI-YMD
               END-IF
             END-IF
      *
             PERFORM  VARYING  RIHA-IDX  FROM  1  BY  1
                       UNTIL   RIHA-IDX    >  31
               IF    (TBL-HASSYO-TAIHIYMD12(RIHA-IDX)  NOT = ZERO)
               AND   (WRK-CHECK-RIHASANTEI-DAY(RIHA-IDX)   = ZERO)
                 MOVE  ZERO  TO  TBL-HASSYO-TAIHIYMD12(RIHA-IDX)
               END-IF
             END-PERFORM
             MOVE  TBL-HASSYO-TAIHIYMD1-DAY-OCC2
                             TO  TBL-RIHASANTEI-DAY
           END-IF
      *
      ***
      *
           IF      FLG-KYUSEI-COMMENT    =      1
               IF  WRK-RIHASANTEI-YMD3 NOT = SPACE
                     MOVE   ZERO   TO  FLG-RIHA
                     MOVE   WRK-RIHASANTEI-YMD3
                                   TO  WRK-KYUSEIYMD
                     PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                     IF     RECE41-SRYYM >= "202008"
                       PERFORM    20011-HASSYO-COM-202008-SEC
                     ELSE
                       IF   RECE41-SRYYM >= "201905"
                         PERFORM  20011-HASSYO-COM-201905-SEC
                       ELSE
                         PERFORM  20011-HASSYO-COMMENT-SEC
                       END-IF
                     END-IF
               END-IF
               IF  WRK-RIHASANTEI-YMD  NOT = SPACE
                     MOVE   1      TO  FLG-RIHA
                     MOVE   WRK-RIHASANTEI-YMD
                                   TO  WRK-KYUSEIYMD
                     PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                     IF     RECE41-SRYYM >= "202008"
                       PERFORM    20011-HASSYO-COM-202008-SEC
                     ELSE
                       IF   RECE41-SRYYM >= "201905"
                         PERFORM  20011-HASSYO-COM-201905-SEC
                       ELSE
                         PERFORM  20011-HASSYO-COMMENT-SEC
                       END-IF
                     END-IF
               END-IF
               PERFORM   VARYING   IDX16    FROM   1   BY   1
                          UNTIL    IDX16      >   31
                 IF  TB-RIHASANTEI-DAY(IDX16)  NOT = ZERO
                     MOVE   2      TO  FLG-RIHA
                     MOVE   RECE41-SRYYM
                                   TO  WRK-KYUSEIYMD(1:6)
                     MOVE   IDX16  TO  WRK-KYUSEIYMD(7:2)
                     PERFORM    20011-KYUSEIHASSYO-KENSAKU-SEC
                     IF     RECE41-SRYYM >= "202008"
                       PERFORM    20011-HASSYO-COM-202008-SEC
                     ELSE
                       IF   RECE41-SRYYM >= "201905"
                         PERFORM  20011-HASSYO-COM-201905-SEC
                       ELSE
                         PERFORM  20011-HASSYO-COMMENT-SEC
                       END-IF
                     END-IF
                 END-IF
               END-PERFORM
           END-IF
      *
           .
       20011-RIHABIRIHASSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ・算定履歴付加判定
      *    （リハ開始日の終了日と診療月をチェック）（履歴番号＝０のデータ）
      *****************************************************************
       200111-RIHA-SANTEIPLUS-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SANTEIPLUS-HANTEI
           MOVE    ZERO                TO  IDX43
      *
           INITIALIZE                      SANTEIPLUS-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEIPLUS-PTID
           MOVE    SANTEI-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    IDX3                TO  SANTEIPLUS-DAYKEY
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =  ZERO
             MOVE  "tbl_santeiplus"    TO  MCP-TABLE
             MOVE  "key4"              TO  MCP-PATHNAME
             PERFORM  900-SANTEIPLUS-READ-SEC
             IF    FLG-SANTEIPLUS   =  ZERO
               MOVE  SANTEIPLUS-ENDYMD(7:2)  TO  IDX43
               IF  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                    (SANTEIPLUS-ENDYMD(1:6)  <  RECE41-SRYYM))
               OR  ((SANTEIPLUS-ENDYMD   NOT =  SPACE       ) AND
                    (SANTEIPLUS-ENDYMD(1:6)  =  RECE41-SRYYM) AND
                    (IDX43                   <  RIHA-ST2    ))
                  MOVE     1           TO  FLG-SANTEIPLUS-HANTEI
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RIHA-SANTEIPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ・算定履歴付加判定
      *    （リハ開始日の終了日と診療月をチェック）（履歴番号＝０のデータ）
      *****************************************************************
       200111-RIHA-SANTEIPLUS2-SEC          SECTION.
      *
           MOVE    99                  TO  IDX43
      *
           INITIALIZE                      SANTEIPLUS-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEIPLUS-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEIPLUS-PTID
           MOVE    RECE41-SRYYM        TO  SANTEIPLUS-SRYYM
           MOVE    WRK-SRYCD           TO  SANTEIPLUS-SRYCD
           MOVE    ZERO                TO  SANTEIPLUS-NYUGAIKBN
           MOVE    ZERO                TO  SANTEIPLUS-SRYKA
           MOVE    RECE41-HKNCOMBI     TO  SANTEIPLUS-HKNCOMBINUM
           MOVE    RIHA-IDX            TO  SANTEIPLUS-DAYKEY
           MOVE    SANTEIPLUS-REC      TO  MCPDATA-REC
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =  ZERO
             MOVE  "tbl_santeiplus"    TO  MCP-TABLE
             MOVE  "key4"              TO  MCP-PATHNAME
             PERFORM  900-SANTEIPLUS-READ-SEC
             IF    FLG-SANTEIPLUS   =  ZERO
               IF   (SANTEIPLUS-ENDYMD   NOT =  SPACE       )
               AND  (SANTEIPLUS-ENDYMD(1:6)  =  RECE41-SRYYM)
                 MOVE  SANTEIPLUS-ENDYMD(7:2)  TO  IDX43
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_santeiplus"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200111-RIHA-SANTEIPLUS2-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責従来様式：加算率 のコメント編集
      *****************************************************************
       2001-KASANRITU-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999998            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999998            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    加算率のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    9                    TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "加算率（　．　　）"
                                        TO  WRK-RECE45XX-NAME(1 1)
      *
           MOVE    RECE41-KSNRATE(1:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(9:2)
           MOVE    RECE41-KSNRATE(2:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(13:2)
           MOVE    RECE41-KSNRATE(3:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(15:2)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           PERFORM 800-RECE459-WRITE-SEC
      *
           .
       2001-KASANRITU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算率編集
      *****************************************************************
       20011-KASANRITU-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-HENSYU-KASANRITU
           MOVE    SPACE                TO  WRK-HENSYU-KASANRITU2
      *
           MOVE    DAY-IDX3             TO  WRK-HENSYU-KASANRITU
      *
           INITIALIZE                       ORCSKANACHKAREA
           MOVE    "2"                  TO  KANACHK-SYORI
           MOVE    WRK-HENSYU-KASANRITU TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
                MOVE    KANACHK-OUT-INPUT(1:2)
                                        TO  WRK-HENSYU-KASANRITU2
           END-IF
      *
           .
       20011-KASANRITU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責従来様式：薬剤等点単価 のコメント編集
      *****************************************************************
       2001-YKZTENTANKA-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999998            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999998            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    薬剤等点単価のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    13                   TO  WRK-RECE45XX-YUKOKETA(1 1)
           MOVE    "薬剤等（１点単価　　　円）"
                                        TO  WRK-RECE45XX-NAME(1 1)
      *
           MOVE    RECE41-YKZTENTANKA(1:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           IF      WRK-HENSYU-KASANRITU2   =   "０"
              MOVE   "　"   TO   WRK-HENSYU-KASANRITU2
           END-IF
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(19:2)
           MOVE    RECE41-YKZTENTANKA(2:1)  TO  DAY-IDX3
           PERFORM    20011-KASANRITU-HENSYU-SEC
           MOVE    WRK-HENSYU-KASANRITU2 TO WRK-RECE45XX-NAME(1 1)(21:2)
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           PERFORM 800-RECE459-WRITE-SEC
      *
           .
       2001-YKZTENTANKA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責従来様式：乳幼児・幼児のコメント編集
      *****************************************************************
       2001-NYUYOJI-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999990            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999990            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *    乳幼児・幼児のコメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           IF   RECE41-SRYYM  >= "201810"
      *      「月の途中まで乳幼児」
             MOVE  "820100005"  TO  WRK-TENSU-SRYCD
             MOVE  RECE41-SRYYM TO  WRK-TENSU-SRYYM
             PERFORM 900-TENSU-TBL-SEC
      *
             MOVE  TNS-SRYCD    TO  WRK-RECE45XX-SRYCD(1 1)
             MOVE  ZERO         TO  WRK-RECE45XX-SRYSURYO(1 1)
             MOVE  TNS-YUKOKETA TO  WRK-RECE45XX-YUKOKETA(1 1)
             MOVE  TNS-NAME     TO  WRK-RECE45XX-NAME(1 1)
                                    WRK-RECE45XX-INPUTCOMENT(1 1)
           ELSE
             MOVE  "810000001"  TO  WRK-RECE45XX-SRYCD(1 1)
             MOVE   ZERO        TO  WRK-RECE45XX-SRYSURYO(1 1)
             MOVE   11          TO  WRK-RECE45XX-YUKOKETA(1 1)
             MOVE  "　　月　　日まで乳幼児"
                                TO  WRK-RECE45XX-NAME(1 1)
      *
             MOVE  RECE41-BIRTHDAY(5:2) TO  DAY-IDX2
             MOVE  TBL-DAY(DAY-IDX2)    TO  WRK-RECE45XX-NAME(1 1)(1:4)
             MOVE  RECE41-BIRTHDAY(7:2) TO  DAY-IDX2
             COMPUTE   DAY-IDX2  =  DAY-IDX2  -  1
             IF    DAY-IDX2  NOT =  ZERO
               MOVE  TBL-DAY(DAY-IDX2)  TO  WRK-RECE45XX-NAME(1 1)(7:4)
             END-IF
           END-IF
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
      *
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           PERFORM 800-RECE459-WRITE-SEC
      *
           .
       2001-NYUYOJI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    在支援のコメント編集
      *****************************************************************
       2001-ZAISIEN-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999965            TO  RECE449-ZAINUM
           MOVE    WRK-RECE44-SRYKBN   TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999965            TO  RECE459-ZAINUM
           MOVE    WRK-RECE44-SRYKBN   TO  RECE459-SRYKBN
      *    在支援コメントの挿入
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    "810000001"          TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                 TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    3                    TO  WRK-RECE45XX-YUKOKETA(1 1)
           EVALUATE  TBL-ZAISIEN-COMMENT
             WHEN      1
               MOVE    "在支援"         TO  WRK-RECE45XX-NAME(1 1)
             WHEN      2
               MOVE    "在支病"         TO  WRK-RECE45XX-NAME(1 1)
           END-EVALUATE
      *
           MOVE    "1"                  TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                        TO  RECE459-SRYACT-TBL(1)
           PERFORM 800-RECE459-WRITE-SEC
      *
           .
       2001-ZAISIEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算（特)のコメント編集（レセ電用）
      *****************************************************************
       2001-GAITOK-COMMENT-SEC          SECTION.
      *
           PERFORM 800-RECE41-SET449-SEC
           MOVE    99999986            TO  RECE449-ZAINUM
           MOVE    "99"                TO  RECE449-SRYKBN
           MOVE    1                   TO  RECE449-MEISAISU
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE449-HKNCOMBI
           PERFORM 800-RECE449-WRITE-SEC
      *
           PERFORM 800-RECE41-SET459-SEC
           MOVE    99999986            TO  RECE459-ZAINUM
           MOVE    "99"                TO  RECE459-SRYKBN
      *
      *    「（特)　５２×回数」のコメント挿入
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
           MOVE    "810000001"         TO  WRK-RECE45XX-SRYCD(1 1)
           MOVE    ZERO                TO  WRK-RECE45XX-SRYSURYO(1 1)
           MOVE    9                   TO  WRK-RECE45XX-YUKOKETA(1 1)
      *
           MOVE    "（特）　"          TO  WRK-RECE45XX-NAME(1 1)(1:8)
      *
           MOVE    "　　　　　"    TO  WRK-TNSKAI
           MOVE    RECE41-GAITOKU-KAISU1
                                   TO  WRK-ZAIKAISUX
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    "52"            TO  KANACHK-MAE-INPUT (1:2)
           MOVE    "X"             TO  KANACHK-MAE-INPUT (3:1)
           MOVE    WRK-ZAIKAISUX   TO  KANACHK-MAE-INPUT (4:2)
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
           AND     KANACHK-MAX     >   ZERO
             MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                   TO  WRK-TNSKAI
             MOVE    "×"          TO  WRK-TNSKAI(5:2)
           END-IF
           MOVE    WRK-TNSKAI          TO  WRK-RECE45XX-NAME(1 1)(9:10)
      *
           MOVE    "1"                 TO  WRK-RECE45XX-RECEPTFLG(1 1)
      *
           MOVE    "1"                 TO  WRK-RECE45XX-SIKFLG(1 1)
           MOVE    WRK-RECE45XX-SRYACT-TBL(1 1)
                                       TO  RECE459-SRYACT-TBL(1)
           PERFORM 800-RECE459-WRITE-SEC
      *
           .
       2001-GAITOK-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診（同日複数診療科）があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-SYODOU-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-SYODOU-COMMENT   =    "1"
                INITIALIZE             WRK-JIDO-SETCOM-AREA
      ***       労災マスタがtbl_recekisaiにない為の処置
      ***>      IF   RECE41-SRYYM  >= "202008"
      ***>        MOVE    "2"       TO     WRK-JIDO-SETCOMFLG
      ***>      ELSE
                  MOVE    "1"       TO     WRK-JIDO-SETCOMFLG
                  MOVE     1        TO     WRK-JIDO-SETCOMATFLG
      ***>      END-IF
                MOVE       99       TO     FLG-COMMENT
                PERFORM    20011-SYODOU-COMMENT-SEC
           END-IF
      *
           .
       2001-SYODOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診（同日複数診療科）のコメント編集
      *****************************************************************
       20011-SYODOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE44-SYODOU-SRYKA(IDX)  =  SPACE)
      *
      *      診療科の取得
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE44-SYODOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE41-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                IF   RECE41-SRYYM  >= "202008"
                  MOVE   1             TO  IDX-JIDO
                  PERFORM  200111-RECECOMMENT-IDXGET-SEC
                  MOVE   SYS-1005-SRYKANAME(1:40)
                                       TO  WRK-RECECOM-COMMENT
                  PERFORM  200111-RECECOMMENT-HENSYU-SEC
                ELSE
                  ADD   1                TO  IDZ
                  MOVE  "複初算定診療科："
                                         TO  WK-FREE-NAME2(IDZ)(1:16)
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
                  PERFORM  VARYING  IDW  FROM  300  BY  -1
                            UNTIL   IDW    <   1
                    IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                      COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                      MOVE    1   TO    IDW
                    END-IF
                  END-PERFORM
                END-IF
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             PERFORM  200111-JIDOSET-HENSYU-SEC
      *
           END-PERFORM
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SYODOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診（同一日２科目）があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-SAIDOU-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-SAIDOU-COMMENT   =    "1" OR "2"
                INITIALIZE             WRK-JIDO-SETCOM-AREA
      ***       労災マスタがtbl_recekisaiにない為の処置
      ***>      IF   RECE41-SRYYM  >= "202008"
      ***>        MOVE    "2"       TO     WRK-JIDO-SETCOMFLG
      ***>      ELSE
                  MOVE    "1"       TO     WRK-JIDO-SETCOMFLG
                  MOVE     1        TO     WRK-JIDO-SETCOMATFLG
      ***>      END-IF
                MOVE       99       TO     FLG-COMMENT
                PERFORM    20011-SAIDOU-COMMENT-SEC
           END-IF
      *
           .
       2001-SAIDOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診（同一日２科目）のコメント編集
      *****************************************************************
       20011-SAIDOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                       >   10  )
                       OR     (RECE44-SAIDOU-SRYKA(IDX)  =  SPACE)
      *
      *      診療科の取得
             INITIALIZE                      SYS-1005-REC
             MOVE    "1005"              TO  SYS-1005-KANRICD
             MOVE    RECE44-SAIDOU-SRYKA(IDX)
                                         TO  SYS-1005-KBNCD
             MOVE    RECE41-SRYYM        TO  SYS-1005-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
             MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
             MOVE    SYS-1005-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI      =   ZERO
                IF   RECE41-SRYYM  >= "202008"
                  EVALUATE  RECE44-FLG-SAIDOU-COMMENT
                    WHEN      "1"
                      MOVE   2         TO  IDX-JIDO
                    WHEN      "2"
                      MOVE   3         TO  IDX-JIDO
                  END-EVALUATE
                  PERFORM  200111-RECECOMMENT-IDXGET-SEC
                  MOVE   SYS-1005-SRYKANAME(1:40)
                                       TO  WRK-RECECOM-COMMENT
                  PERFORM  200111-RECECOMMENT-HENSYU-SEC
                ELSE
                  ADD   1                TO  IDZ
                  MOVE  "複再算定診療科："
                                         TO  WK-FREE-NAME2(IDZ)(1:16)
                  MOVE  SYS-1005-SRYKANAME(1:40)
                                         TO  WK-FREE-NAME2(IDZ)(17:40)
                  PERFORM  VARYING  IDW  FROM  300  BY  -1
                            UNTIL   IDW    <   1
                    IF   WK-FREE-NAME2(IDZ)(IDW:1)  NOT =  SPACE
                      COMPUTE WK-FREE-YUKOKETA2(IDZ) =  IDW  /   2
                      MOVE    1   TO    IDW
                    END-IF
                  END-PERFORM
                END-IF
               END-IF
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             PERFORM  200111-JIDOSET-HENSYU-SEC
      *
           END-PERFORM
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SAIDOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン診療料があるか判定
      *    オンライン医学管理料があるか判定
      *    オンライン在宅管理料（在医総管（月１回訪問診療））があるか判定
      *    精神科オンライン在宅管理料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-ONLINE-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-ONLINE-COMMENT   NOT =  SPACE
             MOVE      ZERO         TO     IDZ
             INITIALIZE                    WRK-FREE-AREA2
             IF  (RECE44-FLG-NINRYO-COMMENT   =
                 "A" OR "B" OR "G" OR "H" OR "C" OR "D" OR
                 "E" OR "F" OR "I" OR "J" OR "P" OR "Q"       )
             AND (RECE44-FLG-ONLINE-COMMENT   =  "4"          )
               PERFORM   2001-NINRYO-HENSYU-SEC
               IF   WRK-NINRYO-FSANTEIYMD   NOT =  SPACE
                 INITIALIZE                WRK-JIDO-SETCOM-AREA
                 IF   RECE41-SRYYM  >= "202008"
                   MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                 ELSE
                   MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                   MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                 END-IF
                 MOVE       99      TO     FLG-COMMENT
      *
                 IF   RECE41-SRYYM  >= "202008"
                   EVALUATE  RECE44-FLG-NINRYO-COMMENT
                     WHEN     "A"
                       MOVE   45            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "B"
                       MOVE   46            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "G"
                       MOVE   47            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "H"
                       MOVE   48            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "C"
                       MOVE   49            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "D"
                       MOVE   50            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "E"
                       MOVE   51            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "F"
                       MOVE   52            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "I"
                       MOVE   53            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "J"
                       MOVE   54            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "P"
                       MOVE   55            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN     "Q"
                       MOVE   56            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-NINRYO-FSANTEIYMD
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                   END-EVALUATE
                 ELSE
                   ADD        1       TO     IDZ
                   MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                   MOVE   "初回算定　　　月　　日"
                                      TO     WK-FREE-NAME2(IDZ)
                   MOVE   WRK-NINRYO-FSANTEIYMD(5:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                                      TO     WK-FREE-NAME2(IDZ)(11:4)
                   MOVE   WRK-NINRYO-FSANTEIYMD(7:2)   TO  DAY-IDX2
                   MOVE   TBL-DAY(DAY-IDX2)
                                      TO     WK-FREE-NAME2(IDZ)(17:4)
                 END-IF
                 PERFORM  200111-JIDOSET-HENSYU-SEC
               END-IF
             END-IF
      *
             MOVE  SPACE            TO  WRK-ONLINE-COMCD
             MOVE  SPACE            TO  WRK-ONLINE-FSANTEIYM
             INITIALIZE                 ORCSRECESRCHKAREA
             MOVE  RECE41-PTID      TO  LNK-RECESRCHK-PTID
             MOVE  RECE41-SRYYM     TO  LNK-RECESRCHK-SRYYM
             MOVE  RECE44-FLG-ONLINE-COMMENT
                                    TO  LNK-RECESRCHK-CHKKBN
             IF    RECE44-FLG-ONLINE-COMMENT       =  "1"
             AND   RECE44-FLG-ONLINE2-COMMENT  NOT =  ZERO
                MOVE  RECE44-FLG-ONLINE2-COMMENT
                                    TO  LNK-RECESRCHK-CHKNO
             END-IF
             CALL  "ORCSRECESRCHK"  USING  SPA-AREA
                                           ORCSRECESRCHKAREA
             IF    LNK-RECESRCHK-RC  =  ZERO
                MOVE  LNK-RECESRCHK-COMCD
                                    TO  WRK-ONLINE-COMCD
                MOVE  LNK-RECESRCHK-FSANTEIYMD
                                    TO  WRK-ONLINE-FSANTEIYM
             END-IF
      *
             IF   WRK-ONLINE-COMCD   NOT =  SPACE
              IF   RECE41-SRYYM  >= "202008"
                  INITIALIZE            WRK-JIDO-SETCOM-AREA
                  MOVE  "2"         TO  WRK-JIDO-SETCOMFLG
                  MOVE   99         TO  FLG-COMMENT
                  EVALUATE  RECE44-FLG-ONLINE-COMMENT
                    WHEN      "1"
                      MOVE   97            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ONLINE-COMCD
                                           TO  WRK-RECECOM-SRYCD
                      MOVE   WRK-ONLINE-FSANTEIYM
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN      "3"
                      MOVE   98            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ONLINE-FSANTEIYM
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN      "4"
                      MOVE   99            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ONLINE-FSANTEIYM
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-EVALUATE
                  PERFORM  200111-JIDOSET-HENSYU-SEC
              ELSE
               IF   RECE41-SRYYM  >= "202004"
               AND  RECE41-SRYYM  <= "202007"
                  EVALUATE  WRK-ONLINE-COMCD
                    WHEN     "850100001"
                      MOVE     "820100006"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100002"
                      MOVE     "820100007"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100003"
                      MOVE     "820100008"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100004"
                      MOVE     "820100009"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100005"
                      MOVE     "820100010"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100006"
                      MOVE     "820100011"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100007"
                      MOVE     "820100012"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100008"
                      MOVE     "820100013"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100009"
                      MOVE     "820100014"  TO  WRK-ONLINE-COMCD
                    WHEN     "850100010"
                      MOVE     "820100015"  TO  WRK-ONLINE-COMCD
                  END-EVALUATE
               END-IF
      *
               IF   RECE44-FLG-ONLINE-COMMENT   =   "1"
               AND  RECE44-FLG-ONLINE2-COMMENT  =   ZERO
               AND  WRK-ONLINE-COMCD  NOT=  "850100011"
      *
                  MOVE  WRK-ONLINE-COMCD
                                       TO  WRK-TENSU-SRYCD
                  MOVE  RECE41-SRYYM   TO  WRK-TENSU-SRYYM
                  PERFORM      900-TENSU-TBL-SEC
      *
                  INITIALIZE               WRK-JIDO-SETCOM-AREA
                  MOVE      "1"        TO  WRK-JIDO-SETCOMFLG
                  MOVE       1         TO  WRK-JIDO-SETCOMATFLG
                  MOVE       99        TO  FLG-COMMENT
      *
                  ADD        1         TO  IDZ
      *
                  MOVE  TNS-YUKOKETA   TO  WK-FREE-YUKOKETA2(IDZ)
                  MOVE  TNS-NAME       TO  WK-FREE-NAME2(IDZ)
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
      *
               END-IF
      *
               IF   RECE44-FLG-ONLINE3-COMMENT  =   SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                EVALUATE  RECE44-FLG-ONLINE-COMMENT
                  WHEN      "1"
                  WHEN      "2"
                  WHEN      "3"
                    MOVE    1      TO     WRK-JIDO-SETCOMATFLG
                  WHEN      "4"
                    MOVE    2      TO     WRK-JIDO-SETCOMATFLG
                END-EVALUATE
                MOVE       99      TO     FLG-COMMENT
      *
                ADD        1       TO     IDZ
      *
                EVALUATE  RECE44-FLG-ONLINE-COMMENT
                  WHEN      "1"
                  WHEN      "2"
                    IF   RECE41-SRYYM >= "201905"
                      MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "管理料等算定開始年月　　　　　年　　月"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       27      TO     IDX41
                      MOVE       33      TO     IDX42
                      MOVE       23      TO     IDX49
                    ELSE
                      MOVE       17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "管理料等算定開始年月　　　年　　月"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       23      TO     IDX41
                      MOVE       29      TO     IDX42
                    END-IF
                  WHEN      "3"
                  WHEN      "4"
                    IF   RECE41-SRYYM >= "201905"
                      MOVE       18      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "管理料算定開始年月　　　　　年　　月"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       25      TO     IDX41
                      MOVE       31      TO     IDX42
                      MOVE       21      TO     IDX49
                    ELSE
                      MOVE       16      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "管理料算定開始年月　　　年　　月"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       21      TO     IDX41
                      MOVE       27      TO     IDX42
                    END-IF
                END-EVALUATE
      *
                INITIALIZE                          STS-AREA-DAY
                INITIALIZE                          LNK-DAY2-AREA
                MOVE    "21"                    TO  LNK-DAY2-IRAI
                MOVE    WRK-ONLINE-FSANTEIYM    TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   IF   RECE41-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                        TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                END-IF
      *
                PERFORM  200111-JIDOSET-HENSYU-SEC
      *
               END-IF
              END-IF
             END-IF
      *
             IF   IDZ    NOT =  ZERO
                PERFORM  200111-RECE45-TAIHI-SEC
      *
      *         ＲＥＣＥ０５ファイルの内容をテーブルに退避
                MOVE    ZERO                 TO  IDX-RECE45
                MOVE    SPACE                TO  WRK-RECE45XX-REC
                INITIALIZE                       WRK-RECE45XX-REC
      *
                MOVE    RECE44-MEISAISU      TO  MAX-MEISAISU
      *
                PERFORM   VARYING   IDX   FROM    1   BY  1
                           UNTIL  ( IDX                >   50    )
                            OR    ( WK-FREE-NAME2(IDX) =   SPACE )
                  COMPUTE  MAX-MEISAISU  =   MAX-MEISAISU  +  1
                  IF   RECE41-SRYYM  >= "202008"
                      MOVE  WK-FREE-SRYCD2(IDX)
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                  ELSE
                    IF   (IDX                         =    1  )
                    AND  (WRK-ONLINE-COMCD        NOT =  SPACE)
                    AND  (RECE44-FLG-ONLINE-COMMENT   =   "1" )
                    AND  (RECE44-FLG-ONLINE2-COMMENT  =   ZERO)
                    AND  (WRK-ONLINE-COMCD  NOT=  "850100011" )
                      MOVE  WRK-ONLINE-COMCD
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                    ELSE
                      MOVE  "810000001"
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                    END-IF
                  END-IF
                  MOVE  ZERO
                          TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                  MOVE  WK-FREE-YUKOKETA2(IDX)
                          TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                  MOVE  WK-FREE-NAME2(IDX)
                          TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                  MOVE  WK-FREE-INPUTCOMENT2(IDX)
                          TO  WRK-RECE45XX-INPUTCOMENT(MAX-MEISAISU 1)
                  MOVE  WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE45XX-MEISKYFLG(MAX-MEISAISU 1)
                  MOVE  WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE45XX-SETCOMATFLG(MAX-MEISAISU 1)
                  MOVE  WK-FREE-RECEDENFLG2(IDX)
                          TO  WRK-RECE45XX-RECEDENFLG(MAX-MEISAISU 1)
                  MOVE  "1"
                          TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
                END-PERFORM
      *
                PERFORM    200111-SRYACTINF-HENSYU-SEC
             END-IF
           END-IF
      *
           .
       2001-ONLINE-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性疼痛疾患管理料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-SHIDOU-HANTEI-SEC          SECTION.
      *
           IF   RECE44-FLG-SHIDOU-COMMENT   =    "1"
      *
             MOVE    ZERO                TO  FLG-SHIDOU-COMMENT
      *
      *      算定履歴検索
             INITIALIZE                  SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    RECE41-SRYYM        TO  SANTEI-SRYYM
             MOVE    CONS-MANSEI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  WRK-MCP-TABLE
             MOVE    "key"               TO  WRK-MCP-PATHNAME
             PERFORM  900-SANTEI-INV-SEC
             IF       FLG-SANTEI        =   ZERO
               IF     SANTEI-FSANTEIYMD(1:6)  =  RECE41-SRYYM
                  MOVE    1    TO    FLG-SHIDOU-COMMENT
               END-IF
             END-IF
      *
             IF      FLG-SHIDOU-COMMENT    =      1
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-SHIDOU-COMMENT-SEC
             END-IF         
      *
           END-IF
      *
           .
       2001-SHIDOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性疼痛疾患管理料のコメント編集
      *****************************************************************
       20011-SHIDOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO          TO   IDZ
           INITIALIZE               WRK-FREE-AREA2
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       RECE44-REC    TO   RECE449-REC
      *@@@@
           MOVE       RECE44-ZAINUM TO   RECE449-ZAINUM
           MOVE       RECE44-SRYKBN TO   RECE449-SRYKBN
      *@@@@
           ADD        1             TO   RECE449-MEISAISU
           IF   RECE41-SRYYM  >= "202008"
             MOVE   "1"             TO   RECE449-SENTAKUCOMFLG
           ELSE
             MOVE   "1"             TO   RECE449-SETCOMFLG
           END-IF
           PERFORM 800-RECE449-WRITE-SEC
      *
           PERFORM 800-RECE44-SET45-SEC
           MOVE    RECE44-ZAINUM        TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE45
                 NOT INVALID
                     PERFORM 990-RECE45-NEXT-SEC
           END-START
      *
           MOVE    RECE45-REC           TO  RECE459-REC
           MOVE    RECE45-SRYACT-TBL(1) TO  RECE459-SRYACT-TBL(1)
           MOVE    RECE45-SRYACT-TBL(2) TO  RECE459-SRYACT-TBL(3)
           MOVE    RECE45-SRYACT-TBL(3) TO  RECE459-SRYACT-TBL(4)
           MOVE    RECE45-SRYACT-TBL(4) TO  RECE459-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE459-SRYACT-TBL(2)
           INITIALIZE                       RECE459-SRYACT-TBL(2)
           IF   RECE41-SRYYM  >= "202008"
             MOVE  4                    TO  IDX-JIDO
             PERFORM  200111-RECECOMMENT-IDXGET-SEC
             MOVE  RECE41-SRYYM         TO  WRK-RECECOM-YMD(1:6)
             MOVE  TB-SANTEI-DAY(1)     TO  WRK-RECECOM-YMD(7:2)
             PERFORM  200111-RECECOMMENT-HENSYU-SEC
             MOVE  WK-FREE-SRYCD2(1)    TO  RECE459-SRYCD(2)
             MOVE  ZERO                 TO  RECE459-SRYSURYO(2)
             MOVE  WK-FREE-YUKOKETA2(1) TO  RECE459-YUKOKETA(2)
             MOVE  WK-FREE-NAME2(1)     TO  RECE459-NAME(2)
             MOVE  WK-FREE-INPUTCOMENT2(1)
                                        TO  RECE459-INPUTCOMENT(2)
             MOVE  "1"                  TO  RECE459-SIKFLG(2)
           ELSE
             MOVE  "810000001"          TO  RECE459-SRYCD(2)
             MOVE  ZERO                 TO  RECE459-SRYSURYO(2)
             MOVE  15                   TO  RECE459-YUKOKETA(2)
             MOVE  "慢性疼痛疾患管理算定日　　　日"
                                        TO  RECE459-NAME(2)
             MOVE  TB-SANTEI-DAY(1)     TO  DAY-IDX
             MOVE  TBL-DAY(DAY-IDX)     TO  RECE459-NAME(2)(25:4)
             MOVE  "2"                  TO  RECE459-MEISKYFLG(2)
      *#     MOVE   5                   TO  RECE459-SETCOMATFLG(2)
             MOVE   1                   TO  RECE459-SETCOMATFLG(2)
             MOVE   SPACE               TO  RECE459-RECEDENFLG(2)
             MOVE  "1"                  TO  RECE459-SIKFLG(2)
           END-IF
           PERFORM 800-RECE459-WRITE-SEC
      *
           .
       20011-SHIDOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ニコチン依存症管理料・重度喘息患者治療管理加算があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-NIKOCHINTOU-HANTEI-SEC          SECTION.
      *
           IF     (RECE44-FLG-NIKOCHIN-COMMENT   =    "1" OR "2")
           OR     (RECE44-FLG-ZENSOKU-COMMENT    =    "1"       )
      *
             IF    RECE44-FLG-NIKOCHIN-COMMENT   =    "1"
                 MOVE   CONS-NIKOCHIN  TO  WRK-SRYCD
             END-IF
             IF    RECE44-FLG-NIKOCHIN-COMMENT   =    "2"
                 MOVE   CONS-NIKOCHIN2 TO  WRK-SRYCD
             END-IF
             IF    RECE44-FLG-ZENSOKU-COMMENT    =    "1"
                 MOVE   CONS-ZENSOKU   TO  WRK-SRYCD
             END-IF
      *
             MOVE     ZERO     TO     FLG-NIKOCHINTOU-COMMENT
                                      FLG-NIKOCHINTOU-COMMENT2
             MOVE     SPACE    TO     WRK-NIKOCHINTOU-YMD
                                      WRK-NIKOCHINTOU-YMD2
      *
      *      算定履歴検索
             INITIALIZE                  SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI              =   1 )
                             OR   ( FLG-NIKOCHINTOU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                    MOVE  SANTEI-SRYYM     TO  WRK-NIKOCHINTOU-YMD(1:6)
                    MOVE  SANTEI-MSANTEID  TO  WRK-NIKOCHINTOU-YMD(7:2)
                    MOVE    1              TO  FLG-NIKOCHINTOU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF    RECE44-FLG-NIKOCHIN-COMMENT   =    "1"
      *          ニコチン依存症管理料１（初回）（診療報酬上臨時的取扱）の
      *          算定履歴検索
                 MOVE   CONS-NIKOCHIN3   TO  WRK-SRYCD
      *
                 INITIALIZE                  SANTEI-REC
                 MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
                 MOVE    RECE41-PTID         TO  SANTEI-PTID
                 MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
                 MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                 MOVE    ZERO                TO  SANTEI-SRYKA
                 MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
                 MOVE    SANTEI-REC          TO  MCPDATA-REC
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC              =   ZERO
                     MOVE    "tbl_santei"        TO  MCP-TABLE
                     MOVE    "key3"              TO  MCP-PATHNAME
                     PERFORM        900-SANTEI-READ-SEC
                     PERFORM   UNTIL  ( FLG-SANTEI               =   1 )
                                 OR   ( FLG-NIKOCHINTOU-COMMENT2 =   1 )
      *
                       IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                        MOVE  SANTEI-SRYYM     TO
                                               WRK-NIKOCHINTOU-YMD2(1:6)
                        MOVE  SANTEI-MSANTEID  TO
                                               WRK-NIKOCHINTOU-YMD2(7:2)
                        IF   WRK-NIKOCHINTOU-YMD2
                                               >   WRK-NIKOCHINTOU-YMD
                            MOVE    WRK-NIKOCHINTOU-YMD2
                                               TO  WRK-NIKOCHINTOU-YMD
                        END-IF
                        MOVE    1              TO
                                               FLG-NIKOCHINTOU-COMMENT
                                               FLG-NIKOCHINTOU-COMMENT2
                       END-IF
      *
                       MOVE    "tbl_santei"    TO  MCP-TABLE
                       MOVE    "key3"          TO  MCP-PATHNAME
                       PERFORM        900-SANTEI-READ-SEC
                     END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_santei"        TO  MCP-TABLE
                 MOVE    "key3"              TO  MCP-PATHNAME
                 PERFORM 900-CLOSE-SEC
             END-IF
      *
             IF      FLG-NIKOCHINTOU-COMMENT    =      1
                 INITIALIZE             WRK-JIDO-SETCOM-AREA
                 IF   RECE41-SRYYM  >= "202008"
                   MOVE    "2"       TO     WRK-JIDO-SETCOMFLG
                 ELSE
                   MOVE    "1"       TO     WRK-JIDO-SETCOMFLG
                   IF    RECE44-FLG-NIKOCHIN-COMMENT   =    "1" OR "2"
                     MOVE   1        TO     WRK-JIDO-SETCOMATFLG
                   END-IF
                   IF    RECE44-FLG-ZENSOKU-COMMENT    =    "1"
                     MOVE   1        TO     WRK-JIDO-SETCOMATFLG
                   END-IF
                 END-IF
                 MOVE       99       TO     FLG-COMMENT
                 PERFORM    20011-NIKOCHINTOU-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-NIKOCHINTOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ニコチン依存症管理料・重度喘息患者治療管理加算のコメント編集
      *****************************************************************
       20011-NIKOCHINTOU-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           IF    RECE44-FLG-NIKOCHIN-COMMENT   =    "1" OR "2"
              IF   RECE41-SRYYM  >= "202008"
                 MOVE   7             TO  IDX-JIDO
                 PERFORM  200111-RECECOMMENT-IDXGET-SEC
                 MOVE   WRK-NIKOCHINTOU-YMD
                                      TO  WRK-RECECOM-YMD
                 PERFORM  200111-RECECOMMENT-HENSYU-SEC
              ELSE
                 ADD      1      TO  IDZ
                 MOVE    "初回算定　　　月　　日"
                                 TO  WK-FREE-NAME2(IDZ)(1:22)
                 MOVE    WRK-NIKOCHINTOU-YMD(5:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(11:4)
                 MOVE    WRK-NIKOCHINTOU-YMD(7:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(17:4)
                 MOVE    11      TO  WK-FREE-YUKOKETA2(IDZ)
              END-IF
           END-IF
           IF    RECE44-FLG-ZENSOKU-COMMENT    =    "1"
              IF   RECE41-SRYYM  >= "202008"
                 MOVE   8             TO  IDX-JIDO
                 PERFORM  200111-RECECOMMENT-IDXGET-SEC
                 MOVE   WRK-NIKOCHINTOU-YMD
                                      TO  WRK-RECECOM-YMD
                 PERFORM  200111-RECECOMMENT-HENSYU-SEC
              ELSE
                 ADD      1      TO  IDZ
                 MOVE    "第１回目治療管理　　　月　　日"
                                 TO  WK-FREE-NAME2(IDZ)(1:30)
                 MOVE    WRK-NIKOCHINTOU-YMD(5:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(19:4)
                 MOVE    WRK-NIKOCHINTOU-YMD(7:2)   TO  DAY-IDX2
                 MOVE    TBL-DAY(DAY-IDX2)
                                 TO  WK-FREE-NAME2(IDZ)(25:4)
                 MOVE    15      TO  WK-FREE-YUKOKETA2(IDZ)
              END-IF
           END-IF
      *
           PERFORM  200111-JIDOSET-HENSYU-SEC
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-NIKOCHINTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児特定疾患カウンセリング料（１回目）があるか判定
      *    二次性骨折予防継続管理料３があるか判定
      *    アレルギー性鼻炎免疫療法治療管理料があるか判定
      *    遠隔連携診療料（その他）があるか判定
      *    こころの連携指導料（１）があるか判定
      *    こころの連携指導料（２）があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-SYOUNI-HANTEI-SEC          SECTION.
      *
           IF  ((RECE44-FLG-SYOUNI-COMMENT   =    "1"       ) AND
                (RECE41-SRYYM               <=    "202003"  ))
           OR  ((RECE44-FLG-SYOUNI-COMMENT   =    "2"       ) AND
                (RECE41-SYOUNIFLG            =    SPACE     ) AND
                (RECE41-SRYYM               <=    "202003"  ))
           OR  ((RECE44-FLG-SYOUNI-COMMENT   =    "1" OR "2") AND
                (RECE41-SRYYM               >=    "202004"  ))
           OR  ( RECE44-FLG-NIJISEI-COMMENT NOT = SPACE      )
      *
             IF    RECE44-FLG-SYOUNI-COMMENT   =  "1" OR "2"
                 MOVE   CONS-SYOUNI    TO  WRK-SRYCD
             END-IF
      *
             EVALUATE  RECE44-FLG-NIJISEI-COMMENT
               WHEN      "1"
                 MOVE   CONS-NIJISEI   TO  WRK-SRYCD
               WHEN      "2"
                 MOVE   CONS-AREBI     TO  WRK-SRYCD
               WHEN      "3"
                 MOVE   CONS-ENREN2    TO  WRK-SRYCD
               WHEN      "4"
                 MOVE   CONS-COCO1     TO  WRK-SRYCD
               WHEN      "5"
                 MOVE   CONS-COCO2     TO  WRK-SRYCD
             END-EVALUATE
      *
             MOVE        ZERO        TO      FLG-SYOUNI-COMMENT
             MOVE        ZERO        TO      FLG-SYOUNI-COMMENT2
             MOVE        ZERO        TO      FLG-SYOUNI-COMMENT3
             MOVE        SPACE       TO      WRK-SYOUNI-FSANTEIYMD
             MOVE        SPACE       TO      WRK-SYOUNI-FSANTEIYMD2
             MOVE        SPACE       TO      WRK-SYOUNI-FSANTEIYMD3
      *
      *      算定履歴検索
             INITIALIZE                      SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"      TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM  UNTIL (FLG-SANTEI         =  1)
                         OR   (FLG-SYOUNI-COMMENT =  1)
                 IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                   MOVE  SANTEI-FSANTEIYMD
                                    TO  WRK-SYOUNI-FSANTEIYMD
                   MOVE     1       TO  FLG-SYOUNI-COMMENT
                 END-IF
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
      *
             IF   (RECE44-FLG-SYOUNI-COMMENT   =  "1" OR "2")
             AND  (RECE41-SRYYM               >=  "202406"  )
               MOVE    CONS-SYOUNI2        TO  WRK-SRYCD
      *        算定履歴検索
               INITIALIZE                      SANTEI-REC
               MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE41-PTID         TO  SANTEI-PTID
               MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM  UNTIL (FLG-SANTEI          =  1)
                           OR   (FLG-SYOUNI-COMMENT2 =  1)
                   IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM    TO
                                           WRK-SYOUNI-FSANTEIYMD2(1:6)
                     MOVE  SANTEI-MSANTEID TO
                                           WRK-SYOUNI-FSANTEIYMD2(7:2)
                     MOVE     1            TO  FLG-SYOUNI-COMMENT2
                   END-IF
                   MOVE    "tbl_santei"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
      *
               MOVE    CONS-SYOUNI3        TO  WRK-SRYCD
      *        算定履歴検索
               INITIALIZE                      SANTEI-REC
               MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE41-PTID         TO  SANTEI-PTID
               MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM  UNTIL (FLG-SANTEI          =  1)
                           OR   (FLG-SYOUNI-COMMENT3 =  1)
                   IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM    TO
                                           WRK-SYOUNI-FSANTEIYMD3(1:6)
                     MOVE  SANTEI-MSANTEID TO
                                           WRK-SYOUNI-FSANTEIYMD3(7:2)
                     MOVE     1            TO  FLG-SYOUNI-COMMENT3
                   END-IF
                   MOVE    "tbl_santei"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
      *
               IF     WRK-SYOUNI-FSANTEIYMD2  NOT = SPACE
               AND    WRK-SYOUNI-FSANTEIYMD3  NOT = SPACE
                 IF   WRK-SYOUNI-FSANTEIYMD2 > WRK-SYOUNI-FSANTEIYMD3
                   MOVE WRK-SYOUNI-FSANTEIYMD2 TO WRK-SYOUNI-FSANTEIYMD
                   MOVE     1                  TO FLG-SYOUNI-COMMENT
                 ELSE
                   MOVE WRK-SYOUNI-FSANTEIYMD3 TO WRK-SYOUNI-FSANTEIYMD
                   MOVE     1                  TO FLG-SYOUNI-COMMENT
                 END-IF
               ELSE
                 IF   WRK-SYOUNI-FSANTEIYMD2  NOT = SPACE
                   MOVE WRK-SYOUNI-FSANTEIYMD2 TO WRK-SYOUNI-FSANTEIYMD
                   MOVE     1                  TO FLG-SYOUNI-COMMENT
                 END-IF
                 IF   WRK-SYOUNI-FSANTEIYMD3  NOT = SPACE
                   MOVE WRK-SYOUNI-FSANTEIYMD3 TO WRK-SYOUNI-FSANTEIYMD
                   MOVE     1                  TO FLG-SYOUNI-COMMENT
                 END-IF
               END-IF
             END-IF
      *
             IF      FLG-SYOUNI-COMMENT    =      1
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 IF   RECE41-SRYYM  >= "202008"
                   MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                 ELSE
                   MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                   MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                 END-IF
                 MOVE       99      TO     FLG-COMMENT
                 IF    RECE44-FLG-SYOUNI-COMMENT  =  "1" OR "2"
                   PERFORM    20011-SYOUNI-COMMENT-SEC
                 ELSE
                   PERFORM    20011-SYOUNI-COMMENT2-SEC
                 END-IF
             END-IF
      *
           END-IF
      *
           .
       2001-SYOUNI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児特定疾患カウンセリング料初回算定日編集
      *****************************************************************
       20011-SYOUNI-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO  IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           IF    RECE41-SRYYM  >= "202008"
                 MOVE   9             TO  IDX-JIDO
                 PERFORM  200111-RECECOMMENT-IDXGET-SEC
                 MOVE   WRK-SYOUNI-FSANTEIYMD
                                      TO  WRK-RECECOM-YMD
                 PERFORM  200111-RECECOMMENT-HENSYU-SEC
           ELSE
            ADD         1       TO     IDZ
            IF   RECE41-SRYYM >= "201905"
             MOVE       23      TO     WK-FREE-YUKOKETA2(IDZ)
             MOVE   "第１回目カウンセリング　　　　　年　　月　　日"
                                TO     WK-FREE-NAME2(IDZ)
             MOVE       29      TO     IDX41
             MOVE       35      TO     IDX42
             MOVE       41      TO     IDX48
             MOVE       25      TO     IDX49
            ELSE
             MOVE       21      TO     WK-FREE-YUKOKETA2(IDZ)
             MOVE   "第１回目カウンセリング　　　年　　月　　日"
                                TO     WK-FREE-NAME2(IDZ)
             MOVE       25      TO     IDX41
             MOVE       31      TO     IDX42
             MOVE       37      TO     IDX48
            END-IF
      *
            INITIALIZE                         STS-AREA-DAY
            INITIALIZE                         LNK-DAY2-AREA
            MOVE    "21"                   TO  LNK-DAY2-IRAI
            MOVE    WRK-SYOUNI-FSANTEIYMD  TO  LNK-DAY2-YMD
            CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                           LNK-DAY2-AREA
            IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
              END-IF
      *
              MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                  TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
              END-IF
      *
              IF   RECE41-SRYYM >= "201905"
                 MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
              END-IF
            END-IF
           END-IF
      *
           PERFORM  200111-JIDOSET-HENSYU-SEC
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SYOUNI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他の初回算定日編集
      *****************************************************************
       20011-SYOUNI-COMMENT2-SEC          SECTION.
      *
           MOVE       ZERO       TO  IDZ
           INITIALIZE            WRK-FREE-AREA2
      *
           EVALUATE  RECE44-FLG-NIJISEI-COMMENT
             WHEN      "1"
               MOVE      119     TO  IDX-JIDO
             WHEN      "2"
               MOVE      120     TO  IDX-JIDO
             WHEN      "3"
               MOVE      128     TO  IDX-JIDO
             WHEN      "4"
               MOVE      129     TO  IDX-JIDO
             WHEN      "5"
               MOVE      130     TO  IDX-JIDO
           END-EVALUATE
      *
           PERFORM  200111-RECECOMMENT-IDXGET-SEC
           MOVE   WRK-SYOUNI-FSANTEIYMD
                                TO  WRK-RECECOM-YMD
           PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SYOUNI-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    傷病手当金意見書交付料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-SYOBYO-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-SYOBYO-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     99      TO     FLG-COMMENT
                  MOVE     5       TO     IDX-JIDO
                  PERFORM  200111-RECECOMMENT-IDXGET-SEC
                ELSE
                  MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                  MOVE     4       TO     FLG-COMMENT
                END-IF
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYOBYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養費同意書交付料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-RYOYO-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-RYOYO-COMMENT    =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     99      TO     FLG-COMMENT
                  MOVE     6       TO     IDX-JIDO
                  PERFORM  200111-RECECOMMENT-IDXGET-SEC
                ELSE
                  MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                  MOVE     4       TO     FLG-COMMENT
                END-IF
                PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-RYOYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    心臓ペースメーカー指導管理料（遠隔モニタリング）があるか判定
      *****************************************************************
       2001-SINPESU-HANTEI-SEC          SECTION.
      *
           IF   ((RECE41-SRYYM                 <  "201604") AND
                 (RECE44-FLG-SINPESU-COMMENT   =  "1"     ))
           OR   ((RECE41-SRYYM                >=  "201604") AND
                 (RECE41-SRYYM                 <  "201804") AND
                 (RECE44-FLG-SINPESU-COMMENT   =  "1"     ) AND
                 (RECE44-FLG-SINPESUR-COMMENT  =  "3"     ))
            IF    RECE41-SRYYM  >=  "201604"
             MOVE    ZERO                TO  FLG-SINPESU-COMMENT
             MOVE    SPACE               TO  WRK-SINPESU-YMD
             MOVE    CONS-SINPESU2       TO  WRK-SINPESU-SRYCD
             PERFORM   20011-SINPESU-CHECK-SEC
            ELSE
             MOVE    ZERO                TO  FLG-SINPESU-COMMENT
             MOVE    SPACE               TO  WRK-SINPESU-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    CONS-SINPESU        TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-SINPESU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESU-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESU-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
            END-IF
      *
            IF    WRK-SINPESU-YMD   NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                MOVE       1       TO     WRK-JIDO-SETCOMATFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                ADD        1       TO     IDZ
      *
                MOVE       13      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "前回算定年月　　　年　　月"
                                   TO     WK-FREE-NAME2(IDZ)
                MOVE       15      TO     IDX41
                MOVE       21      TO     IDX42
      *
                INITIALIZE                         STS-AREA-DAY
                INITIALIZE                         LNK-DAY2-AREA
                MOVE    "21"                   TO  LNK-DAY2-IRAI
                MOVE    WRK-SINPESU-YMD        TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
                END-IF
      *
                PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
            END-IF
           END-IF
      *
           .
       2001-SINPESU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    遠隔モニタリング加算があるか判定
      *****************************************************************
       20011-SINPESU-CHECK-SEC          SECTION.
      *
           INITIALIZE                        SRYACT-REC
           MOVE    RECE41-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    RECE41-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    RECE41-PTID           TO  SRY-PTID
           MOVE    RECE41-SRYYM          TO  SRY-SRYYM
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(1)
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(2)
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(3)
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(4)
           MOVE    WRK-SINPESU-SRYCD     TO  SRY-SRYCD(5)
           MOVE    SRYACT-REC            TO  MCPDATA-REC
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key23"               TO  MCP-PATHNAME
           PERFORM  900-DBSELECT-SEC
           IF       MCP-RC  =  ZERO
             MOVE      "tbl_sryact"      TO  MCP-TABLE
             MOVE      "key23"           TO  MCP-PATHNAME
             PERFORM    900-SRYACT-READ-SEC
             PERFORM  UNTIL   FLG-SRYACT           =   1
                       OR     FLG-SINPESU-COMMENT  =   1
      *
               MOVE     SRY-SRYYM        TO  WRK-SINPESU-YMD(1:6)
               MOVE     "01"             TO  WRK-SINPESU-YMD(7:2)
               MOVE       1              TO  FLG-SINPESU-COMMENT
      *
               MOVE    "tbl_sryact"      TO  MCP-TABLE
               MOVE    "key23"           TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_sryact"          TO  MCP-TABLE
           MOVE    "key23"               TO  MCP-PATHNAME
           PERFORM  900-CLOSE-SEC
      *
           .
       20011-SINPESU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    心臓ペースメーカー指導管理料（ロ）があるか判定
      *    遠隔モニタリング加算があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-SINPESUR-HANTEI-SEC          SECTION.
      *
           IF   ((RECE41-SRYYM                  <    "201604"  ) AND
                 (RECE44-FLG-SINPESUR-COMMENT   =    "1" OR "2"))
           OR   ((RECE41-SRYYM                 >=    "201604"  ) AND
                 (RECE44-FLG-SINPESUR-COMMENT   =    "1" OR "2") AND
                 (RECE44-FLG-SINPESU-COMMENT    =     SPACE    ))
           OR   ((RECE41-SRYYM                 >=    "201804"  ) AND
                 (RECE41-SRYYM                 <=    "202003"  ) AND
                 (RECE44-FLG-SINPESU-COMMENT    =    "1"       ) AND
                 (RECE44-FLG-SINPESUR-COMMENT   =    "3"       ))
           OR   ((RECE41-SRYYM                 >=    "202004"  ) AND
                 (RECE44-FLG-SINPESU-COMMENT    =    "4"       ) AND
                 (RECE44-FLG-SINPESUR-COMMENT   =    "4"       ))
           OR   ((RECE41-SRYYM                 >=    "202004"  ) AND
                 (RECE44-FLG-SINPESU-COMMENT    =    "5"       ) AND
                 (RECE44-FLG-SINPESUR-COMMENT   =    "5"       ))
           OR   ((RECE41-SRYYM                 >=    "201804"  ) AND
                 (RECE44-FLG-SINPESU-COMMENT    =    "A"       ) AND
                 (RECE44-FLG-SINPESUR-COMMENT   =    "A"       ))
           OR   ((RECE41-SRYYM                 >=    "201804"  ) AND
                 (RECE44-FLG-SINPESU-COMMENT    =    "B"       ) AND
                 (RECE44-FLG-SINPESUR-COMMENT   =    "B"       ) AND
                 (RECE44-FLG-YOUATU-COMMENT     =     SPACE    ))
             MOVE    ZERO                TO  FLG-SINPESUR-COMMENT
             MOVE    SPACE               TO  WRK-SINPESUR-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             EVALUATE  RECE44-FLG-SINPESUR-COMMENT
               WHEN     "1"
                 MOVE    CONS-SINPESUR1  TO  SANTEI-SRYCD
               WHEN     "2"
                 MOVE    CONS-SINPESUR2  TO  SANTEI-SRYCD
               WHEN     "3"
                 MOVE    CONS-SINPESUR3  TO  SANTEI-SRYCD
               WHEN     "4"
                 MOVE    CONS-SINPESUR4  TO  SANTEI-SRYCD
               WHEN     "5"
                 MOVE    CONS-SINPESUR5  TO  SANTEI-SRYCD
               WHEN     "A"
                 MOVE    CONS-ZAISAN     TO  SANTEI-SRYCD
               WHEN     "B"
                 MOVE    CONS-YOUATU2    TO  SANTEI-SRYCD
             END-EVALUATE
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI           =   1 )
                             OR   ( FLG-SINPESUR-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESUR-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESUR-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESUR-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF   RECE44-FLG-SINPESUR-COMMENT  =  "A"
               MOVE    ZERO              TO  FLG-SINPESUR-COMMENT2
               MOVE    SPACE             TO  WRK-SINPESUR-YMD2
               INITIALIZE                    SANTEI-REC
               MOVE    RECE41-HOSPNUM    TO  SANTEI-HOSPNUM
               MOVE    RECE41-PTID       TO  SANTEI-PTID
               MOVE    CONS-ZAISAN2      TO  SANTEI-SRYCD
               MOVE    ZERO              TO  SANTEI-NYUGAIKBN
               MOVE    ZERO              TO  SANTEI-SRYKA
               MOVE    ZERO              TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_santei"      TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC            =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI            =  1 )
                             OR   ( FLG-SINPESUR-COMMENT2 =  1 )
      *
                   IF   SANTEI-SRYYM  <  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESUR-YMD2(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESUR-YMD2(7:2)
                     MOVE    1              TO  FLG-SINPESUR-COMMENT2
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"      TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               IF   WRK-SINPESUR-YMD2  NOT =  SPACE
                 IF  ( WRK-SINPESUR-YMD      =  SPACE          )
                 OR  ((WRK-SINPESUR-YMD  NOT =  SPACE         ) AND
                      (WRK-SINPESUR-YMD  <   WRK-SINPESUR-YMD2))
                   MOVE  WRK-SINPESUR-YMD2  TO  WRK-SINPESUR-YMD
                 END-IF
               END-IF
             END-IF
      *
             IF   WRK-SINPESUR-YMD   NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                ELSE
                  MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                END-IF
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                IF   RECE41-SRYYM  >= "202008"
                 EVALUATE  RECE44-FLG-SINPESUR-COMMENT
                  WHEN     "2"
                     MOVE   20            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-SINPESUR-YMD
                                          TO  WRK-RECECOM-YMD
                     PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  WHEN     "4"
                     MOVE   21            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-SINPESUR-YMD(1:6)
                                          TO  WRK-RECECOM-YMD(1:6)
                     MOVE   "00"          TO  WRK-RECECOM-YMD(7:2)
                     PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  WHEN     "5"
                     MOVE   23            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-SINPESUR-YMD(1:6)
                                          TO  WRK-RECECOM-YMD(1:6)
                     MOVE   "00"          TO  WRK-RECECOM-YMD(7:2)
                     PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  WHEN     "A"
                     MOVE   25            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-SINPESUR-YMD(1:6)
                                          TO  WRK-RECECOM-YMD(1:6)
                     MOVE   "00"          TO  WRK-RECECOM-YMD(7:2)
                     PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  WHEN     "B"
                     MOVE   27            TO  IDX-JIDO
                     PERFORM  200111-RECECOMMENT-IDXGET-SEC
                     MOVE   WRK-SINPESUR-YMD(1:6)
                                          TO  WRK-RECECOM-YMD(1:6)
                     MOVE   "00"          TO  WRK-RECECOM-YMD(7:2)
                     PERFORM  200111-RECECOMMENT-HENSYU-SEC
                 END-EVALUATE
                ELSE
                 ADD        1        TO     IDZ
                 EVALUATE  RECE44-FLG-SINPESUR-COMMENT
                  WHEN     "1"
                  WHEN     "2"
                     IF   RECE41-SRYYM >= "201905"
                       MOVE    15    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "前回算定年月　　　　　年　　月"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    19    TO     IDX41
                       MOVE    25    TO     IDX42
                       MOVE    15    TO     IDX49
                     ELSE
                       MOVE    13    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "前回算定年月　　　年　　月"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    15    TO     IDX41
                       MOVE    21    TO     IDX42
                     END-IF
                  WHEN     "3"
                  WHEN     "4"
                  WHEN     "5"
                  WHEN     "A"
                  WHEN     "B"
                     IF   RECE41-SRYYM >= "201905"
                       MOVE    20    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "指導管理料前回算定年月　　　　　年　　月"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    29    TO     IDX41
                       MOVE    35    TO     IDX42
                       MOVE    25    TO     IDX49
                     ELSE
                       MOVE    18    TO     WK-FREE-YUKOKETA2(IDZ)
                       MOVE   "指導管理料前回算定年月　　　年　　月"
                                     TO     WK-FREE-NAME2(IDZ)
                       MOVE    25    TO     IDX41
                       MOVE    31    TO     IDX42
                     END-IF
                 END-EVALUATE
      *
                 INITIALIZE                         STS-AREA-DAY
                 INITIALIZE                         LNK-DAY2-AREA
                 MOVE    "21"                   TO  LNK-DAY2-IRAI
                 MOVE    WRK-SINPESUR-YMD       TO  LNK-DAY2-YMD
                 CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                                LNK-DAY2-AREA
                 IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                        TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   IF   RECE41-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                        TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                 END-IF
                END-IF
      *
                PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-SINPESUR-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    がん患者指導管理料２・３があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-GANSIDO-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-GANSIDO-COMMENT   =    "1" OR "2"
      *
             MOVE   ZERO   TO  IDX27
             INITIALIZE    WRK-GANSIDO-YMD-AREA
             EVALUATE    RECE44-FLG-GANSIDO-COMMENT
               WHEN       "1"
                 MOVE      CONS-GANSIDO1   TO  WRK-SRYCD
               WHEN       "2"
                 MOVE      CONS-GANSIDO2   TO  WRK-SRYCD
             END-EVALUATE
             PERFORM  2001-GANSIDO-HENSYU-SEC
      *
             IF   RECE41-SRYYM  >= "202204"
               EVALUATE  RECE44-FLG-GANSIDO-COMMENT
                 WHEN     "1"
                   MOVE    CONS-GANSIDO3   TO  WRK-SRYCD
                 WHEN     "2"
                   MOVE    CONS-GANSIDO4   TO  WRK-SRYCD
               END-EVALUATE
               PERFORM  2001-GANSIDO-HENSYU-SEC
             END-IF
      *
             IF   WRK-GANSIDO-YMD(1) NOT =  SPACE
      *
                IF   RECE41-SRYYM  >= "202204"
                  PERFORM   VARYING  IDX26  FROM  1  BY  1
                             UNTIL  (IDX26                  >   10  )
                              OR    (WRK-GANSIDO-YMD(IDX26) =  SPACE)
                   COMPUTE  IDZ  =  IDX26  +  1
                   PERFORM  VARYING  IDX27  FROM  IDZ  BY  1
                             UNTIL  (IDX27                  >   10  )
                              OR    (WRK-GANSIDO-YMD(IDX27) =  SPACE)
                     IF WRK-GANSIDO-YMD(IDX26) < WRK-GANSIDO-YMD(IDX27)
                       MOVE  WRK-GANSIDO-YMD(IDX26)
                                              TO WRK-GANSIDO-DATA
                       MOVE  WRK-GANSIDO-YMD(IDX27)
                                              TO WRK-GANSIDO-YMD(IDX26)
                       MOVE  WRK-GANSIDO-DATA TO WRK-GANSIDO-YMD(IDX27)
                     END-IF
                   END-PERFORM
                  END-PERFORM
                END-IF
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                ELSE
      ***         MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
      ***         MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                  CONTINUE
                END-IF
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                PERFORM  VARYING  IDX26  FROM  10  BY  -1
                          UNTIL   IDX26   <     1
                 IF  WRK-GANSIDO-YMD(IDX26)  NOT =  SPACE
                  IF   RECE41-SRYYM  >= "202008"
                   EVALUATE  RECE44-FLG-GANSIDO-COMMENT
                     WHEN      "1"
                       MOVE   29            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-GANSIDO-YMD(IDX26)
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                     WHEN      "2"
                       MOVE   30            TO  IDX-JIDO
                       PERFORM  200111-RECECOMMENT-IDXGET-SEC
                       MOVE   WRK-GANSIDO-YMD(IDX26)
                                            TO  WRK-RECECOM-YMD
                       PERFORM  200111-RECECOMMENT-HENSYU-SEC
                   END-EVALUATE
                   PERFORM  200111-JIDOSET-HENSYU-SEC
                  ELSE
                   ADD         1       TO     IDZ
                   IF   WRK-GANSIDO-YMD(IDX26)(1:6) = RECE41-SRYYM
                    IF   RECE41-SRYYM >= "201905"
                      MOVE     17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "当月算定日　　　　　年　　月　　日"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     17      TO     IDX41
                      MOVE     23      TO     IDX42
                      MOVE     29      TO     IDX48
                      MOVE     13      TO     IDX49
                    ELSE
                      MOVE     15      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "当月算定日　　　年　　月　　日"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     13      TO     IDX41
                      MOVE     19      TO     IDX42
                      MOVE     25      TO     IDX48
                    END-IF
                   ELSE
                    IF   RECE41-SRYYM >= "201905"
                      MOVE     17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "過去算定日　　　　　年　　月　　日"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     17      TO     IDX41
                      MOVE     23      TO     IDX42
                      MOVE     29      TO     IDX48
                      MOVE     13      TO     IDX49
                    ELSE
                      MOVE     15      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "過去算定日　　　年　　月　　日"
                                       TO     WK-FREE-NAME2(IDZ)
                      MOVE     13      TO     IDX41
                      MOVE     19      TO     IDX42
                      MOVE     25      TO     IDX48
                    END-IF
                   END-IF
      *
                   INITIALIZE                         STS-AREA-DAY
                   INITIALIZE                         LNK-DAY2-AREA
                   MOVE    "21"                   TO  LNK-DAY2-IRAI
                   MOVE    WRK-GANSIDO-YMD(IDX26) TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                                  LNK-DAY2-AREA
                   IF      STS-DAY-RC1            =   ZERO
                     MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                     END-IF
      *
                     MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                     END-IF
      *
                     MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                     INITIALIZE                       ORCSKANACHKAREA
                     MOVE    "2"                  TO  KANACHK-SYORI
                     MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                     CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                     IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                     END-IF
      *
                     IF   RECE41-SRYYM >= "201905"
                        MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                     END-IF
                   END-IF
                   PERFORM  200111-JIDOSET-HENSYU-SEC
                  END-IF
                 END-IF
                END-PERFORM
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-GANSIDO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       2001-GANSIDO-HENSYU-SEC          SECTION.
      *
           INITIALIZE                      SANTEI-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM   UNTIL   FLG-SANTEI  =   1
      *
      ***>       IF   SANTEI-SRYYM  <= RECE41-SRYYM
                 IF   SANTEI-SRYYM  <  RECE41-SRYYM
                   PERFORM  VARYING  IDX26  FROM  31  BY  -1
                             UNTIL   IDX26   <     1
                     IF  (SANTEI-DAY(IDX26)  NOT =  ZERO)
                     AND (IDX27              <    10    )
                       ADD   1      TO  IDX27
                       MOVE  SANTEI-SRYYM
                                    TO  WRK-GANSIDO-YMD(IDX27)(1:6)
                       MOVE  IDX26  TO  WRK-GANSIDO-YMD(IDX27)(7:2)
                     END-IF
                   END-PERFORM
                 END-IF
      *
                 MOVE    "tbl_santei"  TO  MCP-TABLE
                 MOVE    "key3"        TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-GANSIDO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    乳腺炎重症化予防ケア・指導料（初回）があるか判定
      *    乳腺炎重症化予防ケア・指導料（２回目から４回目まで）があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-NYUSEN-HANTEI-SEC          SECTION.
      *
           IF  ((RECE44-FLG-NYUSEN-COMMENT   =    "1"       ) AND
                (RECE41-NYUSENFLG            =    SPACE     ) AND
                (RECE41-SRYYM               <=    "202003"  ))
           OR  ((RECE44-FLG-NYUSEN-COMMENT   =    "2"       ) AND
                (RECE41-SRYYM               <=    "202003"  ))
           OR  ((RECE44-FLG-NYUSEN-COMMENT   =    "1" OR "2") AND
                (RECE41-SRYYM               >=    "202004"  ))
             MOVE   CONS-NYUSEN   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             MOVE       ZERO                TO  WRK-ZAIKYO-KAISU
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
               MOVE      1                  TO  WRK-ZAIKYO-KAISU
               IF   RECE44-FLG-NYUSEN-COMMENT  =  "2"
                 MOVE    CONS-NYUSEN2       TO  WRK-NYUSEN-SRYCD
                 PERFORM 20011-NYUSEN-KAISUCNT-SEC
               END-IF
             END-IF
      *
             IF  ((RECE41-SRYYM      >= "202008") AND
                  (WRK-ZAIKYO-KAISU  NOT =  ZERO))
             OR  ((RECE41-SRYYM      <  "202008") AND
                  (WRK-ZAIKYO-KAISU  NOT =  ZERO) AND
                  (WRK-ZAIKYO-KAISU     <=   31 ))
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                ELSE
                  MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                END-IF
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                IF   RECE41-SRYYM  >= "202008"
                  MOVE   31            TO  IDX-JIDO
                  PERFORM  200111-RECECOMMENT-IDXGET-SEC
                  MOVE   WRK-ZAIKYO-KAISU
                                       TO  WRK-RECECOM-NUM
                  PERFORM  200111-RECECOMMENT-HENSYU-SEC
                ELSE
                  ADD      1       TO     IDZ
                  MOVE     11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "通算実施回数（　　回）"
                                   TO     WK-FREE-NAME2(IDZ)
                  MOVE    WRK-ZAIKYO-KAISU TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(15:4)
                END-IF
      *
                PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-NYUSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    乳腺炎重症化予防ケア・指導料２（初回）があるか判定
      *    乳腺炎重症化予防ケア・指導料２（２回目から８回目まで）
      *    があるか判定
      *  ＃202406選択式コメント対応
      *****************************************************************
       2001-NYUSEN2-HANTEI-SEC          SECTION.
      *
           IF    RECE44-FLG-NYUSEN2-COMMENT  =  "1" OR "2"
             MOVE   CONS-NYUSEN3  TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             MOVE       ZERO                TO  WRK-ZAIKYO-KAISU
             IF   WRK-ZAIKYO-YMD    NOT =  SPACE
               MOVE      1                  TO  WRK-ZAIKYO-KAISU
               IF   RECE44-FLG-NYUSEN2-COMMENT  =  "2"
                 MOVE    CONS-NYUSEN4       TO  WRK-NYUSEN-SRYCD
                 PERFORM 20011-NYUSEN-KAISUCNT-SEC
               END-IF
             END-IF
      *
             IF   WRK-ZAIKYO-KAISU  NOT =  ZERO
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "2"      TO     WRK-JIDO-SETCOMFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                MOVE   124                TO  IDX-JIDO
                PERFORM  200111-RECECOMMENT-IDXGET-SEC
                MOVE   WRK-ZAIKYO-KAISU   TO  WRK-RECECOM-NUM
                PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
             END-IF
           END-IF
      *
           .
       2001-NYUSEN2-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    乳腺炎重症化予防ケア・指導料の回数カウント
      *****************************************************************
       20011-NYUSEN-KAISUCNT-SEC          SECTION.
      *
           INITIALIZE                      SANTEI-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-NYUSEN-SRYCD    TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
             MOVE    "tbl_santei"      TO  MCP-TABLE
             MOVE    "key3"            TO  MCP-PATHNAME
             PERFORM  900-SANTEI-READ-SEC
             PERFORM   UNTIL   FLG-SANTEI  =   1
               IF   SANTEI-SRYYM  <=  RECE41-SRYYM
               AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                 MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                 MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                 PERFORM  VARYING  IDX26  FROM  31  BY  -1
                           UNTIL   IDX26   <     1
                   IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                     IF ((SANTEI-SRYYM = WRK-ZAIKYO-YMD(1:6) ) AND
                         (IDX26        < IDX28               ))
                     OR ((SANTEI-SRYYM = WRK-ZAIKYO-YMD9(1:6)) AND
                         (IDX26       >= IDX29               ))
                       CONTINUE
                     ELSE
                       ADD    1    TO   WRK-ZAIKYO-KAISU
                     END-IF
                   END-IF
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
             END-PERFORM
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20011-NYUSEN-KAISUCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    集団栄養食事指導料があるか判定
      *****************************************************************
       2001-SYUEIYO-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-SYUEIYO-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       1       TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SYUEIYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訪問看護指示料があるか判定
      *  ＃202004選択式コメント対応
      *****************************************************************
       2001-HOUSIJI-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-HOUSIJI-COMMENT   =    "1"
           AND    RECE44-FLG-HOUSKSN-COMMENT   =    SPACE
               MOVE      ZERO     TO     IDZ
               INITIALIZE                WRK-FREE-AREA2
               INITIALIZE                WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       1       TO     WRK-JIDO-SETCOMATFLG
               IF   RECE41-SRYYM  >= "202004"
                 MOVE    "1"      TO     WRK-JIDO-RECEDENFLG
               END-IF
               MOVE       3       TO     FLG-COMMENT
               PERFORM   200111-SANTEI-DAY-SEC
               PERFORM   20011-FREE-COMMENT2-SEC
      *
               IF   RECE44-FLG-TEJHOU-COMMENT  =  "1"
                 MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                 PERFORM   2001-TEJHOU-HENSYU-SEC
               END-IF
      *
               PERFORM   200111-SRYACTINF-HENSYU-MAT-SEC
           END-IF
      *
           .
       2001-HOUSIJI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    特別訪問看護指示加算があるか判定
      *  ＃202204コメント対応
      *****************************************************************
       2001-HOUSKSN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-HOUSKSN-COMMENT   =    "1"
               MOVE      ZERO     TO     IDZ
               INITIALIZE                WRK-FREE-AREA2
               INITIALIZE                WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       2       TO     WRK-JIDO-SETCOMATFLG
               MOVE       3       TO     FLG-COMMENT
               PERFORM   200111-SANTEI-DAY-SEC
               PERFORM   20011-FREE-COMMENT2-SEC
      *
               IF   RECE44-FLG-TEJHOU-COMMENT  =  "1"
                 MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                 PERFORM   2001-TEJHOU-HENSYU-SEC
               END-IF
      *
               PERFORM   200111-SRYACTINF-HENSYU-MAT-SEC
           END-IF
      *
           .
       2001-HOUSKSN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手順書加算（訪問看護指示料）があるか判定
      *  ＃202204コメント対応
      *****************************************************************
       2001-TEJHOU-HANTEI-SEC          SECTION.
      *
           IF   ((RECE44-FLG-HOUSIJI-COMMENT   =    SPACE) AND
                 (RECE44-FLG-HOUSKSN-COMMENT   =    SPACE) AND
                 (RECE44-FLG-TEJHOU-COMMENT    =    "1"  ))
           OR   ((RECE44-FLG-HOUSKSN-COMMENT   =    "2"  ) AND
                 (RECE44-FLG-TEJHOU-COMMENT    =    "1"  ))
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             MOVE      "2"      TO     WRK-JIDO-SETCOMFLG
             MOVE       99      TO     FLG-COMMENT
      *
             MOVE      ZERO     TO     IDZ
             INITIALIZE                WRK-FREE-AREA2
      *
             PERFORM   2001-TEJHOU-HENSYU-SEC
      *
             PERFORM   200111-SRYACTINF-HENSYU-MAT-SEC
           END-IF
      *
           .
       2001-TEJHOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手順書加算（訪問看護指示料）編集
      *  ＃202204コメント対応
      *****************************************************************
       2001-TEJHOU-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TEJHOU-COMMENT
           MOVE    SPACE               TO  WRK-TEJHOU-YMD
           INITIALIZE                      SANTEI-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    CONS-TEJHOU         TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                           OR   ( FLG-TEJHOU-COMMENT  =   1 )
      *
                 IF   SANTEI-SRYYM  <  RECE41-SRYYM
                   MOVE  SANTEI-SRYYM     TO  WRK-TEJHOU-YMD(1:6)
                   MOVE  SANTEI-MSANTEID  TO  WRK-TEJHOU-YMD(7:2)
                   MOVE    1              TO  FLG-TEJHOU-COMMENT
                 END-IF
      *
                 MOVE    "tbl_santei"  TO  MCP-TABLE
                 MOVE    "key3"        TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF   WRK-TEJHOU-YMD   NOT =  SPACE
             MOVE     137              TO  IDX-JIDO
             PERFORM  200111-RECECOMMENT-IDXGET-SEC
             MOVE     WRK-TEJHOU-YMD   TO  WRK-RECECOM-YMD
             PERFORM  200111-RECECOMMENT-HENSYU-SEC
           ELSE
             MOVE     637              TO  IDX-JIDO
             PERFORM  200111-RECECOMMENT-IDXGET-SEC
             PERFORM  200111-RECECOMMENT-HENSYU-SEC
           END-IF
      *
           .
       2001-TEJHOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅血液透析頻回指導管理料があるか判定
      *    在宅血液透析指導管理料算定日の歴月で２月以内か判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-HINKAI-HANTEI-SEC          SECTION.
      *
           IF   RECE44-FLG-HINKAI-COMMENT   =    "1"
      *
             MOVE    ZERO    TO   WRK-DAY
             PERFORM  VARYING   IDX3   FROM  1  BY  1
                       UNTIL    IDX3  >   31
                 IF   RECE44-KAISU(1 IDX3)  NOT =  ZERO
                        MOVE  IDX3  TO  WRK-DAY
                        MOVE  31    TO  IDX3
                 END-IF
             END-PERFORM
      *
             MOVE     ZERO     TO     FLG-HINKAI-COMMENT
             MOVE     ZERO     TO     WRK-TOSEKIYMD
      *
      *      算定履歴検索
             INITIALIZE                  SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    CONS-TOSEKI         TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                    MOVE    "tbl_santei" TO  MCP-TABLE
                    MOVE    "key3"       TO  MCP-PATHNAME
                    PERFORM 900-SANTEI-READ-SEC
                    PERFORM   UNTIL  ( FLG-SANTEI         =   1 ) OR
                                     ( FLG-HINKAI-COMMENT =   1 )
      *
                      IF   SANTEI-SRYYM  <=  RECE41-SRYYM
      ***              MOVE   SANTEI-SRYYM     TO  WRK-FSANTEIYMD(1:6)
      ***              MOVE   SANTEI-MSANTEID  TO  WRK-FSANTEIYMD(7:2)
                       MOVE   SANTEI-FSANTEIYMD
                                               TO  WRK-FSANTEIYMD
      *
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    WRK-FSANTEIYMD  TO  LNK-DAY6-KIJUN
                       MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                       MOVE     2              TO  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN TO  WRK-AFTER2
      *
                       MOVE    RECE41-SRYYM    TO  WRK-HINKAIYMD(1:6)
                       MOVE    WRK-DAY         TO  WRK-HINKAIYMD(7:2)
      *
                       IF     WRK-DAY      NOT  =  ZERO
                       AND    WRK-FSANTEIYMD   <=  WRK-HINKAIYMD
                         IF ((WRK-HINKAIYMD    <   WRK-AFTER2) AND
                             (RECE41-SRYYM     <= "201003"   ))
                         OR ( RECE41-SRYYM     >= "201004"    )
                             MOVE    1    TO   FLG-HINKAI-COMMENT
                             MOVE  WRK-FSANTEIYMD
                                          TO   WRK-TOSEKIYMD
                         END-IF
                       END-IF
                      END-IF
      * 
                      MOVE    "tbl_santei"     TO  MCP-TABLE
                      MOVE    "key3"           TO  MCP-PATHNAME
                      PERFORM 900-SANTEI-READ-SEC
                    END-PERFORM
             END-IF
      *
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             IF      FLG-HINKAI-COMMENT    =      1
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 IF   RECE41-SRYYM  >= "202008"
                   MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                 ELSE
                   MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                   MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                 END-IF
                 MOVE       99      TO     FLG-COMMENT
                 PERFORM    20011-HINKAI-COMMENT-SEC
             END-IF
      *
           END-IF
      *
           .
       2001-HINKAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅血液透析頻回指導管理料のコメント編集
      *****************************************************************
       20011-HINKAI-COMMENT-SEC          SECTION.
      *
           PERFORM    200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *    コメントの挿入
           MOVE    RECE44-MEISAISU      TO  MAX-MEISAISU
      *
           IF   RECE41-SRYYM  >= "202008"
              MOVE   ZERO          TO   IDZ
              INITIALIZE           WRK-FREE-AREA2
              MOVE   34            TO  IDX-JIDO
              PERFORM  200111-RECECOMMENT-IDXGET-SEC
              MOVE   WRK-TOSEKIYMD TO  WRK-RECECOM-YMD
              PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
              COMPUTE  MAX-MEISAISU  =   MAX-MEISAISU  +  1
              MOVE  WK-FREE-SRYCD2(1)
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
              MOVE  ZERO    TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
              MOVE  WK-FREE-YUKOKETA2(1)
                            TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  WK-FREE-NAME2(1)
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
              MOVE  WK-FREE-INPUTCOMENT2(1)
                            TO  WRK-RECE45XX-INPUTCOMENT(MAX-MEISAISU 1)
              MOVE  "1"     TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           ELSE
              COMPUTE  MAX-MEISAISU  =   MAX-MEISAISU  +  1
              MOVE  "810000001"
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
              MOVE  ZERO    TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
              MOVE  16      TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
              MOVE  "初回指導管理算定日　　　月　　日"
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
              MOVE  WRK-TOSEKIYMD(5:2)   TO  DAY-IDX2
              MOVE  TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(21:4)
              MOVE  WRK-TOSEKIYMD(7:2)   TO  DAY-IDX2
              MOVE  TBL-DAY(DAY-IDX2)
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(27:4)
              MOVE  "2"     TO  WRK-RECE45XX-MEISKYFLG(MAX-MEISAISU 1)
      *#      MOVE   12     TO  WRK-RECE45XX-SETCOMATFLG(MAX-MEISAISU 1)
              MOVE   1      TO  WRK-RECE45XX-SETCOMATFLG(MAX-MEISAISU 1)
              MOVE  SPACE   TO  WRK-RECE45XX-RECEDENFLG(MAX-MEISAISU 1)
              MOVE  "1"     TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HINKAI-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    認知症専門診断管理料２があるか判定
      *    小児運動器疾患指導管理料があるか判定
      *    療養・就労両立支援指導料があるか判定
      *    認知症サポート指導料があるか判定
      *    一般不妊治療管理料があるか判定があるか判定
      *    認知症専門診断管理料２（連携型）があるか判定
      *    遠隔連携診療料（診断を目的）があるか判定
      *    手順書加算（精神科訪問看護指示料）があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-NINSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-NINSEN-COMMENT   NOT =   SPACE
             MOVE    ZERO                TO  FLG-SINPESU-COMMENT
             MOVE    SPACE               TO  WRK-SINPESU-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             EVALUATE  RECE44-FLG-NINSEN-COMMENT
               WHEN      "1"
                 MOVE  CONS-NINSEN       TO  SANTEI-SRYCD
               WHEN      "2"
                 MOVE  CONS-SYOUNDO      TO  SANTEI-SRYCD
               WHEN      "3"
                 MOVE  CONS-RYOSYU       TO  SANTEI-SRYCD
               WHEN      "4"
                 MOVE  CONS-NINSAPO      TO  SANTEI-SRYCD
               WHEN      "5"
                 MOVE  CONS-FUNIN        TO  SANTEI-SRYCD
               WHEN      "6"
                 MOVE  CONS-NINSEN2      TO  SANTEI-SRYCD
               WHEN      "7"
                 MOVE  CONS-ENREN1       TO  SANTEI-SRYCD
               WHEN      "8"
                 MOVE  CONS-TEJSEI       TO  SANTEI-SRYCD
             END-EVALUATE
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-SINPESU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESU-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESU-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             IF   RECE41-SRYYM  >= "202008"
               MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
             ELSE
               MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE     1       TO     WRK-JIDO-SETCOMATFLG
             END-IF
             MOVE       99      TO     FLG-COMMENT
      *
             MOVE      ZERO     TO     IDZ
             INITIALIZE                WRK-FREE-AREA2
      *
             IF   RECE41-SRYYM  >= "202008"
              EVALUATE  RECE44-FLG-NINSEN-COMMENT
                WHEN    "1"
                  IF   WRK-SINPESU-YMD   NOT =  SPACE
                    MOVE   11            TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    MOVE   WRK-SINPESU-YMD
                                         TO  WRK-RECECOM-YMD
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  ELSE
                    MOVE   511           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-IF
                WHEN    "2"
                  IF   WRK-SINPESU-YMD   NOT =  SPACE
                    MOVE   12            TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    MOVE   WRK-SINPESU-YMD(1:6)
                                         TO  WRK-RECECOM-YMD(1:6)
                    MOVE   "00"          TO  WRK-RECECOM-YMD(7:2)
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  ELSE
                    MOVE   512           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-IF
                WHEN    "4"
                  IF   WRK-SINPESU-YMD   NOT =  SPACE
                    MOVE   13            TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    MOVE   WRK-SINPESU-YMD(1:6)
                                         TO  WRK-RECECOM-YMD(1:6)
                    MOVE   "00"          TO  WRK-RECECOM-YMD(7:2)
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  ELSE
                    MOVE   513           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-IF
                WHEN    "5"
                  IF   WRK-SINPESU-YMD   NOT =  SPACE
                    MOVE   118           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    MOVE   WRK-SINPESU-YMD(1:6)
                                         TO  WRK-RECECOM-YMD(1:6)
                    MOVE   "00"          TO  WRK-RECECOM-YMD(7:2)
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  ELSE
                    MOVE   618           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-IF
                WHEN    "6"
                  IF   WRK-SINPESU-YMD   NOT =  SPACE
                    MOVE   134           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    MOVE   WRK-SINPESU-YMD
                                         TO  WRK-RECECOM-YMD
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  ELSE
                    MOVE   634           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-IF
                WHEN    "7"
                  IF   WRK-SINPESU-YMD   NOT =  SPACE
                    MOVE   127           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    MOVE   WRK-SINPESU-YMD
                                         TO  WRK-RECECOM-YMD
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                    MOVE   627           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-IF
                WHEN    "8"
                  IF   WRK-SINPESU-YMD   NOT =  SPACE
                    MOVE   132           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    MOVE   WRK-SINPESU-YMD
                                         TO  WRK-RECECOM-YMD
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  ELSE
                    MOVE   632           TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-IF
              END-EVALUATE
             ELSE
              IF   WRK-SINPESU-YMD   NOT =  SPACE
                ADD        1       TO     IDZ
                EVALUATE  RECE44-FLG-NINSEN-COMMENT
                  WHEN    "1"
                    IF   RECE41-SRYYM >= "201905"
                      MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "前回算定年月日　　　　　年　　月　　日"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       21      TO     IDX41
                      MOVE       27      TO     IDX42
                      MOVE       33      TO     IDX48
                      MOVE       17      TO     IDX49
                    ELSE
                      MOVE       17      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "前回算定年月日　　　年　　月　　日"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       17      TO     IDX41
                      MOVE       23      TO     IDX42
                      MOVE       29      TO     IDX48
                    END-IF
      *
                    INITIALIZE                      STS-AREA-DAY
                    INITIALIZE                      LNK-DAY2-AREA
                    MOVE    "21"                TO  LNK-DAY2-IRAI
                    MOVE    WRK-SINPESU-YMD     TO  LNK-DAY2-YMD
                    CALL    "ORCSDAY"    USING      STS-AREA-DAY
                                                    LNK-DAY2-AREA
                    IF      STS-DAY-RC1  =  ZERO
                      MOVE  LNK-DAY2-EDTYMD1-Y  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                      END-IF
      *
                      MOVE  LNK-DAY2-EDTYMD1-M  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                      END-IF
      *
                      MOVE  LNK-DAY2-EDTYMD1-D  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                      END-IF
      *
                      IF   RECE41-SRYYM >= "201905"
                        MOVE  LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                      END-IF
                    END-IF
                  WHEN    "2"
                  WHEN    "3"
                  WHEN    "4"
                    IF   RECE41-SRYYM >= "201905"
                      MOVE       15      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "前回算定年月　　　　　年　　月"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       19      TO     IDX41
                      MOVE       25      TO     IDX42
                      MOVE       15      TO     IDX49
                    ELSE
                      MOVE       13      TO     WK-FREE-YUKOKETA2(IDZ)
                      MOVE   "前回算定年月　　　年　　月"
                                         TO     WK-FREE-NAME2(IDZ)
                      MOVE       15      TO     IDX41
                      MOVE       21      TO     IDX42
                    END-IF
      *
                    INITIALIZE                      STS-AREA-DAY
                    INITIALIZE                      LNK-DAY2-AREA
                    MOVE    "21"                TO  LNK-DAY2-IRAI
                    MOVE    WRK-SINPESU-YMD     TO  LNK-DAY2-YMD
                    CALL    "ORCSDAY"    USING      STS-AREA-DAY
                                                    LNK-DAY2-AREA
                    IF      STS-DAY-RC1  =  ZERO
                      MOVE  LNK-DAY2-EDTYMD1-Y  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                      END-IF
      *
                      MOVE  LNK-DAY2-EDTYMD1-M  TO  WRK-TAIHI-XX
                      INITIALIZE                    ORCSKANACHKAREA
                      MOVE  "2"                 TO  KANACHK-SYORI
                      MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                      CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                      IF    KANACHK-RC  =  ZERO
                        MOVE  KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                      END-IF
      *
                      IF   RECE41-SRYYM >= "201905"
                        MOVE  LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                      END-IF
                    END-IF
                END-EVALUATE
              ELSE
                ADD        1       TO     IDZ
                MOVE       6       TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "（初回算定）"  TO     WK-FREE-NAME2(IDZ)
              END-IF
              PERFORM 200111-JIDOSET-HENSYU-SEC
             END-IF
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-NINSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    退院時共同指導料特別管理指導加算があるか判定
      *  ＃202004選択式コメント対応
      *****************************************************************
       2001-TAKYOKSN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-TAKYOKSN-COMMENT   =    "1"
           AND    FLG-TAKYO-HENSYU              =     1 
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       1       TO     WRK-JIDO-SETCOMATFLG
               IF   RECE41-SRYYM  >= "202004"
                 MOVE    "1"      TO     WRK-JIDO-RECEDENFLG
               END-IF
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-TAKYOKSN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来リハビリテーション診療料・外来放射線照射診療料があるか判定
      *  ＃202004選択式コメント対応
      *****************************************************************
       2001-GAIRIHA-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-GAIRIHA-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       1       TO     WRK-JIDO-SETCOMATFLG
               IF   RECE41-SRYYM  >= "202004"
                 MOVE    "1"      TO     WRK-JIDO-RECEDENFLG
               END-IF
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-GAIRIHA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    人工内耳機器調整加算があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-JINNAI-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-JINNAI-COMMENT   NOT =   SPACE
             MOVE      ZERO                TO  FLG-JINNAI-COMMENT
             MOVE      SPACE               TO  WRK-JINNAI-YMD
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL   (IDX                 >  2)
                         OR     (FLG-JINNAI-COMMENT  =  1)
               INITIALIZE                      SANTEI-REC
               MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE41-PTID         TO  SANTEI-PTID
               EVALUATE  RECE44-FLG-JINNAI-COMMENT
                 WHEN      "1"
                       MOVE  CONS-JINNAI1  TO  SANTEI-SRYCD
                 WHEN      "2"
                   EVALUATE  IDX
                     WHEN     1
                       MOVE  CONS-JINNAI2  TO  SANTEI-SRYCD
                     WHEN     2
                       MOVE  CONS-JINNAI1  TO  SANTEI-SRYCD
                   END-EVALUATE
               END-EVALUATE
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-JINNAI-COMMENT  =   1 )
                   IF   SANTEI-SRYYM  <  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-JINNAI-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-JINNAI-YMD(7:2)
                     MOVE    1              TO  FLG-JINNAI-COMMENT
                   END-IF
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               EVALUATE  RECE44-FLG-JINNAI-COMMENT
                 WHEN      "1"
                   MOVE     2              TO  IDX
               END-EVALUATE
             END-PERFORM
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             IF   RECE41-SRYYM  >= "202008"
               MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
             ELSE
               MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE     1       TO     WRK-JIDO-SETCOMATFLG
             END-IF
             MOVE       99      TO     FLG-COMMENT
      *
             MOVE      ZERO     TO     IDZ
             INITIALIZE                WRK-FREE-AREA2
      *
             IF   RECE41-SRYYM  >= "202008"
              IF   WRK-JINNAI-YMD   NOT =  SPACE
                MOVE   17            TO  IDX-JIDO
                PERFORM  200111-RECECOMMENT-IDXGET-SEC
                MOVE   WRK-JINNAI-YMD
                                     TO  WRK-RECECOM-YMD
                PERFORM  200111-RECECOMMENT-HENSYU-SEC
              ELSE
                MOVE   517           TO  IDX-JIDO
                PERFORM  200111-RECECOMMENT-IDXGET-SEC
                PERFORM  200111-RECECOMMENT-HENSYU-SEC
              END-IF
             ELSE
              IF   WRK-JINNAI-YMD   NOT =  SPACE
                ADD        1       TO     IDZ
                MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "前回算定年月日　　　　　年　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
                MOVE       21      TO     IDX41
                MOVE       27      TO     IDX42
                MOVE       33      TO     IDX48
                MOVE       17      TO     IDX49
      *
                INITIALIZE                      STS-AREA-DAY
                INITIALIZE                      LNK-DAY2-AREA
                MOVE    "21"                TO  LNK-DAY2-IRAI
                MOVE    WRK-JINNAI-YMD      TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING      STS-AREA-DAY
                                                LNK-DAY2-AREA
                IF      STS-DAY-RC1  =  ZERO
                  MOVE  LNK-DAY2-EDTYMD1-Y  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD1-M  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD1-D  TO  WRK-TAIHI-XX
                  INITIALIZE                    ORCSKANACHKAREA
                  MOVE  "2"                 TO  KANACHK-SYORI
                  MOVE  WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
                  CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                  IF    KANACHK-RC  =  ZERO
                    MOVE  KANACHK-OUT-INPUT(1:4)
                               TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                  END-IF
      *
                  MOVE  LNK-DAY2-EDTYMD2-G
                               TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                END-IF
              ELSE
                ADD        1       TO     IDZ
                MOVE       6       TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "（初回算定）"  TO     WK-FREE-NAME2(IDZ)
              END-IF
             END-IF
      *
             PERFORM  200111-JIDOSET-HENSYU-SEC
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-JINNAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来排尿自立指導料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-GAIJIR-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-GAIJIR-COMMENT   =    "1"
      *
             MOVE  SPACE             TO  WRK-GAIJIR-FSANTEIYM
             MOVE  ZERO              TO  WRK-GAIJIR-KAISU
      *
             INITIALIZE                  ORCSRECESRCHKAREA
             MOVE  "1"               TO  LNK-RECESRCHK-SYORI
             MOVE  RECE41-PTID       TO  LNK-RECESRCHK-PTID
             MOVE  RECE41-SRYYM      TO  LNK-RECESRCHK-SRYYM
             MOVE  RECE41-NYUGAIKBN  TO  LNK-RECESRCHK-NYUGAIKBN
             MOVE  RECE41-GAIJIRYMD  TO  LNK-RECESRCHK-SRYYMD(1)
             CALL  "ORCSRECESRCHK"   USING  SPA-AREA
                                            ORCSRECESRCHKAREA
             IF    LNK-RECESRCHK-RC  =  ZERO
                MOVE  LNK-RECESRCHK-FSANTEIYMD
                                           TO  WRK-GAIJIR-FSANTEIYM
                MOVE  LNK-RECESRCHK-KAISU  TO  WRK-GAIJIR-KAISU
             END-IF
      *
             IF  ((RECE41-SRYYM      >= "202008"    ) AND
                  (WRK-GAIJIR-FSANTEIYM  NOT = SPACE) AND
                  (WRK-GAIJIR-KAISU      NOT = ZERO ))
             OR  ((RECE41-SRYYM      <  "202008"    ) AND
                  (WRK-GAIJIR-FSANTEIYM  NOT = SPACE) AND
                  (WRK-GAIJIR-KAISU      NOT = ZERO ) AND
                  (WRK-GAIJIR-KAISU         <=  31  ))
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                ELSE
                  MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                END-IF
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                IF   RECE41-SRYYM  >= "202008"
                  MOVE   19            TO  IDX-JIDO
                  PERFORM  200111-RECECOMMENT-IDXGET-SEC
                  MOVE   WRK-GAIJIR-FSANTEIYM
                                       TO  WRK-RECECOM-YMD
                  PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                  MOVE   519           TO  IDX-JIDO
                  PERFORM  200111-RECECOMMENT-IDXGET-SEC
                  MOVE   WRK-GAIJIR-KAISU
                                       TO  WRK-RECECOM-NUM
                  PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
                ELSE
                  ADD        1       TO     IDZ
                  MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "初回算定　　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE    WRK-GAIJIR-FSANTEIYM(5:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO  WK-FREE-NAME2(IDZ)(11:4)
                  MOVE    WRK-GAIJIR-FSANTEIYM(7:2)  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO  WK-FREE-NAME2(IDZ)(17:4)
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                  ADD        1       TO     IDZ
                  MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "通算実施回数（　　回）"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE    WRK-GAIJIR-KAISU  TO  DAY-IDX2
                  MOVE    TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(15:4)
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
      *
           END-IF
      *
           .
       2001-GAIJIR-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    下肢創傷処置管理料があるか判定
      *  ＃2022選択式コメント対応
      *****************************************************************
       2001-SOUKAN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-SOUKAN-COMMENT  =  "1"
           AND    FLG-SOUKAN-HENSYU          =   1
      *
               INITIALIZE             WRK-JIDO-SETCOM-AREA
               MOVE    "2"      TO    WRK-JIDO-SETCOMFLG
               MOVE     99      TO    FLG-COMMENT
      *
               MOVE     121     TO    IDX-JIDO
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
      *
               INITIALIZE             TBL-SANTEI-DAY
               MOVE     ZERO    TO    DAY-IDX
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX      >   31)
                           OR   (DAY-IDX  >   31)
                 IF  TBL-SOUKAN-FLG(IDX)  =    1
                   ADD    1     TO    DAY-IDX
                   MOVE   IDX   TO    TB-SANTEI-DAY(DAY-IDX)
                 END-IF
               END-PERFORM
      *
               MOVE       ZERO    TO  IDZ
               INITIALIZE             WRK-FREE-AREA2
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX                >   31 )
                           OR   (TB-SANTEI-DAY(IDX) =  ZERO)
                 MOVE  RECE41-SRYYM       TO  WRK-RECECOM-YMD(1:6)
                 MOVE  TB-SANTEI-DAY(IDX) TO  WRK-RECECOM-YMD(7:2)
                 PERFORM  200111-RECECOMMENT-HENSYU-SEC
               END-PERFORM
      *
               PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-SOUKAN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    慢性腎臓病透析予防指導管理料があるか判定
      *  ＃202406選択式コメント対応
      *****************************************************************
       2001-MANJIN-HANTEI-SEC          SECTION.
      *
           IF    RECE44-FLG-MANJIN-COMMENT  =  "1"
             MOVE    ZERO        TO      FLG-MANJIN-COMMENT
             MOVE    ZERO        TO      FLG-MANJIN-COMMENT2
             MOVE    SPACE       TO      WRK-MANJIN-FSANTEIYMD
             MOVE    SPACE       TO      WRK-MANJIN-FSANTEIYMD2
      *
             MOVE    CONS-MANJIN         TO  WRK-SRYCD
      *      算定履歴検索
             INITIALIZE                      SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"      TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM  UNTIL (FLG-SANTEI          =  1)
                         OR   (FLG-MANJIN-COMMENT  =  1)
                 IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                   MOVE  SANTEI-FSANTEIYMD
                                    TO  WRK-MANJIN-FSANTEIYMD
                   MOVE     1       TO  FLG-MANJIN-COMMENT
                 END-IF
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
      *
             MOVE    CONS-MANJIN2        TO  WRK-SRYCD
      *      算定履歴検索
             INITIALIZE                      SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"      TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM  UNTIL (FLG-SANTEI          =  1)
                         OR   (FLG-MANJIN-COMMENT2 =  1)
                 IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                   MOVE  SANTEI-FSANTEIYMD
                                    TO  WRK-MANJIN-FSANTEIYMD2
                   MOVE     1       TO  FLG-MANJIN-COMMENT2
                 END-IF
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
      *
             IF     WRK-MANJIN-FSANTEIYMD   NOT = SPACE
             AND    WRK-MANJIN-FSANTEIYMD2  NOT = SPACE
               IF   WRK-MANJIN-FSANTEIYMD   >   WRK-MANJIN-FSANTEIYMD2
                 MOVE WRK-MANJIN-FSANTEIYMD2 TO WRK-MANJIN-FSANTEIYMD
                 MOVE     1                  TO FLG-MANJIN-COMMENT
               ELSE
                 CONTINUE
               END-IF
             ELSE
               IF   WRK-MANJIN-FSANTEIYMD   NOT = SPACE
                 CONTINUE
               END-IF
               IF   WRK-MANJIN-FSANTEIYMD2  NOT = SPACE
                 MOVE WRK-MANJIN-FSANTEIYMD2 TO WRK-MANJIN-FSANTEIYMD
                 MOVE     1                  TO FLG-MANJIN-COMMENT
               END-IF
             END-IF
      *
             IF     FLG-MANJIN-COMMENT  =  1
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                MOVE      "2"      TO     WRK-JIDO-SETCOMFLG
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                MOVE   126                    TO  IDX-JIDO
                PERFORM  200111-RECECOMMENT-IDXGET-SEC
                MOVE   WRK-MANJIN-FSANTEIYMD  TO  WRK-RECECOM-YMD
                PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
             END-IF
           END-IF
      *
           .
       2001-MANJIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    往診・特別往診があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-OSIN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-OSIN-COMMENT   =    "1"
           AND    FLG-OSIN-HENSYU           =     1 
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               IF   RECE41-SRYYM  >= "202008"
                 MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                 MOVE     99      TO     FLG-COMMENT
                 MOVE     35      TO     IDX-JIDO
                 PERFORM  200111-RECECOMMENT-IDXGET-SEC
               ELSE
                 MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                 MOVE     6       TO     FLG-COMMENT
               END-IF
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-OSIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問診療料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-ZAIHOU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-ZAIHOU-COMMENT   =  "1" OR "2" OR "4"
             IF   RECE41-SRYYM  >= "202004"
               IF  FLG-ZAIHOU-HENSYU  =  ZERO
                 PERFORM  20011-ZAIHOU-OSINCHK-SEC
               END-IF
             ELSE
               MOVE  1  TO  FLG-ZAIHOU-HENSYU
             END-IF
      *
             IF  FLG-ZAIHOU-HENSYU  =  1
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               IF    (RECE41-SRYYM  >= "202008"              )
               AND   (RECE44-FLG-ZAIHOU-COMMENT =  "1" OR "4")
                 EVALUATE  RECE44-FLG-ZAIHOU-COMMENT
                   WHEN     "1"
                     MOVE     "2"    TO   WRK-JIDO-SETCOMFLG
                     MOVE      99    TO   FLG-COMMENT
                     MOVE      36    TO   IDX-JIDO
                     PERFORM   200111-RECECOMMENT-IDXGET-SEC
                   WHEN     "4"
                     MOVE     "2"    TO   WRK-JIDO-SETCOMFLG
                     MOVE      99    TO   FLG-COMMENT
                     MOVE      133   TO   IDX-JIDO
                     PERFORM   200111-RECECOMMENT-IDXGET-SEC
                 END-EVALUATE
               ELSE
                 MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                 MOVE     6       TO     FLG-COMMENT
               END-IF
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
             END-IF
           END-IF
      *
           .
       2001-ZAIHOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問診療料の前月往診チェック
      *****************************************************************
       20011-ZAIHOU-OSINCHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  WRK-ZENYYMM
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"            TO  LNK-DAY6-IRAI
           MOVE    RECE41-SRYYM    TO  LNK-DAY6-KIJUN(1:6)
           MOVE    "01"            TO  LNK-DAY6-KIJUN(7:2)
           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
           MOVE     -1             TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN(1:6)
                                   TO  WRK-ZENYYMM
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL   (IDX                 >  2)
                       OR     (FLG-ZAIHOU-HENSYU   =  1)
             EVALUATE  IDX
               WHEN     1
                 MOVE  CONS-OSIN1  TO  WRK-OSIN-SRYCD
               WHEN     2
                 MOVE  CONS-OSIN2  TO  WRK-OSIN-SRYCD
             END-EVALUATE
      *
             INITIALIZE                        SRYACT-REC
             MOVE    RECE41-HOSPNUM        TO  SRY-HOSPNUM
             MOVE    RECE41-NYUGAIKBN      TO  SRY-NYUGAIKBN
             MOVE    RECE41-PTID           TO  SRY-PTID
             MOVE    WRK-ZENYYMM           TO  SRY-SRYYM
             MOVE    WRK-OSIN-SRYCD        TO  SRY-SRYCD(1)
             MOVE    WRK-OSIN-SRYCD        TO  SRY-SRYCD(2)
             MOVE    WRK-OSIN-SRYCD        TO  SRY-SRYCD(3)
             MOVE    WRK-OSIN-SRYCD        TO  SRY-SRYCD(4)
             MOVE    WRK-OSIN-SRYCD        TO  SRY-SRYCD(5)
             MOVE    SRYACT-REC            TO  MCPDATA-REC
             MOVE    "tbl_sryact"          TO  MCP-TABLE
             MOVE    "key7"                TO  MCP-PATHNAME
             PERFORM  900-DBSELECT-SEC
             IF       MCP-RC  =  ZERO
               MOVE      "tbl_sryact"      TO  MCP-TABLE
               MOVE      "key7"            TO  MCP-PATHNAME
               PERFORM    900-SRYACT-READ-SEC
               IF     FLG-SRYACT  =  ZERO
                 MOVE    1    TO    FLG-ZAIHOU-HENSYU
               END-IF
             END-IF
             MOVE    "tbl_sryact"          TO  MCP-TABLE
             MOVE    "key7"                TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
           END-PERFORM
      *
           .
       20011-ZAIHOU-OSINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問点滴注射管理指導料算定時の点滴注射があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-HOUTEN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-HOUTEN-COMMENT   =    "1"
               PERFORM    20011-HOUTEN-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE44-TENSOU-SEC
           END-IF
      *
           .
       2001-HOUTEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問点滴注射管理指導料算定時の点滴注射のコメント編集
      *****************************************************************
       20011-HOUTEN-COMMENT-SEC          SECTION.
      *
             INITIALIZE            WRK-JIDO-SETCOM-AREA
      *
             PERFORM    200111-RECE45-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE45
             MOVE    SPACE                TO  WRK-RECE45XX-REC
             INITIALIZE                       WRK-RECE45XX-REC
      *      コメントの挿入
             ADD     1                    TO  IDX-RECE45
      *
             IF   RECE41-SRYYM  >= "202008"
      *        「訪点」
               MOVE "820100642"    TO  WRK-TENSU-SRYCD
               MOVE  RECE41-SRYYM  TO  WRK-TENSU-SRYYM
               PERFORM 900-TENSU-TBL-SEC
      *
               MOVE  TNS-SRYCD  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
               MOVE  ZERO       TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
               MOVE  TNS-YUKOKETA
                                TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE  TNS-NAME   TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                             WRK-RECE45XX-INPUTCOMENT(IDX-RECE45 1)
               MOVE "1"         TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
             ELSE
               MOVE "810000001" TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
               MOVE  ZERO       TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
               MOVE  4          TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
               MOVE "（訪点）"  TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
               MOVE "1"         TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HOUTEN-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    介護職員等喀痰吸引等指示料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-KAISYOKU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-KAISYOKU-COMMENT   =    "1"
             MOVE    ZERO                TO  FLG-SINPESU-COMMENT
             MOVE    SPACE               TO  WRK-SINPESU-YMD
             INITIALIZE                      SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    CONS-KAISYOKU       TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                             OR   ( FLG-SINPESU-COMMENT =   1 )
      *
                   IF   SANTEI-SRYYM  <  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM     TO  WRK-SINPESU-YMD(1:6)
                     MOVE  SANTEI-MSANTEID  TO  WRK-SINPESU-YMD(7:2)
                     MOVE    1              TO  FLG-SINPESU-COMMENT
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             IF   RECE41-SRYYM  >= "202008"
               MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
             ELSE
               MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE     1       TO     WRK-JIDO-SETCOMATFLG
             END-IF
             MOVE       99      TO     FLG-COMMENT
      *
             MOVE      ZERO     TO     IDZ
             INITIALIZE                WRK-FREE-AREA2
      *
             IF   RECE41-SRYYM  >= "202008"
              IF   WRK-SINPESU-YMD   NOT =  SPACE
                MOVE   38            TO  IDX-JIDO
                PERFORM  200111-RECECOMMENT-IDXGET-SEC
                MOVE   WRK-SINPESU-YMD
                                     TO  WRK-RECECOM-YMD
                PERFORM  200111-RECECOMMENT-HENSYU-SEC
              ELSE
                MOVE   538           TO  IDX-JIDO
                PERFORM  200111-RECECOMMENT-IDXGET-SEC
                PERFORM  200111-RECECOMMENT-HENSYU-SEC
              END-IF
             ELSE
              IF   WRK-SINPESU-YMD   NOT =  SPACE
                ADD          1       TO     IDZ
                IF   RECE41-SRYYM >= "201905"
                  MOVE       19      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "前回交付年月日　　　　　年　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE       21      TO     IDX41
                  MOVE       27      TO     IDX42
                  MOVE       33      TO     IDX48
                  MOVE       17      TO     IDX49
                ELSE
                  MOVE       17      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE   "前回交付年月日　　　年　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                  MOVE       17      TO     IDX41
                  MOVE       23      TO     IDX42
                  MOVE       29      TO     IDX48
                END-IF
      *
                INITIALIZE                         STS-AREA-DAY
                INITIALIZE                         LNK-DAY2-AREA
                MOVE    "21"                   TO  LNK-DAY2-IRAI
                MOVE    WRK-SINPESU-YMD        TO  LNK-DAY2-YMD
                CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                               LNK-DAY2-AREA
                IF      STS-DAY-RC1            =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                   END-IF
      *
                   IF   RECE41-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                END-IF
              ELSE
                ADD        1       TO     IDZ
                MOVE       6       TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE   "（初回交付）"  TO     WK-FREE-NAME2(IDZ)
              END-IF
             END-IF
      *
             PERFORM  200111-JIDOSET-HENSYU-SEC
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-KAISYOKU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅持続陽圧呼吸療法指導管理料があるか判定
      *  ＃202202選択式コメント対応
      *****************************************************************
       2001-YOUATU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-YOUATU-COMMENT   =    "1" OR "2"
             MOVE    SPACE                TO  WRK-YOUATU-FSANTEIYMD
             INITIALIZE                       SANTEI-REC
             MOVE    RECE41-HOSPNUM       TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID          TO  SANTEI-PTID
             MOVE    RECE41-SRYYM         TO  SANTEI-SRYYM
             EVALUATE  RECE44-FLG-YOUATU-COMMENT
               WHEN      "1"
                 MOVE    CONS-YOUATU1     TO  SANTEI-SRYCD
               WHEN      "2"
                 MOVE    CONS-YOUATU2     TO  SANTEI-SRYCD
             END-EVALUATE
             MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                 TO  SANTEI-SRYKA
             MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC           TO  MCPDATA-REC
             MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
             MOVE    "key"                TO  WRK-MCP-PATHNAME
             PERFORM 900-SANTEI-INV-SEC
             IF      FLG-SANTEI  =  ZERO
               MOVE  SANTEI-FSANTEIYMD    TO  WRK-YOUATU-FSANTEIYMD
             END-IF
      *
             MOVE    ZERO                 TO  FLG-YOUATU-COMMENT
             INITIALIZE                       SANTEI-REC
             MOVE    RECE41-HOSPNUM       TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID          TO  SANTEI-PTID
             MOVE    CONS-YOUATU3         TO  SANTEI-SRYCD
             MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                 TO  SANTEI-SRYKA
             MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC           TO  MCPDATA-REC
             MOVE    "tbl_santei"         TO  MCP-TABLE
             MOVE    "key3"               TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC  =  ZERO
               MOVE    "tbl_santei"       TO  MCP-TABLE
               MOVE    "key3"             TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM  UNTIL (FLG-SANTEI         =  1)
                         OR   (FLG-YOUATU-COMMENT =  1)
                 IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                   IF    SANTEI-FSANTEIYMD  NOT =  SPACE
                     MOVE  SANTEI-FSANTEIYMD
                                    TO  WRK-YOUATU-FSANTEIYMD
                   END-IF
                   MOVE     1       TO  FLG-YOUATU-COMMENT
                 END-IF
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM  900-CLOSE-SEC
      *
             MOVE         ZERO     TO     IDZ
             INITIALIZE                   WRK-FREE-AREA2
             IF   WRK-YOUATU-FSANTEIYMD   NOT =  SPACE
               INITIALIZE                 WRK-JIDO-SETCOM-AREA
               MOVE      "2"       TO     WRK-JIDO-SETCOMFLG
               MOVE       99       TO     FLG-COMMENT
      *
               EVALUATE  RECE44-FLG-YOUATU-COMMENT
                 WHEN      "1"
                   MOVE    115     TO     IDX-JIDO
                 WHEN      "2"
                   MOVE    116     TO     IDX-JIDO
               END-EVALUATE
               PERFORM    200111-RECECOMMENT-IDXGET-SEC
               MOVE       WRK-YOUATU-FSANTEIYMD
                                   TO     WRK-RECECOM-YMD
               PERFORM    200111-RECECOMMENT-HENSYU-SEC
             END-IF
      *
             IF   (RECE44-FLG-SINPESU-COMMENT    =    "B")
             AND  (RECE44-FLG-SINPESUR-COMMENT   =    "B")
               MOVE    ZERO                TO  FLG-SINPESUR-COMMENT
               MOVE    SPACE               TO  WRK-SINPESUR-YMD
               INITIALIZE                      SANTEI-REC
               MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE41-PTID         TO  SANTEI-PTID
               MOVE    CONS-YOUATU2        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"      TO  MCP-TABLE
                 MOVE    "key3"            TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI           =   1 )
                             OR   ( FLG-SINPESUR-COMMENT =   1 )
                   IF   SANTEI-SRYYM  <  RECE41-SRYYM
                     MOVE  SANTEI-SRYYM    TO  WRK-SINPESUR-YMD(1:6)
                     MOVE  SANTEI-MSANTEID TO  WRK-SINPESUR-YMD(7:2)
                     MOVE    1             TO  FLG-SINPESUR-COMMENT
                   END-IF
                   MOVE    "tbl_santei"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               IF   WRK-SINPESUR-YMD   NOT =  SPACE
                 INITIALIZE                 WRK-JIDO-SETCOM-AREA
                 MOVE      "2"       TO     WRK-JIDO-SETCOMFLG
                 MOVE       99       TO     FLG-COMMENT
      *
                 MOVE       27       TO     IDX-JIDO
                 PERFORM   200111-RECECOMMENT-IDXGET-SEC
                 MOVE      WRK-SINPESUR-YMD(1:6)
                                     TO     WRK-RECECOM-YMD(1:6)
                 MOVE      "00"      TO     WRK-RECECOM-YMD(7:2)
                 PERFORM   200111-RECECOMMENT-HENSYU-SEC
               END-IF
             END-IF
      *
             IF  WK-FREE-SRYCD2(1)  NOT =  SPACE
             OR  WK-FREE-NAME2(1)   NOT =  SPACE
               PERFORM     200111-SRYACTINF-HENSYU-MAT-SEC
             END-IF
           END-IF
      *
           .
       2001-YOUATU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者共同診療料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-ZAIKYO-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-ZAIKYO-COMMENT   =    "1"
             MOVE   CONS-ZAIKYO   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
      *        在宅患者共同診療料開始日の剤検索
               MOVE       ZERO                  TO  WRK-ZAIKYO-KAISU
               MOVE       ZERO                  TO  FLG-ZAIKYOCOMARI
               PERFORM    20011-ZAIKYO-KENSAKU-SEC
               IF    FLG-ZAIKYOCOMARI  =  1
                  PERFORM  VARYING  IDX30  FROM  1  BY  1
                            UNTIL   IDX30   >   TBL-ZAIKYO-CHECK-MAX
                    INITIALIZE                      SANTEI-REC
                    MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
                    MOVE    RECE41-PTID         TO  SANTEI-PTID
                    MOVE    TBL-ZAIKYO-CHECK(IDX30)
                                                TO  SANTEI-SRYCD
                    MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                    MOVE    ZERO                TO  SANTEI-SRYKA
                    MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
                    MOVE    SANTEI-REC          TO  MCPDATA-REC
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM 900-DBSELECT-SEC
                    IF      MCP-RC              =   ZERO
                      MOVE    "tbl_santei"      TO  MCP-TABLE
                      MOVE    "key3"            TO  MCP-PATHNAME
                      PERFORM  900-SANTEI-READ-SEC
                      PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                        IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                        AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                          MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                          MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                          PERFORM  VARYING  IDX26  FROM  31  BY  -1
                                    UNTIL   IDX26   <     1
                            IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                              IF ((SANTEI-SRYYM =
                                          WRK-ZAIKYO-YMD(1:6) ) AND
                                  (IDX26        < IDX28       ))
                              OR ((SANTEI-SRYYM =
                                          WRK-ZAIKYO-YMD9(1:6)) AND
                                  (IDX26       >= IDX29       ))
                                CONTINUE
                              ELSE
                                ADD    1    TO   WRK-ZAIKYO-KAISU
                              END-IF
                            END-IF
                          END-PERFORM
                        END-IF
      *
                        MOVE    "tbl_santei"    TO  MCP-TABLE
                        MOVE    "key3"          TO  MCP-PATHNAME
                        PERFORM  900-SANTEI-READ-SEC
                      END-PERFORM
                    END-IF
                    MOVE    "tbl_santei"        TO  MCP-TABLE
                    MOVE    "key3"              TO  MCP-PATHNAME
                    PERFORM 900-CLOSE-SEC
                  END-PERFORM
               END-IF
      *
               INITIALIZE                WRK-JIDO-SETCOM-AREA
               IF   RECE41-SRYYM  >= "202008"
                 MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
               ELSE
                 MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE     1       TO     WRK-JIDO-SETCOMATFLG
               END-IF
               MOVE       99      TO     FLG-COMMENT
      *
               MOVE      ZERO     TO     IDZ
               INITIALIZE                WRK-FREE-AREA2
      *
               IF   RECE41-SRYYM  >= "202008"
                  MOVE   39            TO  IDX-JIDO
                  PERFORM  200111-RECECOMMENT-IDXGET-SEC
                  MOVE   WRK-ZAIKYO-YMD
                                       TO  WRK-RECECOM-YMD
                  PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                  IF    WRK-ZAIKYO-KAISU  NOT = ZERO
                    MOVE   539         TO  IDX-JIDO
                    PERFORM  200111-RECECOMMENT-IDXGET-SEC
                    MOVE   WRK-ZAIKYO-KAISU
                                       TO  WRK-RECECOM-NUM
                    PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                    PERFORM  200111-JIDOSET-HENSYU-SEC
                  END-IF
               ELSE
                  ADD      1       TO     IDZ
                  MOVE     11      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE "初回算定　　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
                  MOVE  WRK-ZAIKYO-YMD(5:2)   TO  DAY-IDX2
                  MOVE  TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(11:4)
                  MOVE  WRK-ZAIKYO-YMD(7:2)   TO  DAY-IDX2
                  MOVE  TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(17:4)
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                  IF   (WRK-ZAIKYO-KAISU  NOT = ZERO)
                  AND  (WRK-ZAIKYO-KAISU     <=  31 )
                    ADD    1       TO  IDZ
                    MOVE   11      TO  WK-FREE-YUKOKETA2(IDZ)
                    MOVE "通算実施回数（　　回）"
                                   TO  WK-FREE-NAME2(IDZ)
                    MOVE  WRK-ZAIKYO-KAISU TO  DAY-IDX2
                    MOVE  TBL-DAY(DAY-IDX2)
                                   TO  WK-FREE-NAME2(IDZ)(15:4)
      *
                    PERFORM  200111-JIDOSET-HENSYU-SEC
                  END-IF
               END-IF
      *
               PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-ZAIKYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       20011-ZAIKYO-CHECK-SEC          SECTION.
      *
           MOVE    RECE41-SRYYM        TO  WRK-ZAIKYO-YMD9(1:6)
           MOVE    "99"                TO  WRK-ZAIKYO-YMD9(7:2)
      *
           MOVE    ZERO                TO  FLG-ZAIKYO-COMMENT
           MOVE    SPACE               TO  WRK-ZAIKYO-YMD
           MOVE    ZERO                TO  FLG-RETRY
           INITIALIZE                      SANTEI-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                           OR   ( FLG-ZAIKYO-COMMENT  =   1 )
      *
                 IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                   MOVE  SANTEI-SRYYM     TO  WRK-ZAIKYO-YMD(1:6)
                   MOVE  SANTEI-MSANTEID  TO  WRK-ZAIKYO-YMD(7:2)
                   MOVE    1              TO  FLG-ZAIKYO-COMMENT
      *
                   IF   SANTEI-SRYYM  =  RECE41-SRYYM
                     MOVE   ZERO  TO  FLG-CHECK-MAE
                     MOVE   ZERO  TO  FLG-CHECK-ATO
                     MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                     PERFORM  VARYING  IDX26  FROM   1  BY  1
                               UNTIL   IDX26   >    31
                       IF  RECE44-KAISU(1 IDX26)  NOT =  ZERO
                         IF    IDX26  <  IDX28
                           MOVE    1   TO  FLG-CHECK-MAE
                         ELSE
                           MOVE    1   TO  FLG-CHECK-ATO
                         END-IF
                       END-IF
                     END-PERFORM
                     IF  ((FLG-CHECK-MAE = 1   ) AND
                          (FLG-CHECK-ATO = ZERO))
                     OR  ((FLG-CHECK-MAE = 1   ) AND
                          (FLG-CHECK-ATO = 1   ))
                       MOVE    WRK-ZAIKYO-YMD
                                       TO  WRK-ZAIKYO-YMD9
                       MOVE    SPACE   TO  WRK-ZAIKYO-YMD
                       MOVE    1       TO  FLG-RETRY
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE    "tbl_santei"  TO  MCP-TABLE
                 MOVE    "key3"        TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF   FLG-RETRY =  1
             MOVE    ZERO                TO  FLG-ZAIKYO-COMMENT
             MOVE    SPACE               TO  WRK-ZAIKYO-YMD
             MOVE    ZERO                TO  FLG-RETRY
             INITIALIZE                      SANTEI-REC
             MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
             MOVE    RECE41-PTID         TO  SANTEI-PTID
             MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
             MOVE    ZERO                TO  SANTEI-NYUGAIKBN
             MOVE    ZERO                TO  SANTEI-SRYKA
             MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
             MOVE    SANTEI-REC          TO  MCPDATA-REC
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM  900-SANTEI-READ-SEC
               PERFORM   UNTIL  ( FLG-SANTEI          =   1 )
                           OR   ( FLG-ZAIKYO-COMMENT  =   1 )
      *
                 IF   SANTEI-SRYYM  <   RECE41-SRYYM
                   MOVE  SANTEI-SRYYM     TO  WRK-ZAIKYO-YMD(1:6)
                   MOVE  SANTEI-MSANTEID  TO  WRK-ZAIKYO-YMD(7:2)
                   MOVE    1              TO  FLG-ZAIKYO-COMMENT
                 END-IF
      *
                 MOVE    "tbl_santei"  TO  MCP-TABLE
                 MOVE    "key3"        TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_santei"        TO  MCP-TABLE
             MOVE    "key3"              TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-ZAIKYO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       20011-ZAIKYO-CHECK2-SEC          SECTION.
      *
      *    算定履歴検索
           MOVE    SPACE               TO  WRK-ZAIKYO-YMD2
           INITIALIZE                      SANTEI-REC
           MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID         TO  SANTEI-PTID
           MOVE    RECE41-SRYYM        TO  SANTEI-SRYYM
           MOVE    WRK-CHECK-SRYCD     TO  SANTEI-SRYCD
           MOVE    ZERO                TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                TO  SANTEI-SRYKA
           MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
             MOVE   SANTEI-SRYYM       TO  WRK-ZAIKYO-YMD2(1:6)
             MOVE   SANTEI-MSANTEID    TO  WRK-ZAIKYO-YMD2(7:2)
             IF     WRK-ZAIKYO-YMD     <=  WRK-ZAIKYO-YMD2
               MOVE   ZERO  TO  FLG-CHECK-MAE
               MOVE   ZERO  TO  FLG-CHECK-ATO
               MOVE   WRK-ZAIKYO-YMD2(7:2)  TO  IDX28
               PERFORM  VARYING  IDX26  FROM   1  BY  1
                         UNTIL   IDX26   >    31
                 IF  RECE44-KAISU(1 IDX26)  NOT =  ZERO
                   IF    IDX26  <=  IDX28
                     MOVE    1   TO  FLG-CHECK-MAE
                   ELSE
                     MOVE    1   TO  FLG-CHECK-ATO
                   END-IF
                 END-IF
               END-PERFORM
               IF    FLG-CHECK-MAE = ZERO
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD2
               END-IF
             ELSE
                 MOVE    SPACE   TO  WRK-ZAIKYO-YMD2
             END-IF
           END-IF
      *
           .
       20011-ZAIKYO-CHECK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者共同診療料開始日の剤検索
      *****************************************************************
       20011-ZAIKYO-KENSAKU-SEC          SECTION.
      *
           MOVE    ZERO                  TO  FLG-ZAIKYO-OK
           MOVE    SPACE                 TO  WRK-ZAIKYO-NYUGAIKBN
           MOVE    SPACE                 TO  WRK-ZAIKYO-SRYKA
           MOVE    ZERO                  TO  WRK-ZAIKYO-ZAINUM
      *
           PERFORM   VARYING   IDX15    FROM   2   BY  -1
                      UNTIL   (IDX15          <    1)
                       OR     (FLG-ZAIKYO-OK  =    1)
             INITIALIZE                      SRYACT-REC
             MOVE    RECE41-HOSPNUM      TO  SRY-HOSPNUM
             MOVE    IDX15               TO  SRY-NYUGAIKBN
             MOVE    RECE41-PTID         TO  SRY-PTID
             MOVE    WRK-ZAIKYO-YMD(1:6) TO  SRY-SRYYM
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (1)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (2)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (3)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (4)
             MOVE    CONS-ZAIKYO         TO  SRY-SRYCD (5)
             MOVE    SRYACT-REC          TO  MCPDATA-REC
             MOVE    "tbl_sryact"        TO  MCP-TABLE
             MOVE    "key7"              TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"      TO  MCP-TABLE
               MOVE    "key7"            TO  MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACT     =   1
                         OR     FLG-ZAIKYO-OK  =   1
                 INITIALIZE                  SRYACCT-REC
                 MOVE    SRY-HOSPNUM     TO  ACCT-HOSPNUM
                 MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
                 MOVE    SRY-PTID        TO  ACCT-PTID
                 MOVE    SRY-SRYKA       TO  ACCT-SRYKA
                 MOVE    SRY-SRYYM       TO  ACCT-SRYYM
                 MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
                 MOVE    SRYACCT-REC     TO  MCPDATA-REC
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key31"         TO  MCP-PATHNAME
                 PERFORM 900-DBSELECT-SEC
                 IF      MCP-RC          =   ZERO
                    MOVE "tbl_sryacct"   TO  MCP-TABLE
                    MOVE "key31"         TO  MCP-PATHNAME
                    PERFORM 900-DBFETCH-SEC
                    IF      MCP-RC       =   ZERO
                      MOVE  MCPDATA-REC  TO  SRYACCT-REC
      *
                      MOVE  WRK-ZAIKYO-YMD(7:2)
                                         TO  DAY-IDX2
                      IF   ACCT-DAY(1 DAY-IDX2)  NOT =  ZERO
                        MOVE   1   TO  FLG-ZAIKYO-OK
                        MOVE   ACCT-NYUGAIKBN
                                   TO  WRK-ZAIKYO-NYUGAIKBN
                        MOVE   ACCT-SRYKA
                                   TO  WRK-ZAIKYO-SRYKA
                        MOVE   ACCT-ZAINUM
                                   TO  WRK-ZAIKYO-ZAINUM
                      END-IF
                    END-IF
                 END-IF
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key31"         TO  MCP-PATHNAME
                 PERFORM   900-CLOSE-SEC
      *
                 MOVE    "tbl_sryact"    TO  MCP-TABLE
                 MOVE    "key7"          TO  MCP-PATHNAME
                 PERFORM   900-SRYACT-READ-SEC
               END-PERFORM
             END-IF
             MOVE    "tbl_sryact"        TO  MCP-TABLE
             MOVE    "key7"              TO  MCP-PATHNAME
             PERFORM   900-CLOSE-SEC
           END-PERFORM
      *
      *++++
      *
           IF      WRK-ZAIKYO-ZAINUM    NOT =    ZERO
              INITIALIZE                         SRYACT-REC
              MOVE    RECE41-HOSPNUM         TO  SRY-HOSPNUM
              MOVE    WRK-ZAIKYO-NYUGAIKBN   TO  SRY-NYUGAIKBN
              MOVE    RECE41-PTID            TO  SRY-PTID
              MOVE    WRK-ZAIKYO-SRYKA       TO  SRY-SRYKA
              MOVE    WRK-ZAIKYO-YMD(1:6)    TO  SRY-SRYYM
              MOVE    WRK-ZAIKYO-ZAINUM      TO  SRY-ZAINUM
              MOVE    SRYACT-REC             TO  MCPDATA-REC
              MOVE    "tbl_sryact"           TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC                 =   ZERO
                 MOVE    "tbl_sryact"        TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM  900-SRYACT-READ-SEC
                 PERFORM  UNTIL  (FLG-SRYACT       =  1)
                           OR    (FLG-ZAIKYOCOMARI =  1)
                   PERFORM  VARYING  IDX12  FROM  1  BY  1
                             UNTIL  (IDX12             >    5   )
                              OR    (SRY-SRYCD(IDX12)  =   SPACE)
                              OR    (FLG-ZAIKYOCOMARI  =    1   )
                      IF  SRY-SRYCD(IDX12)  =  "840000086"
                                            OR "842100043"
                         MOVE    1    TO   FLG-ZAIKYOCOMARI
                      END-IF
                   END-PERFORM
                   MOVE    "tbl_sryact"      TO  MCP-TABLE
                   MOVE    "key2"            TO  MCP-PATHNAME
                   PERFORM   900-SRYACT-READ-SEC
                 END-PERFORM
              END-IF
              MOVE    "tbl_sryact"           TO  MCP-TABLE
              MOVE    "key2"                 TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
       20011-ZAIKYO-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問リハビリテーション指導管理料があるか判定
      *  ＃202010選択式コメント対応
      *****************************************************************
       2001-ZAIRIHA-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-ZAIRIHA-COMMENT   =    "1"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       1       TO     WRK-JIDO-SETCOMATFLG
               IF   RECE41-SRYYM  >= "202010"
                 MOVE    "1"      TO     WRK-JIDO-RECEDENFLG
               END-IF
               MOVE       9       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-ZAIRIHA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅患者訪問薬剤管理指導料があるか判定
      *  ＃202004選択式コメント対応
      *****************************************************************
       2001-ZAIYAK-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-ZAIYAK-COMMENT   =    "1"
           AND    RECE44-ZAIKAISU            >=     2
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       1       TO     WRK-JIDO-SETCOMATFLG
               IF   RECE41-SRYYM  >= "202004"
                 MOVE    "1"      TO     WRK-JIDO-RECEDENFLG
               END-IF
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-ZAIYAK-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在医総管、施医総管があるか判定
      *  ＃202010選択式コメント対応
      *****************************************************************
       2001-ZAIISO-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-ZAIISO-COMMENT  =  "1"
           OR     RECE44-FLG-SHIISO-COMMENT  =  "1"
             IF   FLG-OSIN-HENSYU    =  1
             OR   FLG-ZAIHOU-HENSYU  =  1
      *
               INITIALIZE             WRK-JIDO-SETCOM-AREA
               MOVE    "2"      TO    WRK-JIDO-SETCOMFLG
               MOVE     99      TO    FLG-COMMENT
               IF   RECE44-FLG-ZAIISO-COMMENT  =  "1"
                 MOVE   9901    TO    IDX-JIDO
               ELSE
                 MOVE   9902    TO    IDX-JIDO
               END-IF
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
      *
               INITIALIZE             TBL-SANTEI-DAY
               MOVE     ZERO    TO    DAY-IDX
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX      >   31)
                           OR   (DAY-IDX  >   31)
                 IF  TBL-ZAIISO-FLG(IDX)  =    1
                   ADD    1     TO    DAY-IDX
                   MOVE   IDX   TO    TB-SANTEI-DAY(DAY-IDX)
                 END-IF
               END-PERFORM
      *
               MOVE       ZERO    TO  IDZ
               INITIALIZE             WRK-FREE-AREA2
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX                >   31 )
                           OR   (TB-SANTEI-DAY(IDX) =  ZERO)
                 MOVE  RECE41-SRYYM       TO  WRK-RECECOM-YMD(1:6)
                 MOVE  TB-SANTEI-DAY(IDX) TO  WRK-RECECOM-YMD(7:2)
                 PERFORM  200111-RECECOMMENT-HENSYU-SEC
               END-PERFORM
      *
               PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-ZAIISO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来在宅共同指導料があるか判定
      *  ＃202204選択式コメント対応
      *****************************************************************
       2001-GAIZAI-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-GAIZAI-COMMENT   =    "1"
                INITIALIZE              WRK-JIDO-SETCOM-AREA
                MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                MOVE     99      TO     FLG-COMMENT
                MOVE     138     TO     IDX-JIDO
                PERFORM  200111-RECECOMMENT-IDXGET-SEC
                PERFORM  20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-GAIZAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    在宅抗菌薬吸入療法用ネブライザ加算があるか判定
      *  ＃202204選択式コメント対応
      *****************************************************************
       2001-KOUYAK-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-KOUYAK-COMMENT  =  "1" OR "2"
      *
             IF   RECE44-FLG-KOUYAK-COMMENT  =  "2"
               MOVE    ZERO              TO  FLG-KOUYAK-COMMENT
               MOVE    SPACE             TO  WRK-KOUYAK-FSANTEIYMD
               INITIALIZE                    SANTEI-REC
               MOVE    RECE41-HOSPNUM    TO  SANTEI-HOSPNUM
               MOVE    RECE41-PTID       TO  SANTEI-PTID
               MOVE    CONS-KOUYAK       TO  SANTEI-SRYCD
               MOVE    ZERO              TO  SANTEI-NYUGAIKBN
               MOVE    ZERO              TO  SANTEI-SRYKA
               MOVE    ZERO              TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC        TO  MCPDATA-REC
               MOVE    "tbl_santei"      TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL  ( FLG-SANTEI         =   1 )
                             OR   ( FLG-KOUYAK-COMMENT =   1 )
                   IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                     MOVE    SANTEI-FSANTEIYMD
                                         TO  WRK-KOUYAK-FSANTEIYMD
                     MOVE      1         TO  FLG-KOUYAK-COMMENT
                   END-IF
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"      TO  MCP-TABLE
               MOVE    "key3"            TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
             END-IF
      *
             INITIALIZE                WRK-JIDO-SETCOM-AREA
             MOVE      "2"        TO   WRK-JIDO-SETCOMFLG
             MOVE       99        TO   FLG-COMMENT
      *
             MOVE      ZERO       TO   IDZ
             INITIALIZE                WRK-FREE-AREA2
      *
             EVALUATE  RECE44-FLG-KOUYAK-COMMENT
               WHEN      "1"
                 MOVE       139   TO   IDX-JIDO
                 PERFORM    200111-RECECOMMENT-IDXGET-SEC
                 PERFORM    200111-RECECOMMENT-HENSYU-SEC
               WHEN      "2"
                 IF    WRK-KOUYAK-FSANTEIYMD   NOT =  SPACE
                   MOVE     140   TO   IDX-JIDO
                   PERFORM  200111-RECECOMMENT-IDXGET-SEC
                   MOVE     WRK-KOUYAK-FSANTEIYMD
                                  TO  WRK-RECECOM-YMD
                   PERFORM  200111-RECECOMMENT-HENSYU-SEC
                 END-IF
             END-EVALUATE
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-KOUYAK-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    緊急訪問看護加算があるか判定
      *  ＃202406選択式コメント対応
      *****************************************************************
       2001-KINHOU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-KINHOU-COMMENT   =    "1"
               INITIALIZE              WRK-JIDO-SETCOM-AREA
               MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
               MOVE     99      TO     FLG-COMMENT
               MOVE     131     TO     IDX-JIDO
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
               PERFORM  20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KINHOU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訪問診療にかかる記録書コメント（日・人数）があるか判定
      *****************************************************************
       2001-HOMONCOM-HANTEI-SEC          SECTION.
      *
           IF     RECE44-HOMONCOMFLG    =  "1"
           AND    TBL-HOMONCOM-SETFLG   =   1
      *
             INITIALIZE            WRK-JIDO-SETCOM-AREA
      *
             MOVE       99         TO   FLG-COMMENT
      *
             MOVE       ZERO       TO   IDZ
             INITIALIZE            WRK-FREE-AREA2
      *
             PERFORM   VARYING   IDX    FROM   1   BY   1
                        UNTIL    IDX      >   31
      *
               IF    TBL-HOMONCOM-NINZU(IDX)  NOT = ZERO
                 ADD      1                TO  IDZ
                 MOVE     "　　　　　　　日（　　　人）"
                                           TO  WK-FREE-NAME2(IDZ)
                 IF    IDZ  =  1
                   MOVE   "診療人数："     TO  WK-FREE-NAME2(IDZ)(1:10)
                 END-IF
                 MOVE     TBL-DAY(IDX)     TO  WK-FREE-NAME2(IDZ)(11:4)
                 INITIALIZE                    WRK-CHAR-HENKAN-AREA
                 MOVE     TBL-HOMONCOM-NINZU(IDX)
                                           TO  WRK-SRYKAISU-X
                 MOVE     WRK-SRYKAISU-X   TO  WRK-CHAR-X
                 PERFORM  2502111-NUM-ZENKAKU-HENKAN-SEC
                 EVALUATE WRK-CHAR-N-CNT
                   WHEN       2
                     MOVE WRK-CHAR-N(1:2)  TO  WK-FREE-NAME2(IDZ)(23:2)
                   WHEN       4
                     MOVE WRK-CHAR-N(1:4)  TO  WK-FREE-NAME2(IDZ)(21:4)
                   WHEN       6
                     MOVE WRK-CHAR-N(1:6)  TO  WK-FREE-NAME2(IDZ)(19:6)
                 END-EVALUATE
                 MOVE     14               TO  WK-FREE-YUKOKETA2(IDZ)
               END-IF
      *
             END-PERFORM
      *
             PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           END-IF
      *
           .
       2001-HOMONCOM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    磁気による膀胱等刺激法があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-BOKOHO-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-BOKOHO-COMMENT   =    "1"
             MOVE   CONS-BOKOHO1   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
               MOVE    ZERO                TO  IDX27
               INITIALIZE                      WRK-BOKOHO-YMD2-AREA
               MOVE    ZERO                TO  WRK-ZAIKYO-KAISU
               INITIALIZE                      SANTEI-REC
               MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
               MOVE    RECE41-PTID         TO  SANTEI-PTID
               MOVE    CONS-BOKOHO2        TO  SANTEI-SRYCD
               MOVE    ZERO                TO  SANTEI-NYUGAIKBN
               MOVE    ZERO                TO  SANTEI-SRYKA
               MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                 MOVE    "tbl_santei"    TO  MCP-TABLE
                 MOVE    "key3"          TO  MCP-PATHNAME
                 PERFORM  900-SANTEI-READ-SEC
                 PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                   IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                   AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                     MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                     MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                     PERFORM  VARYING  IDX26  FROM  31  BY  -1
                               UNTIL   IDX26   <     1
                      IF   RECE41-SRYYM  >= "202008"
                       IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                         IF ((SANTEI-SRYYM = WRK-ZAIKYO-YMD(1:6) ) AND
                             (IDX26        < IDX28               ))
                         OR ((SANTEI-SRYYM = WRK-ZAIKYO-YMD9(1:6)) AND
                             (IDX26       >= IDX29               ))
                            CONTINUE
                          ELSE
                           ADD   1      TO  WRK-ZAIKYO-KAISU
                         END-IF
                       END-IF
                      ELSE
                       IF  (SANTEI-DAY(IDX26)  NOT =  ZERO)
                       AND (IDX27              <    30    )
                         IF ((SANTEI-SRYYM = WRK-ZAIKYO-YMD(1:6) ) AND
                             (IDX26        < IDX28               ))
                         OR ((SANTEI-SRYYM = WRK-ZAIKYO-YMD9(1:6)) AND
                             (IDX26       >= IDX29               ))
      *                  初回実施日は通算実施日として記載しない
                         OR ((SANTEI-SRYYM = WRK-ZAIKYO-YMD(1:6) ) AND
                             (IDX26        = IDX28               ))
                           CONTINUE
                         ELSE
                           ADD   1      TO  IDX27
                           MOVE  SANTEI-SRYYM
                                        TO  WRK-BOKOHO-YMD2(IDX27)(1:6)
                           MOVE  IDX26  TO  WRK-BOKOHO-YMD2(IDX27)(7:2)
                         END-IF
                       END-IF
                      END-IF
                     END-PERFORM
                   END-IF
      *
                   MOVE    "tbl_santei"  TO  MCP-TABLE
                   MOVE    "key3"        TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                 END-PERFORM
               END-IF
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               IF  ((RECE41-SRYYM      >= "202008"   ) AND
                    (WRK-ZAIKYO-KAISU      NOT = ZERO))
               OR  ( RECE41-SRYYM      <  "202008"    )
                 INITIALIZE                WRK-JIDO-SETCOM-AREA
                 IF   RECE41-SRYYM  >= "202008"
                   MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                 ELSE
      ***          MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
      ***          MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                   CONTINUE
                 END-IF
                 MOVE       99      TO     FLG-COMMENT
      *
                 MOVE      ZERO     TO     IDZ
                 INITIALIZE                WRK-FREE-AREA2
      *
                 IF   RECE41-SRYYM  >= "202008"
                   MOVE   60            TO  IDX-JIDO
                   PERFORM  200111-RECECOMMENT-IDXGET-SEC
                   MOVE   WRK-ZAIKYO-YMD
                                        TO  WRK-RECECOM-YMD
                   PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                   PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                   MOVE   560           TO  IDX-JIDO
                   PERFORM  200111-RECECOMMENT-IDXGET-SEC
                   MOVE   WRK-ZAIKYO-KAISU
                                        TO  WRK-RECECOM-NUM
                   PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                   PERFORM  200111-JIDOSET-HENSYU-SEC
                 ELSE
                   ADD        1       TO     IDZ
                   MOVE       11      TO     WK-FREE-YUKOKETA2(IDZ)
                   MOVE   "初回実施　　　月　　日"
                                      TO     WK-FREE-NAME2(IDZ)
                   MOVE    WRK-ZAIKYO-YMD(5:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                                      TO     WK-FREE-NAME2(IDZ)(11:4)
                   MOVE    WRK-ZAIKYO-YMD(7:2)   TO  DAY-IDX2
                   MOVE    TBL-DAY(DAY-IDX2)
                                      TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                   PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                   IF   WRK-BOKOHO-YMD2(1) NOT =  SPACE
                     MOVE         1       TO     IDX27
                     PERFORM  VARYING  IDX26  FROM  30  BY  -1
                               UNTIL   IDX26   <     1
                      IF  WRK-BOKOHO-YMD2(IDX26)  NOT =  SPACE
                       ADD        1     TO     IDZ
      *
                       ADD        1     TO     IDX27
                       IF    IDX27   <  10
                         MOVE     11    TO     WK-FREE-YUKOKETA2(IDZ)
                         MOVE   "　回目　　　　月　　日"
                                        TO     WK-FREE-NAME2(IDZ)
                         MOVE    IDX27  TO     DAY-IDX2
                         MOVE    TBL-DAY(DAY-IDX2)(3:2)
                                        TO     WK-FREE-NAME2(IDZ)(1:2)
                       ELSE
                         MOVE     11    TO     WK-FREE-YUKOKETA2(IDZ)
                         MOVE   "　　回目　　　月　　日"
                                        TO     WK-FREE-NAME2(IDZ)
                         MOVE    IDX27  TO     DAY-IDX2
                         MOVE    TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(1:4)
                       END-IF
                       MOVE    WRK-BOKOHO-YMD2(IDX26)(5:2) TO  DAY-IDX2
                       MOVE    TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(11:4)
                       MOVE    WRK-BOKOHO-YMD2(IDX26)(7:2) TO  DAY-IDX2
                       MOVE    TBL-DAY(DAY-IDX2)
                                        TO     WK-FREE-NAME2(IDZ)(17:4)
      *
                       PERFORM  200111-JIDOSET-HENSYU-SEC
                      END-IF
                     END-PERFORM
                   END-IF
                 END-IF
      *
                 PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
               END-IF
             END-IF
           END-IF
      *
           .
       2001-BOKOHO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    高気圧酸素治療があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-KIATU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-KIATU-COMMENT    NOT =  SPACE
             MOVE   CONS-KIATU3   TO   WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             MOVE       SPACE               TO  WRK-SJIYMD
             MOVE       ZERO                TO  WRK-ZAIKYO-KAISU
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                INITIALIZE                      SANTEI-REC
                MOVE    RECE41-HOSPNUM      TO  SANTEI-HOSPNUM
                MOVE    RECE41-PTID         TO  SANTEI-PTID
                EVALUATE  RECE44-FLG-KIATU-COMMENT
                  WHEN      "1"
                    MOVE    CONS-KIATU      TO  SANTEI-SRYCD
                  WHEN      "2"
                    MOVE    CONS-KIATU2     TO  SANTEI-SRYCD
                END-EVALUATE
                MOVE    ZERO                TO  SANTEI-NYUGAIKBN
                MOVE    ZERO                TO  SANTEI-SRYKA
                MOVE    ZERO                TO  SANTEI-HKNCOMBINUM
                MOVE    SANTEI-REC          TO  MCPDATA-REC
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-DBSELECT-SEC
                IF      MCP-RC              =   ZERO
                  MOVE    "tbl_santei"      TO  MCP-TABLE
                  MOVE    "key3"            TO  MCP-PATHNAME
                  PERFORM  900-SANTEI-READ-SEC
                  PERFORM   UNTIL   FLG-SANTEI  =   1
      *
                    IF   SANTEI-SRYYM  <=  RECE41-SRYYM
                    AND  SANTEI-SRYYM  >=  WRK-ZAIKYO-YMD(1:6)
                      MOVE  WRK-ZAIKYO-YMD(7:2)  TO  IDX28
                      MOVE  WRK-ZAIKYO-YMD9(7:2) TO  IDX29
                      PERFORM  VARYING  IDX26  FROM  31  BY  -1
                                UNTIL   IDX26   <     1
                        IF   SANTEI-DAY(IDX26)  NOT =  ZERO
                          IF ((SANTEI-SRYYM =
                                      WRK-ZAIKYO-YMD(1:6) ) AND
                              (IDX26        < IDX28       ))
                          OR ((SANTEI-SRYYM =
                                      WRK-ZAIKYO-YMD9(1:6)) AND
                              (IDX26       >= IDX29       ))
                            CONTINUE
                          ELSE
                            ADD   1       TO  WRK-ZAIKYO-KAISU
                            MOVE  SANTEI-SRYYM
                                          TO  WRK-SJIYMD(1:6)
                            MOVE  IDX26   TO  WRK-SJIYMD(7:2)
                          END-IF
                        END-IF
                      END-PERFORM
                    END-IF
      *
                    MOVE    "tbl_santei"    TO  MCP-TABLE
                    MOVE    "key3"          TO  MCP-PATHNAME
                    PERFORM  900-SANTEI-READ-SEC
                  END-PERFORM
                END-IF
                MOVE    "tbl_santei"        TO  MCP-TABLE
                MOVE    "key3"              TO  MCP-PATHNAME
                PERFORM 900-CLOSE-SEC
             END-IF
      *
             IF  ((RECE41-SRYYM      >= "202008") AND
                  (WRK-SJIYMD        NOT = SPACE) AND
                  (WRK-ZAIKYO-KAISU  NOT =  ZERO))
             OR  ((RECE41-SRYYM      <  "202008") AND
                  (WRK-SJIYMD        NOT = SPACE) AND
                  (WRK-ZAIKYO-KAISU  NOT =  ZERO) AND
                  (WRK-ZAIKYO-KAISU     <=   31 ))
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                ELSE
                  MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                END-IF
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                IF   RECE41-SRYYM  >= "202008"
                  EVALUATE  RECE44-FLG-KIATU-COMMENT
                    WHEN      "1"
                      MOVE   61            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-SJIYMD    TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                      PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                      MOVE   561           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-KAISU
                                           TO  WRK-RECECOM-NUM
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                      PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                      MOVE  1061           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                      PERFORM  200111-JIDOSET-HENSYU-SEC
                    WHEN      "2"
                      MOVE   62            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-SJIYMD    TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                      PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                      MOVE   562           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-KAISU
                                           TO  WRK-RECECOM-NUM
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                      PERFORM  200111-JIDOSET-HENSYU-SEC
                  END-EVALUATE
                ELSE
                  IF    RECE44-FLG-KIATU-COMMENT  =  "1"
                    ADD        1       TO   IDZ
                    MOVE       9       TO   WK-FREE-YUKOKETA2(IDZ)
                    MOVE   "発症　　　月　　日"
                                       TO   WK-FREE-NAME2(IDZ)
                    MOVE    WRK-ZAIKYO-YMD(5:2)  TO  DAY-IDX2
                    MOVE    TBL-DAY(DAY-IDX2)
                                       TO   WK-FREE-NAME2(IDZ)(7:4)
                    MOVE    WRK-ZAIKYO-YMD(7:2)  TO  DAY-IDX2
                    MOVE    TBL-DAY(DAY-IDX2)
                                       TO   WK-FREE-NAME2(IDZ)(13:4)
      *
                    PERFORM  200111-JIDOSET-HENSYU-SEC
                  END-IF
      *
                  ADD          1       TO   IDZ
                  MOVE         11      TO   WK-FREE-YUKOKETA2(IDZ)
                  MOVE     "初回実施　　　月　　日"
                                       TO   WK-FREE-NAME2(IDZ)
                  MOVE      WRK-SJIYMD(5:2)   TO  DAY-IDX2
                  MOVE      TBL-DAY(DAY-IDX2)
                                       TO   WK-FREE-NAME2(IDZ)(11:4)
                  MOVE      WRK-SJIYMD(7:2)   TO  DAY-IDX2
                  MOVE      TBL-DAY(DAY-IDX2)
                                       TO   WK-FREE-NAME2(IDZ)(17:4)
      *
                  PERFORM    200111-JIDOSET-HENSYU-SEC
      *
                  ADD          1       TO   IDZ
                  MOVE         11      TO   WK-FREE-YUKOKETA2(IDZ)
                  MOVE     "通算実施回数（　　回）"
                                       TO   WK-FREE-NAME2(IDZ)
                  MOVE      WRK-ZAIKYO-KAISU  TO  DAY-IDX2
                  MOVE      TBL-DAY(DAY-IDX2)
                                       TO   WK-FREE-NAME2(IDZ)(15:4)
      *
                  PERFORM    200111-JIDOSET-HENSYU-SEC
                END-IF
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-KIATU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    人工腎臓等のコメントがあるか判定
      *  ＃202004選択式コメント対応
      *****************************************************************
       2001-JINZOUETC-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-JINZOU-COMMENT   =    "1"
             IF   RECE41-SRYYM  >= "201810"
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               IF   RECE41-SRYYM  >= "202004"
                 MOVE     2       TO     WRK-JIDO-JYOUGEFLG
               ELSE
                 MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                 MOVE     1       TO     WRK-JIDO-JYOUGEFLG
               END-IF
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             ELSE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               MOVE       3       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             END-IF
           END-IF
      *
           .
       2001-JINZOUETC-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    局所陰圧閉鎖処置のコメントがあるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-KYOKUIN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-KYOKUIN-COMMENT   =    "1"
             IF     RECE41-SRYYM  >= "202008"
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 MOVE      "2"      TO     WRK-JIDO-SETCOMFLG
                 MOVE       99      TO     FLG-COMMENT
                 MOVE       59      TO     IDX-JIDO
                 PERFORM    200111-RECECOMMENT-IDXGET-SEC
                 PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
             ELSE
               IF   RECE41-SRYYM  >= "201810"
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE       2       TO     WRK-JIDO-SETCOMATFLG
                 MOVE       1       TO     WRK-JIDO-JYOUGEFLG
                 MOVE       3       TO     FLG-COMMENT
                 PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
               ELSE
                 INITIALIZE            WRK-JIDO-SETCOM-AREA
                 MOVE       3       TO     FLG-COMMENT
                 PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
               END-IF
             END-IF
           END-IF
      *
           .
       2001-KYOKUIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日のコメントがあるか判定
      *  ＃202004選択式コメント対応
      *****************************************************************
       2001-SHUJUTSU-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-SHUJUTU-COMMENT   =    SPACE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               IF   RECE41-SRYYM  >= "202004"
                 MOVE     2       TO     WRK-JIDO-JYOUGEFLG
               END-IF
               MOVE       1       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE44-TENSOU-SEC
           END-IF
      *
           .
       2001-SHUJUTSU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻酔日のコメントがあるか判定
      *  ＃202004選択式コメント対応
      *****************************************************************
       2001-MASUI-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-MASUI-COMMENT   =    SPACE
               INITIALIZE            WRK-JIDO-SETCOM-AREA
               IF   RECE41-SRYYM  >= "202004"
                 MOVE     2       TO     WRK-JIDO-JYOUGEFLG
               END-IF
               MOVE       8       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
           ELSE
               PERFORM    2099-RECE44-TENSOU-SEC
           END-IF
      *
           .
       2001-MASUI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （検選）のコメントがあるか判定
      *****************************************************************
       2001-KENSEN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    （検選）等のコメント編集
      *****************************************************************
       20011-KENSENTOU-COMMENT-SEC          SECTION.
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           PERFORM
             UNTIL  ( FLG-RECE45           =   1                   )
             OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
             OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
             OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
             OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
             OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
             OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
             OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
             OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE45-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
      *++++++
                  IF    IDX-RECE45  =  RECE44-MEISAISU
      *
                    IF  WRK-RECE45XX-SRYCD(IDX-RECE45 1)(1:1)  = "8"
                    OR  WRK-RECE45XX-SRYCD(IDX-RECE45 1)(1:3)  = "008"
      *
                        COMPUTE   IDV  =
                         (WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) * 2) + 1
      *
                        MOVE    1    TO   IDV2
                        IF  WRK-RECE45XX-INPUTCOMENT(IDX-RECE45 1)
                                                           NOT = SPACE
                          PERFORM  VARYING  IDV3  FROM  140  BY  -1
                                    UNTIL   IDV3    <     1
                            IF  WRK-RECE45XX-INPUTCOMENT(IDX-RECE45 1)
                                                 (IDV3:1)  NOT = SPACE
                              COMPUTE  IDV2 = IDV2 + IDV3
                              MOVE     1   TO IDV3
                            END-IF
                          END-PERFORM
                        END-IF
      *
                        IF   IDV2  <= 131
                          MOVE    "　×"   TO
                          WRK-RECE45XX-INPUTCOMENT(IDX-RECE45 1)(IDV2:4)
                        END-IF
                        IF   IDV   <= 291
                          MOVE    "　×"   TO
                          WRK-RECE45XX-NAME(IDX-RECE45 1)(IDV:4)
                        END-IF
      *
                        COMPUTE   IDV  =  IDV  +  4
                        COMPUTE   IDV2 =  IDV2 +  4
      *
                        MOVE    RECE44-ZAIKAISU   TO   DAY-IDX4
                        PERFORM 20011-KAISU-HENSYU2-SEC
                        IF   IDV2  <= 135
                          MOVE    WRK-HENSYU-KAISU4  TO
                          WRK-RECE45XX-INPUTCOMENT(IDX-RECE45 1)(IDV2:6)
                        END-IF
                        IF   IDV   <= 295
                          MOVE    WRK-HENSYU-KAISU4  TO
                          WRK-RECE45XX-NAME(IDX-RECE45 1)(IDV:6)
                          COMPUTE WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) =
                                  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) +
                                  5
                        END-IF
      *
                    END-IF
      *
                  END-IF
      *++++++
             END-PERFORM 
      *
             PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
      *    手術日コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE459-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE459-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE459-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE459-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE459-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE459-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE459-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE459-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                     OR (IDX-RECE45                       >  100 )
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE459-SRYACT-TBL(IDX)
      *
                         ADD    1    TO   WRK-CNT-MEISAI
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE459-SRYCD(1)  NOT =  SPACE
                      PERFORM 800-RECE459-WRITE-SEC
                  END-IF
           END-PERFORM     
      *
           MOVE    RECE44-REC        TO      RECE449-REC
      *@@@@
           MOVE    RECE44-ZAINUM     TO      RECE449-ZAINUM
           MOVE    RECE44-SRYKBN     TO      RECE449-SRYKBN
      *@@@@
           MOVE    WRK-CNT-MEISAI    TO      RECE449-MEISAISU
           PERFORM 800-RECE449-WRITE-SEC
      *
           .
       20011-KENSENTOU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）があるか判定
      *  ＃202010選択式コメント対応
      *****************************************************************
       2001-KENSA-HANTEI-SEC          SECTION.
      *
           IF  ((RECE41-SRYYM  >= "201804"       ) AND
                (RECE44-FLG-KENSA-COMMENT  =  "1") AND
                (CNT-KENSA                >=   2 ))
           OR  ((RECE41-SRYYM  <= "201803"       ) AND
                (RECE44-FLG-KENSA-COMMENT  =  "1"))
               INITIALIZE            WRK-JIDO-SETCOM-AREA
      ***      MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
      ***      MOVE       1       TO     WRK-JIDO-SETCOMATFLG
               IF   RECE41-SRYYM  >= "202010"
                 MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                 MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                 MOVE    "1"      TO     WRK-JIDO-RECEDENFLG
               END-IF
               MOVE       7       TO     FLG-COMMENT
               PERFORM    20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KENSA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外緊急院内検査加算があるか判定
      *  ＃202007コメント対応「840000099」
      *****************************************************************
       2001-KENKSN-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-KENKSN2-COMMENT   =    "1"
                INITIALIZE          WRK-JIDO-SETCOM-AREA
                MOVE    "3"      TO     WRK-JIDO-SETCOMFLG
                MOVE     99      TO     FLG-COMMENT
                PERFORM  20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KENKSN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＴ・ＭＲＩがあるか判定
      *****************************************************************
       2001-CTMRI-HANTEI-SEC          SECTION.
      *
           IF    ( RECE44-FLG-CTMRI-COMMENT   =    "1"  )
           OR    ((RECE44-FLG-CTMRI-COMMENT   =   SPACE) AND
                  (RECE44-SENTAKUBUIFLG   NOT =   SPACE))
             IF    RECE44-FLG-CTMRI-COMMENT   =    "1"
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       2       TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC-COMMENT-SEC
             ELSE
                INITIALIZE            WRK-JIDO-SETCOM-AREA
                MOVE       99      TO     FLG-COMMENT
                PERFORM    20011-SHUJUTSU-ETC3-COMMENT-SEC
             END-IF
           END-IF
      *
           .
       2001-CTMRI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外緊急院内画像診断加算があるか判定
      *  ＃202007コメント対応「840000099」
      *****************************************************************
       2001-KINGAZ-HANTEI-SEC          SECTION.
      *
           IF      RECE44-FLG-KINGAZ2-COMMENT   =    "1"
                INITIALIZE          WRK-JIDO-SETCOM-AREA
                MOVE    "3"      TO     WRK-JIDO-SETCOMFLG
                MOVE     99      TO     FLG-COMMENT
                PERFORM  20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-KINGAZ-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等のコメント編集（差込：上）
      *****************************************************************
       20011-SHUJUTSU-ETC-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           PERFORM  20011-FREE-COMMENT2-SEC                              
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1                  TO  IDX-RECE45
                MOVE   "810000001"
                              TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO   TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE    "1"   TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
                IF     WRK-JIDO-JYOUGEFLG  NOT = ZERO
                  MOVE  "1"   TO  WRK-RECE45XX-RECEDENFLG(IDX-RECE45 1)
                END-IF
           END-PERFORM 
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等のコメント編集２（差込：下）
      *****************************************************************
       20011-SHUJUTSU-ETC2-COMMENT-SEC          SECTION.
      *
           PERFORM   200111-SANTEI-DAY-SEC
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
      *
           IF  ((RECE41-SRYYM       >=  "202008") AND
                (WRK-JIDO-SETCOMFLG  =  "2"     ))
           OR  ((RECE41-SRYYM       >=  "202007") AND
                (WRK-JIDO-SETCOMFLG  =  "3"     ))
             PERFORM  VARYING  IDX  FROM  1  BY  1
                       UNTIL  (IDX                 >    31)
                         OR   (TB-SANTEI-DAY(IDX)  =  ZERO)
               IF  WRK-JIDO-SETCOMFLG  =  "2"
                 MOVE  RECE41-SRYYM       TO  WRK-RECECOM-YMD(1:6)
                 MOVE  TB-SANTEI-DAY(IDX) TO  WRK-RECECOM-YMD(7:2)
                 PERFORM  200111-RECECOMMENT-HENSYU-SEC
               ELSE
                 INITIALIZE                   WRK-RECECOM-AREA
                 MOVE  RECE41-SRYYM       TO  WRK-RECECOM-YMD(1:6)
                 MOVE  TB-SANTEI-DAY(IDX) TO  WRK-RECECOM-YMD(7:2)
                 PERFORM  200111-RECECOMMENT-HENSYU3-SEC
               END-IF
             END-PERFORM
           ELSE
             PERFORM  20011-FREE-COMMENT2-SEC
           END-IF
      *
           PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
           .
       20011-SHUJUTSU-ETC2-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等のコメント編集（差込：上）
      *    ※剤先頭に部位コメントを差し込むために＠明細追加する
      *****************************************************************
       20011-SHUJUTSU-ETC3-COMMENT-SEC          SECTION.
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           PERFORM  VARYING  IDX50  FROM  1  BY  1
                     UNTIL  (IDX50 >   10                )
                      OR    (IDX50 > RECE44-SENTAKUBUICNT)
             ADD   1           TO IDX-RECE45
             MOVE  "810000001" TO WRK-RECE45XX-SRYCD(IDX-RECE45 1)
             MOVE  ZERO        TO WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
             MOVE  ZERO        TO WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
             MOVE  SPACE       TO WRK-RECE45XX-NAME(IDX-RECE45 1)
             MOVE  "1"         TO WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
             MOVE  "1"         TO WRK-RECE45XX-RECEDENFLG(IDX-RECE45 1)
             MOVE  "1"         TO WRK-RECE45XX-RECEPTFLG(IDX-RECE45 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-SHUJUTSU-ETC3-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療分のコメント編集（差込：上）
      *****************************************************************
       20011-HOKATU-COMMENT-SEC          SECTION.
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           ADD    1             TO  IDX-RECE45
           MOVE  "810000001"    TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
           MOVE   ZERO          TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
           MOVE   6             TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
           MOVE  "【包括診療】" TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
           MOVE  "1"            TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
      *
           IF    (RECE44-FLG-CTMRI-COMMENT   =   SPACE)
           AND   (RECE44-SENTAKUBUIFLG   NOT =   SPACE)
            PERFORM  VARYING  IDX50  FROM  1  BY  1
                      UNTIL  (IDX50 >    9                )
                       OR    (IDX50 > RECE44-SENTAKUBUICNT)
             ADD   1            TO  IDX-RECE45
             MOVE  "810000001"  TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
             MOVE  ZERO         TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
             MOVE  ZERO         TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
             MOVE  SPACE        TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
             MOVE  "1"          TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
             MOVE  "1"          TO  WRK-RECE45XX-RECEDENFLG
                                                     (IDX-RECE45 1)
             MOVE  "1"          TO  WRK-RECE45XX-RECEPTFLG(IDX-RECE45 1)
            END-PERFORM
           END-IF
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-HOKATU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定日の確定
      *****************************************************************
       200111-SANTEI-DAY-SEC          SECTION.
      *
           INITIALIZE                TBL-SANTEI-DAY
      *
           MOVE    ZERO          TO  DAY-IDX
           PERFORM    VARYING    IDX  FROM   1   BY   1
                         UNTIL     (IDX      >   31)  OR
                                   (DAY-IDX  >   31)
                   IF    RECE44-KAISU(1 IDX)  NOT =  ZERO
                       ADD   1     TO   DAY-IDX
                       MOVE  IDX   TO  TB-SANTEI-DAY(DAY-IDX)
                   END-IF
           END-PERFORM
      *
           .
       200111-SANTEI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等の日付の編集
      *****************************************************************
       20011-FREE-COMMENT2-SEC          SECTION.                         
      *
           ADD      1          TO    IDZ
      *
           EVALUATE   FLG-COMMENT
              WHEN    1
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "手術"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    2
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "実施"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    3
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "算定"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    4
                 IF   RECE41-SRYYM >= "201905"
                   MOVE     22         TO    WRK-KETA
                                             WRK-KETA-TAIHI
                   MOVE     11         TO    WRK-KETA2
                   MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                   MOVE    "交付　　　　　年　　月"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
                   MOVE     11         TO     IDX41
                   MOVE     17         TO     IDX42
                   MOVE      7         TO     IDX49
                 ELSE
                   MOVE     18         TO    WRK-KETA
                                             WRK-KETA-TAIHI
                   MOVE     9          TO    WRK-KETA2
                   MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                   MOVE    "交付　　　年　　月"
                                 TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
                   MOVE      7         TO     IDX41
                   MOVE     13         TO     IDX42
                 END-IF
      *
                 INITIALIZE                      STS-AREA-DAY
                 INITIALIZE                      LNK-DAY2-AREA
                 MOVE    "21"                TO  LNK-DAY2-IRAI
                 MOVE    RECE41-SRYYM        TO  WRK-HENYMD(1:6)
                 MOVE    "01"                TO  WRK-HENYMD(7:2)
                 MOVE    WRK-HENYMD          TO  LNK-DAY2-YMD
                 CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                             LNK-DAY2-AREA
                 IF  STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                   END-IF
      *
                   MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                   INITIALIZE                       ORCSKANACHKAREA
                   MOVE    "2"                  TO  KANACHK-SYORI
                   MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                   TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                   END-IF
      *
                   IF   RECE41-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                   TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                   END-IF
                 END-IF
              WHEN    6
                 MOVE     0          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     0          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
              WHEN    7
                 MOVE     8          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     4          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "検査実施"
                             TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    8
                 MOVE     4          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     2          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "麻酔"  TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
              WHEN    9
                 MOVE     8          TO    WRK-KETA
                                           WRK-KETA-TAIHI
                 MOVE     4          TO    WRK-KETA2
                 MOVE    WRK-KETA2   TO    WK-FREE-YUKOKETA2(IDZ)
                 MOVE    "訪問指導"
                             TO    WK-FREE-NAME2(IDZ)(1:WRK-KETA)
           END-EVALUATE
      *
           PERFORM  200111-JIDOSET-HENSYU-SEC
      *
           MOVE      ZERO          TO  FLG-FIRST
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                          UNTIL   (IDX                 >    31)
                           OR     (TB-SANTEI-DAY(IDX)  =  ZERO)
      *
                 IF  FLG-FIRST  =  ZERO
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                       MOVE    "　"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    TBL-DAY(IDY)(3:2)     
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                       MOVE    "日"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF   FLG-COMMENT   =   4 OR 6
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       ELSE
                         ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                         MOVE    "　"
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                         ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                         COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                         MOVE    TBL-DAY(IDY)     
                             TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
                       END-IF
                       ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                       COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                       MOVE    "日"
                           TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                    END-IF
                    MOVE      1              TO  FLG-FIRST
                 ELSE
                    IF  TB-SANTEI-DAY(IDX)     <  10 
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  24 OR 25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)(3:2)     
                                    TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2) 
                       END-IF
                    ELSE
      *
                       MOVE   TB-SANTEI-DAY(IDX)  TO  IDY
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    "、"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    "、"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
      *
                       IF    WK-FREE-YUKOKETA2(IDZ)  =  23 OR 24 OR 25
                          PERFORM  200111-FREE-COMMENT2-KAIGYO-SEC
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  1
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       ELSE
                          ADD   2  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                          MOVE    TBL-DAY(IDY)     
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:4)
      *
                          ADD   1  TO  WK-FREE-YUKOKETA2(IDZ)
                          COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                          MOVE    "日"
                                   TO  WK-FREE-NAME2(IDZ)(WRK-KETA:2)
                       END-IF
                    END-IF
                 END-IF
      *
           END-PERFORM
           .
       20011-FREE-COMMENT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術日等の日付の編集（改行）
      *****************************************************************
       200111-FREE-COMMENT2-KAIGYO-SEC          SECTION.                  
      *
           ADD      1              TO  IDZ
      *
           MOVE    WRK-KETA-TAIHI  TO  WRK-KETA
           MOVE    WRK-KETA2       TO  WK-FREE-YUKOKETA2(IDZ)
           MOVE    ALL "　"        TO  WK-FREE-NAME2(IDZ)(1:WRK-KETA)
      *
           PERFORM  200111-JIDOSET-HENSYU-SEC
      *
           .
       200111-FREE-COMMENT2-KAIGYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリのコメント編集
      *****************************************************************
       2001-RIHA-COMMENT-SEC          SECTION.
      *
             INITIALIZE            WRK-JIDO-SETCOM-AREA
             MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
             MOVE       1       TO     WRK-JIDO-SETCOMATFLG
      *
             PERFORM  200111-RECE45-TAIHI-SEC
      *
      *      ＲＥＣＥ０５ファイルの内容をテーブルに退避
             MOVE    ZERO                 TO  IDX-RECE45
             MOVE    SPACE                TO  WRK-RECE45XX-REC
             INITIALIZE                       WRK-RECE45XX-REC
      *      リハビリコメントの挿入
             COMPUTE  MAX-MEISAISU        =   RECE44-MEISAISU  +  1
      *
             MOVE    "810000001"
                            TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
             MOVE    ZERO   TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
             MOVE    7      TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
             MOVE    "実施日数　　日"
                            TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
             MOVE    "1"    TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
             MOVE    ZERO                 TO  CNT-RIHA
             PERFORM  VARYING   IDX  FROM  1  BY  1
                       UNTIL    IDX   >   31
                 IF   RECE44-KAISU(1 IDX)  NOT = ZERO
                        ADD      1        TO  CNT-RIHA
                 END-IF
             END-PERFORM
             IF      CNT-RIHA             NOT =    ZERO
               MOVE    CNT-RIHA             TO  DAY-IDX
               MOVE    TBL-DAY(DAY-IDX)
                           TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(9:4)
             END-IF
      *
             IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
               MOVE  "2"   TO  WRK-RECE45XX-MEISKYFLG(MAX-MEISAISU 1)
               MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE45XX-SETCOMATFLG(MAX-MEISAISU 1)
               MOVE  WRK-JIDO-RECEDENFLG
                           TO  WRK-RECE45XX-RECEDENFLG(MAX-MEISAISU 1)
             END-IF
      *
             PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       2001-RIHA-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ医学管理料のコメント編集
      *****************************************************************
       2001-RIHAIGAKU-COMMENT-SEC          SECTION.
      *
           INITIALIZE            WRK-JIDO-SETCOM-AREA
      *
           EVALUATE      RECE44-RIHABIRI-COMMENT
              WHEN           "2"
                  MOVE   CONS-SINDAI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "3"
                  MOVE   CONS-SINDAI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-SINDAI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "4"
                  MOVE   CONS-KOKYUKI-IGAKU1  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "5"
                  MOVE   CONS-KOKYUKI-IGAKU2  TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-KOKYUKI-DUMMY   TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "6"
                  MOVE   CONS-NOUKE-IGAKU1    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "7"
                  MOVE   CONS-NOUKE-IGAKU2    TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-NOUKE-DUMMY     TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "8"
                  MOVE   CONS-UNDOKI-IGAKU1   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
              WHEN           "9"
                  MOVE   CONS-UNDOKI-IGAKU2   TO  WRK-RIHAIGAKU-SRYCD
                  MOVE   CONS-UNDOKI-DUMMY    TO  WRK-RIHAIGAKU-SRYCD2
           END-EVALUATE
      ***
           MOVE          SPACE          TO  WRK-RIHAIGAKU-FSANTEIYMD
      *    医学管理：算定履歴検索
           INITIALIZE                       SANTEI-REC
           MOVE    RECE41-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID          TO  SANTEI-PTID
           MOVE    RECE41-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD  TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI      TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
                MOVE  SANTEI-FSANTEIYMD TO  WRK-RIHAIGAKU-FSANTEIYMD
           END-IF
      ***
           INITIALIZE      WRK-RIHAIGAKU-SANTEI-AREA
      *    リハ実施ダミー：算定履歴検索
           INITIALIZE                       SANTEI-REC
           MOVE    RECE41-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID          TO  SANTEI-PTID
           MOVE    RECE41-SRYYM         TO  SANTEI-SRYYM
           MOVE    WRK-RIHAIGAKU-SRYCD2 TO  SANTEI-SRYCD
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    RECE41-HKNCOMBI      TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL   IDX    >  31
                 IF   SANTEI-DAY(IDX)   NOT =  ZERO
                    MOVE  "1"  TO  WRK-RIHAIGAKU-SANTEI-FLG
                    MOVE  SANTEI-DAY(IDX)
                               TO  WRK-RIHAIGAKU-SANTEI-DAY(IDX)
                 END-IF
               END-PERFORM
           END-IF
      *
      ***
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           COMPUTE  MAX-MEISAISU        =   RECE44-MEISAISU  +  1
      *
           MOVE    "810000001"
                         TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
           MOVE    ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
           MOVE    9     TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
           MOVE    "開始　　　月　　日"
                         TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
           MOVE    "1"   TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           IF      WRK-RIHAIGAKU-FSANTEIYMD  NOT = SPACE
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(5:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(7:4)
             MOVE    WRK-RIHAIGAKU-FSANTEIYMD(7:2)   TO  DAY-IDX2
             MOVE    TBL-DAY(DAY-IDX2)
                         TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)(13:4)
           END-IF
      *
           IF      WRK-RIHAIGAKU-SANTEI-FLG  NOT = SPACE
             PERFORM    20011-RIHAIGAKU-DUMMY-COM-SEC
           ELSE
             PERFORM    200111-SRYACTINF-HENSYU-SEC
           END-IF
      *
           .
       2001-RIHAIGAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ医学管理料のコメント編集（リハ実施日）
      *****************************************************************
       20011-RIHAIGAKU-DUMMY-COM-SEC          SECTION.
      *
           INITIALIZE             TBL-SANTEI-DAY
      *
           MOVE     ZERO      TO   DAY-IDX
           PERFORM  VARYING  IDX  FROM   1   BY   1
                     UNTIL   IDX    >   31
             IF  WRK-RIHAIGAKU-SANTEI-DAY(IDX)  NOT = ZERO
                 ADD    1     TO   DAY-IDX
                 MOVE   IDX   TO   TB-SANTEI-DAY(DAY-IDX)
             END-IF
           END-PERFORM
      *
           MOVE     ZERO      TO   IDZ
           INITIALIZE         WRK-FREE-AREA2
      *
           MOVE     2         TO   FLG-COMMENT
           PERFORM  20011-FREE-COMMENT2-SEC
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       20011-RIHAIGAKU-DUMMY-COM-EXT.
           EXIT.
      *
      *****************************************************************
      *    （リハ選）のコメントがあるか判定
      *****************************************************************
       2001-RIHASEN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-SAFLG   =    "2"
               MOVE       99      TO     FLG-COMMENT
               PERFORM    20011-KENSENTOU-COMMENT-SEC
           END-IF
      *
           .
       2001-RIHASEN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    認知療法・認知行動療法があるか判定
      *    精神科重症患者早期集中支援管理料があるか判定
      *    精神科在宅患者支援管理料があるか判定
      *    在宅半固形栄養経管栄養法指導管理料があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-NINRYO-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-NINRYO-COMMENT   NOT =  SPACE
             IF  (RECE44-FLG-NINRYO-COMMENT   =
                 "A" OR "B" OR "G" OR "H" OR "C" OR "D" OR
                 "E" OR "F" OR "I" OR "J" OR "P" OR "Q"       )
             AND (RECE44-FLG-ONLINE-COMMENT   =  "4"          )
               CONTINUE
             ELSE
               PERFORM   2001-NINRYO-HENSYU-SEC
               IF   WRK-NINRYO-FSANTEIYMD   NOT =  SPACE
      *
                  INITIALIZE                WRK-JIDO-SETCOM-AREA
                  IF   RECE41-SRYYM  >= "202008"
                    MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                  ELSE
                    MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                    MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                  END-IF
                  MOVE       99      TO     FLG-COMMENT
      *
                  MOVE      ZERO     TO     IDZ
                  INITIALIZE                WRK-FREE-AREA2
      *
                  IF   RECE41-SRYYM  >= "202008"
                    EVALUATE  RECE44-FLG-NINRYO-COMMENT
                      WHEN     "4"
                        MOVE   41            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "5"
                        MOVE   42            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "7"
                        MOVE   43            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "8"
                        MOVE   44            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "A"
                        MOVE   45            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "B"
                        MOVE   46            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "G"
                        MOVE   47            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "H"
                        MOVE   48            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "C"
                        MOVE   49            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "D"
                        MOVE   50            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "E"
                        MOVE   51            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "F"
                        MOVE   52            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "I"
                        MOVE   53            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "J"
                        MOVE   54            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "P"
                        MOVE   55            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "Q"
                        MOVE   56            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                      WHEN     "9"
                        MOVE   57            TO  IDX-JIDO
                        PERFORM  200111-RECECOMMENT-IDXGET-SEC
                        MOVE   WRK-NINRYO-FSANTEIYMD
                                             TO  WRK-RECECOM-YMD
                        PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    END-EVALUATE
                  ELSE
                    ADD       1       TO     IDZ
                    MOVE      11      TO     WK-FREE-YUKOKETA2(IDZ)
                    MOVE  "初回算定　　　月　　日"
                                      TO     WK-FREE-NAME2(IDZ)
                    MOVE  WRK-NINRYO-FSANTEIYMD(5:2)   TO  DAY-IDX2
                    MOVE  TBL-DAY(DAY-IDX2)
                                      TO     WK-FREE-NAME2(IDZ)(11:4)
                    MOVE  WRK-NINRYO-FSANTEIYMD(7:2)   TO  DAY-IDX2
                    MOVE  TBL-DAY(DAY-IDX2)
                                      TO     WK-FREE-NAME2(IDZ)(17:4)
                  END-IF
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                  PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
               END-IF
             END-IF
           END-IF
      *
           .
       2001-NINRYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       2001-NINRYO-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE                TO  WRK-NINRYO-FSANTEIYMD
           INITIALIZE                       SANTEI-REC
           MOVE    RECE41-HOSPNUM       TO  SANTEI-HOSPNUM
           MOVE    RECE41-PTID          TO  SANTEI-PTID
           MOVE    RECE41-SRYYM         TO  SANTEI-SRYYM
           EVALUATE  RECE44-FLG-NINRYO-COMMENT
             WHEN      "1"
               MOVE  CONS-NINRYO        TO  SANTEI-SRYCD
             WHEN      "2"
               MOVE  CONS-NINRYO2       TO  SANTEI-SRYCD
             WHEN      "4"
               MOVE  CONS-NINRYO4       TO  SANTEI-SRYCD
             WHEN      "5"
               MOVE  CONS-NINRYO5       TO  SANTEI-SRYCD
             WHEN      "7"
               MOVE  CONS-NINRYO7       TO  SANTEI-SRYCD
             WHEN      "8"
               MOVE  CONS-NINRYO8       TO  SANTEI-SRYCD
      *
             WHEN      "9"
               MOVE  CONS-ZAIHANKO      TO  SANTEI-SRYCD
      *
             WHEN      "A"
               MOVE  CONS-SEIZAI        TO  SANTEI-SRYCD
             WHEN      "B"
               MOVE  CONS-SEIZAI2       TO  SANTEI-SRYCD
             WHEN      "C"
               MOVE  CONS-SEIZAI3       TO  SANTEI-SRYCD
             WHEN      "D"
               MOVE  CONS-SEIZAI4       TO  SANTEI-SRYCD
             WHEN      "E"
               MOVE  CONS-SEIZAI5       TO  SANTEI-SRYCD
             WHEN      "F"
               MOVE  CONS-SEIZAI6       TO  SANTEI-SRYCD
             WHEN      "G"
               MOVE  CONS-SEIZAI7       TO  SANTEI-SRYCD
             WHEN      "H"
               MOVE  CONS-SEIZAI8       TO  SANTEI-SRYCD
             WHEN      "I"
               MOVE  CONS-SEIZAI9       TO  SANTEI-SRYCD
             WHEN      "J"
               MOVE  CONS-SEIZAI10      TO  SANTEI-SRYCD
             WHEN      "P"
               MOVE  CONS-SEIZAI11      TO  SANTEI-SRYCD
             WHEN      "Q"
               MOVE  CONS-SEIZAI12      TO  SANTEI-SRYCD
      *
             WHEN      "K"
               MOVE  CONS-SEIJYU        TO  SANTEI-SRYCD
             WHEN      "L"
               MOVE  CONS-SEIJYU2       TO  SANTEI-SRYCD
             WHEN      "M"
               MOVE  CONS-SEIJYU3       TO  SANTEI-SRYCD
             WHEN      "N"
               MOVE  CONS-SEIJYU4       TO  SANTEI-SRYCD
           END-EVALUATE
           MOVE    ZERO                 TO  SANTEI-NYUGAIKBN
           MOVE    ZERO                 TO  SANTEI-SRYKA
           MOVE    ZERO                 TO  SANTEI-HKNCOMBINUM
           MOVE    SANTEI-REC           TO  MCPDATA-REC
           MOVE    "tbl_santei"         TO  WRK-MCP-TABLE
           MOVE    "key"                TO  WRK-MCP-PATHNAME
           PERFORM  900-SANTEI-INV-SEC
           IF       FLG-SANTEI        =   ZERO
              MOVE  SANTEI-FSANTEIYMD   TO  WRK-NINRYO-FSANTEIYMD
           END-IF
      *
           .
       2001-NINRYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    通院・在宅精神療法（２０歳未満）加算があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-TUINZAI-HANTEI-SEC          SECTION.
      *
           IF  ((RECE41-SRYYM                >=  "201604"         ) AND
                (RECE44-FLG-TUINZAI-COMMENT   =  "1"              ) AND
                (RECE44-FLG-TUINZAI2-COMMENT  NOT =  SPACE        ))
           OR  ((RECE41-SRYYM                <=  "201603"         ) AND
                (RECE44-FLG-TUINZAI-COMMENT   =  "1"              ))
             MOVE  CONS-TUINZAI  TO  WRK-CHECK-SRYCD
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
      *
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                ELSE
                  MOVE    "1"      TO     WRK-JIDO-SETCOMFLG
                  MOVE     1       TO     WRK-JIDO-SETCOMATFLG
                END-IF
                MOVE       99      TO     FLG-COMMENT
      *
                MOVE      ZERO     TO     IDZ
                INITIALIZE                WRK-FREE-AREA2
      *
                IF   RECE41-SRYYM  >= "202008"
                  EVALUATE  RECE44-FLG-TUINZAI2-COMMENT
                    WHEN     "1"
                      MOVE   105           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN     "2"
                      MOVE   106           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN     "3"
                      MOVE   107           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN     "4"
                      MOVE   135           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-EVALUATE
                ELSE
                  ADD        1       TO     IDZ
                  IF   RECE41-SRYYM >= "201905"
                    MOVE     24      TO     WK-FREE-YUKOKETA2(IDZ)
                MOVE "精神科を初めて受診した日　　　　　年　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                    MOVE     31      TO     IDX41
                    MOVE     37      TO     IDX42
                    MOVE     43      TO     IDX48
                    MOVE     27      TO     IDX49
                  ELSE
                    MOVE     22      TO     WK-FREE-YUKOKETA2(IDZ)
                    MOVE "精神科を初めて受診した日　　　年　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                    MOVE     27      TO     IDX41
                    MOVE     33      TO     IDX42
                    MOVE     39      TO     IDX48
                  END-IF
      *
                  INITIALIZE                         STS-AREA-DAY
                  INITIALIZE                         LNK-DAY2-AREA
                  MOVE    "21"                   TO  LNK-DAY2-IRAI
                  MOVE    WRK-ZAIKYO-YMD         TO  LNK-DAY2-YMD
                  CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  IF      STS-DAY-RC1            =   ZERO
                    MOVE    LNK-DAY2-EDTYMD1-Y   TO  WRK-TAIHI-XX
                    INITIALIZE                       ORCSKANACHKAREA
                    MOVE    "2"                  TO  KANACHK-SYORI
                    MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                    CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                    IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                     TO  WK-FREE-NAME2(IDZ)(IDX41:4)
                    END-IF
      *
                    MOVE    LNK-DAY2-EDTYMD1-M   TO  WRK-TAIHI-XX
                    INITIALIZE                       ORCSKANACHKAREA
                    MOVE    "2"                  TO  KANACHK-SYORI
                    MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                    CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                    IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                     TO  WK-FREE-NAME2(IDZ)(IDX42:4)
                    END-IF
      *
                    MOVE    LNK-DAY2-EDTYMD1-D   TO  WRK-TAIHI-XX
                    INITIALIZE                       ORCSKANACHKAREA
                    MOVE    "2"                  TO  KANACHK-SYORI
                    MOVE    WRK-TAIHI-XX         TO  KANACHK-MAE-INPUT
                    CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
                    IF      KANACHK-RC      =   ZERO
                      MOVE    KANACHK-OUT-INPUT(1:4)
                                     TO  WK-FREE-NAME2(IDZ)(IDX48:4)
                    END-IF
      *
                    IF   RECE41-SRYYM >= "201905"
                      MOVE    LNK-DAY2-EDTYMD2-G
                                     TO  WK-FREE-NAME2(IDZ)(IDX49:4)
                    END-IF
                  END-IF
                END-IF
      *
                PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-TUINZAI-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    依存症集団療法があるか判定
      *    疾患別等専門プログラム加算があるか判定
      *    経頭蓋磁気刺激療法があるか判定
      *  ＃202008選択式コメント対応
      *****************************************************************
       2001-IZONSYO-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-IZONSYO-COMMENT   NOT =  SPACE
             MOVE         ZERO     TO     IDZ
             INITIALIZE                   WRK-FREE-AREA2
      *      ＃202011選択式コメント対応
             IF   RECE44-FLG-SEIKEA2-COMMENT  =  "1"
                MOVE  CONS-SEIKEA  TO  WRK-CHECK-SRYCD
                PERFORM       20011-ZAIKYO-CHECK-SEC
                IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                   INITIALIZE                WRK-JIDO-SETCOM-AREA
                   MOVE      "2"      TO     WRK-JIDO-SETCOMFLG
      *
                   MOVE       99      TO     FLG-COMMENT
      *
                   MOVE     114       TO     IDX-JIDO
                   PERFORM  200111-RECECOMMENT-IDXGET-SEC
                   MOVE     WRK-ZAIKYO-YMD
                                      TO     WRK-RECECOM-YMD
                   PERFORM  200111-RECECOMMENT-HENSYU-SEC
                END-IF
             END-IF
      *      ＃202207選択式コメント対応
             IF   RECE44-FLG-SEISOKI-COMMENT  =  "1"
                MOVE  CONS-SEIKEA  TO  WRK-CHECK-SRYCD
                PERFORM       20011-ZAIKYO-CHECK-SEC
                IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                   INITIALIZE                WRK-JIDO-SETCOM-AREA
                   MOVE      "2"      TO     WRK-JIDO-SETCOMFLG
      *
                   MOVE       99      TO     FLG-COMMENT
      *
                   MOVE     123       TO     IDX-JIDO
                   PERFORM  200111-RECECOMMENT-IDXGET-SEC
                   MOVE     WRK-ZAIKYO-YMD
                                      TO     WRK-RECECOM-YMD
                   PERFORM  200111-RECECOMMENT-HENSYU-SEC
                END-IF
             END-IF
      *
             EVALUATE  RECE44-FLG-IZONSYO-COMMENT
               WHEN      "1"
                 MOVE  CONS-IZONSYO  TO  WRK-CHECK-SRYCD
               WHEN      "2"
                 MOVE  CONS-SCPRO    TO  WRK-CHECK-SRYCD
               WHEN      "3"
                 MOVE  CONS-IZONSYO2 TO  WRK-CHECK-SRYCD
               WHEN      "4"
                 MOVE  CONS-JIKSGK   TO  WRK-CHECK-SRYCD
               WHEN      "5"
                 MOVE  CONS-IZONSYO3 TO  WRK-CHECK-SRYCD
             END-EVALUATE
             PERFORM       20011-ZAIKYO-CHECK-SEC
      *
             IF   RECE44-FLG-IZONSYO-COMMENT  =  "4"
             AND  WRK-ZAIKYO-YMD          NOT =  SPACE
               MOVE    CONS-JIKSGK-E TO  WRK-CHECK-SRYCD
               PERFORM     20011-ZAIKYO-CHECK2-SEC
             END-IF
      *
             IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                INITIALIZE                WRK-JIDO-SETCOM-AREA
                IF   RECE41-SRYYM  >= "202008"
                  MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
                ELSE
                  EVALUATE  RECE44-FLG-IZONSYO-COMMENT
                    WHEN      "1"
                      MOVE  "1"    TO     WRK-JIDO-SETCOMFLG
                      MOVE   1     TO     WRK-JIDO-SETCOMATFLG
                    WHEN      "2"
                      MOVE  "1"    TO     WRK-JIDO-SETCOMFLG
                      MOVE   2     TO     WRK-JIDO-SETCOMATFLG
                    WHEN      "3"
                      MOVE  "1"    TO     WRK-JIDO-SETCOMFLG
                      MOVE   1     TO     WRK-JIDO-SETCOMATFLG
                    WHEN      "4"
                      MOVE  "1"    TO     WRK-JIDO-SETCOMFLG
                      MOVE   1     TO     WRK-JIDO-SETCOMATFLG
                  END-EVALUATE
                END-IF
      *
                MOVE       99      TO     FLG-COMMENT
      *
                IF   RECE41-SRYYM  >= "202008"
                  EVALUATE  RECE44-FLG-IZONSYO-COMMENT
                    WHEN      "1"
                      MOVE   64            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN      "2"
                      MOVE   65            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN      "3"
                      MOVE   66            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN      "4"
                      MOVE   67            TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                    WHEN      "5"
                      MOVE   136           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
                  END-EVALUATE
                  PERFORM  200111-JIDOSET-HENSYU-SEC
                  IF  WRK-ZAIKYO-YMD2   NOT =  SPACE
                      MOVE   567           TO  IDX-JIDO
                      PERFORM  200111-RECECOMMENT-IDXGET-SEC
                      MOVE   WRK-ZAIKYO-YMD2
                                           TO  WRK-RECECOM-YMD
                      PERFORM  200111-RECECOMMENT-HENSYU-SEC
      *
                      PERFORM  200111-JIDOSET-HENSYU-SEC
                  END-IF
                ELSE
                  ADD      1       TO     IDZ
                  MOVE     12      TO     WK-FREE-YUKOKETA2(IDZ)
                  MOVE "治療開始日　　　月　　日"
                                   TO     WK-FREE-NAME2(IDZ)
                  MOVE  WRK-ZAIKYO-YMD(5:2)   TO  DAY-IDX2
                  MOVE  TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(13:4)
                  MOVE  WRK-ZAIKYO-YMD(7:2)   TO  DAY-IDX2
                  MOVE  TBL-DAY(DAY-IDX2)
                                   TO     WK-FREE-NAME2(IDZ)(19:4)
      *
                  PERFORM  200111-JIDOSET-HENSYU-SEC
      *
                  IF   WRK-ZAIKYO-YMD2   NOT =  SPACE
                    ADD      1       TO     IDZ
                    MOVE     12      TO     WK-FREE-YUKOKETA2(IDZ)
                    MOVE "治療終了日　　　月　　日"
                                     TO     WK-FREE-NAME2(IDZ)
                    MOVE  WRK-ZAIKYO-YMD2(5:2)  TO  DAY-IDX2
                    MOVE  TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(13:4)
                    MOVE  WRK-ZAIKYO-YMD2(7:2)  TO  DAY-IDX2
                    MOVE  TBL-DAY(DAY-IDX2)
                                     TO     WK-FREE-NAME2(IDZ)(19:4)
      *
                    PERFORM  200111-JIDOSET-HENSYU-SEC
                  END-IF
                END-IF
             END-IF
      *
             IF  WK-FREE-SRYCD2(1)  NOT =  SPACE
             OR  WK-FREE-NAME2(1)   NOT =  SPACE
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-IZONSYO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神科ショート・ケア、精神科デイ・ケア、精神科ナイト・ケア
      *    又は精神科デイ・ナイト・ケアがあるか判定
      *    早期加算（精神科ショート・ケア）等があるか判定
      *  ＃202011選択式コメント対応
      *  ＃202207選択式コメント対応
      *****************************************************************
       2001-SEIKEA-HANTEI-SEC          SECTION.
      *
           IF   ( RECE44-FLG-IZONSYO-COMMENT   =    SPACE )
           AND  ((RECE44-FLG-SEIKEA2-COMMENT   =    "1"  ) OR
                 (RECE44-FLG-SEISOKI-COMMENT   =    "1"  ))
             MOVE         ZERO     TO     IDZ
             INITIALIZE                   WRK-FREE-AREA2
      *      ＃202011選択式コメント対応
             IF   RECE44-FLG-SEIKEA2-COMMENT  =  "1"
                MOVE  CONS-SEIKEA  TO  WRK-CHECK-SRYCD
                PERFORM       20011-ZAIKYO-CHECK-SEC
                IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                   INITIALIZE                WRK-JIDO-SETCOM-AREA
                   MOVE      "2"      TO     WRK-JIDO-SETCOMFLG
      *
                   MOVE       99      TO     FLG-COMMENT
      *
                   MOVE     114       TO     IDX-JIDO
                   PERFORM  200111-RECECOMMENT-IDXGET-SEC
                   MOVE     WRK-ZAIKYO-YMD
                                      TO     WRK-RECECOM-YMD
                   PERFORM  200111-RECECOMMENT-HENSYU-SEC
                END-IF
             END-IF
      *      ＃202207選択式コメント対応
             IF   RECE44-FLG-SEISOKI-COMMENT  =  "1"
                MOVE  CONS-SEIKEA  TO  WRK-CHECK-SRYCD
                PERFORM       20011-ZAIKYO-CHECK-SEC
                IF   WRK-ZAIKYO-YMD   NOT =  SPACE
                   INITIALIZE                WRK-JIDO-SETCOM-AREA
                   MOVE      "2"      TO     WRK-JIDO-SETCOMFLG
      *
                   MOVE       99      TO     FLG-COMMENT
      *
                   MOVE     123       TO     IDX-JIDO
                   PERFORM  200111-RECECOMMENT-IDXGET-SEC
                   MOVE     WRK-ZAIKYO-YMD
                                      TO     WRK-RECECOM-YMD
                   PERFORM  200111-RECECOMMENT-HENSYU-SEC
                END-IF
             END-IF
      *
             IF  WK-FREE-SRYCD2(1)  NOT =  SPACE
             OR  WK-FREE-NAME2(1)   NOT =  SPACE
      *
                PERFORM  200111-SRYACTINF-HENSYU-MAT-SEC
      *
             END-IF
           END-IF
      *
           .
       2001-SEIKEA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    精神科緊急訪問看護加算があるか判定
      *  ＃202406選択式コメント対応
      *****************************************************************
       2001-SEIKIN-HANTEI-SEC          SECTION.
      *
           IF     RECE44-FLG-SEIKIN-COMMENT   =    "1"
               INITIALIZE              WRK-JIDO-SETCOM-AREA
               MOVE    "2"      TO     WRK-JIDO-SETCOMFLG
               MOVE     99      TO     FLG-COMMENT
               MOVE     141     TO     IDX-JIDO
               PERFORM  200111-RECECOMMENT-IDXGET-SEC
               PERFORM  20011-SHUJUTSU-ETC2-COMMENT-SEC
           END-IF
      *
           .
       2001-SEIKIN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外湿布薬７０枚超理由コメントがあるか判定
      *****************************************************************
       2001-SIPPUCOM-HANTEI-SEC          SECTION.
      *
           IF     RECE44-SIPPUCOMFLG  =  "1"
      *
             MOVE    99                  TO  FLG-COMMENT
      *
             ADD     1                   TO  IDX34
      *
             MOVE    ZERO                TO  IDX-RECE45
             MOVE    SPACE               TO  WRK-RECE45XX-REC
             INITIALIZE                      WRK-RECE45XX-REC
      *
             PERFORM  200111-RECE45-TAIHI-SEC
      *
             PERFORM  UNTIL  (FLG-RECE45           =   1               )
                       OR    (RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM  )
                       OR    (RECE44-SRYYM     NOT =   RECE45-SRYYM    )
                       OR    (RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN)
                       OR    (RECE44-PTID      NOT =   RECE45-PTID     )
                       OR    (RECE44-RECEKA    NOT =   RECE45-RECEKA   )
                       OR    (RECE44-HKNID     NOT =   RECE45-HKNID    )
                       OR    (RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD )
                       OR    (RECE44-ZAINUM    NOT =   RECE45-ZAINUM   )
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX               >   5    )
                          OR    (RECE45-SRYCD(IDX) =   SPACE)
                  IF  ((RECE45-SRYCD(IDX)(1:1)  =  "8"  ) OR
                       (RECE45-SRYCD(IDX)(1:3)  =  "008"))
                  AND ( RECE45-NAME(IDX)        =  SPACE )
                    CONTINUE
                  ELSE
                    IF    (RECE45-SRYCD(IDX)  = "830000052")
                    AND   (RECE41-SRYYM      >= "202010"   )
                      CONTINUE
                    ELSE
                      IF  (RECE45-SRYCD(IDX)(1:1)  =  "8"  )
                      OR  (RECE45-SRYCD(IDX)(1:3)  =  "008")
                        MOVE  "810000001"  TO   RECE45-SRYCD(IDX)
                      END-IF
                    END-IF
                    ADD    1   TO  IDX-RECE45
                    MOVE   RECE45-SRYACT-TBL(IDX)
                               TO  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                  END-IF
               END-PERFORM
               PERFORM  990-RECE45-NEXT-SEC
             END-PERFORM
      *
             MOVE      ZERO                 TO   FLG-WRKRECE45
             MOVE      ZERO                 TO   IDX
             MOVE      ZERO                 TO   IDX-RECE45
             MOVE      ZERO                 TO   WRK-RENNUM
             MOVE      ZERO                 TO   WRK-CNT-MEISAI
             PERFORM  UNTIL  FLG-WRKRECE45  =  1
               INITIALIZE                        RECE459-REC
               MOVE    WRK-RECE45-KEY       TO   RECE459-KEY
               COMPUTE WRK-SIPPU-ZAICNT     =    99991900  +  IDX34
               MOVE    WRK-SIPPU-ZAICNT     TO   RECE459-ZAINUM
               ADD     1                    TO   WRK-RENNUM
               MOVE    WRK-RENNUM           TO   RECE459-RENNUM
               MOVE    WRK-RECE45-PTNUM     TO   RECE459-PTNUM
               MOVE    WRK-RECE45-SRYKA     TO   RECE459-SRYKA
               MOVE    WRK-RECE45-SRYKBN    TO   RECE459-SRYKBN
               MOVE    WRK-RECE45-SRYSYUKBN TO   RECE459-SRYSYUKBN
               MOVE    WRK-RECE45-JIHIMONEY TO   RECE459-JIHIMONEY
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX            >  5)
                          OR    (FLG-WRKRECE45  =  1)
                 ADD        1     TO    IDX-RECE45
                 IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                 OR (IDX-RECE45                       >  100 )
                    MOVE    1     TO    FLG-WRKRECE45
                 ELSE
                    MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                  TO    RECE459-SRYACT-TBL(IDX)
                    ADD     1     TO    WRK-CNT-MEISAI
                 END-IF
               END-PERFORM
               IF   RECE459-SRYCD(1)  NOT =  SPACE
                    PERFORM  800-RECE459-WRITE-SEC
               END-IF
             END-PERFORM
      *
             MOVE     RECE44-REC        TO   RECE449-REC
      *@@@@
             MOVE     RECE44-ZAINUM     TO   RECE449-ZAINUM
             MOVE     RECE44-SRYKBN     TO   RECE449-SRYKBN
      *@@@@
             MOVE     WRK-CNT-MEISAI    TO   RECE449-MEISAISU
             COMPUTE  WRK-SIPPU-ZAICNT  =    99991900  +  IDX34
             MOVE     WRK-SIPPU-ZAICNT  TO   RECE449-ZAINUM
             PERFORM  800-RECE449-WRITE-SEC
      *
           END-IF
      *
           .
       2001-SIPPUCOM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    救急医療管理加算の判定
      *****************************************************************
       2001-KYUKYU-HANTEI-SEC          SECTION.
      *
           IF (( RECE41-KYUKYU-PRTKBN    =      1         ) AND
               ( RECE44-SRYCDTOTAL(6:9)  =   CONS-KYUKYU  ) AND
               ( RECE41-SRYYM           <=   "201306"     ))
      ***  OR (( RECE41-KYUKYU-PRTKBN    =      1         ) AND
      ***      ( RECE44-SRYCDTOTAL(6:9)  =   CONS-KYUKYU2 ) AND
      ***      ( RECE41-SRYYM           >=   "201307"     ))
           OR (( RECE44-JIBAISYUKEIKBN   =      4         ) AND
               ( RECE44-SRYCDTOTAL(6:5)  =   "09593"      ) AND
               ( RECE41-SRYYM           <=   "201306"     ))
               MOVE    SPACE   TO   WRK-TAIHI-SRYYMD
               IF  (RECE44-JIBAISYUKEIKBN   =    4   )
               AND (RECE44-SRYCDTOTAL(6:5)  = "09593")
                 PERFORM  VARYING  IDX20  FROM  1  BY  1
                           UNTIL   IDX20   >   31
                   IF  RECE44-KAISU(1 IDX20)   NOT =    ZERO
                     MOVE  RECE41-SRYYM  TO  WRK-TAIHI-SRYYM
                     MOVE  IDX20         TO  WRK-TAIHI-SRYD
                     MOVE  31            TO  IDX20
                   END-IF
                 END-PERFORM
               ELSE
                 MOVE  RECE41-KYUKYUYMD  TO  WRK-TAIHI-SRYYMD
               END-IF
               MOVE        99            TO  FLG-COMMENT
               PERFORM     2001-KYUKYU-COMMENT-SEC
           END-IF
      *
           .
       2001-KYUKYU-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    救急医療管理加算のコメント編集
      *****************************************************************
       2001-KYUKYU-COMMENT-SEC          SECTION.
      *
           INITIALIZE                    WRK-JIDO-SETCOM-AREA
           MOVE      "1"            TO   WRK-JIDO-SETCOMFLG
           MOVE       1             TO   WRK-JIDO-SETCOMATFLG
      *
           MOVE      RECE44-REC     TO   RECE449-REC
      *@@@@
           MOVE      RECE44-ZAINUM  TO   RECE449-ZAINUM
           MOVE      RECE44-SRYKBN  TO   RECE449-SRYKBN
      *@@@@
           ADD        1             TO   RECE449-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG
                                    TO   RECE449-SETCOMFLG
           END-IF
           PERFORM 800-RECE449-WRITE-SEC
      *
           PERFORM 800-RECE44-SET45-SEC
           MOVE    RECE44-ZAINUM    TO   RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1      TO   FLG-RECE45
                 NOT INVALID
                     PERFORM 990-RECE45-NEXT-SEC
           END-START
      *
           MOVE    RECE45-REC           TO  RECE459-REC
           MOVE    RECE45-SRYACT-TBL(1) TO  RECE459-SRYACT-TBL(1)
           MOVE    RECE45-SRYACT-TBL(2) TO  RECE459-SRYACT-TBL(3)
           MOVE    RECE45-SRYACT-TBL(3) TO  RECE459-SRYACT-TBL(4)
           MOVE    RECE45-SRYACT-TBL(4) TO  RECE459-SRYACT-TBL(5)
      *
           MOVE    SPACE                TO  RECE459-SRYACT-TBL(2)
           INITIALIZE                       RECE459-SRYACT-TBL(2)
           MOVE    "810000001"          TO  RECE459-SRYCD(2)
           MOVE    ZERO                 TO  RECE459-SRYSURYO(2)
           MOVE    9                    TO  RECE459-YUKOKETA(2)
      * 
           MOVE    "算定　　　月　　日" TO  RECE459-NAME(2)
           MOVE     WRK-TAIHI-SRYYMD(5:2)
                                        TO  DAY-IDX2
           MOVE     TBL-DAY(DAY-IDX2)   TO  RECE459-NAME(2)(7:4)
           MOVE     WRK-TAIHI-SRYYMD(7:2)
                                        TO  DAY-IDX2
           MOVE     TBL-DAY(DAY-IDX2)   TO  RECE459-NAME(2)(13:4)
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                  TO  RECE459-MEISKYFLG(2)
             MOVE  WRK-JIDO-SETCOMATFLG TO  RECE459-SETCOMATFLG(2)
           END-IF
      *
           MOVE     "1"                 TO  RECE459-SIKFLG(2)
      *
           PERFORM 800-RECE459-WRITE-SEC
      *    
           .
       2001-KYUKYU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関する編集
      *****************************************************************
       2001-YAKUTEI-HENSYU-SEC          SECTION.
      *
           ADD       1        TO         IDX6
      *
           IF    IDX6    <=    100
             MOVE    RECE44-SRYKBN       TO   TBL-YT1-SRYKBN(IDX6)
             MOVE    RECE44-JIHOKBN      TO   TBL-YT1-JIHOKBN(IDX6)
      ***    MOVE    RECE44-SRYKA        TO   TBL-YT1-SRYKA(IDX6)
      *
             MOVE    RECE44-REC          TO   TBL-YT1-RECE44-REC(IDX6)
      *
             MOVE    RECE44-ZAITEN       TO   TBL-YT1-ZAITEN(IDX6)
             MOVE    RECE44-SRYCDTOTAL   TO   TBL-YT1-SRYCDTOTAL(IDX6)
             MOVE    RECE44-SURYOUTOTAL  TO   TBL-YT1-SURYOUTOTAL(IDX6)
             MOVE    RECE44-RUIKEITOTAL  TO   TBL-YT1-RUIKEITOTAL(IDX6)
             MOVE    ZERO                TO   TBL-YT1-RUIKEITOTAL2(IDX6)
             MOVE    ZERO                TO   TBL-YT1-RUIKEITOTAL3(IDX6)
             MOVE    ZERO                TO   TBL-YT1-RUIKEITOTAL4(IDX6)
             MOVE    RECE44-MEISAISU     TO   TBL-YT1-MEISAISU(IDX6)
      *
             PERFORM   VARYING   IDX7    FROM   2   BY   1
                        UNTIL   (IDX7      >   10)
      *
               COMPUTE   IDX9    =   IDX7  -   1
               PERFORM   VARYING   IDX8    FROM   1   BY   1
                          UNTIL   (IDX8      >    31)
      *
                  MOVE    RECE44-KAISU(IDX7 IDX8)
                            TO    TBL-YT1-KAISU(IDX6 IDX8 IDX9)
      *
               END-PERFORM
      *
             END-PERFORM
      *
             IF      RECE44-SRYCDTOTAL  =   630010002  OR  630010005
                                        OR  630010006  OR  630010007
                  MOVE      2      TO     TBL-YT1-FLG(IDX6)
             ELSE
                  MOVE      1      TO     TBL-YT1-FLG(IDX6)
             END-IF
      *
             IF    TBL-YT1-FLG(IDX6)   =   1
               MOVE    ZERO                TO  IDX58
      *
               PERFORM 800-RECE44-SET45-SEC
               MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
               START   RECE45-FILE
                   KEY IS    >=   RECE45-KEY
                   INVALID
                       MOVE    1           TO  FLG-RECE45
                   NOT INVALID
                       PERFORM 990-RECE45-NEXT-SEC
               END-START
               PERFORM
                   UNTIL  (FLG-RECE45          =   1           )
                    OR    (RECE44-KEY1     NOT =  RECE45-KEY1  )
                    OR    (RECE44-ZAINUM   NOT =  RECE45-ZAINUM)
                   PERFORM  VARYING  IDX57  FROM  1  BY  1
                             UNTIL  (IDX57               >   5   )
                              OR    (RECE45-SRYCD(IDX57) =  SPACE)
                     IF  ((RECE45-SRYCD(IDX57)(1:1) = "8"  ) OR
                          (RECE45-SRYCD(IDX57)(1:3) = "008"))
                     AND ( IDX58                   <=  4    )
                       ADD   1     TO  IDX58
                       MOVE  RECE45-SRYCD(IDX57)
                                   TO  TBL-YT1-COMCD  (IDX6 IDX58)
                       MOVE  RECE45-NAME(IDX57)
                                   TO  TBL-YT1-COMNAME(IDX6 IDX58)
                     END-IF
                   END-PERFORM
                   PERFORM 990-RECE45-NEXT-SEC
               END-PERFORM
             END-IF
           ELSE
      *
             DISPLAY "TABLE OVER1"
      *
           END-IF
      *
           .
       2001-YAKUTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外湿布薬のチェック
      *****************************************************************
       2001-INGAISIPPU-CHECK-SEC          SECTION.
      *
           IF    RECE41-SRYYM  >=  "202010"
             IF    RECE44-SIPPUFLG2 = "1"
      ***      IF    RECE41-SRYYM  >=  "202208"
      ***        MOVE       1       TO    FLG-HANTEI
      ***        MOVE      ZERO     TO    IDX54
      ***        MOVE      ZERO     TO    IDX55
      ***        PERFORM  VARYING  IDX36  FROM  2  BY  1
      ***                  UNTIL   IDX36   >   10
      ***          PERFORM  VARYING  IDX37  FROM  1  BY  1
      ***                    UNTIL   IDX37    >  31
      ***            IF   RECE44-KAISU(IDX36 IDX37)     NOT =  ZERO
      ***            AND  RECE41-SIPPUCHK2(IDX36 IDX37)     =  ZERO
      ***              ADD    1                          TO  IDX54
      ***              ADD    RECE44-KAISU(IDX36 IDX37)  TO  IDX55
      ***            END-IF
      ***          END-PERFORM
      ***        END-PERFORM
      ***        IF    IDX55  NOT = ZERO
      ***              PERFORM  2001-INGAISIPPU-COMMENT-SEC
      ***        END-IF
      ***      ELSE
                 MOVE      ZERO     TO    FLG-HANTEI
                 PERFORM  VARYING  IDX36  FROM  2  BY  1
                           UNTIL   IDX36   >   10
                   PERFORM  VARYING  IDX37  FROM  1  BY  1
                             UNTIL   IDX37    >  31
                     IF   RECE44-KAISU(IDX36 IDX37)     NOT =  ZERO
                     AND  RECE41-SIPPUCHK2(IDX36 IDX37)     =  ZERO
                       PERFORM  2001-INGAISIPPU-COMMENT-SEC
                     END-IF
                   END-PERFORM
                 END-PERFORM
      ***      END-IF
             END-IF
           ELSE
             MOVE     ZERO          TO    FLG-HANTEI
             MOVE     ZERO          TO    FLG-SIPPU-OK
             IF    RECE41-SIPPUCHKFLG = 1
               PERFORM  VARYING  IDX36  FROM  2  BY  1
                         UNTIL   IDX36   >   10
                 PERFORM  VARYING  IDX37  FROM  1  BY  1
                           UNTIL   IDX37    >  31
                   IF   RECE44-KAISU(IDX36 IDX37)     NOT =  ZERO
                   AND  RECE41-SIPPUCHK(IDX36 IDX37)  NOT =  ZERO
                     MOVE     1     TO    FLG-SIPPU-OK
                     MOVE    31     TO    IDX37
                     MOVE    10     TO    IDX36
                   END-IF
                 END-PERFORM
               END-PERFORM
             END-IF
             IF    FLG-SIPPU-OK = 1
               PERFORM  2001-INGAISIPPU-COMMENT-SEC
             END-IF
           END-IF
      *
           .
       2001-INGAISIPPU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外湿布薬のコメント変換
      *****************************************************************
       2001-INGAISIPPU-COMMENT-SEC          SECTION.
      *
           ADD     1                   TO  IDX33
      *
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
           PERFORM  UNTIL  (FLG-RECE45           =   1               )
                     OR    (RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM  )
                     OR    (RECE44-SRYYM     NOT =   RECE45-SRYYM    )
                     OR    (RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN)
                     OR    (RECE44-PTID      NOT =   RECE45-PTID     )
                     OR    (RECE44-RECEKA    NOT =   RECE45-RECEKA   )
                     OR    (RECE44-HKNID     NOT =   RECE45-HKNID    )
                     OR    (RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD )
                     OR    (RECE44-ZAINUM    NOT =   RECE45-ZAINUM   )
             PERFORM  VARYING  IDX  FROM  1  BY  1
                       UNTIL  (IDX               >   5    )
                        OR    (RECE45-SRYCD(IDX) =   SPACE)
               IF   RECE45-SIPPUFLG(IDX) NOT = SPACE
                  ADD    1   TO  IDX-RECE45
                  IF  RECE45-SIPPUFLG(IDX)  =  "1"
                     IF  RECE45-INGAIFLG-SIPPU(IDX)  =  "1"
                       MOVE  RECE45-GENERICNAME(IDX)
                                     TO   RECE45-NAME(IDX)
                       MOVE  RECE45-GENERICYUKOKETA(IDX)
                                     TO   RECE45-YUKOKETA(IDX)
                     END-IF
                     PERFORM  20011-INGAISIPPU-SURYO-SEC
                     MOVE  "810000001"
                                     TO   RECE45-SRYCD(IDX)
                     MOVE   ZERO     TO   RECE45-SRYSURYO(IDX)
                                          RECE45-TANICD(IDX)
                                          RECE45-TANIMOJISU(IDX)
                     MOVE   SPACE    TO   RECE45-TANINAME(IDX)
                     MOVE   SPACE    TO   RECE45-INGAIFLG(IDX)
                                          RECE45-INGAIFLG-RECE(IDX)
                  END-IF
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
               END-IF
             END-PERFORM
             PERFORM  990-RECE45-NEXT-SEC
           END-PERFORM
      *
           IF    FLG-HANTEI      =  1
           AND   IDX-RECE45  NOT = ZERO
             PERFORM  20012-INGAI-KAISU-SEC
      *
             ADD     1            TO   IDX-RECE45
             MOVE   "810000001"   TO   WRK-RECE45XX-SRYCD(IDX-RECE45 1)
             MOVE   WRK-INGAIKAISU-NAME
                                  TO   WRK-RECE45XX-NAME(IDX-RECE45 1)
      *
             PERFORM  VARYING  IDX35  FROM  300  BY  -1
                       UNTIL   IDX35   <    1
                IF  WRK-INGAIKAISU-NAME(IDX35:1)  NOT =  SPACE
                    COMPUTE  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) =
                             IDX35   /    2
                    MOVE     1    TO   IDX35
                END-IF
             END-PERFORM
             MOVE   "1"           TO   WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-IF
      *
           MOVE      ZERO                 TO   FLG-WRKRECE45
           MOVE      ZERO                 TO   IDX
           MOVE      ZERO                 TO   IDX-RECE45
           MOVE      ZERO                 TO   WRK-RENNUM
           MOVE      ZERO                 TO   WRK-CNT-MEISAI
           PERFORM  UNTIL  FLG-WRKRECE45  =  1
             INITIALIZE                        RECE459-REC
             MOVE    WRK-RECE45-KEY       TO   RECE459-KEY
             COMPUTE WRK-SIPPU-ZAICNT     =    99991500  +  IDX33
             MOVE    WRK-SIPPU-ZAICNT     TO   RECE459-ZAINUM
             ADD     1                    TO   WRK-RENNUM
             MOVE    WRK-RENNUM           TO   RECE459-RENNUM
             MOVE    WRK-RECE45-PTNUM     TO   RECE459-PTNUM
             MOVE    WRK-RECE45-SRYKA     TO   RECE459-SRYKA
             MOVE    "80"                 TO   RECE459-SRYKBN
             MOVE    "800"                TO   RECE459-SRYSYUKBN
             MOVE    WRK-RECE45-JIHIMONEY TO   RECE459-JIHIMONEY
             PERFORM  VARYING  IDX  FROM  1  BY  1
                       UNTIL  (IDX            >  5)
                        OR    (FLG-WRKRECE45  =  1)
               ADD        1     TO    IDX-RECE45
               IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
               OR (IDX-RECE45                       >  100 )
                  MOVE    1     TO    FLG-WRKRECE45
               ELSE
                  MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                TO    RECE459-SRYACT-TBL(IDX)
                  ADD     1     TO    WRK-CNT-MEISAI
               END-IF
             END-PERFORM
             IF   RECE459-SRYCD(1)  NOT =  SPACE
                  PERFORM  800-RECE459-WRITE-SEC
             END-IF
           END-PERFORM
      *
           MOVE     RECE44-REC        TO   RECE449-REC
      *@@@@
           MOVE     RECE44-ZAINUM     TO   RECE449-ZAINUM
           MOVE     RECE44-SRYKBN     TO   RECE449-SRYKBN
      *@@@@
           MOVE     WRK-CNT-MEISAI    TO   RECE449-MEISAISU
           COMPUTE  WRK-SIPPU-ZAICNT  =    99991500  +  IDX33
           MOVE     WRK-SIPPU-ZAICNT  TO   RECE449-ZAINUM
           MOVE     "80"              TO   RECE449-SRYKBN
           MOVE     "800"             TO   RECE449-SRYSYUKBN
           MOVE     SPACE             TO   RECE449-TOYAKUFLG
           MOVE     SPACE             TO   RECE449-SETCOMFLG
           PERFORM  800-RECE449-WRITE-SEC
      *
           .
       2001-INGAISIPPU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外湿布薬の数量編集
      *****************************************************************
       20011-INGAISIPPU-SURYO-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    ZERO            TO  WRK-SURYOST
           MOVE    ZERO            TO  WRK-SURYOKETA
           MOVE    ZERO            TO  WRK-SURYOKETA1
           MOVE    ZERO            TO  WRK-SURYOKETA2
           MOVE    SPACE           TO  WRK-SURYO-N
           MOVE    SPACE           TO  WRK-SRYSURYOW
      * 
      *    数字表示用編集（半角）
           MOVE    RECE45-SRYSURYO(IDX)
                                   TO  WRK-SURYO
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
           MOVE    ZERO            TO  FLG-SP
           PERFORM  VARYING  IDX35  FROM  5  BY  -1
                     UNTIL   IDX35  =   ZERO
             IF  WRK-SURYO-S2(IDX35:1)  NOT =  ZERO
                 MOVE    1         TO  FLG-SP
             END-IF
             IF  FLG-SP  =  1
                 MOVE    WRK-SURYO-S2(IDX35:1)
                                   TO  WRK-SURYO-Z3(IDX35:1)
             END-IF
           END-PERFORM
           IF    WRK-SURYO-Z3  NOT =  SPACE
                 MOVE    "."       TO  WRK-SURYO-Z2
           END-IF
      *    表示文字数カウント
           PERFORM  VARYING  IDX35  FROM  1  BY  1
                     UNTIL   IDX35   >   11
             IF  WRK-SURYO-X(IDX35:1)  NOT =  SPACE
                 IF  WRK-SURYOST  =  ZERO
                     MOVE    IDX35    TO   WRK-SURYOST
                 END-IF
      *          数量桁数カウント(６桁目：ピリオド)
                 ADD     1            TO   WRK-SURYOKETA
                 IF  IDX35  <  6
                     ADD     1        TO   WRK-SURYOKETA1
                 END-IF
                 IF  IDX35  >  6
                     ADD     1        TO   WRK-SURYOKETA2
                 END-IF
             END-IF
           END-PERFORM
           IF  WRK-SURYOST  =  6
               MOVE    "0"             TO  WRK-SURYO-Z1(5:1)
               MOVE    5               TO  WRK-SURYOST
               MOVE    1               TO  WRK-SURYOKETA1
               ADD     1               TO  WRK-SURYOKETA
           END-IF
      *    全角変換
           IF  WRK-SURYOKETA1          >   ZERO
               INITIALIZE              WRK-CHAR2-HENKAN-AREA
               MOVE    WRK-SURYO-Z1 (WRK-SURYOST:WRK-SURYOKETA1)
                                       TO  WRK-CHAR2-X
               MOVE    WRK-SURYOKETA1  TO  WRK-CHAR2-CNT
               PERFORM 200111-INGAISIPPU-HENKAN-SEC
               MOVE    WRK-CHAR2-N     TO  WRK-SURYO-N1
           END-IF
           IF  WRK-SURYOKETA2          >   ZERO
               MOVE    "．"            TO  WRK-SURYO-N2
               INITIALIZE              WRK-CHAR2-HENKAN-AREA
               MOVE    WRK-SURYO-Z3 (1:WRK-SURYOKETA2)
                                       TO  WRK-CHAR2-X
               MOVE    WRK-SURYOKETA2  TO  WRK-CHAR2-CNT
               PERFORM 200111-INGAISIPPU-HENKAN-SEC
               MOVE    WRK-CHAR2-N     TO  WRK-SURYO-N3
           END-IF
           STRING      WRK-SURYO-N1    DELIMITED   BY  " "
                       WRK-SURYO-N2    DELIMITED   BY  " "
                       WRK-SURYO-N3    DELIMITED   BY  " "
                       INTO            WRK-SRYSURYOW
           END-STRING
      *
           MOVE        RECE45-NAME(IDX)  TO  WRK-INGAISIPPU-NAME
           MOVE        SPACE             TO  RECE45-NAME(IDX)
           STRING  WRK-INGAISIPPU-NAME   DELIMITED  BY  " "
                   "　"                  DELIMITED  BY  SIZE
                   WRK-SRYSURYOW         DELIMITED  BY  " "
                   RECE45-TANINAME(IDX)  DELIMITED  BY  " "
                   INTO                  RECE45-NAME(IDX)
           END-STRING
      *
           PERFORM  VARYING  IDX35  FROM  300  BY  -1
                     UNTIL   IDX35   <    1
              IF  RECE45-NAME(IDX)(IDX35:1)  NOT =  SPACE
                  COMPUTE  RECE45-YUKOKETA(IDX)  =
                           IDX35   /    2
                  MOVE     1     TO      IDX35
              END-IF
           END-PERFORM
      *
           .
       20011-INGAISIPPU-SURYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値半角→全角変換処理
      *****************************************************************
       200111-INGAISIPPU-HENKAN-SEC    SECTION.
      *
           MOVE    1           TO  WRK-CHAR2-IDX
           MOVE    ZERO        TO  WRK-CHAR2-N-CNT
      *
           PERFORM  VARYING  IDX35  FROM  1  BY  1
                     UNTIL   IDX35   >   WRK-CHAR2-CNT
             IF     WRK-CHAR2-X(IDX35:1)  IS  NUMERIC
               IF      WRK-CHAR2-X(IDX35:1)  =  "0"
                 MOVE    "０"  TO  WRK-CHAR2-N(WRK-CHAR2-IDX:)
               ELSE
                 MOVE  WRK-CHAR2-X(IDX35:1)
                               TO  IDX19
                 MOVE  TBL-DAY(IDX19)(3:2)
                               TO  WRK-CHAR2-N(WRK-CHAR2-IDX:)
               END-IF
             ELSE
                 MOVE  "  "    TO  WRK-CHAR2-N(WRK-CHAR2-IDX:)
             END-IF
             ADD     2         TO  WRK-CHAR2-IDX
                                   WRK-CHAR2-N-CNT
           END-PERFORM
      *
           .
       200111-INGAISIPPU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外湿布薬・院外投薬の回数編集
      *****************************************************************
       20012-INGAI-KAISU-SEC          SECTION.
      *
           MOVE    SPACE            TO  WRK-ZAIKAISU-X
           MOVE    ZERO             TO  WRK-SURYOST
           MOVE    ZERO             TO  WRK-SURYOKETA1
           MOVE    SPACE            TO  WRK-ZAIKAISU-N
           MOVE    SPACE            TO  WRK-KEI-N
           MOVE    SPACE            TO  WRK-INGAIKAISU-NAME
      *
      *    数字表示用編集（半角）
           MOVE    IDX55            TO  WRK-ZAIKAISU-Z
      *
      *    表示文字数カウント
           PERFORM  VARYING  IDX35  FROM  1  BY  1
                     UNTIL   IDX35   >    4
             IF  WRK-ZAIKAISU-X(IDX35:1)  NOT =  SPACE
               IF  WRK-SURYOST  =  ZERO
                   MOVE    IDX35    TO  WRK-SURYOST
               END-IF
               ADD         1        TO  WRK-SURYOKETA1
             END-IF
           END-PERFORM
      *    全角変換
           IF  WRK-SURYOKETA1          >   ZERO
             INITIALIZE              WRK-CHAR2-HENKAN-AREA
             MOVE    WRK-ZAIKAISU-Z(WRK-SURYOST:WRK-SURYOKETA1)
                                     TO  WRK-CHAR2-X
             MOVE    WRK-SURYOKETA1  TO  WRK-CHAR2-CNT
             PERFORM 200111-INGAISIPPU-HENKAN-SEC
             MOVE    WRK-CHAR2-N     TO  WRK-ZAIKAISU-N
           END-IF
      *
           IF   WRK-ZAIKAISU-N   NOT = SPACE
      *****  IF   IDX54  >  1
               MOVE   "計"    TO   WRK-KEI-N
      *****  END-IF
             EVALUATE  RECE44-SRYKBN
               WHEN      "14"
                 STRING  "（"              DELIMITED   BY  SIZE
                         WRK-KEI-N         DELIMITED   BY  " "
                         WRK-ZAIKAISU-N    DELIMITED   BY  " "
                         "回院外処方）"    DELIMITED   BY  SIZE
                         INTO              WRK-INGAIKAISU-NAME
                 END-STRING
               WHEN      "21"
                 STRING  "（"              DELIMITED   BY  SIZE
                         WRK-KEI-N         DELIMITED   BY  " "
                         WRK-ZAIKAISU-N    DELIMITED   BY  " "
                         "日分院外処方）"  DELIMITED   BY  SIZE
                         INTO              WRK-INGAIKAISU-NAME
                 END-STRING
               WHEN      "22"
                 STRING  "（"              DELIMITED   BY  SIZE
                         WRK-KEI-N         DELIMITED   BY  " "
                         WRK-ZAIKAISU-N    DELIMITED   BY  " "
                         "回分院外処方）"  DELIMITED   BY  SIZE
                         INTO              WRK-INGAIKAISU-NAME
                 END-STRING
               WHEN      "23"
                 STRING  "（"              DELIMITED   BY  SIZE
                         WRK-KEI-N         DELIMITED   BY  " "
                         WRK-ZAIKAISU-N    DELIMITED   BY  " "
                         "回院外処方）"    DELIMITED   BY  SIZE
                         INTO              WRK-INGAIKAISU-NAME
                 END-STRING
             END-EVALUATE
           END-IF
      *
           .
       20012-INGAI-KAISU-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外投薬のチェック
      *****************************************************************
       2001-INGAITOYAKU-CHECK-SEC          SECTION.
      *
           MOVE       1       TO    FLG-HANTEI
           MOVE      ZERO     TO    IDX54
           MOVE      ZERO     TO    IDX55
           PERFORM  VARYING  IDX36  FROM  2  BY  1
                     UNTIL   IDX36   >   10
             PERFORM  VARYING  IDX37  FROM  1  BY  1
                       UNTIL   IDX37    >  31
               IF   RECE44-KAISU(IDX36 IDX37)     NOT =  ZERO
               AND  RECE41-SIPPUCHK2(IDX36 IDX37)     =  ZERO
                 ADD    1                          TO  IDX54
                 ADD    RECE44-KAISU(IDX36 IDX37)  TO  IDX55
               END-IF
             END-PERFORM
           END-PERFORM
           IF    IDX55  NOT = ZERO
                 PERFORM  2001-INGAITOYAKU-COMMENT-SEC
           END-IF
      *
           .
       2001-INGAITOYAKU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外投薬のコメント変換
      *****************************************************************
       2001-INGAITOYAKU-COMMENT-SEC          SECTION.
      *
           ADD     1                   TO  IDX56
      *
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           PERFORM  200111-RECE45-TAIHI-SEC
      *
           PERFORM  UNTIL  (FLG-RECE45           =   1               )
                     OR    (RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM  )
                     OR    (RECE44-SRYYM     NOT =   RECE45-SRYYM    )
                     OR    (RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN)
                     OR    (RECE44-PTID      NOT =   RECE45-PTID     )
                     OR    (RECE44-RECEKA    NOT =   RECE45-RECEKA   )
                     OR    (RECE44-HKNID     NOT =   RECE45-HKNID    )
                     OR    (RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD )
                     OR    (RECE44-ZAINUM    NOT =   RECE45-ZAINUM   )
             PERFORM  VARYING  IDX  FROM  1  BY  1
                       UNTIL  (IDX               >   5    )
                        OR    (RECE45-SRYCD(IDX) =   SPACE)
               IF  ( RECE45-TOYAKUSENCOMFLG(IDX) NOT =  SPACE   )
               OR  ((RECE45-SIPPUFLG(IDX)        NOT =  SPACE  ) AND
                    (RECE41-SRYYM                   >= "202410"))
                  ADD    1   TO  IDX-RECE45
                  IF  ( RECE45-TOYAKUSENCOMFLG(IDX) = "1"      )
                  OR  ((RECE45-SIPPUFLG(IDX)        = "1"     ) AND
                       (RECE41-SRYYM               >= "202410"))
                     IF  ((RECE45-TOYAKUSENCOMFLG(IDX) = "1"     ) AND
                          (RECE45-INGAIFLG-TSC(IDX)    = "1"     ))
                     OR  ((RECE45-SIPPUFLG(IDX)        = "1"     ) AND
                          (RECE45-INGAIFLG-SIPPU(IDX)  = "1"     ) AND
                          (RECE41-SRYYM               >= "202410"))
                       MOVE  RECE45-GENERICNAME(IDX)
                                     TO   RECE45-NAME(IDX)
                       MOVE  RECE45-GENERICYUKOKETA(IDX)
                                     TO   RECE45-YUKOKETA(IDX)
                     END-IF
                     PERFORM  20011-INGAISIPPU-SURYO-SEC
                     MOVE  "810000001"
                                     TO   RECE45-SRYCD(IDX)
                     MOVE   ZERO     TO   RECE45-SRYSURYO(IDX)
                                          RECE45-TANICD(IDX)
                                          RECE45-TANIMOJISU(IDX)
                     MOVE   SPACE    TO   RECE45-TANINAME(IDX)
                     MOVE   SPACE    TO   RECE45-INGAIFLG(IDX)
                                          RECE45-INGAIFLG-RECE(IDX)
                  END-IF
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
               END-IF
             END-PERFORM
             PERFORM  990-RECE45-NEXT-SEC
           END-PERFORM
      *
           IF    FLG-HANTEI      =  1
           AND   IDX-RECE45  NOT = ZERO
             PERFORM  20012-INGAI-KAISU-SEC
      *
             ADD     1            TO   IDX-RECE45
             MOVE   "810000001"   TO   WRK-RECE45XX-SRYCD(IDX-RECE45 1)
             MOVE   WRK-INGAIKAISU-NAME
                                  TO   WRK-RECE45XX-NAME(IDX-RECE45 1)
      *
             PERFORM  VARYING  IDX35  FROM  300  BY  -1
                       UNTIL   IDX35   <    1
                IF  WRK-INGAIKAISU-NAME(IDX35:1)  NOT =  SPACE
                    COMPUTE  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1) =
                             IDX35   /    2
                    MOVE     1    TO   IDX35
                END-IF
             END-PERFORM
             MOVE   "1"           TO   WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-IF
      *
           MOVE      ZERO                 TO   FLG-WRKRECE45
           MOVE      ZERO                 TO   IDX
           MOVE      ZERO                 TO   IDX-RECE45
           MOVE      ZERO                 TO   WRK-RENNUM
           MOVE      ZERO                 TO   WRK-CNT-MEISAI
           PERFORM  UNTIL  FLG-WRKRECE45  =  1
             INITIALIZE                        RECE459-REC
             MOVE    WRK-RECE45-KEY       TO   RECE459-KEY
             COMPUTE WRK-SIPPU-ZAICNT     =    99991000  +  IDX56
             MOVE    WRK-SIPPU-ZAICNT     TO   RECE459-ZAINUM
             ADD     1                    TO   WRK-RENNUM
             MOVE    WRK-RENNUM           TO   RECE459-RENNUM
             MOVE    WRK-RECE45-PTNUM     TO   RECE459-PTNUM
             MOVE    WRK-RECE45-SRYKA     TO   RECE459-SRYKA
             MOVE    "80"                 TO   RECE459-SRYKBN
             MOVE    "800"                TO   RECE459-SRYSYUKBN
             MOVE    WRK-RECE45-JIHIMONEY TO   RECE459-JIHIMONEY
             PERFORM  VARYING  IDX  FROM  1  BY  1
                       UNTIL  (IDX            >  5)
                        OR    (FLG-WRKRECE45  =  1)
               ADD        1     TO    IDX-RECE45
               IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
               OR (IDX-RECE45                       >  100 )
                  MOVE    1     TO    FLG-WRKRECE45
               ELSE
                  MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                TO    RECE459-SRYACT-TBL(IDX)
                  ADD   1       TO    WRK-CNT-MEISAI
               END-IF
             END-PERFORM
             IF   RECE459-SRYCD(1)  NOT =  SPACE
                  WRITE   RECE459-REC
                  ADD     1     TO    CNT-RECE459
             END-IF
           END-PERFORM
      *
           MOVE     RECE44-REC        TO   RECE449-REC
           MOVE     WRK-CNT-MEISAI    TO   RECE449-MEISAISU
           COMPUTE  WRK-SIPPU-ZAICNT  =    99991000  +  IDX56
           MOVE     WRK-SIPPU-ZAICNT  TO   RECE449-ZAINUM
           MOVE     "80"              TO   RECE449-SRYKBN
           MOVE     "800"             TO   RECE449-SRYSYUKBN
           MOVE     SPACE             TO   RECE449-TOYAKUFLG
           MOVE     SPACE             TO   RECE449-SETCOMFLG
           WRITE    RECE449-REC
           ADD      1                 TO   CNT-RECE449
      *
           .
       2001-INGAITOYAKU-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０４転送処理
      *****************************************************************
       2099-RECE44-TENSOU-SEC     SECTION.
      *
           MOVE       RECE44-REC    TO   RECE449-REC
      *@@@@
           MOVE       RECE44-ZAINUM TO   RECE449-ZAINUM
           MOVE       RECE44-SRYKBN TO   RECE449-SRYKBN
      *@@@@
           PERFORM 800-RECE449-WRITE-SEC
           PERFORM    20991-RECE45-TENSOU-SEC
      *
           .
       2099-RECE44-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０５転送処理
      *****************************************************************
       20991-RECE45-TENSOU-SEC     SECTION.
      *
           PERFORM 800-RECE44-SET45-SEC
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  (FLG-RECE45          =   1           )
                OR    (RECE44-KEY1     NOT =  RECE45-KEY1  )
                OR    (RECE44-ZAINUM   NOT =  RECE45-ZAINUM)
      *
      *
               MOVE          RECE45-REC       TO      RECE459-REC
               PERFORM 800-RECE459-WRITE-SEC
      *
               PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
           .
       20991-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　退避処理
      *****************************************************************
       200111-RECE45-TAIHI-SEC     SECTION.
      *
           PERFORM 800-RECE44-SET45-SEC
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
                 KEY IS    >=   RECE45-KEY
                 INVALID
                     MOVE    1           TO  FLG-RECE45
                 NOT INVALID
                     PERFORM 990-RECE45-NEXT-SEC
                     MOVE    RECE45-REC  TO  WRK-RECE45-REC
           END-START
      *
           .
       200111-RECE45-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       200111-SRYACTINF-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE45           =   1                   )
             OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
             OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
             OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
             OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
             OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
             OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
             OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
             OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE45-SRYCD(IDX) =   SPACE )
               IF  (RECE44-KISAINGYKFLG    = "1")
               AND (RECE45-RECEPTFLG (IDX) = "1")
               AND (RECE45-RECEDENFLG(IDX) = "1")
                  CONTINUE
               ELSE
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE45-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
               END-IF
             END-PERFORM 
      *
             PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-JIDO-JYOUGEFLG  =  1
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   50    )
                       OR      ( WK-FREE-NAME2(IDX) =   SPACE )
                ADD    1                  TO  IDX-RECE45
                MOVE   "810000001"
                              TO  WRK-RECE45XX-SRYCD(IDX-RECE45 1)
                MOVE   ZERO   TO  WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                              TO  WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
                MOVE   WK-FREE-NAME2(IDX)
                              TO  WRK-RECE45XX-NAME(IDX-RECE45 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE45XX-MEISKYFLG(IDX-RECE45 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE45XX-SETCOMATFLG(IDX-RECE45 1)
                MOVE   WK-FREE-RECEDENFLG2(IDX)
                          TO  WRK-RECE45XX-RECEDENFLG(IDX-RECE45 1)
                MOVE   "1"    TO  WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
                MOVE   "1"    TO  WRK-RECE45XX-RECEPTFLG(IDX-RECE45 1)
             END-PERFORM
           END-IF
      *
      *    手術日コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE459-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE459-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE459-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE459-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE459-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE459-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE459-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE459-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                     OR (IDX-RECE45                       >  100 )
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE459-SRYACT-TBL(IDX)
      *
                         ADD    1    TO   WRK-CNT-MEISAI
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE459-SRYCD(1)  NOT =  SPACE
                      PERFORM 800-RECE459-WRITE-SEC
                  END-IF
           END-PERFORM     
      *
           MOVE    RECE44-REC        TO      RECE449-REC
      *@@@@
           MOVE    RECE44-ZAINUM     TO      RECE449-ZAINUM
           MOVE    RECE44-SRYKBN     TO      RECE449-SRYKBN
      *@@@@
           MOVE    WRK-CNT-MEISAI    TO      RECE449-MEISAISU
           EVALUATE  WRK-JIDO-SETCOMFLG
             WHEN      "1"
               MOVE      "1"         TO      RECE449-SETCOMFLG
             WHEN      "2"
               MOVE      "1"         TO      RECE449-SENTAKUCOMFLG
           END-EVALUATE
           PERFORM 800-RECE449-WRITE-SEC
      *
           .
       200111-SRYACTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ０５　ＳＲＹＡＣＴＩＮＦ編集処理　まとめ
      *****************************************************************
       200111-SRYACTINF-HENSYU-MAT-SEC     SECTION.
      *
           PERFORM    200111-RECE45-TAIHI-SEC
      *
      *    ＲＥＣＥ０５ファイルの内容をテーブルに退避
           MOVE    ZERO                TO  IDX-RECE45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           MOVE    RECE44-MEISAISU     TO  MAX-MEISAISU
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL   (IDX                >   50   )
                        OR    (WK-FREE-NAME2(IDX) =   SPACE)
                COMPUTE  MAX-MEISAISU        =   MAX-MEISAISU  +  1
                IF  WK-FREE-SRYCD2(IDX) = SPACE
                  MOVE "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                ELSE
                  MOVE WK-FREE-SRYCD2(IDX)
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                END-IF
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE   WK-FREE-INPUTCOMENT2(IDX)
                          TO  WRK-RECE45XX-INPUTCOMENT(MAX-MEISAISU 1)
                MOVE   WK-FREE-MEISKYFLG2(IDX)
                          TO  WRK-RECE45XX-MEISKYFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-SETCOMATFLG2(IDX)
                          TO  WRK-RECE45XX-SETCOMATFLG(MAX-MEISAISU 1)
                MOVE   WK-FREE-RECEDENFLG2(IDX)
                          TO  WRK-RECE45XX-RECEDENFLG(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
           END-PERFORM
      *
           PERFORM    200111-SRYACTINF-HENSYU-SEC
      *
           .
       200111-SRYACTINF-HENSYU-MAT-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       200111-JIDOSET-HENSYU-SEC     SECTION.
      *
           IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
             MOVE  "2"                   TO  WK-FREE-MEISKYFLG2(IDZ)
             MOVE  WRK-JIDO-SETCOMATFLG  TO  WK-FREE-SETCOMATFLG2(IDZ)
             MOVE  WRK-JIDO-RECEDENFLG   TO  WK-FREE-RECEDENFLG2(IDZ)
           END-IF
      *
           .
       200111-JIDOSET-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       200111-RECECOMMENT-HENSYU-SEC     SECTION.
      *
           INITIALIZE                  ORCSRECECOMMENTAREA
           MOVE  WRK-RECECOM-AREA   TO ORCSRECECOMMENT-IN-AREA
           CALL  "ORCSRECECOMMENT"  USING  ORCSRECECOMMENTAREA
                                           SPA-AREA
           IF    LNK-RECECOMMENT-RC  =  ZERO
             ADD   1                         TO IDZ
             MOVE  LNK-RECECOMMENT-SRYCD     TO WK-FREE-SRYCD2(IDZ)
             MOVE  LNK-RECECOMMENT-NAME      TO WK-FREE-NAME2(IDZ)
             MOVE  LNK-RECECOMMENT-YUKOKETA  TO WK-FREE-YUKOKETA2(IDZ)
             MOVE  LNK-RECECOMMENT-INPUTCOMMENT
                                         TO  WK-FREE-INPUTCOMENT2(IDZ)
           END-IF
      *
           .
       200111-RECECOMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       200111-RECECOMMENT-HENSYU2-SEC     SECTION.
      *
           INITIALIZE                  ORCSRECECOMMENTAREA
           MOVE  WRK-RECECOM-AREA   TO ORCSRECECOMMENT-IN-AREA
           CALL  "ORCSRECECOMMENT"  USING  ORCSRECECOMMENTAREA
                                           SPA-AREA
           IF    LNK-RECECOMMENT-RC  =  ZERO
             ADD   1         TO IDX-RECE45
             MOVE  LNK-RECECOMMENT-SRYCD
                             TO WRK-RECE45XX-SRYCD(IDX-RECE45 1)
             MOVE  ZERO      TO WRK-RECE45XX-SRYSURYO(IDX-RECE45 1)
             MOVE  LNK-RECECOMMENT-NAME
                             TO WRK-RECE45XX-NAME(IDX-RECE45 1)
             MOVE  LNK-RECECOMMENT-YUKOKETA
                             TO WRK-RECE45XX-YUKOKETA(IDX-RECE45 1)
             MOVE  LNK-RECECOMMENT-INPUTCOMMENT
                             TO WRK-RECE45XX-INPUTCOMENT(IDX-RECE45 1)
             MOVE  "1"       TO WRK-RECE45XX-SIKFLG(IDX-RECE45 1)
           END-IF
      *
           .
       200111-RECECOMMENT-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       200111-RECECOMMENT-HENSYU3-SEC     SECTION.
      *
           ADD    1               TO IDZ
           MOVE  "840000099"      TO WK-FREE-SRYCD2(IDZ)
           MOVE  "検査日　　　日" TO WK-FREE-NAME2(IDZ)
           MOVE   7               TO WK-FREE-YUKOKETA2(IDZ)
      *
           INITIALIZE                         STS-AREA-DAY
           INITIALIZE                         LNK-DAY2-AREA
           MOVE    "21"                   TO  LNK-DAY2-IRAI
           MOVE    WRK-RECECOM-YMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"    USING     STS-AREA-DAY
                                          LNK-DAY2-AREA
           IF      STS-DAY-RC1            =   ZERO
              MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-TAIHI-XX
              INITIALIZE                       ORCSKANACHKAREA
              MOVE    "2"                 TO  KANACHK-SYORI
              MOVE    WRK-TAIHI-XX        TO  KANACHK-MAE-INPUT
              CALL    "ORCSKANACHK"    USING   ORCSKANACHKAREA
              IF      KANACHK-RC      =   ZERO
                 MOVE    KANACHK-OUT-INPUT(1:4)
                                  TO WK-FREE-NAME2(IDZ)(9:4)
                                     WK-FREE-INPUTCOMENT2(IDZ)(1:4)
              END-IF
              PERFORM  VARYING  IDX53  FROM  1  BY  2
                        UNTIL   IDX53    >   4
                 IF  WK-FREE-INPUTCOMENT2(IDZ)(IDX53:2) = "　"
                     MOVE  "０"   TO WK-FREE-INPUTCOMENT2(IDZ)(IDX53:2)
                 END-IF
              END-PERFORM
           END-IF
      *
           MOVE  "1"              TO WK-FREE-RECEDENFLG2(IDZ)
      *
           .
       200111-RECECOMMENT-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    
      *****************************************************************
       200111-RECECOMMENT-IDXGET-SEC     SECTION.
      *
           EVALUATE   IDX-JIDO
             WHEN        1
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100002"   TO   WRK-RCW-SRYCD
             WHEN        2
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100003"   TO   WRK-RCW-SRYCD
             WHEN        3
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100004"   TO   WRK-RCW-SRYCD
             WHEN        4
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100059"   TO   WRK-RCW-SRYCD
             WHEN        5
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100089"   TO   WRK-RCW-SRYCD
             WHEN        6
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100090"   TO   WRK-RCW-SRYCD
             WHEN        7
      *******WHEN      111
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100064"   TO   WRK-RCW-SRYCD
             WHEN        8
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100058"   TO   WRK-RCW-SRYCD
             WHEN        9
      *******WHEN       10
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100051"   TO   WRK-RCW-SRYCD
             WHEN       11
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190004"   TO   WRK-RCW-SRYCD
             WHEN      511
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190004"   TO   WRK-RCW-SRYCD
             WHEN       12
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850190002"   TO   WRK-RCW-SRYCD
             WHEN      512
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190002"   TO   WRK-RCW-SRYCD
             WHEN       13
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850190005"   TO   WRK-RCW-SRYCD
             WHEN      513
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190005"   TO   WRK-RCW-SRYCD
             WHEN       17
      *******WHEN       18
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190043"   TO   WRK-RCW-SRYCD
             WHEN      517
      *******WHEN      518
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190043"   TO   WRK-RCW-SRYCD
             WHEN       19
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100082"   TO   WRK-RCW-SRYCD
             WHEN      519
                MOVE   "4"           TO   WRK-RCW-KBN
                MOVE   "842100030"   TO   WRK-RCW-SRYCD
             WHEN       20
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100054"   TO   WRK-RCW-SRYCD
             WHEN       21
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850190001"   TO   WRK-RCW-SRYCD
             WHEN       23
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850190001"   TO   WRK-RCW-SRYCD
             WHEN       25
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850100140"   TO   WRK-RCW-SRYCD
             WHEN       27
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850100145"   TO   WRK-RCW-SRYCD
             WHEN       29
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100060"   TO   WRK-RCW-SRYCD
             WHEN       30
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100061"   TO   WRK-RCW-SRYCD
             WHEN       31
      *******WHEN       32
                MOVE   "4"           TO   WRK-RCW-KBN
                MOVE   "842100029"   TO   WRK-RCW-SRYCD
             WHEN       34
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100137"   TO   WRK-RCW-SRYCD
             WHEN       35
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100093"   TO   WRK-RCW-SRYCD
             WHEN       36
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100097"   TO   WRK-RCW-SRYCD
             WHEN       38
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190006"   TO   WRK-RCW-SRYCD
             WHEN      538
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190006"   TO   WRK-RCW-SRYCD
             WHEN       39
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100129"   TO   WRK-RCW-SRYCD
             WHEN      539
                MOVE   "4"           TO   WRK-RCW-KBN
                MOVE   "842100043"   TO   WRK-RCW-SRYCD
             WHEN       41
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100244"   TO   WRK-RCW-SRYCD
             WHEN       42
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100244"   TO   WRK-RCW-SRYCD
             WHEN       43
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100244"   TO   WRK-RCW-SRYCD
             WHEN       44
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100244"   TO   WRK-RCW-SRYCD
             WHEN       45
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       46
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       47
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       48
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       49
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       50
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       51
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       52
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       53
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       54
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100261"   TO   WRK-RCW-SRYCD
             WHEN       55
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100266"   TO   WRK-RCW-SRYCD
             WHEN       56
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100266"   TO   WRK-RCW-SRYCD
             WHEN       57
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100142"   TO   WRK-RCW-SRYCD
             WHEN       59
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100270"   TO   WRK-RCW-SRYCD
             WHEN       60
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100286"   TO   WRK-RCW-SRYCD
             WHEN      560
                MOVE   "4"           TO   WRK-RCW-KBN
                MOVE   "842100061"   TO   WRK-RCW-SRYCD
             WHEN       61
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100272"   TO   WRK-RCW-SRYCD
             WHEN      561
                MOVE   "4"           TO   WRK-RCW-KBN
                MOVE   "842100056"   TO   WRK-RCW-SRYCD
             WHEN     1061
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100273"   TO   WRK-RCW-SRYCD
             WHEN       62
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100272"   TO   WRK-RCW-SRYCD
             WHEN      562
                MOVE   "4"           TO   WRK-RCW-KBN
                MOVE   "842100056"   TO   WRK-RCW-SRYCD
             WHEN       64
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100245"   TO   WRK-RCW-SRYCD
             WHEN       65
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100249"   TO   WRK-RCW-SRYCD
             WHEN       66
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100245"   TO   WRK-RCW-SRYCD
             WHEN       67
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100235"   TO   WRK-RCW-SRYCD
             WHEN      567
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100236"   TO   WRK-RCW-SRYCD
             WHEN       68
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100209"   TO   WRK-RCW-SRYCD
             WHEN      568
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100208"   TO   WRK-RCW-SRYCD
             WHEN       69
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100227"   TO   WRK-RCW-SRYCD
             WHEN      569
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100220"   TO   WRK-RCW-SRYCD
             WHEN       70
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100218"   TO   WRK-RCW-SRYCD
             WHEN      570
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100211"   TO   WRK-RCW-SRYCD
             WHEN     1070
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100389"   TO   WRK-RCW-SRYCD
             WHEN     1570
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100390"   TO   WRK-RCW-SRYCD
             WHEN     2070
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100439"   TO   WRK-RCW-SRYCD
             WHEN       71
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100224"   TO   WRK-RCW-SRYCD
             WHEN      571
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100217"   TO   WRK-RCW-SRYCD
             WHEN     1071
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100391"   TO   WRK-RCW-SRYCD
             WHEN     1571
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100392"   TO   WRK-RCW-SRYCD
             WHEN     2071
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100440"   TO   WRK-RCW-SRYCD
             WHEN       72
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100230"   TO   WRK-RCW-SRYCD
             WHEN      572
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100224"   TO   WRK-RCW-SRYCD
             WHEN       73
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100221"   TO   WRK-RCW-SRYCD
             WHEN      573
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100214"   TO   WRK-RCW-SRYCD
             WHEN       74
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100215"   TO   WRK-RCW-SRYCD
             WHEN      574
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100216"   TO   WRK-RCW-SRYCD
             WHEN     1074
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100217"   TO   WRK-RCW-SRYCD
             WHEN     1574
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100799"   TO   WRK-RCW-SRYCD
             WHEN       75
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100212"   TO   WRK-RCW-SRYCD
             WHEN      575
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100213"   TO   WRK-RCW-SRYCD
             WHEN     1075
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100214"   TO   WRK-RCW-SRYCD
             WHEN     1575
                MOVE   "3"           TO   WRK-RCW-KBN
                MOVE   "830100531"   TO   WRK-RCW-SRYCD
             WHEN       97
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   SPACE         TO   WRK-RCW-SRYCD
             WHEN       98
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850100110"   TO   WRK-RCW-SRYCD
             WHEN       99
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850100267"   TO   WRK-RCW-SRYCD
             WHEN      105
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100238"   TO   WRK-RCW-SRYCD
             WHEN      106
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100239"   TO   WRK-RCW-SRYCD
             WHEN      107
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100240"   TO   WRK-RCW-SRYCD
             WHEN      114
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100246"   TO   WRK-RCW-SRYCD
             WHEN      115
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100143"   TO   WRK-RCW-SRYCD
             WHEN      116
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100143"   TO   WRK-RCW-SRYCD
             WHEN      118
                MOVE   "2"           TO   WRK-RCW-KBN
                MOVE   "850100402"   TO   WRK-RCW-SRYCD
             WHEN      618
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190049"   TO   WRK-RCW-SRYCD
             WHEN      119
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100406"   TO   WRK-RCW-SRYCD
             WHEN      120
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100407"   TO   WRK-RCW-SRYCD
             WHEN      121
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190198"   TO   WRK-RCW-SRYCD
             WHEN      123
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100247"   TO   WRK-RCW-SRYCD
             WHEN      124
      *******WHEN      125
                MOVE   "4"           TO   WRK-RCW-KBN
                MOVE   "842100111"   TO   WRK-RCW-SRYCD
             WHEN      126
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190210"   TO   WRK-RCW-SRYCD
             WHEN      127
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100411"   TO   WRK-RCW-SRYCD
             WHEN      627
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820100846"   TO   WRK-RCW-SRYCD
             WHEN      128
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100412"   TO   WRK-RCW-SRYCD
             WHEN      129
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100413"   TO   WRK-RCW-SRYCD
             WHEN      130
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100414"   TO   WRK-RCW-SRYCD
             WHEN      131
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190211"   TO   WRK-RCW-SRYCD
             WHEN      132
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190056"   TO   WRK-RCW-SRYCD
             WHEN      632
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190052"   TO   WRK-RCW-SRYCD
             WHEN      133
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100417"   TO   WRK-RCW-SRYCD
             WHEN      134
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190004"   TO   WRK-RCW-SRYCD
             WHEN      634
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190004"   TO   WRK-RCW-SRYCD
             WHEN      135
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100239"   TO   WRK-RCW-SRYCD
             WHEN      136
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100245"   TO   WRK-RCW-SRYCD
             WHEN      137
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100418"   TO   WRK-RCW-SRYCD
             WHEN      637
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190052"   TO   WRK-RCW-SRYCD
             WHEN      138
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100490"   TO   WRK-RCW-SRYCD
             WHEN      139
                MOVE   "5"           TO   WRK-RCW-KBN
                MOVE   "820190053"   TO   WRK-RCW-SRYCD
             WHEN      140
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100419"   TO   WRK-RCW-SRYCD
             WHEN      141
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850190234"   TO   WRK-RCW-SRYCD
      *
             WHEN     9901
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100106"   TO   WRK-RCW-SRYCD
             WHEN     9902
                MOVE   "1"           TO   WRK-RCW-KBN
                MOVE   "850100107"   TO   WRK-RCW-SRYCD
      *
             WHEN     OTHER
                MOVE   SPACE         TO   WRK-RCW-KBN
                MOVE   SPACE         TO   WRK-RCW-SRYCD
           END-EVALUATE
           MOVE        RECE41-SRYYM  TO   WRK-RCW-SRYYMD(1:6)
           MOVE        "01"          TO   WRK-RCW-SRYYMD(7:2)
      *
           INITIALIZE                     WRK-RECECOM-AREA
           MOVE   WRK-RCW-KBN        TO   WRK-RECECOM-KBN
           MOVE   WRK-RCW-SRYCD      TO   WRK-RECECOM-SRYCD
           MOVE   WRK-RCW-SRYYMD     TO   WRK-RECECOM-SRYYMD
      *
           .
       200111-RECECOMMENT-IDXGET-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関する編集２
      *****************************************************************
       240-YAKUTEI-HENSYU2-SEC          SECTION.
      *
      *    並び替え
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE44-REC(IDX6) =  SPACE)
      *
              COMPUTE    IDX7  =  IDX6  +  1
      *
              PERFORM    VARYING   IDX8   FROM   IDX7   BY   1
                          UNTIL   (IDX8                     >   100 )
                            OR    (TBL-YT1-RECE44-REC(IDX8) =  SPACE)
      *
                IF       TBL-YT1-KEY(IDX6)  >   TBL-YT1-KEY(IDX8)
                   MOVE  TBL-YT1-OCC(IDX6)  TO  WRK-YAKUTEI-DATA
                   MOVE  TBL-YT1-OCC(IDX8)  TO  TBL-YT1-OCC(IDX6)
                   MOVE  WRK-YAKUTEI-DATA   TO  TBL-YT1-OCC(IDX8)
                END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
      *    出力処理
           INITIALIZE                   TBL-YAKUTEI-INF2
           INITIALIZE                   TBL-YAKUTEI-INF3
           MOVE       SPACE     TO      WRK-TAIHI-YT1-KEY
      *
           MOVE       ZERO      TO      IDX11
      *
           PERFORM    VARYING   IDX6    FROM   1   BY   1
                       UNTIL   (IDX6                     >   100 )
                         OR    (TBL-YT1-RECE44-REC(IDX6) =  SPACE)
      *
              IF     TBL-YT1-KEY(IDX6)   NOT =  WRK-TAIHI-YT1-KEY
      *
                 IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
                   PERFORM   2401-YAKUTEI-MATOME-SEC
                   PERFORM   2402-YAKUTEI-WRITE-SEC
                   INITIALIZE                      TBL-YAKUTEI-INF3
                 END-IF
      *
                 INITIALIZE                        TBL-YAKUTEI-INF2
                 MOVE   TBL-YT1-KEY(IDX6)      TO  WRK-TAIHI-YT1-KEY
      *
              END-IF
      *
              IF     TBL-YT1-FLG(IDX6)   =    1
      *
                   PERFORM   VARYING   IDX7    FROM   1   BY   1
                              UNTIL   (IDX7      >    31)
      *
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8      >    9)
      *
                        IF   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                                        NOT =  ZERO
                           PERFORM   VARYING   IDX9    FROM   1   BY   1
                            UNTIL (IDX9      >    25)
                              OR  (TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   25
                             DISPLAY "TABLE OVER2"
                           ELSE
                             MOVE   TBL-YT1-RECE44-REC(IDX6)
                                TO  TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X1-KAISU(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X1-DAY(IDX7 IDX8 IDX9 IDX7)
                             MOVE   TBL-YT1-COMNAME-AREA(IDX6)
                                TO  TBL-YT2X1-COMNAME-AREA
                                                        (IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-HIKAKU(IDX6)
                                TO  TBL-YT2Y1-HIKAKU(IDX7 IDX8 IDX9)
      *
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-MEISAISU(IDX6)        *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8) *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL2
                                                       (IDX7 IDX8 IDX9)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL3
                                                       (IDX7 IDX8 IDX9)
                             COMPUTE WRK-RUIKEITOTAL2 =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             MOVE    WRK-RUIKEITOTAL2
                                TO  TBL-YT2Y1-RUIKEITOTAL4
                                                       (IDX7 IDX8 IDX9)
      *
                             ADD    TBL-YT1-ZAITEN(IDX6)
                                TO  TBL-YT2Y1-ZAITEN(IDX7 IDX8 26)
                             ADD    TBL-YT1-SRYCDTOTAL(IDX6)
                                TO  TBL-YT2Y1-SRYCDTOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-SURYOUTOTAL(IDX6)
                                TO  TBL-YT2Y1-SURYOUTOTAL(IDX7 IDX8 26)
                             ADD    TBL-YT1-RUIKEITOTAL(IDX6)
                                TO  TBL-YT2Y1-RUIKEITOTAL(IDX7 IDX8 26)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-MEISAISU(IDX6)        *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8) *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL2(IDX7 IDX8 26)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y1-RUIKEITOTAL3(IDX7 IDX8 26)
                             COMPUTE WRK-RUIKEITOTAL2 =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             ADD     WRK-RUIKEITOTAL2
                                TO  TBL-YT2Y1-RUIKEITOTAL4(IDX7 IDX8 26)
                             ADD    TBL-YT1-MEISAISU(IDX6)
                                TO  TBL-YT2Y1-MEISAISU(IDX7 IDX8 26)
      *
                           END-IF
                        END-IF
      *
                     END-PERFORM
      *
                   END-PERFORM
      *
              ELSE
      *
                   PERFORM   VARYING   IDX7    FROM   1   BY   1
                              UNTIL   (IDX7      >    31)
      *
                     PERFORM   VARYING   IDX8    FROM   1   BY   1
                                UNTIL   (IDX8      >    9)
      *
                        IF   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                                        NOT =  ZERO
                           PERFORM   VARYING   IDX9    FROM   1   BY   1
                            UNTIL (IDX9      >    3)
                              OR  (TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX9)
                                                              =  SPACE)
                                 CONTINUE
                           END-PERFORM
                           IF    IDX9   >   3
                             DISPLAY "TABLE OVER3"
                           ELSE
                             MOVE   TBL-YT1-RECE44-REC(IDX6)
                                TO  TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X2-KAISU(IDX7 IDX8 IDX9)
                             MOVE   TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                                TO  TBL-YT2X2-DAY(IDX7 IDX8 IDX9 IDX7)
                             MOVE   TBL-YT1-HIKAKU(IDX6)
                                TO  TBL-YT2Y2-HIKAKU(IDX7 IDX8 IDX9)
      *
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-MEISAISU(IDX6)        *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8) *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL2
                                                       (IDX7 IDX8 IDX9)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             MOVE    WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL3
                                                       (IDX7 IDX8 IDX9)
                             COMPUTE WRK-RUIKEITOTAL2 =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             MOVE    WRK-RUIKEITOTAL2
                                TO  TBL-YT2Y2-RUIKEITOTAL4
                                                       (IDX7 IDX8 IDX9)
      *
                             ADD    TBL-YT1-ZAITEN(IDX6)
                                TO  TBL-YT2Y2-ZAITEN(IDX7 IDX8 4)
                             ADD    TBL-YT1-SRYCDTOTAL(IDX6)
                                TO  TBL-YT2Y2-SRYCDTOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-SURYOUTOTAL(IDX6)
                                TO  TBL-YT2Y2-SURYOUTOTAL(IDX7 IDX8 4)
                             ADD    TBL-YT1-RUIKEITOTAL(IDX6)
                                TO  TBL-YT2Y2-RUIKEITOTAL(IDX7 IDX8 4)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-MEISAISU(IDX6)        *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8) *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL2(IDX7 IDX8 4)
                             COMPUTE WRK-RUIKEITOTAL  =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-SURYOUTOTAL(IDX6)
                             ADD     WRK-RUIKEITOTAL
                                TO  TBL-YT2Y2-RUIKEITOTAL3(IDX7 IDX8 4)
                             COMPUTE WRK-RUIKEITOTAL2 =
                                     TBL-YT1-SRYCDTOTAL(IDX6)      *
                                     TBL-YT1-KAISU(IDX6 IDX7 IDX8)
                             ADD     WRK-RUIKEITOTAL2
                                TO  TBL-YT2Y2-RUIKEITOTAL4(IDX7 IDX8 4)
                             ADD    TBL-YT1-MEISAISU(IDX6)
                                TO  TBL-YT2Y2-MEISAISU(IDX7 IDX8 4)
      *
                           END-IF
                        END-IF
      *
                     END-PERFORM
      *
                   END-PERFORM
      *
              END-IF
      *
           END-PERFORM
      *
           IF   WRK-TAIHI-YT1-KEY  NOT =  SPACE
              PERFORM   2401-YAKUTEI-MATOME-SEC
              PERFORM   2402-YAKUTEI-WRITE-SEC
           END-IF
      *
           .
       240-YAKUTEI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイルまとめ処理
      *****************************************************************
       2401-YAKUTEI-MATOME-SEC          SECTION.
      *
           PERFORM   VARYING   IDX7    FROM   1   BY   1
                      UNTIL   (IDX7      >    31)
      *
              PERFORM   VARYING   IDX8    FROM   1   BY   1
                         UNTIL   (IDX8      >    9)
      *
               IF  (TBL-YT2Y1-HIKAKU(IDX7 IDX8 26)  NOT = ZERO)
               OR  (TBL-YT2Y2-HIKAKU(IDX7 IDX8  4)  NOT = ZERO)
                 MOVE     ZERO     TO    FLG-YAKUTEI
                 PERFORM    VARYING   IDX9    FROM   1   BY   1
                       UNTIL   (IDX9                          >   100 )
                         OR    (TBL-YT3X1-RECE44-REC(IDX9 1)  =  SPACE)
                         OR    (FLG-YAKUTEI                   =   1   )
      *
                    IF   (TBL-YT2Y1-HIKAKU(IDX7 IDX8 26) =
                                              TBL-YT3Y1-HIKAKU(IDX9 26))
                    AND  (TBL-YT2Y2-HIKAKU(IDX7 IDX8  4) =
                                              TBL-YT3Y2-HIKAKU(IDX9  4))
                      INITIALIZE         WRK-INDEX2-AREA
                      MOVE   ZERO   TO   FLG-YAKUTEI-CHECK
                      PERFORM    24011-YAKUTEI-CHECK-SEC
                      IF  FLG-YAKUTEI-CHECK = ZERO
                        PERFORM  24012-YAKUTEI-HENSYU-SEC
                      END-IF
                    END-IF
      *
                 END-PERFORM
                 IF  FLG-YAKUTEI  =  ZERO
                    IF   IDX9    >    100
                       DISPLAY   "TABLE OVER4"
                    ELSE
                       MOVE   TBL-YT2X-KAIT(IDX7 IDX8)
                                      TO  TBL-YT3X-KAIT(IDX9)
                       MOVE   TBL-YT2Y-KAIT(IDX7 IDX8)
                                      TO  TBL-YT3Y-KAIT(IDX9)
                    END-IF
                 END-IF
               END-IF
      *
              END-PERFORM
      *
           END-PERFORM
      *
           .
       2401-YAKUTEI-MATOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイルコメントチェック処理
      *****************************************************************
       24011-YAKUTEI-CHECK-SEC          SECTION.
      *
           MOVE      ZERO    TO   FLG-YAKUTEI2
           PERFORM  VARYING  IDX17  FROM  1  BY  1
                     UNTIL  (IDX17                            >  25   )
                      OR    (TBL-YT3X1-RECE44-REC(IDX9 IDX17) =  SPACE)
             MOVE    ZERO    TO   FLG-YAKUTEI2
             PERFORM  VARYING  IDX10  FROM  1  BY  1
                        UNTIL (IDX10                          >  25   )
                         OR   (TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                         OR   (FLG-YAKUTEI2                   =  1    )
               IF   TBL-YT3Y1-HIKAKU(IDX9 IDX17)      =
                    TBL-YT2Y1-HIKAKU(IDX7 IDX8 IDX10)
                 IF   (TBL-YT3X1-COMCD(IDX9 IDX17 1)      NOT = SPACE)
                 AND  (TBL-YT2X1-COMCD(IDX7 IDX8 IDX10 1) NOT = SPACE)
                   MOVE   ZERO    TO  FLG-YAKUTEI-NASHI
                   PERFORM   240111-YAKUTEI-CHECK2-SEC
                   IF  FLG-YAKUTEI-NASHI = ZERO
                     MOVE  1      TO  FLG-YAKUTEI2
                   END-IF
                 ELSE
                   MOVE    1      TO  FLG-YAKUTEI2
                 END-IF
                 IF   FLG-YAKUTEI2 = 1
                   MOVE   IDX10   TO  WRK-INDEX2(IDX17)
                 END-IF
               END-IF
             END-PERFORM
             IF  FLG-YAKUTEI2 = ZERO
               MOVE   1     TO   FLG-YAKUTEI-CHECK
               MOVE   25    TO   IDX17
             END-IF
           END-PERFORM
      *
           .
       24011-YAKUTEI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイルコメントチェック処理２
      *****************************************************************
       240111-YAKUTEI-CHECK2-SEC          SECTION.
      *
           MOVE      ZERO    TO    IDX60
           INITIALIZE              WRK-INDEX-AREA
      *
           PERFORM  VARYING  IDX57  FROM  1  BY  1
                     UNTIL  (IDX57                             >  5   )
                      OR    (TBL-YT3X1-COMCD(IDX9 IDX17 IDX57) = SPACE)
      *
             MOVE    ZERO    TO    FLG-YAKUTEI-COMARI
             PERFORM VARYING IDX58  FROM    1    BY   1
                      UNTIL (IDX58                             >  5   )
                       OR   (TBL-YT2X1-COMCD(IDX7 IDX8 IDX10 IDX58)
                                                               = SPACE)
               IF  (TBL-YT3X1-COMCD  (IDX9 IDX17 IDX57)
                          =  TBL-YT2X1-COMCD  (IDX7 IDX8 IDX10 IDX58))
               AND (TBL-YT3X1-COMNAME(IDX9 IDX17 IDX57)
                          =  TBL-YT2X1-COMNAME(IDX7 IDX8 IDX10 IDX58))
                 MOVE     ZERO     TO   FLG-YAKUTEI-NG
                 PERFORM  VARYING  IDX59  FROM  1  BY  1
                           UNTIL  (IDX59            >  5  )
                            OR    (WRK-INDEX(IDX59) = ZERO)
                   IF  WRK-INDEX(IDX59)  =  IDX58
                     MOVE   1      TO   FLG-YAKUTEI-NG
                     MOVE   5      TO   IDX59
                   END-IF
                 END-PERFORM
                 IF   FLG-YAKUTEI-NG = 1
                   CONTINUE
                 ELSE
                   MOVE     1      TO   FLG-YAKUTEI-COMARI
                   ADD      1      TO   IDX60
                   MOVE     IDX58  TO   WRK-INDEX(IDX60)
                   MOVE     5      TO   IDX58
                 END-IF
               END-IF
             END-PERFORM
             IF   FLG-YAKUTEI-COMARI = ZERO
                  MOVE      1      TO   FLG-YAKUTEI-NASHI
                  MOVE      5      TO   IDX57
             END-IF
      *
           END-PERFORM
      *
           .
       240111-YAKUTEI-CHECK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイル編集処理
      *****************************************************************
       24012-YAKUTEI-HENSYU-SEC          SECTION.
      *
           MOVE      ZERO    TO   FLG-YAKUTEI2
           PERFORM  VARYING  IDX17  FROM  1  BY  1
                     UNTIL  (IDX17                            >  25   )
                      OR    (TBL-YT3X1-RECE44-REC(IDX9 IDX17) =  SPACE)
             MOVE    ZERO    TO   FLG-YAKUTEI2
             PERFORM  VARYING  IDX10  FROM  1  BY  1
                        UNTIL (IDX10                          >  25   )
                         OR   (TBL-YT2X1-RECE44-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                         OR   (FLG-YAKUTEI2                   =  1    )
      ***      IF   TBL-YT3Y1-HIKAKU(IDX9 IDX17)      =
      ***           TBL-YT2Y1-HIKAKU(IDX7 IDX8 IDX10)
               IF   WRK-INDEX2(IDX17)  =  IDX10
                 ADD   TBL-YT2X1-KAISU(IDX7 IDX8 IDX10)
                             TO   TBL-YT3X1-KAISU(IDX9 IDX17)
                 ADD   TBL-YT2X1-DAY(IDX7 IDX8 IDX10 IDX7)
                             TO   TBL-YT3X1-DAY(IDX9 IDX17 IDX7)
                 MOVE    1   TO   FLG-YAKUTEI2
               END-IF
             END-PERFORM
           END-PERFORM
      *
           MOVE      ZERO    TO   FLG-YAKUTEI2
           PERFORM  VARYING  IDX17  FROM  1  BY  1
                     UNTIL  (IDX17                            >  3    )
                      OR    (TBL-YT3X2-RECE44-REC(IDX9 IDX17) =  SPACE)
             MOVE    ZERO    TO   FLG-YAKUTEI2
             PERFORM  VARYING  IDX10  FROM  1  BY  1
                       UNTIL  (IDX10                          >  3    )
                        OR    (TBL-YT2X2-RECE44-REC(IDX7 IDX8 IDX10)
                                                              =  SPACE)
                        OR    (FLG-YAKUTEI2                   =  1    )
               IF   TBL-YT3Y2-HIKAKU(IDX9 IDX17)      =
                    TBL-YT2Y2-HIKAKU(IDX7 IDX8 IDX10)
                 ADD   TBL-YT2X2-KAISU(IDX7 IDX8 IDX10)
                             TO   TBL-YT3X2-KAISU(IDX9 IDX17)
                 ADD   TBL-YT2X2-DAY(IDX7 IDX8 IDX10 IDX7)
                             TO   TBL-YT3X2-DAY(IDX9 IDX17 IDX7)
                 MOVE   1    TO   FLG-YAKUTEI2
               END-IF
             END-PERFORM
           END-PERFORM
      *
           MOVE      1       TO   FLG-YAKUTEI
      *
           .
       24012-YAKUTEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイル出力
      *****************************************************************
       2402-YAKUTEI-WRITE-SEC          SECTION.
      *
           PERFORM   VARYING   IDX7    FROM   1   BY   1
                      UNTIL   (IDX7                          >   100 )
                        OR    (TBL-YT3X1-RECE44-REC(IDX7 1)  =  SPACE)
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                             UNTIL   (IDX8                     >   25  )
                               OR    (TBL-YT3X1-RECE44-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE449-REC
                      MOVE   TBL-YT3X1-RECE44-REC(IDX7 IDX8)
                                  TO   RECE449-REC
                                       WRK-RECE44-REC
      *@@@@
                      MOVE   WRK-RECE44-ZAINUM
                                  TO   RECE449-ZAINUM
                      MOVE   WRK-RECE44-SRYKBN
                                  TO   RECE449-SRYKBN
      *@@@@
                      MOVE   RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      MOVE   TBL-YT3X1-KAISU(IDX7 IDX8)
                                  TO   RECE449-ZAIKAISU
                      INITIALIZE       RECE449-DAY-AREA
                      PERFORM VARYING IDX17 FROM 1 BY 1
                               UNTIL  IDX17   > 31
                        MOVE   TBL-YT3X1-DAY(IDX7 IDX8 IDX17)
                                  TO   RECE449-KAISU(1 IDX17)
                      END-PERFORM
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE449-ZAINUM
                      MOVE      RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      PERFORM 800-RECE449-WRITE-SEC
                      PERFORM   24021-YAKUTEI-RECE45-TENSOU-SEC
      *
                 END-PERFORM
      *
                 PERFORM    VARYING   IDX8    FROM   1   BY   1
                              UNTIL  (IDX8                     >    3  )
                                OR   (TBL-YT3X2-RECE44-REC(IDX7 IDX8)
                                                               =  SPACE)
      *
                      INITIALIZE       RECE449-REC
                      MOVE   TBL-YT3X2-RECE44-REC(IDX7 IDX8)
                                  TO   RECE449-REC
                                       WRK-RECE44-REC
      *@@@@
                      MOVE   WRK-RECE44-ZAINUM
                                  TO   RECE449-ZAINUM
                      MOVE   WRK-RECE44-SRYKBN
                                  TO   RECE449-SRYKBN
      *@@@@
                      MOVE   RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM
                      MOVE   TBL-YT3X2-KAISU(IDX7 IDX8)
                                  TO   RECE449-ZAIKAISU
                      INITIALIZE       RECE449-DAY-AREA
                      PERFORM VARYING IDX17 FROM 1 BY 1
                               UNTIL  IDX17   > 31
                        MOVE   TBL-YT3X2-DAY(IDX7 IDX8 IDX17)
                                  TO   RECE449-KAISU(1 IDX17)
                      END-PERFORM
                      ADD       1             TO  IDX11
                      COMPUTE   WRK-ZAINUM    =  99990000  +  IDX11
                      MOVE      WRK-ZAINUM    TO  RECE449-ZAINUM
                      MOVE      RECE449-ZAINUM  TO  WRK-YAKUTEI-ZAINUM2
      *
                      PERFORM 800-RECE449-WRITE-SEC
                      PERFORM   24021-YAKUTEI-RECE45-TENSOU-SEC
      *
                 END-PERFORM
      *
           END-PERFORM
      *
           .
       2402-YAKUTEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    内服薬剤料逓減に関するファイル出力（４５ファイル）
      *****************************************************************
       24021-YAKUTEI-RECE45-TENSOU-SEC          SECTION.
      *
           PERFORM 800-RECE449-SET45-SEC
           MOVE    WRK-YAKUTEI-ZAINUM   TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE45             =   1                  )
               OR     ( RECE449-HOSPNUM    NOT =   RECE45-HOSPNUM     )
               OR     ( RECE449-SRYYM      NOT =   RECE45-SRYYM       )
               OR     ( RECE449-NYUGAIKBN  NOT =   RECE45-NYUGAIKBN   )
               OR     ( RECE449-PTID       NOT =   RECE45-PTID        )
               OR     ( RECE449-RECEKA     NOT =   RECE45-RECEKA      )
               OR     ( RECE449-HKNID      NOT =   RECE45-HKNID       )
               OR     ( RECE449-TEKSTYMD   NOT =   RECE45-TEKSTYMD    )
               OR     ( WRK-YAKUTEI-ZAINUM NOT =   RECE45-ZAINUM      )
      *
               MOVE     RECE45-REC            TO      RECE459-REC
               MOVE     WRK-YAKUTEI-ZAINUM2   TO      RECE459-ZAINUM
               PERFORM 800-RECE459-WRITE-SEC
      *
               PERFORM 990-RECE45-NEXT-SEC
      *
           END-PERFORM
      *
           .
       24021-YAKUTEI-RECE45-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    差込編集２
      *****************************************************************
       250-SASHIKOMI2-SEC     SECTION.
      *
      *    パラメータ４４９ファイルより編集
           PERFORM 800-RECE41-SET449-SEC
      *@@@@
           MOVE    SPACE               TO  RECE449-SRYKBN
      *@@@@
           MOVE    ZERO                TO  RECE449-ZAINUM
      *
           START   RECE449-FILE
               KEY IS    >=   RECE449-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE449
               NOT INVALID
                   PERFORM 990-RECE449-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE449          =   1                    )
               OR     ( RECE41-HOSPNUM   NOT =   RECE449-HOSPNUM      )
               OR     ( RECE41-SRYYM     NOT =   RECE449-SRYYM        )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE449-NYUGAIKBN    )
               OR     ( RECE41-PTID      NOT =   RECE449-PTID         )
               OR     ( RECE41-RECEKA    NOT =   RECE449-RECEKA       )
               OR     ( RECE41-HKNID     NOT =   RECE449-HKNID        )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE449-TEKSTYMD     )
      *
               MOVE   ZERO   TO  FLG-COMMENT
      *        （酸素計算式のコメントのセット）
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2501-SANSO-HANTEI-SEC
               END-IF
      *        （フイルムのコメントのセット）
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2502-FILM-HANTEI-SEC
               END-IF
      *        （外来管理加算（読み替え）のセット）
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2503-GAIKAN-HANTEI-SEC
               END-IF
               IF   FLG-COMMENT  =  ZERO
                 PERFORM    2599-RECE449-TENSOU-SEC
               END-IF
      *
               PERFORM 990-RECE449-NEXT-SEC
      *
           END-PERFORM
      *
           .
       250-SASHIKOMI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素があるか判定
      *****************************************************************
       2501-SANSO-HANTEI-SEC          SECTION.
      *
           IF     RECE449-FLG-SANSO-COMMENT   =    "1"
               INITIALIZE WRK-JIDO-SETCOM-AREA
               MOVE       99      TO     FLG-COMMENT
               PERFORM    25011-SANSO-TAIHI-SEC
               PERFORM    25011-SANSO-COMMENT-SEC
               IF RECE449-FLG-GAIKAN-COMMENT  =    "1"
                 PERFORM  25032-GAIKAN-INS-HENSYU2-SEC
               END-IF
               PERFORM    2509-SRYACTINF2-KYOTU-HEN-SEC
           END-IF
      *
           .
       2501-SANSO-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素の数量・単価退避
      *****************************************************************
       25011-SANSO-TAIHI-SEC          SECTION.
      *
           MOVE       ZERO    TO     IDX4
           INITIALIZE                TBL-SANSO-INF
      *
           PERFORM 800-RECE449-SET459-SEC
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
               KEY IS    >=   RECE459-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE459
               NOT INVALID
                   PERFORM 990-RECE459-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE459           =   1                 )
               OR     ( RECE449-HOSPNUM   NOT =   RECE459-HOSPNUM   )
               OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM     )
               OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN )
               OR     ( RECE449-PTID      NOT =   RECE459-PTID      )
               OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA    )
               OR     ( RECE449-HKNID     NOT =   RECE459-HKNID     )
               OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD  )
               OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM    )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                          UNTIL  ( IDX                >   5     )
                           OR    ( RECE459-SRYCD(IDX) =   SPACE )
      *
                  SET     TBL-SANSO    TO     1
                  SEARCH      TBL-SANSO-OCC   VARYING   TBL-SANSO
                       AT  END
                           MOVE    ZERO    TO   FLG-SANSO-OK
                       WHEN   RECE459-SRYCD(IDX) = TBL-SANSO-SRYCD
                                                       (TBL-SANSO)
                           MOVE    1       TO   FLG-SANSO-OK
                  END-SEARCH
                  IF      FLG-SANSO-OK        =    1
                     ADD      1             TO  IDX4
                     MOVE  RECE459-TEN(IDX) TO  TBL-SANSO-TEN(IDX4)
                     MOVE  RECE459-SRYSURYO(IDX)
                                            TO  TBL-SANSO-SRYSURYO
                                                             (IDX4)
                  END-IF
                  IF    RECE459-SRYCD(IDX) = "770030070"
                     IF   TBL-KASAN-SRYSURYO  =  ZERO
                        MOVE   RECE459-SRYSURYO(IDX)
                                     TO  TBL-KASAN-SRYSURYO
                     ELSE
                        COMPUTE   TBL-KASAN-SRYSURYO  =  
                                  TBL-KASAN-SRYSURYO  *  
                                  RECE459-SRYSURYO(IDX)  
                     END-IF
                  END-IF
      *
               END-PERFORM 
      *
               PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25011-SANSO-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素のコメント編集（差込：下）
      *****************************************************************
       25011-SANSO-COMMENT-SEC          SECTION.
      *
           MOVE       ZERO       TO   IDZ                               
           INITIALIZE            WRK-FREE-AREA2                         
           INITIALIZE            WRK-FREE-AREA3
      *
           PERFORM  250111-SANSO-FREE-COMMENT-SEC
      *
           PERFORM  250111-RECE459-TAIHI-SEC
      *
      *    ＲＥＣＥ４５９ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           MOVE    ZERO                 TO  WRK-JYUNBAN
                                            WRK-GYOSUU
                                            WRK-GYOSUU2
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   50    )
                         OR      ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD       1      TO     WRK-GYOSUU
      *
           END-PERFORM 
      *
           PERFORM    250111-SRYACTINF2-HENSYU-SEC
      *
           .
       25011-SANSO-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素計算式編集
      *****************************************************************
       250111-SANSO-FREE-COMMENT-SEC          SECTION.                 
      *
           MOVE     1          TO    WRK-KETA
           MOVE    ZERO        TO    WRK-KEISAN4
      *
           PERFORM   VARYING   IDX    FROM   1   BY   1
                      UNTIL   (IDX                 >    10)
                       OR     (TBL-SANSO-TEN(IDX)  =  ZERO)
      *
             IF   IDX  >  1
                MOVE    "＋"   TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
             END-IF
      *
             MOVE    "（"   TO  WK-FREE-NAME3(WRK-KETA:2)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *単価
             MOVE     TBL-SANSO-TEN(IDX)   TO   WRK-KEISAN
             COMPUTE  WRK-KEISAN2  = (TBL-SANSO-TEN(IDX) -  WRK-KEISAN)
                                   *   1000
      *
             PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
             MOVE    "円×" TO  WK-FREE-NAME3(WRK-KETA:4)
             COMPUTE  WRK-KETA   =  WRK-KETA   +  4
      *数量
             MOVE     TBL-SANSO-SRYSURYO(IDX)  TO  WRK-KEISAN
             COMPUTE  WRK-KEISAN2  
                             = (TBL-SANSO-SRYSURYO(IDX) -  WRK-KEISAN)
                             *   1000
      *
             PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
             IF    TBL-KASAN-SRYSURYO  =  ZERO
                MOVE    "Ｌ×１．３）" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3)
                                                                + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             ELSE
                MOVE    "Ｌ×１．３×" TO  WK-FREE-NAME3(WRK-KETA:12)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  12
      *
                MOVE     TBL-KASAN-SRYSURYO  TO  WRK-KEISAN
                COMPUTE  WRK-KEISAN2  
                             = (TBL-KASAN-SRYSURYO -  WRK-KEISAN)
                             *   1000
      *
                PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
                MOVE    "）" TO  WK-FREE-NAME3(WRK-KETA:2)
                COMPUTE  WRK-KETA   =  WRK-KETA   +  2
      *
      ***       COMPUTE  WRK-KEISAN3   =   WRK-KEISAN3  +
      ***         (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
      ***                             * TBL-KASAN-SRYSURYO)
                COMPUTE  WRK-KEISAN3   =
                  (TBL-SANSO-TEN(IDX) * TBL-SANSO-SRYSURYO(IDX) * 1.3
                                      * TBL-KASAN-SRYSURYO)     + 0.5
                COMPUTE  WRK-KEISAN4  = WRK-KEISAN4 + WRK-KEISAN3
             END-IF
      *
           END-PERFORM
      *
           IF   IDX  =  2
              MOVE    "÷１０＝"  TO  WK-FREE-NAME3(WRK-KETA:8)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  8
      *
      ***     COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***     COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
              COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
              MOVE     ZERO       TO   WRK-KEISAN2
      *
              PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
              MOVE    "点"   TO  WK-FREE-NAME3(WRK-KETA:2)
              COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           ELSE
              IF   IDX  >  2
                  MOVE   WK-FREE-NAME3  TO  WK-FREE-NAME3-TAIHI
                  MOVE    "（"    TO   WK-FREE-NAME3(1:2)
                  MOVE   WK-FREE-NAME3-TAIHI
                                  TO   WK-FREE-NAME3(3:)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                  MOVE    "）÷１０＝"  TO  WK-FREE-NAME3(WRK-KETA:10)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  10
      *
      ***         COMPUTE  WRK-KEISAN =   WRK-KEISAN3  +  0.5
      ***         COMPUTE  WRK-KEISAN =  (WRK-KEISAN   /  10)  +  0.5
                  COMPUTE  WRK-KEISAN =  (WRK-KEISAN4  /  10)  +  0.5
                  MOVE     ZERO       TO   WRK-KEISAN2
      *
                  PERFORM  2501111-SANSO-FREE-COM-NUM-SEC
      *
                  MOVE    "点"   TO  WK-FREE-NAME3(WRK-KETA:2)
                  COMPUTE  WRK-KETA   =  WRK-KETA   +  2
              END-IF
           END-IF
      *
           IF     WRK-KETA    >   1
               PERFORM   VARYING   IDX4    FROM   1   BY   50
                          UNTIL   (IDX4     >    1000)
                            OR    (WK-FREE-NAME3(IDX4:1) = SPACE)
                   ADD     1      TO    IDZ
                   MOVE    ZERO   TO    WRK-KETA2
                   PERFORM   VARYING   IDX5    FROM   1   BY   2
                              UNTIL   (IDX5     >    50)
                      IF   WK-FREE-NAME3(IDX5:2)    = SPACE
                          MOVE   IDX5   TO   WRK-KETA2
                          MOVE   50     TO   IDX5
                      END-IF
                   END-PERFORM
                   IF   WRK-KETA2 =  ZERO
                        MOVE   25   TO  WK-FREE-YUKOKETA2(IDZ)
                   ELSE
                        COMPUTE   WK-FREE-YUKOKETA2(IDZ)  =
                                       ( WRK-KETA2 - 1 )  /  2
                   END-IF
                   MOVE   WK-FREE-NAME3(IDX4:50)
                                   TO    WK-FREE-NAME2(IDZ)
               END-PERFORM
           END-IF
      *
           .
       250111-SANSO-FREE-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素計算式・数値編集
      *****************************************************************
       2501111-SANSO-FREE-COM-NUM-SEC          SECTION.          
      *
           IF    WRK-KEISAN  NOT  =  ZERO
               PERFORM   VARYING   IDX4    FROM   1   BY   1
                          UNTIL   (IDX4               >    9)
                     IF   WRK-KEISAN(IDX4:1) =   "0"
                        CONTINUE
                     ELSE
                        MOVE  IDX4   TO  IDX5
                        MOVE   9     TO  IDX4
                     END-IF
               END-PERFORM
               MOVE  SPACE             TO  WRK-KEISAN-TAIHI
               MOVE  WRK-KEISAN(IDX5:) TO  WRK-KEISAN-TAIHI
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
               CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   ZERO
                   MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                               TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
               END-IF
           END-IF
      *
           IF    WRK-KEISAN2  NOT  =  ZERO
                PERFORM   VARYING   IDX4    FROM   3   BY  -1
                           UNTIL   (IDX4               <    1)
                      IF   WRK-KEISAN2(IDX4:1) =   "0"
                         CONTINUE
                      ELSE
                         MOVE  IDX4   TO  IDX5
                         MOVE   1     TO  IDX4
                      END-IF
                END-PERFORM
                MOVE  SPACE               TO  WRK-KEISAN-TAIHI
                MOVE  WRK-KEISAN2(1:IDX5) TO  WRK-KEISAN-TAIHI
      *
                IF    WRK-KEISAN  NOT  =  ZERO
                   MOVE    "．"     TO  WK-FREE-NAME3(WRK-KETA:2)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  2
                ELSE
                   MOVE    "０．"   TO  WK-FREE-NAME3(WRK-KETA:4)
                   COMPUTE  WRK-KETA   =  WRK-KETA   +  4
                END-IF
      *
                INITIALIZE                  ORCSKANACHKAREA
                MOVE    "2"             TO  KANACHK-SYORI
                MOVE  WRK-KEISAN-TAIHI  TO  KANACHK-MAE-INPUT
                CALL  "ORCSKANACHK"   USING   ORCSKANACHKAREA
                IF      KANACHK-RC      =   ZERO
                    MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                TO  WK-FREE-NAME3(WRK-KETA:KANACHK-MAX)
                    COMPUTE  WRK-KETA   =  WRK-KETA   +  KANACHK-MAX
                END-IF
           END-IF
      *
           IF    WRK-KEISAN    =  ZERO
           AND   WRK-KEISAN2   =  ZERO
               MOVE    "０"   TO  WK-FREE-NAME3(WRK-KETA:2)
               COMPUTE  WRK-KETA   =  WRK-KETA   +  2
           END-IF
      *
           .
       2501111-SANSO-FREE-COM-NUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ４５９　退避処理
      *****************************************************************
       250111-RECE459-TAIHI-SEC     SECTION.
      *
           PERFORM 800-RECE449-SET459-SEC
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
                 KEY IS    >=   RECE459-KEY
                 INVALID
                     MOVE    1          TO  FLG-RECE459
                 NOT INVALID
                     PERFORM 990-RECE459-NEXT-SEC
                     MOVE  RECE459-REC  TO  WRK-RECE45-REC
           END-START
      *
           .
       250111-RECE459-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ４５９　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       250111-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE459           =   1                    )
             OR     ( RECE449-HOSPNUM   NOT =   RECE459-HOSPNUM      )
             OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM        )
             OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN    )
             OR     ( RECE449-PTID      NOT =   RECE459-PTID         )
             OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA       )
             OR     ( RECE449-HKNID     NOT =   RECE459-HKNID        )
             OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD     )
             OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   5     )
                       OR      ( RECE459-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE459-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
      *
                  IF   RECE459-SRYCD(IDX) = "770020070" OR "770030070"
                       MOVE   IDX-RECE45   TO   WRK-JYUNBAN
                  END-IF
      *
             END-PERFORM 
      *
             PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
           IF     WRK-GYOSUU   NOT =  ZERO
      *
      *      補正率又は高気圧酸素加算以降の明細をずらす
             IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE45   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  WRK-GYOSUU
                   MOVE   WRK-RECE45XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE45XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE45XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
             END-IF
      *
      *      計算式の差し込み
             IF   WRK-JYUNBAN  NOT =  ZERO
                 MOVE      WRK-JYUNBAN    TO     MAX-MEISAISU
             ELSE
                 MOVE      IDX-RECE45     TO     MAX-MEISAISU
             END-IF
             PERFORM   VARYING   IDX   FROM    1   BY  1
                        UNTIL  ( IDX                >   50    )
                         OR    ( WK-FREE-NAME2(IDX) =   SPACE )
      *
                ADD    1     TO  MAX-MEISAISU
                MOVE   "810000001"
                             TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
                MOVE   ZERO  TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
                MOVE   WK-FREE-YUKOKETA2(IDX)
                             TO  WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1)
                MOVE   WK-FREE-NAME2(IDX)
                             TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-RECEDENFLG(MAX-MEISAISU 1)
                MOVE    "1"  TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
      *
             END-PERFORM
      *
           END-IF
      *
           .
       250111-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    フイルムがあるか判定
      *****************************************************************
       2502-FILM-HANTEI-SEC          SECTION.
      *
           IF     RECE449-FLG-FILM-COMMENT   =    "1"
               INITIALIZE                WRK-JIDO-SETCOM-AREA
               MOVE      "1"      TO     WRK-JIDO-SETCOMFLG
               MOVE       1       TO     WRK-JIDO-SETCOMATFLG
               MOVE       99      TO     FLG-COMMENT
               PERFORM    25021-FILM-COMMENT-SEC
               PERFORM    2509-SRYACTINF2-KYOTU-HEN-SEC
           END-IF
      *
           .
       2502-FILM-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    フイルムのコメント編集（差込：下）
      *****************************************************************
       25021-FILM-COMMENT-SEC          SECTION.
      *
           PERFORM  250111-RECE459-TAIHI-SEC
      *
      *    ＲＥＣＥ４５９ファイルの内容をテーブルに退避
           MOVE    ZERO       TO     IDX-RECE45
           MOVE    SPACE      TO     WRK-RECE45XX-REC
           INITIALIZE                WRK-RECE45XX-REC
           MOVE    ZERO       TO     WRK-JYUNBAN
      *
           PERFORM  250211-SRYACTINF2-HENSYU-SEC
      *
           .
       25021-FILM-COMMENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ４５９　ＳＲＹＡＣＴＩＮＦ編集処理
      *****************************************************************
       250211-SRYACTINF2-HENSYU-SEC     SECTION.
      *
           PERFORM
             UNTIL  ( FLG-RECE459           =   1                    )
             OR     ( RECE449-HOSPNUM   NOT =   RECE459-HOSPNUM      )
             OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM        )
             OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN    )
             OR     ( RECE449-PTID      NOT =   RECE459-PTID         )
             OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA       )
             OR     ( RECE449-HKNID     NOT =   RECE459-HKNID        )
             OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD     )
             OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE459-SRYCD(IDX) =   SPACE )
                    ADD    1             TO      IDX-RECE45
                    MOVE   RECE459-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
      *
                    IF   RECE459-SRYCD(IDX) =
                         "700010000" OR "700030000" OR "700050000" OR
                         "700080000" OR "700110000" OR "700140000" OR
                         "700290000" OR "700310000" OR "700330000" OR
                         "700710000" OR "700730000" OR "700750000" OR
                         "700780000" OR "700810000" OR "700840000" OR
                         "700870000" OR "700160000" OR "700180000" OR
                         "700190000" OR "700200000" OR "700210000" OR
                         "700220000" OR "700230000" OR "700240000" OR
                         "700250000" OR "700260000" OR "700270000" OR
                         "700280000" OR "700590000" OR "700600000" OR
                         "700630000" OR "700640000" OR "700650000" OR
                         "700680000" OR "729780000"
                         MOVE   IDX-RECE45   TO   WRK-JYUNBAN
                    END-IF
      *
               END-PERFORM
      *
               PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
      *    フィルム以降の明細をずらす
           IF   WRK-JYUNBAN  NOT =  ZERO
      *
               PERFORM   VARYING   IDX   FROM   IDX-RECE45   BY  -1
                          UNTIL  ( IDX    <    1           )
                           OR    ( IDX    <=   WRK-JYUNBAN )
      *
                   COMPUTE   WRK-GYOSUU2  =  IDX  +  1
                   MOVE   WRK-RECE45XX-SRYACT-TBL(IDX 1)  TO
                          WRK-RECE45XX-SRYACT-TBL(WRK-GYOSUU2 1)
                   INITIALIZE   WRK-RECE45XX-SRYACT-TBL(IDX 1)
      *
               END-PERFORM
      *
      *        分画数の差し込み
               MOVE   WRK-JYUNBAN     TO  MAX-MEISAISU
      *
               ADD    1               TO  MAX-MEISAISU
               MOVE  "810000001"
                              TO  WRK-RECE45XX-SRYCD(MAX-MEISAISU 1)
               MOVE   ZERO    TO  WRK-RECE45XX-SRYSURYO(MAX-MEISAISU 1)
      *
               MOVE   WRK-RECE45XX-SRYKAISU(WRK-JYUNBAN 1)
                                      TO  WRK-SRYKAISU-X
      *        全角変換
               INITIALIZE                     WRK-CHAR-HENKAN-AREA
               MOVE     WRK-SRYKAISU-X  TO    WRK-CHAR-X
               PERFORM  2502111-NUM-ZENKAKU-HENKAN-SEC
               STRING   WRK-CHAR-N      DELIMITED  BY  SPACE
                        "分画"          DELIMITED  BY  SIZE
                                        INTO  WRK-BUNKAKU-N
               END-STRING
               COMPUTE WRK-BUNKAKUKETA   =   WRK-CHAR-N-CNT  +  4
      *
               COMPUTE WRK-RECE45XX-YUKOKETA(MAX-MEISAISU 1) =
                       WRK-BUNKAKUKETA   /   2
               MOVE    WRK-BUNKAKU-N
                           TO  WRK-RECE45XX-NAME(MAX-MEISAISU 1)
               MOVE    "1" TO  WRK-RECE45XX-RECEPTFLG(MAX-MEISAISU 1)
               MOVE    "1" TO  WRK-RECE45XX-SIKFLG(MAX-MEISAISU 1)
      *
               IF    WRK-JIDO-SETCOMATFLG  NOT = ZERO
                 MOVE  "2" TO  WRK-RECE45XX-MEISKYFLG(MAX-MEISAISU 1)
                 MOVE  WRK-JIDO-SETCOMATFLG
                           TO  WRK-RECE45XX-SETCOMATFLG(MAX-MEISAISU 1)
                 MOVE  WRK-JIDO-RECEDENFLG
                           TO  WRK-RECE45XX-RECEDENFLG(MAX-MEISAISU 1)
               END-IF
      *
           END-IF
      *
           .
       250211-SRYACTINF2-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値半角→全角変換処理
      *****************************************************************
       2502111-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE     1       TO   WRK-CHAR-IDX
           MOVE    ZERO     TO   WRK-CHAR-N-CNT
      *
           PERFORM   VARYING   IDX18   FROM    1   BY  1
                      UNTIL    IDX18     >     3
              IF  WRK-CHAR-X(IDX18:1)  IS   NUMERIC
                 IF   WRK-CHAR-X(IDX18:1) = "0"
                   MOVE   "０"      TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                 ELSE
                   MOVE   WRK-CHAR-X(IDX18:1)
                                    TO  IDX19
                   MOVE   TBL-DAY(IDX19)(3:2)
                                    TO  WRK-CHAR-N(WRK-CHAR-IDX:)
                 END-IF
                 ADD    2           TO  WRK-CHAR-IDX
                                        WRK-CHAR-N-CNT
              END-IF
           END-PERFORM
      *
           .
       2502111-NUM-ZENKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算（読み替え）挿入が必要か判定
      *****************************************************************
       2503-GAIKAN-HANTEI-SEC          SECTION.
      *
           IF   RECE449-FLG-GAIKAN-COMMENT  =    "1"
               INITIALIZE WRK-JIDO-SETCOM-AREA
               MOVE       99      TO     FLG-COMMENT
               PERFORM    25031-GAIKAN-INS-HENSYU-SEC
               PERFORM    25032-GAIKAN-INS-HENSYU2-SEC
               PERFORM    2509-SRYACTINF2-KYOTU-HEN-SEC
           END-IF
      *
           .
       2503-GAIKAN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算（読み替え）挿入編集
      *****************************************************************
       25031-GAIKAN-INS-HENSYU-SEC          SECTION.
      *
           PERFORM  250111-RECE459-TAIHI-SEC
      *
      *    ＲＥＣＥ４５９ファイルの内容をテーブルに退避
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
      *
           PERFORM
             UNTIL  ( FLG-RECE459           =   1                    )
             OR     ( RECE449-HOSPNUM   NOT =   RECE459-HOSPNUM      )
             OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM        )
             OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN    )
             OR     ( RECE449-PTID      NOT =   RECE459-PTID         )
             OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA       )
             OR     ( RECE449-HKNID     NOT =   RECE459-HKNID        )
             OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD     )
             OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM       )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX                >   5     )
                       OR      ( RECE459-SRYCD(IDX) =   SPACE )
                  ADD    1             TO      IDX-RECE45
                  MOVE   RECE459-SRYACT-TBL(IDX)
                             TO   WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
             END-PERFORM
      *
             PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25031-GAIKAN-INS-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算（読み替え）挿入編集２
      *****************************************************************
       25032-GAIKAN-INS-HENSYU2-SEC     SECTION.
      *
           MOVE    ZERO      TO   FLG-GAIKAN
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL  ( IDX                       >  100  )
                       OR    ( WRK-RECE45XX-SRYCD(IDX 1) = SPACE )
             IF     FLG-GAIKAN  =  1
                IF  (WRK-RECE45XX-MEISKYFLG(IDX 1)  NOT = "R")
                AND (WRK-RECE45XX-DATAKBN(IDX 1)        =  2 )
                AND (WRK-RECE45XX-KOKUJISKBKBN1(IDX 1) <=  7 )
                  COMPUTE   IDX21 = IDX - 1
                  MOVE  SPACE  TO  WRK-RECE45XX-MEISKYFLG(IDX21 1)
                  MOVE  "R"    TO  WRK-RECE45XX-MEISKYFLG(IDX   1)
                END-IF
             END-IF
             IF     WRK-RECE45XX-MEISKYFLG(IDX 1)  =  "R"
                MOVE    1      TO  FLG-GAIKAN
             ELSE
                MOVE    ZERO   TO  FLG-GAIKAN
             END-IF
           END-PERFORM
      *
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45YY-REC
           INITIALIZE                       WRK-RECE45YY-REC
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL  ( IDX                       >  100  )
                       OR    ( WRK-RECE45XX-SRYCD(IDX 1) = SPACE )
             ADD     1      TO  IDX-RECE45
             MOVE    WRK-RECE45XX-SRYACT-TBL(IDX 1)
                            TO  WRK-RECE45YY-SRYACT-TBL(IDX-RECE45 1)
             IF      WRK-RECE45XX-MEISKYFLG(IDX 1)  =  "R"
                ADD     1   TO  IDX-RECE45
                EVALUATE  WRK-RECE45-SRYKBN
                  WHEN       "40"
                    MOVE "101400040" TO WRK-RECE45YY-SRYCD(IDX-RECE45 1)
                  WHEN       "54"
                    MOVE "101500210" TO WRK-RECE45YY-SRYCD(IDX-RECE45 1)
                  WHEN       "60"
                    MOVE "101600150" TO WRK-RECE45YY-SRYCD(IDX-RECE45 1)
                  WHEN       "80"
                    MOVE "101800380" TO WRK-RECE45YY-SRYCD(IDX-RECE45 1)
                END-EVALUATE
                MOVE  WRK-RECE45YY-SRYCD(IDX-RECE45 1)
                                     TO WRK-TENSU-SRYCD
                MOVE  WRK-RECE45-SRYYM
                                     TO WRK-TENSU-SRYYM
                PERFORM   900-TENSU-TBL-SEC
                PERFORM   250321-GAIKAN-TBL-HENSYU-SEC
             END-IF
           END-PERFORM
      *
           MOVE    ZERO                 TO  IDX-RECE45
           MOVE    SPACE                TO  WRK-RECE45XX-REC
           INITIALIZE                       WRK-RECE45XX-REC
           PERFORM   VARYING   IDX   FROM    1   BY  1
                      UNTIL  ( IDX                       >  100  )
                       OR    ( WRK-RECE45YY-SRYCD(IDX 1) = SPACE )
             ADD     1      TO  IDX-RECE45
             MOVE    WRK-RECE45YY-SRYACT-TBL(IDX 1)
                            TO  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
           END-PERFORM
      *
           .
       25032-GAIKAN-INS-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来管理加算（読み替え）挿入編集２（診療行為テーブル編集）
      *****************************************************************
       250321-GAIKAN-TBL-HENSYU-SEC     SECTION.
      *
           MOVE TNS-SRYSYUKBN  TO WRK-RECE45YY-SRYSYUKBN-X(IDX-RECE45 1)
           MOVE 1              TO WRK-RECE45YY-SRYSURYO(IDX-RECE45 1)
           MOVE ZERO           TO WRK-RECE45YY-SRYKAISU(IDX-RECE45 1)
           IF   TNS-TANICD  NOT = SPACE AND "000"
             MOVE  TNS-TANICD  TO WRK-RECE45YY-TANICD(IDX-RECE45 1)
           END-IF
           IF   TNS-TANIMOJISU    = ZERO
           AND  TNS-TANINAME  NOT = SPACE
             PERFORM   VARYING   IDX21   FROM   24   BY  -1
                        UNTIL    IDX21    <      1
               IF   TNS-TANINAME(IDX21:1)  NOT =   SPACE
                 COMPUTE  TNS-TANIMOJISU   =   IDX21  /   2
                 MOVE    1     TO IDX21
               END-IF
             END-PERFORM
           END-IF
           MOVE TNS-TANIMOJISU TO WRK-RECE45YY-TANIMOJISU(IDX-RECE45 1)
           MOVE TNS-TANINAME   TO WRK-RECE45YY-TANINAME(IDX-RECE45 1)
           MOVE TNS-TENSIKIBETU
                               TO WRK-RECE45YY-TENSIKIBETU(IDX-RECE45 1)
           MOVE TNS-TEN        TO WRK-RECE45YY-TEN(IDX-RECE45 1)
           MOVE SPACE          TO WRK-RECE45YY-MEISKYFLG(IDX-RECE45 1)
           MOVE SPACE          TO WRK-RECE45YY-AUTOKBN(IDX-RECE45 1)
           MOVE ZERO           TO WRK-RECE45YY-INPUTNUM(IDX-RECE45 1)
           MOVE TNS-GAITENTTLKBN
                               TO WRK-RECE45YY-TENTTLKBN(IDX-RECE45 1)
           MOVE TNS-DATAKBN    TO WRK-RECE45YY-DATAKBN(IDX-RECE45 1)
           MOVE TNS-HOUKNSKBN  TO WRK-RECE45YY-HOUKNSKBN(IDX-RECE45 1)
           MOVE TNS-KNSJISKBN  TO WRK-RECE45YY-KNSJISKBN(IDX-RECE45 1)
           MOVE TNS-KNSJISGRPKBN
                         TO WRK-RECE45YY-KNSJISGRPKBN(IDX-RECE45 1)
           MOVE TNS-CDKBN-KBN
                         TO WRK-RECE45YY-CDKBN-KBN(IDX-RECE45 1)
           MOVE TNS-CDKBN-SYO
                         TO WRK-RECE45YY-CDKBN-SYO(IDX-RECE45 1)
           MOVE TNS-CDKBN-BU
                         TO WRK-RECE45YY-CDKBN-BU(IDX-RECE45 1)
           MOVE TNS-CDKBN-KBNNUM
                         TO WRK-RECE45YY-CDKBN-KBNNUM(IDX-RECE45 1)
           MOVE TNS-CDKBN-KBNNUM-EDA
                         TO WRK-RECE45YY-CDKBN-KBNNUM-EDA(IDX-RECE45 1)
           MOVE TNS-CDKBN-KOUBAN
                         TO WRK-RECE45YY-CDKBN-KOUBAN(IDX-RECE45 1)
           MOVE TNS-KOUHYOJYUNNUM
                         TO WRK-RECE45YY-KOUHYOJYUNNUM(IDX-RECE45 1)
           MOVE TNS-KOKUJISKBKBN1
                         TO WRK-RECE45YY-KOKUJISKBKBN1(IDX-RECE45 1)
           MOVE TNS-KOKUJISKBKBN2
                         TO WRK-RECE45YY-KOKUJISKBKBN2(IDX-RECE45 1)
           MOVE TNS-TUSOKUGAIKBN
                         TO WRK-RECE45YY-TUSOKUGAIKBN(IDX-RECE45 1)
           MOVE TNS-TIMEKSNKBN
                         TO WRK-RECE45YY-TIMEKSNKBN(IDX-RECE45 1)
           MOVE TNS-KZMCOMPSIKIBETU
                         TO WRK-RECE45YY-KZMCOMPSIKIBETU(IDX-RECE45 1)
           MOVE TNS-CHUKSNCD
                         TO WRK-RECE45YY-CHUKSNCD(IDX-RECE45 1)
           MOVE TNS-CHUKSNTUBAN
                         TO WRK-RECE45YY-CHUKSNTUBAN(IDX-RECE45 1)
           MOVE TNS-KENTAICOMMENT
                         TO WRK-RECE45YY-KENTAICOMMENT(IDX-RECE45 1)
           MOVE TNS-NAME TO WRK-RECE45YY-NAME(IDX-RECE45 1)
                            WRK-RECE45YY-INPUTCOMENT(IDX-RECE45 1)
           MOVE TNS-YUKOKETA
                         TO WRK-RECE45YY-YUKOKETA(IDX-RECE45 1)
           MOVE "1"      TO WRK-RECE45YY-SIKFLG(IDX-RECE45 1)
           MOVE TNS-TOKUKIZAISBT1
                         TO WRK-RECE45YY-TOKUKIZAISBT1(IDX-RECE45 1)
           MOVE TNS-SANSOKBN
                         TO WRK-RECE45YY-SANSOKBN(IDX-RECE45 1)
           IF   RECE41-HKNKBN  = "1" OR "2"
              CONTINUE
           ELSE
              MOVE  "1"  TO WRK-RECE45YY-RECEPTFLG(IDX-RECE45 1)
           END-IF
      *
           .
       250321-GAIKAN-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ４５９　ＳＲＹＡＣＴＩＮＦ編集処理（共通）
      *****************************************************************
       2509-SRYACTINF2-KYOTU-HEN-SEC     SECTION.
      *
      *    コメントを含んだ内容で中間ファイルの再作成
           MOVE       ZERO             TO      FLG-WRKRECE45
           MOVE       ZERO             TO      IDX
           MOVE       ZERO             TO      IDX-RECE45
           MOVE       ZERO             TO      WRK-RENNUM
           MOVE       ZERO             TO      WRK-CNT-MEISAI
           PERFORM    UNTIL     FLG-WRKRECE45  =    1
      *
                  INITIALIZE                        RECE451-REC
                  MOVE    WRK-RECE45-KEY       TO   RECE451-KEY
                  ADD     1                    TO   WRK-RENNUM
                  MOVE    WRK-RENNUM           TO   RECE451-RENNUM
                  MOVE    WRK-RECE45-PTNUM     TO   RECE451-PTNUM
                  MOVE    WRK-RECE45-SRYKA     TO   RECE451-SRYKA
                  MOVE    WRK-RECE45-SRYKBN    TO   RECE451-SRYKBN
                  MOVE    WRK-RECE45-SRYSYUKBN TO   RECE451-SRYSYUKBN
                  MOVE    WRK-RECE45-JIHIMONEY TO   RECE451-JIHIMONEY
                  PERFORM  VARYING   IDX   FROM   1   BY   1
                           UNTIL   (IDX       >    5)   OR
                                   (FLG-WRKRECE45   =   1)
                     ADD         1             TO    IDX-RECE45
                     IF (WRK-RECE45XX-SRYCD(IDX-RECE45 1) = SPACE)
                     OR (IDX-RECE45                       >  100 )
                         MOVE    1             TO    FLG-WRKRECE45
                     ELSE
                         MOVE  WRK-RECE45XX-SRYACT-TBL(IDX-RECE45 1)
                                          TO   RECE451-SRYACT-TBL(IDX)
      *
                         ADD    1    TO   WRK-CNT-MEISAI
      *
                     END-IF  
                  END-PERFORM
                  IF   RECE451-SRYCD(1)  NOT =  SPACE
                     PERFORM 800-RECE451-WRITE-SEC
                  END-IF
           END-PERFORM     
      *
           MOVE    RECE449-REC       TO      RECE441-REC
           MOVE    WRK-CNT-MEISAI    TO      RECE441-MEISAISU
           IF   WRK-JIDO-SETCOMFLG   =  "1"
             MOVE  WRK-JIDO-SETCOMFLG   TO   RECE441-SETCOMFLG
           END-IF
           PERFORM 800-RECE441-WRITE-SEC
      *
           .
       2509-SRYACTINF2-KYOTU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４９転送処理
      *****************************************************************
       2599-RECE449-TENSOU-SEC     SECTION.
      *
           MOVE       RECE449-REC    TO   RECE441-REC
           PERFORM 800-RECE441-WRITE-SEC
           PERFORM    25991-RECE459-TENSOU-SEC
      *
           .
       2599-RECE449-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４５９転送処理
      *****************************************************************
       25991-RECE459-TENSOU-SEC     SECTION.
      *
           PERFORM 800-RECE449-SET459-SEC
           MOVE    RECE449-ZAINUM       TO  RECE459-ZAINUM
           START   RECE459-FILE
               KEY IS    >=   RECE459-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE459
               NOT INVALID
                   PERFORM 990-RECE459-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE459           =   1                    )
               OR     ( RECE449-HOSPNUM   NOT =   RECE459-HOSPNUM      )
               OR     ( RECE449-SRYYM     NOT =   RECE459-SRYYM        )
               OR     ( RECE449-NYUGAIKBN NOT =   RECE459-NYUGAIKBN    )
               OR     ( RECE449-PTID      NOT =   RECE459-PTID         )
               OR     ( RECE449-RECEKA    NOT =   RECE459-RECEKA       )
               OR     ( RECE449-HKNID     NOT =   RECE459-HKNID        )
               OR     ( RECE449-TEKSTYMD  NOT =   RECE459-TEKSTYMD     )
               OR     ( RECE449-ZAINUM    NOT =   RECE459-ZAINUM       )
      *
               MOVE    RECE459-REC       TO   RECE451-REC
               PERFORM 800-RECE451-WRITE-SEC
      *
               PERFORM 990-RECE459-NEXT-SEC
      *
           END-PERFORM
      *
           .
       25991-RECE459-TENSOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集
      *****************************************************************
       270-SENTAKUCOM-HENSYU-SEC     SECTION.
      *
      *    パラメータ４４１ファイルより編集
           PERFORM 800-RECE41-SET441-SEC
      *@@@@
           MOVE    SPACE               TO  RECE441-SRYKBN
      *@@@@
           MOVE    ZERO                TO  RECE441-ZAINUM
      *
           START   RECE441-FILE
               KEY IS    >=   RECE441-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE441
               NOT INVALID
                   PERFORM 990-RECE441-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE441          =   1                 )
               OR     ( RECE41-HOSPNUM   NOT =   RECE441-HOSPNUM   )
               OR     ( RECE41-SRYYM     NOT =   RECE441-SRYYM     )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE441-NYUGAIKBN )
               OR     ( RECE41-PTID      NOT =   RECE441-PTID      )
               OR     ( RECE41-RECEKA    NOT =   RECE441-RECEKA    )
               OR     ( RECE41-HKNID     NOT =   RECE441-HKNID     )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE441-TEKSTYMD  )
      *
               IF     RECE441-SENTAKUCOMFLG  =  "1"
                  MOVE    ZERO    TO    FLG-SENTAKU-OKIKAE
                  MOVE    ZERO    TO    FLG-SENTAKU-OKIKAE2
                  PERFORM   2701-SENTAKUCOM-HENSYU2-SEC
                  IF  (FLG-SENTAKU-OKIKAE   =  1)
                  OR  (FLG-SENTAKU-OKIKAE2  =  1)
                    PERFORM 2702-SENTAKUCOM-HENSYU4-SEC
                  END-IF
               END-IF
      *
               PERFORM 990-RECE441-NEXT-SEC
      *
           END-PERFORM
      *
           .
       270-SENTAKUCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集２
      *****************************************************************
       2701-SENTAKUCOM-HENSYU2-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX45
           MOVE    SPACE               TO  WRK-RECE45XX-REC
           INITIALIZE                      WRK-RECE45XX-REC
      *
           MOVE    ZERO                TO  FLG-SENTAKU-SYUGI
           INITIALIZE                      TBL-GAZO-INF
      *
           PERFORM 800-RECE441-SET451-SEC
           MOVE    RECE441-ZAINUM      TO  RECE451-ZAINUM
           START   RECE451-FILE
               KEY IS    >=   RECE451-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE451
               NOT INVALID
                   PERFORM 990-RECE451-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE451           =   1                 )
               OR     ( RECE441-HOSPNUM   NOT =   RECE451-HOSPNUM   )
               OR     ( RECE441-SRYYM     NOT =   RECE451-SRYYM     )
               OR     ( RECE441-NYUGAIKBN NOT =   RECE451-NYUGAIKBN )
               OR     ( RECE441-PTID      NOT =   RECE451-PTID      )
               OR     ( RECE441-RECEKA    NOT =   RECE451-RECEKA    )
               OR     ( RECE441-HKNID     NOT =   RECE451-HKNID     )
               OR     ( RECE441-TEKSTYMD  NOT =   RECE451-TEKSTYMD  )
               OR     ( RECE441-ZAINUM    NOT =   RECE451-ZAINUM    )
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX                >   5  )
                          OR    (RECE451-SRYCD(IDX) = SPACE)
                 ADD    1   TO   IDX45
                 MOVE   RECE451-SRYACT-TBL(IDX)
                            TO   WRK-RECE45XX-SRYACT-TBL(IDX45 1)
      *
                 IF    (RECE441-FLG-CTMRI-COMMENT  =   SPACE)
                 AND   (RECE441-SENTAKUBUIFLG  NOT =   SPACE)
                   IF   WRK-RECE45XX-SRYCD(IDX45 1)(1:1) = "1"
                         MOVE    1    TO  FLG-SENTAKU-SYUGI
                   END-IF
                   IF   WRK-RECE45XX-SRYCD(IDX45 1)(1:3) = "002"
                     MOVE  "1"  TO  WRK-RECE45XX-RECEDENFLG(IDX45 1)
                     MOVE  "1"  TO  WRK-RECE45XX-RECEPTFLG(IDX45 1)
                     MOVE   1   TO  FLG-SENTAKU-OKIKAE2
                   END-IF
                   IF  ((WRK-RECE45XX-SRYCD(IDX45 1)(1:1) = "8"   ) AND
                        (WRK-RECE45XX-SRYCD(IDX45 1)(4:1) = "1"   ) AND
                        (WRK-RECE45XX-SRYCD(IDX45 1)(5:1) = "8"   ))
                   OR  ( WRK-RECE45XX-SRYCD(IDX45 1) = "830100797" )
                     MOVE     ZERO  TO  FLG-GAZO
                     PERFORM  VARYING  IDX50  FROM  1  BY  1
                               UNTIL  (IDX50 >   10                 )
                                OR    (IDX50 > RECE441-SENTAKUBUICNT)
                                OR    (WRK-RECE45XX-NAME(IDX50 1)
                                                             = SPACE)
                       IF   (WRK-RECE45XX-NAME(IDX45 1) =
                                       WRK-RECE45XX-NAME(IDX50 1))
                       AND  (TBL-GAZO-CHEKFLG(IDX50)    =  ZERO  )
                         MOVE   1   TO  FLG-GAZO
                         MOVE   10  TO  IDX50
                       END-IF
                     END-PERFORM
                     IF  FLG-GAZO  =  ZERO
                     AND IDX50    <=  10
                       MOVE   WRK-RECE45XX-SRYACT-TBL(IDX45 1)
                                  TO  WRK-RECE45XX-SRYACT-TBL(IDX50 1)
                       MOVE  "1"  TO  WRK-RECE45XX-RECEDENFLG(IDX50 1)
                       IF     FLG-SENTAKU-SYUGI  =  ZERO
                         MOVE 1   TO  TBL-GAZO-CHEKFLG(IDX50)
                       END-IF
                     END-IF
                     MOVE    "1"  TO  WRK-RECE45XX-RECEPTFLG(IDX45 1)
                     MOVE     1   TO  FLG-SENTAKU-OKIKAE2
                   END-IF
                 END-IF
      *
                 IF   (IDX45                               >  1        )
                 AND  (WRK-RECE45XX-SRYCD(IDX45 1)(1:1)    = "8"       )
                 AND  (WRK-RECE45XX-SRYCD(IDX45 1)(4:1)    = "1" OR "6")
                 AND  (WRK-RECE45XX-MEISKYFLG(IDX45 1) NOT = "2"       )
                   MOVE      ZERO    TO   FLG-GET
                   MOVE      ZERO    TO   FLG-ETC
                   COMPUTE   IDY12   =    IDX45  -  1
                   PERFORM  VARYING  IDY11  FROM  IDY12  BY  -1
                             UNTIL  (IDY11    <  1)
                              OR    (FLG-GET  =  1)
                     IF (WRK-RECE45XX-SRYCD(IDY11 1)(1:1) = "7"  )
                     OR (WRK-RECE45XX-SRYCD(IDY11 1)(1:3) = "059")
                       MOVE    1     TO   FLG-ETC
                       MOVE  IDY11   TO   IDY13
                     ELSE
                       IF WRK-RECE45XX-SRYCD(IDY11 1)(1:1) = "1" OR "6"
                         PERFORM 27011-SENTAKUCOM-HENSYU3-SEC
                         IF   FLG-GET = ZERO
                           MOVE    1     TO   FLG-ETC
                           MOVE  IDY11   TO   IDY13
                         END-IF
                       END-IF
                     END-IF
                   END-PERFORM
                   IF     FLG-GET  =   1
                     MOVE      1    TO   FLG-SENTAKU-OKIKAE
                     MOVE     "2"   TO   WRK-RECE45XX-MEISKYFLG(IDX45 1)
                     IF   WRK-RECE45XX-COMCDCHGFLG(IDX45 1) = "1"
                       MOVE   "810000001"
                                    TO   WRK-RECE45XX-SRYCD(IDX45 1)
                     END-IF
                     IF   FLG-ETC  =   1
                       MOVE    ZERO      TO  IDX46
                       MOVE    SPACE     TO  WRK-RECE45ZZ-REC
                       INITIALIZE            WRK-RECE45ZZ-REC
                       PERFORM  VARYING  IDY11  FROM  1  BY  1
                                 UNTIL   IDY11    >   IDY12
                         IF   IDY11 = IDY13
                           ADD   1   TO   IDX46
                           MOVE  WRK-RECE45XX-SRYACT-TBL(IDX45 1)
                                 TO   WRK-RECE45ZZ-SRYACT-TBL(IDX46 1)
                         END-IF
                         ADD     1   TO   IDX46
                         MOVE    WRK-RECE45XX-SRYACT-TBL(IDY11 1)
                                 TO   WRK-RECE45ZZ-SRYACT-TBL(IDX46 1)
                       END-PERFORM
                       PERFORM  VARYING  IDY11  FROM  1  BY  1
                                 UNTIL   IDY11    >   IDX45
                         MOVE   WRK-RECE45ZZ-SRYACT-TBL(IDY11 1)
                                 TO   WRK-RECE45XX-SRYACT-TBL(IDY11 1)
                       END-PERFORM
                     END-IF
                   ELSE
                     IF   WRK-RECE45XX-COMCDCHGFLG(IDX45 1) = "1"
                       MOVE "810000001" TO WRK-RECE45XX-SRYCD(IDX45 1)
                       MOVE  1          TO FLG-SENTAKU-OKIKAE2
                     END-IF
                   END-IF
                 ELSE
                   IF     WRK-RECE45XX-COMCDCHGFLG(IDX45 1) = "1"
                       MOVE "810000001" TO WRK-RECE45XX-SRYCD(IDX45 1)
                       MOVE  1          TO FLG-SENTAKU-OKIKAE2
                   END-IF
                 END-IF
               END-PERFORM
      *
               PERFORM 990-RECE451-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2701-SENTAKUCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集３
      *****************************************************************
       27011-SENTAKUCOM-HENSYU3-SEC     SECTION.
      *
           MOVE    WRK-RECE45XX-SRYCD(IDY11 1)  TO  WRK-RKISAICD
           PERFORM    270111-SENTAKUCOM-HENSYU3X-SEC
      *
           IF   (FLG-GET = ZERO                                 )
           AND  (WRK-RECE45XX-CYOKISYUSAICD(IDY11 1) NOT = SPACE)
             MOVE  WRK-RECE45XX-CYOKISYUSAICD(IDY11 1)
                                                TO  WRK-RKISAICD
             PERFORM  270111-SENTAKUCOM-HENSYU3X-SEC
           END-IF
      *
           IF  ( FLG-GET = ZERO                            )
           AND ( FLG-ETC = ZERO                            )
           AND ((WRK-RECE45XX-CYOKISYUSAIFLG(IDY11 1) =  1) OR
                (WRK-RECE45XX-CYOKISENTEIKBN(IDY11 1) =  1))
             MOVE  WRK-RECE45XX-SRYCD(IDX45 1)  TO  WRK-RKISAICD
             PERFORM  270112-SENTAKUCOM-HENSYU3Y-SEC
           END-IF
      *
           IF   (FLG-GET = ZERO                           )
           AND  (FLG-ETC = ZERO                           )
           AND  (WRK-RECE45XX-SIPPUFLG(IDY11 1) = "1"     )
           AND  (WRK-RECE45XX-SIPPUFLG(IDX45 1) = "2"     )
           AND  (RECE41-SRYYM                  >= "202410")
             MOVE    1    TO    FLG-GET
           END-IF
      *
           .
       27011-SENTAKUCOM-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集３
      *****************************************************************
       270111-SENTAKUCOM-HENSYU3X-SEC     SECTION.
      *
           INITIALIZE                      RKISAI-REC
           MOVE  WRK-RKISAICD          TO  RKISAI-SRYCD
           MOVE  RECE41-SRYYM          TO  RKISAI-STYM
           MOVE  RECE41-SRYYM          TO  RKISAI-EDYM
           MOVE  RKISAI-REC            TO  MCPDATA-REC
           MOVE  "tbl_recekisai"       TO  MCP-TABLE
           MOVE  "key5"                TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE  "tbl_recekisai"     TO  MCP-TABLE
             MOVE  "key5"              TO  MCP-PATHNAME
             PERFORM  900-RKISAI-READ-SEC
             PERFORM  UNTIL (FLG-RKISAI  =  1)
                       OR   (FLG-GET     =  1)
               IF   RKISAI-C-CD  =  WRK-RECE45XX-SRYCD(IDX45 1)
                 MOVE    1    TO    FLG-GET
               END-IF
               MOVE  "tbl_recekisai"   TO  MCP-TABLE
               MOVE  "key5"            TO  MCP-PATHNAME
               PERFORM  900-RKISAI-READ-SEC
             END-PERFORM
           END-IF
           MOVE  "tbl_recekisai"       TO  MCP-TABLE
           MOVE  "key5"                TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       270111-SENTAKUCOM-HENSYU3X-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集３
      *****************************************************************
       270112-SENTAKUCOM-HENSYU3Y-SEC     SECTION.
      *
           INITIALIZE                      RKISAI-REC
           MOVE  WRK-RKISAICD          TO  RKISAI-C-CD
           MOVE  RECE41-SRYYM          TO  RKISAI-STYM
           MOVE  RECE41-SRYYM          TO  RKISAI-EDYM
           MOVE  RKISAI-REC            TO  MCPDATA-REC
           MOVE  "tbl_recekisai"       TO  MCP-TABLE
           MOVE  "key7"                TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC  =  ZERO
             MOVE  "tbl_recekisai"     TO  MCP-TABLE
             MOVE  "key7"              TO  MCP-PATHNAME
             PERFORM  900-RKISAI-READ-SEC
             PERFORM  UNTIL (FLG-RKISAI  =  1)
                       OR   (FLG-GET     =  1)
               IF   RKISAI-SRYCD  =  "819999999"
                 MOVE    1    TO    FLG-GET
               END-IF
               MOVE  "tbl_recekisai"   TO  MCP-TABLE
               MOVE  "key7"            TO  MCP-PATHNAME
               PERFORM  900-RKISAI-READ-SEC
             END-PERFORM
           END-IF
           MOVE  "tbl_recekisai"       TO  MCP-TABLE
           MOVE  "key7"                TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       270112-SENTAKUCOM-HENSYU3Y-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント編集４
      *****************************************************************
       2702-SENTAKUCOM-HENSYU4-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDX45
      *
           PERFORM 800-RECE441-SET451-SEC
           MOVE    RECE441-ZAINUM      TO  RECE451-ZAINUM
           START   RECE451-FILE
               KEY IS    >=   RECE451-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE451
               NOT INVALID
                   PERFORM 990-RECE451-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE451           =   1                 )
               OR     ( RECE441-HOSPNUM   NOT =   RECE451-HOSPNUM   )
               OR     ( RECE441-SRYYM     NOT =   RECE451-SRYYM     )
               OR     ( RECE441-NYUGAIKBN NOT =   RECE451-NYUGAIKBN )
               OR     ( RECE441-PTID      NOT =   RECE451-PTID      )
               OR     ( RECE441-RECEKA    NOT =   RECE451-RECEKA    )
               OR     ( RECE441-HKNID     NOT =   RECE451-HKNID     )
               OR     ( RECE441-TEKSTYMD  NOT =   RECE451-TEKSTYMD  )
               OR     ( RECE441-ZAINUM    NOT =   RECE451-ZAINUM    )
      *
               PERFORM  VARYING  IDX  FROM  1  BY  1
                         UNTIL  (IDX                >   5  )
                          OR    (RECE451-SRYCD(IDX) = SPACE)
                 ADD    1   TO   IDX45
                 MOVE   WRK-RECE45XX-SRYACT-TBL(IDX45 1)
                            TO   RECE451-SRYACT-TBL(IDX)
               END-PERFORM
      *
               REWRITE   RECE451-REC
               IF    STS-RECE451  =  "00"
                 CONTINUE
               ELSE
                 MOVE    SPACE          TO  WRK-ERR-AREA
                 INITIALIZE                 WRK-ERR-AREA
                 MOVE    4              TO  WRK-ERR-FLG
                 MOVE    RECE451PARA    TO  WRK-ERR-FILE-NM
                 MOVE    STS-RECE451    TO  WRK-ERR-FILE-STS
                 PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
               ADD       1        TO   CNT-RECE45-RE
      *
               PERFORM 990-RECE451-NEXT-SEC
      *
           END-PERFORM
      *
           IF  FLG-SENTAKU-OKIKAE  =  1
             MOVE   "1"   TO   RECE441-SETCOMFLG
             REWRITE   RECE441-REC
             IF    STS-RECE441  =  "00"
               CONTINUE
             ELSE
               MOVE    SPACE          TO  WRK-ERR-AREA
               INITIALIZE                 WRK-ERR-AREA
               MOVE    4              TO  WRK-ERR-FLG
               MOVE    RECE441PARA    TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE441    TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
             END-IF
             ADD     1    TO   CNT-RECE44-RE
           END-IF
      *
           .
       2702-SENTAKUCOM-HENSYU4-EXT.
           EXIT.
      *
      *****************************************************************
      *    関係コメント編集
      *****************************************************************
       260-SETCOM-HENSYU-SEC     SECTION.
      *
      *    パラメータ４４１ファイルより編集
           PERFORM 800-RECE41-SET441-SEC
      *@@@@
           MOVE    SPACE               TO  RECE441-SRYKBN
      *@@@@
           MOVE    ZERO                TO  RECE441-ZAINUM
      *
           START   RECE441-FILE
               KEY IS    >=   RECE441-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE441
               NOT INVALID
                   PERFORM 990-RECE441-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE441          =   1                 )
               OR     ( RECE41-HOSPNUM   NOT =   RECE441-HOSPNUM   )
               OR     ( RECE41-SRYYM     NOT =   RECE441-SRYYM     )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE441-NYUGAIKBN )
               OR     ( RECE41-PTID      NOT =   RECE441-PTID      )
               OR     ( RECE41-RECEKA    NOT =   RECE441-RECEKA    )
               OR     ( RECE41-HKNID     NOT =   RECE441-HKNID     )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE441-TEKSTYMD  )
      *
               MOVE      ZERO      TO    FLG-SETCOM-OKIKAE
      *
               IF     RECE441-SETCOMFLG   =    "1"
                  PERFORM 2601-SETCOM-HENSYU2-SEC
                  IF   FLG-SETCOM-OKIKAE  =  "1"
                    CONTINUE
                  ELSE
                    MOVE   SPACE   TO   RECE441-SETCOMFLG
                    REWRITE   RECE441-REC
                    IF      STS-RECE441         =   "00"
                       CONTINUE
                    ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    4                   TO  WRK-ERR-FLG
                       MOVE    RECE441PARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE441         TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                    END-IF
       *
                    ADD     1      TO   CNT-RECE44-RE
                  END-IF
               END-IF
      *
               PERFORM 990-RECE441-NEXT-SEC
      *
           END-PERFORM
      *
           .
       260-SETCOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    関係コメント編集２
      *****************************************************************
       2601-SETCOM-HENSYU2-SEC     SECTION.
      *
           MOVE    ZERO    TO    FLG-SETCOM
           MOVE    ZERO    TO    FLG-SETCOM2
      *
           MOVE    ZERO    TO    WRK-HEN-RENNUM-X
                                 WRK-HEN-SHORI-RENNUM-X
                                 WRK-HEN-SEQNO-X
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J
                                 WRK-HEN-SHORI-RENNUM-J
                                 WRK-HEN-SEQNO-J
           MOVE    ZERO    TO    WRK-HEN-RENNUM-J2
                                 WRK-HEN-SHORI-RENNUM-J2
                                 WRK-HEN-SEQNO-J2
      *
           PERFORM 800-RECE441-SET451-SEC
           MOVE    RECE441-ZAINUM      TO  RECE451-ZAINUM
           START   RECE451-FILE
               KEY IS    >=   RECE451-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE451
               NOT INVALID
                   PERFORM 990-RECE451-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE451           =   1                 )
               OR     ( RECE441-HOSPNUM   NOT =   RECE451-HOSPNUM   )
               OR     ( RECE441-SRYYM     NOT =   RECE451-SRYYM     )
               OR     ( RECE441-NYUGAIKBN NOT =   RECE451-NYUGAIKBN )
               OR     ( RECE441-PTID      NOT =   RECE451-PTID      )
               OR     ( RECE441-RECEKA    NOT =   RECE451-RECEKA    )
               OR     ( RECE441-HKNID     NOT =   RECE451-HKNID     )
               OR     ( RECE441-TEKSTYMD  NOT =   RECE451-TEKSTYMD  )
               OR     ( RECE441-ZAINUM    NOT =   RECE451-ZAINUM    )
      *
               MOVE      ZERO      TO    FLG-MEISKY-OKIKAE
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX                >   5     )
                         OR      ( RECE451-SRYCD(IDX) =   SPACE )
                IF    RECE451-RECEDENFLG(IDX) = SPACE
                 IF   RECE451-MEISKYFLG(IDX)  =  "2"
                   IF      RECE451-SRYYM  >=  "201307"
                    IF     RECE451-SETCOMATFLG(IDX) = ZERO
                      IF   (FLG-SETCOM              =  1   )
      ***             AND  (RECE451-RECEDENFLG(IDX) = SPACE)
                        ADD    1                 TO  WRK-HEN-SEQNO-X
                        IF   WRK-HEN-SEQNO-X  >  3
                        AND  FLG-SETCOM2      =  ZERO
                          MOVE   SPACE   TO  RECE451-MEISKYFLG(IDX)
                          MOVE   1       TO  FLG-MEISKY-OKIKAE
                        ELSE
                          MOVE   WRK-HEN-RENNUM-X  TO  WRK-HEN-RENNUM
                          MOVE   WRK-HEN-SHORI-RENNUM-X
                                         TO  WRK-HEN-SHORI-RENNUM
                          MOVE   WRK-HEN-SEQNO-X   TO  WRK-HEN-SEQNO
                          PERFORM  26011-SETCOM-HENSYU3-SEC
                        END-IF
                        IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J )
                        AND (WRK-HEN-SHORI-RENNUM-X
                                        = WRK-HEN-SHORI-RENNUM-J )
                          ADD  1                 TO  WRK-HEN-SEQNO-J
                        END-IF
                        IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J2)
                        AND (WRK-HEN-SHORI-RENNUM-X
                                        = WRK-HEN-SHORI-RENNUM-J2)
                          ADD  1                 TO  WRK-HEN-SEQNO-J2
                        END-IF
                      END-IF
                    ELSE
      ***==           ２０２０年４月以降に追加となった選択式コメントの
      ***==           対応は不要
      ***             IF   RECE451-RECEDENFLG(IDX) = SPACE
                       EVALUATE   RECE451-SETCOMATFLG(IDX)
                        WHEN    1
                          ADD  1                 TO  WRK-HEN-SEQNO-J
                          IF   WRK-HEN-SEQNO-J  >  3
                          AND  FLG-SETCOM2      =  ZERO
                            MOVE   SPACE   TO  RECE451-MEISKYFLG(IDX)
                            MOVE   1       TO  FLG-MEISKY-OKIKAE
                          ELSE
                            MOVE WRK-HEN-RENNUM-J  TO  WRK-HEN-RENNUM
                            MOVE WRK-HEN-SHORI-RENNUM-J
                                         TO  WRK-HEN-SHORI-RENNUM
                            MOVE WRK-HEN-SEQNO-J   TO  WRK-HEN-SEQNO
                            PERFORM  26011-SETCOM-HENSYU3-SEC
                          END-IF
                          IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J )
                          AND (WRK-HEN-SHORI-RENNUM-X
                                          = WRK-HEN-SHORI-RENNUM-J )
                            ADD    1             TO  WRK-HEN-SEQNO-X
                          END-IF
      *                 同一剤にコメント自動記載をする診療コードが２つ
      *                 あり、２つ目の自動記載分
                        WHEN    2
                          ADD  1                 TO  WRK-HEN-SEQNO-J2
                          IF   WRK-HEN-SEQNO-J2 >  3
                          AND  FLG-SETCOM2      =  ZERO
                            MOVE   SPACE   TO  RECE451-MEISKYFLG(IDX)
                            MOVE   1       TO  FLG-MEISKY-OKIKAE
                          ELSE
                            MOVE WRK-HEN-RENNUM-J2 TO  WRK-HEN-RENNUM
                            MOVE WRK-HEN-SHORI-RENNUM-J2
                                         TO  WRK-HEN-SHORI-RENNUM
                            MOVE WRK-HEN-SEQNO-J2  TO  WRK-HEN-SEQNO
                            PERFORM  26011-SETCOM-HENSYU3-SEC
                          END-IF
                          IF  (WRK-HEN-RENNUM-X = WRK-HEN-RENNUM-J2)
                          AND (WRK-HEN-SHORI-RENNUM-X
                                          = WRK-HEN-SHORI-RENNUM-J2)
                            ADD    1             TO  WRK-HEN-SEQNO-X
                          END-IF
                       END-EVALUATE
      ***             END-IF
      ***==
                    END-IF
                   ELSE
                    MOVE   SPACE   TO  RECE451-MEISKYFLG(IDX)
                    MOVE   1       TO  FLG-MEISKY-OKIKAE
                   END-IF
                 ELSE
                   IF      RECE451-SRYYM  >=  "201307"
                    IF   (RECE451-SRYCD(IDX)(1:1) = "1" OR "6" OR "7")
                    OR   (RECE451-SRYCD(IDX)(1:3) = "059"            )
                     IF   RECE451-SRYCD(IDX) =
                          "101400040" OR "101500210" OR "101600150" OR
                          "101800380"
                      CONTINUE
                     ELSE
                      MOVE   1               TO  FLG-SETCOM
                      MOVE   RECE451-RENNUM  TO  WRK-HEN-RENNUM-X
                      MOVE   IDX             TO  WRK-HEN-SHORI-RENNUM-X
                      MOVE   ZERO            TO  WRK-HEN-SEQNO-X
                      IF  RECE451-SRYCD(IDX)(1:1) = "6" OR "7"
                        MOVE   1             TO  FLG-SETCOM2
                      ELSE
                        MOVE   ZERO          TO  FLG-SETCOM2
                      END-IF
                     END-IF
                    END-IF
      *
      ***==         ２０２０年４月以降に追加となった選択式コメントの
      ***==         対応は不要
                    IF   (RECE451-SRYCD(IDX) =
      *#1
                          "113000410" OR "113000510" OR "180000110" OR
                          "180000210" OR "113023910" OR
      *#3
                          "113001210" OR "113001310" OR "113002110" OR
      *#4
                          "111011810" OR "111012610" OR "111012810" OR
      *#5
                          "113006510" OR
      *#6
                          "180000710" OR
      *#7
                          "113004310" OR
      *#8
                          "113008310" OR "113008410" OR "113008510" OR
                          "113031610" OR "113031710" OR
      *#9
                          "113008070" OR "113008170" OR
      *#10
                          "113000810" OR "113009910" OR "113029410" OR
      *#11
                          "114008010" OR
      *#12
                          "114009410" OR
      *#13
                          "114000110" OR "114001610" OR
      *#14
                          "114001110" OR "114012910" OR "114018010" OR
                          "114030310" OR "114042110" OR "114042210" OR
                          "114042810" OR "114046310" OR
      *#16
                          "180027410" OR "180027510" OR "180028010" OR
                          "180028110" OR "180027610" OR "180027710" OR
                          "180030810" OR "180027810" OR "180027910" OR
                          "180028210" OR "180028310" OR "180028410" OR
                          "180032410" OR "180032510" OR "180032610" OR
                          "180032710" OR "180033110" OR
                          "180033910" OR "180034010" OR "180034110" OR
                          "180034210" OR "180034310" OR "180034410" OR
                          "180034510" OR "180034610" OR "180034710" OR
                          "180044610" OR "180044710" OR "180044810" OR
                          "180045010" OR "180045210" OR "180045410" OR
                          "180045530" OR "180045630" OR "180045730" OR
                          "180043430" OR "180043530" OR "180043630" OR
                          "180043730" OR "180043830" OR "180043930" OR
                          "180044030" OR "180044130" OR "180044230" OR
                          "180050330" OR "180050430" OR "180050530" OR
                          "180050730" OR "180050930" OR "180051130" OR
                          "180051230" OR "180051330" OR "180051430" OR
                          "180052730" OR "180052830" OR "180052930" OR
                          "180053130" OR "180053330" OR "180053530" OR
                          "180053630" OR "180053730" OR "180053830" OR
                          "180051530" OR "180051630" OR "180051730" OR
                          "180051930" OR "180052130" OR "180052330" OR
                          "180052430" OR "180052530" OR "180052630" OR
      *R6.6
                          "180744210" OR "180744310" OR "180744410" OR
                          "180744510" OR "180744610" OR "180744710" OR
                          "180744810" OR "180744910" OR "180745010" OR
                          "180745110" OR "180745310" OR "180745410" OR
                          "180745510" OR "180745610" OR "180745710" OR
                          "180745810" OR "180745910" OR "180746010" OR
                          "180746110" OR "180746210" OR "180746310" OR
                          "180746410" OR "180746510" OR "180746610" OR
                          "180746710" OR "180746810" OR "180746910" OR
                          "180747010" OR "180747110" OR "180747210" OR
                          "180747310" OR "180747410" OR "180747510" OR
                          "180747610" OR "180747710" OR "180747810" OR
                          "180747930" OR "180748030" OR "180748130" OR
                          "180748230" OR "180748330" OR "180748430" OR
                          "180748530" OR "180748630" OR "180748730" OR
                          "180748830" OR "180748930" OR "180749030" OR
                          "180749130" OR "180749230" OR "180749330" OR
                          "180749430" OR "180749530" OR "180749630" OR
                          "180750030" OR "180749730" OR "180750130" OR
                          "180749830" OR "180750230" OR "180749930" OR
                          "180750330" OR "180750430" OR "180750510" OR
                          "180750610" OR "180750710" OR "180750810" OR
                          "180750910" OR "180751010" OR "180751110" OR
                          "180751210" OR "180751310" OR "180751410" OR
                          "180751510" OR "180751610" OR "180751710" OR
                          "180751810" OR "180751910" OR "180752010" OR
                          "180752110" OR "180752210" OR "180752310" OR
                          "180752410" OR "180752510" OR "180752610" OR
                          "180752710" OR "180752810" OR "180752910" OR
                          "180753010" OR "180753130" OR "180753230" OR
                          "180753330" OR "180753430" OR "180753530" OR
                          "180753630" OR "180753730" OR "180753830" OR
                          "180754330" OR "180753930" OR "180754030" OR
                          "180754130" OR "180754230" OR "180754430" OR
                          "180754530" OR "180754630" OR "180754730" OR
                          "180754830" OR "180754930" OR "180755030" OR
                          "180755130" OR "180755230" OR "180755330" OR
                          "180755430" OR "180755530" OR "180755630" OR
                          "180755710" OR "180755810" OR "180755910" OR
                          "180756010" OR "180756110" OR "180756210" OR
                          "180756310" OR "180756410" OR "180756510" OR
                          "180756610" OR "180757710" OR "180757810" OR
                          "180757910" OR "180758010" OR "180758110" OR
                          "180758210" OR "180758310" OR "180758410" OR
                          "180758510" OR "180758610" OR "180756730" OR
                          "180756830" OR "180756930" OR "180757030" OR
                          "180757130" OR "180757230" OR "180757330" OR
                          "180757430" OR "180757530" OR "180757630" OR
                          "180758730" OR "180758830" OR "180758930" OR
                          "180759030" OR "180759130" OR "180759230" OR
                          "180759330" OR "180759430" OR "180759530" OR
                          "180759630" OR "180759710" OR "180759810" OR
                          "180759910" OR "180760010" OR "180760110" OR
                          "180760210" OR "180760310" OR "180760410" OR
      *#17
                          "113001510" OR
      *#18
                          "113003410" OR
      *#19
                          "180033210" OR "180035910" OR "180047810" OR
                          "180056410" OR
                          "180042110" OR "180042410" OR "180048710" OR
                          "180048810" OR "180057210" OR "180057310" OR
                          "180057410" OR "180057510" OR "180057610" OR
                          "180057710" OR
                          "180057810" OR "180057910" OR
                          "180058010" OR "180058110" OR
                          "180064010" OR "180064110" OR
                          "114045510" OR
                          "188007010" OR "188010610" OR
      *#20
                          "700010000" OR "700030000" OR "700050000" OR
                          "700080000" OR "700110000" OR "700140000" OR
                          "700290000" OR "700310000" OR "700330000" OR
                          "700710000" OR "700730000" OR "700750000" OR
                          "700780000" OR "700810000" OR "700840000" OR
                          "700870000" OR "700160000" OR "700180000" OR
                          "700190000" OR "700200000" OR "700210000" OR
                          "700220000" OR "700230000" OR "700240000" OR
                          "700250000" OR "700260000" OR "700270000" OR
                          "700280000" OR "700590000" OR "700600000" OR
                          "700630000" OR "700640000" OR "700650000" OR
                          "700680000" OR "729780000" OR
      *#21
                          "113014610" OR "113024310" OR "113027910" OR
                          "113028310" OR
      *#22
                          "113014370" OR
      *#23
                          "113013910" OR "113014010" OR "113014110" OR
      *#24
                          "114020910" OR
      *#25
                          "112015810" OR "112015950" OR "112016210" OR
                          "112016410" OR "112017010" OR "112017150" OR
                          "112017610" OR "112023550" OR "112023850" OR
      *#26
                          "114009710" OR
      *#27
                          "113015010" OR "113015130" OR "113018070" OR
                          "114045470" OR "114045670" OR "113030470" OR
                          "113030570" OR
      *#29
                          "114027610" OR "114027710" OR "114027810" OR
                          "114027910" OR
      *#32
                          "114006410" OR "114015010" OR
      *#33
                          "180047910" OR "180058710" OR
                          "188010710" OR "188011810" OR
      *#35
                          "140057510" OR "140057610" OR
      *#36
                          "114044710" OR "114044810" OR "114044910" OR
      *#37
                          "113024410" OR "113024510" OR
      *#38
                          "112023210" OR "113023890" OR "114043970" OR
      *#41
                          "140007710" OR "140008510" OR "140029850" OR
                          "140008210" OR "140039210" OR "140057810" OR
                          "140057910" OR "140058010" OR "140058110" OR
                          "140058210" OR "140058310" OR "140058410" OR
                          "140058510" OR "140058610" OR "140059310" OR
                          "140059410" OR "140059510" OR
                          "140060210" OR "140060310" OR "140060410" OR
                          "140060510" OR "140060610" OR "140060710" OR
                          "140060810" OR "140060910" OR "140061010" OR
      *#43
                          "160000210" OR
      *#44
                          "170016010" OR
      *#45
                          "113030670" OR "113030770" OR
      *#46
                          "113032810" OR
      *#47
                          "180058650"                              )
      *
      *<労災用>
      *#16
                    OR   (RECE451-SRYCD(IDX) =
                          "101800230" OR "101800240" OR "101800250" OR
                          "101800260" OR "101800270" OR "101800280" OR
                          "101800290" OR "101800330" OR "101800340" OR
                          "101800350" OR
                          "101801110" OR "101801120" OR "101801130" OR
                          "101801070" OR "101801080" OR
      *R6.6
                          "101801600" OR "101801700" OR "101801800" OR
                          "101801900" OR "101802000" OR "101802100" OR
                          "101802200" OR "101802300" OR "101802400" OR
                          "101802500" OR "101802600" OR "101802700" OR
                          "101802800" OR "101802900" OR "101803000" OR
                          "101803100" OR "101803200" OR "101803300" OR
                          "101803400" OR "101803500" OR "101803600" OR
                          "101803700" OR "101803800" OR "101803900" OR
                          "101804000" OR "101804100" OR "101804200" OR
                          "101804300" OR "101804400" OR "101804500" OR
                          "101804600" OR "101804700" OR "101804800" OR
                          "101804900" OR "101805000" OR "101805100" OR
                          "101805200" OR "101805300" OR "101805400" OR
                          "101805500" OR "101805600" OR "101805700" OR
                          "101805800" OR "101805900" OR "101806000" OR
                          "101806100" OR "101806200" OR "101806300" OR
                          "101806400" OR "101806500" OR "101806600" OR
                          "101806700" OR "101806800" OR "101806900")
      *#4
                    OR   (RECE451-SRYCD(IDX) = "101110040"
                                            OR "101110060"         )
      *#25
                    OR   (RECE451-SRYCD(IDX) = "101120040"
                                            OR "101120080"         )
      *#301
                    OR   (RECE451-SRYCD(IDX) = "101800890"         )
      *
                    OR   (RECE451-SRYCD(IDX)(1:5)  =  "09593"      )
      *
                       MOVE   RECE451-RENNUM  TO  WRK-HEN-RENNUM-J
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J
                    END-IF
      *
                    IF    RECE451-SRYCD(IDX) =
      *#2
                          "113000670" OR "113000770" OR "113029170" OR
                          "113029270" OR "113029370" OR
      *#39
                          "180058270" OR
      *#40
                          "180056570" OR
                          "188011270" OR
      *#42
                          "140054270" OR "140054370" OR "140054470" OR
      *###
                          "114008370" OR "114054050" OR "114058050"
      *
                       MOVE   RECE451-RENNUM  TO  WRK-HEN-RENNUM-J2
                       MOVE   IDX       TO  WRK-HEN-SHORI-RENNUM-J2
                       MOVE   ZERO            TO  WRK-HEN-SEQNO-J2
                    END-IF
      ***==
                   END-IF
                 END-IF
                END-IF
               END-PERFORM
      *
               IF    FLG-MEISKY-OKIKAE  =  1
                  REWRITE   RECE451-REC
                  IF      STS-RECE451        =   "00"
                       CONTINUE
                  ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    4                   TO  WRK-ERR-FLG
                       MOVE    RECE451PARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE451         TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                  END-IF
      *
                  ADD       1        TO   CNT-RECE45-RE
               END-IF
      *
               PERFORM 990-RECE451-NEXT-SEC
      *
           END-PERFORM
      *
           .
       2601-SETCOM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    関係コメント編集３
      *****************************************************************
       26011-SETCOM-HENSYU3-SEC     SECTION.
      *
           MOVE    SPACE                 TO  RECE455-REC
           INITIALIZE                        RECE455-REC
      *
           MOVE    RECE451-KEY           TO  RECE455-KEY
      *
           MOVE    WRK-HEN-RENNUM        TO  RECE455-RENNUM
           MOVE    WRK-HEN-SHORI-RENNUM  TO  RECE455-SHORI-RENNUM
           MOVE    WRK-HEN-SEQNO         TO  RECE455-SEQNO
      *
           MOVE    RECE451-SRYCD(IDX)    TO  RECE455-SRYCD
      *    後ろの全角空白を削除する
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "1"                 TO  KANACHK-SYORIKBN
           MOVE    RECE451-NAME(IDX)   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-MAX         >   ZERO )
               MOVE    SPACE           TO  RECE451-NAME(IDX) 
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                       TO  RECE451-NAME(IDX) 
           END-IF
           MOVE    RECE451-NAME(IDX)     TO  RECE455-NAME
      *    後ろの全角空白を削除する
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "1"                 TO  KANACHK-SYORIKBN
           MOVE    RECE451-INPUTCOMENT(IDX)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-MAX         >   ZERO )
               MOVE    SPACE           TO  RECE451-INPUTCOMENT(IDX)
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                       TO  RECE451-INPUTCOMENT(IDX)
           END-IF
           MOVE    RECE451-INPUTCOMENT(IDX)
                                         TO  RECE455-INPUTCOMENT
      *
           WRITE   RECE455-REC
           IF      STS-RECE455         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE455PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE455         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                     TO  CNT-RECE455
      *
           MOVE    1                     TO  FLG-SETCOM-OKIKAE
      *
           .
       26011-SETCOM-HENSYU3-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理開始処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCR0551 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC            SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC      SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
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
           STRING  "ORCR0551 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE41-FILE
           CLOSE                       RECE44-FILE
           CLOSE                       RECE45-FILE
           CLOSE                       RECE441-FILE     
           CLOSE                       RECE451-FILE     
           CLOSE                       RECE449-FILE
           CLOSE                       RECE459-FILE
           CLOSE                       RECE455-FILE
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "RECE41  CNT-IN  :" CNT-RECE41  
           DISPLAY "RECE44G CNT-WK  :" CNT-RECE449 
           DISPLAY "RECE45G CNT-WK  :" CNT-RECE459 
           DISPLAY "RECE44F CNT-OUT :" CNT-RECE44  
           DISPLAY "RECE45F CNT-OUT :" CNT-RECE45  
           DISPLAY "RECE44F REWRITE :" CNT-RECE44-RE
           DISPLAY "RECE45F REWRITE :" CNT-RECE45-RE
           DISPLAY "RECE455 CNT-OUT :" CNT-RECE455
           DISPLAY "*** ORCR0551 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４１データ編集処理（４４）
      *****************************************************************
       800-RECE41-SET44-SEC           SECTION.
      *
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPNUM      TO  RECE44-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
      *
           .
       800-RECE41-SET44-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４１データ編集処理（４４９）
      *****************************************************************
       800-RECE41-SET449-SEC           SECTION.
      *
           INITIALIZE                      RECE449-REC
           MOVE    RECE41-HOSPNUM      TO  RECE449-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE449-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE449-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE449-PTID
           MOVE    RECE41-RECEKA       TO  RECE449-RECEKA
           MOVE    RECE41-HKNID        TO  RECE449-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE449-TEKSTYMD
      *
           MOVE    RECE41-RECESYUBETU  TO  RECE449-RECESYUBETU 
      *
           .
       800-RECE41-SET449-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４１データ編集処理（４４１）
      *****************************************************************
       800-RECE41-SET441-SEC           SECTION.
      *
           INITIALIZE                      RECE441-REC
           MOVE    RECE41-HOSPNUM      TO  RECE441-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE441-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE441-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE441-PTID
           MOVE    RECE41-RECEKA       TO  RECE441-RECEKA
           MOVE    RECE41-HKNID        TO  RECE441-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE441-TEKSTYMD
      *
           .
       800-RECE41-SET441-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４１データ編集処理（４５９）
      *****************************************************************
       800-RECE41-SET459-SEC           SECTION.
      *
           INITIALIZE                      RECE459-REC
           MOVE    RECE41-HOSPNUM      TO  RECE459-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE459-PTID
           MOVE    RECE41-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE41-HKNID        TO  RECE459-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE459-TEKSTYMD
      *
           .
       800-RECE41-SET459-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４データ編集処理（４５）
      *****************************************************************
       800-RECE44-SET45-SEC           SECTION.
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
      *
           .
       800-RECE44-SET45-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４９データ編集処理（４５）
      *****************************************************************
       800-RECE449-SET45-SEC           SECTION.
      *
           INITIALIZE                       RECE45-REC
           MOVE    RECE449-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE449-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE449-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE449-PTID         TO  RECE45-PTID
           MOVE    RECE449-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE449-HKNID        TO  RECE45-HKNID
           MOVE    RECE449-TEKSTYMD     TO  RECE45-TEKSTYMD
      *
           .
       800-RECE449-SET45-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４９データ編集処理（４５９）
      *****************************************************************
       800-RECE449-SET459-SEC           SECTION.
      *
           INITIALIZE                       RECE459-REC
           MOVE    RECE449-HOSPNUM      TO  RECE459-HOSPNUM
           MOVE    RECE449-SRYYM        TO  RECE459-SRYYM
           MOVE    RECE449-NYUGAIKBN    TO  RECE459-NYUGAIKBN
           MOVE    RECE449-PTID         TO  RECE459-PTID
           MOVE    RECE449-RECEKA       TO  RECE459-RECEKA
           MOVE    RECE449-HKNID        TO  RECE459-HKNID
           MOVE    RECE449-TEKSTYMD     TO  RECE459-TEKSTYMD
      *
           .
       800-RECE449-SET459-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４１データ編集処理（４５１）
      *****************************************************************
       800-RECE441-SET451-SEC           SECTION.
      *
           INITIALIZE                       RECE451-REC
           MOVE    RECE441-HOSPNUM      TO  RECE451-HOSPNUM
           MOVE    RECE441-SRYYM        TO  RECE451-SRYYM
           MOVE    RECE441-NYUGAIKBN    TO  RECE451-NYUGAIKBN
           MOVE    RECE441-PTID         TO  RECE451-PTID
           MOVE    RECE441-RECEKA       TO  RECE451-RECEKA
           MOVE    RECE441-HKNID        TO  RECE451-HKNID
           MOVE    RECE441-TEKSTYMD     TO  RECE451-TEKSTYMD
      *
           .
       800-RECE441-SET451-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４９ＷＲＩＴＥ処理
      *****************************************************************
       800-RECE449-WRITE-SEC           SECTION.
      *
           WRITE   RECE449-REC
           IF      STS-RECE449         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE449PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE449         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-RECE449
      *
           .
       800-RECE449-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４５９ＷＲＩＴＥ処理
      *****************************************************************
       800-RECE459-WRITE-SEC           SECTION.
      *
           WRITE   RECE459-REC
           IF      STS-RECE459         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE459PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE459         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1            TO   CNT-RECE459
      *
           .
       800-RECE459-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４４１ＷＲＩＴＥ処理
      *****************************************************************
       800-RECE441-WRITE-SEC           SECTION.
      *
           IF      RECE441-SRYKBN      =   "99"
               IF      RECE441-SRYSYUKBN   =   "991"
                   MOVE    "99"            TO  RECE441-SRYKBN
               ELSE
                   MOVE    "01"            TO  RECE441-SRYKBN
               END-IF
           ELSE
               IF      RECE441-SRYKBN   =   "64"
                   MOVE    "60"            TO  RECE441-SRYKBN
               END-IF
           END-IF
      *
           WRITE   RECE441-REC
           IF      STS-RECE441         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE441PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE441         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1                 TO      CNT-RECE44
      *
           .
       800-RECE441-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト４５１ＷＲＩＴＥ処理
      *****************************************************************
       800-RECE451-WRITE-SEC           SECTION.
      *
           WRITE   RECE451-REC
           IF      STS-RECE451         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               MOVE    3                   TO  WRK-ERR-FLG
               MOVE    RECE451PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE451         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           ADD     1            TO   CNT-RECE45
      *
           .
       800-RECE451-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
               NOT AT  END
                   IF      STS-RECE41          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE41PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE41      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
      *            公務災害、アフターケア、種別不明のとき対象外
      *            指定医療機関ではないとき対象外
      *            ０９５９３で始まるコードがあるとき対象外
                   IF  (   RECE41-SRYYM        <   201307 )
                   OR  (   RECE41-PTNUM        =   SPACE  )
                   OR  (   RECE41-TSTPTNUMKBN  =   "1"    )
                   OR  (   RECE41-RECESYUBETU  =   ZERO
      *****************************************OR  7021
                                               OR  7051   )
                   OR  (   RECE41-ERR-ERRKBN   >   ZERO   )
                   OR  (   RECE41-RSIIRYOKBN   =   "0"    )
                   OR  ((  RECE41-HKNKBN    = "1" OR "2" ) AND
                        (  RECE41-TOTALKIN  =  ZERO      ) AND
                        (  RECE41-TOTALKIN2 =  ZERO      ))
                   OR  (   RECE41-ORIGINALCDKBN
                                               =   1      )
                   OR  ((  RECE41-RECESYUBETU  =   7021  ) AND
                        (  RECE41-SRYYM        <   202012))
                       GO  TO  990-RECE41-READ-SEC
                   END-IF
      *
                   ADD     1           TO  CNT-RECE41
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE44-NEXT-SEC           SECTION.
      *
           READ    RECE44-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE44
               NOT AT  END
                   IF      STS-RECE44          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE44PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE44      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE44
           END-READ
      *
           .
       990-RECE44-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト４４９データ読み
      ****************************************************************
       990-RECE449-NEXT-SEC           SECTION.
      *
           READ    RECE449-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE449
               NOT AT  END
                   IF      STS-RECE449         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE449PARA    TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE449     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE449
           END-READ
      *
           .
       990-RECE449-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト４４１データ読み
      ****************************************************************
       990-RECE441-NEXT-SEC           SECTION.
      *
           READ    RECE441-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE441
               NOT AT  END
                   IF      STS-RECE441         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE441PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE441     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE441
           END-READ
      *
           .
       990-RECE441-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE45-NEXT-SEC           SECTION.
      *
           READ    RECE45-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE45
               NOT AT  END
                   IF      STS-RECE45          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE45PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE45      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE45
           END-READ
      *
           .
       990-RECE45-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト４５９データ読み
      ****************************************************************
       990-RECE459-NEXT-SEC           SECTION.
      *
           READ    RECE459-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE459
               NOT AT  END
                   IF      STS-RECE459         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE459PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE459     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE459
           END-READ
      *
           .
       990-RECE459-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト４５１データ読み
      ****************************************************************
       990-RECE451-NEXT-SEC           SECTION.
      *
           READ    RECE451-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE451
               NOT AT  END
                   IF      STS-RECE451         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE451PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE451     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE451
           END-READ
      *
           .
       990-RECE451-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴マスタ読み込み
      *****************************************************************
       900-SANTEI-INV-SEC             SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SANTEI-REC
                   MOVE    ZERO                TO  FLG-SANTEI
               ELSE
                   MOVE    1                   TO  FLG-SANTEI
                   INITIALIZE                      SANTEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SANTEI
               INITIALIZE                      SANTEI-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SANTEI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       900-SANTEI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       900-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴付加読込
      *****************************************************************
       900-SANTEIPLUS-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEIPLUS-REC
               MOVE    ZERO            TO  FLG-SANTEIPLUS
           ELSE
               INITIALIZE                  SANTEIPLUS-REC
               MOVE    1               TO  FLG-SANTEIPLUS
           END-IF
      *
           .
       900-SANTEIPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト記載
      *****************************************************************
       900-RKISAI-READ-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  RKISAI-REC
               MOVE    ZERO            TO  FLG-RKISAI
           ELSE
               INITIALIZE                  RKISAI-REC
               MOVE    1               TO  FLG-RKISAI
           END-IF
      *
           .
       900-RKISAI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       900-PTCOM-READ-SEC           SECTION.
      *
           MOVE     PTCOM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数テーブルの検索
      *****************************************************************
       900-TENSU-TBL-SEC               SECTION.
      *
           INITIALIZE                           TNS-TENSU-REC
           MOVE    WRK-TENSU-SRYCD      TO      TNS-SRYCD
           MOVE    WRK-TENSU-SRYYM      TO      TNS-YUKOSTYMD(1:6)
           MOVE    "01"                 TO      TNS-YUKOSTYMD(7:2)
           MOVE    TNS-YUKOSTYMD        TO      TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM          TO      TNS-HOSPNUM
           MOVE    TNS-TENSU-REC        TO      MCPDATA-REC
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC               =       ZERO
               MOVE    "tbl_tensu"      TO      MCP-TABLE
               MOVE    "key"            TO      MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC           =       ZERO
                   MOVE    MCPDATA-REC  TO      TNS-TENSU-REC
                   MOVE    ZERO         TO      FLG-TENSU
               ELSE
                   INITIALIZE                   TNS-TENSU-REC
                   MOVE    1            TO      FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                       TNS-TENSU-REC
               MOVE    1                TO      FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"          TO      MCP-TABLE
           MOVE    "key"                TO      MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       900-TENSU-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
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
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0551 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
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
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
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
      *    ＤＢ　オープン処理
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       120-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBSTART-EXT.
           EXIT.
