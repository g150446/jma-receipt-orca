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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBG026.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 後発医薬品数量シェア（置き換え率）（月報）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/02/12    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.08.01    NACL-森脇     16/09/01  持参薬対象外
      *  05.01.00    ORCAMO        22/06/XX  除外コード判定対応
      *  05.01.00    ORCAMO        22/10/XX  R05年3月までの除外コード判定
      *  05.02.00    ORCAMO        23/04/XX  R05年9月までの除外コード判定
      *  05.02.00    ORCAMO        23/10/XX  R06年3月までの除外コード判定
      *  05.02.00    ORCAMO        24/03/XX  R06年9月までの除外コード判定
      *  05.02.00    ORCAMO        24/07/XX  R06年4月からカットオフ値対応
      *  05.02.00    ORCAMO        24/09/XX  R06年10月からの対応
      *  05.02.00    ORCAMO        25/03/XX  R07年03月から09月までの対応
      *                                      除外ファイル固定廃止
      *  05.02.00    ORCAMO        25/06/XX  R07年04月からカットオフ値対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計用ファイル
           SELECT  TOKEI-FILE          ASSIGN    TOKEIPARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  TOKEI-KEY
                                       FILE    STATUS  IS  STS-TOKEI.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *2022/06
      *    除外コードファイル
           SELECT  JYOGAI-FILE        ASSIGN    FILE-NAME
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                       FILE    STATUS  IS  STS-JYOGAI.
      *    除外ファイル（Ｒ４年３月まで）
           SELECT  JYOGAI01-FILE        ASSIGN    JYOGAI01PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAI01-KEY
                                       FILE    STATUS  IS  STS-JYOGAI01.
      *    除外ファイル（Ｒ４年４月から９月まで）
           SELECT  JYOGAI02-FILE        ASSIGN    JYOGAI02PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAI02-KEY
                                       FILE    STATUS  IS  STS-JYOGAI02.
      *2022/10
      *    除外ファイル（Ｒ４年１０月からR０５年３月まで）
      *2023/04 対応
      *    除外ファイル（Ｒ４年１０月からR０４年１２月まで）
           SELECT  JYOGAI03-FILE       ASSIGN    JYOGAI03PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAI03-KEY
                                       FILE    STATUS  IS  STS-JYOGAI03.
      *2023/04
      *    除外ファイル（Ｒ５年１月からR０５年３月まで）
           SELECT  JYOGAI04-FILE       ASSIGN    JYOGAI04PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAI04-KEY
                                       FILE    STATUS  IS  STS-JYOGAI04.
      *    除外ファイル（Ｒ５年４月からR０５年９月まで）
           SELECT  JYOGAI05-FILE       ASSIGN    JYOGAI05PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAI05-KEY
                                       FILE    STATUS  IS  STS-JYOGAI05.
      *    除外ファイル（Ｒ５年１０月からR０６年３月まで）
           SELECT  JYOGAI06-FILE       ASSIGN    JYOGAI06PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAI06-KEY
                                       FILE    STATUS  IS  STS-JYOGAI06.
      *2024/03
      *    除外ファイル（Ｒ６年４月からR６年９月まで）
           SELECT  JYOGAI07-FILE       ASSIGN    JYOGAI07PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAI07-KEY
                                       FILE    STATUS  IS  STS-JYOGAI07.
      *
      *2024/09
      *    除外ファイル（Ｒ６年１０月からR７年３月まで）
           SELECT  JYOGAI08-FILE       ASSIGN    JYOGAI08PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAI08-KEY
                                       FILE    STATUS  IS  STS-JYOGAI08.
      *2024/07
      *    カットオフ対象ファイル（Ｒ６年４月から）
           SELECT  CUTOFF01-FILE       ASSIGN    CUTOFF01PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  CUTOFF01-KEY
                                       FILE    STATUS  IS  STS-CUTOFF01.
      *2024/09
      *    カットオフ対象ファイル（Ｒ６年９月からＲ７年３月まで）
           SELECT  CUTOFF02-FILE       ASSIGN    CUTOFF02PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  CUTOFF02-KEY
                                       FILE    STATUS  IS  STS-CUTOFF02.
      *2025/03
      *    カットオフ対象ファイル（Ｒ７年４月からＲ７年９月まで）
           SELECT  CUTOFF03-FILE       ASSIGN    CUTOFF03PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  CUTOFF03-KEY
                                       FILE    STATUS  IS  STS-CUTOFF03.
      *2025/03
      *    除外ファイル１
           SELECT  JYOGAIF01-FILE      ASSIGN    JYOGAIF01PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAIF01-KEY
                                       FILE    STATUS  IS  STS-JYOGAIF1.
      *    除外ファイル２
           SELECT  JYOGAIF02-FILE      ASSIGN    JYOGAIF02PARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  JYOGAIF02-KEY
                                       FILE    STATUS  IS  STS-JYOGAIF2.
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-YKZKBN        PIC 9(01).
               05  TOKEI-YAKKAKJNCD    PIC X(12).
               05  TOKEI-NAME          PIC X(50).
               05  TOKEI-KOUHATU       PIC 9(01).
               05  TOKEI-SRYCD         PIC X(09).
           03  TOKEI-TANISU            PIC 9(04).
           03  TOKEI-TANINAME          PIC X(24).
           03  TOKEI-TUKI-G            OCCURS 3.
               05  TOKEI-SRYSURYO      PIC 9(10)V9(05).
      *2022/06 除外分
           03  TOKEI-GAI-TUKI-G            OCCURS 3.
               05  TOKEI-GAI-SRYSURYO     PIC 9(10)V9(05).
      *2024/07 カットオフ対象分
           03  TOKEI-OFF-TUKI-G            OCCURS 3.
               05  TOKEI-OFF-SRYSURYO      PIC 9(10)V9(05).
           03  TOKEI-OFF-TUKI-KBN          PIC X(01).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
      *2022/06
      *    除外コードファイル
       FD  JYOGAI-FILE.
       01  JYOGAI-REC.
           03  JYOGAI-YAKKAKJNCD       PIC X(12).
      *
      *    除外ファイル（Ｒ４年３月まで）
       FD  JYOGAI01-FILE.
       01  JYOGAI01-REC.
           03  JYOGAI01-KEY.
               05  JYOGAI01-YAKKAKJNCD      PIC X(12).
      *
      *    除外ファイル（Ｒ４年９月まで）
       FD  JYOGAI02-FILE.
       01  JYOGAI02-REC.
           03  JYOGAI02-KEY.
               05  JYOGAI02-YAKKAKJNCD      PIC X(12).
      *
      *    除外ファイル（Ｒ５年３月まで）
       FD  JYOGAI03-FILE.
       01  JYOGAI03-REC.
           03  JYOGAI03-KEY.
               05  JYOGAI03-YAKKAKJNCD      PIC X(12).
      *2023/04
      *    除外ファイル（Ｒ５年３月まで）
       FD  JYOGAI04-FILE.
       01  JYOGAI04-REC.
           03  JYOGAI04-KEY.
               05  JYOGAI04-YAKKAKJNCD      PIC X(12).
      *    除外ファイル（Ｒ５年９月まで）
       FD  JYOGAI05-FILE.
       01  JYOGAI05-REC.
           03  JYOGAI05-KEY.
               05  JYOGAI05-YAKKAKJNCD      PIC X(12).
      *
      *    除外ファイル（Ｒ６年３月まで）
       FD  JYOGAI06-FILE.
       01  JYOGAI06-REC.
           03  JYOGAI06-KEY.
               05  JYOGAI06-YAKKAKJNCD      PIC X(12).
      *
      *    除外ファイル（Ｒ６年９月まで）
       FD  JYOGAI07-FILE.
       01  JYOGAI07-REC.
           03  JYOGAI07-KEY.
               05  JYOGAI07-YAKKAKJNCD      PIC X(12).
      *2024/09
      *    除外ファイル（Ｒ７年３月まで）
       FD  JYOGAI08-FILE.
       01  JYOGAI08-REC.
           03  JYOGAI08-KEY.
               05  JYOGAI08-YAKKAKJNCD      PIC X(12).
      *2024/07
      *    カットオフ値対象ファイル（Ｒ６年４月から）
       FD  CUTOFF01-FILE.
       01  CUTOFF01-REC.
           03  CUTOFF01-KEY.
               05  CUTOFF01-YAKKAKJNCD      PIC X(12).
      *2024/09
      *    カットオフ値対象ファイル（Ｒ７年３月まで）
       FD  CUTOFF02-FILE.
       01  CUTOFF02-REC.
           03  CUTOFF02-KEY.
               05  CUTOFF02-YAKKAKJNCD      PIC X(12).
      *2025/03
      *    カットオフ値対象ファイル（Ｒ７年９月まで）
       FD  CUTOFF03-FILE.
       01  CUTOFF03-REC.
           03  CUTOFF03-KEY.
               05  CUTOFF03-YAKKAKJNCD      PIC X(12).
      *
      *    除外ファイル１
       FD  JYOGAIF01-FILE.
       01  JYOGAIF01-REC.
           03  JYOGAIF01-KEY.
               05  JYOGAIF01-YAKKAKJNCD      PIC X(12).
      *    除外ファイル２
       FD  JYOGAIF02-FILE.
       01  JYOGAIF02-REC.
           03  JYOGAIF02-KEY.
               05  JYOGAIF02-YAKKAKJNCD      PIC X(12).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAI01//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAI02//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAI03//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAI04//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAI05//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAI06//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAI07//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //CUTOFF01//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAI08//.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //CUTOFF02//.
      *2025/05
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //CUTOFF03//.
      *2025/03
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAIF01//.
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //JYOGAIF02//.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCMG007.INC".
      *
       01  FILE-NAME               PIC X(512).
      *
      *    ファイル名(Ｒ０４年３月まで))
       01  DATA-FILE11.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20220331_20220314.csv".
       01  DATA-FILE21.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20220331_20220314.csv".
      *    ファイル名(Ｒ０４年４月から))
       01  DATA-FILE12.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20220401_20220314.csv".
       01  DATA-FILE22.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20220401_20220314.csv".
      *
      *    ファイル名(Ｒ０４年１０月から)
       01  DATA-FILE13.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20221001_20221004.csv".
       01  DATA-FILE23.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20221001_20221004.csv".
      *2023/04
      *    ファイル名(Ｒ０５年１月から)
       01  DATA-FILE14.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20230101_20230406.csv".
       01  DATA-FILE24.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20230101_20230406.csv".
      *    ファイル名(Ｒ０５年４月から)
       01  DATA-FILE15.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20230401_20230406.csv".
       01  DATA-FILE25.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20230401_20230406.csv".
      *2023/10
      *    ファイル名(Ｒ０５年１０月から)
       01  DATA-FILE16.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20231001_20231024.csv".
       01  DATA-FILE26.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20231001_20231024.csv".
      *
      *2024/03
      *    ファイル名(Ｒ０６年４月から)
       01  DATA-FILE17.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20240401_20240318.csv".
       01  DATA-FILE27.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20240401_20240318.csv".
      *
      *2024/09
      *    ファイル名(Ｒ０６年１０月から)
       01  DATA-FILE18.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20241001_20240927.csv".
       01  DATA-FILE28.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20241001_20240927.csv".
      *2025/03
      *    ファイル名(Ｒ０７年４月から)
       01  DATA-FILE19.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20250401_20250318.csv".
       01  DATA-FILE29.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_except_20250401_20250318.csv".
      *
      *2024/07
      *    カットオフファイル名(Ｒ０６年４月から)
       01  DATA-CUTOFF11.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_cutoff_20240401_20240627.csv".
       01  DATA-CUTOFF21.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_cutoff_20240401_20240627.csv".
      *
      *2024/09
      *    カットオフファイル名(Ｒ０６年１０から)
       01  DATA-CUTOFF12.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_cutoff_20241001_20240927.csv".
       01  DATA-CUTOFF22.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
               "ge_add_cutoff_20241001_20240927.csv".
      *2025/03
      *    カットオフファイル名(Ｒ０７年４月から)
       01  DATA-CUTOFF13.
           03  FILLER              PIC X(15)   VALUE  "patch-lib/data/".
           03  FILLER              PIC X(36)   VALUE
      *2025/06 ファイル名変更
      ******   "ge_add_cutoff_20250401_20250318.csv".
               "ge_add_cutoff_20250401_20250603.csv".
       01  DATA-CUTOFF23.
           03  FILLER              PIC X(05)   VALUE   "data/".
           03  FILLER              PIC X(36)   VALUE
      *2025/06 ファイル名変更
      *****    "ge_add_cutoff_20250401_20250318.csv".
               "ge_add_cutoff_20250401_20250603.csv".
      *
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
           03  STS-JYOGAI              PIC X(02).
           03  STS-JYOGAI01            PIC X(02).
           03  STS-JYOGAI02            PIC X(02).
           03  STS-JYOGAI03            PIC X(02).
           03  STS-JYOGAI04            PIC X(02).
           03  STS-JYOGAI05            PIC X(02).
           03  STS-JYOGAI06            PIC X(02).
           03  STS-JYOGAI07            PIC X(02).
           03  STS-CUTOFF01            PIC X(02).
           03  STS-JYOGAI08            PIC X(02).
           03  STS-CUTOFF02            PIC X(02).
           03  STS-JYOGAIF1            PIC X(02).
           03  STS-JYOGAIF2            PIC X(02).
           03  STS-CUTOFF03            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-JYOGAI              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-GECLASS             PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-SP                  PIC 9(01).
      *
           03  FLG-JYOGAI2              PIC 9(01).
           03  FLG-GAIARI               PIC 9(01).
           03  FLG-JYOGAIARI           PIC 9(01).
      *
           03  FLG-CUTOFFARI           PIC 9(01).
           03  FLG-CUTOFF              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(03).
           03  CNT-TOKEI               PIC 9(05).
           03  CNT-JYOGAI1             PIC 9(05).
           03  CNT-JYOGAI2             PIC 9(05).
           03  CNT-JYOGAI3             PIC 9(05).
           03  CNT-JYOGAI4             PIC 9(05).
           03  CNT-JYOGAI5             PIC 9(05).
           03  CNT-JYOGAI6             PIC 9(05).
           03  CNT-JYOGAI7             PIC 9(05).
           03  CNT-CUTOFF1             PIC 9(05).
           03  CNT-JYOGAI8             PIC 9(05).
           03  CNT-CUTOFF2             PIC 9(05).
           03  CNT-JYOGAIF1            PIC 9(05).
           03  CNT-JYOGAIF2            PIC 9(05).
           03  CNT-CUTOFF3             PIC 9(05).
      *
           03  CNT-KETA                PIC 9(02).
           03  CNT-KETA2               PIC 9(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX-T                   PIC 9(01).
           03  IDX-S                   PIC 9(04).
           03  IDX-D                   PIC 9(02).
           03  IDX-W                   PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-SYORIYM        PIC X(06).
           03  WRK-PARA-MEISAIKBN      PIC 9(01).
           03  WRK-PARA-CSVKBN         PIC 9(01).
           03  WRK-PARA-TAISYOKBN      PIC 9(01).
           03  WRK-PARA-NYUGAIKBN      PIC 9(01).
      *2022/07
           03  WRK-PARA-SYUKEIKBN      PIC 9(01).
      *2024/07
           03  WRK-PARA-CUTOFFKBN      PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-TOUKEIERR           PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *2022/06
           03  WRK-DAY                 PIC 9(02).
      *
           03  WRK-TUKI-G.
               05  WRK-TUKI            PIC X(06)   OCCURS  3.
           03  WRK-KIKAKU-G.
               05  WRK-KIKAKU          PIC 9(07)   OCCURS  4.
           03  WRK-GOUKEI-G.
               05  WRK-KBN-ALL         PIC 9(07)   OCCURS  4.
               05  WRK-KBN-MIX         PIC 9(07)   OCCURS  4.
               05  WRK-KBN-KOU         PIC 9(07)   OCCURS  4.
      *2022/06
           03  WRK-G-KIKAKU-G.
               05  WRK-G-KIKAKU          PIC 9(07)   OCCURS  4.
           03  WRK-G-GOUKEI-G.
               05  WRK-GKBN-ALL         PIC 9(07)   OCCURS  4.
               05  WRK-GKBN-MIX         PIC 9(07)   OCCURS  4.
               05  WRK-GKBN-KOU         PIC 9(07)   OCCURS  4.
      *2024/07
      *    カットオフ集計
           03  WRK-O-KIKAKU-G.
               05  WRK-O-KIKAKU        PIC 9(07)   OCCURS  4.
           03  WRK-O-GOUKEI-G.
               05  WRK-OFF-MIX         PIC 9(07)   OCCURS  4.
      *
           03  WRK-ALL                 PIC 9(07).
           03  WRK-MIX                 PIC 9(07).
      *
           03  WRK-PRTYMD              PIC X(22).
           03  WRK-KIKAN               PIC X(36).
           03  WRK-TAISYO              PIC X(16).
      *
           03  WRK-SRYCD               PIC X(09).
           03  WRK-TANISU              PIC 9(03).
           03  WRK-KOUHATU             PIC 9(01).
      *
           03  WRK-WARIAI              PIC 9(07).
           03  WRK-SURYO               PIC 9(10)V9(05).
      *
           03  WRK-ZZ                  PIC ZZZZZZZ.
           03  WRK-Z7                  PIC ZZZZZZ9.
           03  WRK-Z4                  PIC ZZZ9.
           03  WRK-Z3                  PIC ZZ9.
           03  WRK-Z2                  PIC Z9.
      *
      *    ＣＳＶデータ編集用
           03  WRK-REC             PIC X(20000).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(20000).
               05  WRK-NUM         PIC -------------9.99999. 
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-RECSTR      PIC 9(02). 
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-RECEND      PIC 9(01).
           03  WRK-REC02           PIC X(20000).
      *
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(18)   VALUE
                                                   "kouhatsuhin_ritsu_".
               05  WRK-TO-KIKAN-STRYM  PIC X(06).
               05  FILLER              PIC X(01)   VALUE   "-".
               05  WRK-TO-KIKAN-ENDYM  PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "薬剤区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "診療行為コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "後発区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "薬価基準コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "規格数量".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV-HEAD-TUKI-G                 OCCURS  3.
               05  CSV-HEAD-TUKI   PIC X(02)   VALUE   SPACE.
               05  FILLER          PIC X(03)   VALUE   "月,".
           03  FILLER              PIC X(04)   VALUE   "合計".
      *2024/07
      *    ヘッダー情報（カットオフあり）
       01  CSV-HEAD-012.
           03  FILLER              PIC X(08)   VALUE   "薬剤区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "診療行為コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "後発区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "薬価基準コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "規格数量".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV12-HEAD-TUKI-G                 OCCURS  3.
               05  CSV12-HEAD-TUKI   PIC X(02)   VALUE   SPACE.
               05  FILLER          PIC X(03)   VALUE   "月,".
           03  FILLER              PIC X(04)   VALUE   "合計".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "オフ".
      *2022/06
      *    ヘッダー情報（除外あり）
       01  CSV-HEAD-02.
           03  FILLER              PIC X(08)   VALUE   "薬剤区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "診療行為コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "後発区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "薬価基準コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "規格数量".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV2-HEAD-TUKI-G                 OCCURS  3.
               05  CSV2-HEAD-TUKI   PIC X(02)   VALUE   SPACE.
               05  FILLER          PIC X(03)   VALUE   "月,".
           03  FILLER              PIC X(04)   VALUE   "合計".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "除外".
      *2024/07
      *    ヘッダー情報（除外あり）（カットオフあり）
       01  CSV-HEAD-022.
           03  FILLER              PIC X(08)   VALUE   "薬剤区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "診療行為コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "後発区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "薬価基準コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "規格数量".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  CSV22-HEAD-TUKI-G                 OCCURS  3.
               05  CSV22-HEAD-TUKI   PIC X(02)   VALUE   SPACE.
               05  FILLER          PIC X(03)   VALUE   "月,".
           03  FILLER              PIC X(04)   VALUE   "合計".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "除外".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "オフ".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    医薬品分類マスタ
       01  GECLASS-REC.
           COPY    "CPGENERICCLASS.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    指定診療行為件数調サブ２
           COPY    "CPORCSS009.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
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
      *    主　　処理
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
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  TTNS-AREA
           INITIALIZE                  SPA-AREA
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
                                       WRK-PARA-SYORIYM
                                       WRK-PARA-MEISAIKBN
                                       WRK-PARA-CSVKBN
                                       WRK-PARA-TAISYOKBN
                                       WRK-PARA-NYUGAIKBN
                                       WRK-PARA-SYUKEIKBN
                                       WRK-PARA-CUTOFFKBN
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG026"          TO  JOB-PGID
           MOVE    "後発医薬品数量シェア（置換え率）"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "BG02601"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  TOKEIPARA-HOSPNUM
      *
           MOVE    "BG02611"           TO  JYOGAI01PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAI01PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAI01PARA-HOSPNUM
      *
           MOVE    "BG02612"           TO  JYOGAI02PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAI02PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAI02PARA-HOSPNUM
      *
           MOVE    "BG02613"           TO  JYOGAI03PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAI03PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAI03PARA-HOSPNUM
      *
           MOVE    "BG02614"           TO  JYOGAI04PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAI04PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAI04PARA-HOSPNUM
      *
           MOVE    "BG02615"           TO  JYOGAI05PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAI05PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAI05PARA-HOSPNUM
      *
      *
           MOVE    "BG02616"           TO  JYOGAI06PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAI06PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAI06PARA-HOSPNUM
      *
      *2024/03
           MOVE    "BG02617"           TO  JYOGAI07PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAI07PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAI07PARA-HOSPNUM
      *
      *2024/09
           MOVE    "BG02618"           TO  JYOGAI08PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAI08PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAI08PARA-HOSPNUM
      *
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES(1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES(2)
           MOVE    JYOGAI01PARA-BASENAME   TO  SGETTEMP-BASENAMES(3)
           MOVE    JYOGAI02PARA-BASENAME   TO  SGETTEMP-BASENAMES(4)
           MOVE    JYOGAI03PARA-BASENAME   TO  SGETTEMP-BASENAMES(5)
           MOVE    JYOGAI04PARA-BASENAME   TO  SGETTEMP-BASENAMES(6)
           MOVE    JYOGAI05PARA-BASENAME   TO  SGETTEMP-BASENAMES(7)
           MOVE    JYOGAI06PARA-BASENAME   TO  SGETTEMP-BASENAMES(8)
           MOVE    JYOGAI07PARA-BASENAME   TO  SGETTEMP-BASENAMES(9)
           MOVE    JYOGAI08PARA-BASENAME   TO  SGETTEMP-BASENAMES(10)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(2)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES(3)
                                       TO  JYOGAI01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(4)
                                       TO  JYOGAI02PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(5)
                                       TO  JYOGAI03PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(6)
                                       TO  JYOGAI04PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(7)
                                       TO  JYOGAI05PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(8)
                                       TO  JYOGAI06PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(9)
                                       TO  JYOGAI07PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(10)
                                       TO  JYOGAI08PARA-FULLNAME
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
      *2022/06
           MOVE    ZERO                TO  FLG-JYOGAIARI
           MOVE    ZERO                TO  FLG-CUTOFFARI
           IF      FLG-END             =   ZERO
      *        除外コードファイル作成
      *        対象の時のみ
               IF     (WRK-PARA-SYUKEIKBN      =   2 )
      *****        AND (WRK-TUKI(3)             <=  "202209")
      *2022/10     R05年３月まで
      ****         AND (WRK-TUKI(1)             <=  "202303")
      *2023/04     R05年9月まで
      ***          AND (WRK-TUKI(1)             <=  "202309")
      *2023/10     R06年3月まで
      *            AND (WRK-TUKI(1)             <=  "202403")
      *2024/03     R06年9月まで
      *            AND (WRK-TUKI(1)             <=  "202409")
      *2024/09     R07年3月まで
      **           AND (WRK-TUKI(1)             <=  "202503")
      *2024/09     R07年9月まで
                   AND (WRK-TUKI(1)             <=  "202509")
                   PERFORM 120-JYOGAIFILE-SEC
                   MOVE    1                   TO  FLG-JYOGAIARI
               END-IF
      *2024/07
      *        カットオフ値
               IF     (WRK-PARA-CUTOFFKBN      =   2 )
      *            R06年4月から
                  AND (WRK-TUKI(3)             >=  "202404")
      *2024/09     R07年3月まで
      *****       AND (WRK-TUKI(1)             <=  "202503")
      *2025/03    R07年9月まで
                  AND (WRK-TUKI(1)             <=  "202509")
                   PERFORM 120-CUTOFFFILE-SEC
                   MOVE    1                   TO  FLG-CUTOFFARI
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
           IF      WRK-PARA-SYORIYM    <   "201603"
               MOVE    "診療年月は平成２８年３月以降を指定してください"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-MEISAIKBN      =   0
                                           OR  1
                                           OR  2 )
               CONTINUE
           ELSE
               MOVE    "明細区分が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-CSVKBN         =   0
                                           OR  1
                                           OR  2 )
               CONTINUE
           ELSE
               MOVE    "ＣＳＶ区分が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-TAISYOKBN  NOT =   0 ) AND
                 ( WRK-PARA-TAISYOKBN  NOT =   1 ) AND
                 ( WRK-PARA-TAISYOKBN  NOT =   2 )
               MOVE    "対象区分が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
           IF    ( WRK-PARA-NYUGAIKBN  NOT =   0 ) AND
                 ( WRK-PARA-NYUGAIKBN  NOT =   1 ) AND
                 ( WRK-PARA-NYUGAIKBN  NOT =   2 )
               MOVE    "入外区分が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *R04.07
           IF    ( WRK-PARA-SYUKEIKBN      =   0
                                           OR  1
                                           OR  2 )
               CONTINUE
           ELSE
               MOVE    "集計方法が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *R04.07
           IF    ( WRK-PARA-CUTOFFKBN      =   0
                                           OR  1
                                           OR  2 )
               CONTINUE
           ELSE
               MOVE    "カットオフ集計方法が正しくありません"
                                      TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-PRTYMD
      *
           MOVE    SPACE               TO  WRK-TUKI-G
      *    対象年月編集（開始年月は２カ月前）
           MOVE    WRK-PARA-SYORIYM    TO  WRK-SYMD(1:6)
           PERFORM VARYING IDX-T   FROM    3   BY  -1
                   UNTIL   IDX-T   =   0
               MOVE    WRK-SYMD(1:6)   TO  WRK-TUKI(IDX-T)
               MOVE    WRK-SMM         TO  WRK-Z2
               MOVE    WRK-Z2          TO  CSV-HEAD-TUKI(IDX-T)
      *2022/06
               MOVE    WRK-Z2          TO  CSV2-HEAD-TUKI(IDX-T)
      *2024/07
               MOVE    WRK-Z2          TO  CSV12-HEAD-TUKI(IDX-T)
               MOVE    WRK-Z2          TO  CSV22-HEAD-TUKI(IDX-T)
      *
               IF      WRK-SMM     =   1
                   COMPUTE WRK-SYY =   WRK-SYY -   1
                   MOVE    12          TO  WRK-SMM
               ELSE
                   COMPUTE WRK-SMM =   WRK-SMM -   1
               END-IF
           END-PERFORM
      *
           MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
      *
      *    開始日
           MOVE    WRK-TUKI(1)         TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-KIKAN(1:16)
           MOVE    "〜"                TO  WRK-KIKAN(17:2)
      *    終了日
           MOVE    WRK-TUKI(3)         TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-KIKAN(19:16)
           MOVE    "分"                TO  WRK-KIKAN(35:2)
           MOVE    WRK-TUKI(1)         TO  WRK-TO-KIKAN-STRYM
           MOVE    WRK-TUKI(3)         TO  WRK-TO-KIKAN-ENDYM
      *
           EVALUATE    WRK-PARA-TAISYOKBN
               WHEN    0
                   MOVE    "全て"              TO  WRK-TAISYO
               WHEN    1
                   MOVE    "投薬"              TO  WRK-TAISYO
               WHEN    2
                   MOVE    "在・投・注"        TO  WRK-TAISYO
           END-EVALUATE
      *
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    0
                   MOVE    "入外"              TO  WRK-TAISYO(13:4)
               WHEN    1
                   MOVE    "入院"              TO  WRK-TAISYO(13:4)
               WHEN    2
                   MOVE    "外来"              TO  WRK-TAISYO(13:4)
           END-EVALUATE
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *2022/06
      *****************************************************************
      *    除外コードファイル作成処理
      *****************************************************************
       120-JYOGAIFILE-SEC                SECTION.
      *
           IF      WRK-TUKI(3)         <=  "202503"
      *        終了月が2025/3 までは全部
               CONTINUE
           ELSE
      *        開始月から終了月分
               PERFORM 1202-JYOGAI-FILE02-SEC
               GO  TO  120-JYOGAIFILE-EXT
           END-IF
      *
           OPEN    OUTPUT              JYOGAI01-FILE
           OPEN    OUTPUT              JYOGAI02-FILE
           OPEN    OUTPUT              JYOGAI03-FILE
           OPEN    OUTPUT              JYOGAI04-FILE
           OPEN    OUTPUT              JYOGAI05-FILE
           OPEN    OUTPUT              JYOGAI06-FILE
      *
           OPEN    OUTPUT              JYOGAI07-FILE
           OPEN    OUTPUT              JYOGAI08-FILE
      *
      *    除外コードテーブル（Ｒ０４年３月まで）
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE11         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI     NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE21         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI     NOT =   ZERO
                   DISPLAY "JYOGAI-FILE21 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAI1
                      MOVE    JYOGAI-REC       TO  JYOGAI01-REC
                      WRITE                    JYOGAI01-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *    除外コードテーブル（Ｒ０４年４月から）
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE12         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE22         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE22 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAI2
                      MOVE    JYOGAI-REC       TO  JYOGAI02-REC
                      WRITE                    JYOGAI02-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *    除外コードテーブル（Ｒ０４年１０月から）
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE13         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE23         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE23 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAI3
                      MOVE    JYOGAI-REC       TO  JYOGAI03-REC
                      WRITE                    JYOGAI03-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *2023/04
      *    除外コードテーブル（Ｒ０５年１月から）
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE14         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE24         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE24 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAI4
                      MOVE    JYOGAI-REC       TO  JYOGAI04-REC
                      WRITE                    JYOGAI04-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *    除外コードテーブル（Ｒ０５年４月から）
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE15         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE25         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE25 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAI5
                      MOVE    JYOGAI-REC       TO  JYOGAI05-REC
                      WRITE                    JYOGAI05-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *2023/10
      *    除外コードテーブル（Ｒ０５年１０月から）
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE16         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE26         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE26 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAI6
                      MOVE    JYOGAI-REC       TO  JYOGAI06-REC
                      WRITE                    JYOGAI06-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *2024/03
      *    除外コードテーブル（Ｒ０６年４月から）
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE17         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           MOVE    ZERO                TO  STS-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE27         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE27 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAI7
                      MOVE    JYOGAI-REC       TO  JYOGAI07-REC
                      WRITE                    JYOGAI07-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *2024/09
      *    除外コードテーブル（Ｒ０６年１０月から）
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE18         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           MOVE    ZERO                TO  STS-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE28         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE28 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAI8
                      MOVE    JYOGAI-REC       TO  JYOGAI08-REC
                      WRITE                    JYOGAI08-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *
           CLOSE                        JYOGAI01-FILE
           CLOSE                        JYOGAI02-FILE
           CLOSE                        JYOGAI03-FILE
           CLOSE                        JYOGAI04-FILE
           CLOSE                        JYOGAI05-FILE
           CLOSE                        JYOGAI06-FILE
           CLOSE                        JYOGAI07-FILE
           CLOSE                        JYOGAI08-FILE
           .
       120-JYOGAIFILE-EXT.
           EXIT.
      *
      *2025/03
      *****************************************************************
      *     開始月から終了月分除外コードファイル作成処理
      *****************************************************************
       1202-JYOGAI-FILE02-SEC                SECTION.
      *
      *    除外コードテーブル（Ｒ０６年１０月から）
           MOVE    "BG02618"           TO  JYOGAIF01PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAIF01PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAIF01PARA-HOSPNUM
      *
      *    除外コードテーブル（Ｒ０７年４月から）
           MOVE    "BG02619"           TO  JYOGAIF02PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JYOGAIF02PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  JYOGAIF02PARA-HOSPNUM
      *
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    JYOGAIF01PARA-BASENAME   TO  SGETTEMP-BASENAMES(1)
           MOVE    JYOGAIF02PARA-BASENAME   TO  SGETTEMP-BASENAMES(2)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
            MOVE    SGETTEMP-FULLNAMES(1)
                                        TO  JYOGAIF01PARA-FULLNAME
            MOVE    SGETTEMP-FULLNAMES(2)
                                        TO  JYOGAIF02PARA-FULLNAME
      *
           OPEN    OUTPUT              JYOGAIF01-FILE
           OPEN    OUTPUT              JYOGAIF02-FILE
      *
      *    除外コードテーブル（Ｒ０６年１０月から）
      *    ファイルアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE18         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           MOVE    ZERO                TO  STS-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE28         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE28 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAIF1
                      MOVE    JYOGAI-REC       TO  JYOGAIF01-REC
                      WRITE                    JYOGAIF01-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *    除外コードテーブル（Ｒ０７年４月から）
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE19         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           MOVE    ZERO                TO  STS-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-FILE29         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "JYOGAI-FILE29 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-JYOGAIF2
                      MOVE    JYOGAI-REC       TO  JYOGAIF02-REC
                      WRITE                    JYOGAIF02-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *
           CLOSE                        JYOGAIF01-FILE
           CLOSE                        JYOGAIF02-FILE
           .
       1202-JYOGAI-FILE02-EXT.
           EXIT.
      *
      *2024/07
      *****************************************************************
      *    カットオフ対象コードファイル作成処理
      *****************************************************************
       120-CUTOFFFILE-SEC                SECTION.
      *
      *    カットオフ対象
           MOVE    "BG02650"           TO  CUTOFF01PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  CUTOFF01PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  CUTOFF01PARA-HOSPNUM
      *
      *    カットオフ対象
           MOVE    "BG02651"           TO  CUTOFF02PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  CUTOFF02PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  CUTOFF02PARA-HOSPNUM
      *2025/03
      *    カットオフ対象
           MOVE    "BG02652"           TO  CUTOFF03PARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  CUTOFF03PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  CUTOFF03PARA-HOSPNUM
      *
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    CUTOFF01PARA-BASENAME   TO  SGETTEMP-BASENAMES(1)
           MOVE    CUTOFF02PARA-BASENAME   TO  SGETTEMP-BASENAMES(2)
           MOVE    CUTOFF03PARA-BASENAME   TO  SGETTEMP-BASENAMES(3)
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  CUTOFF01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(2)
                                       TO  CUTOFF02PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(3)
                                       TO  CUTOFF03PARA-FULLNAME
      *
           OPEN    OUTPUT              CUTOFF01-FILE
           OPEN    OUTPUT              CUTOFF02-FILE
           OPEN    OUTPUT              CUTOFF03-FILE
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-CUTOFF11       TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           MOVE    ZERO                TO  STS-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-CUTOFF21       TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "CUTOFF-FILE1 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-CUTOFF1
                      MOVE    JYOGAI-REC       TO  CUTOFF01-REC
                      WRITE                    CUTOFF01-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *2024/09
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-CUTOFF12       TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           MOVE    ZERO                TO  STS-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-CUTOFF22       TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "CUTOFF-FILE12 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-CUTOFF2
                      MOVE    JYOGAI-REC       TO  CUTOFF02-REC
                      WRITE                    CUTOFF02-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *2025/03
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-CUTOFF13       TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
           MOVE    ZERO                TO  FLG-JYOGAI
           MOVE    ZERO                TO  STS-JYOGAI
           OPEN    INPUT               JYOGAI-FILE
           IF      STS-JYOGAI      NOT =   ZERO
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    DATA-CUTOFF23       TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01        TO  FILE-NAME
               OPEN    INPUT               JYOGAI-FILE
               IF      STS-JYOGAI      NOT =   ZERO
                   DISPLAY "CUTOFF-FILE13 OPEN ERR :" STS-JYOGAI "."
                   MOVE    1              TO  FLG-JYOGAI
               END-IF
           END-IF
      *
           PERFORM UNTIL    FLG-JYOGAI     =   1
               READ                    JYOGAI-FILE
                   AT  END
                       MOVE    1               TO  FLG-JYOGAI
                   NOT AT  END
                      ADD     1                TO  CNT-CUTOFF3
                      MOVE    JYOGAI-REC       TO  CUTOFF03-REC
                      WRITE                    CUTOFF03-REC
               END-READ
           END-PERFORM
           CLOSE                        JYOGAI-FILE
      *
      *
           CLOSE                       CUTOFF01-FILE
           CLOSE                       CUTOFF02-FILE
           CLOSE                       CUTOFF03-FILE
           .
       120-CUTOFFFILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           OPEN    I-O                 TOKEI-FILE
      *2022/06
           IF      FLG-JYOGAIARI       =   1
               IF      WRK-TUKI(3)         <=  "202503"
               OPEN    INPUT               JYOGAI01-FILE
               OPEN    INPUT               JYOGAI02-FILE
               OPEN    INPUT               JYOGAI03-FILE
               OPEN    INPUT               JYOGAI04-FILE
               OPEN    INPUT               JYOGAI05-FILE
               OPEN    INPUT               JYOGAI06-FILE
               OPEN    INPUT               JYOGAI07-FILE
               OPEN    INPUT               JYOGAI08-FILE
      *2025/03
               ELSE
               OPEN    INPUT               JYOGAIF01-FILE
               OPEN    INPUT               JYOGAIF02-FILE
               END-IF
           END-IF
      *2024/07
           IF      FLG-CUTOFFARI       =   1
               OPEN    INPUT               CUTOFF01-FILE
               OPEN    INPUT               CUTOFF02-FILE
               OPEN    INPUT               CUTOFF03-FILE
           END-IF
      *
      *    診療行為集計処理
           PERFORM 220-MAIN-SRYACT-SEC
      *
           CLOSE                       TOKEI-FILE
      *2022/06
           IF      FLG-JYOGAIARI       =   1
               IF      WRK-TUKI(3)         <=  "202503"
               CLOSE                       JYOGAI01-FILE
               CLOSE                       JYOGAI02-FILE
               CLOSE                       JYOGAI03-FILE
               CLOSE                       JYOGAI04-FILE
               CLOSE                       JYOGAI05-FILE
               CLOSE                       JYOGAI06-FILE
               CLOSE                       JYOGAI07-FILE
               CLOSE                       JYOGAI08-FILE
      *
               OPEN    OUTPUT              JYOGAI01-FILE
               OPEN    OUTPUT              JYOGAI02-FILE
               OPEN    OUTPUT              JYOGAI03-FILE
               OPEN    OUTPUT              JYOGAI04-FILE
               OPEN    OUTPUT              JYOGAI05-FILE
               OPEN    OUTPUT              JYOGAI06-FILE
               OPEN    OUTPUT              JYOGAI07-FILE
               OPEN    OUTPUT              JYOGAI08-FILE
               CLOSE                       JYOGAI01-FILE
               CLOSE                       JYOGAI02-FILE
               CLOSE                       JYOGAI03-FILE
               CLOSE                       JYOGAI04-FILE
               CLOSE                       JYOGAI05-FILE
               CLOSE                       JYOGAI06-FILE
               CLOSE                       JYOGAI07-FILE
               CLOSE                       JYOGAI08-FILE
      *
               ELSE
               CLOSE                       JYOGAIF01-FILE
               CLOSE                       JYOGAIF02-FILE
               OPEN    OUTPUT              JYOGAIF01-FILE
               OPEN    OUTPUT              JYOGAIF02-FILE
               CLOSE                       JYOGAIF01-FILE
               CLOSE                       JYOGAIF02-FILE
               END-IF
           END-IF
      *2024/07
           IF      FLG-CUTOFFARI       =   1
               CLOSE                       CUTOFF01-FILE
               CLOSE                       CUTOFF02-FILE
               OPEN    OUTPUT              CUTOFF01-FILE
               OPEN    OUTPUT              CUTOFF02-FILE
               CLOSE                       CUTOFF01-FILE
               CLOSE                       CUTOFF02-FILE
           END-IF
      *
           IF      CNT-TOKEI       =   ZERO
               GO  TO  200-MAIN-EXT
           END-IF
      *
           OPEN    INPUT               TOKEI-FILE
      *
      *    統計出力処理
           PERFORM 260-TOKEI-OUT-SEC
      *
           CLOSE                       TOKEI-FILE
      *2022/06
      *    除外品の明細作成
           IF    ((WRK-PARA-MEISAIKBN  =    2 )  OR
                  (WRK-PARA-CSVKBN     =    2 ))
              AND (FLG-GAIARI          =    1 )
               PERFORM 270-TOKEI-GAI-SEC
           END-IF
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為集計処理
      *****************************************************************
       220-MAIN-SRYACT-SEC         SECTION.
      *
      *    診療行為読み込み
           PERFORM 900-SRYACT-DBSELECT-SEC
      *
           PERFORM UNTIL   FLG-SRYACT  =   1
               IF    (WRK-PARA-NYUGAIKBN   =   0)   OR
                    ((WRK-PARA-NYUGAIKBN   =   1 )  AND
                     (SRY-NYUGAIKBN        =  "1")) OR
                    ((WRK-PARA-NYUGAIKBN   =   2 )  AND
                     (SRY-NYUGAIKBN        =  "2"))
                 EVALUATE    WRK-PARA-TAISYOKBN
      *              全て
                     WHEN    0
      *                  診療会計集計処理
                         PERFORM 230-MAIN-SRYACCT-SEC
      *              投薬
                     WHEN    1
                         IF      SRY-SRYKBN(1:1)     =  "2"
      *                      診療会計集計処理
                             PERFORM 230-MAIN-SRYACCT-SEC
                         END-IF
      *              在宅、投薬、注射
                     WHEN    2
                         IF    ( SRY-SRYKBN(1:1)     =  "2" ) OR
                               ( SRY-SRYKBN(1:1)     =  "3" ) OR
                               ( SRY-SRYKBN          =  "14" )
      *                      診療会計集計処理
                             PERFORM 230-MAIN-SRYACCT-SEC
                         END-IF
                 END-EVALUATE
               END-IF
      *        診療行為読み込み
               PERFORM 900-SRYACT-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
            .
       220-MAIN-SRYACT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計集計処理
      *****************************************************************
       230-MAIN-SRYACCT-SEC           SECTION.
      *
      *    診療会計読み込み
           PERFORM 900-SRYACCT-READ-SEC
           IF      FLG-SRYACCT        =   1
               GO  TO  230-MAIN-SRYACCT-EXT
           END-IF
      *
      *----(04.08.01)--ADD-START---
           IF  ( ACCT-JIHOKBN        = "1" )   AND
               ( ACCT-TEIGENRATE     =  3 OR 4 )
               GO  TO  230-MAIN-SRYACCT-EXT
           END-IF
      *----(04.08.01)--ADD-END-----
      *
           IF      ACCT-HKNCOMBI   NOT =   9999
      *        保険組合せ読み込み
               PERFORM 900-HKNCOMBI-READ-SEC
               IF      FLG-HKNCOMBI       =   1
                   GO  TO  230-MAIN-SRYACCT-EXT
               END-IF
           END-IF
      *2022/06
      *    算定日をマスタ検索日とする
           MOVE    ZERO            TO  WRK-DAY
           PERFORM VARYING     IDX-D  FROM    1   BY  1
                   UNTIL      (IDX-D       >   31 )
                        OR    (WRK-DAY     >   ZERO )
               IF      ACCT-DAY (1 IDX-D)      >   ZERO
                   MOVE    IDX-D               TO  WRK-DAY
               END-IF
           END-PERFORM
           IF      WRK-DAY         =   ZERO 
      *       算定なしは対象外
               GO  TO  230-MAIN-SRYACCT-EXT
           END-IF
      *
      *    院内チェック
           MOVE   ZERO             TO  FLG-OK
           IF    ( ACCT-ZAIKBN     NOT = 1 )   AND
                 ( ACCT-ZAIKBN     NOT = 2 )
               IF    ( ACCT-ZAITEN         =   ZERO )  AND
                    (( ACCT-SRYKBN         =   "21" )  OR
                     ( ACCT-SRYKBN         =   "22" )  OR
                     ( ACCT-SRYKBN         =   "23" )  OR
                     ( ACCT-SRYKBN         =   "14" ))
                   IF    ( SRY-SRYSYUKBN       =   "213")  OR
                         ( SRY-SRYSYUKBN       =   "223")  OR
                         ( SRY-SRYSYUKBN       =   "233")
                       MOVE    1           TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    1           TO  FLG-OK
               END-IF
      *        院内分のみ対象
               IF    ( FLG-OK          =   1 )
      *            主編集処理
                   PERFORM 240-MAIN-HEN-SEC
               END-IF
           END-IF
      *
           .
       230-MAIN-SRYACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主編集処理
      *****************************************************************
       240-MAIN-HEN-SEC            SECTION.
      *
      *
           PERFORM VARYING IDX-S   FROM    1   BY  1
                   UNTIL ( IDX-S   >   5 )
                   OR    ( SRY-SRYCD(IDX-S)    =   SPACE )
      *        薬剤を対象とする
               IF    ( SRY-SRYCD(IDX-S)(1:1)   =   "6" )
                   PERFORM 250-TOKEI-HEN-SEC 
               END-IF
           END-PERFORM
      *
            .
       240-MAIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計編集処理
      *****************************************************************
       250-TOKEI-HEN-SEC           SECTION.
      *
      *    対象外コード
           IF    ( SRY-SRYCD(IDX-S)    =   "620754301" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "620754601" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "613250027" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "610407025" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622071001" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622070901" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622346201" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622307001" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "621490901" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "621491001" )   OR
                 ( SRY-SRYCD(IDX-S)    =   "622676401" )
      *2025/02   イノソリッド配合経腸用半固形剤
              OR ( SRY-SRYCD(IDX-S)    =   "622952701" )
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
           MOVE    SRY-SRYCD(IDX-S)    TO  WRK-SRYCD
      *
      *    点数マスタ検索
           PERFORM 900-TNS-READ-SEC
      *    点数がない場合は対象外
           IF      TNS-TEN             =   ZERO
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
      *    対象外分類
           IF    ( TNS-YAKKAKJNCD(1:3) =   "510" ) OR
                 ( TNS-YAKKAKJNCD(1:3) =   "520" ) OR
                 ( TNS-YAKKAKJNCD(1:3) =   "590" )
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *2022/06
           MOVE    ZERO                TO  FLG-JYOGAI2
      *    除外コード判定(R04年9月診療分まで)
           IF     (WRK-PARA-SYUKEIKBN  =   2    )
      ***    AND  (SRY-SRYYM           <=  "202209" )
      *2022/10    R05年3月分まで
      ****   AND  (SRY-SRYYM           <=  "202303" )
      *2023/04    R05年9月分まで
      ****   AND  (SRY-SRYYM           <=  "202309" )
      *2023/10    R06年3月分まで
      **     AND  (SRY-SRYYM           <=  "202403" )
      *2024/04    R06年9月分まで
      **    AND  (SRY-SRYYM           <=  "202409" )
      *2024/09    R07年3月分まで
      ***    AND  (SRY-SRYYM           <=  "202503" )
      *2025/03    R07年9月分まで
             AND  (SRY-SRYYM           <=  "202509" )
               PERFORM 2501-JYOGAICD-CHK-SEC
           END-IF
      *2024/07
      *    カットオフコード判定(R06年4月診療分から)
           MOVE    ZERO                TO  FLG-CUTOFF
           IF     (FLG-CUTOFFARI       =   1     )
             AND  (SRY-SRYYM           >=  "202404" )
      *2024/09    R07年3月分まで
      **       IF      (SRY-SRYYM           <=  "202503" )
      *2025/03    R07年9月分まで
               IF      (SRY-SRYYM           <=  "202509" )
                   PERFORM 2501-CUTOFFCD-CHK-SEC
               END-IF
           END-IF
      *
      *
      *     IF      FLG-JYOGAI          =   1
      *         GO  TO  250-TOKEI-HEN-EXT
      *     END-IF
      *
      *
      *    点数がない場合は対象外
      *     IF      TNS-TEN             =   ZERO
      *         GO  TO  250-TOKEI-HEN-EXT
      *     END-IF
      *
      *    単位数
           IF      TNS-SSTKIJUNCD(1)   =   ZERO
               MOVE    1                   TO  WRK-TANISU
           ELSE
               MOVE    TNS-SSTKIJUNCD(1)   TO  WRK-TANISU
           END-IF
      *
      *    対象月
           IF      SRY-SRYYM       =   WRK-TUKI(1)
               MOVE    1           TO  IDX-T
           END-IF
           IF      SRY-SRYYM       =   WRK-TUKI(2)
               MOVE    2           TO  IDX-T
           END-IF
           IF      SRY-SRYYM       =   WRK-TUKI(3)
               MOVE    3           TO  IDX-T
           END-IF
      *
           PERFORM VARYING IDX-D FROM  1   BY  1
                   UNTIL   IDX-D >     31
               IF      ACCT-DAY(1 IDX-D)   NOT =   ZERO
      *            医薬品分類マスタ検索
                   PERFORM 900-GECLASS-READ-SEC
                   IF      FLG-GECLASS     =   ZERO
                       MOVE    GECLASS-KOUHATU TO  WRK-KOUHATU
                   ELSE
                       MOVE    ZERO            TO  WRK-KOUHATU
                   END-IF
      *            統計セット処理
                   PERFORM 250-TOKEI-SET-SEC
               END-IF
           END-PERFORM
      *
           .
       250-TOKEI-HEN-EXT.
           EXIT.
      *
      *2022/06
      *****************************************************************
      *    除外コード判定処理
      *****************************************************************
       2501-JYOGAICD-CHK-SEC           SECTION.
      *
           IF      WRK-TUKI(3)         >   "202503"
      *        終了月がR07年３月以降
               PERFORM 25011-JYOGAICDCHK-02-SEC
               GO  TO  2501-JYOGAICD-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-JYOGAI2
           IF      SRY-SRYYM           <=  "202203"
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAI01-YAKKAKJNCD
               READ    JYOGAI01-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
           END-IF
      *
           IF      SRY-SRYYM           >=  "202204"   AND
                                       <=  "202209"
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAI02-YAKKAKJNCD
               READ    JYOGAI02-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
           END-IF
      *
           IF      SRY-SRYYM           >=  "202210"
                                 AND   <=  "202212"
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAI03-YAKKAKJNCD
               READ    JYOGAI03-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
           END-IF
      *2023/04
      *    Ｒ０５年１月から３月
           IF      SRY-SRYYM           >=  "202301"
                                 AND   <=  "202303"
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAI04-YAKKAKJNCD
               READ    JYOGAI04-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ 
           END-IF
      *    Ｒ０５年４月から９月
           IF      SRY-SRYYM           >=  "202304"
                                 AND   <=  "202309"
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAI05-YAKKAKJNCD
               READ    JYOGAI05-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
           END-IF
      *
      *    Ｒ０５年１０月からＲ０６年３月
           IF      SRY-SRYYM           >=  "202310"
                                 AND   <=  "202403"
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAI06-YAKKAKJNCD
               READ    JYOGAI06-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
           END-IF
      *
      *2024/03
      *    Ｒ０６年４月からＲ０６年９月
           IF      SRY-SRYYM           >=  "202404"
                                 AND   <=  "202409"
      *
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAI07-YAKKAKJNCD
               READ    JYOGAI07-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
           END-IF
      *
      *2024/09
      *    Ｒ０６年１０月からＲ０７年３月
           IF      SRY-SRYYM           >=  "202410"
                                 AND   <=  "202503"
      *
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAI08-YAKKAKJNCD
               READ    JYOGAI08-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
           END-IF
      *
           .
       2501-JYOGAICD-CHK-EXT.
           EXIT.
      *
      *2025/03
      *****************************************************************
      *    除外コード判定処理（R０７年４月から）
      *****************************************************************
       25011-JYOGAICDCHK-02-SEC           SECTION.
      *
      *    Ｒ０６年１０月からＲ０７年３月
           IF      SRY-SRYYM           >=  "202410"
                                 AND   <=  "202503"
      *
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAIF01-YAKKAKJNCD
               READ    JYOGAIF01-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
      *
           END-IF
      *    Ｒ０７年４月からＲ０７年９月
           IF      SRY-SRYYM           >=  "202504"
                                 AND   <=  "202509"
      *
               MOVE    TNS-YAKKAKJNCD      TO  JYOGAIF02-YAKKAKJNCD
               READ    JYOGAIF02-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-JYOGAI2
                   NOT INVALID
                       MOVE    1           TO  FLG-JYOGAI2
               END-READ
           END-IF
      *
           .
       25011-JYOGAICDCHK-02-EXT.
           EXIT.
      *
      *2024/06
      *****************************************************************
      *    カットオフ値セット処理
      *****************************************************************
       2501-CUTOFFCD-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CUTOFF
           IF      SRY-SRYYM           >=  "202404"
                                 AND   <=  "202409"
      *        カットオフ対象
               MOVE    TNS-YAKKAKJNCD      TO  CUTOFF01-YAKKAKJNCD
               READ    CUTOFF01-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-CUTOFF
                   NOT INVALID
                       MOVE    1           TO  FLG-CUTOFF
               END-READ
           END-IF
      *2024/09
           IF      SRY-SRYYM           >=  "202410"
                                 AND   <=  "202503"
      *        カットオフ対象
               MOVE    TNS-YAKKAKJNCD      TO  CUTOFF02-YAKKAKJNCD
               READ    CUTOFF02-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-CUTOFF
                   NOT INVALID
                       MOVE    1           TO  FLG-CUTOFF
               END-READ
           END-IF
      *2025/03
           IF      SRY-SRYYM           >=  "202504"
                                 AND   <=  "202509"
      *        カットオフ対象
               MOVE    TNS-YAKKAKJNCD      TO  CUTOFF03-YAKKAKJNCD
               READ    CUTOFF03-FILE
                   INVALID
                       MOVE    ZERO        TO  FLG-CUTOFF
                   NOT INVALID
                       MOVE    1           TO  FLG-CUTOFF
      *2025/06
      *                薬品コード対象外
                       IF      WRK-SRYCD       =   "622030102"
                                               OR  "622276201"
                           MOVE    ZERO        TO  FLG-CUTOFF
                       END-IF
               END-READ
      *
           END-IF
      *
           .
       2501-CUTOFFCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       250-TOKEI-SET-SEC           SECTION.
      *
           COMPUTE WRK-SURYO       =   SRY-SRYSURYO(IDX-S)
                                   *   ACCT-DAY(1 IDX-D)
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
           MOVE    TNS-YAKKAKJNCD  TO  TOKEI-YAKKAKJNCD
           MOVE    TNS-NAME        TO  TOKEI-NAME
           MOVE    WRK-KOUHATU     TO  TOKEI-KOUHATU
           MOVE    WRK-SRYCD       TO  TOKEI-SRYCD
           PERFORM 900-TOKEI-READ-SEC
      *
           IF      FLG-TOKEI       =   1
      *2022/06
               INITIALIZE                  TOKEI-REC
               MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
               MOVE    TNS-YAKKAKJNCD  TO  TOKEI-YAKKAKJNCD
               MOVE    TNS-NAME        TO  TOKEI-NAME
               MOVE    WRK-KOUHATU     TO  TOKEI-KOUHATU
               MOVE    WRK-SRYCD       TO  TOKEI-SRYCD
               MOVE    TNS-TANINAME    TO  TOKEI-TANINAME
               MOVE    WRK-TANISU      TO  TOKEI-TANISU
               MOVE    ZERO            TO  TOKEI-SRYSURYO(1)
                                           TOKEI-SRYSURYO(2)
                                           TOKEI-SRYSURYO(3)
      *2022/06
               IF      FLG-JYOGAI2     =   ZERO
                   MOVE    WRK-SURYO       TO  TOKEI-SRYSURYO(IDX-T)
               ELSE
                   MOVE    WRK-SURYO       TO  TOKEI-GAI-SRYSURYO(IDX-T)
               END-IF
      *2024/07
      *        カットオフ対象
               IF     (FLG-CUTOFF      =   1   )
                 AND  (FLG-JYOGAI2     =   ZERO)
                   MOVE    WRK-SURYO       TO  TOKEI-OFF-SRYSURYO(IDX-T)
                   MOVE    "1"             TO  TOKEI-OFF-TUKI-KBN
               END-IF
      *
               WRITE   TOKEI-REC
               COMPUTE CNT-TOKEI       =   CNT-TOKEI   +   1
           ELSE
      *2022/06
               IF      FLG-JYOGAI2     =   ZERO
               COMPUTE TOKEI-SRYSURYO(IDX-T)
                   =   TOKEI-SRYSURYO(IDX-T)   +   WRK-SURYO
      *
               ELSE
               COMPUTE TOKEI-GAI-SRYSURYO(IDX-T)
                   =   TOKEI-GAI-SRYSURYO(IDX-T)   +   WRK-SURYO
               END-IF
      *2024/07
      *        カットオフ対象
               IF     (FLG-CUTOFF      =   1   )
                 AND  (FLG-JYOGAI2     =   ZERO)
                   COMPUTE TOKEI-OFF-SRYSURYO(IDX-T)
                       =   TOKEI-OFF-SRYSURYO(IDX-T)   +   WRK-SURYO
                   MOVE    "1"             TO  TOKEI-OFF-TUKI-KBN
               END-IF
      *
               REWRITE TOKEI-REC
           END-IF
      *
           .
       250-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計出力処理
      *****************************************************************
       260-TOKEI-OUT-SEC           SECTION.
      *
      *    薬剤区分毎に印刷する
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
           MOVE    ZERO            TO  WRK-GOUKEI-G
           MOVE    ZERO            TO  WRK-KIKAKU-G
      *
           INITIALIZE                  WRK-G-GOUKEI-G
           INITIALIZE                  WRK-G-KIKAKU-G
      *2024/07
           INITIALIZE                  WRK-O-KIKAKU-G
           INITIALIZE                  WRK-O-GOUKEI-G
           MOVE    ZERO            TO  FLG-GAIARI
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *
      *    見出編集
           PERFORM 310-PRT-HEAD-SET-SEC
           PERFORM 410-CSV-HEAD-SET-SEC
      *
           PERFORM UNTIL ( FLG-TOKEI           =   1  )
      *        統計規格処理
               PERFORM 260-TOKEI-KIKAKU-SEC
      *2022/06
      *        対象あり
               IF      WRK-KIKAKU-G    NOT =   ZERO
      *            明細編集
                   PERFORM 320-PRT-BODY-SET-SEC
                   PERFORM 420-CSV-BODY-SET-SEC
               END-IF
      *        統計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    割合編集
           PERFORM 320-PRT-WARIAI-SET-SEC
      *
             .
       260-TOKEI-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計規格処理
      *****************************************************************
       260-TOKEI-KIKAKU-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-KIKAKU-G
      *2022/06
           MOVE    ZERO            TO  WRK-G-KIKAKU-G
           INITIALIZE                  WRK-O-KIKAKU-G
      *
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   3
      *        単位数量
               COMPUTE WRK-KIKAKU(IDX-T)   =   TOKEI-SRYSURYO(IDX-T)
                                           /   TOKEI-TANISU
               COMPUTE WRK-KIKAKU(4)       =   WRK-KIKAKU(4)
                                           +   WRK-KIKAKU(IDX-T)
      *        全体
               COMPUTE WRK-KBN-ALL(IDX-T)
                   =   WRK-KBN-ALL(IDX-T)  +   WRK-KIKAKU(IDX-T)
               COMPUTE WRK-KBN-ALL(4)
                   =   WRK-KBN-ALL(4)      +   WRK-KIKAKU(IDX-T)
      *        先発品と後発品
               IF    ( TOKEI-KOUHATU   =   2 )   OR
                     ( TOKEI-KOUHATU   =   3 )
                    COMPUTE WRK-KBN-MIX(IDX-T)
                       =   WRK-KBN-MIX(IDX-T)  +   WRK-KIKAKU(IDX-T)
                   COMPUTE WRK-KBN-MIX(4)
                       =   WRK-KBN-MIX(4)      +   WRK-KIKAKU(IDX-T)
               END-IF
      *        後発品のみ
               IF      TOKEI-KOUHATU   =   3
                   COMPUTE WRK-KBN-KOU(IDX-T)
                       =   WRK-KBN-KOU(IDX-T)  +   WRK-KIKAKU(IDX-T)
                   COMPUTE WRK-KBN-KOU(4)
                       =   WRK-KBN-KOU(4)      +   WRK-KIKAKU(IDX-T)
               END-IF
      *2024/07
      *    カットオフ値対応
               IF    ( TOKEI-KOUHATU   =   2 )   OR
                     ( TOKEI-KOUHATU   =   3 )
                   CONTINUE
               ELSE
      *            後発・先発以外を集計
                   IF      TOKEI-OFF-SRYSURYO (IDX-T)    >   ZERO
                       COMPUTE WRK-O-KIKAKU(IDX-T)
                                           =   TOKEI-OFF-SRYSURYO(IDX-T)
                                           /   TOKEI-TANISU
                       COMPUTE WRK-O-KIKAKU(4) 
                                           =   WRK-O-KIKAKU(4)
                                           +   WRK-O-KIKAKU(IDX-T)
                       COMPUTE WRK-OFF-MIX(IDX-T)
                       =   WRK-OFF-MIX(IDX-T)  +   WRK-O-KIKAKU(IDX-T)
                       COMPUTE WRK-OFF-MIX(4)
                       =   WRK-OFF-MIX(4)      +   WRK-O-KIKAKU(IDX-T)
                   END-IF
               END-IF
      *2022/06
      *    対象外集計
               IF      TOKEI-GAI-SRYSURYO (IDX-T)  >   ZERO
                   MOVE    1               TO  FLG-GAIARI
               END-IF
      *        単位数量
               COMPUTE WRK-G-KIKAKU(IDX-T) =   TOKEI-GAI-SRYSURYO(IDX-T)
                                           /   TOKEI-TANISU
               COMPUTE WRK-G-KIKAKU(4)     =   WRK-G-KIKAKU(4)
                                           +   WRK-G-KIKAKU(IDX-T)
      *        全体
               COMPUTE WRK-GKBN-ALL(IDX-T)
                   =   WRK-GKBN-ALL(IDX-T)  +   WRK-G-KIKAKU(IDX-T)
               COMPUTE WRK-GKBN-ALL(4)
                   =   WRK-GKBN-ALL(4)      +   WRK-G-KIKAKU(IDX-T)
      *        先発品と後発品
               IF    ( TOKEI-KOUHATU   =   2 )   OR
                     ( TOKEI-KOUHATU   =   3 )
                    COMPUTE WRK-GKBN-MIX(IDX-T)
                       =   WRK-GKBN-MIX(IDX-T)  +   WRK-G-KIKAKU(IDX-T)
                   COMPUTE WRK-GKBN-MIX(4)
                       =   WRK-GKBN-MIX(4)      +   WRK-G-KIKAKU(IDX-T)
               END-IF
      *        後発品のみ
               IF      TOKEI-KOUHATU   =   3
                   COMPUTE WRK-GKBN-KOU(IDX-T)
                       =   WRK-GKBN-KOU(IDX-T)  +   WRK-G-KIKAKU(IDX-T)
                   COMPUTE WRK-GKBN-KOU(4)
                       =   WRK-GKBN-KOU(4)      +   WRK-G-KIKAKU(IDX-T)
               END-IF
      *
           END-PERFORM
      *
           .
       260-TOKEI-KIKAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理見出し
      *****************************************************************
       310-PRT-HEAD-SET-SEC        SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    SPACE           TO  HCMG007
      *
           MOVE    WRK-PRTYMD      TO  HCMG007-PRTYMD
      *
           MOVE    CNT-PAGE        TO  WRK-Z3
           MOVE    WRK-Z3          TO  HCMG007-PAGE
      *
           MOVE    WRK-KIKAN       TO  HCMG007-KIKAN
           MOVE    WRK-TAISYO      TO  HCMG007-TAISYO
      *
           MOVE    CSV-HEAD-TUKI(1)    TO  HCMG007-LBL(1)
           MOVE    CSV-HEAD-TUKI(2)    TO  HCMG007-LBL(2)
           MOVE    CSV-HEAD-TUKI(3)    TO  HCMG007-LBL(3)
      *
           .
       310-PRT-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理明細
      *****************************************************************
       320-PRT-BODY-SET-SEC        SECTION.
      *
           IF      WRK-PARA-MEISAIKBN  =   0
               GO  TO  320-PRT-BODY-SET-EXT
           END-IF
      *
           IF      CNT-LINE        =   51
      *        印刷処理
               PERFORM 330-PRINT-OUT-SEC
      *        見出編集
               PERFORM 310-PRT-HEAD-SET-SEC
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           EVALUATE    TOKEI-YKZKBN
               WHEN    "1"
                   MOVE    "内"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "2"
                   MOVE    "他"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "4"
                   MOVE    "注"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "6"
                   MOVE    "外"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "8"
                   MOVE    "歯"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "9"
                   MOVE    "特"        TO  HCMG007-YKZKBN(CNT-LINE)
           END-EVALUATE
      *
           MOVE    TOKEI-SRYCD     TO  HCMG007-SRYCD(CNT-LINE)
           MOVE    TOKEI-NAME      TO  HCMG007-NAME(CNT-LINE)
           MOVE    TOKEI-KOUHATU   TO  HCMG007-KOUHATU(CNT-LINE)
           MOVE    TOKEI-YAKKAKJNCD
                                   TO  HCMG007-YAKKAKJNCD(CNT-LINE)
      *
           MOVE    TOKEI-TANISU    TO  WRK-Z4
           MOVE    WRK-Z4          TO  HCMG007-TANISU(CNT-LINE)
      *
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-KIKAKU(IDX-T)   TO  WRK-ZZ
               MOVE    WRK-ZZ      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
           .
       320-PRT-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理割合
      *****************************************************************
       320-PRT-WARIAI-SET-SEC      SECTION.
      *
           IF      WRK-PARA-MEISAIKBN  =   1
                                       OR  2
      *        印刷処理
               PERFORM 330-PRINT-OUT-SEC
      *        見出編集
               PERFORM 310-PRT-HEAD-SET-SEC
           END-IF
      *
      *    全体
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    "全医薬品の規格単位数量　　　　　　　…（１）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-KBN-ALL(IDX-T)   TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    先発品と後発品
           COMPUTE CNT-LINE    =   CNT-LINE    +   2
           MOVE    "後発医薬品あり先発医薬品"
                                       TO  HCMG007-NAME(CNT-LINE)
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    "及び後発医薬品の規格単位数量　　　　…（２）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-KBN-MIX(IDX-T)  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    後発品
           COMPUTE CNT-LINE    =   CNT-LINE    +   2
           MOVE    "後発医薬品の規格単位数量　　　　　　…（３）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-KBN-KOU(IDX-T)  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    置換え率
           COMPUTE CNT-LINE    =   CNT-LINE    +   2
           MOVE    "カットオフ値の割合（２／１）（％）　…（４）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    ZERO        TO  WRK-WARIAI
      ****     IF    ( WRK-KBN-ALL(IDX-T)  >   ZERO ) AND
      *               ( WRK-KBN-MIX(IDX-T)  >   ZERO )
      *             COMPUTE WRK-WARIAI
      *                 = ( WRK-KBN-MIX(IDX-T)  /   WRK-KBN-ALL(IDX-T) )
      *                 *   100
      ****         END-IF
      *2022/06
      *        対象外を集計する
               COMPUTE WRK-ALL         =   WRK-KBN-ALL (IDX-T) 
                                       +   WRK-GKBN-ALL(IDX-T) 
               COMPUTE WRK-MIX         =   WRK-KBN-MIX (IDX-T) 
                                       +   WRK-GKBN-MIX(IDX-T) 
      *2024/07 カットオフ値追加
                                       +    WRK-OFF-MIX(IDX-T)
      *
               IF    ( WRK-ALL             >   ZERO ) AND
                     ( WRK-MIX             >   ZERO )
                   COMPUTE WRK-WARIAI
                       = ( WRK-MIX         /   WRK-ALL  )
                       *   100
               END-IF
      ***
               MOVE    WRK-WARIAI  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *    置換え率
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    "後発医薬品の割合　（３／２）（％）　…（５）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    ZERO        TO  WRK-WARIAI
               IF    ( WRK-KBN-MIX(IDX-T)  >   ZERO ) AND
                     ( WRK-KBN-KOU(IDX-T)  >   ZERO )
                   COMPUTE WRK-WARIAI
                       = ( WRK-KBN-KOU(IDX-T)  /   WRK-KBN-MIX(IDX-T) )
                       *   100
               END-IF
               MOVE    WRK-WARIAI  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
      *TEST
      *TEST
           IF      FLG-CUTOFFARI   =   1
           COMPUTE CNT-LINE    =   CNT-LINE    +   4
           MOVE    "　　　（カットオフ値の割合（２）に含む数量）"
                                       TO  HCMG007-NAME(CNT-LINE)
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-OFF-MIX(IDX-T)  TO  WRK-Z7
               MOVE    WRK-Z7      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
           END-IF
      *TEST
      *
      *    印刷処理
           PERFORM 330-PRINT-OUT-SEC
      *
           .
       320-PRT-WARIAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-SYORIYM    TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMG007.red"       TO  SPRT-PRTID
           MOVE    "後発医薬品数量シェア（置換え率）"
                                       TO  SPRT-TITLE
           MOVE    HCMG007             TO  SPRT-PRTDATA
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
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       410-CSV-HEAD-SET-SEC        SECTION.
      *
           IF      WRK-PARA-CSVKBN     =   ZERO
               GO  TO  410-CSV-HEAD-SET-EXT
           END-IF
      *
           INITIALIZE                  WRK-REC
      *    改行コード編集
           IF      WRK-PARA-CSVKBN     =   1
      *2024/07
      *        カットオフなし
               IF      FLG-CUTOFFARI       =   ZERO
               STRING  CSV-HEAD-01         DELIMITED  BY  SIZE
                       WRK-KAIGYO          DELIMITED  BY  SIZE
                       INTO                WRK-REC
               END-STRING
               ELSE
      *        カットオフあり
               STRING  CSV-HEAD-012        DELIMITED  BY  SIZE
                       WRK-KAIGYO          DELIMITED  BY  SIZE
                       INTO                WRK-REC
               END-STRING
               END-IF
      *2022/06
           ELSE
      *2024/07
      *        カットオフなし
               IF      FLG-CUTOFFARI       =   ZERO
               STRING  CSV-HEAD-02         DELIMITED  BY  SIZE
                       WRK-KAIGYO          DELIMITED  BY  SIZE
                       INTO                WRK-REC
               END-STRING
               ELSE
      *        カットオフあり
               STRING  CSV-HEAD-022        DELIMITED  BY  SIZE
                       WRK-KAIGYO          DELIMITED  BY  SIZE
                       INTO                WRK-REC
               END-STRING
               END-IF
           END-IF
      *
           PERFORM 430-TOUKEICSV-SEC
      *
           .
       410-CSV-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       420-CSV-BODY-SET-SEC        SECTION.
      *
           IF      WRK-PARA-CSVKBN     =   ZERO
               GO  TO  420-CSV-BODY-SET-EXT
           END-IF
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    薬剤区分
           MOVE    TOKEI-YKZKBN    TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    診療行為コード
           MOVE    TOKEI-SRYCD     TO  WRK-DATA
           MOVE    9               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    薬剤名称
           MOVE    TOKEI-NAME      TO  WRK-DATA
           MOVE    50              TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    後発区分
           MOVE    TOKEI-KOUHATU   TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    薬価基準コード
           MOVE    TOKEI-YAKKAKJNCD    TO  WRK-DATA
           MOVE    12              TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    規格単位
           MOVE    TOKEI-TANISU    TO  WRK-DATA
           MOVE    4               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    規格単位数量
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
      *        単位数量
               MOVE    WRK-KIKAKU(IDX-T)   TO  WRK-DATA
               MOVE    7               TO  WRK-NAGASA
               PERFORM 800-MOJI-HENSYU-SEC
           END-PERFORM
      *
      *    除外分追加
           IF      WRK-PARA-CSVKBN     =   2
               MOVE    SPACE           TO  WRK-DATA
               MOVE    2               TO  WRK-NAGASA
               IF      FLG-CUTOFFARI       =   ZERO
                   MOVE    1               TO  WRK-RECEND
               END-IF
               PERFORM 800-MOJI-HENSYU-SEC
           END-IF
      *2024/07
      *    カットオフ対象
           IF      FLG-CUTOFFARI       =   1
               IF      TOKEI-OFF-TUKI-KBN  =   "1"
                   MOVE    "**"            TO  WRK-DATA
               ELSE
                   MOVE    SPACE           TO  WRK-DATA
               END-IF
               MOVE    2               TO  WRK-NAGASA
               MOVE    1               TO  WRK-RECEND
               PERFORM 800-MOJI-HENSYU-SEC
           END-IF
      *
      *    改行コード編集
           MOVE    SPACE               TO  WRK-REC02
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC02
           END-STRING
           MOVE    WRK-REC02           TO  WRK-REC
      *
      *
           PERFORM 430-TOUKEICSV-SEC
      *
           .
       420-CSV-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       430-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                      ORCSTOUKEICSVAREA
           MOVE    "INS"               TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                       TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  STOUKEICSV-SHELLID
           MOVE    WRK-PARA-SYORIYM    TO  STOUKEICSV-SRYYM
           MOVE    SPACE               TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC             TO  STOUKEICSV-CSVDATA
           MOVE    "後発医薬品数量シェア（置換え率）ＣＳＶ作成"
                                       TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"         USING
                                           ORCSTOUKEICSVAREA
                                           SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       430-TOUKEICSV-EXT.
           EXIT.
      *
      *2022/06
      *****************************************************************
      *    対象外の明細作成 統計出力処理
      *****************************************************************
       270-TOKEI-GAI-SEC           SECTION.
      *
           OPEN    INPUT               TOKEI-FILE
      *
      *    薬剤区分毎に印刷する
      ***  MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
           INITIALIZE                  TOKEI-REC
      *    統計ファイル読み込み
           PERFORM 900-TOKEI-NEXT-SEC
      *TEST
           MOVE    "（除外品）"       TO  WRK-TAISYO
      *
      *    見出編集
           IF      WRK-PARA-MEISAIKBN     =   2
               PERFORM 310-PRT-HEAD-SET-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-TOKEI           =   1  )
      *        対象外データ
               PERFORM 260-TOKEI-KIKAKU-SEC
      *        明細編集
               IF      WRK-G-KIKAKU-G  NOT =   ZERO
                   PERFORM 320-PRT-BODY-SET2-SEC
                   PERFORM 420-CSV-BODY-SET2-SEC
               END-IF
      *        統計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
           IF      CNT-LINE            >   ZERO
      *        印刷処理
               PERFORM 330-PRINT-OUT-SEC
            END-IF
      *
           CLOSE                       TOKEI-FILE
      *
             .
       270-TOKEI-GAI-EXT.
            EXIT.
      *
      *****************************************************************
      *    帳票編集処理明細
      *****************************************************************
       320-PRT-BODY-SET2-SEC        SECTION.
      *
           IF      WRK-PARA-MEISAIKBN  NOT =   2
               GO  TO  320-PRT-BODY-SET2-EXT
           END-IF
      *
           IF      CNT-LINE        =   51
      *        印刷処理
               PERFORM 330-PRINT-OUT-SEC
      *        見出編集
               PERFORM 310-PRT-HEAD-SET-SEC
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           EVALUATE    TOKEI-YKZKBN
               WHEN    "1"
                   MOVE    "内"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "2"
                   MOVE    "他"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "4"
                   MOVE    "注"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "6"
                   MOVE    "外"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "8"
                   MOVE    "歯"        TO  HCMG007-YKZKBN(CNT-LINE)
               WHEN    "9"
                   MOVE    "特"        TO  HCMG007-YKZKBN(CNT-LINE)
           END-EVALUATE
      *
           MOVE    TOKEI-SRYCD     TO  HCMG007-SRYCD(CNT-LINE)
           MOVE    TOKEI-NAME      TO  HCMG007-NAME(CNT-LINE)
           MOVE    TOKEI-KOUHATU   TO  HCMG007-KOUHATU(CNT-LINE)
           MOVE    TOKEI-YAKKAKJNCD
                                   TO  HCMG007-YAKKAKJNCD(CNT-LINE)
      *
           MOVE    TOKEI-TANISU    TO  WRK-Z4
           MOVE    WRK-Z4          TO  HCMG007-TANISU(CNT-LINE)
      *
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
               MOVE    WRK-G-KIKAKU(IDX-T)   TO  WRK-ZZ
               MOVE    WRK-ZZ      TO  HCMG007-KIKAKU(CNT-LINE IDX-T)
           END-PERFORM
      *
           .
       320-PRT-BODY-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       420-CSV-BODY-SET2-SEC        SECTION.
      *
           IF      WRK-PARA-CSVKBN NOT =   2
               GO  TO  420-CSV-BODY-SET2-EXT
           END-IF
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    薬剤区分
           MOVE    TOKEI-YKZKBN    TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    診療行為コード
           MOVE    TOKEI-SRYCD     TO  WRK-DATA
           MOVE    9               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    薬剤名称
           MOVE    TOKEI-NAME      TO  WRK-DATA
           MOVE    50              TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    後発区分
           MOVE    TOKEI-KOUHATU   TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    薬価基準コード
           MOVE    TOKEI-YAKKAKJNCD    TO  WRK-DATA
           MOVE    12              TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    規格単位
           MOVE    TOKEI-TANISU    TO  WRK-DATA
           MOVE    4               TO  WRK-NAGASA
           PERFORM 800-MOJI-HENSYU-SEC
      *
      *    規格単位数量
           PERFORM VARYING IDX-T   FROM    1   BY  1
                   UNTIL   IDX-T   >   4
      *        単位数量
               MOVE    WRK-G-KIKAKU(IDX-T)   TO  WRK-DATA
               MOVE    7               TO  WRK-NAGASA
               PERFORM 800-MOJI-HENSYU-SEC
           END-PERFORM
      *
      *    除外区分
           MOVE    "**"            TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           IF      FLG-CUTOFFARI       =   ZERO
               MOVE    1               TO  WRK-RECEND
           END-IF
           PERFORM 800-MOJI-HENSYU-SEC
      *2024/07
      *    カットオフ対象
           IF      FLG-CUTOFFARI       =   1
               IF      TOKEI-OFF-TUKI-KBN  =   "1"
                   MOVE    "**"            TO  WRK-DATA
               ELSE
                   MOVE    SPACE           TO  WRK-DATA
               END-IF
               MOVE    2               TO  WRK-NAGASA
               MOVE    1               TO  WRK-RECEND
               PERFORM 800-MOJI-HENSYU-SEC
           END-IF
      *
      *
      *    改行コード編集
           MOVE    SPACE               TO  WRK-REC02
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC02
           END-STRING
      *
           PERFORM 430-TOUKEICSV-SEC
      *
           .
       420-CSV-BODY-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
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
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       800-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
               CONTINUE
           ELSE                   
               PERFORM VARYING IDX-W   FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDX-W   <       1
                   IF      WRK-DATA (IDX-W:1)  NOT =   SPACE
                       MOVE    SPACE               TO  WRK-REC02
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA(1:IDX-W)
                                                   TO  WRK-REC02
                       ELSE
                           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                   DELIMITED   BY  SIZE
                               WRK-DATA(1:IDX-W)   DELIMITED   BY  SIZE
                               INTO    WRK-REC02
                           END-STRING
                       END-IF
                       MOVE    WRK-REC02   TO  WRK-REC
                       ADD     IDX-W       TO  WRK-REC-LENGTH
                       MOVE    1           TO  IDX-W
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-RECEND         =   ZERO
               MOVE    SPACE              TO  WRK-REC02
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC02
               END-STRING
               MOVE    WRK-REC02           TO  WRK-REC
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           MOVE    ZERO                TO  WRK-NUM    
      *
           .
       800-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
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
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
grpsys         MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG026 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *2025/03     R07.3 まで
           IF      WRK-TUKI(3)         <=  "202503"
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    JYOGAI01PARA    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           MOVE    ZERO            TO  IF-RESULT
           MOVE    JYOGAI02PARA    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    JYOGAI03PARA    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           MOVE    ZERO            TO  IF-RESULT
           MOVE    JYOGAI04PARA    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           MOVE    ZERO            TO  IF-RESULT
           MOVE    JYOGAI05PARA    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    JYOGAI06PARA    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    JYOGAI07PARA    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    ZERO            TO  IF-RESULT
           MOVE    JYOGAI08PARA    TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           ELSE
      *        ２月分
               MOVE    ZERO            TO  IF-RESULT
               MOVE    JYOGAIF01PARA   TO  IF-FILENAME
               CALL    "orcfiledel"        USING
                                           ORCSFDELAREA
      *
               MOVE    ZERO            TO  IF-RESULT
               MOVE    JYOGAIF02PARA   TO  IF-FILENAME
               CALL    "orcfiledel"        USING
                                           ORCSFDELAREA
           END-IF
      *
           IF      FLG-CUTOFFARI   =   1
               MOVE    ZERO            TO  IF-RESULT
               MOVE    CUTOFF01PARA    TO  IF-FILENAME
               CALL    "orcfiledel"        USING
                                           ORCSFDELAREA
               MOVE    ZERO            TO  IF-RESULT
               MOVE    CUTOFF02PARA    TO  IF-FILENAME
               CALL    "orcfiledel"        USING
                                           ORCSFDELAREA
               MOVE    ZERO            TO  IF-RESULT
               MOVE    CUTOFF03PARA    TO  IF-FILENAME
               CALL    "orcfiledel"        USING
                                           ORCSFDELAREA
           END-IF
      *
           IF      CNT-TOKEI       =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG026 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG026 END ***"
      *     
      *    ステップ管理終了処理
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
      *    統計ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1           TO  FLG-TOKEI
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
      *
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC          SECTION.
      *
           READ    TOKEI-FILE      NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
      *
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBSELECT-SEC     SECTION.
      *
      *    対象診療年月で検索
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM     TO  SRY-HOSPNUM
           MOVE    WRK-TUKI(1)     TO  SRY-SRYYM
           MOVE    WRK-TUKI(3)     TO  SRY-CREYMD
           MOVE    "%"             TO  SRY-NYUGAIKBN
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ検索
      *****************************************************************
       900-SRYACT-DBFETCH-SEC      SECTION.
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ検索
      *****************************************************************
       900-SRYACCT-READ-SEC        SECTION.
      *
      *    該当の診療会計データを検索
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID        TO  ACCT-PTID
           MOVE    SRY-SRYKA       TO  ACCT-SRYKA
           MOVE    SRY-SRYYM       TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN      TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ検索
      *****************************************************************
       900-HKNCOMBI-READ-SEC        SECTION.
      *
      *    該当の診療会計データを検索
           INITIALIZE                  HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM    TO  COMB-HOSPNUM
           MOVE    ACCT-PTID       TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI   TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-HKNCOMBI
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
      *
               IF    ( COMB-HKNNUM(1:2)    =   "98" ) OR
                     ( COMB-HKNNUM     =   "975" ) OR
                     ( COMB-HKNNUM     =   "973" ) OR
                     ( COMB-HKNNUM     =   "971" )
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索
      *****************************************************************
       900-TNS-READ-SEC            SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    SRY-SRYYM           TO  ORC-DBYMD(1:6)
      *****MOVE    "01"                TO  ORC-DBYMD(7:2)
           MOVE    WRK-DAY             TO  ORC-DBYMD(7:2)
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    ORC-DBYMD           TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF      TTNS-FLG        =   ZERO
               INITIALIZE                  TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD  TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
R0706 *            令和７年４月から薬価基準コードをリセット
R0706              PERFORM 900-TNS-RESET-SEC
                   MOVE    ZERO            TO  FLG-TENSU
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-TNS-READ-EXT.
           EXIT.
      *
      *****************************************************************
R0706 *    点数マスタメンテ
R0706 *    令和７年４月から薬価基準コードをリセット
      *****************************************************************
       900-TNS-RESET-SEC           SECTION.
      *
           IF      TNS-YUKOSTYMD       >=  "20250401"
               EVALUATE    TNS-YAKKAKJNCD
                   WHEN    "2649729S2185"
                       MOVE    "2649729S2010"      TO  TNS-YAKKAKJNCD
                   WHEN    "2649729S3092"
                       MOVE    "2649729S3017"      TO  TNS-YAKKAKJNCD
                   WHEN    "2649735S2237"
                   WHEN    "2649735S2261"
                       MOVE    "2649735S2016"      TO  TNS-YAKKAKJNCD
                   WHEN    OTHER
                       CONTINUE
               END-EVALUATE
           END-IF
      *
           .
       900-TNS-RESET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  TTNS-FLG
           INITIALIZE                  TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                   OR    ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD(TTNS-IDX)
                                               =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD(TTNS-IDX)
                                               <=  TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD(TTNS-IDX)
                                               >=  TTNS-CHK-YMD )
                   MOVE    1                   TO  TTNS-FLG
                   MOVE    TTNS-DAT(TTNS-IDX)  TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC           SECTION.
      *
           IF      TTNS-CONST-MAX  >   TTNS-MAX
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD(TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD(TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD(TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT(TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医薬品分類マスタ検索
      *****************************************************************
       900-GECLASS-READ-SEC        SECTION.
      *
           INITIALIZE                  GECLASS-REC
           MOVE    TNS-YAKKAKJNCD  TO  GECLASS-YAKKAKJNCD
           MOVE    SRY-SRYYM       TO  GECLASS-YUKOSTYMD(1:6)
           MOVE    IDX-D           TO  GECLASS-YUKOSTYMD(7:2)
           MOVE    SRY-SRYYM       TO  GECLASS-YUKOEDYMD(1:6)
           MOVE    IDX-D           TO  GECLASS-YUKOEDYMD(7:2)
           MOVE    GECLASS-REC     TO  MCPDATA-REC
           MOVE    "tbl_generic_class"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  GECLASS-REC
               MOVE    ZERO            TO  FLG-GECLASS
           ELSE
               MOVE    1               TO  FLG-GECLASS
           END-IF
      *
           MOVE    "tbl_generic_class"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           .
       900-GECLASS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
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
      *    ＤＢＦＥＴＣＨ処理
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
      *    ＤＢＣＬＯＳＥ処理
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
      *    ＤＢ　オープン処理
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
      *    ＤＢ　クローズ処理
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
