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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBSETSAIKETU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ツール
      *  コンポーネント名  : 検査採血区分一括設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/01/17    NACL-伊藤     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  05.01.00    NMED-多々納   21/03/03  月次統計対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：検査採血区分設定データ
      **** SELECT  IN-FILE         ASSIGN  "saiketukbn.csv"
           SELECT  IN-FILE         ASSIGN  IN-FILENAME
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  IN-FILE.
       01  IN-REC.
           03  IN-SRYCD                PIC X(09).
           03  IN-COMMA                PIC X(01).
           03  IN-KBN                  PIC 9(01).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    一覧（縦）
           COPY    "HCMSL80.INC".
      *    一覧（横）
      **   COPY    "HCMSL55.INC".
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
      *    ＣＳＶデータ
       01  ACC-AREA.
           03  IN-FILENAME             PIC X(64).
       01  WRK-FILE-NAME.
           03  FILLER                  PIC X(21)   VALUE
               "/usr/lib/jma-receipt/".
           03  FILLER                  PIC X(20)   VALUE
               "data/saiketukbn.csv".
       01  WRK-FILE-NAME-PACHI.
           03  FILLER                  PIC X(31)   VALUE
               "/usr/lib/jma-receipt/patch-lib/".
           03  FILLER                  PIC X(20)   VALUE
               "data/saiketukbn.csv".
      *
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
      *
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-IN                  PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
      *
           03  FLG-HZN                 PIC 9(01).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *****03  RECEERR                 PIC X(100).
           03  WRK-PARA-MSTDATE        PIC X(08).
      *
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(05).
           03  CNT-ADD                 PIC 9(05).
           03  CNT-UPD                 PIC 9(05).
           03  CNT-DIFF                PIC 9(05).
           03  CNT-SAME                PIC 9(05).
           03  CNT-SKIP                PIC 9(05).
      *
           03  CNT-NUM                 PIC 9(05).
           03  CNT-PAGE                PIC 9(05).
           03  CNT-LINE                PIC 9(02).
      *
       01  WRK-AREA.
      *    エラー情報
           03  WRK-RECEERR             PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-SYSTIME             PIC X(08).
      *
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-CNT.
               05  WRK-CNT-I           PIC ZZZZ9.
      *
           03  WRK-ERRCD               PIC X(01).
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
      * 確認リスト
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1.
               05  FILLER              PIC X(02)   VALUE   SPACE.
               05  FILLER              PIC X(40)   VALUE
                   "＃＃　採血料区分設定処理リスト　＃＃".
               05  PRT-MIDASI          PIC X(14)   VALUE
                   "マスター日付：".
               05  PRT-SRYYMD          PIC X(22).
               05  FILLER              PIC X(04)   VALUE
                   SPACE.
               05  FILLER              PIC X(05)   VALUE   SPACE.
               05  PRT-SYSYMD          PIC X(10)   VALUE   SPACE.
               05  PRT-TIME            PIC X(07)   VALUE   SPACE.
               05  PRT-PAGE            PIC ZZZ9.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  LST-PRT-HEAD2.
               05  FILLER              PIC X(13)   VALUE
                   "検査コード".
               05  FILLER              PIC X(50)   VALUE   SPACE.
               05  FILLER              PIC X(01)   VALUE   SPACE.
               05  FILLER              PIC X(50)   VALUE
                   "採血区分".
      *
           03  LST-PRT-BODY-01.
               05  PRT-SYRCD           PIC X(09).
               05  PRT-FIL1            PIC X(02).
               05  PRT-NAME            PIC X(50).
               05  PRT-FIL2            PIC X(01).
               05  PRT-SAIKETUKBN      PIC X(01).
               05  PRT-FIL3            PIC X(01).
               05  PRT-ERRMSG          PIC X(40).
      *
           03  LST-PRT-BODY-98.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(14)   VALUE
                   "入力件数　　：".
               05  PRT-CNT-IN          PIC ZZZZ9.
               05  FILLER              PIC X(06)   VALUE   "件，".
               05  FILLER              PIC X(14)   VALUE
                   "追加件数　　：".
               05  PRT-CNT-ADD         PIC ZZZZ9.
               05  FILLER              PIC X(06)   VALUE   "件，".
               05  FILLER              PIC X(14)   VALUE
                   "更新件数　　：".
               05  PRT-CNT-UPD         PIC ZZZZ9.
               05  FILLER              PIC X(04)   VALUE   "件".
      *
           03  LST-PRT-BODY-99.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(14)   VALUE
                   "区分相違件数：".
               05  PRT-CNT-DIFF        PIC ZZZZ9.
               05  FILLER              PIC X(06)   VALUE    "件，".
               05  FILLER              PIC X(14)   VALUE
                   "区分同一件数：".
               05  PRT-CNT-SAME        PIC ZZZZ9.
               05  FILLER              PIC X(06)   VALUE    "件，".
               05  FILLER              PIC X(14)   VALUE
                   "スキップ件数：".
               05  PRT-CNT-SKIP        PIC ZZZZ9.
               05  FILLER              PIC X(04)   VALUE    "件".
      *
      *
       01  MAX-LINE           PIC 9(03)   VALUE   80.
      *
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  FILLER          PIC X(01)   VALUE   "(".
               05  WRK-THH         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-TMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ")".
      *
       01  HZN-TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC"  REPLACING
                                     //TNSP-// BY //HZN-TNSP-//.
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
           COPY    "CPTENSU.INC".
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "COMMON-SPA".
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM.
      ****************************************************************
      *    制御処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
           PERFORM 200-MAIN-SEC
                   UNTIL ( FLG-IN  =   1 )
                      OR ( FLG-END =   1 )
           PERFORM 300-TERM-SEC
           STOP    RUN
           .
      ****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           DISPLAY "*** ORCBSETSAIKETU START ***"
      *
           INITIALIZE  STS-AREA
           INITIALIZE  FLG-AREA
           INITIALIZE  PARA-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  WRK-AREA
      *
      *    UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
      *                            INTO    PARA-HOSPNUM
      *                                    PARA-MSTDATE
      *    END-UNSTRING
      ***  MOVE    PARA-HOSPNUM        TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    パラメタセット
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
                                               WRK-PARA-MSTDATE
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    一時ファイル名取得
           INITIALIZE  SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBSETSAIKETU"    TO  JOB-PGID
           MOVE    "採血料区分設定処理"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *
           PERFORM 800-HIZUKE-SET-SEC
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
      *    パッチを検索
           MOVE    WRK-FILE-NAME-PACHI     TO  IN-FILENAME
           OPEN    INPUT   IN-FILE
           IF      STS-IN          NOT =   ZERO
               CLOSE   IN-FILE
      *        パッチなし
               MOVE    WRK-FILE-NAME           TO  IN-FILENAME
               OPEN    INPUT   IN-FILE
               IF      STS-IN          NOT =   ZERO
                   MOVE    1                   TO  FLG-IN
                   DISPLAY "*(ORCBSETSAIKETU)* IN-FILE OPEN STATUS ["
                           STS-IN "]"
               END-IF
           END-IF
      *
           IF      FLG-IN              =   ZERO
               PERFORM 900-IN-READ-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  WRK-SYSYMD
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
      *
      *    処理日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SYSYMD
      *
      *    処理時間
           MOVE    SMCNDATE-HMS(1:2)   TO  WRK-THH
           MOVE    SMCNDATE-HMS(3:2)   TO  WRK-TMM
      * 
           MOVE    WRK-HENTIME         TO  PRT-TIME
      *
      *    診療日
           IF      WRK-PARA-MSTDATE    =   SPACE
               MOVE    SPA-SYSYMD      TO  WRK-PARA-MSTDATE
           END-IF
      *
      *    処理日
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-PARA-MSTDATE    TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                       TO  PRT-SRYYMD
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      ****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-STYMD
           MOVE    SPACE               TO  WRK-EDYMD
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    IN-SRYCD            TO  TNS-SRYCD
           MOVE    WRK-PARA-MSTDATE    TO  TNS-YUKOSTYMD
           MOVE    WRK-PARA-MSTDATE    TO  TNS-YUKOEDYMD
           PERFORM 900-TENSU-READ-SEC
      *
           IF      FLG-TENSU           =   ZERO
               INITIALIZE                  TENSUPLUS-REC
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TNS-SRYCD           TO  TNSP-SRYCD
               MOVE    WRK-PARA-MSTDATE    TO  TNSP-YUKOSTYMD
               MOVE    WRK-PARA-MSTDATE    TO  TNSP-YUKOEDYMD
               PERFORM 900-TENSUPLUS-READ-SEC
               IF      FLG-TENSUPLUS       =   ZERO
                   IF      TNSP-SAIKETUKBN     =   ZERO
                       PERFORM 400-TENSUPLUS-UPDATE-SEC
                   ELSE
                       IF      TNSP-SAIKETUKBN NOT =   IN-KBN
      *                    採血区分不一致
                           MOVE    "2"                 TO  WRK-ERRCD
                           PERFORM 20020-PRINT-SYORI-SEC
      *                    DISPLAY " "
      *                    DISPLAY "採血料区分設定処理 採血区分が違う"
      *                            " [" TNSP-SRYCD "] : DB ["
      *                            TNSP-SAIKETUKBN "] <> CSV ["
      *                            IN-KBN "]"
      *                    DISPLAY " "
                           ADD     1                   TO  CNT-DIFF
                       ELSE
                           ADD     1                   TO  CNT-SAME
                       END-IF
                   END-IF
               ELSE
      *            追加
                   PERFORM 2001-TENSPULS-ADD-CHEK-SEC
               END-IF
           ELSE
               MOVE    "1"                 TO  WRK-ERRCD
               PERFORM 20020-PRINT-SYORI-SEC
      *        DISPLAY " "
      *        DISPLAY "スキップ マスタ廃止？ [" IN-SRYCD "]"
      *        DISPLAY " "
               ADD     1                   TO  CNT-SKIP
           END-IF
      *
           PERFORM 900-IN-READ-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       2001-TENSPULS-ADD-CHEK-SEC       SECTION.
      *
      *    付加情報の登録チェック
           INITIALIZE                  HZN-TENSUPLUS-REC
           MOVE    ZERO                TO  FLG-HZN
           MOVE    SPACE               TO  WRK-ERRCD
      *
           INITIALIZE                  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-TENSUPLUS-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           PERFORM UNTIL       FLG-TENSUPLUS   =   1
      *        点数マスタの有効期間内の付加情報
               IF     (TNSP-YUKOSTYMD      >   TNS-YUKOEDYMD )
                  OR  (TNSP-YUKOEDYMD      <   TNS-YUKOSTYMD )
                   CONTINUE
               ELSE
                   MOVE    TENSUPLUS-REC   TO  HZN-TENSUPLUS-REC
                   MOVE    1               TO  FLG-HZN
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-TENSUPLUS-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HZN             =   1
               IF     HZN-TNSP-YUKOEDYMD   <   WRK-PARA-MSTDATE
      *            最終履歴の翌日を開始日とする
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    HZN-TNSP-YUKOEDYMD  TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY6-KEISAN     TO  WRK-STYMD
                   ELSE
                       MOVE    "3"                 TO  WRK-ERRCD
                   END-IF
                   MOVE    TNS-YUKOEDYMD       TO  WRK-EDYMD
      *            前履歴の情報を複写
                   MOVE    HZN-TENSUPLUS-REC   TO  TENSUPLUS-REC
               ELSE
                   MOVE    "3"                 TO  WRK-ERRCD
               END-IF
               IF      WRK-ERRCD       NOT =   SPACE
                   PERFORM 20020-PRINT-SYORI-SEC
                   ADD     1                   TO  CNT-SKIP
                   GO      TO     2001-TENSPULS-ADD-CHEK-EXT
               END-IF
           ELSE
      *        点数マスタの有効期間
               MOVE    TNS-YUKOSTYMD       TO  WRK-STYMD
               MOVE    TNS-YUKOEDYMD       TO  WRK-EDYMD
      *        新規追加
               MOVE    SPACE               TO  TENSUPLUS-REC
               INITIALIZE                  TENSUPLUS-REC
           END-IF
      *
           PERFORM 400-TENSUPLUS-ADD-SEC
           .
       2001-TENSPULS-ADD-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       20020-PRINT-SYORI-SEC       SECTION.
      *
      *    明細編集
           MOVE    SPACE               TO  LST-PRT-BODY-01
      *
           IF      CNT-LINE            >=  MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           IF      CNT-LINE            =   ZERO
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
      *
           MOVE    IN-SRYCD            TO  PRT-SYRCD
           MOVE    IN-KBN              TO  PRT-SAIKETUKBN
           EVALUATE    WRK-ERRCD
           WHEN    "1"
               MOVE    "【スキップ マスタ廃止？】"
                                       TO  PRT-ERRMSG
           WHEN    "2"
               STRING  "【採血区分不一致："
                       TNSP-SAIKETUKBN
                       "】"                DELIMITED  BY  SPACE
                                           INTO  PRT-ERRMSG
               END-STRING
               MOVE    TNS-NAME        TO  PRT-NAME
           WHEN    "3"
               MOVE    "【付加情報有効期間？】"
                                       TO  PRT-ERRMSG
               MOVE    TNS-NAME        TO  PRT-NAME
           END-EVALUATE
           ADD     1                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-01     TO  HCMSL80-MOJI (CNT-LINE)
      *
           .
       20020-PRINT-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       280-HEAD-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  HCMSL80
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  PRT-PAGE
      *
           MOVE    LST-PRT-HEAD1       TO  HCMSL80-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL80-MOJI (3)
           MOVE    ALL "-"             TO  HCMSL80-MOJI (4)
      *
           MOVE    4                   TO  CNT-LINE
           .
       280-HEAD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SEC               SECTION.
      *
           INITIALIZE  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "HCMSL80.red"       TO  SPRT-PRTID
           MOVE    "採血料区分設定処理"
                                       TO  SPRT-TITLE
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"       USING   ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-EXIT.
           EXIT.
      *
      *****************************************************************
      *    処理件数印刷編集処理
      *****************************************************************
       2009-END-PRINT-SYORI-SEC       SECTION.
      *
      *    明細編集
           IF     (CNT-LINE + 6)       >=  MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           IF      CNT-LINE            =   ZERO
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
      *
           MOVE    CNT-IN              TO  PRT-CNT-IN
           MOVE    CNT-ADD             TO  PRT-CNT-ADD
           MOVE    CNT-UPD             TO  PRT-CNT-UPD
           MOVE    CNT-DIFF            TO  PRT-CNT-DIFF
           MOVE    CNT-SAME            TO  PRT-CNT-SAME
           MOVE    CNT-SKIP            TO  PRT-CNT-SKIP
           ADD     4                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-98     TO  HCMSL80-MOJI (CNT-LINE)
           ADD     2                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-99     TO  HCMSL80-MOJI (CNT-LINE)
      *
           PERFORM 400-PRINT-SEC
           .
       2009-END-PRINT-SYORI-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
            DISPLAY "*** ORCBSETSAIKETU END ***"
      *
           IF      CNT-IN              =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               PERFORM 2009-END-PRINT-SYORI-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           CLOSE   IN-FILE
      *
      *    DISPLAY " "
      *    MOVE    CNT-IN              TO  WRK-CNT-I
      *    DISPLAY "採血料区分設定処理 入力件数     : [ " WRK-CNT " ]"
      *    MOVE    CNT-ADD             TO  WRK-CNT-I
      *    DISPLAY "                   追加件数     : [ " WRK-CNT " ]"
      *    MOVE    CNT-UPD             TO  WRK-CNT-I
      *    DISPLAY "                   更新件数     : [ " WRK-CNT " ]"
      *    MOVE    CNT-DIFF            TO  WRK-CNT-I
      *    DISPLAY "                   区分相違件数 : [ " WRK-CNT " ]"
      *    MOVE    CNT-SAME            TO  WRK-CNT-I
      *    DISPLAY "                   区分同一件数 : [ " WRK-CNT " ]"
      *    MOVE    CNT-SKIP            TO  WRK-CNT-I
      *    DISPLAY "                   スキップ件数 : [ " WRK-CNT " ]"
      *    DISPLAY " "
      *
      *    DISPLAY " "
      *    DISPLAY "採血料区分設定処理 終了"
      **   DISPLAY " "
           .
       300-TERM-EXT.
           EXIT.
      *****************************************************************
      *    点数付加追加処理
      *****************************************************************
       400-TENSUPLUS-ADD-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-STYMD           TO  TNSP-YUKOSTYMD
           MOVE    WRK-EDYMD           TO  TNSP-YUKOEDYMD
           MOVE    IN-KBN              TO  TNSP-SAIKETUKBN
      *
           MOVE    SMCNDATE-YMD        TO  TNSP-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           ADD     1                   TO  CNT-ADD
           .
       400-TENSUPLUS-ADD-EXT.
           EXIT.
      *****************************************************************
      *    点数付加更新処理
      *****************************************************************
       400-TENSUPLUS-UPDATE-SEC    SECTION.
      *
           MOVE    IN-KBN              TO  TNSP-SAIKETUKBN
      *
           MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
           MOVE    TNSP-KEY            TO  TNSP-UP-KEY
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "saiketuupd"        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           ADD     1                   TO  CNT-UPD
           .
       400-TENSUPLUS-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    日時編集処理
      *****************************************************************
       800-HIZUKE-SET-SEC          SECTION.
      *
      *    ACCEPT  WRK-SYMD2       FROM    DATE
      *    COMPUTE WRK-SYY             =   WRK-SYY2    +   2000
      *    MOVE    WRK-SMM2            TO  WRK-SMM
      *    MOVE    WRK-SDD2            TO  WRK-SDD
      *
      **** ACCEPT  WRK-SYSTIME     FROM    TIME
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       800-HIZUKE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力データ読込処理
      *****************************************************************
       900-IN-READ-SEC             SECTION.
      *
           READ    IN-FILE
               AT  END
                   MOVE    1                   TO  FLG-IN
               NOT AT  END
                   MOVE    ZERO                TO  FLG-IN
                   ADD     1                   TO  CNT-IN
           END-READ
           .
       900-IN-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数付加読込処理
      *****************************************************************
       900-TENSUPLUS-READ-SEC      SECTION.
      *
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-TENSUPLUS-READ-EXT.
           EXIT.
           .
      *****************************************************************
      *    点数付加読込処理
      *****************************************************************
       900-TENSUPLUS-NEXT-SEC      SECTION.
      *
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
               MOVE    ZERO                TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE  TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           .
       900-TENSUPLUS-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
