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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI102R1V2.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入力・診療コード情報取得
      *  管理者            :
      *  作成日付    作業者        記述
      *  24/11/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.02.00     ORCAMO       24/12/XX  診療コード内容取得対応
      *****************************************************************
      *
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       INPUT-OUTPUT          SECTION.
      *
       FILE-CONTROL.
       DATA                DIVISION.
      *FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-INPUTCD             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-RECEKISAI           PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
           03  WRK-ERRCD-G.
               05  WRK-ERRCD-1         PIC X(01).
               05  WRK-ERRCD-2         PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-ZZ9                 PIC ZZZ9.
      *
           03  WRK-CNT                 PIC 9(04).
      *    リクエスト番号
           03  WRK-REQUEST-NUMBER      PIC X(02).
      *
           03  WRK-ICD-CDSYU           PIC X(01).
      *
           03  WRK-SRYCD               PIC X(09).
      *
       01  WRK-XML-AREA.
           03  WRK-IN-DATE                PIC X(10).
      *
           03  WRK-IN-YMD                 PIC X(08).
      *    リクエストコード
           03  WRK-REQUEST-CODE           PIC X(20).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    API XML読み込み用定義体
           COPY    "CPMEDICATIONGETV2REQ.INC".
      *    API XML書き込み用定義体
           COPY    "CPMEDICATIONGETV2RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1010.INC".
      *
      *    入力ＣＤ
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    点数マスタ
           COPY     "CPTENSU.INC".
      *
      *    レセプト記載要領マスター
       01  RECEKISAI-REC.
           COPY    "CPRECEKISAI.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "************************"
           DISPLAY   "* medicationget   start*"
           DISPLAY   "************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *
           IF      WRK-ERRCD           =   "99"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST52-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-PTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "***********************"
           DISPLAY   "* medicationget   end *"
           DISPLAY   "***********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-MEDICATIONGETREQ
           INITIALIZE                      XML-MEDICATIONGETRES
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  MEDIRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MEDIRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  MEDIRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
           END-IF
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-APTLSTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           EVALUATE    MEDIREQ-REQUEST-NUMBER
           WHEN    "01"
      *        入力コード
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        選択式コメント一覧
               MOVE    2                   TO  FLG-SYORI
           WHEN    "03"
      *        診療コード内容
               MOVE    3                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    一覧処理
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    MEDIREQ-REQUEST-NUMBER
               WHEN    "01"
      *            入力コードから点数マスタ検索
                   PERFORM 2002-INPUTCD-SYORI-SEC
               WHEN    "02"
      *            選択式コメント一覧
                   PERFORM 2003-RECEKISAI-SYORI-SEC
               WHEN    "03"
      *            診療コードから点数マスタ検索
                   PERFORM 2004-SYRCD-SYORI-SEC
               END-EVALUATE
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST52-BODY       NOT =   LOW-VALUE
               DISPLAY "MEDIREQ-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
      *
      * XML情報取り出し
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_medicationgetv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST52-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "medicationgetreq"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-MEDICATIONGETREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               MOVE   "98"             TO  WRK-ERRCD
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      MEDIREQ-REQUEST-CODE    =   LOW-VALUE
               MOVE    SPACE           TO  MEDIREQ-REQUEST-CODE
           END-IF
           IF      MEDIREQ-BASE-DATE   =   LOW-VALUE
               MOVE    SPACE           TO  MEDIREQ-BASE-DATE
           END-IF
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    設定内容チェック
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    基準日チェック
           IF    ( WRK-ERRCD           =   SPACE )
               PERFORM 20011-BASE-DATE-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC             SECTION.
      *
           MOVE    MEDIREQ-BASE-DATE   TO  WRK-IN-DATE
      *    処理日
           MOVE    MEDIREQ-BASE-DATE   TO  WRK-HEN-DATE
           MOVE    SPACE               TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD            TO  WRK-IN-YMD
      *    開始日未設定はシステム日付
           IF      WRK-IN-DATE         =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-IN-YMD
               MOVE    WRK-IN-YMD          TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-IN-DATE
           END-IF
           MOVE    WRK-IN-DATE         TO  MEDIRES-BASE-DATE
      *
      *    リクエストコード
           MOVE    MEDIREQ-REQUEST-CODE    TO  MEDIRES-REQUEST-CODE
           IF      MEDIREQ-REQUEST-CODE    =   SPACE
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック処理
      *****************************************************************
       20011-BASE-DATE-CHK-SEC             SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-IN-YMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "02"                TO  WRK-ERRCD
           END-IF
           .
       20011-BASE-DATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ検索処理
      *****************************************************************
       2002-INPUTCD-SYORI-SEC             SECTION.
      *
      *    リクエストＣＤチェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    MEDIREQ-REQUEST-CODE
                                       TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-REQUEST-CODE
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "10"                TO  WRK-ERRCD
           ELSE
      *        間に空白がある時
               IF      KANACHK-RC2         =   8
                   MOVE    "10"            TO  WRK-ERRCD
               END-IF
               MOVE    KANACHK-MAX         TO  WRK-CNT
           END-IF
           MOVE    WRK-REQUEST-CODE    TO  MEDIRES-REQUEST-CODE
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2002-INPUTCD-SYORI-EXT
           END-IF
      *
           INITIALIZE                   MEDIRES-MEDICATION-INF
      *
           MOVE    SPACE               TO  WRK-ICD-CDSYU
      *    全角
           IF      KANACHK-SYUBETU     =   2
      *        全角のコードチェック
               MOVE    "J"             TO  WRK-ICD-CDSYU
           ELSE
      *        診療コードチェック
               IF      WRK-REQUEST-CODE(1:1)   =   "P"  OR  "S"
                   MOVE    "6"             TO  WRK-ICD-CDSYU
               ELSE
                   IF     (WRK-CNT         >   1   AND  <=  6 )  AND
                          (WRK-REQUEST-CODE(1:WRK-CNT)  NUMERIC  )
                       EVALUATE    WRK-CNT
      *                    ４桁
                           WHEN    4
                               MOVE    "4"         TO  WRK-ICD-CDSYU
      *                    ５桁
                           WHEN    5
                               MOVE    "5"         TO  WRK-ICD-CDSYU
      *                    ６桁
                           WHEN    6
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *
                           WHEN    OTHER
                               MOVE    "A"         TO  WRK-ICD-CDSYU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  WRK-ICD-CDSYU
                   END-IF
               END-IF
           END-IF
      *
      *    入力コード存在チェック
           IF     WRK-ICD-CDSYU        =   SPACE
      *        検索不可
               MOVE    "10"            TO  WRK-ERRCD
           ELSE
               MOVE    SPACE               TO  INPUTCD-REC
               INITIALIZE                      INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    WRK-ICD-CDSYU       TO  ICD-CDSYU
               MOVE    WRK-REQUEST-CODE    TO  ICD-INPUTCD
      *
               PERFORM 930-INPUTCD-READ-SEC
               IF      FLG-INPUTCD         =   ZERO
                   PERFORM 20021-INPUTCD-HEN-SEC
               ELSE
      *            登録なし
                   MOVE    "11"            TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2002-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤから点数マスタ情報編集処理
      *****************************************************************
       20021-INPUTCD-HEN-SEC               SECTION.
      *
           IF      ICD-SRYCD(1:1)      =   "P"
                                       OR  "S"
      *        セットコード
               MOVE    ICD-SRYCD           TO  MEDIRES-MEDICATION-CODE
      *        セット名称
               MOVE    ICD-DSPNAME         TO  MEDIRES-MEDICATION-NAME
      *        警告
               MOVE    "13"                TO  WRK-KEICD
               GO  TO  20021-INPUTCD-HEN-EXT
           END-IF
      *    点数マスタ検索
           INITIALIZE                      TNS-TENSU-REC
           MOVE    ICD-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-IN-YMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-IN-YMD          TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-KEYREAD-SEC
           IF      FLG-TENSU               =   ZERO
      *        基準日でマスタあり
               PERFORM 200211-TENSU-HEN-SEC
           ELSE
      *        最終履歴の検索
               PERFORM 20022-TENSU-RRK-SYORI-SEC
           END-IF
           .
       20021-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ履歴情報編集処理
      *****************************************************************
       20022-TENSU-RRK-SYORI-SEC          SECTION.
      *
      *    最終履歴の検索
           INITIALIZE                      TNS-TENSU-REC
           MOVE    ICD-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           PERFORM 911-TENSU-KEYREAD24-SEC
      *
           IF      FLG-TENSU           =   ZERO
               PERFORM 200211-TENSU-HEN-SEC
               MOVE    "14"            TO  WRK-KEICD
           ELSE
      *        対象なし
               MOVE    ICD-SRYCD       TO  MEDIRES-MEDICATION-CODE
               MOVE    "12"            TO  WRK-ERRCD
           END-IF
           .
       20022-TENSU-RRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ情報編集処理
      *****************************************************************
       200211-TENSU-HEN-SEC               SECTION.
      *
           MOVE    TNS-SRYCD           TO  MEDIRES-MEDICATION-CODE
           MOVE    TNS-NAME            TO  MEDIRES-MEDICATION-NAME
           MOVE    TNS-KANANAME        TO  MEDIRES-MEDICATION-KNAME
      *    開始日
           MOVE    TNS-YUKOSTYMD       TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MEDIRES-START-DATE
      *    終了日
           MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MEDIRES-END-DATE
      *    薬剤・器材、単位コードあり
           IF     (TNS-SRYCD (1:1)     =   "6"
                                       OR  "7" )
             OR   (TNS-TANICD      NOT =   SPACE
                                       AND ZERO )
      *        単位コード
               MOVE    TNS-TANICD          TO  MEDIRES-UNIT-CODE
      *        単位名称
               MOVE    TNS-TANINAME        TO  MEDIRES-UNIT-NAME
           END-IF
           .
       200211-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント一覧処理
      *****************************************************************
       2003-RECEKISAI-SYORI-SEC             SECTION.
      *
      *    診療コードチェック
           IF     (MEDIREQ-REQUEST-CODE(1:9)
                                       NUMERIC  )
           AND    (MEDIREQ-REQUEST-CODE(10:)
                                       =   SPACE )
      *        数値９桁であること
               MOVE    MEDIREQ-REQUEST-CODE(1:9)
                                       TO  WRK-SRYCD
           ELSE
               MOVE    "20"            TO  WRK-ERRCD
           END-IF
           MOVE    MEDIREQ-REQUEST-CODE   TO  MEDIRES-REQUEST-CODE
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2003-RECEKISAI-SYORI-EXT
           END-IF
      *
           INITIALIZE                   MEDIRES-MEDICATION-INF
      *
      *    点数マスタ検索
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-IN-YMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-IN-YMD          TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-KEYREAD-SEC
           IF      FLG-TENSU               =   ZERO
      *        基準日でマスタあり
               PERFORM 200211-TENSU-HEN-SEC
           ELSE
      *        最終履歴の検索
               INITIALIZE                      TNS-TENSU-REC
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               PERFORM 911-TENSU-KEYREAD24-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 200211-TENSU-HEN-SEC
                   MOVE    "14"            TO  WRK-KEICD
               ELSE
                   MOVE    "21"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  RECEKISAI-REC
           INITIALIZE                      RECEKISAI-REC
      *    診療コード
           MOVE    WRK-SRYCD           TO  RKISAI-SRYCD
      *    開始年月
           MOVE    WRK-IN-YMD(1:6)     TO  RKISAI-STYM
           MOVE    WRK-IN-YMD(1:6)     TO  RKISAI-EDYM
      *
           MOVE    RECEKISAI-REC       TO  MCPDATA-REC
           MOVE    "tbl_recekisai"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 9103-RECEKISAI-READ-SEC
           ELSE
               INITIALIZE                      RECEKISAI-REC
               MOVE    1                   TO  FLG-RECEKISAI
           END-IF
           MOVE    ZERO                TO  IDX
           IF      FLG-TENSU           =   1
               IF      FLG-RECEKISAI       =   ZERO
      *        点数マスタがなく、レセ記載コメントがある時
                   MOVE    SPACE           TO  WRK-ERRCD
      *            診療行為名称等
                   MOVE    SPACE           TO  MEDIRES-MEDICATION-CODE
                   MOVE    SPACE           TO  MEDIRES-MEDICATION-NAME
               END-IF
           END-IF
      *
           PERFORM
               UNTIL      (IDX             >   200 )
                     OR  (FLG-RECEKISAI   =   1    )
               INITIALIZE                  TNS-TENSU-REC
      *        コメントコード
               MOVE    RKISAI-C-CD         TO  TNS-SRYCD
               MOVE    WRK-IN-YMD          TO  TNS-YUKOSTYMD
               MOVE    WRK-IN-YMD          TO  TNS-YUKOEDYMD
               PERFORM 910-TENSU-KEYREAD-SEC
               IF      FLG-TENSU           =   ZERO
      *            選択コメント編集
                   PERFORM 20031-RECECOMMNT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_recekisai"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 9103-RECEKISAI-READ-SEC
           END-PERFORM
           MOVE    "tbl_recekisai"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "23"            TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2003-RECEKISAI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    選択コメント編集処理
      *****************************************************************
       20031-RECECOMMNT-HEN-SEC       SECTION.
      *
           ADD     1                   TO  IDX 
           IF      IDX                 >   200
               MOVE   "24"             TO  WRK-KEICD
               GO  TO  20031-RECECOMMNT-HEN-EXT
           END-IF
           MOVE    RKISAI-C-CD         TO  MEDIRES-RKISAI-CODE (IDX)
           MOVE    TNS-NAME            TO  MEDIRES-RKISAI-NAME (IDX)
      *    項番
           MOVE    RKISAI-ITEMNO       TO  MEDIRES-RKISAI-ITEMNO (IDX)
      *    枝番
           MOVE    RKISAI-EDANO        TO  MEDIRES-RKISAI-EDANO (IDX)
      *    区分
           MOVE    RKISAI-KUBUN        TO  MEDIRES-RKISAI-KUBUN (IDX)
      *    条件区分
           MOVE    RKISAI-JYOKEN       TO  MEDIRES-RKISAI-JYOKEN (IDX)
      *    非算定理由コメント
           MOVE    RKISAI-HISANTEI     TO  MEDIRES-RKISAI-HISANTEI
                                                              (IDX)
      *    処理区分
           MOVE    RKISAI-SYORIKBN     TO  MEDIRES-RKISAI-SYORIKBN
                                                              (IDX)
      *    診療行為名称等
           MOVE    RKISAI-MEISYOETC
                                       TO  MEDIRES-RKISAI-MEISYOETC
                                                              (IDX)
           .
       20031-RECECOMMNT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択式コメント一覧処理
      *****************************************************************
       2004-SYRCD-SYORI-SEC             SECTION.
      *
      *    診療コードチェック
           IF     (MEDIREQ-REQUEST-CODE(1:9)
                                       NUMERIC  )
           AND    (MEDIREQ-REQUEST-CODE(10:)
                                       =   SPACE )
      *        数値９桁であること
               MOVE    MEDIREQ-REQUEST-CODE(1:9)
                                       TO  WRK-SRYCD
           ELSE
               MOVE    "20"            TO  WRK-ERRCD
           END-IF
           MOVE    MEDIREQ-REQUEST-CODE   TO  MEDIRES-REQUEST-CODE
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2004-SYRCD-SYORI-EXT
           END-IF
      *
           INITIALIZE                   MEDIRES-MEDICATION-INF
      *
      *    点数マスタ検索
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-IN-YMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-IN-YMD          TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-KEYREAD-SEC
           IF      FLG-TENSU               =   ZERO
      *        基準日でマスタあり
               PERFORM 200211-TENSU-HEN-SEC
           ELSE
      *        最終履歴の検索
               INITIALIZE                      TNS-TENSU-REC
               MOVE    WRK-SRYCD           TO  TNS-SRYCD
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               PERFORM 911-TENSU-KEYREAD24-SEC
               IF      FLG-TENSU           =   ZERO
                   PERFORM 200211-TENSU-HEN-SEC
                   MOVE    "22"            TO  WRK-ERRCD
               ELSE
                   MOVE    "25"            TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2004-SYRCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-PTXMLMAKE-SEC               SECTION.
      *
           MOVE    SPACE               TO   WRK-ERRCD-G
      *    エラーメッセージ編集
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
      *        ワーニングメッセージ
               IF      WRK-KEICD       NOT =   SPACE
                   MOVE    "W"                 TO  WRK-ERRCD-1
                   MOVE    WRK-KEICD           TO  WRK-ERRCD-2
               END-IF
           ELSE
               MOVE    "E"                 TO  WRK-ERRCD-1
               MOVE    WRK-ERRCD           TO  WRK-ERRCD-2
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "処理終了"          TO  MEDIRES-API-RESULT-MSG
               MOVE    ZERO                TO  MEDIRES-API-RESULT
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD-G         TO  MEDIRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  MEDIRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST52-BODY     NOT =   LOW-VALUE
               DISPLAY "MEDIRES-OBJECT not low_value"
           END-IF
      *
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_medicationgetv2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  MEDIRES-MODE
           MOVE    "medicationgetres"  TO  MEDIRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-MEDICATIONGETRES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               MOVE    MEDIRES-OBJECT      TO  APILST52-BODY 
               MOVE    "application/xml"   TO  APILST52-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       300-PTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
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
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "01"
               MOVE    "リクエストコードの設定がありません。"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "基準日が暦日ではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "入力コードではありません。"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "入力コードの登録はありません。"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               STRING  "入力コードに設定されている"
                       "診療コードは点数マスタに登録されていません。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "13"
               STRING  "セットコードです。"
                       "セット名称を返却します。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "14"
               STRING  "警告！"
                       "診療コードは基準日で有効期間外です。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
      *    リクエスト＝０２
           WHEN    "20"
               STRING  "診療行為コードは数値９桁で設定して下さい。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "21"
               STRING  "診療行為コードは点数マスタ、"
                       "レセプト特記事項テーブルに存在しません。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "22"
               STRING  "診療行為コードは基準日で有効期間外です。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "23"
               STRING  "選択式コメントが存在しません。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
           WHEN    "24"
               STRING  "警告！"
                       "対象の選択式コメントが２００件以上"
                       "存在します。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
      *    リクエスト＝０３
           WHEN    "25"
               STRING  "診療行為コードは点数マスタに存在しません。"
                                               DELIMITED   BY  SIZE
                                               INTO  WRK-ERRMSG
               END-STRING
      *共通エラー
           WHEN    "89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
      ****     MOVE    "処理区分未設定"        TO  WRK-ERRMSG
               MOVE    "リクエスト番号がありません"
                                               TO  WRK-ERRMSG
           WHEN    "97"
               MOVE    "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE    "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤ未登録。"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   MOVE    SPACE               TO  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-KEYREAD-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込 KEY24
      *****************************************************************
       911-TENSU-KEYREAD24-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       911-TENSU-KEYREAD24-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       9001-SYSKANRI-KEYREAD-SEC            SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                      SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9001-SYSKANRI-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC            SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター次読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       9103-RECEKISAI-READ-SEC            SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECEKISAI
               MOVE    MCPDATA-REC         TO  RECEKISAI-REC
           ELSE
               MOVE    1                   TO  FLG-RECEKISAI
               INITIALIZE                      RECEKISAI-REC
           END-IF
      *
           .
       9103-RECEKISAI-READ-EXT.
           EXIT.
           
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
