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
       PROGRAM-ID.                 ORCGW031.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 施設基準情報　マイナ保険証利用率（Ｗ０３１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  24/09/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "W03SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET              PIC X(64).
           03  WRK-WIDMSG              PIC X(80).
           03  WRK-SDATE               PIC X(06).
           03  WRK-STIME.
               05  WRK-SHH              PIC 9(02).
               05  WRK-SMM              PIC 9(02).
               05  WRK-SSS              PIC 9(02).
           03  WRK-TUKI                PIC X(02).
      *
           03  WRK-SELNUM              PIC 9(04).
           03  WRK-ZZ9                 PIC ZZZ9.
           03  WRK-ZZZZ                PIC ZZZZ.
           03  WRK-MIDASI              PIC X(100).
           03  WRK-MIDASI2             PIC X(20).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *    コラムリスト位置
           03  WRK-W031-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-YMD                 PIC X(09). 
           03  WRK-H-STYMD             PIC X(09).
           03  WRK-H-EDYMD             PIC X(09).
           03  WRK-YUKOSTYMD           PIC X(08).
           03  WRK-YUKOEDYMD           PIC X(08).
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
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    施設基準 マイナ保険証利用率
           COPY  "CPSK1060.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W03
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W03         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
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
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      SPA-WIDCD       NOT =   SPACE
                   PERFORM 520-WIDSET-SEC
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "W031"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
      *            終了日変更登録
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2401-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W031-CUR
                   END-IF
               WHEN    "1003"
      *            削除処理
                   MOVE    SPACE           TO  SPA-WIDCD
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 2402-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-W031-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE           TO  SPA-WID1-FLG
      *
           .
       330-WID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-W031-AREA
      *
           MOVE    SPA-SYSYMD (1:4)    TO  SPA-W031-NEN
      *
           PERFORM 3101-SPA-TBLHEN-SEC
           MOVE    1                   TO  SPA-W031-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       3101-SPA-TBLHEN-SEC                  SECTION.
      *
      *    対象年の１月１日から１２月３１日
           MOVE    SPA-W031-NEN        TO  SPA-W031-SYY
           MOVE    "0101"              TO  SPA-W031-SMMDD
           MOVE    SPA-W031-NEN        TO  SPA-W031-EYY
           MOVE    "1231"              TO  SPA-W031-EMMDD
      *
           MOVE    SPA-W031-STYUKYMD   TO  WRK-YMD
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3(1:10)  TO  SPA-W031-GMN-NEN
      *     PERFORM 5002-HIZUKE-CHK-SEC
      *     IF      FLG-ERR             =   ZERO
      *         MOVE    WRK-HENYMDG(1:3)    TO  SPA-W031-GMN-NEN
      *     END-IF
      *
           INITIALIZE                  SPA-W031-RITU-G
           MOVE    ZERO                TO  SPA-W031-KEIFLG
      *    マイナ保険証利用率情報
           MOVE    SPACE               TO  SYS-1060-REC
           INITIALIZE                      SYS-1060-REC
           MOVE    "1060"              TO  SYS-1060-KANRICD
           MOVE    "403501"            TO  SYS-1060-KBNCD
           MOVE    SPA-W031-STYUKYMD   TO  SYS-1060-STYUKYMD
           MOVE    SPA-W031-STYUKYMD   TO  SYS-1060-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1060-HOSPNUM
      *
           MOVE    SYS-1060-REC      TO  SYSKANRI-REC
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
      *        更新
               MOVE    SYSKANRI-REC        TO  SYS-1060-REC
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL      IDX      >   12
                   MOVE    SYS-1060-RIYORITU (IDX)
                                           TO  SPA-W031-RITU (IDX)
               END-PERFORM
               MOVE    ZERO                TO  SPA-W031-SYORIKBN
           ELSE
      *        新規
               MOVE    1               TO  SPA-W031-SYORIKBN
           END-IF
           .
       3101-SPA-TBLHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *
           MOVE    SPACE               TO  W031
           INITIALIZE                      W031
      *
           MOVE    SPA-W031-GMN-NEN    TO  W031-NEN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   12
               MOVE    SPA-W031-RITU (IDX)
                                       TO  W031-RITU (IDX)
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-W031-CUR        =   ZERO
               EVALUATE    WRK-WIDGET(1:4)
                   WHEN    "RITU"
                       MOVE    WRK-WIDGET(5:2)     TO  WRK-TUKI
                       IF      WRK-TUKI        >=  "01"   AND
                                               <=  "11"
                           MOVE    WRK-TUKI        TO  SPA-W031-CUR
                           ADD     1               TO  SPA-W031-CUR
                       ELSE
                           MOVE    20              TO  SPA-W031-CUR
                       END-IF
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-W031-CUR
               WHEN    1   THRU   12
                   MOVE    SPACE               TO  MCP-WIDGET
                   STRING  "RITU"
                           SPA-W031-CUR    DELIMITED BY  SPACE
                                           INTO  MCP-WIDGET
                   END-STRING
               WHEN    20
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLREA-SEC
      *        前年
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 260-MAE-NEN-SEC
      *        翌年
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 270-NEXT-NEN-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 2120-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-W031-CUR
      *    入力項目の決定
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    画面をＳＰＡへ編集
           PERFORM 2201-GMNSPA-SET-SEC
      *
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2201-GMNSPA-SET-SEC               SECTION.
      *
           INITIALIZE                   SPA-W031-RITU-G
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   12
               MOVE    W031-RITU (IDX)     TO  SPA-W031-RITU (IDX)
           END-PERFORM
           .
       2201-GMNSPA-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLREA-SEC                  SECTION.
      *
           
      *    初期画面表示
           PERFORM 310-SPASET-SEC
           MOVE    1                   TO  SPA-W031-CUR
           .
       220-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *    前年処理
      *****************************************************************
       260-MAE-NEN-SEC                  SECTION.
      *
           IF      SPA-W031-NEN        <=  "2024"
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-W031-NEN        =   SPA-W031-NEN
                                           -   1
               PERFORM 3101-SPA-TBLHEN-SEC
               MOVE    1                   TO  SPA-W031-CUR
           END-IF
      *
           .
       260-MAE-NEN-EXT.
           EXIT.
      *****************************************************************
      *    翌年処理
      *****************************************************************
       270-NEXT-NEN-SEC                SECTION.
      *
           COMPUTE SPA-W031-NEN        =   SPA-W031-NEN
                                       +   1
           PERFORM 3101-SPA-TBLHEN-SEC
           MOVE    1                   TO  SPA-W031-CUR
           .
       270-NEXT-NEN-EXT.
           EXIT.
      *****************************************************************
      *    更新処理
      *****************************************************************
       2120-TOUROKU-SEC                   SECTION.
      *
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *
           IF      (SPA-ERRCD           =   SPACE)
      *        関連チェック
               PERFORM 230-KANREN-CHK-SEC
           END-IF
      *
           .
       2120-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       230-KANREN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI 
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   12
               IF      SPA-W031-RITU (IDX) NOT =   ZERO
                   MOVE    1               TO  FLG-ARI 
               END-IF
           END-PERFORM
      *
           IF      SPA-W031-SYORIKBN   =   1
      *        新規で入力なしはエラー
               IF      FLG-ARI             =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD 
               ELSE
      *            未来日は警告
                   IF      SPA-W031-NEN    >   SPA-SYSYMD(1:4)
                       IF      SPA-W031-KEIFLG =   ZERO
                           MOVE    "K002"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-W031-KEIFLG
                       END-IF
                   END-IF
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "1001"          TO  SPA-WIDCD
               END-IF
           ELSE
      *        すべてゼロは削除
               IF      FLG-ARI             =   ZERO
                   MOVE    "1003"          TO  SPA-WIDCD
               ELSE
                   MOVE    "1001"          TO  SPA-WIDCD
               END-IF
           END-IF
      *test
           DISPLAY "W031 SPA-W031-SYORIKBN:"  SPA-W031-SYORIKBN
            ",FLG-ARI:"  FLG-ARI
            ",SPA-ERRCD:"  SPA-ERRCD
            ",SPA-WIDCD:"  SPA-WIDCD
           .
       230-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       2401-TOUROKU-SYORI-SEC          SECTION.
      *
      *    更新日取得
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
      *    マイナ保険証利用率情報
           MOVE    SPACE               TO  SYS-1060-REC
           INITIALIZE                      SYS-1060-REC
           MOVE    "1060"              TO  SYS-1060-KANRICD
           MOVE    "403501"            TO  SYS-1060-KBNCD
           MOVE    SPA-W031-STYUKYMD   TO  SYS-1060-STYUKYMD
           MOVE    SPA-W031-STYUKYMD   TO  SYS-1060-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1060-HOSPNUM
           MOVE    SYS-1060-REC      TO  SYSKANRI-REC
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
      *
           IF      FLG-SYSKANRI        =   1
      *        新規
               MOVE    SPACE               TO  SYS-1060-REC
               INITIALIZE                      SYS-1060-REC
               MOVE    "1060"              TO  SYS-1060-KANRICD
               MOVE    "403501"            TO  SYS-1060-KBNCD
               MOVE    SPA-W031-STYUKYMD   TO  SYS-1060-STYUKYMD
               MOVE    SPA-W031-EDYUKYMD   TO  SYS-1060-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1060-HOSPNUM
      *
               PERFORM 24011-RITU-HENSYU-SEC
      *
               PERFORM 2401-SYSKANRI-INS-SEC
           ELSE
      *        更新
               MOVE    SYSKANRI-REC        TO  SYS-1060-REC
      *
               PERFORM 24011-RITU-HENSYU-SEC
      *
               PERFORM 2402-SYSKANRI-UPD-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 210-BACK
           END-IF
           .
       2401-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理更新処理
      *****************************************************************
       24011-RITU-HENSYU-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   12
               MOVE    SPA-W031-RITU (IDX)
                                      TO  SYS-1060-RIYORITU (IDX)
           END-PERFORM
            .
       24011-RITU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理更新処理
      *****************************************************************
       2401-SYSKANRI-INS-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  SYS-1060-CREYMD
           MOVE    SMCNDATE-YMD        TO  SYS-1060-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-1060-UPHMS
      *
           MOVE    SPA-OPID            TO  SYS-1060-OPID
      *
           MOVE    SYS-1060-REC      TO  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "W031 DB INS ERR KEY:" SYS-1060-KEY 
           END-IF
      *
      *
           .
       2401-SYSKANRI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理更新処理
      *****************************************************************
       2402-SYSKANRI-UPD-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  SYS-1060-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-1060-UPHMS
      *
           MOVE    SPA-OPID            TO  SYS-1060-OPID
      *
           MOVE    SYS-1060-REC      TO  SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYSUP-HOSPNUM
           MOVE    SYS-KANRICD         TO  SYSUP-KANRICD
           MOVE    SYS-KBNCD           TO  SYSUP-KBNCD
           MOVE    SYS-STYUKYMD        TO  SYSUP-STYUKYMD
           MOVE    SYS-EDYUKYMD        TO  SYSUP-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "W031 DB UPD ERR KEY:" SYS-1060-KEY 
           END-IF
      *
      *
           .
       2402-SYSKANRI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       2402-DELETE-SYORI-SEC          SECTION.
      *
      *    マイナ保険証利用率情報
           MOVE    SPACE               TO  SYS-1060-REC
           INITIALIZE                      SYS-1060-REC
           MOVE    "1060"              TO  SYS-1060-KANRICD
           MOVE    "403501"            TO  SYS-1060-KBNCD
           MOVE    SPA-W031-STYUKYMD   TO  SYS-1060-STYUKYMD
           MOVE    SPA-W031-STYUKYMD   TO  SYS-1060-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1060-HOSPNUM
           MOVE    SYS-1060-REC      TO  SYSKANRI-REC
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "W031 DB DEL ERR KEY:" SYS-1060-KEY 
               END-IF
           END-IF
      *
      *    初期画面表示
           IF      SPA-ERRCD       =   SPACE
               PERFORM 310-SPASET-SEC
           END-IF
           .
       2402-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W03"               TO  SPA-SAKIPG
           MOVE    "W031"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "W03"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "W031"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   STRING  "マイナ保険証利用率は、令和６年からです。"
                           "前年はありません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING  "警告！"
                           "対象年が未来年です。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "利用率の入力がありません。"
                           "登録できません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "登録エラーです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W031"              TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "WERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   STRING  "マイナ保険証利用率を登録します。"
                           "よろしいですか？"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1002"
                   STRING  "マイナ保険証利用率を削除します。"
                           "よろしいですか？"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
               WHEN    "1003"
                   STRING  "利用率の入力がありません。"
                           "マイナ保険証利用率を削除します。"
                           "よろしいですか？"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-WIDMSG
                   END-STRING
          END-EVALUATE
      *
           MOVE    SPACE               TO  WID1
           MOVE    SPA-WIDCD           TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG          TO  WID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "W031"              TO  SPA-MOTOPG
           MOVE    "WID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-ERR
           EVALUATE    WRK-YMD(1:8)
               WHEN    ZERO
                   MOVE    ZERO                TO  WRK-SYMD
                   MOVE    WRK-YMD(1:8)        TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"               TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)          TO  WRK-HENYMDG  
               WHEN    OTHER
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-YMD             TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    ZERO
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    WRK-SYMD            TO  WRK-HENYMDG
               WHEN    OTHER
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-EVALUATE
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
                   MOVE    SPACE               TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               MOVE    SPACE               TO  SYSKANRI-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
            .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               MOVE    SPACE           TO  SYSKANRI-REC
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
