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
       PROGRAM-ID.                 ORCGZ061.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ更新
      *  コンポーネント名  : 用法コード一覧選択サブ（Ｚ０６１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  23/01/XX    ORCAMO        新規作成
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
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    画面ＳＰＡエリア
           COPY    "Z06SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-YOUHOU          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
      *
           03  FLG-NO              PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *
           03  FLG-KIKAN           PIC 9(01).
           03  FLG-KENSAKU01       PIC 9(01).
      *
           03  FLG-INPUTCD-ARI     PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS                 PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
           03  IDX-N               PIC 9(02).
      *
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXC                PIC 9(04).
      *
           03  ID1                 PIC 9(04).
           03  ID2                 PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
      *
           03  IDX-KAI             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SEYMD.
               05  WRK-SEYY        PIC 9(04).
               05  WRK-SEMM        PIC 9(02).
               05  WRK-SEDD        PIC 9(02).
      *
           03  WRK-STR             PIC 9(06).
      *
           03  WRK-CNT-ENT         PIC 9(04).
           03  WRK-MOJI            PIC 9(04).
           03  WRK-MAP-INDEX       PIC 9(03).
           03  WRK-SELNO           PIC 9(03).
           03  WRK-GMN-SELNO       PIC 9(03).
           03  WRK-NOZ             PIC ZZZ.
      *
           03  WRK-MOJI1           PIC X(22).
           03  WRK-MOJI2           PIC X(22).
      *
           03  WRK-INPUTCD         PIC X(22).
      *
      *
           03  WRK-PATH            PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-KETA            PIC 9(04).
           03  WRK-KEY-INPUT       PIC X(50).
      *
           03  WRK-SENTAKU         PIC X(70).
           03  WRK-HEN-NAME        PIC X(70).
           03  WRK-KENSA-HEN       PIC X(02).
      *
           03  WRK-Z061-ROW         PIC S9(09).
           03  FLG-GMN-INIT        PIC  9(01).
      *
           03  WRK-ERRMSG          PIC  X(80).
           03  WRK-ERRMSG2         PIC  X(80).
      *
           03  WRK-NAME1-G.
               05  WRK-NAME11           PIC X(20).
               05  WRK-NAME12           PIC X(50).
           03  WRK-NAME2-G.
               05  WRK-NAME21           PIC X(30).
               05  WRK-NAME22           PIC X(40).
      *H24.4
      *    一般記載区分名称
           03  WRK-NAME                    PIC X(100).
           03  WRK-MSG-2                   PIC X(10).
      *
       01  WRK-HEN-AREA.
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
      *    用法コード
       01  YOUHOU-REC.
           COPY    "CPYOUHOU.INC".
      *    用法コード対応テーブル
       01  YOUHOUCHG-REC.
           COPY    "CPYOUHOUCHG.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *    検索記号設定
           COPY    "CPORCSADDSIGN.INC".
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
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA102SCRAREA.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE        TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA   TO  SPA-Z06
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z06             TO  SPA-Z01SONOTA 
           MOVE    SPA-Z01FREE         TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           PERFORM 5001-MAPCUR-SEC
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "Z061"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    1                   TO  SPA-Z061-PAGE
      *
           MOVE    SPA-Z061-INPUTCD    TO  SPA-Z061-GMN-INPUT
      *
           PERFORM 310-SPA-SET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                      SPA-Z061-LIST-G
           INITIALIZE                      SPA-Z061-SELNUM
      *
      *    開始
           IF     (SPA-Z061-PAGE        =   ZERO )
               MOVE    1                   TO  SPA-Z061-PAGE
           END-IF
           COMPUTE WRK-STR             =  (SPA-Z061-PAGE   -   1  )
                                       *   200
                                       +   1
      *
      *    名称で検索
           IF      SPA-Z061-INPUTCD    =   SPACE
               MOVE    "key4"              TO  WRK-PATH
               MOVE    SPACE               TO  WRK-KEY-INPUT
           ELSE
      *        検索キー編集
               INITIALIZE                  ORCSADDSIGNAREA
               MOVE    "1"                 TO  SADDSIGN-KBN
               MOVE    SPA-Z061-INPUTCD    TO  SADDSIGN-STR
               CALL    "ORCSADDSIGN"       USING
                                           ORCSADDSIGNAREA
               MOVE    SADDSIGN-STR        TO  WRK-KEY-INPUT
      *
               MOVE    "key3"              TO  WRK-PATH
           END-IF
      *
      *    用法コード検索
           INITIALIZE                  YOUHOU-REC
           MOVE    SPA-Z061-INPUTCD    TO  YOUHOU-NAME
           MOVE    SPA-SYSYMD          TO  YOUHOU-YUKOSTYMD
           MOVE    SPA-SYSYMD          TO  YOUHOU-YUKOEDYMD
           MOVE    WRK-KEY-INPUT       TO  YOUHOU-NAME
           MOVE    WRK-KEY-INPUT       TO  YOUHOU-KANA-NAME
      *
           MOVE    YOUHOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_youhou"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 940-YOUHOU-READ-SEC
           ELSE
               INITIALIZE                  YOUHOU-REC
               MOVE    1               TO  FLG-YOUHOU
           END-IF
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL      (IDX              >    200 )
                      OR  (FLG-YOUHOU       =   1    )
                      OR  (SPA-Z061-NEXT    =   1    )
               MOVE    ZERO                TO  FLG-NO
               IF     (SPA-Z061-YH-BASIC   NOT =   SPACE  )
                 AND  (SPA-Z061-YH-BASIC   NOT =   YOUHOU-BASIC-C)
                   MOVE    1               TO  FLG-NO
               END-IF
               IF     (SPA-Z061-YH-DETAIL   NOT =   SPACE  )
                 AND  (SPA-Z061-YH-DETAIL   NOT =   YOUHOU-DETAIL-C)
                   MOVE    1               TO  FLG-NO
               END-IF
               IF     (SPA-Z061-YH-TIMING   NOT =   SPACE  )
                 AND  (SPA-Z061-YH-TIMING   NOT =   YOUHOU-TIMING-C)
                   MOVE    1               TO  FLG-NO
               END-IF
      *
               IF      FLG-NO              =   ZERO
                   ADD     1               TO  IDX2
                   IF      IDX2            <   WRK-STR
                       CONTINUE
                   ELSE
                       PERFORM 30011-YOUHOU-HEN-SEC
                   END-IF
                END-IF
      *
               MOVE    "tbl_youhou"        TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 940-YOUHOU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_youhou"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z061-GMN-MAX     =   ZERO
                   MOVE    21                  TO  SPA-Z061-GMN-CUR
                   MOVE    "0010"              TO  SPA-ERRCD
               ELSE
                   MOVE    1                   TO  SPA-Z061-GMN-CUR
               END-IF
           END-IF
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ編集処理
      *****************************************************************
       30011-YOUHOU-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDX
      *    ２００件オーバーチェック
           IF      IDX                 >   200
               MOVE    1                   TO  SPA-Z061-NEXT
               GO      TO      30011-YOUHOU-HEN-EXT
           END-IF
      *
           MOVE    IDX                 TO  SPA-Z061-LNUM   (IDX)
           MOVE    YOUHOU-CODE         TO  SPA-Z061-LYH-CODE
                                                            (IDX)
           MOVE    YOUHOU-NAME         TO  SPA-Z061-LYH-NAME(IDX)
           MOVE    YOUHOU-BASIC-NAME   TO  SPA-Z061-LBASIC-MEI (IDX)
           MOVE    YOUHOU-DETAIL-NAME  TO  SPA-Z061-LDETAIL-MEI (IDX)
           MOVE    YOUHOU-TIMING-NAME  TO  SPA-Z061-LTIMING-MEI (IDX)
           MOVE    YOUHOU-REFER-NO     TO  SPA-Z061-LREFER (IDX)
      *
           MOVE    IDX                 TO  SPA-Z061-GMN-MAX
      *
           .
       30011-YOUHOU-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-Z061-GMN-MAX     =   ZERO )
               MOVE    ZERO                TO  WRK-Z061-ROW
           ELSE
               MOVE    Z061-LIST-ROW       TO  WRK-Z061-ROW
           END-IF
      *
           MOVE    SPACE               TO  Z061
           INITIALIZE                      Z061
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  Z061-LIST-ROW
      *     MOVE    ZERO                TO  Z061-LIST-ROWATTR
           IF      WRK-Z061-ROW     NOT =   ZERO
               MOVE    WRK-Z061-ROW         TO  Z061-LIST-ROW
           END-IF
      *
           MOVE    SPA-Z061-GMN-INPUT   TO  Z061-INPUT
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-Z061-GMN-MAX
      *
               MOVE    SPA-Z061-LNUM (IDX)     TO  WRK-NOZ
               MOVE    WRK-NOZ             TO  Z061-TNO  (IDX)
               MOVE    SPA-Z061-LYH-NAME (IDX)
                                           TO  Z061-TNAME (IDX)
               MOVE    SPA-Z061-LBASIC-MEI (IDX)
                                           TO  Z061-TBASIC (IDX)
               MOVE    SPA-Z061-LDETAIL-MEI (IDX)
                                           TO  Z061-TDETAIL (IDX)
               MOVE    SPA-Z061-LTIMING-MEI (IDX)
                                           TO  Z061-TTIMING (IDX)
               MOVE    SPA-Z061-LREFER (IDX)
                                           TO  Z061-TREFER-NO (IDX)
      *
               IF      IDX                 =   SPA-Z061-GMN-SELNUM
                   MOVE    "T"             TO  Z061-LIST-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  Z061-LIST-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-Z061-GMN-MAX        TO  Z061-LIST-COUNT
      *
           MOVE    SPA-Z061-GMN-SELNUM     TO  Z061-SELNUM1
      *
           MOVE    SPA-Z061-YH-BASIC-G     TO  Z061-BASIC
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-YH-BASICMAX
               MOVE    SPA-GMN-YH-BASIC-LST(IDX)
                                          TO  Z061-BASIC-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YH-BASICMAX    TO  Z061-BASIC-COUNT
      *
           MOVE    SPA-Z061-YH-DETAIL-G   TO  Z061-DETAIL
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-Z061-YH-DETAILMAX
               MOVE    SPA-Z061-YH-DETAIL-LST(IDX)
                                          TO  Z061-DETAIL-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-Z061-YH-DETAILMAX   TO  Z061-DETAIL-COUNT
      *
           MOVE    SPA-Z061-YH-TIMING-G TO  Z061-TIMING
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-YH-TIMINGMAX
               MOVE    SPA-GMN-YH-TIMING-LST(IDX)
                                          TO  Z061-TIMING-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-YH-TIMINGMAX   TO  Z061-TIMING-COUNT
      *
      *    次頁あり
           IF      SPA-Z061-NEXT        =   1
               IF      SPA-Z061-PAGE        =   1
                   MOVE    "次頁有り"          TO  Z061-NEXT-VALUE
               ELSE
                   MOVE    "前頁・次頁有り"    TO  Z061-NEXT-VALUE
               END-IF
               MOVE    "red"               TO  Z061-NEXT-STYLE
           ELSE
               IF      SPA-Z061-PAGE        >   1
                   MOVE    "前頁有り"          TO  Z061-NEXT-VALUE
                   MOVE    "red"               TO  Z061-NEXT-STYLE
               ELSE
                   MOVE    SPACE               TO  Z061-NEXT-VALUE
                   MOVE    "black"             TO  Z061-NEXT-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-Z061-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM1"      TO  MCP-WIDGET
               WHEN    21
                   MOVE    "INPUT"         TO  MCP-WIDGET
               WHEN    22
                   MOVE    "BASIC"         TO  MCP-WIDGET
               WHEN    23
                   MOVE    "DETAIL"         TO  MCP-WIDGET
               WHEN    24
                   MOVE    "TIMING"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE               TO  SPA-Z061-YHCODE
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLREA-SEC
      *        前
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 290-MAE-SEC
      *        部分
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 291-BUB-SEC
      *        後方
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 292-ATO-SEC
      *        次ページ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 270-NEXT-SEC
      *        前ページ
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 270-BACK-SEC
      *        確定
      *        WHEN    "CLICKED"       ALSO    "B12"
      ****         PERFORM 290-KAKUTEI-SEC
           END-EVALUATE
          .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        キー入力
               WHEN    "ACTIVATE"      ALSO    "INPUT"
                   PERFORM 41013-INPUT-CHK-SEC
      *        番号入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM1"
                   PERFORM 41011-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   MOVE    Z061-LIST-ROW       TO  WRK-Z061-ROW
                   PERFORM 41012-LISTSEL-SEC
      *        基本用法コード
               WHEN    "ACTIVATE"      ALSO    "BASIC"
                   PERFORM 41014-BASIC-CHK-SEC
      *        用法詳細
               WHEN    "ACTIVATE"      ALSO    "DETAIL"
                   PERFORM 41014-DETAIL-CHK-SEC
      *        タイミング
               WHEN    "ACTIVATE"      ALSO    "TIMING"
                   PERFORM 41014-TIMING-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー入力　処理
      *****************************************************************
       41013-INPUT-CHK-SEC                SECTION.
      *
           MOVE    21                  TO  SPA-Z061-GMN-CUR
      *
           MOVE    Z061-INPUT          TO  SPA-Z061-GMN-INPUT
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  FLG-KANA
           MOVE    ZERO                TO  WRK-MOJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-Z061-GMN-INPUT  TO  KANACHK-MAE-INPUT
           MOVE    40                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-Z061-GMN-INPUT
           IF      KANACHK-RC2         =   7
               MOVE    KANACHK-OUT-INPUT   TO  SPA-Z061-GMN-INPUT
           END-IF
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO                  41013-INPUT-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX     TO  WRK-MOJI
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
           IF     (WRK-MOJI            =   ZERO  )
              OR  (FLG-KANJI           =   ZERO  )
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO                  41013-INPUT-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-Z061-PAGE
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    SPA-Z061-GMN-INPUT  TO  SPA-Z061-INPUTCD
           PERFORM 310-SPA-SET-SEC
           IF      SPA-Z061-GMN-MAX     =   ZERO
      *        全角で対象がない時
               MOVE    22               TO  SPA-Z061-GMN-CUR 
           ELSE
               MOVE    1                TO  SPA-Z061-GMN-CUR
           END-IF
      *
           .
       41013-INPUT-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    番号選択ＳＰＡ処理
      *****************************************************************
       410110-GMN-SPA-SEC                SECTION.
      *
           MOVE    Z061-SELNUM1       TO  SPA-Z061-GMN-SELNUM
           IF      SPA-Z061-GMN-SELNUM  =   SPACE
               MOVE    ZERO                TO  SPA-Z061-SELNUM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-Z061-GMN-SELNUM  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN         =   "1"   )   OR
                      (SNUM-SYOKBN         =   "1"   )
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    IDX-1           TO  SPA-Z061-GMN-CUR
               ELSE
                   MOVE    SNUM-OUTNUM     TO  SPA-Z061-SELNUM
                   MOVE    SPA-Z061-SELNUM
                                           TO  WRK-NOZ
                   MOVE    WRK-NOZ         TO  SPA-Z061-GMN-SELNUM
               END-IF
           END-IF
      *
           .
       410110-GMN-SPA-EXT.
           EXIT.
      *****************************************************************
      *    番号選択　処理
      *****************************************************************
       41011-SELNUM-CHK-SEC                SECTION.
      *
           MOVE    1                   TO  SPA-Z061-GMN-CUR
      *
           MOVE    Z061-SELNUM1        TO  SPA-Z061-GMN-SELNUM
      *    選択番号入力
           INITIALIZE                      ORCSNUMAREA
           MOVE    Z061-SELNUM1        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41011-SELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  SPA-Z061-SELNUM
           END-IF
      *    １件のみとき、１とする
           IF     (SPA-Z061-GMN-MAX    =   1  )  AND
                  (SPA-Z061-SELNUM     =  ZERO)
               MOVE    1               TO  SPA-Z061-SELNUM
           END-IF
      *
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z061-GMN-MAX
               IF       SPA-Z061-LNUM (IDX)    =   SPA-Z061-SELNUM
                   MOVE    IDX                 TO  WRK-SELNO
                   MOVE    SPA-Z061-GMN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNO           <   01  OR
                                       >   SPA-Z061-GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    WRK-SELNO           TO  IDX
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-Z061-LYH-CODE (IDX)
                                       TO  SPA-Z061-YHCODE
      *         戻るへ
                PERFORM 210-BACK
           END-IF
           .
       41011-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-Z061-GMN-MAX
               IF      Z061-LIST-SELECT (IDX)    =   "T"
                   MOVE    SPA-Z061-LNUM (IDX)  TO  WRK-SELNO
                   MOVE    SPA-Z061-GMN-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNO       NOT =   ZERO
               MOVE    WRK-SELNO       TO  Z061-SELNUM1
               PERFORM 41011-SELNUM-CHK-SEC
           END-IF
           .
       41012-LISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本用法コード入力処理
      *****************************************************************
       41014-BASIC-CHK-SEC              SECTION.
      *
           MOVE    22                  TO  SPA-Z061-GMN-CUR
      *
           MOVE    Z061-BASIC          TO  SPA-Z061-YH-BASIC-G
      *
           MOVE    ZERO                TO  FLG-OK
           IF      SPA-Z061-YH-BASIC    =   SPACE
               MOVE    SPACE            TO  SPA-Z061-YH-BASIC-G
               MOVE    1                TO  FLG-OK
           END-IF
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-GMN-YH-BASICMAX )
                        OR   (FLG-OK   =   1    )
               IF      SPA-Z061-YH-BASIC      =   SPA-GMN-YH-BASIC-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-YH-BASIC-LST (IDX)
                                           TO  SPA-Z061-YH-BASIC-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO   41014-BASIC-CHK-EXT
           END-IF
      *
      *    用法詳細区分コードリストボックス
           MOVE    SPACE               TO  SPA-Z061-YH-DETAIL-G
           PERFORM 31032-YOUHOU-COBHEN02-SEC
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    1                   TO  SPA-Z061-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       41014-BASIC-CHK-EXT.
           EXIT.
      *****************************************************************
      *    リストボックス設定処理
      *****************************************************************
       31032-YOUHOU-COBHEN02-SEC          SECTION.
      *
           INITIALIZE                      SPA-Z061-YH-DETAIL-LSTG
           INITIALIZE                      SPA-Z061-YH-DETAILMAX
      *
           EVALUATE      SPA-Z061-YH-BASIC
           WHEN    "1"
      *    内服
                MOVE    "0 経口"       TO  SPA-Z061-YH-DETAIL-LST (1)
                MOVE    "1 舌下"       TO  SPA-Z061-YH-DETAIL-LST (2)
                MOVE    "2 バッカル"   TO  SPA-Z061-YH-DETAIL-LST (3)
                MOVE    "3 口腔内塗布" TO  SPA-Z061-YH-DETAIL-LST (4)
                MOVE    4              TO  SPA-Z061-YH-DETAILMAX
           WHEN    "2"
                MOVE    "A 貼付"       TO  SPA-Z061-YH-DETAIL-LST (1)
                MOVE    "B 塗布"       TO  SPA-Z061-YH-DETAIL-LST (2)
                MOVE    "C 湿布"       TO  SPA-Z061-YH-DETAIL-LST (3)
                MOVE    "D 撒布"       TO  SPA-Z061-YH-DETAIL-LST (4)
                MOVE    "E 噴霧"       TO  SPA-Z061-YH-DETAIL-LST (5)
                MOVE    "F 消毒"       TO  SPA-Z061-YH-DETAIL-LST (6)
                MOVE    "G 点耳"       TO  SPA-Z061-YH-DETAIL-LST (7)
                MOVE    "H 点眼"       TO  SPA-Z061-YH-DETAIL-LST (8)
                MOVE    "J 点鼻"       TO  SPA-Z061-YH-DETAIL-LST (9)
                MOVE    "K うがい"     TO  SPA-Z061-YH-DETAIL-LST (10)
                MOVE    "L 吸入"       TO  SPA-Z061-YH-DETAIL-LST (11)
                MOVE    "M トローチ"   TO  SPA-Z061-YH-DETAIL-LST (12)
                MOVE    "N 膀胱洗浄"   TO  SPA-Z061-YH-DETAIL-LST (13)
                MOVE    "P 鼻腔内洗浄" TO  SPA-Z061-YH-DETAIL-LST (14)
                MOVE    "Q 浣腸"       TO  SPA-Z061-YH-DETAIL-LST (15)
                MOVE    "R 肛門挿入"   TO  SPA-Z061-YH-DETAIL-LST (16)
                MOVE    "S 肛門注入"   TO  SPA-Z061-YH-DETAIL-LST (17)
                MOVE    "T 腟内挿入"   TO  SPA-Z061-YH-DETAIL-LST (18)
                MOVE    "U 膀胱注入"   TO  SPA-Z061-YH-DETAIL-LST (19)
                MOVE    19             TO  SPA-Z061-YH-DETAILMAX
           WHEN    "3"
      *        注射
                MOVE    "0 静脈注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (1)
                MOVE    "1 中心静脈注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (2)
                MOVE    "2 皮下注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (3)
                MOVE    "3 筋肉内注射"
                                       TO  SPA-Z061-YH-DETAIL-LST (4)
                MOVE    "4 皮内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (5)
                MOVE    "5 動脈注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (6)
                MOVE    "A 硬膜外注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (7)
                MOVE    "B 脳脊髄腔内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (8)
                MOVE    "C 髄内注射"
                                       TO  SPA-Z061-YH-DETAIL-LST (9)
                MOVE    "D 関節腔内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (10)
                MOVE    "E 腱鞘内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (11)
                MOVE    "F 腱鞘周囲注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (12)
                MOVE    "G 硝子体内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (13)
                MOVE    "H 結膜下注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (14)
                MOVE    "J テノン氏のう内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (15)
                MOVE    "K 耳茸内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (16)
                MOVE    "L 咽頭注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (17)
                MOVE    "M 胸腔内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (18)
                MOVE    "N 角膜内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (19)
                MOVE    "Q 球後注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (20)
                MOVE    "R 腹腔内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (21)
                MOVE    "Z 局所・病巣内注射" 
                                       TO  SPA-Z061-YH-DETAIL-LST (22)
                MOVE    22             TO  SPA-Z061-YH-DETAILMAX
           WHEN    "4"
      *        注入の場合
               MOVE    "0 腹膜透析" 
                                       TO  SPA-Z061-YH-DETAIL-LST (1)
               MOVE    "1 気管内注入" 
                                       TO  SPA-Z061-YH-DETAIL-LST (2)
               MOVE    "2 涙のう内注入" 
                                       TO  SPA-Z061-YH-DETAIL-LST (3)
               MOVE    "3 鼓室内注入" 
                                       TO  SPA-Z061-YH-DETAIL-LST (4)
               MOVE    "4 滑液嚢穿刺後の注入" 
                                       TO  SPA-Z061-YH-DETAIL-LST (5)
               MOVE    "5 腹腔内注入" 
                                       TO  SPA-Z061-YH-DETAIL-LST (6)
               MOVE    "Z 病巣内注入" 
                                       TO  SPA-Z061-YH-DETAIL-LST (7)
               MOVE    7               TO  SPA-Z061-YH-DETAILMAX
           WHEN    "X"
      *        指定なし
               MOVE    "X ー"
                                       TO  SPA-Z061-YH-DETAIL-LST (1)
               MOVE    1               TO  SPA-Z061-YH-DETAILMAX
           END-EVALUATE
           .
       31032-YOUHOU-COBHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本用法コード入力処理
      *****************************************************************
       41014-DETAIL-CHK-SEC              SECTION.
      *
           MOVE    23                  TO  SPA-Z061-GMN-CUR
      *
           MOVE    Z061-DETAIL          TO  SPA-Z061-YH-DETAIL-G
      *
           MOVE    ZERO                TO  FLG-OK
           IF      SPA-Z061-YH-DETAIL   =   SPACE
               MOVE    SPACE            TO  SPA-Z061-YH-DETAIL-G
               MOVE    1                TO  FLG-OK
           END-IF
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-Z061-YH-DETAILMAX )
                        OR   (FLG-OK   =   1    )
               IF      SPA-Z061-YH-DETAIL   =   SPA-Z061-YH-DETAIL-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-Z061-YH-DETAIL-LST (IDX)
                                           TO  SPA-Z061-YH-DETAIL-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO      TO   41014-DETAIL-CHK-EXT
           END-IF
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    1                   TO  SPA-Z061-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       41014-DETAIL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本用法コード入力処理
      *****************************************************************
       41014-TIMING-CHK-SEC              SECTION.
      *
           MOVE    24                  TO  SPA-Z061-GMN-CUR
      *
           MOVE    Z061-TIMING         TO  SPA-Z061-YH-TIMING-G
      *
           MOVE    ZERO                TO  FLG-OK
           IF      SPA-Z061-YH-TIMING  =   SPACE
               MOVE    SPACE           TO  SPA-Z061-YH-TIMING-G
               MOVE    1               TO  FLG-OK
           END-IF
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-GMN-YH-TIMINGMAX )
                        OR   (FLG-OK   =   1    )
               IF      SPA-Z061-YH-TIMING   =   SPA-GMN-YH-TIMING-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-YH-TIMING-LST (IDX)
                                           TO  SPA-Z061-YH-TIMING-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               GO      TO   41014-TIMING-CHK-EXT
           END-IF
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    1                   TO  SPA-Z061-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       41014-TIMING-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全体一致処理
      *****************************************************************
       290-ALL-SEL-SEC                   SECTION.
      *
          .
       290-ALL-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLREA-SEC                   SECTION.
      *
           MOVE    SPACE               TO  SPA-Z061-INPUTCD
           MOVE    SPACE               TO  SPA-Z061-GMN-INPUT
           INITIALIZE                      SPA-Z061-YH-BASIC-G
           INITIALIZE                      SPA-Z061-YH-DETAIL-G
           INITIALIZE                      SPA-Z061-YH-DETAIL-LSTG
           MOVE    ZERO                TO  SPA-Z061-YH-DETAILMAX
           INITIALIZE                      SPA-Z061-YH-TIMING-G
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    1                   TO  SPA-Z061-PAGE
           PERFORM                     310-SPA-SET-SEC
          .
       220-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    次ページ処理
      *****************************************************************
       270-NEXT-SEC                   SECTION.
      *
           IF      SPA-Z061-NEXT        >   ZERO
               ADD     1                   TO  SPA-Z061-PAGE
               MOVE    ZERO                TO  SPA-Z061-NEXT
      *        ＳＰＡセット
               PERFORM                     310-SPA-SET-SEC
           ELSE
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
      *
          .
       270-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    前ページ処理
      *****************************************************************
       270-BACK-SEC                   SECTION.
      *
           IF      SPA-Z061-PAGE        =   1
               MOVE    "0007"              TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-Z061-PAGE    =   SPA-Z061-PAGE  -   1
               MOVE    ZERO                TO  SPA-Z061-NEXT
      *        ＳＰＡセット
               PERFORM                     310-SPA-SET-SEC
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
          .
       270-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前方一致処理
      *****************************************************************
       290-MAE-SEC                   SECTION.
      *
      *    全角入力時、のみ
           IF      SPA-Z061-GMN-INPUT  =   SPACE
               GO      TO      290-MAE-EXT
           END-IF
      *
           INSPECT SPA-Z061-GMN-INPUT   REPLACING  ALL "＊"  BY  "  "
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    ZERO                TO  ID2
           PERFORM VARYING     ID1     FROM    1   BY  1
                   UNTIL       ID1     >   40
               IF      SPA-Z061-GMN-INPUT(ID1:1)    NOT =   SPACE
                   ADD     1                   TO  ID2
                   MOVE    SPA-Z061-GMN-INPUT(ID1:1)
                                               TO  WRK-INPUTCD(ID2:1)
               END-IF
           END-PERFORM
      *
           MOVE    WRK-INPUTCD         TO  SPA-Z061-GMN-INPUT
      *
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    1                   TO  SPA-Z061-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       290-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    部分一致処理
      *****************************************************************
       291-BUB-SEC                   SECTION.
      *
      *    全角入力時、のみ
           IF      SPA-Z061-GMN-INPUT  =   SPACE
               GO      TO      291-BUB-EXT
           END-IF
      *
           INSPECT SPA-Z061-GMN-INPUT   REPLACING  ALL "＊"  BY  "  "
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    ZERO                TO  ID2
           PERFORM VARYING     ID1     FROM    1   BY  1
                   UNTIL       ID1     >   40
               IF      SPA-Z061-GMN-INPUT(ID1:1)    NOT =   SPACE
                   ADD     1                   TO  ID2
                   MOVE    SPA-Z061-GMN-INPUT(ID1:1)
                                               TO  WRK-INPUTCD(ID2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-Z061-GMN-INPUT
           STRING  "＊"                DELIMITED  BY  SIZE
                   WRK-INPUTCD         DELIMITED  BY  SPACE
                   "＊"                DELIMITED  BY  SIZE
                                       INTO    SPA-Z061-GMN-INPUT
           END-STRING
      *
           MOVE    SPA-Z061-GMN-INPUT  TO  SPA-Z061-INPUTCD
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    1                   TO  SPA-Z061-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       291-BUB-EXT.
           EXIT.
      *****************************************************************
      *    後分一致処理
      *****************************************************************
       292-ATO-SEC                   SECTION.
      *
      *    全角入力時、のみ
           IF      SPA-Z061-GMN-INPUT  =   SPACE
               GO      TO      292-ATO-EXT
           END-IF
      *
           INSPECT SPA-Z061-GMN-INPUT   REPLACING  ALL "＊"  BY  "  "
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    ZERO                TO  ID2
           PERFORM VARYING     ID1     FROM    1   BY  1
                   UNTIL       ID1     >   22
               IF      SPA-Z061-GMN-INPUT(ID1:1)    NOT =   SPACE
                   ADD     1                   TO  ID2
                   MOVE    SPA-Z061-GMN-INPUT(ID1:1)
                                               TO  WRK-INPUTCD(ID2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-Z061-GMN-INPUT
           STRING  "＊"                DELIMITED  BY  SIZE
                   WRK-INPUTCD         DELIMITED  BY  SPACE
                                       INTO    SPA-Z061-GMN-INPUT
           END-STRING
      *
           MOVE    SPA-Z061-GMN-INPUT  TO  SPA-Z061-INPUTCD
      *
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-Z061-NEXT
           MOVE    1                   TO  SPA-Z061-PAGE
           PERFORM                     310-SPA-SET-SEC
      *
          .
       292-ATO-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Z06W"              TO  SPA-SAKIPG
           MOVE    "Z061"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
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
      *    確定処理
      *****************************************************************
       290-KAKUTEI-SEC            SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
           MOVE    ZERO                TO  IDX-N
      *
      *     PERFORM 41011-SELNUM-ALLCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      290-KAKUTEI-EXT
           END-IF
      *
      *    戻るへ
           PERFORM 210-BACK
      *
           .
       290-KAKUTEI-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "Z061"              TO  MCP-WINDOW
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
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   STRING  "基本用法コード入力エラー"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "用法詳細区分コード入力エラー"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "投与タイミング入力エラー"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   MOVE
                   "次ページはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE
                   "前ページはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE
                   "対象の用法がありません。"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
      *
           MOVE    SPACE               TO  ZERR
           MOVE    SPA-ERRCD           TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  ZERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "Z061"               TO  SPA-MOTOPG
           MOVE    "ZERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "ZERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    用法マスター読込
      *****************************************************************
       940-YOUHOU-READ-SEC         SECTION.
      *
           MOVE    "tbl_youhou"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  YOUHOU-REC
               MOVE    ZERO                TO  FLG-YOUHOU
           ELSE
               INITIALIZE                  YOUHOU-REC
               MOVE    1                   TO  FLG-YOUHOU
           END-IF
      *
           .
       940-YOUHOU-READ-EXT.
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
