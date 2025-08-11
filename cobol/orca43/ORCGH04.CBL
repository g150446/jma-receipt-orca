      ********************************************************************
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
       PROGRAM-ID.             ORCGH04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 返戻データ取込一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  22/08/05    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.02.01    ORCAMO       23/05/26  改ページ対応
      *  05.02.02    ORCAMO       23/07/11  返戻データを取り込み後に患者
      *                                     番号の再発行をした場合の対応
      *  05.02.03    ORCAMO       23/07/25  オン資を行うことができない
      *                                     場合のレセプト対応
      *  05.02.02    ORCAMO       23/08/26  返戻データ患者番号置換処理対応
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求管理スパ領域
           COPY    "H01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "H01SCR-SPA".
      *
      *    設定済返戻データ
           02  SPA-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //SPA-HENREI-B-//.
      *
      *    対象一覧用返戻データ
           02  TBL-HENREI-B-REC    OCCURS  30.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //TBL-HENREI-B-//.
           02  TBL-HENREI-B-ERR    PIC 9(01)   OCCURS  30.
           02  TBL-HENREI-B-MAX    PIC 9(02).
      *
      *    更新対象返戻データ
           02  UPD-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //UPD-HENREI-B-//.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HENREI-K        PIC 9(01).
           03  FLG-HENREI-B        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-HENREI-B-CHK    PIC 9(01).
      *
           03  FLG-KANRI-ROW       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
           03  IDE                 PIC 9(04).
      *
           03  IDWW                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNO           PIC 9(05).
      *
           03 WRK-ZZ9              PIC ZZ9.
           03  WRK-STR             PIC 9(05).
           03  WRK-BANGO           PIC 9(05).
      *
           03  WRK-H04-BODY-ROW    PIC S9(09).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-H04-KANRI-ROW   PIC S9(09).
      *
           03  WRK-RECEIPT-NO      PIC X(06).
      *
      *    ＣＳＶ項目数
           03  SPLIT-CNT               PIC 9(03).
           03  SPLIT-CNT1              PIC 9(03).
      *
           03  FLG-DATAEND             PIC 9(01).
           03  FLG-MATCH               PIC 9(01).
           03  FLG-SET                 PIC 9(01).
           03  FLG-AREA1.
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DELI1           PIC 9(01).
               05  FLG-DELI2           PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
      *
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-WK              PIC 9(04).
      *
           03  WRK-PARA-REC.
               05  WRK-PARA-X          PIC X(01)  OCCURS   3000.
           03  WRK-PARA-REC1           PIC X(3000).
           03  WRK-PARA-REC2           PIC X(3000).
      *
           03  WRK-GMN-H04-COMMENT     PIC X(2420).
      *
           01  CHK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //CHK-HENREI-B-//.
      *
           01  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //WRK-HENREI-B-//.
      *
       01  WRK-HEN-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG.
               05  WRK-HENYM       PIC X(06).
               05  WRK-HENYM-FIL   PIC X(03).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-BODY-LINE-MAX
                                   PIC 9(04)   VALUE   100.
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
           03  WRK-KUGIRI-LINE.
               05  FILLER          PIC X(40)   VALUE
                   "ーーーーーーーーーーーーーーーーーーーー".
               05  FILLER          PIC X(40)   VALUE
                   "ーーーーーーーーーーーーーーーーーーーー".
               05  FILLER          PIC X(30)   VALUE
                   "ーーーーーーーーーーーーーーー".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  HENREI-K-REC.
           COPY    "CPHENREI-KANRI.INC".
      *
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
      *
           COPY    "CPSK1001.INC".
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
           COPY    "CPORCSPTNUM.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA43SCRAREA.INC".
      *
       PROCEDURE                       DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-H01FREE
           MOVE    SPA-WORK        TO  SPA-H01KYOUTU
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
           MOVE    SPA-H01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-H01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           IF      SPA-MOTOPG          =   "HERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "H04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
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
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-H04-AREA
           INITIALIZE                  SPA-H04-AREA
      *     
           MOVE    SPA-GMN-H03-MAX TO  SPA-GMN-H04-MAX
           MOVE    SPA-GMN-H03-PAGE
                                   TO  SPA-GMN-H04-PAGE
           MOVE    SPA-GMN-H03-NEXT
                                   TO  SPA-GMN-H04-NEXT
      *
           MOVE    SPA-GMN-H03-BODY-TBL
                                   TO  SPA-GMN-H04-BODY-TBL
           MOVE    SPA-NAI-H03-BODY-TBL
                                   TO  SPA-NAI-H04-BODY-TBL
      *
           MOVE    1               TO  FLG-KANRI-ROW
      *
           MOVE    1               TO  SPA-GMN-H04-CUR
      *
           .
       310-SPASET-EXT.
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
                   PERFORM 210-BACK
      *    前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *    次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
                WHEN   "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 410-SELNUM-CHK-SEC
                WHEN   ANY             ALSO    "KANRI_LIST"
                   PERFORM 450-KANRILST-SELECT-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       410-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-GMN-H04-CUR
      *
           INITIALIZE                      SPA-GMN-H04-KOUMOKU
                                           SPA-NAI-H04-KOUMOKU
      *
           MOVE    H04-SELNUM          TO  SPA-GMN-H04-SELNUM
           MOVE    ZERO                TO  SPA-NAI-H04-SELNUM
      *
           IF      SPA-GMN-H04-SELNUM  =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               COMPUTE WRK-BANGO       =   SPA-GMN-H04-SELNUM
                                       - ( SPA-GMN-H04-PAGE    -   1 )
                                           *   WRK-CONS-BODY-LINE-MAX
      *
      *        選択番号チェック
               IF    ( WRK-BANGO       >   WRK-CONS-BODY-LINE-MAX )
               OR    ( WRK-BANGO       <   1                   )
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-BANGO       TO  SPA-NAI-H04-SELNUM
                                               H04-KANRI-LIST-ROW
      *            返戻取り込みデータ明細詳細情報編集処理
                   PERFORM 4101-HENREI-B-SHOSAI-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       410-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   返戻取り込みデータ明細詳細情報編集処理
      *****************************************************************
       4101-HENREI-B-SHOSAI-HENSYU-SEC SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-H04-COMMENT
      *
            INITIALIZE                     HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    SPA-NAI-H03-K-TSYORIYM       (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-SYORIYM
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI  (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI2 (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI2
           MOVE    SPA-NAI-H03-K-THOSPCD        (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-HOSPCD
           MOVE    SPA-NAI-H04-B-TRECEIPT-NO    (SPA-NAI-H04-SELNUM)
                                       TO  HENREI-B-RECEIPT-NO
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-HENREI-BODY-READ-KEY10-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-B
           END-IF
      *
           MOVE    ZERO                TO  IDZZ
           PERFORM         UNTIL   FLG-HENREI-B    =   1
               MOVE    HENREI-B-HENREI-INFO2
                                       TO  WRK-PARA-REC
               INITIALIZE              WRK-HENREI-B-REC    
               ADD     1               TO  IDZZ
               MOVE    7               TO  SPLIT-CNT
               MOVE    8               TO  SPLIT-CNT1
               PERFORM 20011-PARA-SPLIT-SEC
               IF      IDZZ            >   1 
                   STRING  SPA-GMN-H04-COMMENT DELIMITED  BY  SPACE
                           WRK-KUGIRI-LINE     DELIMITED  BY  SIZE
                           WRK-KAIGYO          DELIMITED  BY  SIZE
                                           INTO    WRK-GMN-H04-COMMENT
                   END-STRING
               ELSE    
                   MOVE    SPA-GMN-H04-COMMENT
                                           TO  WRK-GMN-H04-COMMENT
               END-IF
               MOVE    SPACE           TO  SPA-GMN-H04-COMMENT
               STRING  WRK-GMN-H04-COMMENT  DELIMITED  BY  SPACE
                       "【返戻事由コード】" DELIMITED  BY  SIZE
                       WRK-HENREI-B-HR-CODE DELIMITED  BY  SPACE
                       WRK-KAIGYO           DELIMITED  BY  SIZE
                       "【返戻事由】"       DELIMITED  BY  SIZE
                       WRK-HENREI-B-HR-MSG  DELIMITED  BY  SPACE
                        WRK-KAIGYO          DELIMITED  BY  SIZE
                       "【補足事項】"       DELIMITED  BY  SIZE
                       WRK-HENREI-B-HENREI-INFO2
                                            DELIMITED  BY  SPACE
                       WRK-KAIGYO           DELIMITED  BY  SIZE
                                            INTO    SPA-GMN-H04-COMMENT
               END-STRING
      *
               PERFORM 900-HENREI-BODY-READ-KEY10-SEC
           END-PERFORM
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       4101-HENREI-B-SHOSAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データカンマ分離処理
      *****************************************************************
       20011-PARA-SPLIT-SEC          SECTION.
      *
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  IDE
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   SPLIT-CNT)
                   OR      ( WRK-PO-ED     >=  3000 )
                   OR      ( FLG-END       =   1    )
      *
               MOVE    ZERO            TO  FLG-AREA1
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
      *
                IF    ( WRK-PARA-REC (WRK-PO-ST:)
                                       =   SPACE )
               OR    ( FLG-DATAEND     =   1     )
                   MOVE    1               TO  FLG-DATAEND
                   MOVE    WRK-PO-ST       TO  WRK-PO-ED
               ELSE
                   ADD     1               TO  WRK-KOUMOKU-NO
      *            項目終了文字後のカンマ位置を取得(WRK-PO-ED)
                   PERFORM 200111-CHAR-END-GET-SEC
               END-IF
      *        項目共通処理
               PERFORM 200112-KOUMOKU-KYOTU-SEC
      *        項目別処理
               IF      FLG-DATAEND     =   ZERO
                   PERFORM 2001133-RECEIP-KOUMOKU-BETU-SEC
               END-IF
           END-PERFORM
      *
           .
       20011-PARA-SPLIT-EXT.
           EXIT.
      *****************************************************************
      *    対象項目の文字終了位置を取得
      *****************************************************************
       200111-CHAR-END-GET-SEC        SECTION.
      *
           MOVE    WRK-PO-ST           TO  WRK-PO-ED
      *
           PERFORM VARYING   IDYY   FROM    WRK-PO-ST   BY  1
                   UNTIL   ( IDYY           >=  3000 )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF    ( WRK-PARA-X (IDYY)     =   "," )  OR
                     ((IDX             =   SPLIT-CNT  OR  
                                           SPLIT-CNT1) AND
                     ( WRK-PARA-X (IDYY)     =   SPACE))
                   MOVE    IDYY         TO  WRK-PO-ED
                   MOVE    1           TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      WRK-PARA-X (IDYY) NOT =   SPACE
                       MOVE    IDYY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *        デリミタ(")存在するかどうか
               IF      WRK-PARA-X (IDYY)         =   """"
                   IF      FLG-DELI1       =   ZERO
                       MOVE    1       TO  FLG-DELI1
                   ELSE
                       MOVE    1       TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200111-CHAR-END-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目共通処理
      *****************************************************************
       200112-KOUMOKU-KYOTU-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-MOVE-AREA
      *
      *    データが省略されている場合、FLG-NODATAに1をセット
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   1
               END-IF
               IF      WRK-WK          =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  200112-KOUMOKU-KYOTU-EXT
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF      WRK-PO-ST       =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  200112-KOUMOKU-KYOTU-EXT
               END-IF
           END-IF
      *
      *    転送開始位置・転送桁数を算出
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               COMPUTE WRK-MO-ST       =   WRK-PO-ST   +   1
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-MO-CNT  =   WRK-PO-ED
                                       -  (WRK-PO-ST   +   2)
               ELSE
                   COMPUTE WRK-MO-CNT  =  (WRK-PO-ED   -   1)
                                       -   WRK-PO-ST
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               MOVE    WRK-PO-ST       TO  WRK-MO-ST
               COMPUTE WRK-MO-CNT      =   WRK-PO-ED  -  WRK-PO-ST
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT) =   X"0D" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT) =   X"0A" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           .
       200112-KOUMOKU-KYOTU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    返戻ファイル項目別処理
      *****************************************************************
       2001133-RECEIP-KOUMOKU-BETU-SEC       SECTION.
      * 
      *
           EVALUATE    WRK-PARA-REC(1:2)
               WHEN    "HR"
                   EVALUATE    WRK-KOUMOKU-NO
      *                返戻事由コード
                       WHEN    5
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   1    )
                           AND   ( KANACHK-MAX     <=  5    )
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                                   WRK-HENREI-B-HR-CODE
      *                     ELSE
      *                         MOVE    "W03"       TO  WRK-ERRCD
      *                         PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF    
      *                返戻理由
                       WHEN    6
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   2    )
                           AND   ( KANACHK-MAX     <=  1000 )
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                                   WRK-HENREI-B-HR-MSG
      *                     ELSE
      *                         MOVE    "W04"       TO  WRK-ERRCD
      *                         PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF    
      *                    補足事項
                       WHEN    7
                           IF      WRK-MO-CNT      >   ZERO
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "1"         TO  KANACHK-SYORI
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF    ( KANACHK-RC      =   ZERO )
                               AND   ( KANACHK-RC2     =   ZERO )
                               AND   ( KANACHK-SYUBETU =   2    )
                               AND   ( KANACHK-MAX     <=  1000 )
                                   MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                               WRK-HENREI-B-HENREI-INFO2
      *                         ELSE
      *                             MOVE    "W04"       TO  WRK-ERRCD
      *                             PERFORM 500-ERRCD-HENSYU-SEC
                               END-IF
                           END-IF    
                  END-EVALUATE
           END-EVALUATE
      *
           .
       2001133-RECEIP-KOUMOKU-BETU-EXT.
           EXIT.
      *****************************************************************
      *    管理情報一覧選択処理
      *****************************************************************
       450-KANRILST-SELECT-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-H04-KOUMOKU
                                           SPA-NAI-H04-KOUMOKU
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H04-BODY-MAX
               IF      H04-KANRI-LIST-SELECT (IDX)
                                       =   "T"
                   MOVE    IDX             TO  WRK-SELNO
                   MOVE    SPA-GMN-H04-BODY-MAX
                                           TO  IDX
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-NAI-H04-SELNUM
                                               H04-KANRI-LIST-ROW
               MOVE    SPA-GMN-H04-B-TSEQNUM (SPA-NAI-H04-SELNUM)
                                           TO  SPA-GMN-H04-SELNUM
      *        明細情報編集処理
               PERFORM 4101-HENREI-B-SHOSAI-HENSYU-SEC
           END-IF
           .
       450-KANRILST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF    ( SPA-GMN-H04-PAGE  =   1   )
           OR    ( SPA-GMN-H04-MAX  =   ZERO )
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-H04-CUR
               GO      TO      460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-H04-PAGE    =   SPA-GMN-H04-PAGE    -   1
      *
           INITIALIZE                      SPA-GMN-H04-AREA
                                           SPA-NAI-H04-AREA
           MOVE    ZERO                TO  SPA-GMN-H04-MAX
      *
           MOVE    1                   TO  SPA-GMN-H04-CUR
      *    検索処理
           PERFORM 4601-HENREI-BODY-HENSYU-SEC
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF    ( SPA-GMN-H04-NEXT =   ZERO )
           OR    ( SPA-GMN-H04-MAX  =   ZERO )
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-H04-CUR
               GO      TO      470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-H04-PAGE
      *
           INITIALIZE                      SPA-GMN-H04-AREA
                                           SPA-NAI-H04-AREA
           MOVE    ZERO                TO  SPA-GMN-H04-MAX
      *
           MOVE    1                   TO  SPA-GMN-H04-CUR
      *    検索処理
           PERFORM 4601-HENREI-BODY-HENSYU-SEC
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *   返戻取り込みデータ明細情報編集処理
      *****************************************************************
       4601-HENREI-BODY-HENSYU-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-H04-BODY-TBL
                                           SPA-NAI-H04-BODY-TBL
      *
           INITIALIZE                      HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    SPA-NAI-H03-K-TSYORIYM       (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-SYORIYM
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI  (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI2 (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI2
           MOVE    SPA-NAI-H03-K-THOSPCD        (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-HOSPCD
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-HENREI-BODY-READ-KEY4-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-B
           END-IF
      *
           COMPUTE WRK-STR         = ( SPA-GMN-H04-PAGE  -  1 )
                                   *   WRK-CONS-BODY-LINE-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-H04-MAX
           PERFORM         UNTIL   FLG-HENREI-B    =   1
                           OR      IDX             =
                                               WRK-CONS-BODY-LINE-MAX
      *
            ADD     1               TO  IDX2
            IF      IDX2            >=  WRK-STR
             ADD     1               TO  IDX
      *
             IF      IDX             <=  WRK-CONS-BODY-LINE-MAX
               MOVE    IDX             TO  SPA-GMN-H04-BODY-MAX
               MOVE    IDX2            TO  SPA-GMN-H04-B-TSEQNUM   (IDX)
      *
               MOVE    HENREI-B-NYUGAIKBN
                                       TO  SPA-NAI-H04-B-TNYUGAIKBN(IDX)
               MOVE    HENREI-B-PTID   TO  SPA-NAI-H04-B-TPTID     (IDX)
               MOVE    HENREI-B-PTNUM  TO  SPA-GMN-H04-B-TPTNUM    (IDX)
               MOVE    HENREI-B-NAME   TO  SPA-GMN-H04-B-TNAME     (IDX)
               MOVE    HENREI-B-SRYYM  TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG  (1:6)
                                       TO  SPA-GMN-H04-B-TSRYYM    (IDX)
               MOVE    HENREI-B-RECESYUBETU
                                       TO  SPA-NAI-H04-B-TRECESYUBETU
                                                                   (IDX)
               MOVE    HENREI-B-HKNJANUM
                                       TO  SPA-GMN-H04-B-THKNJANUM (IDX)
               STRING  HENREI-B-KIGO   DELIMITED   BY  SPACE
                       "　"            DELIMITED   BY  SIZE
                       HENREI-B-NUM    DELIMITED   BY  SPACE
                                       INTO    SPA-GMN-H04-B-TKIGO (IDX)
               END-STRING
               MOVE    HENREI-B-RECEIPT-KBN
                                       TO  SPA-NAI-H04-B-TRECEIPT-KBN
                                                                   (IDX)
               MOVE    HENREI-B-DISASTER
                                       TO  SPA-NAI-H04-B-TDISASTER (IDX)
               MOVE    HENREI-B-SHIKAKU-INFO-FLG
                                       TO
                                   SPA-NAI-H04-B-TSHIKAKU-INFO-FLG (IDX)
               MOVE    HENREI-B-HR-MSG TO  SPA-GMN-H04-B-TERR      (IDX)
      *********IF    ( SPA-NAI-H03-K-TTEISYUTUSAKI (SPA-NAI-H03-SELNUM)
      *********                        =   "3" )
      *********AND   ( SPA-NAI-H03-K-TTEISYUTUSAKI2 (SPA-NAI-H03-SELNUM)
      *********               =   "1" )
      *********    MOVE    HENREI-B-NUM
      *********                        TO  SPA-NAI-H04-B-TSRYDD    (IDX)
      *********END-IF
               MOVE    HENREI-B-RECEIPT-NO
                                       TO  SPA-NAI-H04-B-TRECEIPT-NO
                                                                   (IDX)
               MOVE     HENREI-B-SCREEN-UPD-FLG
                                       TO  SPA-NAI-H04-B-SCREEN-UPD-FLG
                                                                   (IDX)
      *
               MOVE    HENREI-B-REC        TO  MCPDATA-REC
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
               IF      FLG-HENREI-B-CHK    =   ZERO
                   MOVE  CHK-HENREI-B-ERRCD    TO
                                           SPA-GMN-H04-B-TERRCD    (IDX)
               END-IF
      *
      *        患者ＩＤチェック
               IF      HENREI-B-PTID   >   ZERO
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    HENREI-B-PTNUM  TO  SPTID-PTNUM
                   CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                   SPA-AREA
                   IF      SPTID-RC        NOT =   ZERO
                       INITIALIZE                      ORCSPTIDAREA
                       MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                       MOVE    HENREI-B-PTID       TO  SPTID-PTID
                       CALL    "ORCSPTID"           USING   ORCSPTIDAREA
                                                           SPA-AREA
                       IF      SPTID-RC            =   ZERO
                           CONTINUE
                       ELSE
                           MOVE    1           TO  SPA-NAI-H04-B-TERRFLG
                                                                   (IDX)
                       END-IF
                   ELSE
                       IF      SPTID-PTID      NOT =   HENREI-B-PTID
                           MOVE    2           TO  SPA-NAI-H04-B-TERRFLG
                                                                   (IDX)
                       END-IF
                   END-IF
               END-IF
               MOVE    IDX             TO  SPA-GMN-H04-MAX
             END-IF
            END-IF
      *
            PERFORM 900-HENREI-BODY-READ-KEY4-SEC
           END-PERFORM
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-CONS-BODY-LINE-MAX )  AND
                 ( FLG-HENREI-B          =   ZERO                )
               MOVE    1                   TO  SPA-GMN-H04-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-H04-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-KANRI-ROW
      *
           .
       4601-HENREI-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "H03"           TO  SPA-SAKIPG
           MOVE    "H04"           TO  SPA-MOTOPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO   WRK-HENYMDG
           ELSE
               IF      WRK-SYMD            =   ALL "9"
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
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               END-IF
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "H04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-KANRI-ROW       =   1
               MOVE    ZERO                TO  WRK-H04-KANRI-ROW
           ELSE
               MOVE    H04-KANRI-LIST-ROW  TO  WRK-H04-KANRI-ROW
           END-IF
      * 
           INITIALIZE                  H04
      *
           MOVE    SPA-GMN-H03-K-TSYORIYM      (SPA-NAI-H03-SELNUM)
                                       TO  H04-SYORIYM
           MOVE    SPA-GMN-H03-K-TTEISYUTUSAKI (SPA-NAI-H03-SELNUM)
                                       TO  H04-TEISYUTUSAKI
           MOVE    SPA-GMN-H03-K-TTEISYUTUSAKI2(SPA-NAI-H03-SELNUM)
                                       TO  H04-TEISYUTUSAKI2
           MOVE    SPA-GMN-H03-K-TCREYMD       (SPA-NAI-H03-SELNUM)
                                       TO  H04-CREYMD
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H04-BODY-MAX
               MOVE    SPA-GMN-H04-B-TSEQNUM    (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  H04-KANRI-TSEQNUM   (IDX)
               EVALUATE    SPA-NAI-H04-B-TNYUGAIKBN (IDX)
                   WHEN    "1"
                       MOVE    "入"        TO  H04-KANRI-TNYUGAIKBN(IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  H04-KANRI-TNYUGAIKBN(IDX)
               END-EVALUATE
               IF   ( SPA-NAI-H04-B-SCREEN-UPD-FLG (IDX)
                                           =   1 )
               AND  ( SPA-NAI-H04-B-TPTID (IDX)
                                           >   0 )
      *            患者番号設定で設定したとき
                   STRING  "*"             DELIMITED  BY  SIZE
                           SPA-GMN-H04-B-TPTNUM     (IDX)
                                           DELIMITED  BY  SIZE
                                           INTO    H04-KANRI-TPTNUM(IDX)
                   END-STRING
               ELSE
                   MOVE    SPA-GMN-H04-B-TPTNUM     (IDX)
                                           TO  H04-KANRI-TPTNUM    (IDX)
               END-IF
               MOVE    SPA-GMN-H04-B-TNAME      (IDX)
                                           TO  H04-KANRI-TNAME     (IDX)
               MOVE    SPA-GMN-H04-B-TSRYYM     (IDX)
                                           TO  H04-KANRI-TSRYYM    (IDX)
               MOVE    SPA-NAI-H04-B-TRECESYUBETU (IDX)
                                           TO
                                           H04-KANRI-TRECESYUBETU (IDX)
               IF     SPA-GMN-H04-B-THKNJANUM  (IDX)
                                   NOT =   "00000000"
                   MOVE    SPA-GMN-H04-B-THKNJANUM  (IDX)
                                           TO  H04-KANRI-THKNJANUM (IDX)
               END-IF
               MOVE    SPA-GMN-H04-B-TKIGO      (IDX)
                                           TO  H04-KANRI-TKIGO     (IDX)
               EVALUATE    SPA-NAI-H04-B-TRECEIPT-KBN (IDX)(1:1)
                   WHEN    "1"
                       MOVE    "老併"      TO  H04-KANRI-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "2"
                       MOVE    "老健"      TO  H04-KANRI-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "3"
                       MOVE    "医併"      TO  H04-KANRI-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "4"
                       MOVE    "医療"      TO  H04-KANRI-TRECEIPT-KBN
                                                                  (IDX)
               END-EVALUATE
               EVALUATE    SPA-NAI-H04-B-TDISASTER (IDX)
                   WHEN    "00"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "災"            TO  H04-KANRI-TDISASTER
                                                                  (IDX)
               END-EVALUATE
               EVALUATE    SPA-NAI-H04-B-TSHIKAKU-INFO-FLG (IDX)
                   WHEN    "0"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "資"            TO  H04-KANRI-TDISASTER
                                                                  (IDX)
               END-EVALUATE
               MOVE    SPA-GMN-H04-B-TERR      (IDX)
                                           TO  H04-KANRI-TERR     (IDX)
               MOVE    SPA-GMN-H04-B-TERRCD    (IDX)
                                           TO  H04-KANRI-TERRCD   (IDX)
      *
               IF      SPA-H01NAI-PTID     >   ZERO
                   IF      SPA-H01NAI-PTID     =
                                               SPA-NAI-H04-B-TPTID (IDX)
                       MOVE    "#FFCC99"       TO
                                           H04-KANRI-LIST-BGCOLOR (IDX)
                   END-IF
               END-IF
      *
               IF      SPA-GMN-H04-B-TERRCD (IDX)
                                        NOT =   SPACE
                   MOVE    "#4169E1"       TO
                                           H04-KANRI-LIST-FGCOLOR (IDX)
               ELSE
                   IF      SPA-NAI-H04-B-TERRFLG (IDX)
                                               >   0
                       MOVE    "#FF0000"       TO
                                           H04-KANRI-LIST-FGCOLOR (IDX)
                   END-IF
               END-IF
      *         
               IF      IDX                 =   SPA-NAI-H04-SELNUM
                   MOVE    "T"                 TO  H04-KANRI-LIST-SELECT
                                                                   (IDX)
               ELSE
                   MOVE    "F"                 TO  H04-KANRI-LIST-SELECT
                                                                   (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-H04-BODY-MAX    TO  H04-KANRI-LIST-COUNT
      *
           MOVE    ZERO                    TO  H04-KANRI-LIST-ROW
           IF      WRK-H04-KANRI-ROW       >   ZERO
               MOVE    WRK-H04-KANRI-ROW       TO  H04-KANRI-LIST-ROW
           END-IF
      *
      ***  MOVE    SPA-GMN-H03-B-HYODAI    TO  H04-KANRI-LIST-HYODAI
      ***  MOVE    SPA-GMN-H03-B-HYODAI2   TO  H04-KANRI-LIST-HYODAI2
      *
           MOVE    SPA-GMN-H04-COMMENT     TO  H04-HENREI-JIYU
           MOVE    SPA-GMN-H04-SELNUM      TO  H04-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-GMN-H04-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-H04-CUR
               WHEN    1
                   MOVE  "SELNUM"          TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"             TO  MCP-WIDGET   
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    1               TO  SPA-GMN-H04-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           MOVE    SPACE                TO  HERR
           MOVE    SPA-ERRCD            TO  HERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"        TO  HERR-ERRMSG
               WHEN    "0005"
                   MOVE    "前ページはありません"
                                        TO  HERR-ERRMSG
               WHEN    "0006"
                   MOVE    "次ページはありません"
                                        TO  HERR-ERRMSG
           END-EVALUATE
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "H04"                TO  SPA-MOTOPG
           MOVE    "HERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻管理読み込み処理
      *****************************************************************
       900-HENREI-KANRI-READ-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE   ZERO                 TO  FLG-HENREI-K
               MOVE   MCPDATA-REC          TO  HENREI-K-REC
           ELSE
               INITIALIZE                      HENREI-K-REC
               MOVE   1                    TO  FLG-HENREI-K
           END-IF
           .
       900-HENREI-KANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-SEC        SECTION.
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE  MCPDATA-REC           TO  CHK-HENREI-B-REC
                   MOVE  ZERO                  TO  FLG-HENREI-B-CHK
               ELSE
                   INITIALIZE                      CHK-HENREI-B-REC
                   MOVE  1                     TO  FLG-HENREI-B-CHK
               END-IF
           ELSE
               INITIALIZE                      CHK-HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B-CHK
           END-IF
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HENREI-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-KEY4-SEC   SECTION.
      *
           MOVE    "tbl_henrei_body"       TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-KEY4-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-KEY10-SEC   SECTION.
      *
           MOVE    "tbl_henrei_body"       TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-KEY10-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
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
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
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
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
