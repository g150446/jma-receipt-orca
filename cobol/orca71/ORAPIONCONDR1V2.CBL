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
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.          ORAPIONCONDR1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : オンライン資格確認
      *  管理者            :
      *  作成日付    作業者        記述
      *  22/08/03                  新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-ONSHI-COND          PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-FAILURE-DATE        PIC X(08).
           03  WRK-FAILURE-TIME        PIC X(08).
           03  WRK-ERR-CODE            PIC X(09).
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONSHICONDREQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPONSHICONDRES.INC".
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    状態通知テーブル
       01  ONSCOND-REC.       
           COPY    "CPONSHI-COND.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA71SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "*********************"
           DISPLAY   "*  onshicond start  *"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
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
           IF      WRK-ERRCD           =   "999"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST02-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "*  onshicond end  *"
           DISPLAY   "*******************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      XML-ONSHICONDRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "999"               TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      *
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
               MOVE   "E89"            TO  WRK-ERRCD
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    状態通知登録処理
           PERFORM 2002-ONSHI-COND-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST02-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONSHICONDREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onshicondreq"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST02-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onshicondreq"      TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONSHICONDREQ
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "000"               TO  ONCONRES-API-RESULT
               MOVE    "処理終了"          TO  ONCONRES-API-RESULT-MSG
           ELSE
               DISPLAY "300 WRK-ERRCD=" WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  ONCONRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  ONCONRES-API-RESULT-MSG
            END-IF
      *
           IF      APILST02-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onshicondres"  TO  MCP-TABLE
           MOVE    1                   TO  ONCONRES-MODE
           MOVE    "onshicondres"      TO  ONCONRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONSHICONDRES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  ONCONRES-OBJECT       TO  APILST02-BODY
               MOVE  "application/xml"     TO  APILST02-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
           .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           IF      XML-ONSHICONDREQ    =   SPACE
               MOVE    "E01"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    障害発生日
           IF      ONCONREQ-FAILURE-DATE
                                       =   SPACE
               DISPLAY "FAILURE-DATE ERROR"
               MOVE    "E11"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *    障害発生時間
           IF      ONCONREQ-FAILURE-TIME
                                       =   SPACE
               DISPLAY "FAILURE-TIME ERROR"
               MOVE    "E12"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *    エラーコード
           IF      ONCONREQ-ERR-CODE   =   SPACE
               DISPLAY "ERR-CODE ERROR"
               MOVE    "E13"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *    エラーメッセージ
           IF      ONCONREQ-ERR-MSG    =   SPACE
               DISPLAY "ERR-MSG ERROR"
               MOVE    "E14"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     状態通知登録処理
      *****************************************************************
       2002-ONSHI-COND-SEC         SECTION.
      *
           MOVE    ONCONREQ-FAILURE-DATE
                                       TO  WRK-FAILURE-DATE
           MOVE    ONCONREQ-FAILURE-TIME
                                       TO  WRK-FAILURE-TIME
           MOVE    ONCONREQ-ERR-CODE   TO  WRK-ERR-CODE
      *    同一キーチェック
           MOVE    "key"               TO  WRK-PATHNAME
           PERFORM 900-ONSHI-COND-READ-SEC
           IF      FLG-ONSHI-COND      =   ZERO
               DISPLAY "TBL_ONSHI_COND KEY CHECK"
               MOVE    "E21"           TO  WRK-ERRCD
               GO  TO  2002-ONSHI-COND-EXT
           END-IF
      *    同一エラーコードチェック
           MOVE    "key2"              TO  WRK-PATHNAME
           PERFORM 900-ONSHI-COND-READ-SEC
           IF      FLG-ONSHI-COND      =   ZERO
               DISPLAY "TBL_ONSHI_COND ERR-CODE CHECK"
               MOVE    "E22"           TO  WRK-ERRCD
               GO  TO  2002-ONSHI-COND-EXT
           END-IF
      *    状態通知登録
           PERFORM 20022-ONSHI-COND-INS-SEC
           .
       2002-ONSHI-COND-EXT.
           EXIT.
      *
      *****************************************************************
      *     状態通知登録処理
      *****************************************************************
       20022-ONSHI-COND-INS-SEC    SECTION.
      *
           INITIALIZE                      ONSCOND-REC
           MOVE    SPA-HOSPNUM         TO  ONSCOND-HOSPNUM
           MOVE    ONCONREQ-FAILURE-DATE
                                       TO  ONSCOND-FAILURE-DATE
           MOVE    ONCONREQ-FAILURE-TIME
                                       TO  ONSCOND-FAILURE-TIME
           MOVE    ONCONREQ-TERM-NO    TO  ONSCOND-TERM-NO
           MOVE    ONCONREQ-TERM-NAME  TO  ONSCOND-TERM-NAME
           MOVE    ONCONREQ-ERR-CODE   TO  ONSCOND-ERR-CODE
           MOVE    ONCONREQ-ERR-MSG    TO  ONSCOND-ERR-MSG
           MOVE    SMCNDATE-YMD        TO  ONSCOND-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONSCOND-CREHMS
           MOVE    ONSCOND-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_cond"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "006 ONSHI-COND INSERT ERR:"  MCP-RC
                       ",KEY:" ONSCOND-KEY
           END-IF
           .
       20022-ONSHI-COND-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E01"
                   MOVE    "リクエストが空です。"
                                                   TO  WRK-ERRMSG
      *        共通エラー
               WHEN    "E89"
      *            ORCSCOMMON のエラー
                   EVALUATE    SPA-ERRCD
                       WHEN    "1001"
                           MOVE    "職員情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1002"
                           MOVE    "医療機関情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1003"
                           MOVE    "システム日付が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1005"
                           MOVE    "患者番号構成情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1015"
                           STRING  "グループ医療機関が不整合です。"
                               "処理を終了して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                           END-STRING
                       WHEN    OTHER
                           MOVE    "システム項目が設定できません。"
                                                   TO  WRK-ERRMSG
                   END-EVALUATE
               WHEN    "E90"
                   MOVE    "他端末で使用中です。"  TO  WRK-ERRMSG
               WHEN    "E91"
                   MOVE    "リクエスト番号が不正です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E97"
                   MOVE   "送信内容に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E98"
                   MOVE   "送信内容の読込ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E99"
                   MOVE    "ユーザＩＤが未登録です。"
                                                   TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    状態通知読込
      *****************************************************************
       900-ONSHI-COND-READ-SEC     SECTION.
      *
           INITIALIZE                      ONSCOND-REC
           MOVE    SPA-HOSPNUM         TO  ONSCOND-HOSPNUM
           MOVE    WRK-FAILURE-DATE    TO  ONSCOND-FAILURE-DATE
           MOVE    WRK-FAILURE-TIME    TO  ONSCOND-FAILURE-TIME
           MOVE    WRK-ERR-CODE        TO  ONSCOND-ERR-CODE
           MOVE    ONSCOND-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_cond"    TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_cond"    TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ONSCOND-REC
                   MOVE    ZERO                TO  FLG-ONSHI-COND
               ELSE
                   INITIALIZE                      ONSCOND-REC
                   MOVE    1                   TO  FLG-ONSHI-COND
               END-IF
           ELSE
               INITIALIZE                      ONSCOND-REC
               MOVE    1                   TO  FLG-ONSHI-COND
           END-IF
           MOVE    "tbl_onshi_cond"    TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-ONSHI-COND-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
       900-ORCDBMAIN-EXT.
           EXIT.
