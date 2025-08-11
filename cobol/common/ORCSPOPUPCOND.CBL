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
       PROGRAM-ID.             ORCSPOPUPCOND.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 資格確認
      *  コンポーネント名  : 状態通知処理
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  22/08/17                  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-ONSCOND         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TIME.
               05  WRK-HH              PIC 9(02).
               05  WRK-MM              PIC 9(02).
               05  WRK-SSSS            PIC 9(04).
      *
           03  WRK-MEMO-BODY           PIC X(1060).
           03  WRK-MEMO-BODY2          PIC X(1060).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENTIME.
               05  WRK-THH         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TSS         PIC X(04).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    資格確認状態通知テーブル
       01  ONSCOND-REC.
           COPY    "CPONSHI-COND.INC".
      *
      *    共通領域
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    患者情報通知パラメタ
           COPY    "CPORCSPOPUPCOND.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SPA-AREA
           ORCSPOPUPCONDAREA
           .
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSPOPUPCOND-BODY
           INITIALIZE                  ORCSPOPUPCOND-SUMMARY
      *
      *    表示済みレコード削除
           PERFORM 220-ONSHICOND-DLT-SEC
      *
           PERFORM 210-ONSHICOND-SYORI-SEC
           IF      FLG-ONSCOND         =   1
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    見出し編集
           MOVE    "資格確認状態通知"  TO  ORCSPOPUPCOND-SUMMARY
      *
      *    出力内容編集
           IF      WRK-MEMO-BODY   NOT =   SPACE
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
      *        出力桁数
               MOVE    1000                TO  KANACHK-MAX-CNT
               MOVE    WRK-MEMO-BODY       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  ORCSPOPUPCOND-BODY
           END-IF
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    資格確認状態通知編集処理
      *****************************************************************
       210-ONSHICOND-SYORI-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-MEMO-BODY
      *
           PERFORM 2101-ONSHICOND-SEL-SEC
           IF      FLG-ONSCOND         =   1
               GO  TO  210-ONSHICOND-SYORI-EXT
           END-IF
      *
      *障害発生時間
           MOVE    ONSCOND-FAILURE-TIME(1:2)
                                       TO  WRK-THH
           MOVE    ONSCOND-FAILURE-TIME(3:2)
                                       TO  WRK-TMM
           MOVE    ONSCOND-FAILURE-TIME(5:4)
                                       TO  WRK-TSS
           STRING  "【発生時間："          DELIMITED BY  SIZE
                   WRK-HENTIME             DELIMITED BY  SIZE
                   "】"                    DELIMITED BY  SIZE
                   X"0A"                   DELIMITED BY  SIZE
                                       INTO    WRK-MEMO-BODY
           END-STRING
      *端末番号
           IF      ONSCOND-TERM-NO NOT =   ZERO
               MOVE    ZERO                TO  IDX
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL  (IDY        >    1000)
                       OR     (WRK-MEMO-BODY(IDY:) =   SPACE)
                   MOVE   IDY                  TO  IDX
               END-PERFORM
               MOVE    WRK-MEMO-BODY       TO  WRK-MEMO-BODY2
               MOVE    SPACE               TO  WRK-MEMO-BODY
               STRING  WRK-MEMO-BODY2(1:IDX)
                                           DELIMITED BY  SIZE
                       "【端末番号："      DELIMITED BY  SIZE
                       ONSCOND-TERM-NO     DELIMITED BY  SIZE
                       "】"                DELIMITED BY  SIZE
                       X"0A"               DELIMITED BY  SIZE
                                       INTO    WRK-MEMO-BODY
               END-STRING
           END-IF
      *端末名称
           IF      ONSCOND-TERM-NAME
                                   NOT =   SPACE
               MOVE    ZERO                TO  IDX
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL  (IDY        >    1000)
                       OR     (WRK-MEMO-BODY(IDY:) =   SPACE)
                   MOVE   IDY                  TO  IDX
               END-PERFORM
               MOVE    WRK-MEMO-BODY       TO  WRK-MEMO-BODY2
               MOVE    SPACE               TO  WRK-MEMO-BODY
               STRING  WRK-MEMO-BODY2(1:IDX)
                                           DELIMITED BY  SIZE
                       "【端末名称："      DELIMITED BY  SIZE
                       ONSCOND-TERM-NAME   DELIMITED BY  SPACE
                       "】"                DELIMITED BY  SIZE
                       X"0A"               DELIMITED BY  SIZE
                                       INTO    WRK-MEMO-BODY
               END-STRING
           END-IF
      *エラーコード、エラーメッセージ
           MOVE    ZERO                TO  IDX
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL  (IDY        >    1000)
                   OR     (WRK-MEMO-BODY(IDY:) =   SPACE)
               MOVE   IDY                  TO  IDX
           END-PERFORM
           MOVE    WRK-MEMO-BODY       TO  WRK-MEMO-BODY2
           MOVE    SPACE               TO  WRK-MEMO-BODY
           STRING  WRK-MEMO-BODY2(1:IDX)   DELIMITED BY  SIZE
                   "【コード："            DELIMITED BY  SIZE
                   ONSCOND-ERR-CODE        DELIMITED BY  SIZE
                   "】"                    DELIMITED BY  SIZE
                   X"0A"                   DELIMITED BY  SIZE
                   ONSCOND-ERR-MSG         DELIMITED BY  SPACE
                                       INTO    WRK-MEMO-BODY
           END-STRING
      *
      *    表示済みとして削除フラグを立てておく
           PERFORM 2101-ONSHICOND-FLG-SEC
           .
       210-ONSHICOND-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    資格確認状態通知抽出処理
      *****************************************************************
       2101-ONSHICOND-SEL-SEC      SECTION.
      *
           INITIALIZE                      ONSCOND-REC
           MOVE    SPA-HOSPNUM         TO  ONSCOND-HOSPNUM
           MOVE    SPA-SYSYMD          TO  ONSCOND-FAILURE-DATE
           MOVE    ONSCOND-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_cond"    TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-ONSCOND-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSCOND
           END-IF
           MOVE    "tbl_onshi_cond"    TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2101-ONSHICOND-SEL-EXT.
           EXIT.
      *****************************************************************
      *    資格確認状態通知フラグ処理
      *****************************************************************
       2101-ONSHICOND-FLG-SEC      SECTION.
      *
           MOVE    1                   TO  ONSCOND-DLTFLG
           MOVE    ONSCOND-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_cond"    TO  MCP-TABLE
           MOVE    "updflg"            TO  MCP-PATHNAME
           PERFORM 930-DBUPDATE-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  FLG-ONSCOND
           END-IF
           .
       2101-ONSHICOND-FLG-EXT.
           EXIT.
      *****************************************************************
      *    資格確認状態通知削除処理
      *****************************************************************
       220-ONSHICOND-DLT-SEC       SECTION.
      *
      *    時間取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"   USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-TIME       TO  WRK-TIME
           IF      WRK-TIME(4:1)       NOT =   "0"
               GO  TO  220-ONSHICOND-DLT-EXT
           END-IF
      *    １時間前
           IF      WRK-HH              =   ZERO
               MOVE    23                  TO  WRK-HH
           ELSE
               COMPUTE WRK-HH              =   WRK-HH  -   1
           END-IF
           IF      WRK-HH              =   ZERO
               MOVE    24                  TO  WRK-HH
           END-IF
           MOVE    ZERO                TO  WRK-SSSS
      *
           INITIALIZE                      ONSCOND-REC
           MOVE    SPA-HOSPNUM         TO  ONSCOND-HOSPNUM
           MOVE    SPA-SYSYMD          TO  ONSCOND-FAILURE-DATE
           MOVE    WRK-TIME            TO  ONSCOND-FAILURE-TIME
           MOVE    ONSCOND-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_cond"    TO  MCP-TABLE
           MOVE    "dlt1"              TO  MCP-PATHNAME
           PERFORM 940-DBDELETE-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  FLG-ONSCOND
           END-IF
           .
       220-ONSHICOND-DLT-EXT.
           EXIT.
      *****************************************************************
      *    資格確認状態通知読込
      *****************************************************************
       900-ONSCOND-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSCOND-REC
               MOVE    ZERO                TO  FLG-ONSCOND
           ELSE
               INITIALIZE                      ONSCOND-REC
               MOVE    1                   TO  FLG-ONSCOND
           END-IF
           .
       900-ONSCOND-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読込み処理
      *****************************************************************
       920-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       930-DBUPDATE-SEC            SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       930-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       940-DBDELETE-SEC            SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       940-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
