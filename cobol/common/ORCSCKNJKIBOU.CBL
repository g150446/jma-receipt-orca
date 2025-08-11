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
       PROGRAM-ID.             ORCSCKNJKIBOU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 長期収載品の患者負担額　編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  24/09/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TOKUYKZMONY         PIC 9(07).
           03  WRK-TOUKU-TAX           PIC 9(07).
           03  WRK-TOUKU-MONY          PIC 9(07).
           03  WRK-KEI-Z9-X.
               05  WRK-KEI-Z9          PIC ZZZZZZ9.
           03  WRK-ZAIKEI2             PIC X(40).
           03  WRK-ZAIKEIJ2            PIC X(40).
           03  WRK-KEI-H               PIC X(80).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *    COPY    "CPSYSKANRI.INC".
      *    処方箋ＧＥ表示
      *    COPY    "CPSK1030.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
      *     COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
      *  01 TENSUPLUS-REC.
      *     COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
      * 01  SRYACT-REC.
      *     COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
      * 01  SRYACCT-REC.
      *    COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタワーク
      * 01  TBL-SRYACT-AREA.
      *     02  TBL-SRYACT-REC         OCCURS   10.
      *     COPY    "CPSRYACT.INC"     REPLACING
      *                                //SRY-// BY //TBL-SRY-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    長期収載品の患者負担額編集パラメタ
           COPY    "CPORCSCKNJKIBOU.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                   DIVISION    USING
           ORCSCKNJKIBOUAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSCKIBOU-OUT-AREA
      *
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　　処理
      *****************************************************************
       100-INIT-SEC                      SECTION.
      *
      *    回数
           IF      ORCSCKIBOU-ZAIKAISU     =   ZERO
               MOVE    1                   TO  ORCSCKIBOU-ZAIKAISU
           END-IF
      *
      *    診療日付
           IF      ORCSCKIBOU-SRYYMD    =   SPACE
               MOVE    SPA-SRYYMD           TO  WRK-SRYYMD
           ELSE
               MOVE    ORCSCKIBOU-SRYYMD    TO  WRK-SRYYMD
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    特別料金計算
           MOVE    ZERO                TO  WRK-TOKUYKZMONY
           MOVE    ZERO                TO  WRK-TOUKU-TAX
      *    特別料金　剤点数　×　回数 
           COMPUTE WRK-TOKUYKZMONY     =   ORCSCKIBOU-CYOKISENTEITEN
                                       *   ORCSCKIBOU-ZAIKAISU
                                       *   10
      *    消費税(10%)
           COMPUTE WRK-TOUKU-TAX      =    WRK-TOKUYKZMONY
                                      *    0.10
           COMPUTE WRK-TOUKU-MONY     =    WRK-TOKUYKZMONY
                                      +    WRK-TOUKU-TAX
      *    特別料金（税込み）
           MOVE    WRK-TOUKU-MONY     TO  ORCSCKIBOU-TOKMONEY
      *    内消費税
           MOVE    WRK-TOUKU-TAX      TO  ORCSCKIBOU-TOKMONEY-TAX
      *
           IF      ORCSCKIBOU-SYORIKBN2   =   "1"
      *        全角変換
               PERFORM 2001-ZENKAKUHEN-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2001-ZENKAKUHEN-SEC      SECTION.
      *
      *    特別料金（税込み）
      *    前ずめ
           MOVE    ZERO                TO  IDK2
           MOVE    WRK-TOUKU-MONY      TO  WRK-KEI-Z9
           MOVE    SPACE               TO  WRK-KEI-H
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   7
               IF      WRK-KEI-Z9-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KEI-Z9-X   (IDK1:1)
                                       TO  WRK-KEI-H   (IDK2:1)
               END-IF
           END-PERFORM
           MOVE    SPACE                   TO  WRK-ZAIKEI2
           STRING   WRK-KEI-H              DELIMITED   BY  SPACE
                   "円"                    DELIMITED   BY  SIZE
                   INTO                    WRK-ZAIKEI2
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI2         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  ORCSCKIBOU-TOKMONEY-MEI
      *    うしろづめ
           MOVE    SPACE                   TO  WRK-ZAIKEI2
           STRING   WRK-KEI-Z9             DELIMITED   BY  SIZE
                   "円"                    DELIMITED   BY  SIZE
                   INTO                    WRK-ZAIKEI2
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI2         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  ORCSCKIBOU-TOKMONEY-MEI2
      *
      *    消費税（前詰めとする）
           MOVE    ZERO                TO  IDK2
           MOVE    WRK-TOUKU-TAX       TO  WRK-KEI-Z9
           MOVE    SPACE               TO  WRK-KEI-H
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   7
               IF      WRK-KEI-Z9-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KEI-Z9-X   (IDK1:1)
                                       TO  WRK-KEI-H   (IDK2:1)
               END-IF
           END-PERFORM
           MOVE    SPACE                   TO  WRK-ZAIKEI2
           STRING  "内消費税"
                   WRK-KEI-H               DELIMITED   BY  SPACE
                   "円"                    DELIMITED   BY  SIZE
                   INTO                    WRK-ZAIKEI2
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI2         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  ORCSCKIBOU-TOKMONEY-TAXMEI
      *
           .
       2001-ZENKAKUHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
