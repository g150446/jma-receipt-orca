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
       PROGRAM-ID.             ORCSMSTVERCHECK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 起動時マスタデータ移行チェック結果取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  20/12/01    NMED−伊藤    新規作成
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
      *    プログラム更新　管理ファイル
           SELECT  PGUP-FILE       ASSIGN  FILE-PGUP
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PGUP.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       FD  PGUP-FILE.
       01  PGUP-REC.
           03  PGUP-TIMES              PIC 9(04).
           03  FILLER                  PIC X(01).
           03  PGUP-UPDATEFLG          PIC X(01).
           03  FILLER                  PIC X(01).
           03  PGUP-RELEASEYMD         PIC X(08).
           03  FILLER                  PIC X(01).
           03  PGUP-COMMENT            PIC X(100).
           03  FILLER                  PIC X(01).
           03  PGUP-MST-KANRICD        PIC X(08).
           03  FILLER                  PIC X(01).
           03  PGUP-MST-VERSION        PIC X(64).
      *
       WORKING-STORAGE             SECTION.
      *
       01  STS-AREA.
           03  STS-PGUP            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-PGKANRI         PIC 9(01).
           03  FLG-MSTKANRI        PIC 9(01).
      *
       01  FILE-AREA.
           03  FILE-PGUP           PIC X(48)  VALUE
               "/usr/lib/jma-receipt/patch-lib/patch-program.prf".
      *
       01  WRK-AREA.
           03  WRK-PGK-PATCH-YMD   PIC X(08).
           03  WRK-MST-PATCH-YMD   PIC X(08).
      *
       01  MSTKANRI-REC.
           COPY    "CPMSTKANRI.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           .
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE  STS-AREA
           INITIALIZE  FLG-AREA
           INITIALIZE  WRK-AREA
      *
           PERFORM 100-PHASE1-SEC
      **** DISPLAY "PHASE1 : [ " SPA-ERRCD " ]"
           IF      SPA-ERRCD           =   SPACE
               PERFORM 100-PHASE2-SEC
      ****     DISPLAY "PHASE2 : [ " SPA-ERRCD " ]"
           END-IF
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *****************************************************************
      *    フェーズ１処理（パッチからのチェック）
      *****************************************************************
       100-PHASE1-SEC              SECTION.
      *
           OPEN    INPUT   PGUP-FILE
           IF      STS-PGUP        NOT =   ZERO
               MOVE    1                   TO  FLG-PGKANRI
           ELSE
               PERFORM 900-PROOF-READ-SEC
           END-IF
      *
           PERFORM UNTIL   FLG-PGKANRI =   1
               IF      PGUP-TIMES      NOT =   "9999"
                   IF      PGUP-MST-KANRICD (1:6)
                                               =   "ORCADB"
                       PERFORM 900-MSTKANRI-READ-SEC
                       IF      FLG-MSTKANRI        =   1
                           MOVE    "1024"              TO  SPA-ERRCD
                       ELSE
      ****             DISPLAY "MSTKANRI  VER [ " MSTKANRI-DBRVERSION2
      ****                     " ]"
      ****             DISPLAY "PATCH MST-VER [ " PGUP-MST-VERSION " ]"
                           IF      MSTKANRI-DBRVERSION2
                                                   <   PGUP-MST-VERSION
                               MOVE    "1024"              TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
                   IF      PGUP-RELEASEYMD     >   WRK-PGK-PATCH-YMD
                       MOVE    PGUP-RELEASEYMD     TO  WRK-PGK-PATCH-YMD
                   END-IF
               END-IF
               PERFORM 900-PROOF-READ-SEC
           END-PERFORM
           CLOSE   PGUP-FILE
           .
       100-PHASE1-EXT.
           EXIT.
      *****************************************************************
      *    フェーズ２処理（マスタ更新からのチェック）
      *****************************************************************
       100-PHASE2-SEC              SECTION.
      *
           INITIALIZE  MSTKANRI-REC
           MOVE    1                   TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_mstkanri"      TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 900-MSTKANRI-FETCH-SEC
           ELSE
               INITIALIZE  MSTKANRI-REC
               MOVE    1                   TO  FLG-MSTKANRI
           END-IF
      *
           PERFORM UNTIL   FLG-MSTKANRI    =   1
               IF      MSTKANRI-PATCH-YMD  NOT =   SPACE
                   IF      MSTKANRI-PATCH-YMD  >   WRK-MST-PATCH-YMD
                       MOVE    MSTKANRI-PATCH-YMD  TO  WRK-MST-PATCH-YMD
                   END-IF
               END-IF
               MOVE    "tbl_mstkanri"      TO  MCP-TABLE
               MOVE    "all"               TO  MCP-PATHNAME
               PERFORM 900-MSTKANRI-FETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      **** DISPLAY "PGKANRI  [ " WRK-PGK-PATCH-YMD " ]"
      **** DISPLAY "MSTKANRI [ " WRK-MST-PATCH-YMD " ]"
           IF      WRK-PGK-PATCH-YMD   <   WRK-MST-PATCH-YMD
               MOVE    "1024"              TO  SPA-ERRCD
           END-IF
           .
       100-PHASE2-EXT.
           EXIT.
      *****************************************************************
      *    プルーフ読み込み処理
      *****************************************************************
       900-PROOF-READ-SEC          SECTION.
           READ    PGUP-FILE
               AT END
                   MOVE    1                   TO  FLG-PGKANRI
           END-READ
           .
       900-PROOF-READ-EXT.
           EXIT.
      *****************************************************************
      *    マスタ管理読込処理
      *****************************************************************
       900-MSTKANRI-READ-SEC       SECTION.
           INITIALIZE  MSTKANRI-REC
           MOVE    1                   TO  MSTKANRI-HOSPNUM
           MOVE    PGUP-MST-KANRICD    TO  MSTKANRI-KANRICD
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_mstkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-MSTKANRI-FETCH-SEC
           ELSE
               INITIALIZE  MSTKANRI-REC
               MOVE    1                   TO  FLG-MSTKANRI
           END-IF
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-MSTKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    マスタ管理読込処理
      *****************************************************************
       900-MSTKANRI-FETCH-SEC      SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  MSTKANRI-REC
               MOVE    ZERO                TO  FLG-MSTKANRI
           ELSE
               INITIALIZE  MSTKANRI-REC
               MOVE    1                   TO  FLG-MSTKANRI
           END-IF
           .
       900-MSTKANRI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
