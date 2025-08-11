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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSBARCODE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 印刷
      *  コンポーネント名  : バーコードファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/11/29    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-太田    14/08/06  4.8クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  BARCODE-FILE    ASSIGN  ASGN-BARCODE
                                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-BARCODE.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
       FD  BARCODE-FILE.
       01  BARCODE-REC.
           03  BARCODE-UID         PIC X(36).
      *
       WORKING-STORAGE             SECTION.
      *
       01  ASGN-BARCODE            PIC X(1024).
      *
       01  STS-AREA.
           03  STS-BARCODE         PIC X(02).
      *
       01  WRK-AREA.
           03  WRK-BASE-BARCODE    PIC X(1024).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    シェル
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSBARCODE.INC".
           COPY    "COMMON-SPA".
      *
      **************************************************************************
       PROCEDURE           DIVISION USING
               SBARCODE-AREA
               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
      *
           INITIALIZE              UIDIO-AREA
           CALL    "orcuidset"     USING
                                   UIDIO-ST
                                   UIDIO-AREA
           MOVE    UIDIO-UID       TO  WRK-BASE-BARCODE
      *
           INITIALIZE              SGETTEMP-AREA
           MOVE    WRK-BASE-BARCODE
                                   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGN-BARCODE
      *
           MOVE    SPACE           TO  SBARCODE-UID
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
           MOVE    "generate_barcode.sh"
                                       TO  SHELLTBL-NAME
           MOVE    SBARCODE-CODES      TO  SHELLTBL-ARG1
           MOVE    WRK-BASE-BARCODE    TO  SHELLTBL-ARG2
           IF    ( SBARCODE-REMOVE-IMAGE   NOT =   ZERO )
               MOVE    "T"             TO  SHELLTBL-ARG3
           ELSE
               MOVE    "F"             TO  SHELLTBL-ARG3
           END-IF
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "EXCOMMAND"         TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           OPEN INPUT BARCODE-FILE
           READ BARCODE-FILE
           END-READ
           IF    ( STS-BARCODE         =   ZERO )
            AND  ( BARCODE-UID     NOT =   SPACE )
               MOVE    BARCODE-UID     TO  SBARCODE-UID
               MOVE    SPACE           TO  SBARCODE-FILENAME
               STRING  ASGN-BARCODE        DELIMITED BY SPACE
                       ".png"              DELIMITED BY SIZE
               INTO    SBARCODE-FILENAME
               END-STRING
           END-IF
           CLOSE   BARCODE-FILE
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ASGN-BARCODE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
