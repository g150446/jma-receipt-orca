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
       PROGRAM-ID.                 ORCSONSHIIMG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 資格確認
      *  コンポーネント名  : 画像イメージ読込
      *  管理者            :
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.01    ORCAMO       23/01/11  画面からの画像削除処理の中止
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    イメージデータファイル
           SELECT  IMAGE-FILE      ASSIGN  IMAGE-FILENAME
                                   ORGANIZATION    IS  LINE  SEQUENTIAL
                                   FILE    STATUS  IS  STS-IMAGE.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    イメージデータファイル
       FD  IMAGE-FILE.
       01  IMAGE-REC               PIC X(500000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IMAGE           PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OCRIMAGE        PIC 9(01).
      *
      *    イメージデータファイル名
       01  IMAGE-FILENAME          PIC X(512).
       01  MED-FILENAME            PIC X(512).
      *
      *    コマンド
       01  COMMAND                 PIC X(1024).
      *
      *    返却値
       01  RESULT                  PIC S9(09).
      *
      *    画像ファイルフォルダ名
       01  WRK-CONS-AREA.
           03  WRK-CONS-MEDIMGDIR  PIC X(20)   
                                   VALUE   "onshiimage".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  ONSHI-IMAGE-REC.
           COPY    "CPONSHI-IMAGE.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ファイル存在チェック
           COPY    "CPORCSACCESS.INC".
      *
      *    共通パラメタ
           COPY    "MCPDATA.INC".
      *
       01  MCPDATA.
           03  MCPREC              PIC X(510000).
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSONSHIIMG.INC".
           COPY    "COMMON-SPA".
           COPY    "MCPAREA".
      *
       PROCEDURE                   DIVISION
                                       USING
                                       ORCSONSHIIMGAREA
                                       SPA-AREA
                                       MCPAREA
           .
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-TERM-SEC
           .
       000-PROC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *     DISPLAY "LNK-ONSHIIMG-SYOKBN=" LNK-ONSHIIMG-SYOKBN
      *     DISPLAY "LNK-ONSHIIMG-IMGKBN=" LNK-ONSHIIMG-IMGKBN
      *     DISPLAY "LNK-ONSHIIMG-FILE-NAME=" LNK-ONSHIIMG-FILE-NAME
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
      *
      *    処理区分チェック
           EVALUATE    LNK-ONSHIIMG-SYOKBN
               WHEN    "01"
               WHEN    "02"
                   CONTINUE
               WHEN    OTHER
                   MOVE    1               TO  LNK-ONSHIIMG-RC
                   MOVE    1               TO  FLG-END
                   GO  TO  100-INIT-EXT
           END-EVALUATE
      *
      *    画像区分チェック
           EVALUATE    LNK-ONSHIIMG-IMGKBN
               WHEN    "01"
                   CONTINUE
               WHEN    OTHER
                   MOVE    1               TO  LNK-ONSHIIMG-RC
                   MOVE    1               TO  FLG-END
                   GO  TO  100-INIT-EXT
           END-EVALUATE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    LNK-ONSHIIMG-SYOKBN
               WHEN    "01"
                   PERFORM 2001-IMAGE-GET-SEC
               WHEN    "02"
                   MOVE    1               TO  FLG-END
      *************PERFORM 2001-IMAGE-DEL-SEC
           END-EVALUATE
            .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像取出し処理
      *****************************************************************
       2001-IMAGE-GET-SEC          SECTION.
      *
           INITIALIZE                  ONSHI-IMAGE-REC
           MOVE    SPA-HOSPNUM     TO  ONS-IMAGE-HOSPNUM
           MOVE    LNK-ONSHIIMG-FILE-NAME 
                                   TO  ONS-IMAGE-FILE-NAME
           PERFORM 900-OCRIMAGE-READ-SEC
           IF      FLG-OCRIMAGE    =   ZERO
               MOVE    ONS-IMAGE-FILE-STATUS
                                       TO  LNK-ONSHIIMG-IMG-STATUS
               IF      ONS-IMAGE-FILE-STATUS
                                   NOT =   "0"
                   MOVE    1               TO  FLG-END
                   GO  TO  2001-IMAGE-GET-EXT
               END-IF
      *
      *        イメージデータファイル名
               MOVE    SPACE           TO  IMAGE-FILENAME
               STRING  MCP-TEMPDIR         DELIMITED BY SPACE
                       "/"                 DELIMITED BY SIZE
                       LNK-ONSHIIMG-FILE-NAME
                                           DELIMITED BY SPACE
                       ".txt"              DELIMITED BY SIZE
                                   INTO    IMAGE-FILENAME
               END-STRING
      *
      *        イメージデータファイル作成
               OPEN    OUTPUT              IMAGE-FILE
               IF      STS-IMAGE   NOT =   ZERO
                   DISPLAY "ORCSMILOAD IMAGE FILE OPEN ERROR"
                   MOVE    "5"             TO  LNK-ONSHIIMG-IMG-STATUS
                   MOVE    1               TO  FLG-END
                   GO  TO  2001-IMAGE-GET-EXT
               ELSE
                   MOVE    ONS-IMAGE-DATA  TO  IMAGE-REC
                   WRITE   IMAGE-REC
                   IF      STS-IMAGE   NOT =   ZERO
                       DISPLAY "ORCSMILOAD IMAGE FILE WRITE ERROR"
                       MOVE    "6"         TO  LNK-ONSHIIMG-IMG-STATUS
                       MOVE    1           TO  FLG-END
                       GO  TO  2001-IMAGE-GET-EXT
                   END-IF    
                   CLOSE   IMAGE-FILE
               END-IF
      *
      *        画像保存用フォルダ作成コマンド
               MOVE    LOW-VALUE       TO  COMMAND
               STRING  "mkdir -p "         DELIMITED BY SIZE
                       MCP-TEMPDIR         DELIMITED BY SPACE
                       "/"                 DELIMITED BY SIZE
                       WRK-CONS-MEDIMGDIR  DELIMITED BY SIZE
                                   INTO    COMMAND
               END-STRING
      *        コマンド実行
               CALL    "system"            USING
                                           COMMAND
      *        画像データ変換後ファイル名称決定
      *        及び画像ファイル格納
               MOVE    SPACE           TO  MED-FILENAME
               STRING  MCP-TEMPDIR         DELIMITED BY SPACE
                       "/"                 DELIMITED BY SIZE
                       WRK-CONS-MEDIMGDIR  DELIMITED BY SPACE
                       "/"                 DELIMITED BY SIZE
                       LNK-ONSHIIMG-FILE-NAME
                                           DELIMITED BY SPACE
               INTO    MED-FILENAME
               END-STRING
      *
      *        イメージデータから画像データ変換コマンド
      *        及び画像ファイル格納
               MOVE    LOW-VALUE       TO  COMMAND
               STRING  "base64 -d "        DELIMITED BY SIZE
                       IMAGE-FILENAME      DELIMITED BY SPACE
                       " > "               DELIMITED BY SIZE
                       MED-FILENAME        DELIMITED BY SPACE
                       " || rm -f "        DELIMITED BY SIZE
                       MED-FILENAME        DELIMITED BY SPACE
               INTO    COMMAND
               END-STRING
      *
      *        コマンド実行
               CALL    "system"            USING
                                           COMMAND
      *
      *        ファイル存在チェック
               INITIALIZE                  SACCESS-AREA
               MOVE    MED-FILENAME    TO  SACCESS-IN-FILENAME
               CALL    "ORCSACCESS"        USING
                                           SACCESS-AREA
               IF    ( SACCESS-RC      NOT =   ZERO )
                   MOVE    "4"             TO  LNK-ONSHIIMG-IMG-STATUS
               END-IF
      *
      *        イメージデータ元ファイル削除コマンド
               MOVE    LOW-VALUE       TO  COMMAND
               STRING  "rm -f "            DELIMITED BY SIZE
                       IMAGE-FILENAME      DELIMITED BY SPACE
                                   INTO    COMMAND
               END-STRING
      *        コマンド実行
               CALL    "system"            USING
                                           COMMAND
           ELSE
               MOVE    "3"             TO  LNK-ONSHIIMG-IMG-STATUS
           END-IF
      *
           MOVE    1               TO  FLG-END
           . 
       2001-IMAGE-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像削除処理
      *****************************************************************
       2001-IMAGE-DEL-SEC         SECTION.
      *
           INITIALIZE                  ONSHI-IMAGE-REC
           MOVE    SPA-HOSPNUM     TO  ONS-IMAGE-HOSPNUM
           MOVE    LNK-ONSHIIMG-FILE-NAME 
                                   TO  ONS-IMAGE-FILE-NAME
      *
           MOVE    ONSHI-IMAGE-REC TO  MCPREC
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_onshi_image"
                                   TO  MCP-TABLE
           MOVE    "del"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
      *
           MOVE    SPACE           TO  MED-FILENAME
           STRING  MCP-TEMPDIR         DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   WRK-CONS-MEDIMGDIR  DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   LNK-ONSHIIMG-FILE-NAME
                                       DELIMITED BY SPACE
                                       INTO    MED-FILENAME
           END-STRING
      *
           MOVE    LOW-VALUE       TO  COMMAND
           STRING  "rm -f "            DELIMITED BY SIZE
                   MED-FILENAME        DELIMITED BY SPACE
                               INTO    COMMAND
           END-STRING
      *    コマンド実行
           CALL    "system"            USING
                                       COMMAND
      *
           MOVE    1               TO  FLG-END
           . 
       2001-IMAGE-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           EVALUATE    LNK-ONSHIIMG-IMG-STATUS
               WHEN    "0"
                   MOVE    "正常"          TO  LNK-ONSHIIMG-IMG-MSG
               WHEN    "1"
                   MOVE    "画像データなし"
                                           TO  LNK-ONSHIIMG-IMG-MSG
               WHEN    "2"
                   MOVE    "画像ファイルサイズオーバー"
                                           TO  LNK-ONSHIIMG-IMG-MSG
               WHEN    "3"
                   MOVE    "画像レコードなし"
                                           TO  LNK-ONSHIIMG-IMG-MSG
               WHEN    "4"
                   MOVE    "画像データ変換エラー"
                                           TO  LNK-ONSHIIMG-IMG-MSG
               WHEN    "5"
                   MOVE    "変換ファイルオープンエラーです"
                                           TO  LNK-ONSHIIMG-IMG-MSG
               WHEN    "6"
                   MOVE    "変換ファイル書き込みエラーです"
                                           TO  LNK-ONSHIIMG-IMG-MSG
           END-EVALUATE
      *
           IF      LNK-ONSHIIMG-RC     =   ZERO
               IF       LNK-ONSHIIMG-IMG-STATUS
                                   NOT =   "0" AND   SPACE
                   MOVE    1              TO  LNK-ONSHIIMG-RC
               END-IF
           END-IF
      * 
           IF      LNK-ONSHIIMG-RC     = ZERO
               MOVE  MED-FILENAME      TO  LNK-ONSHIIMG-FILE-FOLDER
           END-IF
      *
           DISPLAY "LNK-ONSHIIMG-RC        =" LNK-ONSHIIMG-RC 
           DISPLAY "LNK-ONSHIIMG-IMG-STATUS=" LNK-ONSHIIMG-IMG-STATUS 
           DISPLAY "LNK-ONSHIIMG-FILE-FOLDER=" LNK-ONSHIIMG-FILE-FOLDER
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    イメージデータ読み込み処理
      *****************************************************************
       900-OCRIMAGE-READ-SEC       SECTION.
      *
           MOVE    ONSHI-IMAGE-REC TO  MCPREC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_onshi_image"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_onshi_image"
                                       TO  MCP-TABLE
               MOVE    "key1"          TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPREC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-OCRIMAGE
                   MOVE    MCPREC          TO  ONSHI-IMAGE-REC
               ELSE
                   MOVE    1               TO  FLG-OCRIMAGE
               END-IF
           ELSE
               MOVE    1               TO  FLG-OCRIMAGE
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_onshi_image"
                                   TO  MCP-TABLE
           MOVE    "key1"          TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
           .
      *
       900-OCRIMAGE-READ-EXT.
           EXIT.
