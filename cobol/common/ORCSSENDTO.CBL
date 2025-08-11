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
       PROGRAM-ID.         ORCSSENDTO.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : 共通
      *  コンポーネント名  : ファイルデフォルト保存先返却
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/04/20    NMED−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  18/10/XX  妊婦設定から患者禁忌薬剤追加
      *****************************************************************
      *
      *ENVIRONMENT           DIVISION.
      *CONFIGURATION         SECTION.
      *INPUT-OUTPUT          SECTION.
      *
      *FILE-CONTROL.
       DATA                DIVISION.
      *FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *01  FLG-AREA.
      *01  IDX-AREA.
       01  WRK-AREA.
           03  WRK-DIR             PIC X(1024).
           03  WRK-DEFAULT-DIR     PIC X(64).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "CPORCSSENDTO.INC".
      *
       PROCEDURE                   DIVISION    USING
           SSENDTO-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
      *    INITIALIZE                      FLG-AREA
      *    INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    "/home/orca"        TO  WRK-DEFAULT-DIR
           IF ( SSENDTO-DIR        NOT =   SPACE )
               MOVE    SSENDTO-DIR     TO  WRK-DIR
               MOVE    SPACE           TO  SSENDTO-DIR
               STRING  WRK-DEFAULT-DIR        DELIMITED BY SPACE
                       "/"                    DELIMITED BY SIZE
                       FUNCTION TRIM(WRK-DIR) DELIMITED BY SIZE
               INTO    SSENDTO-DIR
               END-STRING
           ELSE
               MOVE    "/home/orca"    TO  SSENDTO-DIR
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
