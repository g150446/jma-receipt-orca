      ******************************************************************
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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSKAISEICHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : レセプト改正対応チェック処理
      *  管理者            : 
      *  22/02/17    ORCAMO        新規作成
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  WRK-CONS-AREA.
      *    次回改正年月
           03  WRK-CONS-SRYYM      PIC X(06)   VALUE   "202406".
      *    画面表示メッセージ
           03  WRK-CONS-MSG1.
      *        05  FILLER          PIC X(52)   VALUE
      *        "＜重要　令和４年４月診療分以降のレセプトは対応作業中".
      *        05  FILLER          PIC X(42)   VALUE
      *        "です。対応プログラム提供をお待ち下さい。＞".
      *        05  FILLER          PIC X(52)   VALUE
      *        "＜令和４年度記載要領対応プログラム適用済＞".
      *        05  FILLER          PIC X(52)   VALUE
      *        "＜重要　令和４年１０月以降の後期高齢２割のレセプトは".
      *        05  FILLER          PIC X(52)   VALUE
      *        "対応作業中です。対応プログラム提供をお待ち下さい。＞".
      *        05  FILLER          PIC X(52)   VALUE
      *        "＜令和４年１０月以降の後期高齢２割レセプト対応プログ".
      *        05  FILLER          PIC X(52)   VALUE
      *        "ラム適用済＞".
      *       05  FILLER          PIC X(52)   VALUE
      *       "＜重要　令和６年６月診療分以降のレセプトは対応作業中".
      *       05  FILLER          PIC X(42)   VALUE
      *       "です。対応プログラム提供をお待ち下さい。＞".
              05  FILLER          PIC X(52)   VALUE
              "＜令和６年度記載要領対応プログラム適用済＞".
           03  WRK-CONS-MSG2.
      *        05  FILLER          PIC X(52)   VALUE
      *        "＜重要　令和４年４月診療分以降のレセ電作成等は対応作".
      *        05  FILLER          PIC X(46)   VALUE
      *        "業中です。対応プログラム提供をお待ち下さい。＞".
      *        05  FILLER          PIC X(52)   VALUE
      *        "＜令和４年度記載要領対応プログラム適用済＞".
      *        05  FILLER          PIC X(52)   VALUE
      *        "＜重要　令和４年１０月以降の後期高齢２割のレセ電作成".
      *        05  FILLER          PIC X(52)   VALUE
      *        "等は対応作業中です。対応プログラム提供をお待ち下さい".
      *        05  FILLER          PIC X(06)   VALUE
      *        "。＞".
      *         05  FILLER          PIC X(52)   VALUE
      *         "＜令和４年１０月以降の後期高齢２割レセ電作成等対応プ".
      *         05  FILLER          PIC X(52)   VALUE
      *         "ログラム適用済＞".
      *        05  FILLER          PIC X(52)   VALUE
      *        "＜重要　令和６年６月診療分以降のレセ電作成等は対応作".
      *        05  FILLER          PIC X(46)   VALUE
      *        "業中です。対応プログラム提供をお待ち下さい。＞".
              05  FILLER          PIC X(52)   VALUE
              "＜令和６年度記載要領対応プログラム適用済＞".
      *    03  WRK-CONS-STYLE      PIC X(20)   VALUE   "red" .
           03  WRK-CONS-STYLE      PIC X(20)   VALUE   "blue" .
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSKAISEICHK.INC".
      *     
      *****************************************************************
      *
       PROCEDURE                   DIVISION    USING
                                   ORCSKAISEICHKAREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY "LNK-KAISEICHK-SRYYM=" LNK-KAISEICHK-SRYYM
      *
           IF    ( LNK-KAISEICHK-SRYYM
                               NOT =   SPACE )
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    LNK-KAISEICHK-SRYYM
                                       TO  LNK-DAY2-YMD (1:6)
               MOVE    "01"            TO  LNK-DAY2-YMD (7:2)
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               IF    ( STS-DAY-RC1 NOT =   ZERO )
                   MOVE    1               TO  LNK-KAISEICHK-RC
               ELSE
                   IF    ( LNK-KAISEICHK-SRYYM >=  WRK-CONS-SRYYM )
                       MOVE    "1"             TO  LNK-KAISEICHK-KBN
                   END-IF
               END-IF
           END-IF
      *
           EVALUATE    LNK-KAISEICHK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-CONS-MSG2   TO  LNK-KAISEICHK-MSG
               WHEN    OTHER
                   MOVE    WRK-CONS-MSG1   TO  LNK-KAISEICHK-MSG
           END-EVALUATE 
           MOVE    WRK-CONS-STYLE  TO  LNK-KAISEICHK-STYLE
      *
           DISPLAY "LNK-KAISEICHK-KBN  =" LNK-KAISEICHK-KBN
           DISPLAY "LNK-KAISEICHK-RC   =" LNK-KAISEICHK-RC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
