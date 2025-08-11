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
      *----------------------------------------------------------------*
      *    初診料コードテーブル COPY (CMSYOSINCDTBL.INC)
      *                    2022年４月から
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 
      *****************************************************************
      *
       01  TBL-SYOSIN-AREA.
      *
           03  FILLER          PIC X(15)   VALUE   "0     099110001".
           03  FILLER          PIC X(15)   VALUE   "      111000110".
           03  FILLER          PIC X(15)   VALUE   "4     101110010".
           03  FILLER          PIC X(15)   VALUE   "5   2 111012510".
           03  FILLER          PIC X(15)   VALUE   "1   1 111012710".
           03  FILLER          PIC X(15)   VALUE   " 1    111013850".
      *    小児科外来診療料
           03  FILLER          PIC X(15)   VALUE   "2     113003510".
           03  FILLER          PIC X(15)   VALUE   "2     113003710".
      *    小児かかりつけ診療料
           03  FILLER          PIC X(15)   VALUE   "3     113019710".
           03  FILLER          PIC X(15)   VALUE   "3     113019910".
      *    小児かかりつけ診療料１
           03  FILLER          PIC X(15)   VALUE   "3  1  113037210".
           03  FILLER          PIC X(15)   VALUE   "3  1  113037410".
      *    小児かかりつけ診療料２
           03  FILLER          PIC X(15)   VALUE   "3  1  113037610".
           03  FILLER          PIC X(15)   VALUE   "3  1  113037810".
      *    情報通信機器を用いた場合
           03  FILLER          PIC X(15)   VALUE   " 2 1  111014210".
           03  FILLER          PIC X(15)   VALUE   "52 12 111014310".
           03  FILLER          PIC X(15)   VALUE   "12 11 111014410".
      *    初診（同一日複数診療科受診）
           03  FILLER          PIC X(15)   VALUE   "  D   111011810".
           03  FILLER          PIC X(15)   VALUE   "5 D 2 111012610".
           03  FILLER          PIC X(15)   VALUE   "1 D 1 111012810".
           03  FILLER          PIC X(15)   VALUE   "4 D   101110040".
           03  FILLER          PIC X(15)   VALUE   " 2D1  111014510".
           03  FILLER          PIC X(15)   VALUE   "52D12 111014610".
           03  FILLER          PIC X(15)   VALUE   "12D11 111014710".
      *    初診料（文書による紹介がない患者）（労災）
           03  FILLER          PIC X(15)   VALUE   "4  22 101110050".
      *    初診料（文書による紹介がない患者）（同一日複数診療科（労災）
           03  FILLER          PIC X(15)   VALUE   "4 D22 101110060".
      *    初診料（文書による紹介がない患者の場合）（初減）（特例）
           03  FILLER          PIC X(15)   VALUE   " 1    111016150".
      *
      *
       01  TBL-SYOSIN-AREA-R       REDEFINES   TBL-SYOSIN-AREA.
      **** 03  TBL-SYOSIN-OCC      OCCURS  25  INDEXED   TBL-SYS.
           03  TBL-SYOSIN-OCC      OCCURS  27  INDEXED   TBL-SYS.
      *        1:特定妥結率初診、2:小児科外来、3:小児かかり、4:労災、
      *        5:文書による紹介がない患者、0:ダミー、
               05  TBL-SYOSIN-KBN          PIC X(01).
      *        1:コロナ特例、2:情報通信機器
               05  TBL-SYOSIN-KBN2         PIC X(01).
      *        D:同一日複数診療科受診
               05  TBL-SYOSIN-KBN3         PIC X(01).
      *        1:令和４年４月からの追加分、2:令和４年１０月から
               05  TBL-SYOSIN-KBN4         PIC X(01).
      *        1:特定妥結率、2:文書による紹介がない患者
               05  TBL-SYOSIN-KBN5         PIC X(01).
               05  TBL-YOBI1               PIC X(01).
               05  TBL-SYOSIN-SRYCD        PIC X(09).
      *
