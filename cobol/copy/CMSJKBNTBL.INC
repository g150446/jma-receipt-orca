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
      *    症状詳記区分テーブル            COPY (CMSJKBNTBL.INC)
      *                             令和６年１０月から適応
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       01  TBL-SJKBN-AREA.
           03  FILLER              PIC X(53)   VALUE
               "01 主たる疾患の臨床症状".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "02 主たる疾患の診療・検査所見".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "03 主な治療行為の必要性".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "04 主な治療行為の経過".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "05 １００万点以上の薬剤に係る症状等".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "06 １００万点以上の処置に係る症状等".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "07 その他（１）".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "08 その他（２）".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "09 その他（３）".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "50 治験概要".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "51 疾患別リハビリテーションに係る治療継続理由".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "52 廃用症候群に係る評価表".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "90 上記以外の診療報酬明細書".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "99 レセプト摘要欄コメント".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "AA 労災レセプト「傷病の経過」".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
           03  FILLER              PIC X(53)   VALUE
               "BB 自賠責（第三者行為）レセプト摘要欄コメント".
           03  FILLER              PIC X(06)   VALUE   "000000".
           03  FILLER              PIC X(06)   VALUE   "999999".
      *
      *
       01  TBL-SJKBN-AREA-R        REDEFINES   TBL-SJKBN-AREA.
           03  TBL-SJKBN-TBL           OCCURS  16  INDEXED   TBL-SJK.
      *        症状詳記区分
               05  TBL-SJKBN-CD        PIC X(02).
               05  TBL-SJK-YOBI        PIC X(01).
      *        症状詳記区分名称
               05  TBL-SJKBN-NAME      PIC X(50).
      *        適応開始年月
               05  TBL-SJKBN-STYM      PIC X(06).
      *        適応終了年月
               05  TBL-SJKBN-EDYM      PIC X(06).
      *
      *テーブル件数
       01   TBL-SJK-MAX                PIC 9(03)   VALUE   16.
      *
