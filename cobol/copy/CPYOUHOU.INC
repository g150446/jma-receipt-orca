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
      ************************************************************
      *        用法コードテーブル        COPY (CPYOUHOU.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  YOUHOU-REC.
      *用法コード
           03  YOUHOU-CODE              PIC  X(16).
      *基本用法区分コード
           03  YOUHOU-BASIC-C           PIC  X(01).
      *基本用法区分
           03  YOUHOU-BASIC-NAME        PIC  X(08).
      *用法詳細区分コード
           03  YOUHOU-DETAIL-C          PIC  X(01).
      *用法詳細区分
           03  YOUHOU-DETAIL-NAME       PIC  X(60).
      *タイミング指定区分コード
           03  YOUHOU-TIMING-C          PIC  9(01).
      *タイミング指定区分
           03  YOUHOU-TIMING-NAME       PIC  X(240).
      *用法名称
           03  YOUHOU-NAME              PIC  X(200).
      *標準用法整理番号
           03  YOUHOU-REFER-NO          PIC  9(04).
      *使用開始日
           03  YOUHOU-YUKOSTYMD         PIC  X(08).
      *使用終了日
           03  YOUHOU-YUKOEDYMD         PIC  X(08).
      *用法コード区分
           03  YOUHOU-CODE-C            PIC  9(01).
      *頓服の条件指定
           03  YOUHOU-ADD-CONDITION     PIC  9(01).
      *投与タイミング
           03  YOUHOU-ADD-TIMING        PIC  9(01).
      *投与時刻
           03  YOUHOU-ADD-TIME          PIC  9(01).
      *投与間隔
           03  YOUHOU-ADD-INTERVAL      PIC  9(01).
      *投与部位
           03  YOUHOU-ADD-BODY          PIC  9(01).
      *用法カナ名称
           03  YOUHOU-KANA-NAME         PIC  X(480).
      *用法コード（調剤レセプト）
           03  YOUHOU-CD-CHOZAI         PIC  9(03).
      *
           03  YOUHOU-TERMID            PIC  X(16).
           03  YOUHOU-OPID              PIC  X(16).
           03  YOUHOU-CREYMD            PIC  X(08).
           03  YOUHOU-CREHMS            PIC  X(06).
           03  YOUHOU-UPYMD             PIC  X(08).
           03  YOUHOU-UPHMS             PIC  X(06).
      *更新用
           03  YOUHOU-UP-KEY.
               05  YOUHOU-UPCODE        PIC  X(16).
               05  YOUHOU-UPYUKOSTYMD   PIC  X(08).
               05  YOUHOU-UPYUKOEDYMD   PIC  X(08).
