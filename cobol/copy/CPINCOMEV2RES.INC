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
      ******************************************************************
      *    ＡＰＩレスポンス入金方法情報一覧
      *                                     COPY (CPINCOMEV2RES.INC)
      ******************************************************************
        01  XML-INCOMERES.
            02  INCOMERES-CONTEXT          PIC S9(9)   BINARY.
            02  INCOMERES-OBJECT           PIC X(36).
            02  INCOMERES-MODE             PIC S9(9)   BINARY.
            02  INCOMERES-RECORDNAME       PIC X(128).
            02  INCOMERES-APPOINTRES.
      *       実施日
              03  INCOMERES-INFORMATION-DATE   PIC X(10).
      *       実施時間
              03  INCOMERES-INFORMATION-TIME   PIC X(8).
      *       返却コード
              03  INCOMERES-API-RESULT         PIC X(2).
      *       返却メッセージ
              03  INCOMERES-API-RESULT-MSG     PIC X(100).
      *
              03  INCOMERES-RESKEY             PIC X(100).
      *       基準日
              03  INCOMERES-BASE-DATE          PIC X(10).
      *       診療科一覧
              03  INCOMERES-INCOME-INFOMATION.
                 05  INCOMERES-INCOME-INFO     OCCURS  100.
      *               入金方法
                      07  INCOMERES-CODE       PIC X(02).
      *               入金方法名称
                      07  INCOMERES-NAME       PIC X(30).
      *               入金方法略称
                      07  INCOMERES-TANNAME    PIC X(10).
      *               入金方法分類区分
                      07  INCOMERES-BUNRUI     PIC X(02).
      *               入金状態
                      07  INCOMERES-JYOTAI     PIC X(01).
      *               入金状態（退院時）
                      07  INCOMERES-JYOTAI-N   PIC X(01).
      *               入金状態（定期請求）
                      07  INCOMERES-JYOTAI-T   PIC X(01).
      *
