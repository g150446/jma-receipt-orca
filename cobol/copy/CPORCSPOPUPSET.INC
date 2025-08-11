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
      *    患者情報通知パラメタ　　COPY (CPORCSPOPUPSET.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSPOPUPSETAREA.
      *
      *保険確認日付
           03  ORCSPOPUPSET-HKNCHK     PIC X(01).
      *メモ１
           03  ORCSPOPUPSET-MEMO1      PIC X(01).
      *メモ２
           03  ORCSPOPUPSET-MEMO2      PIC X(01).
      *R02.10
      *患者番号
           03  ORCSPOPUPSET-PTNUM      PIC X(20).
      *患者ＩＤ
           03  ORCSPOPUPSET-PTID       PIC 9(10).
      *R02.10
      *オンライン資格確認対応
           03  ORCSPOPUPSET-ONSHI-KAKU.
      *オンライン資格確認業務
               05  ORCSPOPUPSET-GYOUMUCD   PIC X(02).
      *オンライン資格確認対応
               05  ORCSPOPUPSET-ONSHI      PIC X(01).
      *オンライン資格確認表示時間
               05  ORCSPOPUPSET-ONS-TIMEOUT PIC 9(03).
      *R04.1
      *オンライン資格　診療行為照会確認有無
               05  ORCSPOPUPSET-ONSHISINRYO    PIC X(01).
      *オンライン資格確認保険証確認の限度額認定証同意
               05  ORCSPOPUPSET-ONS-DOUIFLG    PIC X(01).
      *出力
      *表示内容
           03  ORCSPOPUPSET-BODY       PIC X(1000).
      *表示見出し内容
           03  ORCSPOPUPSET-SUMMARY    PIC X(256).
      *表示時間
           03  ORCSPOPUPSET-TIMEOUT    PIC 9(03).
      *オン資格通知
           03  ORCSPOPUPSET-SIKAKU-YUKO    PIC X(01).
      *
