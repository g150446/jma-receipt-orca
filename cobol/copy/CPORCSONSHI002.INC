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
      *    オン資格確認　限度額適用認定証・*特定疾病療養受療証編集サブ
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSONSHI002AREA.
      *    入力項目
           03  ORCSONSHI002-IN-AREA.
      *        処理区分    01:明細編集のみ、02:公費編集のみ、03:明細・公費編集
               05  LNK-ONSHI002-SYOKBN PIC X(02).
      *R04.1
      *        保険証確認の限度額認定証同意
               05  LNK-ONSHI002-DOUIFLG    PIC X(01).
      *        TBL_UUID (オン資格確認データONSKAKU-RECの検索有り）
               05  LNK-ONSHI002-TBL-UUID
                                           PIC X(36).
      *R06.3
      *        医療扶助　交付番号の転記
               05  LNK-ONSHI002-KFTNKKBN   PIC X(01).
      *
      *    返却項目
           03  LNK-ONSHI002-OUT-AREA.
      *        限度額編集
               05  LNK-ONSHI002-GENDO-MSG      PIC X(200).
               05  LNK-ONSHI002-GENDO-MAX      PIC 9(03).
      *TEST
               05  LNK-ONSHI002-GENDO-KEIMSG  PIC X(100).
      *TEST
      *        特定疾病編集
               05  LNK-ONSHI002-SIKKAN-TBL     OCCURS   3.
                   07  LNK-ONSHI002-SIKKAN-MSG     PIC X(200).
                   07  LNK-ONSHI002-SIKKAN-MAX     PIC 9(03).
      *        医療扶助編集
               05  LNK-ONSHI002-FUJYO-G.
                   07  LNK-ONSHI002-FUJYO-TBL      OCCURS   10.
                       09  LNK-ONSHI002-FUJYO-MSG     PIC X(200).
                       09  LNK-ONSHI002-FUJYO-MAX     PIC 9(03).
      *    公費など返却項目
           03  LNK-ONSHI002-OUT02-AREA.
      *        限度額認定証資格有効期間
               05  LNK-ONSHI002-GENDO-STYMD    PIC X(08).
               05  LNK-ONSHI002-GENDO-EDYMD    PIC X(08).
      *        限度額公費情報
               05  LNK-ONSHI002-KOH-AREA.
                   07  LNK-ONSHI002-KOH-TBL        OCCURS   10.
                       09  LNK-ONSHI002-KOHNUM     PIC X(03).
      *                負担者番号
                       09  LNK-ONSHI002-FTNJANUM   PIC X(08).
      *                受給者番号
                       09  LNK-ONSHI002-JKYSNUM    PIC X(20).
                       09  LNK-ONSHI002-TEKSTYMD   PIC X(08).
                       09  LNK-ONSHI002-TEKEDYMD   PIC X(08).
      *                特定疾病からの長期
                       09  LNK-ONSHI002-SIPKBN     PIC X(01).
      *                地方公費の公費
                       09  LNK-ONSHI002-CHIKOU     PIC X(01).
      *                公費照会のTBL-UUID
                       09  LNK-ONSHI002-KOHUUID    PIC X(36).
      *                医療扶助　交付番号あり
                       09  LNK-ONSHI002-FUJYOKOUF-G.
                           11  LNK-ONSHI002-FUJYOKF-OCC  OCCURS 10.
                               13  LNK-ONSHI002-KOFUNUM    PIC X(15).
                               13  LNK-ONSHI002-NYUGAIKBN  PIC X(01).
                               13  LNK-ONSHI002-SRYYM      PIC X(06).
      *
                   07  LNK-ONSHI002-KOHMAX         PIC 9(03).
      *
      *        所得者１情報（低年金）
               05  LNK-ONSHI002-SYOTO1-AREA.
                   07  LNK-ONSHI002-STK1-TEKSTYMD  PIC X(08).
                   07  LNK-ONSHI002-STK1-TEKEDYMD  PIC X(08).
      *            老齢福祉年金受給者証
                   07  LNK-ONSHI002-STK1-RRIFUK    PIC X(01).
      *            認定範囲区分
                   07  LNK-ONSHI002-STK1-HKNTEKKBN PIC X(01).
      *            境界層該当区分
                   07  LNK-ONSHI002-STK1-KYOUKAI   PIC X(01).
      *        所得者２情報（低所得）
               05  LNK-ONSHI002-SYOTO2-AREA.
                   07  LNK-ONSHI002-STK2-TEKSTYMD  PIC X(08).
                   07  LNK-ONSHI002-STK2-TEKEDYMD  PIC X(08).
      *            長期入院該当年月日
                   07  LNK-ONSHI002-STK2-SKJGNSTYMD    PIC X(08).
      *            認定範囲区分
                   07  LNK-ONSHI002-STK2-HKNTEKKBN PIC X(01).
      *            境界層該当区分
                   07  LNK-ONSHI002-STK2-KYOUKAI   PIC X(01).
      *        公費・低所得者あり
               05  LNK-ONSHI002-KOH-ARI          PIC X(01).
      *        医療扶助あり
               05  LNK-ONSHI002-FUJ-ARI          PIC X(01).
      *
      *
      *復帰コード １：区分エラー
           03  LNK-ONSHI002-RC         PIC S9(9)   BINARY.
      *
