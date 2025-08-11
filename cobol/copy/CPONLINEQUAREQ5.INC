      *****************************************************************
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
      *****************************************************************
      *    ＡＰＩ資格確認　特定検診用     COPY (CPONLINEQUAREQ5.INC)*
      *****************************************************************
       01  XML-ONLINEQUAREQ5.
           03  QUAREQ5-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ5-OBJECT              PIC X(36).
           03  QUAREQ5-MODE                PIC S9(9)   BINARY.
           03  QUAREQ5-RECORDNAME          PIC X(128).
           03  QUAREQ5-PATIENTREQ.
      *メッセージヘッダー部     
               05  QUAREQ5-HEADER.
      *処理実行日時
                   07  QUAREQ5-SHORI-TIME  PIC X(14).
      *要求情報
                   07  QUAREQ5-REQUESTINFO-INFO.
      *要求医療機関(薬局)コード
                       09  QUAREQ5-REQ-HOSPCD      PIC X(10).
      *要求保険者番号
                       09  QUAREQ5-REQ-HKNJANUM    PIC X(08).
      *記号
                       09  QUAREQ5-REQ-KIGO        PIC X(80).
      *番号
                       09  QUAREQ5-REQ-NUM         PIC X(80).
      *番号枝番
                       09  QUAREQ5-REQ-EDABAN      PIC X(02).
      *要求任意のファイル識別子(医療機関固有項目)
                       09  QUAREQ5-FILE-SIKIBETU   PIC X(50).
      *要求ファイル種別
                       09  QUAREQ5-FILE-SYUBETU    PIC X(01).
      *資格情報
                   07  QUAREQ5-QUAINF.
      *被保険者番号
                       09  QUAREQ5-RES-HKNJANUM    PIC X(08).
      *被保険者証記号
                       09  QUAREQ5-RES-KIGO        PIC X(80).
      *被保険者証番号
                       09  QUAREQ5-RES-NUM         PIC X(80).
      *番号枝番
                       09  QUAREQ5-RES-EDABAN      PIC X(02).
      *氏名カナ
                       09  QUAREQ5-RES-KANANAME        PIC X(100).
      *氏名カナ（その他）
                       09  QUAREQ5-RES-KANANAME-ETC    PIC X(100).
      *氏名
                       09  QUAREQ5-RES-NAME            PIC X(100).
      *氏名（その他）
                       09  QUAREQ5-RES-NAME-ETC        PIC X(100).
      *生年月日
                       09  QUAREQ5-RES-BIRTHDAY        PIC X(08).
      *年齢
                       09  QUAREQ5-RES-AGE             PIC X(03).
      *性別１
                       09  QUAREQ5-RES-SEX1            PIC X(01).
      *性別２
                       09  QUAREQ5-RES-SEX2            PIC X(01).
      *照会番号
                       09  QUAREQ5-REFNUM              PIC X(50).
      *処理結果区分(特定健診)
                   07  QUAREQ5-RESULTCODE              PIC X(01).
      *メッセージID
                   07  QUAREQ5-MESSAGE-ID              PIC X(09).
      *メッセージ内容
                   07  QUAREQ5-MESSAGE                 PIC X(120).
      *文字コード識別
                   07  QUAREQ5-CHARACTER               PIC X(01).
      *メッセージボディ部
               05  QUAREQ5-MESSAGEBODY.
                   07  QUAREQ5-MESSAGE-BODY-R      OCCURS   50.
      *診療年月別情報
      *実施年月日
                       09  QUAREQ5-JISSHIYMD      PIC X(08).
      *健診結果・質問票情報群
                       09  QUAREQ5-CHECK-REC.
      *健診結果・質問票情報
                           11  QUAREQ5-CHECK-OCC      OCCURS   200.
      *項目コード
                               13  QUAREQ5-ITEMCODE     PIC X(17).
      *項目名
                               13  QUAREQ5-ITEMNAME     PIC X(80).
      *データ型
                               13  QUAREQ5-DATA-TYPE     PIC X(02).
      *データ値
                               13  QUAREQ5-DATA-VALUE    PIC X(256).
      *単位
                               13  QUAREQ5-DATA-TANNI    PIC X(26).


