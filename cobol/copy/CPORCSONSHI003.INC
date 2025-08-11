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
      *    オン資格確認　保険情報と患者保険情報チェックサブ
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSONSHI003AREA.
      *    入力項目
           03  ORCSONSHI003-IN-AREA.
      *        処理区分 (01:不一致チェック、02:保険の種類変換、03:予約照会)
               05  LNK-ONSHI003-SYOKBN     PIC X(02).
      *        予約照会の処理区分 (CHK、空白:チェック、UPD:枝番更新）
               05  LNK-ONSHI003-SYOKBN2    PIC X(03).
      *        患者ID（オン資格の患者ID以外の場合のみ）
               05  LNK-ONSHI003-PTID       PIC 9(10).
      *        TBL_UUID (オン資格確認データONSKAKU-RECの検索有り）
               05  LNK-ONSHI003-TBL-UUID
                                           PIC X(36).
      *        オン資格照会日付（未設定はシステム日付）
               05  LNK-ONSHI003-SYSYMD     PIC X(08).
      *
      *    返却項目
           03  LNK-ONSHI003-OUT1-AREA.
      *        チェック情報 00:すべて一致、01:不一致あり、99:対象なし
               05  LNK-ONSHI003-CHK-KBN      PIC X(02).
      *        本人家族区分(1:不一致)
               05  LNK-ONSHI003-CHK-HONKZKKBN  PIC X(01).
      *        負担割合
               05  LNK-ONSHI003-CHK-FTNRATE    PIC X(01).
      *    返却項目(02)
           03  LNK-ONSHI003-OUT2-AREA.
      *        オン資格保険の種類（被保険者資格証明書）
               05  LNK-ONSHI003-HKNNUM         PIC X(03).
      *        オン資格保険の種類（置換え前）
               05  LNK-ONSHI003-MAE-HKNNUM     PIC X(03).
      *        不一致
               05  LNK-ONSHI003-CHK-HKNNUM     PIC X(01).
      *    返却項目(03)
           03  LNK-ONSHI003-OUT3-AREA.
      *        枝番不一致（更新可）
               05  LNK-ONSHI003-CHK-EDABAN     PIC X(01).
      *        00:一致する患者保険情報あり、01：枝番のみ不一致（枝番空白以外で更新不可）
      *        02:更新対応なし、09:更新エラー、99:排他中
      *        10:更新終了
               05  LNK-ONSHI003-YYKERR         PIC X(02).
               05  LNK-ONSHI003-YYKERRMSG      PIC X(100).
      *
      *
      *復帰コード １：パラメタエラー
           03  LNK-ONSHI003-RC         PIC S9(9)   BINARY.
      *
