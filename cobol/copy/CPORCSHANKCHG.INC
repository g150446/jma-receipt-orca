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
      *----------------------------------------------------------*
      *    全角→半角変換パラメタ　  COPY (CPORCSHANKCHG.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSHANKCHGAREA.
      *    処理フラグ（１：英数値のみ、２：英数値全角カナ）
           03  HANKCHG-SYORI               PIC X(01).
      *    処理フラグ２（1:変換不可文字は「?」で返却
      *                  2:変換不可文字（ひらがな等）があれば変換エラー出力領域は入力領域を編集
      *                  3:変換不可削除（ひらがな等）は返却なし）
           03  HANKCHG-SYORIKBN            PIC X(01).
      *    入力領域
           03  HANKCHG-MAE-INPUT           PIC X(1000).
      *
      *    出力領域（半角文字）
           03  HANKCHG-OUT-INPUT           PIC X(1000).
      *    最大桁数
           03  HANKCHG-MAX                 PIC 9(04).
      *    復帰コード （０：正常、１：変換エラー、２：改行コードありで変換不可）
           03  HANKCHG-RC                  PIC 9(02).
      *    復帰コード２（０：正常、２：変換エラーあり）
           03  HANKCHG-RC2                 PIC 9(02).
      *
      *
