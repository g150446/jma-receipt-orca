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
      *    オンライン資格確認状態通知テーブル
      *                                COPY  (CPONSHI-COND.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  ONSCOND-REC.
      *キー
           03  ONSCOND-KEY.
      *医療機関番号
               05  ONSCOND-HOSPNUM             PIC 9(02).
      *障害発生日
               05  ONSCOND-FAILURE-DATE        PIC X(08).
      *障害発生時間
               05  ONSCOND-FAILURE-TIME        PIC X(08).
      *端末番号
           03  ONSCOND-TERM-NO             PIC 9(02).
      *端末名称
           03  ONSCOND-TERM-NAME           PIC X(100).
      *エラーコード
           03  ONSCOND-ERR-CODE            PIC X(09).
      *エラーメッセージ
           03  ONSCOND-ERR-MSG             PIC X(120).
      *削除フラグ
           03  ONSCOND-DLTFLG              PIC 9(01).
      *作成年月日
           03  ONSCOND-CREYMD              PIC X(08).
      *作成時間
           03  ONSCOND-CREHMS              PIC X(06).
