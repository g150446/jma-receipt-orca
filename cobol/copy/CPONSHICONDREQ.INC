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
      *    ＡＰＩ資格確認用状態通知   COPY (CPONSHICONDREQ.INC)       *
      *****************************************************************
       01  XML-ONSHICONDREQ.
           03  ONCONREQ-CONTEXT            PIC S9(9)   BINARY.
           03  ONCONREQ-OBJECT             PIC X(36).
           03  ONCONREQ-MODE               PIC S9(9)   BINARY.
           03  ONCONREQ-RECORDNAME         PIC X(128).
           03  ONCONREQ-ONSHICONDREQ.
      *処理区分
               05  ONCONREQ-REQUEST-NUMBER PIC X(02).
      *障害発生日
               05  ONCONREQ-FAILURE-DATE   PIC X(08).
      *障害発生時間
               05  ONCONREQ-FAILURE-TIME   PIC X(08).
      *端末番号
               05  ONCONREQ-TERM-NO        PIC X(02).
      *端末名称
               05  ONCONREQ-TERM-NAME      PIC X(100).
      *エラーコード
               05  ONCONREQ-ERR-CODE       PIC X(09).
      *エラーメッセージ
               05  ONCONREQ-ERR-MSG        PIC X(120).
