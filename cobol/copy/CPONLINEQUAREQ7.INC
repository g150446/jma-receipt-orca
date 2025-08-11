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
      *    ＡＰＩ資格確認　保険証OCR画像情報用 COPY (CPONLINEQUAREQ7.INC)*
      *****************************************************************
       01  XML-ONLINEQUAREQ7.
           03  QUAREQ7-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ7-OBJECT              PIC X(36).
           03  QUAREQ7-MODE                PIC S9(9)   BINARY.
           03  QUAREQ7-RECORDNAME          PIC X(128).
           03  QUAREQ7-PATIENTREQ.
      *保険証OCRファイル名     
               05  QUAREQ7-FILENAME            PIC X(100).
      *処理日付
               05  QUAREQ7-INFORMATION-DATE    PIC X(10).
      *処理日付
               05  QUAREQ7-INFORMATION-TIME    PIC X(08).
      *画像ファイル
               05  QUAREQ7-IMAGE-FILE-STS      PIC X(01).
      *画像データ
               05  QUAREQ7-IMAGE-DATA          PIC X(500000).
