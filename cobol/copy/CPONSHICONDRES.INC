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
      *    ＡＰＩ資格確認用       COPY (CPONSHICONDRES.INC)           *
      *****************************************************************
       01  XML-ONSHICONDRES.
           03  ONCONRES-CONTEXT            PIC S9(9)   BINARY.
           03  ONCONRES-OBJECT             PIC X(36).
           03  ONCONRES-MODE               PIC S9(9)   BINARY.
           03  ONCONRES-RECORDNAME         PIC X(128).
           03  ONCONRES-ONSHICONDRES.
      * 返却コード
               05  ONCONRES-API-RESULT        PIC X(3).
      * 返却メッセージ
               05  ONCONRES-API-RESULT-MSG    PIC X(100).
