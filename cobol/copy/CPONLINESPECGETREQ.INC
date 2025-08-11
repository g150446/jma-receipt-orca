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
      *    ＡＰＩ資格確認特定検診情報　取得 COPY (CPONLINESPECGETREQ.INC)*
      *****************************************************************
       01  XML-ONLINESPECGETREQ.
           03  SPECREQ-CONTEXT             PIC S9(9)   BINARY.
           03  SPECREQ-OBJECT              PIC X(36).
           03  SPECREQ-MODE                PIC S9(9)   BINARY.
           03  SPECREQ-RECORDNAME          PIC X(128).
           03  SPECREQ-PATIENTREQ.
      *        リクエスト番号
               05  SPECREQ-REQUEST-NUMBER  PIC X(2).
      *        患者番号
               05  SPECREQ-PATIENTID       PIC X(20).
      *        実施年月日
               05  SPECREQ-JISSIYMD        PIC X(10).

