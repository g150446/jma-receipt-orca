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
      *    ＡＰＩ資格確認診療情報　取得 COPY (CPONLINEMEDGETREQ1.INC)*
      *****************************************************************
       01  XML-ONLINEMEDGETREQ1.
           03  MEDREQ1-CONTEXT             PIC S9(9)   BINARY.
           03  MEDREQ1-OBJECT              PIC X(36).
           03  MEDREQ1-MODE                PIC S9(9)   BINARY.
           03  MEDREQ1-RECORDNAME          PIC X(128).
           03  MEDREQ1-PATIENTREQ.
      *        リクエスト番号（01）
               05  MEDREQ1-REQUEST-NUMBER  PIC X(2).
      *        患者番号
               05  MEDREQ1-PATIENTID       PIC X(20).
      *        確認年月
               05  MEDREQ1-SRYYM           PIC X(07).
      *        入外等の別
               05  MEDREQ1-NYUGAIKBN       PIC X(01).
      *
      *        医療機関コード
               05  MEDREQ1-HOSPCD          PIC  X(10).
      *        調剤薬局コード
               05  MEDREQ1-CHOZAICD        PIC  X(10).
      *        

