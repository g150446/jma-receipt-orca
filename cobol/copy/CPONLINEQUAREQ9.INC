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
      *    ＡＰＩ資格確認　医療扶助　交付番号情報用    COPY (CPONLINEQUAREQ9.INC)*
      *****************************************************************
       01  XML-ONLINEQUAREQ9.
           03  QUAREQ9-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ9-OBJECT              PIC X(36).
           03  QUAREQ9-MODE                PIC S9(9)   BINARY.
           03  QUAREQ9-RECORDNAME          PIC X(128).
           03  QUAREQ9-PATIENTREQ.
      *メッセージヘッダー部     
               05  QUAREQ9-HEADER.
      *資格確認日
                   07  QUAREQ9-KAKU-KAKUNINYMD PIC X(08).
      *FUJYO_UUID
                   07  QUAREQ9-FUJYO-UUID      PIC X(36).
      *メッセージボディ部
      *資格確認結果
               05  QUAREQ9-MESSAGEBODY.
      *医療扶助資格確認結果リスト
                   07  QUAREQ9-MESSAGE-BODY-TBL1   OCCURS  2.
      *保険者番号
                       09  QUAREQ9-RES-HKNJANUM        PIC X(08).
      *被保険者証番号
                       09  QUAREQ9-RES-NUM             PIC X(80).
      *医療扶助資格確認結果リスト
      *
                       09  QUAREQ9-MESSAGE-BODY-TBL2   OCCURS   10.
      *医療券・調剤券別(01 :医療券 02 :調剤券)
                           11  QUAREQ9-TICKET-TYPE     PIC X(02).
      *医療券・調剤券有効開始年月日
                           11  QUAREQ9-MDC-TEKSTYMD   PIC X(08).
      *医療券・調剤券有効終了年月日
                           11  QUAREQ9-MDC-TEKEDYMD   PIC X(08).
      *交付番号
                           11  QUAREQ9-KOFUNUM        PIC X(30).
      *診療年月
                           11  QUAREQ9-SRYYM           PIC X(06).
      *指定医療機関コード
                           11  QUAREQ9-DESIGN-CODE     PIC X(10).
      *指定医療機関確認フラグ
                           11  QUAREQ9-DESIGN-FLG      PIC X(01).
      *診療別(1: 入院 2:入院外　これ以外は捨て(3:歯科 etc))
                           11  QUAREQ9-MEDICAL-TYPE     PIC X(02).
      *本人支払額（自己負担額）
                           11  QUAREQ9-FTNGAKU         PIC X(06).
      *
                       09  QUAREQ9-IMAGE-FILE-STATUS   PIC X(01).
                       09  QUAREQ9-IMAGE-DATA          PIC X(20000).
