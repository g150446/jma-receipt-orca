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
      *    ＡＰＩ資格確認特定検診情報　取得   COPY (CPONLINESPECGETRES.INC)*
      *****************************************************************
       01  XML-ONLINESPECGETRES.
           03  SPECRES-CONTEXT             PIC S9(9)   BINARY.
           03  SPECRES-OBJECT              PIC X(36).
           03  SPECRES-MODE                PIC S9(9)   BINARY.
           03  SPECRES-RECORDNAME          PIC X(128).
           03  SPECRES-PATIENTRES.
      *       実施日
              05  SPECRES-INFORMATION-DATE         PIC X(10).
      *       実施時間
              05  SPECRES-INFORMATION-TIME         PIC X(08).
      *       返却コード
              05  SPECRES-API-RESULT               PIC X(03).
      *       返却メッセージ
              05  SPECRES-API-RESULT-MSG           PIC X(100).
      *
              05  SPECRES-RESKEY                   PIC X(100).
      *       患者情報
              05  SPECRES-PATIENT-INFOMATION.
      *           患者番号
                  07  SPECRES-PATIENTID            PIC X(20).
      *           患者氏名
                  07  SPECRES-NAME                 PIC X(100).
      *           患者カナ氏名
                  07  SPECRES-KANANAME             PIC X(100).
      *           誕生日
                  07  SPECRES-BIRTHDAY             PIC X(10).
      *           性別
                  07  SPECRES-SEX                  PIC X(1).
      *
      *        実施年月日
               05  SPECRES-IN-JISSIYMD            PIC X(10).
      *リクエスト番号＝０１
      *        実施年月日
               05  SPECRES-DATA01-AREA.
                   07  SPECRES-TOR-JISSIYMD       PIC X(10)
                                                  OCCURS   50.
      *リクエスト番号＝０２
      *        実施年月日
               05  SPECRES-DATA02-AREA.
      *保険情報
                   07  SPECRES-HKNINF-G.
      *保険者番号
                       09  SPECRES-HKNJANUM          PIC  X(08).
      *記号
                       09  SPECRES-KIGO              PIC  X(80).
      *番号
                       09  SPECRES-NUM               PIC  X(80).
      *番号枝番
                       09  SPECRES-EDABAN            PIC  X(02).
      *
      *            実施年月日
                   07  SPECRES-MEISA-OCC             OCCURS  10.
                       09  SPECRES-JISSIYMD            PIC X(10).
                       09  SPECRES-DATA-AREA.
                           11  SPECRES-DATA-OCC        OCCURS   200.
      *項目コード
                               13  SPECRES-ITEMCODE        PIC X(17).
      *項目名
                               13  SPECRES-ITEMNAME        PIC X(80).
      *データ型
                               13  SPECRES-DATA-TYPE       PIC X(02).
      *データ値
                               13  SPECRES-DATA-VALUE      PIC X(256).
      *単位
                               13  SPECRES-DATA-TANNI      PIC X(26).


