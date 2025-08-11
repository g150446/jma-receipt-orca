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
      *    ＡＰＩ資格確認診療情報　取得   COPY (CPONLINEMEDGETRES1.INC)*
      *****************************************************************
       01  XML-ONLINEMEDGETRES1.
           03  MEDRES1-CONTEXT             PIC S9(9)   BINARY.
           03  MEDRES1-OBJECT              PIC X(36).
           03  MEDRES1-MODE                PIC S9(9)   BINARY.
           03  MEDRES1-RECORDNAME          PIC X(128).
           03  MEDRES1-PATIENTRES.
      *       実施日
              05  MEDRES1-INFORMATION-DATE         PIC X(10).
      *       実施時間
              05  MEDRES1-INFORMATION-TIME         PIC X(08).
      *       返却コード
              05  MEDRES1-API-RESULT               PIC X(03).
      *       返却メッセージ
              05  MEDRES1-API-RESULT-MSG           PIC X(100).
      *
              05  MEDRES1-RESKEY                   PIC X(100).
      *       患者情報
              05  MEDRES1-PATIENT-INFOMATION.
      *           患者番号
                  07  MEDRES1-PATIENTID            PIC X(20).
      *           患者氏名
                  07  MEDRES1-NAME                 PIC X(100).
      *           患者カナ氏名
                  07  MEDRES1-KANANAME             PIC X(100).
      *           誕生日
                  07  MEDRES1-BIRTHDAY             PIC X(10).
      *           性別
                  07  MEDRES1-SEX                  PIC X(1).
      *
      *        資格年月
               05  MEDRES1-SRYYM               PIC X(07).
      *リクエスト番号＝０１
      *        資格登録年月
               05  MEDRES1-DATA01-AREA.
                   07  MEDRES1-TOR-SRYYM-TBL       OCCURS   6.
                       09  MEDRES1-TOR-SRYYM       PIC X(07).
                       09  MEDRES1-TOR-HOSP-TBL    OCCURS   20. 
      *                    医療機関・薬局コード
                           11  MEDRES1-TOR-HOSPCD      PIC  X(10).
      *                    医療機関・薬局名
                           11  MEDRES1-TOR-HOSPNAME    PIC  X(80).
      *                    処方箋発行医療機関コード
                           11  MEDRES1-TOR-CHOZAICD    PIC  X(10).
      *                    処方箋発行医療機関名
                           11  MEDRES1-TOR-CHOZAINAME  PIC  X(80).
      *                    入外等の別
                           11  MEDRES1-TOR-NYUGAIKBN   PIC X(01).


