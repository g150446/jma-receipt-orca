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
      *    ＡＰＩ資格確認薬剤情報　取得   COPY (CPONLINEDRUGGETRES1.INC)*
      *****************************************************************
       01  XML-ONLINEDRUGGETRES1.
           03  DRGRES1-CONTEXT             PIC S9(9)   BINARY.
           03  DRGRES1-OBJECT              PIC X(36).
           03  DRGRES1-MODE                PIC S9(9)   BINARY.
           03  DRGRES1-RECORDNAME          PIC X(128).
           03  DRGRES1-PATIENTRES.
      *       実施日
              05  DRGRES1-INFORMATION-DATE         PIC X(10).
      *       実施時間
              05  DRGRES1-INFORMATION-TIME         PIC X(08).
      *       返却コード
              05  DRGRES1-API-RESULT               PIC X(03).
      *       返却メッセージ
              05  DRGRES1-API-RESULT-MSG           PIC X(100).
      *
              05  DRGRES1-RESKEY                   PIC X(100).
      *       患者情報
              05  DRGRES1-PATIENT-INFOMATION.
      *           患者番号
                  07  DRGRES1-PATIENTID            PIC X(20).
      *           患者氏名
                  07  DRGRES1-NAME                 PIC X(100).
      *           患者カナ氏名
                  07  DRGRES1-KANANAME             PIC X(100).
      *           誕生日
                  07  DRGRES1-BIRTHDAY             PIC X(10).
      *           性別
                  07  DRGRES1-SEX                  PIC X(1).
      *
      *        資格年月
               05  DRGRES1-SRYYM               PIC X(07).
      *リクエスト番号＝０１
      *        資格登録年月
               05  DRGRES1-DATA01-AREA.
                   07  DRGRES1-TOR-SRYYM-TBL       OCCURS   12.
                       09  DRGRES1-TOR-SRYYM       PIC X(07).
                       09  DRGRES1-TOR-HOSP-TBL    OCCURS   20. 
      *                    医療機関コード
                           11  DRGRES1-TOR-HOSPCD      PIC  X(10).
      *                    医療機関名
                           11  DRGRES1-TOR-HOSPNAME    PIC  X(80).
      *                    調剤薬局コード
                           11  DRGRES1-TOR-CHOZAICD    PIC  X(10).
      *                    調剤薬局名
                           11  DRGRES1-TOR-CHOZAINAME  PIC  X(80).
      *リクエスト番号＝０２
      *        資格登録年月
               05  DRGRES1-DATA02-AREA.
      *保険情報
                   07  DRGRES1-HKNINF-G.
      *保険者番号
                       09  DRGRES1-HKNJANUM          PIC  X(08).
      *記号
                       09  DRGRES1-KIGO              PIC  X(80).
      *番号
                       09  DRGRES1-NUM               PIC  X(80).
      *番号枝番
                       09  DRGRES1-EDABAN            PIC  X(02).
      *
                   07  DRGRES1-MEISA-AREA.
                       09  DRGRES1-MEI-TBL           OCCURS   3.
      *            入外等の別
                       11  DRGRES1-NYUGAIKBN           PIC X(01).
      *
                       11  DRGRES1-DATE-REC.
      *調剤日別情報
      *2022/08*****        13  DRGRES1-DI-DATE-OCC       OCCURS   31.
                           13  DRGRES1-DI-DATE-OCC       OCCURS   100.
      *調剤日
                               15  DRGRES1-DI-DAY        PIC X(02).
      *処方箋発行日
                               15  DRGRES1-PR-DATE       PIC X(10).
      *                    医療機関コード
                               15  DRGRES1-HOSPCD        PIC  X(10).
      *                    医療機関名
                               15  DRGRES1-HOSPNAME      PIC  X(80).
      *                    調剤薬局コード
                               15  DRGRES1-CHOZAICD      PIC  X(10).
      *                    調剤薬局名
                               15  DRGRES1-CHOZAINAME    PIC  X(80).
      *薬剤情報群
                               15  DRGRES1-PR-DATA-INFG.
      *薬剤情報
                                   16  DRGRES1-PR-DATA-INF
      *2022/08*****                                      OCCURS  100.
                                                         OCCURS  60.
      *診療識別等区分
                                       17  DRGRES1-SRYKBN   PIC X(02).
      *用法コード
                                       17  DRGRES1-YOHOCD   PIC X(03).
      *用法名称
                                       17  DRGRES1-YOHONAME PIC X(30).
      *特別指示
                                       17  DRGRES1-SHIJI    PIC X(80).
      *医薬品コード
                                       17  DRGRES1-SRYCD    PIC X(09).
      *薬剤名
                                       17  DRGRES1-YAKUZAINAME
                                                            PIC X(64).
      *成分名
                                       17  DRGRES1-SEIBUNNAME
                                                            PIC X(200).
      *単位
                                       17  DRGRES1-TANINAME PIC X(12).
      *使用量
                                       17  DRGRES1-SURYO    PIC X(11).
      *1回用量
                                       17  DRGRES1-YORYO    PIC X(11).
      *回数
                                       17  DRGRES1-KAISU    PIC X(03).


