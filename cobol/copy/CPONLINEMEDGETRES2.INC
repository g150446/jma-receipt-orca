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
      *    ＡＰＩ資格確認診療情報（医科）取得   COPY (CPONLINEMEDGETRES2.INC)*
      *****************************************************************
       01  XML-ONLINEMEDGETRES2.
      *    03  MEDRES2-CONTEXT             PIC S9(9)   BINARY.
           03  MEDRES2-OBJECT              PIC X(36).
           03  MEDRES2-MODE                PIC S9(9)   BINARY.
      *    03  MEDRES2-RECORDNAME          PIC X(128).
           03  MEDRES2-PATIENTRES.
      *       実施日
              05  MEDRES2-INFORMATION-DATE         PIC X(10).
      *       実施時間
              05  MEDRES2-INFORMATION-TIME         PIC X(08).
      *       返却コード
              05  MEDRES2-API-RESULT               PIC X(03).
      *       返却メッセージ
              05  MEDRES2-API-RESULT-MSG           PIC X(100).
      *
     ***      05  MEDRES2-RESKEY                   PIC X(100).
      *       患者情報
              05  MEDRES2-PATIENT-INFOMATION.
      *           患者番号
                  07  MEDRES2-PATIENTID            PIC X(20).
      *           患者氏名
                  07  MEDRES2-NAME                 PIC X(100).
      *           患者カナ氏名
                  07  MEDRES2-KANANAME             PIC X(100).
      *           誕生日
                  07  MEDRES2-BIRTHDAY             PIC X(10).
      *           性別
                  07  MEDRES2-SEX                  PIC X(1).
      *
      *保険情報
                   07  MEDRES2-HKNINF-G.
      *保険者番号
                       09  MEDRES2-HKNJANUM          PIC  X(08).
      *記号
                       09  MEDRES2-KIGO              PIC  X(80).
      *番号
                       09  MEDRES2-NUM               PIC  X(80).
      *番号枝番
                       09  MEDRES2-EDABAN            PIC  X(02).
      *
      *        資格登録年月
               05  MEDRES2-DATA02-AREA.
                   07  MEDRES2-MEISA-AREA.
      *入外等の別
                       09  MEDRES2-NYUGAIKBN           PIC  X(01).
      *医療機関・薬局コード
                       09  MEDRES2-HOSPCD              PIC  X(10).
      *医療機関・薬局名
                       09  MEDRES2-HOSPNAME            PIC  X(80).
      *処方箋発行医療機関コード
                       09  MEDRES2-CHOZAICD            PIC  X(10).
      *処方箋発行医療機関名
                       09  MEDRES2-CHOZAINAME          PIC  X(80).
      *
                       09  MEDRES2-DATE-REC.
      *診療日
                           11  MEDRES2-DI-DATE       PIC X(10).
      *処方箋発行日
                           11  MEDRES2-PR-DATE       PIC X(10).
      *診療情報群
                           11  MEDRES2-PR-DATA-INFG.
      *診療情報
                               13  MEDRES2-PR-DATA-INF
                                                         OCCURS  200.
      *診療識別等区分
                                   15  MEDRES2-SRYKBN   PIC X(02).
      *診療行為コード
                                   15  MEDRES2-SRYCD    PIC X(09).
      *診療行為名
                                   15  MEDRES2-SRYNAME  PIC X(64).
      *単位
                                   15  MEDRES2-TANINAME PIC X(12).
      *使用量
                                   15  MEDRES2-SURYO    PIC X(11).
      *数量２
                                   15  MEDRES2-SURYO2   PIC X(11).
      *回数
                                   15  MEDRES2-KAISU    PIC X(03).

