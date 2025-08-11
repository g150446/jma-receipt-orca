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
      *    ＡＰＩ資格確認　薬剤情報用       COPY (CPONLINEQUAREQ4.INC)*
      *****************************************************************
       01  XML-ONLINEQUAREQ4.
           03  QUAREQ4-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ4-OBJECT              PIC X(36).
           03  QUAREQ4-MODE                PIC S9(9)   BINARY.
           03  QUAREQ4-RECORDNAME          PIC X(128).
           03  QUAREQ4-PATIENTREQ.
      *メッセージヘッダー部     
               05  QUAREQ4-HEADER.
      *処理実行日時
                   07  QUAREQ4-SHORI-TIME  PIC X(14).
      *要求情報
                   07  QUAREQ4-REQUESTINFO-INFO.
      *要求医療機関(薬局)コード
                       09  QUAREQ4-REQ-HOSPCD      PIC X(10).
      *要求保険者番号
                       09  QUAREQ4-REQ-HKNJANUM    PIC X(08).
      *記号
                       09  QUAREQ4-REQ-KIGO        PIC X(80).
      *番号
                       09  QUAREQ4-REQ-NUM         PIC X(80).
      *番号枝番
                       09  QUAREQ4-REQ-EDABAN      PIC X(02).
      *要求任意のファイル識別子(医療機関固有項目)
                       09  QUAREQ4-FILE-SIKIBETU   PIC X(50).
      *要求開始年月
                       09  QUAREQ4-REQ-STRDATE     PIC X(06).
      *要求終了年月
                       09  QUAREQ4-REQ-ENDDATE     PIC X(06).
      *要求前回取得日
                       09  QUAREQ4-REQ-LASTDATE    PIC X(06).
      *要求前回取得日
                       09  QUAREQ4-REQ-FILEGET     PIC X(01).
      *資格情報
                   07  QUAREQ4-QUAINF.
      *被保険者番号
                       09  QUAREQ4-RES-HKNJANUM    PIC X(08).
      *被保険者証記号
                       09  QUAREQ4-RES-KIGO        PIC X(80).
      *被保険者証番号
                       09  QUAREQ4-RES-NUM         PIC X(80).
      *番号枝番
                       09  QUAREQ4-RES-EDABAN      PIC X(02).
      *氏名カナ
                       09  QUAREQ4-RES-KANANAME        PIC X(100).
      *氏名カナ（その他）
                       09  QUAREQ4-RES-KANANAME-ETC    PIC X(100).
      *氏名
                       09  QUAREQ4-RES-NAME            PIC X(100).
      *氏名（その他）
                       09  QUAREQ4-RES-NAME-ETC        PIC X(100).
      *生年月日
                       09  QUAREQ4-RES-BIRTHDAY        PIC X(08).
      *年齢
                       09  QUAREQ4-RES-AGE             PIC X(03).
      *性別１
                       09  QUAREQ4-RES-SEX1            PIC X(01).
      *性別２
                       09  QUAREQ4-RES-SEX2            PIC X(01).
      *照会番号
                       09  QUAREQ4-REFNUM              PIC X(50).
      *処理結果区分(薬剤)
                   07  QUAREQ4-RESULTCODE              PIC X(01).
      *メッセージID
                   07  QUAREQ4-MESSAGE-ID              PIC X(09).
      *メッセージ内容
                   07  QUAREQ4-MESSAGE                 PIC X(120).
      *文字コード識別
                   07  QUAREQ4-CHARACTER               PIC X(01).
      *メッセージボディ部
               05  QUAREQ4-MESSAGEBODY.
                   07  QUAREQ4-MESSAGE-BODY-R      OCCURS   6.
      *診療年月別情報
      *診療年月
                       09  QUAREQ4-SRYYM         PIC X(06).
      *入外等の別
                       09  QUAREQ4-NYUGAIKBN     PIC X(01).
      *調剤機関毎連番
                       09  QUAREQ4-GRPNUM        PIC X(03).
      *調剤機関区分
                       09  QUAREQ4-DIORG         PIC X(01).
      *処方機関毎連番
                       09  QUAREQ4-GRPNUM2       PIC X(03).
      *処方箋発行機関区分
                       09  QUAREQ4-PR-ISORG      PIC X(01).
      *2022/08
      *医療機関・薬局コード
                       09  QUAREQ4-PR-METRDIHCD  PIC X(10).
      *医療機関・薬局名
                       09  QUAREQ4-PR-METRDIHNM  PIC X(80).
      *処方箋発行医療機関コード
                       09  QUAREQ4-PR-PRLSHCD    PIC X(10).
      *処方箋発行医療機関名
                       09  QUAREQ4-PR-PRLSHNM    PIC X(80).
      ***
      *調剤日別情報群
                       09  QUAREQ4-DI-DATE-REC.
      *調剤日別情報
                           11  QUAREQ4-DI-DATE-OCC      OCCURS   31.
      *調剤日
                               13  QUAREQ4-DI-DATE      PIC X(08).
      *処方箋発行日
                               13  QUAREQ4-PR-DATE      PIC X(08).
      *薬剤情報群
                               13  QUAREQ4-PR-DATA-INFG.
      *薬剤情報
                                   15  QUAREQ4-PR-DATA-INF
                                                         OCCURS  100.
      *診療識別等区分
                                       17  QUAREQ4-SRYKBN   PIC X(02).
      *用法コード
                                       17  QUAREQ4-USAGEC   PIC X(03).
      *用法名称
                                       17  QUAREQ4-USAGEN   PIC X(30).
      *特別指示
                                       17  QUAREQ4-SPINST   PIC X(80).
      *医薬品コード
                                       17  QUAREQ4-DRUGCD   PIC X(09).
      *薬剤名
                                       17  QUAREQ4-DRUGNAME PIC X(64).
      *成分名
                                       17  QUAREQ4-INGRENAME
                                                            PIC X(200).
      *単位
                                       17  QUAREQ4-UNIT     PIC X(12).
      *使用量
                                       17  QUAREQ4-QUAL     PIC X(11).
      *1回用量
                                       17  QUAREQ4-DNDOSE   PIC X(11).
      *回数
                                       17  QUAREQ4-TIMES    PIC X(03).


