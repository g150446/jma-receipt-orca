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
      *    ＡＰＩ資格確認　歯科診療情報用    COPY (CPONLINEQUAREQ8.INC)*
      *****************************************************************
       01  XML-ONLINEQUAREQ8.
      ***  03  QUAREQ8-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ8-OBJECT              PIC X(36).
           03  QUAREQ8-MODE                PIC S9(9)   BINARY.
      ***  03  QUAREQ8-RECORDNAME          PIC X(128).
           03  QUAREQ8-PATIENTREQ.
      *メッセージヘッダー部     
               05  QUAREQ8-HEADER.
      *要求保険者番号
                       09  QUAREQ8-REQ-HKNJANUM    PIC X(08).
      *記号
                       09  QUAREQ8-REQ-KIGO        PIC X(80).
      *番号
                       09  QUAREQ8-REQ-NUM         PIC X(80).
      *番号枝番
                       09  QUAREQ8-REQ-EDABAN      PIC X(02).
      *要求任意のファイル識別子(医療機関固有項目)
                       09  QUAREQ8-FILE-SIKIBETU   PIC X(50).
      *処理結果区分(薬剤)
                       09  QUAREQ8-RESULTCODE      PIC X(01).
      *メッセージボディ部
               05  QUAREQ8-MESSAGEBODY.
                   07  QUAREQ8-MESSAGE-BODY-R.
      *診療年月別情報
      *診療年月
                       09  QUAREQ8-SRYYM         PIC X(06).
      *入外等の別
                       09  QUAREQ8-NYUGAIKBN     PIC X(01).
      *診療機関毎連番
                       09  QUAREQ8-GRPNUM        PIC X(03).
      *診療機関区分
                       09  QUAREQ8-DIORG         PIC X(01).
      *処方機関毎連番
                       09  QUAREQ8-GRPNUM2       PIC X(03).
      *処方箋発行機関区分
                       09  QUAREQ8-PR-ISORG      PIC X(01).
      *医療機関・薬局コード
                       09  QUAREQ8-PR-METRDIHCD  PIC X(10).
      *医療機関・薬局名
                       09  QUAREQ8-PR-METRDIHNM  PIC X(80).
      *処方箋発行医療機関コード
                       09  QUAREQ8-PR-PRLSHCD    PIC X(10).
      *処方箋発行医療機関名
                       09  QUAREQ8-PR-PRLSHNM    PIC X(80).
      ***
      *診療日別情報群
                       09  QUAREQ8-DI-DATE-REC.
      *診療日別情報
                           11  QUAREQ8-DI-DATE-OCC.
      *診療日
                               13  QUAREQ8-DI-DATE      PIC X(08).
      *処方箋発行日
                               13  QUAREQ8-PR-DATE      PIC X(08).
      *診療情報群
                               13  QUAREQ8-PR-DATA-INFG.
      *診療情報
                                   15  QUAREQ8-PR-DATA-INF
                                                         OCCURS  100.
      *診療識別等区分
                                       17  QUAREQ8-SRYKBN   PIC X(02).
      *診療行為コード
                                       17  QUAREQ8-SRYCD    PIC X(09).
      *診療行為名
                                       17  QUAREQ8-SRYNAME  PIC X(64).
      *単位
                                       17  QUAREQ8-UNIT     PIC X(12).
      *使用量
                                       17  QUAREQ8-QUA1     PIC X(11).
      *数量２
                                       17  QUAREQ8-QUA2     PIC X(08).
      *回数
                                       17  QUAREQ8-TIMES    PIC X(03).
      *歯科診療行為加算情報
                                       17  QUAREQ8-DEN-INF
                                                         OCCURS  30.
      *診療行為加算コード
                                           19  QUAREQ8-DEN-ADDCD
                                                          PIC X(05).
      *診療行為加算名称
                                           19  QUAREQ8-DEN-ADDNAME
                                                          PIC X(64).
      *診療行為加算数量
                                           19  QUAREQ8-DEN-QUA
                                                          PIC X(08).
      *単位
                                           19  QUAREQ8-DEN-UNIT
                                                          PIC X(12).



