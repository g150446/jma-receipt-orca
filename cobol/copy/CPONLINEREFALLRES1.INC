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
      *    ＡＰＩ照会番号一括登録用   COPY (CPONLINEREFALLRES1.INC)      *
      *****************************************************************
       01  XML-ONLINEREFALLRES1.
           02  REFALLRES1-CONTEXT          PIC S9(9)   BINARY.
           02  REFALLRES1-OBJECT           PIC X(36).
           02  REFALLRES1-MODE             PIC S9(9)   BINARY.
           02  REFALLRES1-RECORDNAME       PIC X(128).
           02  REFALLRES1-QUARES1.
      *実施日
               03  REFALLRES1-INFORMATION-DATE
                                           PIC X(10).
      *実施時間
               03  REFALLRES1-INFORMATION-TIME
                                           PIC X(8).
      *返却コード
               03  REFALLRES1-API-RESULT   PIC X(3).
      *返却メッセージ
               03  REFALLRES1-API-RESULT-MSG
                                           PIC X(100).
      *照会番号継続フラグ
               03  REFALLRES1-REF-CONT     PIC X(3).
      *最終照会番号(受け取った最後の照会番号(ptid))
               03  REFALLRES1-PTID         PIC X(10).
      *照会番号登録
               03  REFALLRES1-HEADER.
      *医療機関コード          
                   05  REFALLRES1-HOSPCD   PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   05  REFALLRES1-SIKIBETU PIC X(50).
      *メッセージボディ部          
               03  REFALLRES1-BODY.
      *照会番号登録情報
                   05  REFALLRES1-BODY-INFO    OCCURS  5000.
      *照会番号
                       07  REFALLRES1-SHOKAI-NUM
                                               PIC X(50).
      *保険者番号
                       07  REFALLRES1-HKNJANUM PIC X(08).
      *記号
                       07  REFALLRES1-KIGO     PIC X(80).
      *番号
                       07  REFALLRES1-NUM      PIC X(80).
      *番号枝番
                       07  REFALLRES1-EDABAN   PIC X(02).
