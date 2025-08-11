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
      *    ＡＰＩ資格確認用       COPY (CPONLINEQUAAPPRES1.INC)       *
      *****************************************************************
       01  XML-ONLINEQUAAPPRES1.
           02  QUAAPPRES1-CONTEXT          PIC S9(9)   BINARY.
           02  QUAAPPRES1-OBJECT           PIC X(36).
           02  QUAAPPRES1-MODE             PIC S9(9)   BINARY.
           02  QUAAPPRES1-RECORDNAME       PIC X(128).
           02  QUAAPPRES1-DATAREQ.
      *実施日
               03  QUAAPPRES1-INFORMATION-DATE PIC X(10).
      *実施時間
               03  QUAAPPRES1-INFORMATION-TIME PIC X(8).
      *返却コード
               03  QUAAPPRES1-API-RESULT       PIC X(3).
      *返却メッセージ
               03  QUAAPPRES1-API-RESULT-MSG   PIC X(100).
      *プロセスクラス（訪問同意等の識別)
               03  QUAAPPRES1-PROCESS-CLASS    PIC X(5).
      *一括同意取得用エリア
               03  QUAAPPRES1-IKKATU-DOUI-AREA.
      *医療機関コード
                   04  QUAAPPRES1-IKKATU-HOSPCD
                                               PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   04  QUAAPPRES1-IKKATU-SIKIBETU
                                               PIC X(50).
      * 同意年月日（自）
                   04  QUAAPPRES1-DOUI-STARYMD PIC X(10).
      * 同意年月日（至）
                   04  QUAAPPRES1-DOUI-ENDYMD  PIC X(10).
      * 診療区分
                   04  QUAAPPRES1-SRYKBN       PIC X(1).
      *未実施一覧
               03  QUAAPPRES1-BULK.
                   04  QUAAPPRES1-BULK-INFO    OCCURS  10.
      *予約日
                       05  QUAAPPRES1-YYKYMD   PIC X(10).
      *受付番号
                       05  QUAAPPRES1-YYKNO    PIC X(38).
      *プロセスクラス
                       05  QUAAPPRES1-BULK-PROCESS-CLASS
                                               PIC X(5).
      *一括照会継続フラグ
               03  QUAAPPRES1-CONT-FLG         PIC X(3).
      *最終予約番号(受け取った最後の予約カウント)
               03  QUAAPPRES1-LAST-YYKNO       PIC X(10).
      * 一括資格確認
               03  QUAAPPRES1-HEADER.
      *資格確認日
                   04  QUAAPPRES1-KAKUNINYMD   PIC X(12).
      *医療機関コード          
                   04  QUAAPPRES1-HOSPCD       PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   04  QUAAPPRES1-SIKIBETU     PIC X(50).
      *メッセージボディ部          
               03  QUAAPPRES1-MESSAGEBODY.
      *一括資格確認情報
                   04  QUAAPPRES1-INFO OCCURS  300.
      *保険者番号
                       05  QUAAPPRES1-HKNJANUM PIC X(08).
      *記号
                       05  QUAAPPRES1-KIGO     PIC X(80).
      *番号
                       05  QUAAPPRES1-NUM      PIC X(80).
      *番号枝番
                       05  QUAAPPRES1-EDABAN   PIC X(02).
      *生年月日
                       05  QUAAPPRES1-BIRTHDAY PIC X(10).
      *限度額適用認定証区分
                       05  QUAAPPRES1-GENDO-SHOKBN
                                               PIC X(02).
      *限度額適用認定証適用区分
                       05  QUAAPPRES1-GENDO-TEKKBN
                                               PIC X(03).
      *任意のファイル識別子(医療機関固有項目)
                       05  QUAAPPRES1-RES-SIKIBETU
                                               PIC X(50).
