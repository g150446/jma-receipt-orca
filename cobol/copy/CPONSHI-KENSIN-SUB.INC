      *******************************************************************
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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      *****************************************************************
      *****************************************************************
      *  資格確認特定検診情報格納２   (CPONSHI-KENSIN-SUB.INC)
      *****************************************************************
      *
      *01  ONSHI-KENSIN-SUB-REC.
           03  ONS-KNS-S-KEY.
      *医療機関識別番号
               05  ONS-KNS-S-HOSPNUM       PIC 9(02).
      *患者ＩＤ
               05  ONS-KNS-S-PTID          PIC 9(10).
      *実施年月日
               05  ONS-KNS-S-JISSHIYMD     PIC X(08).
      *連番
               05  ONS-KNS-S-RENNUM        PIC 9(03).
      *健診結果・質問票情報
           03  ONS-KNS-S-KEKKA-G.
      *項目コード
               05  ONS-KNS-S-ITEMCODE      PIC X(17).
      *項目名
               05  ONS-KNS-S-ITEMNAME      PIC X(80).
      *データ型
               05  ONS-KNS-S-DATA-TYPE     PIC X(02).
      *データ値
               05  ONS-KNS-S-DATA-VALUE    PIC X(256).
      *単位
               05  ONS-KNS-S-DATA-TANNI    PIC X(26).
      *端末ＩＤ
           03  ONS-KNS-S-TERMID        PIC X(64).
      *オペレータＩＤ
           03  ONS-KNS-S-OPID          PIC X(16).
      *作成年月日
           03  ONS-KNS-S-CREYMD        PIC X(08).
      *作成時間
           03  ONS-KNS-S-CREHMS        PIC X(06).
      *更新年月日
           03  ONS-KNS-S-UPYMD         PIC X(08).
      *更新時間
           03  ONS-KNS-S-UPHMS         PIC X(06).
