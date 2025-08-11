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
      *  資格確認特定検診情報格納１   (CPONSHI-KENSIN-MAIN.INC)
      *****************************************************************
      *
      *01  ONSHI-KENSIN-MAIN-REC.
           03  ONS-KNS-M-KEY.
      *医療機関識別番号
               05  ONS-KNS-M-HOSPNUM       PIC 9(02).
      *患者ＩＤ
               05  ONS-KNS-M-PTID          PIC 9(10).
      *実施年月日
               05  ONS-KNS-M-JISSHIYMD     PIC X(08).
      *保険情報
           03  ONS-KNS-M-HKNINF-G.
      *保険者番号
               05  ONS-KNS-M-HKNJANUM      PIC  X(08).
      *記号
               05  ONS-KNS-M-KIGO          PIC  X(80).
      *番号
               05  ONS-KNS-M-NUM           PIC  X(80).
      *番号枝番
               05  ONS-KNS-M-EDABAN        PIC  X(02).
      *端末ＩＤ
           03  ONS-KNS-M-TERMID        PIC X(64).
      *オペレータＩＤ
           03  ONS-KNS-M-OPID          PIC X(16).
      *作成年月日
           03  ONS-KNS-M-CREYMD        PIC X(08).
      *作成時間
           03  ONS-KNS-M-CREHMS        PIC X(06).
      *更新年月日
           03  ONS-KNS-M-UPYMD         PIC X(08).
      *更新時間
           03  ONS-KNS-M-UPHMS         PIC X(06).
