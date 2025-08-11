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
      *  資格確認保険証OSC画面情報   (CPONSHI-IMAGE.INC)
      *****************************************************************
      *
      *01  ONSHI-IMAGE-REC.
           03  ONS-IMAGE-KEY.
      *医療機関識別番号
               05  ONS-IMAGE-HOSPNUM       PIC 9(02).
      *TBL_UUID
               05  ONS-IMAGE-TBL-UUID      PIC X(36).
      *保険証OCRファイル名
               05  ONS-IMAGE-FILE-NAME     PIC X(100).
      *ステータス
           03  ONS-IMAGE-FILE-STATUS       PIC  X(01).
      *画面データ
           03  ONS-IMAGE-DATA              PIC  X(500000).
      *端末ＩＤ
           03  ONS-IMAGE-TERMID        PIC X(64).
      *オペレータＩＤ
           03  ONS-IMAGE-OPID          PIC X(16).
      *作成年月日
           03  ONS-IMAGE-CREYMD        PIC X(08).
      *作成時間
           03  ONS-IMAGE-CREHMS        PIC X(06).
      *更新年月日
           03  ONS-IMAGE-UPYMD         PIC X(08).
      *更新時間
           03  ONS-IMAGE-UPHMS         PIC X(06).
