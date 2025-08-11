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
      *  資格確認薬剤情報格納１   (CPONSHI-YAKUZAI-MAIN.INC)
      *****************************************************************
      *
      *01  ONSHI-YAKUZAI-MAIN-REC.
           03  ONS-YKZ-M-KEY.
      *医療機関識別番号
               05  ONS-YKZ-M-HOSPNUM       PIC 9(02).
      *TBL_UUID(2022/08)
               05  ONS-YKZ-M-TBL-UUID      PIC X(36).
      *患者ＩＤ
               05  ONS-YKZ-M-PTID          PIC 9(10).
      *診療年月
               05  ONS-YKZ-M-SRYYM         PIC X(06).
      *入外区分（４：調剤）
               05  ONS-YKZ-M-NYUGAIKBN     PIC X(01).
      *保険情報
           03  ONS-YKZ-M-HKNINF-G.
      *保険者番号
               05  ONS-YKZ-M-HKNJANUM      PIC  X(08).
      *記号
               05  ONS-YKZ-M-KIGO          PIC  X(80).
      *番号
               05  ONS-YKZ-M-NUM           PIC  X(80).
      *番号枝番
               05  ONS-YKZ-M-EDABAN        PIC  X(02).
      *2022/08
      *医療機関コード
               05  ONS-YKZ-M-HOSPCD        PIC  X(10).
      *医療機関名
               05  ONS-YKZ-M-HOSPNAME      PIC  X(80).
      *調剤薬局コード
               05  ONS-YKZ-M-CHOZAICD      PIC  X(10).
      *調剤薬局名
               05  ONS-YKZ-M-CHOZAINAME    PIC  X(80).
      ***
      *調剤機関毎連番
           03  ONS-YKZ-M-CHOZAI-SEQNUM PIC 9(03).
      *調剤機関区分
           03  ONS-YKZ-M-CHOZAI-KBN    PIC X(01).
      *処方機関毎連番
           03  ONS-YKZ-M-SHOHO-SEQNUM  PIC 9(03).
      *処方箋発行機関区分
           03  ONS-YKZ-M-SHOHO-KBN     PIC X(01).
      *診療日
           03  ONS-YKZ-M-SRYDD-AREA.
               05  ONS-YKZ-M-SRYDD     PIC X(02)
                                               OCCURS  31.
      *端末ＩＤ
           03  ONS-YKZ-M-TERMID        PIC X(64).
      *オペレータＩＤ
           03  ONS-YKZ-M-OPID          PIC X(16).
      *作成年月日
           03  ONS-YKZ-M-CREYMD        PIC X(08).
      *作成時間
           03  ONS-YKZ-M-CREHMS        PIC X(06).
      *更新年月日
           03  ONS-YKZ-M-UPYMD         PIC X(08).
      *更新時間
           03  ONS-YKZ-M-UPHMS         PIC X(06).
