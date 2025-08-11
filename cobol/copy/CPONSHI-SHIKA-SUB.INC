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
      *  資格確認診療情報歯科情報格納   (CPONSHI-SHIKA-SUB.INC)
      *****************************************************************
      *
      *01  ONSHI-SHIKA-SUB-REC.
           03  ONS-SIK-S-KEY.
      *医療機関識別番号
               05  ONS-SIK-S-HOSPNUM       PIC 9(02).
      *TBL_UUID
               05  ONS-SIK-S-TBL-UUID      PIC X(36).
      *患者ＩＤ
               05  ONS-SIK-S-PTID          PIC 9(10).
      *診療年月
               05  ONS-SIK-S-SRYYM         PIC X(06).
      *入外等区分
               05  ONS-SIK-S-NYUGAIKBN     PIC X(01).
      *診療日
               05  ONS-SIK-S-SRYDD         PIC X(02).
      *処方箋発行日
               05  ONS-SIK-S-HAKKOYMD      PIC X(08).
      *明細番号
               05  ONS-SIK-S-RENNUM        PIC 9(03).
      *
      *歯科明細番号
               05  ONS-SIK-S-RENNUM1       PIC 9(03).
      *
           03  ONS-SIK-S-DATA.
      *診療行為加算コード
               05  ONS-SIK-S-SRYKASANCD      PIC X(05).
      *診療行為加算名
               05  ONS-SIK-S-SRYKASAN-NAME   PIC X(64).
      *使用量
               05  ONS-SIK-S-SURYO           PIC 9(05)V9(05).
      *単位
               05  ONS-SIK-S-TANINAME        PIC X(12).
      *端末ＩＤ
           03  ONS-SIK-S-TERMID        PIC X(64).
      *オペレータＩＤ
           03  ONS-SIK-S-OPID          PIC X(16).
      *作成年月日
           03  ONS-SIK-S-CREYMD        PIC X(08).
      *作成時間
           03  ONS-SIK-S-CREHMS        PIC X(06).
      *更新年月日
           03  ONS-SIK-S-UPYMD         PIC X(08).
      *更新時間
           03  ONS-SIK-S-UPHMS         PIC X(06).
      *
