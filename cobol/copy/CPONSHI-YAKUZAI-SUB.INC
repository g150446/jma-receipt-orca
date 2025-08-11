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
      *  資格確認薬剤情報格納２   (CPONSHI-YAKUZAI-SUB.INC)
      *****************************************************************
      *
      *01  ONSHI-YAKUZAI-SUB-REC.
           03  ONS-YKZ-S-KEY.
      *医療機関識別番号
               05  ONS-YKZ-S-HOSPNUM       PIC 9(02).
      *TBL_UUID
               05  ONS-YKZ-S-TBL-UUID      PIC X(36).
      *患者ＩＤ
               05  ONS-YKZ-S-PTID          PIC 9(10).
      *診療年月
               05  ONS-YKZ-S-SRYYM         PIC X(06).
      *入外等区分
               05  ONS-YKZ-S-NYUGAIKBN     PIC X(01).
      *調剤機関毎連番
               05  ONS-YKZ-S-CHOZAI-SEQNUM PIC 9(03).
      *処方機関毎連番
               05  ONS-YKZ-S-SHOHO-SEQNUM  PIC 9(03).
      *診療日
               05  ONS-YKZ-S-SRYDD         PIC X(02).
      *処方箋発行日
               05  ONS-YKZ-S-HAKKOYMD      PIC X(08).
      *明細番号
               05  ONS-YKZ-S-RENNUM        PIC 9(03).
      *
           03  ONS-YKZ-S-DATA.
      *診療識別等区分
               05  ONS-YKZ-S-SRYKBN        PIC X(02).
      *用法コード
               05  ONS-YKZ-S-YOHOCD        PIC X(03).
      *用法名称
               05  ONS-YKZ-S-YOHONAME      PIC X(30).
      *特別指示
               05  ONS-YKZ-S-SHIJI         PIC X(80).
      *医薬品コード
               05  ONS-YKZ-S-SRYCD         PIC X(09).
      *薬剤名
               05  ONS-YKZ-S-YAKUZAINAME   PIC X(64).
      *成分名
               05  ONS-YKZ-S-SEIBUNNAME    PIC X(200).
      *単位
               05  ONS-YKZ-S-TANINAME      PIC X(12).
      *使用量
               05  ONS-YKZ-S-SURYO         PIC 9(05)V9(05).
      *１回用量
               05  ONS-YKZ-S-YORYO         PIC 9(05)V9(05).
      *回数
               05  ONS-YKZ-S-KAISU         PIC 9(03).
      *端末ＩＤ
           03  ONS-YKZ-S-TERMID        PIC X(64).
      *オペレータＩＤ
           03  ONS-YKZ-S-OPID          PIC X(16).
      *作成年月日
           03  ONS-YKZ-S-CREYMD        PIC X(08).
      *作成時間
           03  ONS-YKZ-S-CREHMS        PIC X(06).
      *更新年月日
           03  ONS-YKZ-S-UPYMD         PIC X(08).
      *更新時間
           03  ONS-YKZ-S-UPHMS         PIC X(06).
      *
