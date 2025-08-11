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
      *  資格確認診療情報格納２   (CPONSHI-SHINRYO-SUB.INC)
      *****************************************************************
      *
      *01  ONSHI-SHINRYO-SUB-REC.
           03  ONS-SNR-S-KEY.
      *医療機関識別番号
               05  ONS-SNR-S-HOSPNUM       PIC 9(02).
      *TBL_UUID
               05  ONS-SNR-S-TBL-UUID      PIC X(36).
      *患者ＩＤ
               05  ONS-SNR-S-PTID          PIC 9(10).
      *診療年月
               05  ONS-SNR-S-SRYYM         PIC X(06).
      *入外等区分
               05  ONS-SNR-S-NYUGAIKBN     PIC X(01).
      *診療日
               05  ONS-SNR-S-SRYDD         PIC X(02).
      *処方箋発行日
               05  ONS-SNR-S-HAKKOYMD      PIC X(08).
      *明細番号
               05  ONS-SNR-S-RENNUM        PIC 9(03).
      *
           03  ONS-SNR-S-DATA.
      *診療識別等区分
               05  ONS-SNR-S-SRYKBN        PIC X(02).
      *診療コード
               05  ONS-SNR-S-SRYCD         PIC X(09).
      *診療名
               05  ONS-SNR-S-SRYNAME       PIC X(64).
      *単位
               05  ONS-SNR-S-TANINAME      PIC X(12).
      *使用量
               05  ONS-SNR-S-SURYO         PIC 9(05)V9(05).
      *数量２
               05  ONS-SNR-S-SURYO2        PIC 9(05)V9(05).
      *回数
               05  ONS-SNR-S-KAISU         PIC 9(03).
      *歯科加算情報有無
               05  ONS-SNR-SHIKA-KBN       PIC X(01).
      *端末ＩＤ
           03  ONS-SNR-S-TERMID        PIC X(64).
      *オペレータＩＤ
           03  ONS-SNR-S-OPID          PIC X(16).
      *作成年月日
           03  ONS-SNR-S-CREYMD        PIC X(08).
      *作成時間
           03  ONS-SNR-S-CREHMS        PIC X(06).
      *更新年月日
           03  ONS-SNR-S-UPYMD         PIC X(08).
      *更新時間
           03  ONS-SNR-S-UPHMS         PIC X(06).
      *
