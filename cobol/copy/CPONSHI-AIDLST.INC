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
      *  資格確認　医療扶助交付番号格納  (CPONSHI-AIDLST.INC)
      *****************************************************************
      *
      *01  ONSHI-AIDLST-REC.
           03  ONS-AIDLST-KEY.
      *医療機関識別番号
               05  ONS-AIDLST-HOSPNUM       PIC 9(02).
      *UID
               05  ONS-AIDLST-TBL-UUID      PIC X(36).
      *FUJYO_UID
           03  ONS-AIDLST-FUJYO-UUID       PIC X(36).
      *患者ＩＤ
           03  ONS-AIDLST-PTID             PIC 9(10).
      *負担者番号
           03  ONS-AIDLST-FTNJANUM         PIC X(08).
      *受給者番号
           03  ONS-AIDLST-JKYSNUM          PIC X(20).
      *連番
           03  ONS-AIDLST-RENNUM           PIC 9(03).
      *医療券・調剤券有効開始年月日
           03  ONS-AIDLST-TEKSTYMD         PIC X(08).
      *医療券・調剤券有効終了年月日
           03  ONS-AIDLST-TEKEDYMD         PIC X(08).
      *診療別
           03  ONS-AIDLST-MEDICAL-TYPE     PIC X(02).
      *入外区分
           03  ONS-AIDLST-NYUGAIKBN        PIC X(01).
      *診療年月
           03  ONS-AIDLST-SRYYM            PIC X(06).
      *交付番号
           03  ONS-AIDLST-KOFUNUM          PIC X(15).
      *本人支払額
           03  ONS-AIDLST-FTNGAKU          PIC 9(07).
      *
           03  ONS-AIDLST-IMAGE-STATUS     PIC X(01).
           03  ONS-AIDLST-IMAGE-DATA       PIC X(20000).
      *端末ＩＤ
           03  ONS-AIDLST-TERMID        PIC X(64).
      *オペレータＩＤ
           03  ONS-AIDLST-OPID          PIC X(16).
      *作成年月日
           03  ONS-AIDLST-CREYMD        PIC X(08).
      *作成時間
           03  ONS-AIDLST-CREHMS        PIC X(06).
      *更新年月日
           03  ONS-AIDLST-UPYMD         PIC X(08).
      *更新時間
           03  ONS-AIDLST-UPHMS         PIC X(06).
