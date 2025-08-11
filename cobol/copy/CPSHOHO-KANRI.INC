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
      ************************************************************
      *    処方箋管理テーブル 
      *                            COPY (CPSHOHO-KANRI.INC)
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SHOHO-KANRI-REC.
           03  SHOKANR-KEY.
      *医療機関ＩＤ
               05  SHOKANR-HOSPNUM         PIC 9(02).
      *ＵＩＤ
               05  SHOKANR-TBL-UUID        PIC X(36).
      *連番
               05  SHOKANR-RENNUM          PIC 9(01).
      *診療日
           03  SHOKANR-SRYYMD          PIC  X(08).
      *患者ＩＤ
           03  SHOKANR-PTID            PIC  9(10).
      *発行元区分（１：API中途データ送信）
           03  SHOKANR-HAKKOKBN        PIC  X(01).
      *入外区分
           03  SHOKANR-NYUGAIKBN       PIC  X(01).
      *診療科
           03  SHOKANR-SRYKA           PIC  X(02).
      *保険組合せ番号
           03  SHOKANR-HKNCOMBI        PIC  X(04).
      *伝票番号
           03  SHOKANR-DENPNUM         PIC  9(07).
      *処方箋発行区分
           03  SHOKANR-SHOHO-KEITAI    PIC  X(01).
      *処方箋ＩＤ
           03  SHOKANR-PRESCRIPTIONID  PIC  X(36).
      *引換番号
           03  SHOKANR-ACCESSCODE      PIC  X(16).
      *2024/01
      *リフィル回数
           03  SHOKANR-REFILL-NUM      PIC  9(01).
      *リフィル処方日数
           03  SHOKANR-REFILL-ZAIKAISU PIC  9(03).
      *処方箋取消日時
           03  SHOKANR-CANCEL-TIME     PIC  X(14).
      *処方箋取消UNDO日時
           03  SHOKANR-CANCEL-UD-TIME  PIC  X(14).
      *処方箋変更日時
           03  SHOKANR-CHANGE-TIME     PIC  X(14).
      *処方箋変更UNDO日時
           03  SHOKANR-CHANGE-UD-TIME  PIC  X(14).
      *端末ＩＤ
           03  SHOKANR-TERMID          PIC  X(32).
      *オペレータＩＤ
           03  SHOKANR-OPID            PIC  X(16).
      *作成年月日
           03  SHOKANR-CREYMD          PIC  X(08).
      *作成時間
           03  SHOKANR-CREHMS          PIC  X(06).
      *更新年月日
           03  SHOKANR-UPYMD           PIC  X(08).
      *更新時間
           03  SHOKANR-UPHMS           PIC  X(06).
      *
