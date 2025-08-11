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
      ********************************************************************
      ************************************************************
      *    保険者付加        COPY (CPHKNJAPLUS.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  HKNJAPLUS-REC.
      *医療機関ＩＤ
           03  HKNJAPLUS-HOSPNUM             PIC 9(02).
      *保険者番号
           03  HKNJAPLUS-HKNJANUM            PIC X(08).
      *標準記号オフ
           03  HKNJAPLUS-KIGOOFF             PIC X(01).
      *記号１
           03  HKNJAPLUS-KIGO1               PIC X(80).
      *端末ＩＤ
           03  HKNJAPLUS-TERMID              PIC X(32).
      *オペーレータＩＤ
           03  HKNJAPLUS-OPID                PIC X(16).
      *作成年月日
           03  HKNJAPLUS-CREYMD              PIC X(08).
      *更新年月日
           03  HKNJAPLUS-UPYMD               PIC X(08).
      *更新時間
           03  HKNJAPLUS-UPHMS               PIC X(06).
