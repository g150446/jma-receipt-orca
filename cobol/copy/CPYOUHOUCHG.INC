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
      *        用法コード対応テーブル  COPY (CPYOUHOUCHG.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  YOUHOUCHG-REC.
           03  YHCHG-HOSPNUM           PIC  9(2).
      *用法コード
           03  YHCHG-SRYCD             PIC  X(09).
      *標準用法コード（汎用用法コード）
           03  YHCHG-CODE              PIC  X(16).
      *汎用用法コード出力区分
           03  YHCHG-YOUHOUFLG1        PIC  9(01).
      *漢字名称補足出力区分
           03  YHCHG-YOUHOUFLG2        PIC  9(01).
      *用法置換え区分
           03  YHCHG-YOUHOUFLG3        PIC  9(01).
      *
           03  YHCHG-TERMID            PIC  X(64).
           03  YHCHG-OPID              PIC  X(16).
           03  YHCHG-CREYMD            PIC  X(08).
           03  YHCHG-CREHMS            PIC  X(06).
           03  YHCHG-UPYMD             PIC  X(08).
           03  YHCHG-UPHMS             PIC  X(06).
