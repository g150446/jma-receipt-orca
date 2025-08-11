      ******************************************************************
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
      ******************************************************************
      *    レセプトコメントチェック
      ******************************************************************
       01  ORCSRECECOMCHKAREA.
      *    入力項目
           03  ORCSRECECOMCHK-IN-AREA.
      *        入外区分
               05  LNK-RECECOMCHK-NYUGAIKBN
                                           PIC X(01).
      *        診療コード
               05  LNK-RECECOMCHK-SRYCD    PIC X(09).
      *        診療年月
               05  LNK-RECECOMCHK-SRYYM    PIC X(06).
      *        チェック
               05  LNK-RECECOMCHK-TBL-IN.
                   07  LNK-RECECOMCHK-TBL-IN-O
                                           PIC X(01)   OCCURS 200.
      *    返却項目
           03  ORCSRECECOMCHK-OUT-AREA.
      *        チェック
               05  LNK-RECECOMCHK-TBL-OUT.
                   07  LNK-RECECOMCHK-TBL-OUT-O
                                           PIC X(01)   OCCURS  200.
      *
      *復帰コード １：入外区分エラー
           03  LNK-RECECOMCHK-RC           PIC S9(9)   BINARY.
      *
