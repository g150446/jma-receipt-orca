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
      *    レセプト改正対応チェックパラメタ   COPY (CPORCSKAISEICHK.INC)
      *                                  使用プログラム (ORCSKAISEICHK)
      *    設定項目
      *      診療年月
      ************************************************************
      *
       01  ORCSKAISEICHKAREA.
      *
           03  LNK-KAISEICHK-IN.
      *        診療年月
               05  LNK-KAISEICHK-SRYYM PIC X(06).
      *        処理区分
               05  LNK-KAISEICHK-SYOKBN
                                       PIC X(1).
      *
           03  LNK-KAISEICHK-OT.
      *        改正対応メッセージ
               05  LNK-KAISEICHK-MSG   PIC X(140).
               05  LNK-KAISEICHK-STYLE PIC X(20).
      *        診療年月チェック区分　１：改正未対応
               05  LNK-KAISEICHK-KBN   PIC X(01).
      *
      *    復帰コード  １：診療年月エラー
           03  LNK-KAISEICHK-RC        PIC S9(9) BINARY.
