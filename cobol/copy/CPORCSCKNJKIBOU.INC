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
      *    長期収載品の患者希望編集パラメタ　　COPY (CPORCSCKNJKIBOU.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCKNJKIBOUAREA.
      *
      *入力
      *処理区分
           03  ORCSCKIBOU-SYORIKBN        PIC  X(01).
      *処理区分２（1:全角変換あり)
           03  ORCSCKIBOU-SYORIKBN2       PIC  X(01).
      *診療日付
           03  ORCSCKIBOU-SRYYMD          PIC  X(08).
      *診療会計情報
           03  ORCSCKIBOU-SRYACT-G.
      *剤回数
               05  ORCSCKIBOU-ZAIKAISU        PIC  9(03).
      *特別料金　剤点数
               05  ORCSCKIBOU-CYOKISENTEITEN  PIC  9(07).
      *
           03  ORCSCKIBOU-OUT-AREA.
      *特別料金
               05  ORCSCKIBOU-TOKMONEY         PIC 9(07).
      *消費税（再掲）
               05  ORCSCKIBOU-TOKMONEY-TAX     PIC 9(07).
      *明細内容（特別料金全角編集）
      *        　　　　　　Ｎ円
               05  ORCSCKIBOU-TOKMONEY-MEI     PIC X(20).
      *        Ｎ円
               05  ORCSCKIBOU-TOKMONEY-MEI2    PIC X(20).
      *        内消費税　Ｎ円
               05  ORCSCKIBOU-TOKMONEY-TAXMEI  PIC X(30).
      *
