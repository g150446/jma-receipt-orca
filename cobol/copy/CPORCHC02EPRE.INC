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
      *----------------------------------------------------------*
      *    電子処方箋ＣＳＶ出力パラメタ          (CPORCHC02EPRE.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHC02EPREAREA.
      *    区分:ONL、API
           03  ORCHCEPRE-SYORIKBN              PIC X(03).
      *2025/04 CHK:チェック用
           03  ORCHCEPRE-CHKKBN                PIC X(03).
      *    処方箋情報
           03  ORCHCEPRE-PRESCRIPTION-INFO.
               05  ORCHCEPRE-PRESCRIPTION-OCC    OCCURS  5.
      *            処方箋発行形態
                   07  ORCHCEPRE-PRE-ISSUESELECT   PIC X(1).
      *            処方箋ID
                   07  ORCHCEPRE-PRE-ID            PIC X(36).
      *            引換番号
                   07  ORCHCEPRE-ACCESS-CODE       PIC X(16).
      *            リフィル回数
                   07  ORCHCEPRE-REFILL-NUM        PIC X(1).
      *            リフィル処方日数
                   07  ORCHCEPRE-REFILL-KAISU      PIC 9(3).
      *            処方箋コメント情報
                   07  ORCHCEPRE-COMMENT-G.
                       09   ORCHCEPRE-COMMENT-OCC   OCCURS  10.
                            11   ORCHCEPRE-PRE-NAME     PIC X(180).
                            11   ORCHCEPRE-PRE-COMMENT  PIC X(300).
      *
               05  ORCHCEPRE-REF-MAX       PIC 9(02).
      *
      *
           03  ORCHCEPRE-OT-CSVFILE            PIC X(1024).
      *
      *    APIの時ＣＳＶデータを返却
           03  ORCHCEPRE-CSVDATA-AREA.
               05  ORCHCEPRE-CSVDATA-OCC       OCCURS   5.
                   07  ORCHCEPRE-CSVDATA-OCC      OCCURS   500.
                       09  ORCHCEPRE-CSVDATA       PIC X(500).
                   07  ORCHCEPRE-CSVMAX            PIC 9(04).
               05  ORCHCEPRE-DATAMAX               PIC 9(02).
      *
           03  ORCHCEPRE-ERRCD                 PIC X(02).
           03  ORCHCEPRE-ERRMSG                PIC X(200).
      
