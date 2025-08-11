      *****************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      *****************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *****************************************************************
      *    ＡＰＩ照会番号一括登録用  COPY (CPONLINEREFALLREQ1.INC)    *
      *****************************************************************
        01  XML-ONLINEREFALLREQ1.
            02  REFALLREQ1-CONTEXT      PIC S9(9)   BINARY.
            02  REFALLREQ1-OBJECT       PIC X(36).
            02  REFALLREQ1-MODE         PIC S9(9)   BINARY.
            02  REFALLREQ1-RECORDNAME   PIC X(128).
            02  REFALLREQ1-DATAREQ.
      *         実施日
                03  REFALLREQ1-PERFORM-DATE PIC X(10).
      *         実施時間
                03  REFALLREQ1-PERFORM-TIME PIC X(8).
      *         最終照会番号(受け取った最後の照会番号(ptid))
                03  REFALLREQ1-PTID         PIC X(10).
