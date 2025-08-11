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
      *    ＡＰＩ資格確認予約患者一括用 COPY (CPONLINEQUAAPPREQ1.INC) *
      *****************************************************************
       01  XML-ONLINEQUAAPPREQ1.
           02  QUAAPPREQ1-CONTEXT    PIC S9(9)   BINARY.
           02  QUAAPPREQ1-OBJECT     PIC X(36).
           02  QUAAPPREQ1-MODE       PIC S9(9)   BINARY.
           02  QUAAPPREQ1-RECORDNAME PIC X(128).
           02  QUAAPPREQ1-DATAREQ.
      *        処理区分
               03  QUAAPPREQ1-QUALIFICATION-MODE
                                         PIC X(2).
      *        処理日
               03  PUSHQUA-INFORMATION-DATE    PIC X(10).
      *        処理時間
               03  PUSHQUA-INFORMATION-TIME    PIC X(8).
      *        資格確認日（予約日）
               03  QUAAPPREQ1-DATE       PIC X(10).
      *        UUID
               03  QUAAPPREQ1-UUID       PIC X(36).
      *        受付番号
               03  QUAAPPREQ1-UKENO      PIC X(38).
      *        任意のファイル識別子(医療機関固有項目)
               03  QUAAPPREQ1-SIKIBETU   PIC X(50).
      *        最終予約番号(受け取った最後の予約カウント)
               03  QUAAPPREQ1-LAST-YYKNO PIC X(10).
      *        プロセスクラス（訪問同意等の識別)
               03  QUAAPPREQ1-PROCESS-CLASS    PIC X(5).
