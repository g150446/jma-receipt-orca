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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ未請求設定 
      *                             COPY (CPUNCLAIMEDV3REQ.INC)
      ******************************************************************
        01  XML-UNCLAIMEDREQ.
            02  UNCLAIMEDQ-CONTEXT          PIC S9(9)   BINARY.
            02  UNCLAIMEDQ-OBJECT           PIC X(36).
            02  UNCLAIMEDQ-MODE             PIC S9(9)   BINARY.
            02  UNCLAIMEDQ-RECORDNAME       PIC X(128).
            02  UNCLAIMEDQ-REQ.
      *         リクエスト番号
                03  UNCLAIMEDQ-REQUEST-NUMBER
                                           PIC X(2).
      *         カルテUID
                03  UNCLAIMEDQ-KARTE-UID   PIC X(36).
      *         オルカUID
                03  UNCLAIMEDQ-ORCA-UID    PIC X(36).
      *         診療年月
                03  UNCLAIMEDQ-PERFORM-MONTH
                                           PIC X(7).
      *         入外区分
                03  UNCLAIMEDQ-INOUT       PIC X(1).
      *         未請求設定患者一覧
                03  UNCLAIMEDQ-UNCLAIMED-INF.
                    05  UNCLAIMEDQ-UNCLAIMED-OCC       OCCURS  100.
      *                 患者番号
                        07  UNCLAIMEDQ-UNCLAIMED-PTNUM PIC X(20).
      *         未請求設定解除患者一覧
                03  UNCLAIMEDQ-CLAIMED-INF.
                    05  UNCLAIMEDQ-CLAIMED-OCC         OCCURS  100.
      *                 患者番号
                        07  UNCLAIMEDQ-CLAIMED-PTNUM   PIC X(20).
      *                 診療年月
                        07  UNCLAIMEDQ-CLAIMED-MONTH   PIC X(7).
      *
