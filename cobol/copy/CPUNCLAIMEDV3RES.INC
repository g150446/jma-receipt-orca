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
      *                                   COPY (CPUNCLAIMEDV3RES.INC)
      ******************************************************************
       01  XML-UNCLAIMEDRES.
           02  UNCLAIMEDS-CONTEXT          PIC S9(9)   BINARY.
           02  UNCLAIMEDS-OBJECT           PIC X(36).
           02  UNCLAIMEDS-MODE             PIC S9(9)   BINARY.
           02  UNCLAIMEDS-RECORDNAME       PIC X(128).
           02  UNCLAIMEDS-RES.
      *        リクエスト番号
               03  UNCLAIMEDS-REQUEST-NUMBER   PIC X(2).
      *
               03  UNCLAIMEDS-RESPONSE-NUMBER  PIC X(2).
      *        カルテUID
               03  UNCLAIMEDS-KARTE-UID    PIC X(36).
      *        オルカUID
               03  UNCLAIMEDS-ORCA-UID     PIC X(36).
      *        実施日
               03  UNCLAIMEDS-INFORMATION-DATE
                                           PIC X(10).
      *        実施時間
               03  UNCLAIMEDS-INFORMATION-TIME
                                           PIC X(8).
      *        返却コード
               03  UNCLAIMEDS-API-RESULT   PIC X(3).
      *        返却メッセージ
               03  UNCLAIMEDS-API-RESULT-MSG
                                           PIC X(100).
               03  UNCLAIMEDS-API-WARNING  PIC X(3).
               03  UNCLAIMEDS-API-WARNING-MSG
                                           PIC X(100).
               03  UNCLAIMEDS-RESKEY       PIC X(100).
               03  UNCLAIMEDS-PERFORM-MONTH
                                           PIC X(7).
               03  UNCLAIMEDS-INOUT        PIC X(1).
      *
               03  UNCLAIMEDS-UNCLAIMED-INF.
                   05  UNCLAIMEDS-UNCLAIMED-OVERFLOW   PIC X(04).
                   05  UNCLAIMEDS-UNCLAIMED-OCC        OCCURS  500.
                       07  UNCLAIMEDS-UNCLAIMED-PTNUM  PIC X(20).
                       07  UNCLAIMEDS-UNCLAIMED-NAME   PIC X(30).
                       07  UNCLAIMEDS-UNCLAIMED-MONTH  PIC X(7).
                       07  UNCLAIMEDS-UNCLAIMED-ERRCD  PIC X(3).
                       07  UNCLAIMEDS-UNCLAIMED-ERRMSG PIC X(100).
                       07  UNCLAIMEDS-UNCLAIMED-WARNING
                                                      PIC X(3).
                       07  UNCLAIMEDS-UNCLAIMED-WARNINGMSG
                                                       PIC X(100).
                       07  UNCLAIMEDS-UNCLAIMED-RECE-INF   OCCURS  10.
                           09  UNCLAIMEDS-UNCLAIMED-TEISYUTU
                                                       PIC X(04).
      *                    種別コード
                           09  UNCLAIMEDS-UNCLAIMED-CODE
                                                       PIC X(10).
      *                    保険者番号
                           09  UNCLAIMEDS-UNCLAIMED-HKNJANUM
                                                       PIC X(08).
      *                    補助区分
                           09  UNCLAIMEDS-UNCLAIMED-HOJOKBN
                                                       PIC X(12).
                       07  UNCLAIMEDS-UNCLAIMED-CHECK  PIC X(4).
      *
               03  UNCLAIMEDS-CLAIMED-INF.
                   05  UNCLAIMEDS-CLAIMED-OCC          OCCURS  500.
                       07  UNCLAIMEDS-CLAIMED-PTNUM    PIC X(20).
                       07  UNCLAIMEDS-CLAIMED-NAME     PIC X(30).
                       07  UNCLAIMEDS-CLAIMED-MONTH    PIC X(7).
                       07  UNCLAIMEDS-CLAIMED-ERRCD    PIC X(3).
                       07  UNCLAIMEDS-CLAIMED-ERRMSG   PIC X(100).
                       07  UNCLAIMEDS-CLAIMED-RECE-INF     OCCURS  10.
                           09  UNCLAIMEDS-CLAIMED-TEISYUTU
                                                       PIC X(04).
      *                    種別コード
                           09  UNCLAIMEDS-CLAIMED-CODE PIC X(10).
      *                    保険者番号
                           09  UNCLAIMEDS-CLAIMED-HKNJANUM
                                                       PIC X(08).
      *                    補助区分
                           09  UNCLAIMEDS-CLAIMED-HOJOKBN
                                                       PIC X(12).
                       07  UNCLAIMEDS-CLAIMED-CHECK    PIC X(4).
