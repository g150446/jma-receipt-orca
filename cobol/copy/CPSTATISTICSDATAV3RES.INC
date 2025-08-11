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
      ******************************************************************
      *    ＡＰＩ日次月次統計一覧取得 (xml2)
      *                             COPY (CPSTATISTICSDATAV3RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  XML-LSTRES.
           03  LSTRES-CONTEXT              PIC S9(9)   BINARY.
           03  LSTRES-OBJECT               PIC X(36).
           03  LSTRES-MODE                 PIC S9(9)   BINARY.
           03  LSTRES-RECORDNAME           PIC X(128).
           03  LSTRES-MEDICALRES.
      *        リクエスト番号
               05  LSTRES-REQUEST-NUMBER   PIC X(2).
      *        レスポンス番号
               05  LSTRES-RESPONSE-NUMBER  PIC X(2).
      *        UID（電子カルテ）
               05  LSTRES-KARTE-UID        PIC X(36).
      *        オルカUID
               05  LSTRES-ORCA-UID         PIC X(36).
      *        実施日
               05  LSTRES-INFORMATION-DATE PIC X(10).
      *        実施時間
               05  LSTRES-INFORMATION-TIME PIC X(8).
      *        返却コード
               05  LSTRES-API-RESULT       PIC X(3).
      *        返却メッセージ
               05  LSTRES-API-RESULT-MSG   PIC X(100).
      *
               05  LSTRES-RESKEY           PIC X(100).
      *        処理区分
               05  LSTRES-STATISTICS-MODE  PIC X(7).
      *        データ形式
               05  LSTRES-DATA-FORMAT      PIC X(10).
      *
               05  LSTRES-STATISTICS-INFO.
                   07  LSTRES-GROUP        PIC X(14).
                   07  LSTRES-RENNUM       PIC X(4).
                   07  LSTRES-SHORI-RENNUM PIC X(4).
                   07  LSTRES-CHARACTER-CODE
                                           PIC X(10).
                   07  LSTRES-TITLE        PIC X(100).
                   07  LSTRES-TO-DATA      PIC X(200).
      *        処理一覧
               05  LSTRES-STATISTICS-INF-OVER
                                           PIC X(4).
               05  LSTRES-STATISTICS-INFORMATION.
                   07  LSTRES-STATISTICS-INF   OCCURS  100.
                       09  LSTRES-TGROUP       PIC X(14).
                       09  LSTRES-TRENNUM      PIC X(4).
                       09  LSTRES-TSHORI-RENNUM
                                               PIC X(4).
                       09  LSTRES-TCHARACTER-CODE
                                               PIC X(10).
                       09  LSTRES-TTO-DATA     PIC X(200).
                       09  LSTRES-TKENSU       PIC X(5).
                       09  LSTRES-TSRYYM       PIC X(7).
                       09  LSTRES-TSRYYMD      PIC X(10).
                       09  LSTRES-TNYUGAIKBN   PIC X(1).
                       09  LSTRES-TTITLE       PIC X(100).
                       09  LSTRES-TCREYMD      PIC X(10).
                       09  LSTRES-TFLG         PIC X(4).
              05  LSTRES-OVERFLOW         PIC X(4).
              05  LSTRES-LENGTH-OVERFLOW  PIC X(4).
      *       件数
              05  LSTRES-OUTPUT-COUNT     PIC X(10).
      *
              05  LSTRES-DATA-ID-INFORMATION   OCCURS  10.
                  07  LSTRES-DATA-ID       PIC X(37).
                  07  LSTRES-PRINT-ID      PIC X(37).
