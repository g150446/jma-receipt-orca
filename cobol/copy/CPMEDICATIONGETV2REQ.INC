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
      ******************************************************************
      *    ＡＰＩリクエスト点数マスタ情報取得
      *                                    COPY (CPMEDICATIONGETV2REQ.INC)
      ******************************************************************
        01  XML-MEDICATIONGETREQ.
            03  MEDIREQ-CONTEXT            PIC S9(9)   BINARY.
            03  MEDIREQ-OBJECT             PIC X(36).
            03  MEDIREQ-MODE               PIC S9(9)   BINARY.
            03  MEDIREQ-RECORDNAME         PIC X(128).
            03  MEDIREQ-MEDICATIONGET.
      *       リクエスト番号（01:セットＣＤ検索、02:診療コード検索、03:選択式コメント検索）
              05  MEDIREQ-REQUEST-NUMBER       PIC X(02).
      *       リクエストコード
              05  MEDIREQ-REQUEST-CODE         PIC X(20).
      *       基準日付
              05  MEDIREQ-BASE-DATE            PIC X(10).
