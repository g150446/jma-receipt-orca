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
      *    ＡＰＩレスポンス診療内容情報取得
      *                                 COPY (CPAPISYS1012V2RES.INC)
      ******************************************************************
        01  XML-SYSTEM1012RES.
            02  SYS1012RES-CONTEXT            PIC S9(9)   BINARY.
            02  SYS1012RES-OBJECT             PIC X(36).
            02  SYS1012RES-MODE               PIC S9(9)   BINARY.
            02  SYS1012RES-RECORDNAME         PIC X(128).
            02  SYS1012RES-MEDICALRES.
      *       実施日
              03  SYS1012RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  SYS1012RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  SYS1012RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  SYS1012RES-API-RESULT-MSG      PIC X(100).
      *
              03  SYS1012RES-RESKEY              PIC X(100).
      *       基準日
              03  SYS1012RES-BASE-DATE           PIC X(10).
      *       診療内容情報
              03  SYS1012RES-MEDICAL-INFOMATION.
                  05  SYS1012RES-MEDICAL-INF-OCC  OCCURS   100.
      *               診療内容区分
                      07  SYS1012RES-SRYNAIYO-CODE    PIC X(02).
      *               診療内容名称
                      07  SYS1012RES-SRYNAIYO-NAME    PIC X(40).
      *               診療内容表示名
                      07  SYS1012RES-SRYNAIYODSP      PIC X(24).
      *
