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
      *    ＡＰＩレスポンス患者状態コメント情報取得
      *                                 COPY (CPAPISYS1018V2RES.INC)
      ******************************************************************
        01  XML-SYSTEM1018RES.
            02  SYS1018RES-CONTEXT            PIC S9(9)   BINARY.
            02  SYS1018RES-OBJECT             PIC X(36).
            02  SYS1018RES-MODE               PIC S9(9)   BINARY.
            02  SYS1018RES-RECORDNAME         PIC X(128).
            02  SYS1018RES-MEDICALRES.
      *       実施日
              03  SYS1018RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  SYS1018RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  SYS1018RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  SYS1018RES-API-RESULT-MSG      PIC X(100).
      *
              03  SYS1018RES-RESKEY              PIC X(100).
      *       基準日
              03  SYS1018RES-BASE-DATE           PIC X(10).
      *       患者状態コメント情報
              03  SYS1018RES-CONDITION-INF      OCCURS   3.
                  05  SYS1018RES-CONDITION-OCC      OCCURS   100.
      *               患者状態コメント区分
                      07  SYS1018RES-CONDITION         PIC X(02).
      *               状態内容
                      07  SYS1018RES-CONDITION-NAME    PIC X(40).
      *               比喩単語
                      07  SYS1018RES-CONDITION-WORD    PIC X(04).
      *               アイコンファイル名
                      07  SYS1018RES-CONDITION-ICON    PIC X(100).
      *
