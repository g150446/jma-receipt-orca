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
      *    ＡＰＩレスポンス　旧姓履歴情報情報取得(xml2)
      *                                   COPY (CPPATIENTLST8V2RES.INC)
      ******************************************************************
        01  XML-PATIENTLST8RES.
            02  PTRES-CONTEXT          PIC S9(9)   BINARY.
            02  PTRES-OBJECT           PIC X(36).
            02  PTRES-MODE             PIC S9(9)   BINARY.
            02  PTRES-RECORDNAME       PIC X(128).
            02  PTRES-PATIENTINFORES.
      *       実施日
              03  PTRES-INFORMATION-DATE   PIC X(10).
      *       実施時間
              03  PTRES-INFORMATION-TIME   PIC X(8).
      *       返却コード
              03  PTRES-API-RESULT         PIC X(3).
      *       返却メッセージ
              03  PTRES-API-RESULT-MSG     PIC X(100).
              03  PTRES-RESKEY             PIC X(100).
      *
              03  PTRES-PATIENT-INFOMATION.
      *           患者番号
                  05  PTRES-PATIENTID          PIC X(20).
      *           漢字氏名
                  05  PTRES-NAME               PIC X(100).
      *           カナ氏名
                  05  PTRES-KANANAME           PIC X(100).
      *           通称
                  05  PTRES-NICKNAME           PIC X(100).
      *           生年月日
                  05  PTRES-BIRTHDAY           PIC X(10).
      *           性別
                  05  PTRES-SEX                PIC X(1).
      *        旧姓履歴情報
               03  PTRES-KYUSEI-INFOMATION.
                   05  PTRES-KYUSEI-INFO     OCCURS  20 TIMES.
      *                変更年月日
                       07  PTRES-KYUSEI-CHGYMD         PIC  X(10).
      *                漢字氏名
                       07  PTRES-KYUSEI-NAME           PIC  X(100).
      *                カナ氏名
                       07  PTRES-KYUSEI-KANANAME       PIC  X(100).
      *                通称
                       07  PTRES-KYUSEI-NICKNAME       PIC  X(100).
      *
