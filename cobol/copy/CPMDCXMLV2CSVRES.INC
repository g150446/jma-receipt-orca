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
      *    ＡＰＩ　電子処方箋ＣＳＶ情報(xml2)   COPY (CPMDCXMLV2CSVRES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-MEDICALCSVRES.
            02  MDCCSV-CONTEXT             PIC S9(9)   BINARY.
            02  MDCCSV-OBJECT              PIC X(36).
            02  MDCCSV-MODE                PIC S9(9)   BINARY.
            02  MDCCSV-RECORDNAME          PIC X(128).
            02  MDCCSV-MEDICALRES.
      *       実施日
              03  MDCCSV-INFORMATION-DATE  PIC X(10).
      *       実施時間
              03  MDCCSV-INFORMATION-TIME  PIC X(8).
      *       返却コード
              03  MDCCSV-API-RESULT        PIC X(2).
      *       返却メッセージ
              03  MDCCSV-API-RESULT-MSG    PIC X(100).
      *
              03  MDCCSV-RESKEY            PIC X(100).
      *       診療日付
              03  MDCCSV-PERFORM-DATE      PIC X(10).
              03  MDCCSV-PERFORM-TIME      PIC X(8).
      *
              03  MDCCSV-APPOINT-DEP-CODE  PIC X(2).
              03  MDCCSV-APPOINT-DEP-NAME  PIC X(80).
              03  MDCCSV-APPOINT-DR-CODE   PIC X(5).
              03  MDCCSV-APPOINT-DR-NAME   PIC X(80).
              03  MDCCSV-PATIENT-INFOMATION.
                04  MDCCSV-PATIENTID       PIC X(20).
                04  MDCCSV-NAME            PIC X(100).
                04  MDCCSV-KANANAME        PIC X(100).
                04  MDCCSV-BIRTHDAY        PIC X(10).
                04  MDCCSV-SEX             PIC X(1).
      *         保険情報
                04  MDCCSV-INSURANCE-G.
      *             保険組合せ番号
                    06  MDCCSV-COMBINATION-NUMBER     PIC X(04).
      *             保険の種類
                    06  MDCCSV-MAIN-INSURANCE-CLASS   PIC X(03).
      *             保険者番号
                    06  MDCCSV-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             保険の種類名称
                    06  MDCCSV-MAIN-INSURANCE-NAME    PIC X(20).
      *             記号
                    06  MDCCSV-MAIN-MARK              PIC X(80).
      *             番号
                    06  MDCCSV-MAIN-NUMBER            PIC X(80).
      *             枝番
                    06  MDCCSV-MAIN-EDABAN            PIC X(02).
      *             継続区分
                    06  MDCCSV-MAIN-CONTINUATION      PIC X(01).
      *             補助区分
                    06  MDCCSV-MAIN-ASSISTANCE        PIC X(01).
      *             本人家族区分
                    06  MDCCSV-MAIN-FAMILY            PIC X(01).
      *             被保険者名
                    06  MDCCSV-MAIN-POLICYHOLDER      PIC X(100).
      *             有効開始日
                    06  MDCCSV-MAIN-START-DATE        PIC X(10).
      *             有効終了日
                    06  MDCCSV-MAIN-END-DATE          PIC X(10).
      *             公費情報
                    06  MDCCSV-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               公費の種類
                      07  MDCCSV-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      07  MDCCSV-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      07  MDCCSV-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      07  MDCCSV-RECIPIENT-NUMBER        PIC X(20).
      *               入院−負担率（割）
                      07  MDCCSV-RATE-ADMISSION          PIC X(04).
      *               入院−固定額
                      07  MDCCSV-MONEY-ADMISSION         PIC X(06).
      *               外来−負担率（割）
                      07  MDCCSV-RATE-OUTPATIENT         PIC X(04).
      **              外来−固定額
                      07  MDCCSV-MONEY-OUTPATIENT        PIC X(06).
      *               有効開始日
                      07  MDCCSV-INSURANCE-START-DATE    PIC X(10).
      *               有効終了日
                      07  MDCCSV-INSURANCE-END-DATE      PIC X(10).
      *       中途データメッセージ
              03  MDCCSV-MEDICAL-MSG1-G.
      *           エラーコード
                  05  MDCCSV-MEDI-RESULT        PIC X(4).
      *           エラーメッセージ
                  05  MDCCSV-MEDI-RESULT-MSG1   PIC X(100).
                  05  MDCCSV-MEDI-RESULT-MSG2   PIC X(100).
      *電子処方箋情報
              03  MDCCSV-EPRECSV-G.
                  05  MDCCSV-CSVDATA             PIC X(250000).
                  05  MDCCSV-CSVDATA2            PIC X(250000).
                  05  MDCCSV-CSVDATA3            PIC X(250000).
                  05  MDCCSV-CSVDATA4            PIC X(250000).
                  05  MDCCSV-CSVDATA5            PIC X(250000).
      *!!!
