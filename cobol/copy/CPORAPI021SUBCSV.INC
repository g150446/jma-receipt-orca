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
      *    ＡＰＩ中途終了データ編集サブパラメタ（一体化）
      *                                         (CPORAPI021SUBCSV.INC)
      ******************************************************************
       01  ORAPI021SUBCSVAREA.
      *    処理区分
           03  ORAPICSV-SYORIKBN           PIC X(03).
      *    患者番号
           03  ORAPICSV-PTNUM              PIC X(20).
           03  ORAPICSV-PTID               PIC 9(10).
      *    保険組合せ番号
           03  ORAPICSV-HKNCOMBI           PIC X(04).
      *    診療日付
           03  ORAPICSV-SRYYMD             PIC X(08).
      *    診療科
           03  ORAPICSV-SRYKA              PIC X(02).
           03  ORAPICSV-DRCD               PIC X(05).
      *    出力情報
           03  ORAPICSV-OUT-AREA.
               05  ORAPICSV-FILENAME       PIC X(1024).
      *
      *    電子処方箋ＣＳＶデータ
           03  ORAPICSV-CSVDATA-AREA.
               05  ORAPICSV-CSVDATA-OCC       OCCURS   5.
                   07  ORAPICSV-CSVDATA-OCC2      OCCURS   500.
                       09  ORAPICSV-CSVDATA       PIC X(500).
                   07  ORAPICSV-CSVMAX            PIC 9(04).
               05  ORAPICSV-DATAMAX               PIC 9(02).
      *
      *    エラー区分
           03  ORAPICSV-ERRCD        PIC X(02).
           03  ORAPICSV-ERRMSG       PIC X(100).
      *    コード毎のエラー
           03  ORAPICSV-MD-ERR-G.
               05  ORAPICSV-MD-ERR-OCC       OCCURS  100.
                   07  ORAPICSV-MD-ERRCD         PIC X(04).
                   07  ORAPICSV-MD-ERRMSG.
                       09  ORAPICSV-MD-ERRMSG1    PIC X(100).
                       09  ORAPICSV-MD-ERRMSG2    PIC X(100).
      *            エラー剤番
                   07  ORAPICSV-MD-POS       PIC 9(03).
      *            エラー行番
                   07  ORAPICSV-MD-LINE      PIC 9(02).
      *            エラー診療コード
                   07  ORAPICSV-MD-SRYCD     PIC X(09).
      *            エラー診療名称
                   07  ORAPICSV-MD-SRYNAME   PIC X(100).
      *
