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
      ************************************************************
      *    診療行為コードチェックサブ
      *      パラメタ      COPY (CPORCSSRYCDCHK.INC)
      *      プログラム         (ORCSSRYCDCHK.CBL)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  LNK-ORCSSRYCDCHK-REC.
      *
      *医療機関番号
           03  LNK-ORCSSRYCDCHK-HOSPNUM        PIC 9(02).
      *入外区分
           03  LNK-ORCSSRYCDCHK-NYUGAIKBN      PIC X(01).
      *患者ＩＤ
           03  LNK-ORCSSRYCDCHK-PTID           PIC 9(10).
      *算定年月
           03  LNK-ORCSSRYCDCHK-SANTEIYM       PIC X(06).
      *保険組み合わせ番号
           03  LNK-ORCSSRYCDCHK-HKNCOMBIINF.
               05  LNK-ORCSSRYCDCHK-HKNCOMBI   PIC 9(04)  OCCURS 10.
      *診療行為コード
           03  LNK-ORCSSRYCDCHK-SRYCD          PIC X(09).
      *
      *返却値
           03  LNK-ORCSSRYCDCHK-OUT-AREA.
      *診療行為コード該当フラグ（１：該当）
               05  LNK-ORCSSRYCDCHK-GAITOFLG   PIC 9(01).
      *
      *復帰コード
      *        １：入外区分エラー
      *        ２：患者ＩＤエラー
      *        ３：算定年月エラー
      *        ４：保険組み合わせ番号エラー
      *        ５：診療行為コードエラー
           03  LNK-ORCSSRYCDCHK-RC             PIC 9(02).
      *
