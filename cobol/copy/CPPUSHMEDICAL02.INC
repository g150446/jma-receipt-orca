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
       01  PUSHMD02-REC.
           03  PUSHMD02-EVENT                  PIC X(50).
           03  PUSHMD02-BODY.
      *      リクエスト番号
             05  PUSHMD02-REQUEST-NUMBER       PIC X(02).
      *      入外区分（1:入院、2:外来）
             05  PUSHMD02-NYUGAIKBN            PIC X(01).
      *      患者番号
             05  PUSHMD02-PTNUM                PIC X(20).
      *      システム日付
             05  PUSHMD02-SYSYMD               PIC X(10).
      *      システム時間
             05  PUSHMD02-SYSTIME              PIC X(08).
      *      診療日付
             05  PUSHMD02-SRYYMD               PIC X(10).
      *      診療時間
             05  PUSHMD02-SRYTIME              PIC X(08).
      *      保険組合せ
             05  PUSHMD02-HKNCOMBI             PIC X(04).
      *      診療科
             05  PUSHMD02-SRYKA                PIC X(02).
      *      ドクターコード
             05  PUSHMD02-DRCD                 PIC X(05).
