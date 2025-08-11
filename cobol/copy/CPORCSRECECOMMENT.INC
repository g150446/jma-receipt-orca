      ******************************************************************
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
      ******************************************************************
      *    レセプト自動記載コメント編集
      ******************************************************************
       01  ORCSRECECOMMENTAREA.
      *    入力項目
           03  ORCSRECECOMMENT-IN-AREA.
      *        処理区分    １：コメントパターン５０（年月日）
      *                    ２：コメントパターン５０（年月）
      *                    ３：コメントパターン３０
      *                    ４：コメントパターン４２
      *                    ５：コメントパターン２０
               05  LNK-RECECOMMENT-KBN     PIC X(01).
      *        診療コード
               05  LNK-RECECOMMENT-SRYCD   PIC X(09).
      *        日付
               05  LNK-RECECOMMENT-YMD     PIC X(08).
      *        コメント
               05  LNK-RECECOMMENT-COMMENT PIC X(100).
      *        数値
               05  LNK-RECECOMMENT-NUM     PIC S9(05)V9(06).
      *        基準日
               05  LNK-RECECOMMENT-SRYYMD  PIC X(08).
      *    返却項目
           03  ORCSRECECOMMENT-OUT-AREA.
      *        名称
               05  LNK-RECECOMMENT-NAME    PIC X(300).
      *        名称文字数
               05  LNK-RECECOMMENT-YUKOKETA
                                           PIC 9(03).
      *        入力コメント
               05  LNK-RECECOMMENT-INPUTCOMMENT
                                           PIC X(140).
      *
      *復帰コード １：区分エラー
      *           ２：診療コードエラー
      *           ３：年月日エラー
      *           ４：コメントエラー
      *           ５：数値エラー
      *           ６：基準日エラー
      *           ７：診療コード存在エラー
      *           ８：返却名称エラー
      *           ９：
           03  LNK-RECECOMMENT-RC          PIC S9(9)   BINARY.
      *
