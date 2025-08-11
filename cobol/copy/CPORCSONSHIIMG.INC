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
      *    資格確認サブ(画像取出し)
      ******************************************************************
       01  ORCSONSHIIMGAREA.
      *    入力項目
           03  ORCSONSHIIMG-IN-AREA.
      *        処理区分    01:画像取出し
      *                    02:画像削除
               05  LNK-ONSHIIMG-SYOKBN PIC X(02).
      *        画像区分    01:保険証OCR
      *                    02:公費OCR
               05  LNK-ONSHIIMG-IMGKBN
                                       PIC X(02).
      *        TBL_UUID(削除時必須)
               05  LNK-ONSHIIMG-TBL-UUID
                                       PIC X(36).
      *        ファイル名
               05  LNK-ONSHIIMG-FILE-NAME   
                                       PIC X(100).
      *    返却項目
           03  ORCSONSHIIMG-OUT-AREA.
      *        画像出力フォルダ
               05  LNK-ONSHIIMG-FILE-FOLDER
                                       PIC X(512).
      *        画像状態
      *        0:正常 1:画像なし 2:画像サイズオーバ
               05  LNK-ONSHIIMG-IMG-STATUS
                                       PIC X(01).
      *        画像状態メッセージ
               05  LNK-ONSHIIMG-IMG-MSG
                                       PIC X(100).
      *        TBL_UUID
               05  LNK-ONSHIIMG-OUT-TBL-UUID
                                       PIC X(36).
      *
      *復帰コード １：区分エラー
           03  LNK-ONSHIIMG-RC         PIC S9(9)   BINARY.
      *
