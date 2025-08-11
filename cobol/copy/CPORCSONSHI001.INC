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
      *    資格確認サブ
      ******************************************************************
       01  ORCSONSHI001AREA.
      *    入力項目
           03  ORCSONSHI001-IN-AREA.
      *        処理区分    01:資格確認情報返却
      *                    02:資格確認要否依頼
      *                    03:資格確認情報返却
      *                    04:患者番号変更時強制資格確認
      *                    05:保険証ＯＣＲ再照会
      *R06.12              06:訪問診療同意一括照会
      *R06.12              07:オンライン診療同意一括照会
               05  LNK-ONSHI001-SYOKBN PIC X(02).
      *        業務コード  11:受付
      *                    12:登録
      *                    21:診療行為
      *                    99:その他
               05  LNK-ONSHI001-GYOUMUCD
                                       PIC X(02).
      *        TBL_UUID
               05  LNK-ONSHI001-TBL-UUID
                                       PIC X(36).
      *        患者ID
               05  LNK-ONSHI001-PTID   PIC 9(10).
      *R04.1
      *        診療行為資格確認有無
      *            0:無し、1:有り
               05  LNK-ONSHI001-ONSHISINRYO  PIC X(01).
      *R04.9
      *    保険証ＯＣＲ再照会
      *        保険者番号
               05  LNK-ONSHI001-OCR-HKNJANUM     PIC X(08).
      *        記号
               05  LNK-ONSHI001-OCR-KIGO         PIC X(80).
      *        番号
               05  LNK-ONSHI001-OCR-NUM          PIC X(80).
      *        枝番
               05  LNK-ONSHI001-OCR-EDABAN       PIC X(02).
      *        生年月日
               05  LNK-ONSHI001-OCR-BIRTHDAY     PIC X(09).
      *R06.02
      *    オン資格診療区分
               05  LNK-ONSHI001-ONSHINRYO-KBN    PIC X(01).
      *R06.12
      *    一括照会
      *        開始年月日
               05  LNK-ONSHI001-STARTYMD         PIC X(08).
      *        終了年月日
               05  LNK-ONSHI001-ENDYMD           PIC X(08).
      *
      *    返却項目
           03  ORCSONSHI001-OUT-AREA.
      *        資格確認要否
               05  LNK-ONSHI001-KAKUNIN-FLG
                                       PIC X(03).
      *        資格状態
               05  LNK-ONSHI001-SIKAKU-YUKO
                                       PIC X(01).
      *        資格状態内容
               05  LNK-ONSHI001-SIKAKU-YUKO-NM
                                       PIC X(100).
      *        UID
               05  LNK-ONSHI001-UID    PIC X(300).
      *        保険変更情報
               05  LNK-ONSHI001-HKN-FLG
                                       PIC X(03).
      *        資格確認情報(状態:喪失 etc)(受付、患者登録表示、登録用情報)
               05  LNK-ONSHI001-SHIKAKU
                                       PIC X(01). 
      *        マイナンバー情報
               05  LNK-ONSHI001-MYNUMBER
                                       PIC X(01).                                 
      *        枝番
               05  LNK-ONSHI001-EDABAN PIC X(02).                                 
      *        TBL_UUID
               05  LNK-ONSHI001-OUT-TBL-UUID
                                       PIC X(36).
      *
      *復帰コード １：区分エラー
           03  LNK-ONSHI001-RC         PIC S9(9)   BINARY.
      *
