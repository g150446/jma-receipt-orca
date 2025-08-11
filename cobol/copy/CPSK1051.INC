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
      *         システム管理マスタ     (CPSK1051.INC)            *
      *         （１０５１）                                     *
      *         オンライン資格確認照会情報                       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1051-REC.
           03  SYS-1051-KEY.
               05  SYS-1051-HOSPNUM        PIC 9(2).
      *管理コード
               05  SYS-1051-KANRICD        PIC  X(04).
      *区分コード
               05  SYS-1051-KBNCD          PIC  X(08).
      *有効開始年月日
               05  SYS-1051-STYUKYMD       PIC  X(08).
      *有効終了年月日
               05  SYS-1051-EDYUKYMD       PIC  X(08).
      *管理テーブル
           03  SYS-1051-TBL.
      *オンライン資格確認処理区分
               05  SYS-1051-ONSHIKAKU      PIC  X(01).
      *資格確認情報通知時間(999 通知なし）
               05  SYS-1051-POPUP-TIMEOUT  PIC  9(03).
      *
      *薬剤情報PDF区分
               05  SYS-1051-ONSHIYAKUZAI   PIC  X(01).
      *特定検診情報PDF区分
               05  SYS-1051-ONSHIKENSIN    PIC  X(01).
      *R04.1
      *診療行為資格確認有無区分
               05  SYS-1051-ONSHISINRYO    PIC  X(01).
      *
      *薬剤・診療情報PDF区分
               05  SYS-1051-ONSHIYKZSINRYO PIC  X(01).
      *
      *電子処方箋区分
               05  SYS-1051-DENSHISHOHO    PIC  X(01).
      *資格確認状態通知時間(999 通知なし）
               05  SYS-1051-POPUP-COND-TIMEOUT
                                           PIC  9(03).
      *R05/10
      *用法置換え区分
               05  SYS-1051-YOHOCHGKBN     PIC  X(01).
      *電子処方箋（紙）標準用法名称
               05  SYS-1051-YOUHOUNAME     PIC  X(01).
      *ＣＳＶディレクトリ
               05  SYS-1051-CSVFILEDIR     PIC  X(200).
      *R06/03
      *訪問診療
               05  SYS-1051-HOUMON         PIC  X(01).
      *オンライン診療
               05  SYS-1051-ONLINE         PIC  X(01).
      *医療扶助
               05  SYS-1051-FUJYO          PIC  X(01).
      *交付番号の転記
               05  SYS-1051-KOFUTNK        PIC  X(01).
      *2024/10
      *診療行為資格通知時間(999 通知なし）
               05  SYS-1051-POPUP-KGMN-TIMEOUT
                                        PIC  X(03).
               05  SYS-1051-POPUP-KGMN-TIMEOUT-9
                                        REDEFINES
                   SYS-1051-POPUP-KGMN-TIMEOUT
                                        PIC  9(03).
      *2025/05
      *医療機関コード
               05  SYS-1051-HOSPCDF.
      *県番号
                   07  SYS-1051-PREFNUM     PIC  X(02).
      *点数表区分
                   07  SYS-1051-TENHYOKBN   PIC  X(01).
      *医療機関コード
                   07  SYS-1051-HOSPCD      PIC  X(07).
      *予備
      **** 03  SYS-1051-YOBI            PIC  X(492).
      **** 03  SYS-1051-YOBI            PIC  X(488).
      ***  03  SYS-1051-YOBI            PIC  X(286).
      ***  03  SYS-1051-YOBI            PIC  X(282).
      ***  03  SYS-1051-YOBI            PIC  X(279).
           03  SYS-1051-YOBI            PIC  X(269).
      *端末ＩＤ
           03  SYS-1051-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1051-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1051-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1051-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1051-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1051-UPD-KEY.
               05  SYS-1051-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1051-UPD-KANRICD     PIC X(04).
               05  SYS-1051-UPD-KBNCD       PIC X(08).
               05  SYS-1051-UPD-STYUKYMD    PIC X(08).
               05  SYS-1051-UPD-EDYUKYMD    PIC X(08).
