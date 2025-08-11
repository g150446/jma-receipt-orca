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
      *    特定入院料コード取得パラメタ　　COPY (CPORCSTOKNYUIN.INC)
      *                                使用プログラム (ORCSTOKNYUIN)
      *    設定項目
      *      特定入院料区分
      *      特定入院料
      *      算定年月
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-小豆沢  20/05/29  新型コロナウイルス感染症
      *                                   （診療報酬上臨時的取扱）対応の為、
      *                                     引数の算定年月を算定年月日に変更
      *                                     ※５月２６日から算定コード変更の対応
      ************************************************************
       01  ORCSTOKNYUIN-AREA.
      *
      *特定入院料区分（１：医療機関／２：病棟／３：病室）
           03  LNK-TOKNYUIN-KBN                       PIC  X(01).
      *特定入院料
           03  LNK-TOKNYUIN-NYUIN                     PIC  X(02).
      *算定年月日
           03  LNK-TOKNYUIN-SANTEIYMD                 PIC  X(08).
      *病棟番号
           03  LNK-TOKNYUIN-BTUNUM                    PIC  X(02).
      *病室番号
           03  LNK-TOKNYUIN-BRMNUM                    PIC  X(08).
      *特定入院料算定要件区分（空白：該当する／１：該当しない）
           03  LNK-TOKNYUIN-SANTEIYOUKENKBN           PIC  X(01).
      *返却項目
           03  LNK-TOKNYUIN-HENKYAKU-AREA.
      *特定入院料コード
               05  LNK-TOKNYUIN-SRYCD                 PIC  X(09).
      *特定入院料算定要件外コード
               05  LNK-TOKNYUIN-GAI-SRYCD             PIC  X(09).
      *新型コロナ判定用フラグ（置き換え対象となる特定入院料コードは１を返却）
               05  LNK-TOKNYUIN-COVID19-FLG           PIC  X(01).
      *急性血液浄化等判定用フラグ（置き換え対象となる特定入院料コードは１を返却）
               05  LNK-TOKNYUIN-BLOOD-FLG             PIC  X(01).
      *ＦＩＭ測定なし（置き換え対象となる特定入院料コードは１を返却）
               05  LNK-TOKNYUIN-FIM-FLG             PIC  X(01).
      *特定入院料算定要件外で療養病棟入院基本料Ｉを算定時に入院料加算
      *を一般病棟特別入院基本料の例により算定する場合の判定区分
      *（空白：通常の加算算定　１：一般病棟特別入院基本料の例で加算算定
      *　２：新型コロナ感染症２倍　３：新型コロナ感染症３倍
      *　４：新型コロナ感染症２倍上限延長　５：新型コロナ感染症３倍上限延長
      *　６：急性血液浄化等　７：ＦＩＭ測定なし）
               05  LNK-TOKNYUIN-GAI-NYUKSNKBN         PIC  X(01).
      *復帰コード（０：特定入院料返却あり　１：特定入院料返却なし
      *            ２：算定に必要なデータが不足）
           03  LNK-TOKNYUIN-RC                     PIC S9(9) BINARY.
      *
