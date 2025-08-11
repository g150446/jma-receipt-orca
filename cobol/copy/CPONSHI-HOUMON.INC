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
      ****************************************************************
      *    オンライン資格確認訪問診療テーブル                        *
      *                                COPY  (CPONSHI-HOUMON.INC)    *
       ***************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ****************************************************************
      *01  ONSHI-HOUMON-REC.
      *キー
           03  ONS-HOUMON-KEY.
      *医療機関ＩＤ
               05  ONS-HOUMON-HOSPNUM          PIC 9(02).
      *ＵＩＤ
               05  ONS-HOUMON-TBL-UUID         PIC X(36).
      *患者ＩＤ
           03  ONS-HOUMON-PTID                 PIC 9(10).
      *診療区分(2:訪問診療 3:オンライン診療)
           03  ONS-HOUMON-ONSHINRYO-KBN        PIC X(01).
      *照会区分(1:顔認証 2:保険証)
           03  ONS-HOUMON-SHOKAI-KBN           PIC X(01).
      *保険者番号
           03  ONS-HOUMON-HKNJANUM             PIC X(08).
      *記号
           03  ONS-HOUMON-KIGO                 PIC X(80).
      *番号
           03  ONS-HOUMON-NUM                  PIC X(80).
      *番号枝番
           03  ONS-HOUMON-EDABAN               PIC X(02).
      *負担者番号
           03  ONS-HOUMON-FTNJANUM             PIC X(08).
      *受給者番号
           03  ONS-HOUMON-JKYSNUM              PIC X(07).
      *同意年月日（自）
           03  ONS-HOUMON-DOUI-STARTYMD        PIC X(08).
      *同意年月日（至）
           03  ONS-HOUMON-DOUI-ENDYMD          PIC X(08).
      *同意取消(1:取消)
           03  ONS-HOUMON-DOUI-TORIKESI        PIC X(01).
      *同意取消日
           03  ONS-HOUMON-DOUI-TORIKESI-YMD    PIC X(08).
      *特定健診同意フラグ
           03  ONS-HOUMON-KENSHIN-DOUIFLG      PIC X(01).
      *薬剤同意フラグ
           03  ONS-HOUMON-YAKUZAI-DOUIFLG      PIC X(01).
      *診療情報同意フラグ
           03  ONS-HOUMON-SHINRYO-DOUIFLG      PIC X(01).
      *手術情報同意フラグ
           03  ONS-HOUMON-OPE-DOUIFLG          PIC X(01).
      *限度額適用認定証取得フラグ
           03  ONS-HOUMON-GENDO-SHUTOKUFLG     PIC X(01).
      *特定疾病療養受療証取得フラグ
           03  ONS-HOUMON-SIKKAN-SHUTOKUFLG    PIC X(01).
      *確認依頼日
           03  ONS-HOUMON-KAKU-KAKUNINYMD      PIC X(08).
      *資格確認依頼時間
           03  ONS-HOUMON-KAKU-KAKUNINHMS      PIC X(06).
      *資格確認返却済みフラグ
           03  ONS-HOUMON-KAKU-KAKUNINFLG      PIC X(01).
      *施設ID
           03  ONS-HOUMON-SHISETU-ID           PIC X(02).
      *ターゲットUID
           03  ONS-HOUMON-TARGET-UUID          PIC X(36).
      *端末ＩＤ
           03  ONS-HOUMON-TERMID               PIC X(64).
      *オペーレータＩＤ
           03  ONS-HOUMON-OPID                 PIC X(16).
      *作成年月日
           03  ONS-HOUMON-CREYMD               PIC X(08).
      *作成時間
           03  ONS-HOUMON-CREHMS               PIC X(06).
      *更新年月日
           03  ONS-HOUMON-UPYMD                PIC X(08).
      *更新時間
           03  ONS-HOUMON-UPHMS                PIC X(06).
