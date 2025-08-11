      *****************************************************************
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
      *****************************************************************
      *    ＡＰＩ資格確認用       COPY (CPONLINEQUAREQ3.INC)          *
      *****************************************************************
       01  XML-ONLINEQUAREQ3.
           03  QUAREQ3-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ3-OBJECT              PIC X(36).
           03  QUAREQ3-MODE                PIC S9(9)   BINARY.
           03  QUAREQ3-RECORDNAME          PIC X(128).
           03  QUAREQ3-PATIENTREQ.
      *メッセージヘッダー部     
               05  QUAREQ3-HEADER.
      *処理実行日時
                   07  QUAREQ3-SHORI-TIME  PIC X(14).
      *資格確認日
                   07  QUAREQ3-KAKUNINYMD  PIC X(08).
      *医療機関コード
                   07  QUAREQ3-HOSPCD      PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   07  QUAREQ3-FILE-SIKIBETU PIC X(50).
      *照会区分
                   07  QUAREQ3-SHOKAI-KBN  PIC X(01).
      *処理結果区分
                   07  QUAREQ3-RESULT-KBN  PIC X(01).
      *エラーコード
                   07  QUAREQ3-ERR-CODE    PIC X(09).
      *エラーメッセージ
                   07  QUAREQ3-ERR-MSG     PIC X(60).
      *文字コード識別
                   07  QUAREQ3-CODE-TYPE   PIC X(01).
      *メッセージボディ部    
               05  QUAREQ3-MESSAGEBODY.
      *照会内容
                   07  QUAREQ3-SHOKAI-AREA.
      *保険者番号
                       09  QUAREQ3-SHO-HKNJANUM    PIC X(08).
      *記号
                       09  QUAREQ3-SHO-KIGO        PIC X(80).
      *番号
                       09  QUAREQ3-SHO-NUM         PIC X(80).
      *番号枝番
                       09  QUAREQ3-SHO-EDABAN      PIC X(02).
      *生年月日
                       09  QUAREQ3-SHO-BIRTHDAY    PIC X(08).
      *限度額適用認定証提供同意フラグ
                       09  QUAREQ3-SHO-DOUIFLG     PIC X(01).
      *任意の識別子(医療機関固有項目)
                       09  QUAREQ3-SHO-SIKIBETU    PIC X(50).
      *処理結果状況
                   07  QUAREQ3-RESULT              PIC X(01).
      *処理結果コード
                   07  QUAREQ3-RESULT-CODE         PIC X(09).
      *処理結果メッセージ
                   07  QUAREQ3-RESULT-MSG          PIC X(60).
      *資格有効性
                   07  QUAREQ3-SIKAKU-YUKO         PIC X(01).
      *確認結果リスト
                   07  QUAREQ3-RESULT-AREA         OCCURS  50.
      *被保険者証区分(確認結果内容)
                       09  QUAREQ3-CARD-CLASS          PIC X(02).
      *保険者番号
                       09  QUAREQ3-RES-HKNJANUM        PIC X(100).
      *記号
                       09  QUAREQ3-RES-KIGO            PIC X(80).
      *番号
                       09  QUAREQ3-RES-NUM             PIC X(80).
      *番号枝番
                       09  QUAREQ3-RES-EDABAN          PIC X(02).
      *本人家族区分
                       09  QUAREQ3-RES-HONKZKKBN       PIC X(01).
      *被保険者氏名(世帯主氏名)
                       09  QUAREQ3-RES-HIHKNJANAME     PIC X(100).
      *氏名
                       09  QUAREQ3-RES-NAME            PIC X(100).
      *氏名（その他）
                       09  QUAREQ3-RES-NAME-ETC        PIC X(100).
      *氏名カナ
                       09  QUAREQ3-RES-KANANAME        PIC X(100).
      *氏名カナ（その他）
                       09  QUAREQ3-RES-KANANAME-ETC    PIC X(100).
      *性別１
                       09  QUAREQ3-RES-SEX1            PIC X(01).
      *性別２
                       09  QUAREQ3-RES-SEX2            PIC X(01).
      *生年月日
                       09  QUAREQ3-RES-BIRTHDAY        PIC X(08).
      *住所
                       09  QUAREQ3-RES-ADRS            PIC X(250).
      *郵便番号
                       09  QUAREQ3-RES-POST            PIC X(08).
      *
      *被保険者証交付年月日
                       09  QUAREQ3-SKKGETYMD           PIC X(08).
      *被保険者証有効開始年月日
                       09  QUAREQ3-TEKSTYMD            PIC X(100).
      *被保険者証有効終了年月日
                       09  QUAREQ3-TEKEDYMD            PIC X(100).
      *被保険者証一部負担金割合
                       09  QUAREQ3-FTNRATE             PIC X(100).
      *被保険者証一部負担金限度額
                       09  QUAREQ3-FTNGAKU             PIC X(100).
      *未就学区分
                       09  QUAREQ3-MISYUGAKU           PIC X(01).
      *資格喪失事由
                       09  QUAREQ3-LOSS-JIYU           PIC X(02).
      *保険者名称
                       09  QUAREQ3-HKNJANUM-NAME       PIC X(200).
      *
      *高齢受給者証交付年月日
                       09  QUAREQ3-ELDER-SKKGETYMD     PIC X(08).
      *高齢受給者証有効開始年月日
                       09  QUAREQ3-ELDER-TEKSTYMD      PIC X(08).
      *高齢受給者証有効終了年月日
                       09  QUAREQ3-ELDER-TEKEDYMD      PIC X(08).
      *高齢受給者一部負担金割合
                       09  QUAREQ3-ELDER-FTNRATE       PIC X(03).
      *
      *限度額適用認定証提供同意フラグ
                       09  QUAREQ3-GENDO-DOUIFLG       PIC X(01).
      *限度額適用認定証提供同意日時
                       09  QUAREQ3-GENDO-TIME          PIC X(14).
      *限度額適用認定証区分
                       09  QUAREQ3-GENDO-SHOKBN        PIC X(02).
      *限度額適用認定証適用区分
                       09  QUAREQ3-GENDO-TEKKBN        PIC X(03).
      *限度額適用認定証交付年月日
                       09  QUAREQ3-GENDO-SKKGETYMD     PIC X(08).
      *限度額適用認定証有効開始年月日
                       09  QUAREQ3-GENDO-TEKSTYMD      PIC X(08).
      *限度額適用認定証有効終了年月日
                       09  QUAREQ3-GENDO-TEKEDYMD      PIC X(08).
      *限度額適用認定証長期入院該当年月日
                       09  QUAREQ3-GENDO-SKJGNSTYMD    PIC X(08).
      *特定疾病療養受療証提供同意フラグ
                       09  QUAREQ3-SIKKAN-DOUIFLG      PIC X(01).
      *特定疾病療養受療証提供同意日時
                       09  QUAREQ3-SIKKAN-TIME         PIC X(14).
      *特定疾病療養受療証認定疾病
                       09  QUAREQ3-SIKKAN-AREA.
                           11  QUAREQ3-SIKKAN-OCC      OCCURS  3.
      *特定疾病療養受療証認定疾病区分
                               13  QUAREQ3-SIKKAN-SIPKBN
                                                       PIC X(01).
      *特定疾病療養受療証交付年月日
                               13  QUAREQ3-SIKKAN-SKJGNSTYMD
                                                       PIC X(08).
      *特定疾病療養受療証有効開始年月日
                               13  QUAREQ3-SIKKAN-TEKSTYMD
                                                       PIC X(08).
      *特定疾病療養受療証有効終了年月日
                               13  QUAREQ3-SIKKAN-TEKEDYMD
                                                       PIC X(08).
      *特定疾病療養受療証自己負担限度額
                               13  QUAREQ3-SIKKAN-FTNMONEY
                                                       PIC X(06).
      *特定健診情報
                       09  QUAREQ3-KENSIN-AREA.
      *特定健診情報閲覧同意フラグ
                           11  QUAREQ3-KENSIN-DOUIFLG  PIC X(01).
      *特定健診情報閲覧同意日時
                           11  QUAREQ3-KENSIN-TIME     PIC X(14).
      *特定健診情報閲覧有効期限
                           11  QUAREQ3-KENSIN-KIGENYMD PIC X(14).
      *薬剤情報閲覧
                       09  QUAREQ3-YAKUZAI-AREA.
      *薬剤情報閲覧同意フラグ
                           11  QUAREQ3-YAKUZAI-DOUIFLG PIC X(01).
      *薬剤情報閲覧同意日時
                           11  QUAREQ3-YAKUZAI-TIME    PIC X(14).
      *薬剤情報閲覧有効期限
                           11  QUAREQ3-YAKUZAI-KIGEN   PIC X(14).
      *任意の識別子(医療機関固有項目)
                       09  QUAREQ3-SIKIBETU            PIC X(50).
      *照会番号
                       09  QUAREQ3-SHOKAI-NUM          PIC X(50).
                   
                   
                   


