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
      *    ＡＰＩ資格確認用       COPY (CPONLINEQUAREQ2.INC)          *
      *****************************************************************
       01  XML-ONLINEQUAREQ2.
           03  QUAREQ2-CONTEXT             PIC S9(9)   BINARY.
           03  QUAREQ2-OBJECT              PIC X(36).
           03  QUAREQ2-MODE                PIC S9(9)   BINARY.
           03  QUAREQ2-RECORDNAME          PIC X(128).
           03  QUAREQ2-PATIENTREQ.
      *メッセージヘッダー部     
               05  QUAREQ2-HEADER.
      *処理実行日時
                   07  QUAREQ2-SHORI-TIME  PIC X(14).
      *資格確認日
                   07  QUAREQ2-KAKUNINYMD  PIC X(08).
      *医療機関コード
                   07  QUAREQ2-HOSPCD      PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   07  QUAREQ2-FILE-SIKIBETU PIC X(50).
      *照会区分
                   07  QUAREQ2-SHOKAI-KBN  PIC X(01).
      *処理結果区分
                   07  QUAREQ2-RESULT-KBN  PIC X(01).
      *エラーコード
                   07  QUAREQ2-ERR-CODE    PIC X(09).
      *エラーメッセージ
                   07  QUAREQ2-ERR-MSG     PIC X(60).
      *文字コード識別
                   07  QUAREQ2-CODE-TYPE   PIC X(01).
      *扶助UID
			       07  QUAREQ2-FUJYO-UUID  PIC X(36).
      *メッセージボディ部    
               05  QUAREQ2-MESSAGEBODY.
      *照会内容
                   07  QUAREQ2-SHOKAI-AREA.
      *保険者番号
                       09  QUAREQ2-SHO-HKNJANUM    PIC X(08).
      *記号
                       09  QUAREQ2-SHO-KIGO        PIC X(80).
      *番号
                       09  QUAREQ2-SHO-NUM         PIC X(80).
      *番号枝番
                       09  QUAREQ2-SHO-EDABAN      PIC X(02).
      *生年月日
                       09  QUAREQ2-SHO-BIRTHDAY    PIC X(08).
      *限度額適用認定証提供同意フラグ
                       09  QUAREQ2-SHO-DOUIFLG     PIC X(01).
      *任意の識別子(医療機関固有項目)
                       09  QUAREQ2-SHO-SIKIBETU    PIC X(50).
      * 診療区分
                   07  QUAREQ2-SRYKBN              PIC X(1).
      *処方箋発行形態
                   07  QUAREQ2-SHO-SHOHO-KEITAI    PIC X(01).
      *処理結果状況
                   07  QUAREQ2-RESULT              PIC X(01).
      *処理結果コード
                   07  QUAREQ2-RESULT-CODE         PIC X(09).
      *処理結果メッセージ
                   07  QUAREQ2-RESULT-MSG          PIC X(60).
      *資格有効性
                   07  QUAREQ2-SIKAKU-YUKO         PIC X(01).
      *確認結果リスト
                   07  QUAREQ2-RESULT-AREA         OCCURS  50.
      *被保険者証区分(確認結果内容)
                       09  QUAREQ2-CARD-CLASS          PIC X(02).
      *保険者番号
                       09  QUAREQ2-RES-HKNJANUM        PIC X(08).
      *記号
                       09  QUAREQ2-RES-KIGO            PIC X(80).
      *番号
                       09  QUAREQ2-RES-NUM             PIC X(80).
      *番号枝番
                       09  QUAREQ2-RES-EDABAN          PIC X(02).
      *本人家族区分
                       09  QUAREQ2-RES-HONKZKKBN       PIC X(01).
      *被保険者氏名(世帯主氏名)
                       09  QUAREQ2-RES-HIHKNJANAME     PIC X(100).
      *氏名
                       09  QUAREQ2-RES-NAME            PIC X(100).
      *氏名（その他）
                       09  QUAREQ2-RES-NAME-ETC        PIC X(100).
      *氏名カナ
                       09  QUAREQ2-RES-KANANAME        PIC X(100).
      *氏名カナ（その他）
                       09  QUAREQ2-RES-KANANAME-ETC    PIC X(100).
      *性別１
                       09  QUAREQ2-RES-SEX1            PIC X(01).
      *性別２
                       09  QUAREQ2-RES-SEX2            PIC X(01).
      *生年月日
                       09  QUAREQ2-RES-BIRTHDAY        PIC X(08).
      *住所
                       09  QUAREQ2-RES-ADRS            PIC X(250).
      *郵便番号
                       09  QUAREQ2-RES-POST            PIC X(08).
      *
      *資格取得年月日（加入者資格）
                       09  QUAREQ2-JOIN-SKKGETYMD      PIC X(08).
      *資格喪失年月日（加入者資格）
                       09  QUAREQ2-JOIN-SKKLOSSYMD     PIC X(08).
      *
      *被保険者証交付年月日
                       09  QUAREQ2-SKKGETYMD           PIC X(08).
      *被保険者証有効開始年月日
                       09  QUAREQ2-TEKSTYMD            PIC X(08).
      *被保険者証有効終了年月日
                       09  QUAREQ2-TEKEDYMD            PIC X(08).
      *被保険者証一部負担金割合
                       09  QUAREQ2-FTNRATE             PIC X(03).
      *未就学区分
                       09  QUAREQ2-MISYUGAKU           PIC X(01).
      *資格喪失事由
                       09  QUAREQ2-LOSS-JIYU           PIC X(02).
      *保険者名称
                       09  QUAREQ2-HKNJANUM-NAME       PIC X(200).
      *
      *高齢受給者証交付年月日
                       09  QUAREQ2-ELDER-SKKGETYMD     PIC X(08).
      *高齢受給者証有効開始年月日
                       09  QUAREQ2-ELDER-TEKSTYMD      PIC X(08).
      *高齢受給者証有効終了年月日
                       09  QUAREQ2-ELDER-TEKEDYMD      PIC X(08).
      *高齢受給者一部負担金割合
                       09  QUAREQ2-ELDER-FTNRATE       PIC X(03).
      *
      *限度額適用認定証提供同意フラグ
                       09  QUAREQ2-GENDO-DOUIFLG       PIC X(01).
      *限度額適用認定証提供同意日時
                       09  QUAREQ2-GENDO-TIME          PIC X(14).
      *限度額適用認定証区分
                       09  QUAREQ2-GENDO-SHOKBN        PIC X(02).
      *限度額適用認定証適用区分
                       09  QUAREQ2-GENDO-TEKKBN        PIC X(03).
      *限度額適用認定証交付年月日
                       09  QUAREQ2-GENDO-SKKGETYMD     PIC X(08).
      *限度額適用認定証有効開始年月日
                       09  QUAREQ2-GENDO-TEKSTYMD      PIC X(08).
      *限度額適用認定証有効終了年月日
                       09  QUAREQ2-GENDO-TEKEDYMD      PIC X(08).
      *限度額適用認定証長期入院該当年月日
                       09  QUAREQ2-GENDO-SKJGNSTYMD    PIC X(08).
      *特定疾病療養受療証提供同意フラグ
                       09  QUAREQ2-SIKKAN-DOUIFLG      PIC X(01).
      *特定疾病療養受療証提供同意日時
                       09  QUAREQ2-SIKKAN-TIME         PIC X(14).
      *特定疾病療養受療証認定疾病
                       09  QUAREQ2-SIKKAN-AREA.
                           11  QUAREQ2-SIKKAN-OCC      OCCURS  3.
      *特定疾病療養受療証認定疾病区分
                               13  QUAREQ2-SIKKAN-SIPKBN
                                                       PIC X(01).
      *特定疾病療養受療証交付年月日
                               13  QUAREQ2-SIKKAN-SKJGNSTYMD
                                                       PIC X(08).
      *特定疾病療養受療証有効開始年月日
                               13  QUAREQ2-SIKKAN-TEKSTYMD
                                                       PIC X(08).
      *特定疾病療養受療証有効終了年月日
                               13  QUAREQ2-SIKKAN-TEKEDYMD
                                                       PIC X(08).
      *特定疾病療養受療証自己負担限度額
                               13  QUAREQ2-SIKKAN-FTNMONEY
                                                       PIC X(06).
      *特定健診情報
                       09  QUAREQ2-KENSIN-AREA.
      *特定健診情報閲覧同意フラグ
                           11  QUAREQ2-KENSIN-DOUIFLG  PIC X(01).
      *特定健診情報閲覧同意日時
                           11  QUAREQ2-KENSIN-TIME     PIC X(14).
      *特定健診情報閲覧有効期限
                           11  QUAREQ2-KENSIN-KIGENYMD PIC X(14).
      *薬剤情報閲覧
                       09  QUAREQ2-YAKUZAI-AREA.
      *薬剤情報閲覧同意フラグ
                           11  QUAREQ2-YAKUZAI-DOUIFLG PIC X(01).
      *薬剤情報閲覧同意日時
                           11  QUAREQ2-YAKUZAI-TIME    PIC X(14).
      *薬剤情報閲覧有効期限
                           11  QUAREQ2-YAKUZAI-KIGEN   PIC X(14).
      *
      *診療情報閲覧(2022/7 追加)
                       09  QUAREQ2-ONSKAKU-SHINRYO-AREA.
      *診療情報閲覧同意フラグ(2022/7 追加)
                           11  QUAREQ2-SHINRYO-DOUIFLG PIC X(01).
      *診療情報閲覧同意日時(2022/7 追加)
                           11  QUAREQ2-SHINRYO-TIME    PIC X(14).
      *診療情報閲覧有効期限(2022/7 追加)
                           11  QUAREQ2-SHINRYO-KIGEN   PIC X(14).
      *手術情報閲覧(2022/12 追加)
                        09  QUAREQ2-ONSKAKU-OPE-AREA.
      *診療情報閲覧同意フラグ
                           11  QUAREQ2-OPE-DOUIFLG     PIC X(01).
      *手術情報閲覧同意日時
                           11  QUAREQ2-OPE-TIME        PIC X(14).
      *手術情報閲覧有効期限
                           11  QUAREQ2-OPE-KIGEN       PIC X(14).
      *
      *傷病名情報閲覧(2025/01 追加)
                       09  QUAREQ2-ONSKAKU-SHOBYO-AREA.
      *傷病名情報閲覧同意フラグ
                           11  QUAREQ2-SHOBYO-DOUIFLG      PIC  X(01).
      *傷病名情報閲覧同意日時
                           11  QUAREQ2-SHOBYO-TIME         PIC  X(14).
      *傷病名情報閲覧有効期限
                           11  QUAREQ2-SHOBYO-KIGEN        PIC  X(14).
      *
      *感染症情報閲覧(2025/01 追加)
                       09  QUAREQ2-ONSKAKU-KANSENSYO-AREA.
      *感染症情報閲覧同意フラグ
                           11  QUAREQ2-KANSENSYO-DOUIFLG   PIC  X(01).
      *感染症情報閲覧同意日時
                           11  QUAREQ2-KANSENSYO-TIME      PIC  X(14).
      *感染症情報閲覧有効期限
                           11  QUAREQ2-KANSENSYO-KIGEN     PIC  X(14).
      *
      *その他アレルギー情報閲覧(2025/01 追加)
                       09  QUAREQ2-ONSKAKU-ET-ALLERGY-AREA.
      *その他アレルギー情報閲覧同意フラグ
                           11  QUAREQ2-ETC-ALLERGY-DOUIFLG PIC  X(01).
      *その他アレルギー情報閲覧同意日時
                           11  QUAREQ2-ETC-ALLERGY-TIME    PIC  X(14).
      *その他アレルギー情報閲覧有効期限
                           11  QUAREQ2-ETC-ALLERGY-KIGEN   PIC  X(14).
      *
      *薬剤アレルギー情報閲覧(2025/01 追加)
                       09  QUAREQ2-ONSKAKU-YK-ALLERGY-AREA.
      *薬剤アレルギー情報閲覧同意フラグ
                           11  QUAREQ2-YKZ-ALLERGY-DOUIFLG PIC  X(01).
      *薬剤アレルギー情報閲覧同意日時
                           11  QUAREQ2-YKZ-ALLERGY-TIME    PIC  X(14).
      *薬剤アレルギー情報閲覧有効期限
                           11  QUAREQ2-YKZ-ALLERGY-KIGEN   PIC  X(14).
      *
      *検査情報閲覧(2025/01 追加)
                       09  QUAREQ2-ONSKAKU-KENSA-AREA.
      *検査情報閲覧同意フラグ
                           11  QUAREQ2-KENSA-DOUIFLG       PIC  X(01).
      *検査情報閲覧同意日時
                           11  QUAREQ2-KENSA-TIME          PIC  X(14).
      *検査情報閲覧有効期限
                           11  QUAREQ2-KENSA-KIGEN         PIC  X(14).
      *
      *処方情報閲覧(2025/01 追加)
                       09  QUAREQ2-ONSKAKU-SHOHO-AREA.
      *処方情報閲覧同意フラグ
                           11  QUAREQ2-SHOHO-DOUIFLG       PIC  X(01).
      *処方情報閲覧同意日時
                           11  QUAREQ2-SHOHO-TIME          PIC  X(14).
      *処方情報閲覧有効期限
                           11  QUAREQ2-SHOHO-KIGEN         PIC  X(14).
      *
      *オン資登録（更新）日時(2024/12)
                           11  QUAREQ2-ONSHI-UPYMD     PIC X(14).
      *
      *任意の識別子(医療機関固有項目)
                       09  QUAREQ2-SIKIBETU            PIC X(50).
      *照会番号
                       09  QUAREQ2-SHOKAI-NUM          PIC X(50).
      *資格確認キー(2024/12)
                       09  QUAREQ2-SIKAKU-KAKUNIN-KEY  PIC X(64).
      *資格情報変更確認フラグ(2024/12)
                       09  QUAREQ2-SIKAKU-UPDATE-FLG   PIC X(01).
                   
                   
                   


