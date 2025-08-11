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
      *    ＡＰＩ資格確認用       COPY (CPONLINEQUAAPPREQ3.INC)       *
      *****************************************************************
       01  XML-ONLINEQUAAPPREQ3.
           03  QUAAPPREQ3-OBJECT               PIC X(36).
           03  QUAAPPREQ3-MODE                 PIC S9(9)   BINARY.
           03  QUAAPPREQ3-DATAREQ.
      *メッセージヘッダー部     
               05  QUAAPPREQ3-HEADER.
      *プロセス識別 (2024-12 訪問:2 オン診:3)
                   07  QUAAPPREQ3-PROCESS-CLASS
                                               PIC X(1).
      *処理実行日時
                   07  QUAAPPREQ3-SHORI-TIME   PIC X(14).
      *資格確認日
                   07  QUAAPPREQ3-KAKUNINYMD   PIC X(08).
      *医療機関コード
                   07  QUAAPPREQ3-HOSPCD       PIC X(10).
      *任意のファイル識別子(医療機関固有項目)
                   07  QUAAPPREQ3-FILE-SIKIBETU
                                               PIC X(50).
      *受付番号
                   07  QUAAPPREQ3-SHOKAI-KBN   PIC X(38).
      *処理結果区分
                   07  QUAAPPREQ3-RESULT-KBN   PIC X(01).
      *エラーコード
                   07  QUAAPPREQ3-ERR-CODE     PIC X(09).
      *エラーメッセージ
                   07  QUAAPPREQ3-ERR-MSG      PIC X(60).
      *総処理件数             
                   07  QUAAPPREQ3-ALL-CNT      PIC X(4).
      *処理件数(正常)             
                   07  QUAAPPREQ3-OK-CNT       PIC X(4).
      *処理件数(エラー)            
                   07  QUAAPPREQ3-ERR-CNT      PIC X(4).
      *文字コード識別
                   07  QUAAPPREQ3-CODE-TYPE    PIC X(1).
      *メッセージボディ部    
               05  QUAAPPREQ3-MESSAGEBODY.
      *資格確認照会用情報(2024-12 訪問のみ)
                   07  QUAAPPREQ3-HO-SHOKAI-AREA.
      * 同意年月日（自） (2024-12 訪問のみ)
                       09  QUAAPPREQ3-HO-DOUI-STARYMD PIC X(8).
      * 同意年月日（至） (2024-12 訪問のみ)
                       09  QUAAPPREQ3-HO-DOUI-ENDYMD  PIC X(8).
      * 診療区分 (2024-12 訪問のみ)
                       09  QUAAPPREQ3-HO-SRYKBN    PIC X(1).
      *処理結果状況(2024-12 訪問のみ)
                   07  QUAAPPREQ3-HO-RESULT        PIC X(1).
      *処理結果コード(2024-12 訪問のみ)
                   07  QUAAPPREQ3-HO-RESULT-CODE   PIC X(9).
      *個人単位情報
                   07  QUAAPPREQ3-KOJIN-AREA.
      *保険者番号
                       09  QUAAPPREQ3-SHO-HKNJANUM PIC X(08).
      *記号
                       09  QUAAPPREQ3-SHO-KIGO     PIC X(80).
      *番号
                       09  QUAAPPREQ3-SHO-NUM      PIC X(80).
      *番号枝番
                       09  QUAAPPREQ3-SHO-EDABAN   PIC X(02).
      *生年月日
                       09  QUAAPPREQ3-SHO-BIRTHDAY PIC X(08).
      *限度額適用認定証区分
                       09  QUAAPPREQ3-GENDO-SHOKBN PIC X(02).
      *限度額適用認定証適用区分
                       09  QUAAPPREQ3-GENDO-TEKKBN PIC X(03).
      *任意の識別子(医療機関固有項目)
                       09  QUAAPPREQ3-SHO-SIKIBETU PIC X(50).
      *処理結果状況
                       09  QUAAPPREQ3-RESULT       PIC X(01).
      *処理結果コード
                       09  QUAAPPREQ3-RESULT-CODE  PIC X(09).
      *処理結果メッセージ
                       09  QUAAPPREQ3-RESULT-MSG   PIC X(60).
      *資格有効性
                       09  QUAAPPREQ3-SIKAKU-YUKO  PIC X(01).
      *資格情報提供同意フラグ (2024-12 訪問のみ)
                       09  QUAAPPREQ3-HO-DOUIFLG   PIC X(01).
      *資格情報提供同意日時 (2024-12 訪問のみ)
                       09  QUAAPPREQ3-HO-TIME      PIC X(14).
      *資格情報提供有効期限 (2024-12 訪問のみ)
                       09  QUAAPPREQ3-HO-KIGEN     PIC X(14).
      *資格情報提供同意有効期限切れ理由 (2024-12 訪問のみ)
                       09  QUAAPPREQ3-HO-RIYU      PIC X(1).
      *確認結果リスト
                       09  QUAAPPREQ3-RESULT-AREA  OCCURS  2.
      *被保険者証区分(確認結果内容)
                         11  QUAAPPREQ3-CARD-CLASS         PIC X(02).
      *保険者番号
                         11  QUAAPPREQ3-RES-HKNJANUM       PIC X(08).
      *記号
                         11  QUAAPPREQ3-RES-KIGO           PIC X(80).
      *番号
                         11  QUAAPPREQ3-RES-NUM            PIC X(80).
      *番号枝番
                         11  QUAAPPREQ3-RES-EDABAN         PIC X(02).
      *本人家族区分
                         11  QUAAPPREQ3-RES-HONKZKKBN      PIC X(01).
      *被保険者氏名(世帯主氏名)
                         11  QUAAPPREQ3-RES-HIHKNJANAME    PIC X(100).
      *氏名
                         11  QUAAPPREQ3-RES-NAME           PIC X(100).
      *氏名（その他）
                         11  QUAAPPREQ3-RES-NAME-ETC       PIC X(100).
      *氏名カナ
                         11  QUAAPPREQ3-RES-KANANAME       PIC X(100).
      *氏名カナ（その他）
                         11  QUAAPPREQ3-RES-KANANAME-ETC   PIC X(100).
      *性別１
                         11  QUAAPPREQ3-RES-SEX1           PIC X(01).
      *性別２
                         11  QUAAPPREQ3-RES-SEX2           PIC X(01).
      *生年月日
                         11  QUAAPPREQ3-RES-BIRTHDAY       PIC X(08).
      *住所
                         11  QUAAPPREQ3-RES-ADRS           PIC X(250).
      *郵便番号
                         11  QUAAPPREQ3-RES-POST           PIC X(08).
      *
      *資格取得年月日（加入者資格）
                         11  QUAAPPREQ3-JOIN-SKKGETYMD     PIC X(08).
      *資格喪失年月日（加入者資格）
                         11  QUAAPPREQ3-JOIN-SKKLOSSYMD    PIC X(08).
      *被保険者証交付年月日
                         11  QUAAPPREQ3-SKKGETYMD          PIC X(08).
      *被保険者証有効開始年月日
                         11  QUAAPPREQ3-TEKSTYMD           PIC X(08).
      *被保険者証有効終了年月日
                         11  QUAAPPREQ3-TEKEDYMD           PIC X(08).
      *被保険者証一部負担金割合
                         11  QUAAPPREQ3-FTNRATE            PIC X(03).
      *未就学区分
                         11  QUAAPPREQ3-MISYUGAKU          PIC X(01).
      *資格喪失事由
                         11  QUAAPPREQ3-LOSS-JIYU          PIC X(02).
      *保険者名称
                         11  QUAAPPREQ3-HKNJANUM-NAME      PIC X(200).
      *
      *高齢受給者証交付年月日
                         11  QUAAPPREQ3-ELDER-SKKGETYMD    PIC X(08).
      *高齢受給者証有効開始年月日
                         11  QUAAPPREQ3-ELDER-TEKSTYMD     PIC X(08).
      *高齢受給者証有効終了年月日
                         11  QUAAPPREQ3-ELDER-TEKEDYMD     PIC X(08).
      *高齢受給者一部負担金割合
                         11  QUAAPPREQ3-ELDER-FTNRATE      PIC X(03).
      *限度額適用認定証情報変更有無
                         11  QUAAPPREQ3-GENDO-HENKOU-UM    PIC X(01).
      *限度額適用認定証提供同意フラグ (2024-12 訪問のみ)
                         11  QUAAPPREQ3-HO-GENDO-DOUIFLG   PIC X(01).
      *限度額適用認定証提供同意日時 (2024-12 訪問のみ)
                         11  QUAAPPREQ3-HO-GENDO-TIME      PIC X(14).
      *限度額適用認定証提供有効期限 (2024-12 訪問のみ)
                         11  QUAAPPREQ3-HO-GENDO-KIGEN     PIC X(14).
      *限度額適用認定証関連情報 (2024-12 訪問のみ)
                         11  QUAAPPREQ3-HO-GENDO-AREA.
      *限度額適用認定証区分 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-GENDO-SHOKBN     PIC X(02).
      *限度額適用認定証適用区分 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-GENDO-TEKKBN     PIC X(03).
      *限度額適用認定証交付年月日 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-GENDO-SKKGETYMD  PIC X(08).
      *限度額適用認定証有効開始年月日 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-GENDO-TEKSTYMD   PIC X(08).
      *限度額適用認定証有効終了年月日 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-GENDO-TEKEDYMD   PIC X(08).
      *限度額適用認定証長期入院該当年月日 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-GENDO-SKJGNSTYMD PIC X(08).
      *特定疾病療養受療証提供同意フラグ (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SIKKAN-DOUIFLG   PIC X(01).
      *特定疾病療養受療証提供同意日時 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SIKKAN-TIME      PIC X(14).
      *特定疾病療養受療証提供有効期限 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SIKKAN-KIGEN     PIC X(14).
      *特定疾病療養受療証提供同意有効期限切れ理由 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SIKKAN-RIYU      PIC X(1).
      *特定疾病療養受療証情報リスト (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SIKKAN-AREA.
      *特定疾病療養受療証情報 (2024-12 訪問のみ)
                             15  QUAAPPREQ3-HO-SIKKAN-OCC  OCCURS   3.
      *特定疾病療養受療証認定疾病区分 (2024-12 訪問のみ)
                               17  QUAAPPREQ3-HO-SIKKAN-SIPKBN
                                                              PIC X(01).
      *特定疾病療養受療証交付年月日 (2024-12 訪問のみ)
                               17  QUAAPPREQ3-HO-SIKKAN-SKJGNSTYMD
                                                              PIC X(08).
      *特定疾病療養受療証有効開始年月日 (2024-12 訪問のみ)
                               17  QUAAPPREQ3-HO-SIKKAN-TEKSTYMD
                                                              PIC X(08).
      *特定疾病療養受療証有効終了年月日 (2024-12 訪問のみ)
                               17  QUAAPPREQ3-HO-SIKKAN-TEKEDYMD
                                                              PIC X(08).
      *特定疾病療養受療証自己負担限度額 (2024-12 訪問のみ)
                               17  QUAAPPREQ3-HO-SIKKAN-FTNMONEY
                                                              PIC X(06).
      *医療扶助資格確認結果リスト (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-FUJYO-AREA.
      *医療券・調剤券情報 (2024-12 訪問のみ)
                             15  QUAAPPREQ3-HO-FUJYO-OCC  OCCURS   4.
      *交付番号 (2024-12 訪問のみ)
                               17  QUAAPPREQ3-HO-FUJYO-KOFUNUM
                                                              PIC X(30).
      *診療年月 (2024-12 訪問のみ)
                               17  QUAAPPREQ3-HO-FUJYO-SRYYM  PIC X(06).
      *医療券・調剤券別(01 :医療券 02 :調剤券)
                               17  QUAAPPREQ3-HO-TICKET-TYPE  PIC X(02).
      *診療別(1: 入院 2:入院外　これ以外は捨て(3:歯科 etc))
                               17  QUAAPPREQ3-HO-MEDICAL-TYPE PIC X(02).
      *特定健診情報閲覧同意フラグ (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-KENSIN-DOUIFLG   PIC X(01).
      *特定健診情報閲覧同意日時 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-KENSIN-TIME      PIC X(14).
      *特定健診情報閲覧有効期限 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-KENSIN-KIGENYMD  PIC X(14).
      *特定健診情報閲覧同意有効期限切れ理由 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-KENSIN-RIYU      PIC X(1).
      *薬剤情報閲覧同意フラグ (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-YAKUZAI-DOUIFLG  PIC X(1).
      *薬剤情報閲覧同意日時 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-YAKUZAI-TIME     PIC X(14).
      *薬剤情報閲覧有効期限 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-YAKUZAI-KIGEN    PIC X(14).
      *薬剤情報閲覧同意有効期限切れ理由 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-YAKUZAI-RIYU     PIC X(1).
      *診療情報閲覧同意フラグ (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SHINRYO-DOUIFLG  PIC X(01).
      *診療情報閲覧同意日時 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SHINRYO-TIME     PIC X(14).
      *診療情報閲覧有効期限 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SHINRYO-KIGEN    PIC X(14).
      *診療情報閲覧同意有効期限切れ理由 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-SHINRYO-RIYU     PIC X(1).
      *手術情報閲覧同意フラグ (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-OPE-DOUIFLG      PIC X(01).
      *手術情報閲覧同意日時 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-OPE-TIME         PIC X(14).
      *手術情報閲覧有効期限 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-OPE-KIGEN        PIC X(14).
      *手術情報閲覧同意有効期限切れ理由 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-OPE-RIYU         PIC X(1).
      *オン資登録（更新）日時 (2024-12 訪問のみ)
                           13  QUAAPPREQ3-HO-ONSHI-CREYMD     PIC X(14).
      *照会番号
                       09  QUAAPPREQ3-SHOKAI-NUM          PIC X(50).
