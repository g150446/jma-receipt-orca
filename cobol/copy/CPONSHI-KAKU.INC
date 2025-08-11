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
      *    オンライン資格確認照会結果テーブル
      *                                COPY  (CPONSHI-KAKU.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.01.00     ORCAMO       22/01/28  削除フラグ追加
      * 05.01.01     ORCAMO       22/12/09  処方箋発行形態、手術項目
      *                                     情報追加
      *                                     特定健診情報閲覧有効期限
      *                                     桁数変更
      * 05.02.01    ORCAMO        23/12/21  医療扶助対応
      * 05.02.02    ORCAMO        24/04/08  医療扶助併用対応
      *                                     併用フラグ、仮医療扶助ＵＩＤ追加
      * 05.02.03    ORCAMO        24/07/26  資格取得年月日追加
      * 05.02.04    ORCAMO        24/11/15  訪問診療対応
      * 05.02.05    ORCAMO        25/01/16  電子カルテ共有サービス対応
      ************************************************************
      *01  ONSKAKU-REC.
      *キー
           03  ONSKAKU-KEY.
      *医療機関ＩＤ
               05  ONSKAKU-HOSPNUM             PIC 9(02).
      *ＵＩＤ
               05  ONSKAKU-TBL-UUID            PIC X(36).
      *相手ＵＩＤ
           03  ONSKAKU-AITE-UUID           PIC X(36).
      *親ＵＩＤ
           03  ONSKAKU-OYA-UUID            PIC X(36).
      *公費ＵＩＤ
           03  ONSKAKU-KOUHI-UUID          PIC X(36).
      *医療扶助ＵＩＤ
           03  ONSKAKU-FUJYO-UUID          PIC X(36).
      *仮医療扶助ＵＩＤ
           03  ONSKAKU-FUJYO-UUID1         PIC X(36).
      *併用フラグ
           03  ONSKAKU-FUJYO-HEIYO         PIC X(01).
      *一括同意識別
           03  ONSKAKU-PROCESS-CLASS       PIC X(01).
      *日レセ用エラーコード
           03  ONSKAKU-ORCA-ERR-CODE       PIC X(02).
      *PTID設定区分
           03  ONSKAKU-SET-PTID            PIC X(01).
      *日レセ資格有効性
           03  ONSKAKU-ORCA-SIKAKU-YUKO    PIC X(01).
      *画像ファイル名
           03  ONSKAKU-HKNOCR-FILENAME     PIC X(100).
      *資格確認リトライ回数
           03  ONSKAKU-KAKU-RENUMBER       PIC 9(02).
      *資格確認依頼済フラグ
           03  ONSKAKU-KAKU-ENDFLG         PIC X(02).
      *資格確認依頼年月日
           03  ONSKAKU-KAKU-KAKUNINYMD     PIC X(08).
      *資格確認依頼時間
           03  ONSKAKU-KAKU-KAKUNINHMS     PIC X(06).
      *照会番号登録リトライ回数
           03  ONSKAKU-SHOU-RENUMBER       PIC 9(02).
      *照会番号登録依頼済フラグ
           03  ONSKAKU-SHOU-ENDFLG         PIC X(02).
      *薬剤情報取得リトライ回数
           03  ONSKAKU-YAKUZAI-RENUMBER    PIC 9(02).
      *薬剤情報取得依頼済フラグ
           03  ONSKAKU-YAKUZAI-ENDFLG      PIC X(02).
      *特定検診情報取得リトライ回数
           03  ONSKAKU-KENSIN-RENUMBER     PIC 9(02).
      *特定検診情報取得依頼済フラグ
           03  ONSKAKU-KENSIN-ENDFLG       PIC X(02).
      *
      *患者ＩＤ
           03  ONSKAKU-PTID                PIC  9(10).
      *複数該当
           03  ONSKAKU-FUKUSU-GAITO        PIC  X(01).
      *処理実行日時
           03  ONSKAKU-SHORI-TIME          PIC  X(14).
      *資格確認日
           03  ONSKAKU-KAKUNINYMD          PIC  X(08).
      *医療機関コード
           03  ONSKAKU-HOSPCD              PIC  X(10).
      *任意のファイル識別子(医療機関固有項目)
           03  ONSKAKU-FILE-SIKIBETU       PIC  X(50).
      *照会区分
           03  ONSKAKU-SHOKAI-KBN          PIC  X(01).
      *日レセ照会区分
           03  ONSKAKU-ORCA-SHOKAI-KBN     PIC  X(01).
      *診療区分
           03  ONSKAKU-ONSHINRYO-KBN       PIC  X(01).
      *処理結果区分
           03  ONSKAKU-RESULT-KBN          PIC  X(01).
      *エラーコード
           03  ONSKAKU-ERR-CODE            PIC  X(09).
      *エラーメッセージ
           03  ONSKAKU-ERR-MSG             PIC  X(120).
      *文字コード識別
           03  ONSKAKU-CODE-TYPE           PIC  X(01).
      *照会内容
           03  ONSKAKU-SHOKAI-AREA.
      *保険者番号
               05  ONSKAKU-SHO-HKNJANUM         PIC  X(08).
      *記号
               05  ONSKAKU-SHO-KIGO             PIC  X(80).
      *番号
               05  ONSKAKU-SHO-NUM              PIC  X(80).
      *番号枝番
               05  ONSKAKU-SHO-EDABAN           PIC  X(02).
      *生年月日
               05  ONSKAKU-SHO-BIRTHDAY         PIC  X(08).
      *限度額適用認定証提供同意フラグ
               05  ONSKAKU-SHO-DOUIFLG          PIC  X(01).
      *処方箋発行形態(2022/7)
      *処方箋発行形態
           03  ONSKAKU-SHO-SHOHO-KEITAI         PIC  X(01).
      *処理結果状況
           03  ONSKAKU-RESULT              PIC  X(01).
      *処理結果コード
           03  ONSKAKU-RESULT-CODE         PIC  X(09).
      *処理結果メッセージ
           03  ONSKAKU-RESULT-MSG          PIC  X(120).
      *
      *資格有効性
           03  ONSKAKU-SIKAKU-YUKO         PIC  X(01).
      *
      *確認結果
           03  ONSKAKU-RESULT-AREA.
      *被保険者証区分(確認結果内容)
               05  ONSKAKU-CARD-CLASS          PIC  X(02).
      *保険者番号
               05  ONSKAKU-RES-HKNJANUM        PIC  X(08).
      *記号
               05  ONSKAKU-RES-KIGO            PIC  X(80).
      *番号
               05  ONSKAKU-RES-NUM             PIC  X(80).
      *番号枝番
               05  ONSKAKU-RES-EDABAN          PIC  X(02).
      *本人家族区分
               05  ONSKAKU-RES-HONKZKKBN       PIC  X(01).
      *被保険者氏名(世帯主氏名)
               05  ONSKAKU-RES-HIHKNJANAME     PIC  X(100).
      *氏名
               05  ONSKAKU-RES-NAME            PIC  X(100).
      *氏名（その他）
               05  ONSKAKU-RES-NAME-ETC        PIC  X(100).
      *氏名カナ
               05  ONSKAKU-RES-KANANAME        PIC  X(100).
      *氏名カナ（その他）
               05  ONSKAKU-RES-KANANAME-ETC    PIC  X(100).
      *性別１
               05  ONSKAKU-RES-SEX1            PIC  X(01).
      *性別２
               05  ONSKAKU-RES-SEX2            PIC  X(01).
      *生年月日
               05  ONSKAKU-RES-BIRTHDAY        PIC  X(08).
      *住所
               05  ONSKAKU-RES-ADRS            PIC  X(250).
      *郵便番号
               05  ONSKAKU-RES-POST            PIC  X(08).
      *
      *資格取得年月日
               05  ONSKAKU-JOIN-SKKGETYMD      PIC  X(08).
      *被保険者証交付年月日
               05  ONSKAKU-SKKGETYMD           PIC  X(08).
      *被保険者証有効開始年月日
               05  ONSKAKU-TEKSTYMD            PIC  X(08).
      *被保険者証有効終了年月日
               05  ONSKAKU-TEKEDYMD            PIC  X(08).
      *被保険者証一部負担金割合
               05  ONSKAKU-FTNRATE             PIC  X(03).
      *未就学区分
               05  ONSKAKU-MISYUGAKU           PIC  X(01).
      *資格喪失事由
               05  ONSKAKU-LOSS-JIYU           PIC  X(02).
      *保険者名称
               05  ONSKAKU-HKNJANUM-NAME       PIC  X(128).
      *
      *高齢受給者証交付年月日
               05  ONSKAKU-ELDER-SKKGETYMD     PIC  X(08).
      *高齢受給者証有効開始年月日
               05  ONSKAKU-ELDER-TEKSTYMD      PIC  X(08).
      *高齢受給者証有効終了年月日
               05  ONSKAKU-ELDER-TEKEDYMD      PIC  X(08).
      *高齢受給者一部負担金割合
               05  ONSKAKU-ELDER-FTNRATE       PIC  X(03).
      *
      *公費情報（１：入院　２：外来）
               05  ONSKAKU-KOH-OCC             OCCURS  2.
      *公費有効開始年月日
                   07  ONSKAKU-KOH-TEKSTYMD    PIC  X(08).
      *公費有効終了年月日
                   07  ONSKAKU-KOH-TEKEDYMD    PIC  X(08).
      *公費負担割合
                   07  ONSKAKU-KOH-FTNRATE     PIC  9(03).
      *公費上限額
                   07  ONSKAKU-KOH-JGNGAK      PIC  9(06).
      *
      *限度額適用認定証提供同意フラグ
               05  ONSKAKU-GENDO-DOUIFLG       PIC  X(01).
      *限度額適用認定証提供同意日時
               05  ONSKAKU-GENDO-TIME          PIC  X(14).
      *限度額適用認定証区分
               05  ONSKAKU-GENDO-SHOKBN        PIC  X(02).
      *限度額適用認定証適用区分
               05  ONSKAKU-GENDO-TEKKBN        PIC  X(03).
      *限度額適用認定証交付年月日
               05  ONSKAKU-GENDO-SKKGETYMD     PIC  X(08).
      *限度額適用認定証有効開始年月日
               05  ONSKAKU-GENDO-TEKSTYMD      PIC  X(08).
      *限度額適用認定証有効終了年月日
               05  ONSKAKU-GENDO-TEKEDYMD      PIC  X(08).
      *限度額適用認定証長期入院該当年月日
               05  ONSKAKU-GENDO-SKJGNSTYMD    PIC  X(08).
      *
      *特定疾病療養受療証提供同意フラグ
               05  ONSKAKU-SIKKAN-DOUIFLG      PIC  X(01).
      *特定疾病療養受療証提供同意日時
               05  ONSKAKU-SIKKAN-TIME         PIC  X(14).
      *特定疾病療養受療証認定疾病
               05  ONSKAKU-SIKKAN-AREA.
                   07  ONSKAKU-SIKKAN-OCC     OCCURS   3.
      *特定疾病療養受療証認定疾病区分
                       09  ONSKAKU-SIKKAN-SIPKBN       PIC  X(01).
      *特定疾病療養受療証交付年月日
                       09  ONSKAKU-SIKKAN-SKJGNSTYMD   PIC  X(08).
      *特定疾病療養受療証有効開始年月日
                       09  ONSKAKU-SIKKAN-TEKSTYMD     PIC  X(08).
      *特定疾病療養受療証有効終了年月日
                       09  ONSKAKU-SIKKAN-TEKEDYMD     PIC  X(08).
      *特定疾病療養受療証自己負担限度額
                       09  ONSKAKU-SIKKAN-FTNMONEY     PIC  X(06).
      *特定健診情報
               05  ONSKAKU-KENSIN-AREA.
      *特定健診情報閲覧同意フラグ
                   07  ONSKAKU-KENSIN-DOUIFLG      PIC  X(01).
      *特定健診情報閲覧同意日時
                   07  ONSKAKU-KENSIN-TIME         PIC  X(14).
      *特定健診情報閲覧有効期限
                   07  ONSKAKU-KENSIN-KIGENYMD     PIC  X(14).
      *薬剤情報閲覧
               05  ONSKAKU-YAKUZAI-AREA.
      *薬剤情報閲覧同意フラグ
                   07  ONSKAKU-YAKUZAI-DOUIFLG     PIC  X(01).
      *薬剤情報閲覧同意日時
                   07  ONSKAKU-YAKUZAI-TIME        PIC  X(14).
      *薬剤情報閲覧有効期限
                   07  ONSKAKU-YAKUZAI-KIGEN       PIC  X(14).
      *
      *診療情報閲覧
               05  ONSKAKU-SHINRYO-AREA.
      *診療情報閲覧同意フラグ
                   07  ONSKAKU-SHINRYO-DOUIFLG     PIC  X(01).
      *診療情報閲覧同意日時
                   07  ONSKAKU-SHINRYO-TIME        PIC  X(14).
      *診療情報閲覧有効期限
                   07  ONSKAKU-SHINRYO-KIGEN       PIC  X(14).
      *
      *手術情報閲覧
               05  ONSKAKU-OPE-AREA.
      *手術情報閲覧同意フラグ
                   07  ONSKAKU-OPE-DOUIFLG         PIC  X(01).
      *手術情報閲覧同意日時
                   07  ONSKAKU-OPE-TIME            PIC  X(14).
      *手術情報閲覧有効期限
                   07  ONSKAKU-OPE-KIGEN           PIC  X(14).
      *
      *傷病名情報閲覧
               05  ONSKAKU-SHOBYO-AREA.
      *傷病名情報閲覧同意フラグ
                   07  ONSKAKU-SHOBYO-DOUIFLG      PIC  X(01).
      *傷病名情報閲覧同意日時
                   07  ONSKAKU-SHOBYO-TIME         PIC  X(14).
      *傷病名情報閲覧有効期限
                   07  ONSKAKU-SHOBYO-KIGEN        PIC  X(14).
      *
      *感染症情報閲覧
               05  ONSKAKU-KANSENSYO-AREA.
      *感染症情報閲覧同意フラグ
                   07  ONSKAKU-KANSENSYO-DOUIFLG   PIC  X(01).
      *感染症情報閲覧同意日時
                   07  ONSKAKU-KANSENSYO-TIME      PIC  X(14).
      *感染症情報閲覧有効期限
                   07  ONSKAKU-KANSENSYO-KIGEN     PIC  X(14).
      *
      *その他アレルギー情報閲覧
               05  ONSKAKU-ETC-ALLERGY-AREA.
      *その他アレルギー情報閲覧同意フラグ
                   07  ONSKAKU-ETC-ALLERGY-DOUIFLG PIC  X(01).
      *その他アレルギー情報閲覧同意日時
                   07  ONSKAKU-ETC-ALLERGY-TIME    PIC  X(14).
      *その他アレルギー情報閲覧有効期限
                   07  ONSKAKU-ETC-ALLERGY-KIGEN   PIC  X(14).
      *
      *薬剤アレルギー情報閲覧
               05  ONSKAKU-YKZ-ALLERGY-AREA.
      *薬剤アレルギー情報閲覧同意フラグ
                   07  ONSKAKU-YKZ-ALLERGY-DOUIFLG PIC  X(01).
      *薬剤アレルギー情報閲覧同意日時
                   07  ONSKAKU-YKZ-ALLERGY-TIME    PIC  X(14).
      *薬剤アレルギー情報閲覧有効期限
                   07  ONSKAKU-YKZ-ALLERGY-KIGEN   PIC  X(14).
      *
      *検査情報閲覧
               05  ONSKAKU-KENSA-AREA.
      *検査情報閲覧同意フラグ
                   07  ONSKAKU-KENSA-DOUIFLG       PIC  X(01).
      *検査情報閲覧同意日時
                   07  ONSKAKU-KENSA-TIME          PIC  X(14).
      *検査情報閲覧有効期限
                   07  ONSKAKU-KENSA-KIGEN         PIC  X(14).
      *
      *処方情報閲覧
               05  ONSKAKU-SHOHO-AREA.
      *処方情報閲覧同意フラグ
                   07  ONSKAKU-SHOHO-DOUIFLG       PIC  X(01).
      *処方情報閲覧同意日時
                   07  ONSKAKU-SHOHO-TIME          PIC  X(14).
      *処方情報閲覧有効期限
                   07  ONSKAKU-SHOHO-KIGEN         PIC  X(14).
      *
      *オン資登録（更新）日時(2024/12)
           03  ONSKAKU-ONSHI-UPYMD         PIC  X(14).
      *資格確認キー(2024/12)
           03  ONSKAKU-SIKAKU-KAKUNIN-KEY  PIC  X(64).
      *資格情報変更確認フラグ(2024/12)
           03  ONSKAKU-SIKAKU-UPDATE-FLG   PIC  X(01).
      *任意の識別子(医療機関固有項目)
           03  ONSKAKU-SIKIBETU            PIC  X(50).
      *照会番号
           03  ONSKAKU-SHOKAI-NUM          PIC  X(50).
      *処理済みフラグ
           03  ONSKAKU-MOD-FLG             PIC  X(01).
      *処理中区分
           03  ONSKAKU-LOCK-KBN            PIC  X(01).
      *削除フラグ
           03  ONSKAKU-SCR-DLTFLG          PIC  X(01).
      *端末ＩＤ
           03  ONSKAKU-TERMID              PIC  X(64).
      *オペーレータＩＤ
           03  ONSKAKU-OPID                PIC  X(16).
      *作成年月日
           03  ONSKAKU-CREYMD              PIC  X(08).
      *作成時間
           03  ONSKAKU-CREHMS              PIC  X(06).
      *更新年月日
           03  ONSKAKU-UPYMD               PIC  X(08).
      *更新時間
           03  ONSKAKU-UPHMS               PIC  X(06).
      *LIMIT
           03  ONSKAKU-SELLIMIT            PIC  9(08).
      *OFFSET
           03  ONSKAKU-SELOFFSET           PIC  9(08).
