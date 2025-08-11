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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ請求管理情報    (xml2)
      *                        COPY (CPCLAIM-MANAGEMENTV3RES.INC)
      ******************************************************************
       01  XML-CLAIMED-MANAGEMENTRES.
           03  CLAIMS-CONTEXT         PIC S9(9)   BINARY.
           03  CLAIMS-OBJECT          PIC X(36).
           03  CLAIMS-MODE            PIC S9(9)   BINARY.
           03  CLAIMS-RECORDNAME      PIC X(128).
           03  CLAIMS-RES.
      *        リクエスト番号
               05  CLAIMS-REQUEST-NUMBER
                                       PIC X(2).
      *
               05  CLAIMS-RESPONS-NUMBER
                                       PIC X(2).
      *        カルテUID
               05  CLAIMS-KARTE-UID   PIC X(36).
      *        オルカUID
               05  CLAIMS-ORCA-UID    PIC X(36).
      *        実施日
               05  CLAIMS-INFORMATION-DATE
                                       PIC X(10).
      *        実施時間
               05  CLAIMS-INFORMATION-TIME
                                       PIC X(8).
      *        返却コード
               05  CLAIMS-API-RESULT  PIC X(3).
      *        返却メッセージ
               05  CLAIMS-API-RESULT-MSG
                                       PIC X(100).
      *
      *        ワーニング
               05  CLAIMS-API-WARNING-INFO     OCCURS  5.
                   07  CLAIMS-API-WARNING
                                           PIC X(3).
                   07  CLAIMS-API-WARNING-MSG
                                           PIC X(100).
      *
               05  CLAIMS-RESKEY      PIC X(100).
      *        患者情報
               05  CLAIMS-PATIENT-INFORMATION.
      *            患者番号
                   07  CLAIMS-PATIENT-ID   PIC X(20).
      *            患者氏名
                   07  CLAIMS-NAME         PIC X(100).
      *            診療年月
                   07  CLAIMS-PERFORM-MONTH
                                           PIC X(7).
      *            請求区分
                   07  CLAIMS-SKYKBN      PIC X(1).
      *            請求年月
                   07  CLAIMS-SKYYM       PIC X(7).
      *            返戻年月
                   07  CLAIMS-HENREIYM    PIC X(7).
      *            請求年月（公費請求分）
                   07  CLAIMS-KOHSKY-SKYYM PIC X(7).
      *            返戻年月（公費請求分）
                   07  CLAIMS-KOHSKY-HENREIYM    PIC X(7).
      *            レセプト情報
                   07  CLAIMS-RECEIPT-INFORMATION.
                     08 CLAIMS-RECEIPT-DETAIL-O OCCURS  50.
                       09  CLAIMS-RECEIPT-ERRCD
                                               PIC X(3).
                       09  CLAIMS-RECEIPT-ERR-MSG
                                           PIC X(100).
      *                提出先
                       09  CLAIMS-DETAIL-TEISYUTUSAKI
                                           PIC X(2).
                       09  CLAIMS-DETAIL-INOUT
                                           PIC X(1).
      *                種別コード
                       09  CLAIMS-DETAIL-CODE
                                           PIC X(04).
      *                種別
                       09  CLAIMS-DETAIL-TYPE
                                           PIC X(40).
                       09  CLAIMS-DETAIL-SKYKBN
                                           PIC X(1).
      *                請求年月
                       09  CLAIMS-RECEIPT-SKYYM       PIC X(7).
      *                返戻年月
                       09  CLAIMS-RECEIPT-HENREIYM    PIC X(7).
      *                保険ＩＤ
                       09  CLAIMS-HKNID    PIC X(10).
      *                保険番号
                       09  CLAIMS-HKNNUM   PIC X(03).
      *                保険情報
                       09  CLAIMS-MAIN-INSURANCE.
                           11  CLAIMS-MAIN-INSURANCE-1.
      *                        保険者番号
                               13  CLAIMS-HKNJANUM
                                           PIC X(08).
      *                        記号
                               13  CLAIMS-KIGO
                                           PIC X(38).
      *                        番号
                               13  CLAIMS-NUM
                                           PIC X(38).
      *                        補助区分
                               13  CLAIMS-HOJOKBN
                                           PIC X(01).
      *                    災害該当の有無
                           11  CLAIMS-ACCIDENT-TARGET
                                           PIC X(4).
      *                    入所中の有無
                           11  CLAIMS-INCARE-CLASS
                                           PIC X(4).
      *                公費情報
                       09  CLAIMS-PUBLIC-INSURANCE.
                           11  CLAIMS-PUBLIC-INSURANCE-O  OCCURS  4.
      *                        公費ＩＤ
                               13  CLAIMS-KOHID
                                           PIC X(10).
      *                        公費番号
                               13  CLAIMS-KOHNUM
                                           PIC X(03).
      *                        負担者番号
                               13  CLAIMS-FTNJANUM
                                           PIC X(08).
      *                        受給者番号
                               13  CLAIMS-JKYSNUM
                                           PIC X(20).
      *                労災情報
                       09  CLAIMS-ACCIDENT-INSURANCE.
      *                    労災自賠保険区分
                           11  CLAIMS-ACCIDENT
                                           PIC X(01).
      *                    労働保険番号
                           11  CLAIMS-ACCIDENT-NUM
                                           PIC X(14).
      *                     年金証書番号
                           11  CLAIMS-PENSION-NUM
                                           PIC X(09).
      *                    災害区分
                           11  CLAIMS-ACCIDENT-CLASS
                                           PIC X(1).
      *                    健康管理手帳番号
                           11  CLAIMS-KANRITCHO-NUMBER
                                           PIC X(13).
      *                    損傷区分
                           11  CLAIMS-DAMAGE-CLASS
                                           PIC X(03).
      *                    療養年月日
                           11  CLAIMS-RYOYO-DATE
                                           PIC X(10).
      *                    傷病の部位
                           11  CLAIMS-DISEASE-LOCATION
                                           PIC X(20).
      *                    療養開始日
                           11  CLAIMS-START-DATE
                                           PIC X(10).
      *                自賠責
                       09  CLAIMS-LIABILITY-INSURANCE.
      *                    傷病の部位
                           11  CLAIMS-LI-DISEASE-LOCATION
                                           PIC X(20).
      *                    様式
                           11  CLAIMS-FORM-STYLE PIC X(01).
      *                    療養開始日
                           11  CLAIMS-LI-START-DATE
                                           PIC X(10).
      *                公害
                       09  CLAIMS-POLLUTION-INSURANCE.
      *                    記号
                           11  CLAIMS-POLLUTION-KIGO
                                           PIC X(38).
      *                    番号
                           11  CLAIMS-POLLUTION-NUM
                                           PIC X(38).
      *                地方公費情報
                       09  CLAIMS-LOCAL-PUBLIC-INSURANCE.
      *                    請求年月
                           11  CLAIMS-LOCAL-PUBLIC-SKYYM     PIC X(7).
      *                    返戻年月
                           11  CLAIMS-LOCAL-PUBLIC-HENREIYM  PIC X(7).
                           11  CLAIMS-LOCAL-PUBLIC-O OCCURS  4.
      *                        公費ＩＤ
                               15  CLAIMS-LOCAL-KOHID
                                           PIC X(10).
      *                        公費番号
                               15  CLAIMS-LOCAL-KOHNUM
                                           PIC X(03).
      *                        負担者番号
                               15  CLAIMS-LOCAL-FTNJANUM
                                           PIC X(08).
      *                        受給者番号
                               15  CLAIMS-LOCAL-JKYSNUM
                                           PIC X(20).
      *
      *                返戻情報（紐付け設定済）
                       09  CLAIMS-RETURN-INFORMATION.
      *                    処理年月
                           11  CLAIMS-RE-HENREIYM
                                           PIC X(7).
      *                        提出先
                           11  CLAIMS-RE-TEISYUTUSAKI
                                           PIC X(2).
      *                        提出先（再請求）
                           11  CLAIMS-RE-TEISYUTUSAKI2
                                           PIC X(1).
                           11  CLAIMS-RE-INOUT
                                           PIC X(1).
      *                        種別コード
                           11  CLAIMS-RE-CODE
                                           PIC X(04).
      *                        種別
                           11  CLAIMS-RE-TYPE
                                           PIC X(40).
      *                        保険情報
                           11  CLAIMS-RE-MAIN-INSURANCE.
      *                            保険者番号
                               13  CLAIMS-RE-HKNJANUM
                                           PIC X(08).
      *                            記号
                               13  CLAIMS-RE-KIGO
                                           PIC X(38).
      *                            番号
                               13  CLAIMS-RE-NUM
                                           PIC X(38).
      *                        災害該当の有無
                           11  CLAIMS-RE-ACCIDENT-TARGET
                                           PIC X(4).
      *                        入所中の有無
                           11  CLAIMS-RE-INCARE-CLASS
                                           PIC X(4).
      *                        公費情報
                           11  CLAIMS-RE-PUBLIC-INSURANCE.
                               13  CLAIMS-RE-PUBLIC-INSURANCE-O
                                                             OCCURS  4.
      *                            負担者番号
                                   15  CLAIMS-RE-PROVIDER-NUMBER
                                           PIC X(08).
      *                            受給者番号
                                   15  CLAIMS-RE-RECIPIENT-NUMBER
                                           PIC X(20).
      *                    労災情報
                           11  CLAIMS-RE-ACCIDENT-INSURANCE.
      *                        労災自賠保険区分
                               13  CLAIMS-RE-ACCIDENT
                                           PIC X(1).
      *                        労働保険番号
                               13  CLAIMS-RE-ACCIDENT-NUM
                                           PIC X(14).
      *                        年金証書番号
                               13 CLAIMS-RE-PENSION-NUM
                                           PIC X(09).
      *                        災害区分
                               13  CLAIMS-RE-ACCIDENT-CLASS
                                           PIC X(1).
      *                        返戻事由
                               13  CLAIMS-RE-ERRMSG
                                           PIC X(100).
      *            返戻情報
                   07  CLAIMS-RE-DETAIL-INFORMATION.
                       09  CLAIMS-RE-DET-INFORMATION-G OCCURS  30.
      *???                 返戻設定済
      *???                 11  CLAIMS-RE-DET-HENREI-FLG
      *???                                  PIC X(05).
      *                    処理年月
                           11  CLAIMS-RE-DET-HENREIYM
                                           PIC X(7).
      *                        提出先
                           11  CLAIMS-RE-DET-TEISYUTUSAKI
                                           PIC X(2).
      *                        提出先（再請求）
                           11  CLAIMS-RE-DET-TEISYUTUSAKI2
                                           PIC X(1).
                           11  CLAIMS-RE-DET-INOUT
                                           PIC X(1).
      *                        種別コード
                           11  CLAIMS-RE-DET-CODE
                                           PIC X(04).
      *                        種別
                           11  CLAIMS-RE-DET-TYPE
                                           PIC X(40).
      *                        保険情報
                           11  CLAIMS-RE-DET-MAIN-INSURANCE.
      *                        保険者番号
                               13  CLAIMS-RE-DET-INSURANCE-NUM
                                           PIC X(08).
      *                        記号
                               13  CLAIMS-RE-DET-KIGO
                                           PIC X(38).
      *                         号
                               13  CLAIMS-RE-DET-NUM
                                           PIC X(38).
      *                    災害該当の有無
                           11  CLAIMS-RE-DET-ACCIDENT-TARGET
                                           PIC X(4).
      *                    入所中の有無
                           11  CLAIMS-RE-DET-INCARE-CLASS
                                           PIC X(4).
      *                    公費情報
                           11  CLAIMS-RE-DET-PUBLIC-INS.
                               13  CLAIMS-RE-DET-PUBLIC-INS-O
                                                             OCCURS  4.
      *                            負担者番号
                                   15  CLAIMS-RE-DET-PROVIDER-NUM
                                           PIC X(08).
      *                            受給者番号
                                   15  CLAIMS-RE-DET-RECIPIENT-NUM
                                           PIC X(20).
      *                    労災情報
                           11  CLAIMS-RE-DET-ACCIDENT-INS.
      *                        労災自賠保険区分
                               13  CLAIMS-RE-DET-ACCIDENT
                                           PIC X(1).
      *                        労働保険番号
                               13  CLAIMS-RE-DET-ACCIDENT-NUM
                                           PIC X(14).
      *                        年金証書番号
                               13  CLAIMS-RE-DET-PENSION-NUM
                                           PIC X(09).
      *                        災害区分
                               13  CLAIMS-RE-DET-ACCIDENT-CLASS
                                           PIC X(1).
      *                        返戻事由
                               13  CLAIMS-RE-DET-ERRMSG
                                           PIC X(100).
      *            エラーレセプト情報
                   07  CLAIMS-ERR-INFO-G.
                       09  CLAIMS-ERR-INFORMATION  OCCURS  50.
      *                  エラーコード
                         10  CLAIMS-ERR-ERRCD
                                           PIC X(3).
      *                  提出先
                         10  CLAIMS-ERR-ERRMSG
                                           PIC X(100).
                         10  CLAIMS-ERR-DETAIL-INFORMATION.
      *                    提出先
                           11  CLAIMS-ERR-TEISYUTUSAKI
                                           PIC X(2).
                           11  CLAIMS-ERR-INOUT
                                           PIC X(1).
      *                    種別コード
                           11  CLAIMS-ERR-CODE
                                           PIC X(04).
      *                    種別
                           11  CLAIMS-ERR-TYPE
                                           PIC X(40).
      *                    保険情報
                           11  CLAIMS-ERR-MAIN-INSURANCE.
      *                        保険ＩＤ
                               13  CLAIMS-ERR-HKNID
                                           PIC X(10).
      *                        保険番号
                               13  CLAIMS-ERR-INS-CLASS
                                           PIC X(03).
                               13 CLAIMS-ERR-MAIN-INSURANCE-1.
      *                            保険者番号
                                   15 CLAIMS-ERR-HKNJANUM
                                           PIC X(08).
      *                            記号
                                   15 CLAIMS-ERR-KIGO
                                           PIC X(38).
      *                            番号
                                   15 CLAIMS-ERR-NUM
                                           PIC X(38).
      *                            補助区分
                                   15 CLAIMS-ERR-HOJOKBN
                                           PIC X(01).
      *                        災害該当の有無
                               13 CLAIMS-ERR-ACCIDENT-TARGET
                                           PIC X(4).
      *                        入所中の有無
                               13 CLAIMS-ERR-INCARE-CLASS
                                           PIC X(4).
      *                    公費情報
                           11 CLAIMS-ERR-PUBLIC-INSURANCE.
                               13 CLAIMS-ERR-PUBLIC-INSURANCE-O
                                                           OCCURS  4.
      *                            公費ＩＤ
                                   15 CLAIMS-ERR-KOHID
                                           PIC X(10).
      *                            公費番号
                                   15 CLAIMS-ERR-KOHNUM
                                           PIC X(03).
      *                            負担者番号
                                   15 CLAIMS-ERR-FTNJANUM
                                           PIC X(08).
      *                            受給者番号
                                   15 CLAIMS-ERR-JKYSNUM
                                           PIC X(20).
      *                    労災情報
                           11 CLAIMS-ERR-ACCIDENT-INSURANCE.
      *                        労災自賠保険区分
                               13 CLAIMS-ERR-ACCIDENT
                                           PIC X(01).
      *                        労働保険番号
                               13 CLAIMS-ERR-ACCIDENT-NUM
                                           PIC X(14).
      *                         年金証書番号
                               13 CLAIMS-ERR-PENSION-NUM
                                           PIC X(09).
      *                        災害区分
                               13 CLAIMS-ERR-ACCIDENT-CLASS
                                           PIC X(1).
      *                        健康管理手帳番号
                               13 CLAIMS-ERR-KANRITCHO-NUMBER
                                          PIC X(13).
      *                        損傷区分
                               13 CLAIMS-ERR-DAMAGE-CLASS
                                           PIC X(03).
      *                        療養年月日
                               13 CLAIMS-ERR-RYOYO-DATE
                                           PIC X(10).
      *                        傷病の部位
                               13 CLAIMS-ERR-DISEASE-LOCATION
                                           PIC X(20).
      *                        療養開始日
                               13 CLAIMS-ERR-START-DATE
                                           PIC X(10).
      *                    自賠責
                           11 CLAIMS-ERR-LIABILITY-INS.
      *                        傷病の部位
                               13  CLAIMS-ERR-LI-DISEASE-LOCATION
                                           PIC X(20).
      *                        様式
                               13  CLAIMS-ERR-FORM-STYLE PIC X(01).
      *                        療養開始日
                               13  CLAIMS-ERR-LI-START-DATE
                                           PIC X(10).
      *                    公害
                           11  CLAIMS-ERR-POLLUTION-INS.
      *                        記号
                               13  CLAIMS-ERR-POLLUTION-KIGO
                                           PIC X(38).
      *                        番号
                               13  CLAIMS-ERR-POLLUTION-NUM
                                           PIC X(38).
      *                    地方公費情報
                           11  CLAIMS-ERR-PUBLIC-INSURANCE.
                               13  CLAIMS-ERR-PUBLIC-INSURANCE-O
                                                           OCCURS  4.
      *                            公費ＩＤ
                                   15 CLAIMS-ERR-KOHID
                                           PIC X(10).
      *                            公費番号
                                   15 CLAIMS-ERR-KOHNUM
                                           PIC X(03).
      *                            負担者番号
                                   15 CLAIMS-ERR-FTNJANUM
                                           PIC X(08).
      *                            受給者番号
                                   15 CLAIMS-ERR-JKYSNUM
                                           PIC X(20).
      *                    返戻情報
                           11  CLAIMS-ERR-RET-INFORMATION.
      *                        処理年月
                               13  CLAIMS-ERR-RET-HENREIYM
                                           PIC X(7).
      *                        提出先
                               13  CLAIMS-ERR-RET-TEISYUTUSAKI
                                           PIC X(2).
      *                        提出先（再請求）
                               13 CLAIMS-ERR-RET-TEISYUTUSAKI2
                                           PIC X(1).
                               13 CLAIMS-ERR-RET-INOUT
                                           PIC X(1).
      *                        種別コード
                               13  CLAIMS-ERR-RET-CODE
                                           PIC X(04).
      *                        種別
                               13  CLAIMS-ERR-RET-TYPE
                                           PIC X(40).
      *                        保険情報
                               13  CLAIMS-ERR-RET-MAIN-INSURANCE.
      *                            保険者番号
                                   15  CLAIMS-ERR-RET-HKNJANUM
                                           PIC X(08).
      *                            記号
                                   15  CLAIMS-ERR-RET-KIGO
                                           PIC X(38).
      *                            番号
                                   15  CLAIMS-ERR-RET-NUM
                                           PIC X(38).
      *                        災害該当の有無
                               13  CLAIMS-ERR-RET-ACCIDENT-TARGET
                                           PIC X(4).
      *                        入所中の有無
                               13  CLAIMS-ERR-RET-INCARE-CLASS
                                           PIC X(4).
      *                        公費情報
                               13  CLAIMS-ERR-RET-PUBLIC-INS.
                                   15  CLAIMS-ERR-RET-PUBLIC-INS-O
                                                           OCCURS  4.
      *                                負担者番号
                                       17  CLAIMS-ERR-RET-PROVIDER-NUM
                                           PIC X(08).
      *                                受給者番号
                                       17  CLAIMS-ERR-RET-RECIPIENT-NUM
                                           PIC X(20).
      *                        労災情報
                               13  CLAIMS-ERR-RET-ACCIDENT-INS.
      *                            労災自賠保険区分
                                   15  CLAIMS-ERR-RET-ACCIDENT
                                           PIC X(1).
      *                            労働保険番号
                                   15  CLAIMS-ERR-RET-ACCIDENT-NUM
                                           PIC X(14).
      *                             年金証書番号
                                   15  CLAIMS-ERR-RET-PENSION-NUM
                                           PIC X(09).
      *                            災害区分
                                   15  CLAIMS-ERR-RET-ACCIDENT-CLASS
                                           PIC X(1).
      *                        返戻事由
                               13 CLAIMS-ERR-RET-ERRMSG
                                           PIC X(100).
