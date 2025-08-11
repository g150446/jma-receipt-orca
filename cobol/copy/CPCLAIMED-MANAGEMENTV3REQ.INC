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
      *    ＡＰＩ請求管理情報      (xml2)
      *                        COPY (CPCLAIMED-MANAGEMENTV3REQ.INC)
      ******************************************************************
       01  XML-CLAIMED-MANAGEMENTREQ.
           03  CLAIMQ-CONTEXT         PIC S9(9)   BINARY.
           03  CLAIMQ-OBJECT          PIC X(36).
           03  CLAIMQ-MODE            PIC S9(9)   BINARY.
           03  CLAIMQ-RECORDNAME      PIC X(128).
           03  CLAIMQ-PATIENTREQ.
      *        リクエスト番号
               05  CLAIMQ-REQUEST-NUMBER
                                       PIC X(2).
      *        カルテUID
               05  CLAIMQ-KARTE-UID  PIC X(36).
      *        オルカUID
               05  CLAIMQ-ORCA-UID   PIC X(36).
      *        実施日
               05  CLAIMQ-PERFORM-DATE
                                       PIC X(10).
      *        患者情報
               05  CLAIMQ-PATIENT-INFORMATION.
      *            患者番号
                   07 CLAIMQ-PATIENT-ID  PIC X(20).
      *            診療年月
                   07 CLAIMQ-PERFORM-MONTH
                                           PIC X(7).
      *            請求区分
                   07 CLAIMQ-SKYKBN      PIC X(1).
      *            請求年月
                   07 CLAIMQ-SKYYM       PIC X(7).
      *            返戻年月
                   07 CLAIMQ-HENREIYM    PIC X(7).
      *            請求年月（公費請求分）
                   07 CLAIMQ-KOHSKY-SKYYM  PIC X(7).
      *            返戻年月（公費請求分）
                   07 CLAIMQ-KOHSKY-HENREIYM    PIC X(7).
      *            レセプト情報
                   07 CLAIMQ-DETAIL-INFORMATION-G.
                       09  CLAIMQ-DETAIL-INFORMATION  OCCURS  50.
      *                    提出先
                           11  CLAIMQ-DETAI-TEISYUTUSAKI
                                           PIC X(2).
                           11  CLAIMQ-DETAIL-INOUT
                                           PIC X(1).
      *                    種別コード
                           11  CLAIMQ-DETAIL-CODE
                                           PIC X(04).
      *                    種別
                           11  CLAIMQ-DETAIL-TYPE
                                           PIC X(40).
                           11  CLAIMQ-HKN.
      *                        保険ＩＤ
                               13  CLAIMQ-HKNID
                                           PIC X(10).
      *                        保険番号
                               13  CLAIMQ-HKNNUM
                                           PIC X(03).
      *                    保険情報
                           11  CLAIMQ-MAIN-INSURANCE.
      *                        保険者番号
                               13  CLAIMQ-HKNJANUM
                                           PIC X(08).
      *                        記号
                               13  CLAIMQ-KIGO
                                           PIC X(38).
      *                        番号
                               13  CLAIMQ-NUM
                                           PIC X(38).
      *                        補助区分
                               13  CLAIMQ-HOJOKBN
                                           PIC X(01).
      *                        災害該当の有無
                               13  CLAIMQ-ACCIDENT-TARGET
                                           PIC X(4).
      *                        入所中の有無
                               13  CLAIMQ-INCARE-CLASS
                                           PIC X(4).
      *                    公費情報
                           11  CLAIMQ-PUBLIC-INSURANCE.
                               13  CLAIMQ-PUBLIC-INSURANCE-O  OCCURS  4.
      *                            公費ＩＤ
                                   15  CLAIMQ-KOHID
                                           PIC X(10).
      *                            公費番号
                                   15  CLAIMQ-KOHNUM
                                           PIC X(03).
      *                            負担者番号
                                   15  CLAIMQ-FTNJANUM
                                           PIC X(08).
      *                            受給者番号
                                   15  CLAIMQ-JKYSNUM
                                           PIC X(20).
      *                    労災情報
                           11  CLAIMQ-ACCIDENT-INSURANCE.
      *                        労災自賠保険区分
                               13  CLAIMQ-ACCIDENT
                                           PIC X(01).
      *                        労働保険番号
                               13  CLAIMQ-ACCIDENT-NUM
                                           PIC X(14).
      *                         年金証書番号
                               13  CLAIMQ-PENSION-NUM
                                           PIC X(09).
      *                        災害区分
                               13  CLAIMQ-ACCIDENT-CLASS
                                           PIC X(1).
      *                        健康管理手帳番号
                               13  CLAIMQ-KANRITCHO-NUMBER
                                          PIC X(13).
      *                        損傷区分
                               13  CLAIMQ-DAMAGE-CLASS
                                           PIC X(03).
      *                        療養年月日
                               13  CLAIMQ-RYOYO-DATE
                                           PIC X(10).
      *                        傷病の部位
                               13  CLAIMQ-DISEASE-LOCATION
                                           PIC X(20).
      *                        療養開始日
                               13  CLAIMQ-START-DATE
                                           PIC X(10).
      *                    自賠責
                           11  CLAIMQ-LIABILITY-INSURANCE.
      *                        傷病の部位
                               13  CLAIMQ-LI-DISEASE-LOCATION
                                           PIC X(20).
      *                        様式
                               13  CLAIMQ-FORM-STYLE PIC X(01).
      *                        療養開始日
                               13  CLAIMQ-LI-START-DATE
                                           PIC X(10).
      *                    公害
                           11  CLAIMQ-POLLUTION-INSURANCE.
      *                        記号
                               13  CLAIMQ-POLLUTION-KIGO
                                           PIC X(38).
      *                        番号
                               13  CLAIMQ-POLLUTION-NUM
                                           PIC X(38).
      *                    地方公費情報
                           11  CLAIMQ-LOCAL-PUBLIC-INSURANCE.
                               13  CLAIMQ-LOCAL-PUBLIC-INS-O
                                                           OCCURS  4.
      *                            公費ＩＤ
                                   15  CLAIMQ-LOCAL-KOHID
                                           PIC X(10).
      *                            公費番号
                                   15  CLAIMQ-LOCAL-KOHNUM
                                           PIC X(03).
      *                            負担者番号
                                   15  CLAIMQ-LOCAL-FTNJANUM
                                           PIC X(08).
      *                            受給者番号
                                   15  CLAIMQ-LOCAL-JKYSNUM
                                           PIC X(20).
      *                    返戻情報
                           11  CLAIMQ-RETURN-INFORMATION.
      *                        処理年月
                               13  CLAIMQ-RE-HENREIYM    PIC X(7).
      *                        提出先
                               13  CLAIMQ-RE-TEISYUTUSAKI
                                           PIC X(2).
      *                        提出先（再請求）
                               13  CLAIMQ-RE-TEISYUTUSAKI2
                                           PIC X(1).
                               13  CLAIMQ-DETAILL-RETURN-INOUT
                                           PIC X(1).
      *                        種別コード
                               13  CLAIMQ-RE-CODE
                                           PIC X(04).
      *                        種別
                               13  CLAIMQ-RE-TYPE
                                           PIC X(40).
      *                        保険情報
                               13  CLAIMQ-RE-MAIN-INSURANCE.
      *                            保険者番号
                                   15  CLAIMQ-RE-HKNJANUM
                                           PIC X(08).
      *                            記号
                                   15  CLAIMQ-RE-KIGO
                                           PIC X(38).
      *                            番号
                                   15  CLAIMQ-RE-NUM
                                           PIC X(38).
      *                        災害該当の有無
                               13  CLAIMQ-RE-ACCIDENT-TARGET
                                           PIC X(4).
      *                        入所中の有無
                               13  CLAIMQ-RE-INCARE-CLASS
                                           PIC X(4).
      *                        公費情報
                               13  CLAIMQ-RE-PUBLIC-INSURANCE.
                                   15  CLAIMQ-RE-PUBLIC-INS-O
                                                           OCCURS  4.
      *                              負担者番号
                                     17  CLAIMQ-RE-FTNJANUM
                                           PIC X(08).
      *                              受給者番号
                                     17  CLAIMQ-RE-JKYSNUM
                                           PIC X(20).
      *                        労災情報
                               13  CLAIMQ-RE-ACCIDENT-INSURANCE.
      *                            労災自賠保険区分
                                   15  CLAIMQ-RE-ACCIDENT
                                           PIC X(1).
      *                            労働保険番号
                                   15  CLAIMQ-RE-ACCIDENT-NUM
                                           PIC X(14).
      *                             年金証書番号
                                   15 CLAIMQ-RE-PENSION-NUM
                                           PIC X(09).
      *                            災害区分
                                   15  CLAIMQ-RE-ACCIDENT-CLASS
                                           PIC X(1).
      *
