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
      *****************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI043R1V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 請求管理登録
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/01/01    NMED-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル名称領域 
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *
      *    請求管理スパ領域
           COPY    "H01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "H01SCR-SPA".
      *
      *    設定済返戻データ
           02  SPA-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //SPA-HENREI-B-//.
      *
      *    対象一覧用返戻データ
           02  TBL-HENREI-B-REC    OCCURS  30.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //TBL-HENREI-B-//.
           02  TBL-HENREI-B-ERR    PIC 9(01)   OCCURS  30.
           02  TBL-HENREI-B-MAX    PIC 9(02).
      *
      *    更新対象返戻データ
           02  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //WRK-HENREI-B-//.
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-KOHSKY              PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SEIKYU              PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-DETAIL-CHK          PIC 9(01).
           03  FLG-RECEIPT-CHK         PIC 9(01).
      *
           03  FLG-API-PARA-FLG        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDVV                    PIC 9(04).
           03  IDWW                    PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
           03  WRK-HENYMDG             PIC X(09).
           03  WRK-HENYMDG3            PIC X(22).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-WARNING-INFO.
               05  WRK-WARNING-OCC         OCCURS 5.
                   07  WRK-WARNING     PIC X(03).
                   07  WRK-WARNING-MSG PIC X(100).
               05  WRK-WARNING-MAX     PIC 9(02).
      *
           03  WRK-TEMPDIR-ROOT        PIC X(1024).
      *
           03  WRK-SPA-TERMID          PIC X(64).
      *
      *    オルカUID
           03  WRK-CLAIMQ-ORCA-UID     PIC X(36).
           03  WRK-ORCA-UID            PIC X(36).
           03  WRK-OPID                PIC X(16).
           03  WRK-MCP-WINDOW          PIC X(62).
           03  WRK-MCP-TERM            PIC X(64).
      *
           03  WRK-H99-TBL.
      *        対象となるレセプト件数
               05  WRK-H99-DET-OCC     OCCURS  50.
                   07  WRK-H99-CNT         PIC 9(02).
                   07  WRK-H99-INDEX       PIC 9(02).
      *
       01  WRK-RECEIPT-MSG-AREA.
           03  WRK-RECEIPT-ERRCD       PIC X(03).
           03  WRK-RECEIPT-ERRMSG      PIC X(100).
           03  WRK-RECEIPT-WARNING     PIC X(03).
           03  WRK-RECEIPT-WARNING-MSG PIC X(100).
      *
       01  WRK-RECEIPT-MAX             PIC 9(03).
      *
       01  WRK-XML-AREA.
           03  WRK-DETAIL-AREA.
               05  WRK-PTNUM           PIC X(20).
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SKYKBN          PIC X(01).
               05  WRK-SKYYM-X.
                   07  WRK-SKYYM           PIC 9(06).
               05  WRK-HENREIYM-X.
                   07  WRK-HENREIYM        PIC 9(06).
               05  WRK-KOHSKY-SKYYM-X.
                   07  WRK-KOHSKY-SKYYM    PIC 9(06).
               05  WRK-KOHSKY-HENREIYM-X.
                   07  WRK-KOHSKY-HENREIYM PIC 9(06).
               05  WRK-DETAIL-OCC              OCCURS  50.
                   07  WRK-TEISYUTUSAKI    PIC 9(01).
                   07  WRK-NYUGAIKBN       PIC X(01).
                   07  WRK-RECESYUBETU     PIC X(04).
                   07  WRK-HKNID           PIC 9(10).
                   07  WRK-HKNNUM          PIC X(03).
                   07  WRK-HKNJANUM        PIC X(08).
                   07  WRK-HOJOKBN         PIC X(01).
                   07  WRK-KIGO            PIC X(40).
                   07  WRK-NUM             PIC X(40).
                   07  WRK-ACCIDENT-TARGET PIC X(04).
                   07  WRK-INCARE-CLASS    PIC X(04).
                   07  WRK-KOH-OCC             OCCURS  4.
                       09  WRK-KOHID       PIC 9(10).
                       09  WRK-KOHNUM      PIC X(03).
                       09  WRK-FTNJANUM    PIC X(08).
                       09  WRK-JKYSNUM     PIC X(20).
      *
                   07  WRK-ACCIDENT        PIC X(01).
                   07  WRK-ACCIDENT-NUM    PIC X(14).
                   07  WRK-ACCIDENT-CLASS  PIC X(01).
                   07  WRK-KANRITCHO-NUMBER
                                           PIC X(13).
                   07  WRK-DAMAGE-CLASS    PIC X(03).
                   07  WRK-RYOYO-DATE      PIC X(09).
                   07  WRK-DISEASE-LOCATION
                                           PIC X(20).
                   07  WRK-START-DATE      PIC X(09).
      *
                   07  WRK-LI-DISEASE-LOCATION
                                           PIC X(20).
                   07  WRK-FORM-STYLE      PIC X(01).
                   07  WRK-LI-START-DATE   PIC X(09).
      *
                   07  WRK-POLLUTION-KIGO  PIC X(40).
                   07  WRK-POLLUTION-NUM   PIC X(40).
      *
                   07  WRK-LOCAL-KOH-INF.
                       09  WRK-LOCAL-KOH-OCC       OCCURS  4.
                           11  WRK-LOCAL-KOHID     PIC 9(10).
                           11  WRK-LOCAL-KOHNUM    PIC X(03).
                           11  WRK-LOCAL-FTNJANUM  PIC X(08).
                           11  WRK-LOCAL-JKYSNUM   PIC X(20).
      *
      *    共通パラメタ内容
       01  WRK-APIV3DATA-AREA.
           03  WRK-APIV3DATA-REC.
               05  WRK-CLAIMQ-PATIENT-ID
                                       PIC X(20).
               05  WRK-CLAIMQ-PERFORM-MONTH
                                       PIC X(7).
               05  WRK-CLAIMQ-SKYKBN   PIC X(1).
               05  WRK-CLAIMQ-SKYYM    PIC X(7).
               05  WRK-CLAIMQ-HENREIYM PIC X(7).
               05  WRK-CLAIMQ-KOHSKY-SKYYM
                                       PIC X(7).
               05  WRK-CLAIMQ-KOHSKY-HENREIYM
                                      PIC X(7).
               05  WRK-CLAIMQ-SKYYM-9 PIC X(06).
               05  WRK-CLAIMQ-HENREIYM-9
                                      PIC X(06). 
               05  WRK-CLAIMQ-KOHSKY-SKYYM-9
                                      PIC X(06).
               05  WRK-CLAIMQ-KOHSKY-HENREIYM-9
                                     PIC X(06).
               05  WRK-CLAIMQ-DETAIL-INFORMATION-G.
                   07  WRK-CLAIMQ-DETAIL-INFORMATION  OCCURS  50.
                       09  WRK-CLAIMQ-DETAI-TEISYUTUSAKI
                                           PIC X(2).
                       09  WRK-CLAIMQ-DETAIL-INOUT
                                           PIC X(1).
                       09  WRK-CLAIMQ-DETAIL-CODE
                                           PIC X(04).
                       09  WRK-CLAIMQ-HKN.
                           11  WRK-CLAIMQ-HKNID
                                           PIC X(10).
                           11  WRK-CLAIMQ-HKNNUM
                                           PIC X(03).
                           11  WRK-CLAIMQ-HKNJANUM
                                           PIC X(08).
                           11  WRK-CLAIMQ-HOJOKBN
                                        PIC X(01).
                           11  WRK-CLAIMQ-ACCIDENT-TARGET
                                           PIC X(4).
                           11  WRK-CLAIMQ-INCARE-CLASS
                                           PIC X(4).
           03  WRK-APIV3DATA-REC1.
               05  WRK-CLAIMQ-DETAIL-INF-G1.
                   07  WRK-CLAIMQ-DETAIL-INF-1         OCCURS  50.
                       09  WRK-CLAIMQ-PUBLIC.
                           11  WRK-CLAIMQ-PUBLIC-O         OCCURS  4.
                               13  WRK-CLAIMQ-KOHID
                                           PIC X(10).
                               13  WRK-CLAIMQ-KOHNUM
                                           PIC X(03).
                               13 WRK-CLAIMQ-FTNJANUM
                                           PIC X(08).
           03  WRK-APIV3DATA-REC2.
               05  WRK-CLAIMQ-DETAIL-INF-G2.
                   07  WRK-CLAIMQ-DETAIL-INF-2         OCCURS  50.
                       09  WRK-CLAIMQ-LOCAL-PUBLIC.
                           11  WRK-CLAIMQ-LOCAL-PUBLIC-O   OCCURS  4.
                               13  WRK-CLAIMQ-LOCAL-KOHID
                                           PIC X(10).
                               13  WRK-CLAIMQ-LOCAL-KOHNUM
                                           PIC X(03).
           03  WRK-APIV3DATA-REC3.
               05  WRK-CLAIMQ-DETAIL-INF-G3.
                   07  WRK-CLAIMQ-DETAIL-INF-3         OCCURS  50.
                       09  WRK-CLAIMQ-ACCIDENT-INSURANCE.
                           11  WRK-CLAIMQ-ACCIDENT
                                           PIC X(01).
                           11  WRK-CLAIMQ-ACCIDENT-NUM
                                           PIC X(14).
                           11  WRK-CLAIMQ-PENSION-NUM
                                           PIC X(09).
                           11  WRK-CLAIMQ-ACCIDENT-CLASS
                                           PIC X(1).
                           11  WRK-CLAIMQ-KANRITCHO-NUMBER
                                          PIC X(13).
                           11  WRK-CLAIMQ-RYOYO-DATE
                                           PIC X(10).
                           11  WRK-CLAIMQ-START-DATE
                                           PIC X(10).
                       09  WRK-CLAIMQ-LIABILITY-INSURANCE.
                           11  WRK-CLAIMQ-FORM-STYLE PIC X(01).
                           11  WRK-CLAIMQ-LI-START-DATE
                                           PIC X(10).
           03  WRK-APIV3DATA-REC4.
               05  WRK-CLAIMQ-DETAIL-INF-G4.
                   07  WRK-CLAIMQ-DETAIL-INF-4         OCCURS  50.
                       09  WRK-CLAIMQ-POLLUTION-INSURANCE.
                           11  WRK-CLAIMQ-POLLUTION-KIGO
                                           PIC X(38).
                           11  WRK-CLAIMQ-POLLUTION-NUM
                                           PIC X(38).
           03  WRK-APIV3DATA-REC5.
               05  WRK-CLAIMQ-DETAIL-INF-G5.
                   07  WRK-CLAIMQ-DETAIL-INF-5         OCCURS  50.
                       09  WRK-SPA-H99-KEY.
                           11  WRK-SPA-H99-HOSPNUM      PIC  9(02).
                           11  WRK-SPA-H99-SRYYM        PIC  9(06).
                           11  WRK-SPA-H99-NYUGAIKBN    PIC  X(01).
                           11  WRK-SPA-H99-PTID         PIC  9(10).
                           11  WRK-SPA-H99-RECEKA       PIC  X(02).
                           11  WRK-SPA-H99-TEISYUTUSAKI PIC  9(01).
                           11  WRK-SPA-H99-RECESYUBETU  PIC  X(04).
                           11  WRK-SPA-H99-HKNJANUM     PIC  X(08).
                           11  WRK-SPA-H99-HOJOKBN      PIC  X(01).
                           11  WRK-SPA-H99-KOHID-KEY    PIC  9(10).
                           11  WRK-SPA-H99-TEKSTYMD     PIC  X(08).
                           11  WRK-SPA-H99-RECEIPT-KBN  PIC  X(10).
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *    APIパラメタ退避用
       01  WRK-APIPARA-REC.
           COPY    "CPAPIPARA.INC" REPLACING  //APIPARA-//
                                   BY         //WRK-APIPARA-//.
      *    画面退避用
           COPY    "MCPAREA"       REPLACING  //MCP//
                                   BY         //TAIHI-MCP//.
      *
           COPY    "COMMON-SPA"    REPLACING  //SPA//
                                   BY         //TAIHI-SPA//.
      *
           COPY    "ORCA-SPA"      REPLACING  //SPA//
                                   BY         //TAIHI-SPA//.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "044".
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID1
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-JOB-JOBID1 PIC 9(07)   VALUE   ZERO.
      *
           03  WRK-CONS-DETAIL-MAX PIC 9(03)   VALUE   50.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    API XML読み込み用定義体
           COPY    "CPCLAIMED-MANAGEMENTV3REQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPCLAIMED-MANAGEMENTV3RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPDATA2.INC".
      *
           COPY    "CPORCSTEMPDIR.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK3003.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC".
      *
      *    労災患者保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA43SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "**********************************"
           DISPLAY   "*    claimed_management start    *"
           DISPLAY   "**********************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *****INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      SPA-H01FREE
                                           SPA-H01KYOUTU
      *
           MOVE    MCPAREA             TO  TAIHI-MCPAREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "999"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************************"
           DISPLAY   "*     claimed_management end     *"
           DISPLAY   "**********************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-CLAIMED-MANAGEMENTRES
           INITIALIZE                  WRK-XML-AREA
           INITIALIZE                  WRK-APIV3DATA-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "claimed_management_Info"
                                      TO  CLAIMS-RESKEY
      *
      ***  STRING  "A+"                DELIMITED  BY  SIZE
      ***          MCP-USER            DELIMITED  BY  SIZE
      ***                              INTO    WRK-OPID
      ***  END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  CLAIMS-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  CLAIMS-INFORMATION-TIME
      *
      *    医療機関識別番号セット
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "999"               TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "E89"                TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  CLAIMS-API-RESULT
           INITIALIZE                      WRK-ERRCD
                                           WRK-ERRMSG
      *                                    WRK-WARNING-INFO
      *
           MOVE    CLAIMQ-ORCA-UID TO  WRK-CLAIMQ-ORCA-UID
      *    オルカＵＩＤ
           IF    ( CLAIMQ-ORCA-UID NOT =   SPACE )
           AND   ( CLAIMQ-REQUEST-NUMBER
                                       =   "01"  )
               PERFORM 2009-PARA-INIT-SEC
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    CLAIMQ-REQUEST-NUMBER
               WHEN    "00"
      *            処理実施
                   MOVE    9                   TO  FLG-SYORI
               WHEN    "01"
      *            処理実施
                   MOVE    1                   TO  FLG-SYORI
               WHEN    "02"
      *            処理確認
                   MOVE    2                   TO  FLG-SYORI
               WHEN    OTHER
                   MOVE    "E91"               TO  WRK-ERRCD
           END-EVALUATE
      *
      *    入力項目チェック処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
           AND   ( SPA-ERRCD           =   SPACE )
               CONTINUE
           ELSE
               IF      CLAIMQ-REQUEST-NUMBER   =   "01"
      *            APIパラメタ削除処理
                   PERFORM 2902-PARA-ALLDEL-SEC
      *            排他制御解除処理
                   PERFORM 990-LOCK-OUT-SEC
               END-IF
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE    CLAIMQ-REQUEST-NUMBER
               WHEN    "01"
                   PERFORM 2002-CLAIMED-CHECK-SEC
               WHEN    "02"
                   PERFORM 2003-CLAIMED-UPDATE-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           INITIALIZE                  XML-APIREQ
           INITIALIZE                  XML-CLAIMED-MANAGEMENTREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_claimed_managementv3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "claimed_managementv3req"
                                       TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-CLAIMED-MANAGEMENTREQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "01"    TO  CLAIMS-RESPONS-NUMBER
                   WHEN    OTHER
                       MOVE    "02"    TO  CLAIMS-RESPONS-NUMBER
               END-EVALUATE
       *
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理確認終了"
                                       TO  CLAIMS-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理実施終了"
                                       TO  CLAIMS-API-RESULT-MSG
                   WHEN    9
                       MOVE    "情報取得終了"
                                       TO  CLAIMS-API-RESULT-MSG
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "00"    TO  CLAIMS-RESPONS-NUMBER
                   WHEN    OTHER
                       MOVE    "01"    TO  CLAIMS-RESPONS-NUMBER
               END-EVALUATE
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  CLAIMS-API-RESULT
               MOVE    WRK-ERRMSG      TO  CLAIMS-API-RESULT-MSG
           END-IF
      *
      *    警告メッセージ
           IF      WRK-WARNING (1) NOT =   SPACE
               MOVE    WRK-WARNING (1) TO  CLAIMS-API-WARNING     (1)
               MOVE    WRK-WARNING-MSG (1)
                                       TO  CLAIMS-API-WARNING-MSG (1)
           END-IF
      *
           MOVE    CLAIMQ-KARTE-UID    TO  CLAIMS-KARTE-UID
           IF      FLG-SYORI               =   9
               CONTINUE
           ELSE
               IF      FLG-SYORI               =   1
                   IF    ( WRK-ERRCD               =   SPACE )
                       MOVE    WRK-ORCA-UID        TO  CLAIMS-ORCA-UID
                   ELSE
                       MOVE    WRK-CLAIMQ-ORCA-UID TO  CLAIMS-ORCA-UID
                   END-IF
               ELSE
                   MOVE    CLAIMQ-ORCA-UID         TO  CLAIMS-ORCA-UID
               END-IF
           END-IF
           MOVE    CLAIMQ-REQUEST-NUMBER
                                       TO  CLAIMS-REQUEST-NUMBER
           MOVE    CLAIMQ-PATIENT-ID   TO  CLAIMS-PATIENT-ID
           MOVE    SPA-H01NAI-NAME     TO  CLAIMS-NAME
           MOVE    CLAIMQ-PERFORM-MONTH
                                       TO  CLAIMS-PERFORM-MONTH
           IF      FLG-SYORI           =   9
               CONTINUE
           ELSE
               MOVE    CLAIMQ-SKYKBN       TO  CLAIMS-SKYKBN
               MOVE    CLAIMQ-SKYYM        TO  CLAIMS-SKYYM
               MOVE    CLAIMQ-HENREIYM     TO  CLAIMS-HENREIYM
               MOVE    CLAIMQ-KOHSKY-SKYYM TO  CLAIMS-KOHSKY-SKYYM
               MOVE    CLAIMQ-KOHSKY-HENREIYM
                                           TO  CLAIMS-KOHSKY-HENREIYM
           END-IF
      *
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_claimed_managementv3res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  CLAIMS-MODE
           MOVE    "claimed_managementv3res"
                                       TO  CLAIMS-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-CLAIMED-MANAGEMENTRES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  CLAIMS-OBJECT        TO  APILST-BODY
               MOVE  "application/xml"     TO  APILST-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
               DISPLAY "accept xml write failure "
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ORCA-UID 初期処理
      *****************************************************************
       2009-PARA-INIT-SEC          SECTION.
      *
      *    UIDコードをTERMID
           MOVE    CLAIMQ-ORCA-UID     TO  WRK-ORCA-UID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"              DELIMITED   BY  SIZE
                   WRK-ORCA-UID        DELIMITED   BY  SPACE
                                       INTO        SPA-TERMID
           END-STRING
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP43"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API43R1V3"         TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           PERFORM 900-APIPARA-READ-SEC
      *
           IF      FLG-APIPARA         =   ZERO
               MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
               IF      CLAIMQ-KARTE-UID    NOT =   APIPARA-KARTE-UID
                   MOVE   1                    TO  FLG-APIPARA
               END-IF
           END-IF
      *
           IF      FLG-APIPARA         =   ZERO
      *        排他解除
               PERFORM 990-LOCK-OUT-SEC
           ELSE
      *        APIパラメタ削除処理
               PERFORM 2902-PARA-ALLDEL-SEC
               MOVE    SPACE           TO  CLAIMQ-ORCA-UID
           END-IF
      *
           .
       2009-PARA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    カルテUID
           IF      CLAIMQ-KARTE-UID  =   SPACE
               MOVE    "E04"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *   オルカUID
           IF      CLAIMQ-REQUEST-NUMBER
                                       =   "00"    OR  "01"
               CONTINUE
           ELSE
               IF      CLAIMQ-ORCA-UID    =   SPACE
                   MOVE    "E05"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    ジョブ管理チェック処理（レセプト作成処理中）
      **???IF      CLAIMQ-REQUEST-NUMBER
      **???                            =   "00"
      **???    CONTINUE
      **???ELSE
      **???    PERFORM 800-JOBKANRI-CHK-SEC
      **???    IF      WRK-ERRCD       NOT =   SPACE
      **???        GO  TO  2001-INPUT-CHK-EXT
      **???    END-IF
      **???END-IF
      *
           IF      CLAIMQ-REQUEST-NUMBER
                                       =   "02"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
          INITIALIZE                   WRK-APIV3DATA-AREA
      *
      *    初画面情報
           MOVE    SPACE               TO  LINKAREA
                                           SPA-MOTOPG
           INITIALIZE                      SPA-H01KYOUTU
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           PERFORM 800-ORCGH01-CALL-SEC
      *
      *    診療年月
           IF      SPA-ERRCD           =   SPACE
               IF      CLAIMQ-PERFORM-MONTH
                                           =   SPACE
                   MOVE    "E14"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               ELSE
                   MOVE    CLAIMQ-PERFORM-MONTH
                                               TO  WRK-HEN-DATE
                   PERFORM 802-DAYHEN02-SEC
                   MOVE    WRK-SYMD (1:6)      TO  H01-SRYYM
                                                   WRK-SRYYM
      *
                   MOVE    SPACE               TO  MCP-STATUS
                   MOVE    "ACTIVATE"          TO  MCP-EVENT
                   MOVE    "SRYYM"             TO  MCP-WIDGET
                   PERFORM 800-ORCGH01-CALL-SEC
               END-IF
           END-IF
      *
      *    患者番号
           IF      SPA-ERRCD           =   SPACE
               IF      CLAIMQ-PATIENT-ID   =   SPACE
                   MOVE    "E01"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               ELSE
                   MOVE    CLAIMQ-KARTE-UID    TO  SPA-H01-JOB-KARTE-UID
                   MOVE    CLAIMQ-ORCA-UID     TO  SPA-H01-JOB-ORCA-UID
                   MOVE    CLAIMQ-PATIENT-ID   TO  H01-PTNUM
                                                   WRK-PTNUM
      *
                   MOVE    SPACE               TO  MCP-STATUS
                   MOVE    "ACTIVATE"          TO  MCP-EVENT
                   MOVE    "PTNUM"             TO  MCP-WIDGET
                   PERFORM 800-ORCGH01-CALL-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               EVALUATE    SPA-ERRCD
                   WHEN    "0002"
                       MOVE    "E11"               TO  WRK-ERRCD
                   WHEN    "0003"
                       MOVE    "E15"               TO  WRK-ERRCD
                   WHEN    "0004"
                       MOVE    "E10"               TO  WRK-ERRCD
                   WHEN    "9999"
                       MOVE    "E90"               TO  WRK-ERRCD
                   WHEN    OTHER
                       MOVE    "E60"               TO  WRK-ERRCD
               END-EVALUATE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    地方公費情報の取得処理
           PERFORM 20010-LOCAL-PUBLIC-HENSYU-SEC
      *
           IF      CLAIMQ-REQUEST-NUMBER
                                       =   "00"
      *        レセプト情報一覧の返却処理
               PERFORM 20010-DETAIL-INF-HENSYU-SEC
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    請求区分
           EVALUATE    CLAIMQ-SKYKBN
               WHEN    "0"
               WHEN    "1"
               WHEN    "2"
                   MOVE    CLAIMQ-SKYKBN   TO  WRK-SKYKBN
               WHEN    OTHER
                   MOVE    "E16"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
           END-EVALUATE
      *
      *    請求年月
           IF      CLAIMQ-SKYYM        =   SPACE
               MOVE    ZERO                TO  WRK-SKYYM
           ELSE
               MOVE    CLAIMQ-SKYYM        TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-SYMD (1:6)      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    WRK-SYMD (1:6)  TO  WRK-SKYYM-X
                   IF      WRK-SKYYM           <   WRK-SRYYM
                       MOVE    "E21"               TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "E25"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    返戻年月
           IF      CLAIMQ-HENREIYM     =   SPACE
               MOVE    ZERO                TO  WRK-HENREIYM
           ELSE
               MOVE    CLAIMQ-HENREIYM     TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-SYMD (1:6)      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    WRK-SYMD (1:6)      TO  WRK-HENREIYM
                   IF      WRK-HENREIYM        <   WRK-SRYYM
                       MOVE    "E22"               TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "E17"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    請求年月（公費請求分）
           IF      CLAIMQ-KOHSKY-SKYYM =   SPACE
               MOVE    ZERO                TO  WRK-KOHSKY-SKYYM
           ELSE
               MOVE    CLAIMQ-KOHSKY-SKYYM TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-SYMD (1:6)      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    WRK-SYMD (1:6)      TO  WRK-KOHSKY-SKYYM
                   IF      WRK-KOHSKY-SKYYM    <   WRK-SRYYM
                       MOVE    "E23"               TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "E18"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    返戻年月（公費請求分）
           IF      CLAIMQ-KOHSKY-HENREIYM
                                       =   SPACE
               MOVE    ZERO                TO  WRK-KOHSKY-HENREIYM
           ELSE
               MOVE    CLAIMQ-KOHSKY-HENREIYM
                                           TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-SYMD (1:6)      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    WRK-SYMD (1:6)      TO  WRK-KOHSKY-HENREIYM
                   IF      WRK-KOHSKY-HENREIYM
                                               <   WRK-SRYYM
                       MOVE    "E24"               TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "E19"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           IF      CLAIMQ-DETAIL-INFORMATION-G
                                       =   SPACE
      *        レセプト情報が未設定のとき
               MOVE    "E20"               TO  WRK-ERRCD
           ELSE
      *        レセプト情報チェック処理
               PERFORM 20011-RECEIPT-CHK-SEC
               IF      WRK-RECEIPT-MAX     >   ZERO
                   MOVE    "E69"               TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費情報の取得処理
      *****************************************************************
       20010-LOCAL-PUBLIC-HENSYU-SEC   SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
                   OR      SPA-H99NAI-RECE10-HOSPNUM (IDX)
                               =       ZERO
              IF    ( SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                                       =   "6666"              )
              OR    ( SPA-H99NAI-RECE10-TEISYUTUSAKI (IDX)
                                       =   "5" OR  "3" OR  "4" )
                   CONTINUE
              ELSE
      *            地方公費情報の取得処理
                   MOVE    SPACE           TO  KOHSKY-REC
                   INITIALIZE                  KOHSKY-REC
                   MOVE    SPA-H99NAI-RECE10-KEY (IDX)
                                           TO  KOHSKY-KEY
                   PERFORM 900-KOHSKY-READ-SEC
                   IF      FLG-KOHSKY      =   ZERO
                       MOVE    MCPDATA-REC     TO  KOHSKY-REC
                       MOVE    KOHSKY-SKYYM    TO
                                   SPA-H99NAI-LOCAL-SKYYM   (IDX)
                       MOVE    KOHSKY-HENREIYM TO
                                   SPA-H99NAI-LOCAL-HENREIYM(IDX)
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >       4
                               OR      KOHSKY-KOHID (IDY)  =   ZERO
                           MOVE    KOHSKY-KOHID       (IDY)
                                               TO
                                   SPA-H99NAI-LOCAL-KOHID      (IDX IDY)
                           MOVE    KOHSKY-KOHNUM      (IDY)
                                               TO
                                   SPA-H99NAI-LOCAL-KOHNUM     (IDX IDY)
                           MOVE    KOHSKY-KOHFTNJANUM (IDY)
                                               TO
                                   SPA-H99NAI-LOCAL-FTNJANUM   (IDX IDY)
                           MOVE    KOHSKY-KOHJKYSNUM  (IDY)
                                               TO
                                   SPA-H99NAI-LOCAL-JKYSNUM    (IDX IDY)
                       END-PERFORM
                   END-IF
               END-IF
      *
              IF       SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                                           =   "7021"
      *            アフターケア情報の取得処理
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    SPA-H99NAI-RECE10-HOSPNUM (IDX)
                                               TO  PTRSI-HOSPNUM
                   MOVE    SPA-H99NAI-RECE10-PTID    (IDX)
                                               TO  PTRSI-PTID
                   MOVE    SPA-H99NAI-RECE10-HKNID   (IDX)
                                               TO  PTRSI-HKNID
                   PERFORM 900-PTRSIINF-READ-SEC
                   IF      FLG-PTRSIINF        =   ZERO
                       MOVE    PTRSI-SONSHOUKBN    TO
                               SPA-H99NAI-RECE10-DAMAGE-CLASS (IDX)(1:2)
                       MOVE    PTRSI-SONSHOUKBN-EDANUM
                                                   TO
                               SPA-H99NAI-RECE10-DAMAGE-CLASS (IDX)(3:1)
                       MOVE    PTRSI-COMMENT       TO
                               SPA-H99NAI-RECE10-COMMENT (IDX)
                   END-IF
               END-IF
           END-PERFORM
           .
       20010-LOCAL-PUBLIC-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト情報一覧の返却処理
      *****************************************************************
       20010-DETAIL-INF-HENSYU-SEC     SECTION.
      *
          IF      SPA-H01NAI-HENREI-MSG  =   1
               MOVE    "W01"                  TO  WRK-WARNING     (1)
               MOVE    "返戻データあり"       TO  WRK-WARNING-MSG (1)
           END-IF
      *     
           INITIALIZE                  CLAIMS-RECEIPT-INFORMATION
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
                   OR      SPA-H99NAI-RECE10-HOSPNUM (IDX)
                               =       ZERO
      *        点検レセ０点公費あり
               IF      SPA-H99NAI-SKYKBN-RECEDEN  (IDX)
                                       =   "1"
                   MOVE    "E35"           TO
                                           CLAIMS-RECEIPT-ERRCD  (IDX)
                   MOVE
                   "０点公費記載のレセプトです。"
                                           TO
                                           CLAIMS-RECEIPT-ERR-MSG(IDX)
               END-IF
      *
      *        主科設定時の主科未設定のチェック（特別療養費を除く）
               IF    ( CLAIMS-RECEIPT-ERRCD  (IDX)
                                       =   SPACE  )
               AND   ( SPA-H99NAI-RECE10-RECEKA (IDX)
                                       =   "00"   )
               AND   ( SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                                   NOT =   "6666" )
                   EVALUATE    SPA-H99NAI-RECE10-TEISYUTUSAKI (IDX)
                       WHEN    "1"
                           IF     SPA-NAI-2009-SYUKA-SYAHO  =   "1"
                               MOVE    "E36"          TO
                                           CLAIMS-RECEIPT-ERRCD  (IDX)
                               MOVE    "主科未設定です。"
                                           TO
                                           CLAIMS-RECEIPT-ERR-MSG(IDX)
                           END-IF
                       WHEN    "2"
                           IF      SPA-NAI-2009-SYUKA-KOKUHO =   "1"
                               MOVE    "E36"          TO
                                           CLAIMS-RECEIPT-ERRCD  (IDX)
                               MOVE    "主科未設定です。"
                                           TO
                                           CLAIMS-RECEIPT-ERR-MSG(IDX)
                           END-IF
                       WHEN    "6"
                           IF      SPA-NAI-2009-SYUKA-KOUIKI =   "1"
                               MOVE    "E36"          TO
                                           CLAIMS-RECEIPT-ERRCD  (IDX)
                               MOVE    "主科未設定です。"
                                           TO
                                           CLAIMS-RECEIPT-ERR-MSG(IDX)
                           END-IF
                   END-EVALUATE
               END-IF
      *
      *        レセ電データ存在チェック処理
               IF      CLAIMS-RECEIPT-ERRCD  (IDX)
                                            =   SPACE
                   IF      SPA-H99NAI-RECEDEN-FLG (IDX)    =   1
                       MOVE    "E37"          TO
                                           CLAIMS-RECEIPT-ERRCD  (IDX)
                       MOVE    "レセ電データがありません。"
                                           TO
                                           CLAIMS-RECEIPT-ERR-MSG(IDX)
                   END-IF
               END-IF
      *
      *        返戻データ紐付けチェック処理
               IF      CLAIMS-RECEIPT-ERRCD  (IDX)
                                            =   SPACE
                   IF      SPA-H99NAI-RECE10-HENREI-SJ-FLG (IDX)
                                       =   1
                       MOVE    "E3t"       TO
                                           CLAIMS-RECEIPT-ERRCD  (IDX)
                       MOVE    "返戻データが紐付けされています。"
                                           TO
                                           CLAIMS-RECEIPT-ERR-MSG(IDX)
                   END-IF
               END-IF
      *
               EVALUATE    SPA-H99NAI-RECE10-TEISYUTUSAKI (IDX)
                   WHEN    "1"
                       MOVE    "02"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "2"
                       MOVE    "03"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "3"
                       MOVE    "05"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "4"
                       MOVE    "06"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "5"
                       MOVE    "09"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "6"
                       MOVE    "04"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
               END-EVALUATE
      *
               EVALUATE    SPA-H99NAI-RECE10-TEISYUTUSAKI (IDX)
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "6"
                       IF      SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                                           =   "6666"
                           MOVE    SPA-H99GMN-TRECESYUBETU (IDX)
                                       TO  CLAIMS-DETAIL-TYPE (IDX)
                       ELSE 
                           MOVE    SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                                       TO  CLAIMS-DETAIL-CODE (IDX)
                           MOVE    SPA-H99GMN-TRECESYUBETU (IDX) (5:)
                                       TO  CLAIMS-DETAIL-TYPE (IDX)
                       END-IF 
                   WHEN    OTHER
                       MOVE    SPA-H99GMN-TRECESYUBETU (IDX)
                                       TO  CLAIMS-DETAIL-TYPE (IDX)
               END-EVALUATE
      *
               EVALUATE    SPA-H99NAI-RECE10-NYUGAIKBN (IDX)
                   WHEN    "1"
                       MOVE    "I"     TO  CLAIMS-DETAIL-INOUT (IDX)
                   WHEN    "2"
                       MOVE    "O"     TO  CLAIMS-DETAIL-INOUT (IDX)
               END-EVALUATE
      *
               MOVE    SPA-H99NAI-RECE10-SKYKBN (IDX)
                                       TO  CLAIMS-DETAIL-SKYKBN (IDX)
      *
      *        レセプト情報
               MOVE    SPA-H99NAI-RECE10-SKYYM (IDX)
                                       TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE (1:7)
                                       TO  CLAIMS-RECEIPT-SKYYM  (IDX)
               MOVE    SPA-H99NAI-RECE10-HENREIYM (IDX)
                                       TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE (1:7)
                                       TO
                                       CLAIMS-RECEIPT-HENREIYM  (IDX)
      *
               IF      SPA-H99NAI-RECE10-HKNNUM (IDX)
                                   NOT =   SPACE
                   MOVE    SPA-H99NAI-RECE10-HKNID  (IDX)
                                           TO  CLAIMS-HKNID   (IDX)
                   MOVE    SPA-H99NAI-RECE10-HKNNUM (IDX)
                                           TO  CLAIMS-HKNNUM  (IDX)
               END-IF
      *
               EVALUATE    SPA-H99NAI-RECE10-TEISYUTUSAKI (IDX)
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "6"
                       MOVE    SPA-H99GMN-THKNJANUM     (IDX)
                                              TO  CLAIMS-HKNJANUM(IDX)
                       MOVE    SPA-H99NAI-RECE10-KIGO   (IDX)
                                              TO  CLAIMS-KIGO    (IDX)
                       MOVE    SPA-H99NAI-RECE10-NUM    (IDX)
                                              TO  CLAIMS-NUM     (IDX)
                       IF      SPA-H99NAI-RECE10-HOJOKBN (IDX)
                                          NOT =   "0"
                           MOVE    SPA-H99NAI-RECE10-HOJOKBN (IDX)
                                              TO  CLAIMS-HOJOKBN (IDX)
                       END-IF
      *
                       EVALUATE  SPA-H99NAI-RECE10-RECEIPT-KBN(IDX)(1:1)
                           WHEN    "1"
                           WHEN    "2"
                           WHEN    "3"
                           WHEN    "4"
                               MOVE    "True"          TO
                                           CLAIMS-INCARE-CLASS  (IDX)
                       END-EVALUATE
                       IF      SPA-H99GMN-TDISASTER (IDX) NOT =   SPACE
                               MOVE    "True"          TO
                                           CLAIMS-ACCIDENT-TARGET(IDX)
                       END-IF
      *
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >       4
                               OR      SPA-H99NAI-RECE10-KOHID(IDX IDY)
                                               =   ZERO
                           MOVE    SPA-H99NAI-RECE10-KOHID    (IDX IDY)
                                           TO  CLAIMS-KOHID (IDX IDY)
                           MOVE    SPA-H99NAI-RECE10-KOHNUM   (IDX IDY)
                                           TO  CLAIMS-KOHNUM (IDX IDY)
                           MOVE    SPA-H99NAI-RECE10-FTNJANUM (IDX IDY)
                                           TO  CLAIMS-FTNJANUM
                                                               (IDX IDY)
                           MOVE    SPA-H99NAI-RECE10-JKYSNUM  (IDX IDY)
                                           TO  CLAIMS-JKYSNUM  (IDX IDY)
                       END-PERFORM
                   WHEN    "3"
                      MOVE    SPA-H99NAI-RECE10-HKNKBN (IDX)
                                       TO   CLAIMS-ACCIDENT  (IDX)
                       EVALUATE    SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                           WHEN    "7021"
      *                        アフターケア
                               MOVE    SPA-H99NAI-RECE10-KOFUNUM (IDX)
                                       TO
                                       CLAIMS-KANRITCHO-NUMBER (IDX)
                               MOVE    SPA-H99NAI-RECE10-DAMAGE-CLASS
                                                                 (IDX)
                                       TO  CLAIMS-DAMAGE-CLASS  (IDX)
                               MOVE    SPA-H99NAI-RECE10-TEKSTYMD (IDX)
                                       TO  WRK-SYMD
                               PERFORM 801-DAYHEN01-SEC
                               MOVE    WRK-HEN-DATE
                                       TO  CLAIMS-RYOYO-DATE (IDX)
                               MOVE    SPA-H99GMN-TKIGO(IDX) (9:)
                                       TO
                                       CLAIMS-DISEASE-LOCATION (IDX)
                           WHEN    "7051"
                           WHEN    "7061"
      *                        公務災害
                               MOVE    SPA-H99GMN-TKIGO(IDX)
                                       TO
                                       CLAIMS-DISEASE-LOCATION (IDX)
                               MOVE    SPA-H99NAI-RECE10-TEKSTYMD (IDX)
                                       TO  WRK-SYMD
                               PERFORM 801-DAYHEN01-SEC
                               MOVE    WRK-HEN-DATE
                                       TO  CLAIMS-START-DATE (IDX)
                           WHEN    OTHER
                               EVALUATE  SPA-H99NAI-RECE10-HKNKBN (IDX)
                                   WHEN    "1"
                                       MOVE
                                       SPA-H99NAI-RECE10-KOFUNUM (IDX)
                                           TO
                                           CLAIMS-ACCIDENT-NUM(IDX)
                                   WHEN    "2"
                                       MOVE
                                       SPA-H99NAI-RECE10-KOFUNUM (IDX)
                                           TO
                                           CLAIMS-PENSION-NUM (IDX)
                               END-EVALUATE
                               MOVE    SPA-H99NAI-RECE10-SAIGAIKBN(IDX)
                                           TO
                                           CLAIMS-ACCIDENT-CLASS (IDX)
                       END-EVALUATE
                   WHEN    "4"
                       MOVE    SPA-H99GMN-TKIGO (IDX)
                                       TO
                           CLAIMS-LI-DISEASE-LOCATION (IDX)
                       MOVE    SPA-H99NAI-RECE10-RECEKBN (IDX)
                                       TO  CLAIMS-FORM-STYLE (IDX)
                       MOVE    SPA-H99NAI-RECE10-TEKSTYMD (IDX)
                                       TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                       TO
                                   CLAIMS-LI-START-DATE (IDX)
                   WHEN    "5"
                       MOVE    SPA-H99NAI-RECE10-KIGO  (IDX)
                                      TO  CLAIMS-POLLUTION-KIGO  (IDX)
                       MOVE    SPA-H99NAI-RECE10-NUM   (IDX) 
                                      TO  CLAIMS-POLLUTION-NUM   (IDX)
               END-EVALUATE
      *
      *        地方公費情報（存在する場合のみ返却）
               IF      SPA-H99NAI-LOCAL-KOHID (IDX 1)
                                      =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SPA-H99NAI-LOCAL-SKYYM (IDX)
                                          TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                          TO
                                       CLAIMS-LOCAL-PUBLIC-SKYYM (IDX)
                   MOVE    SPA-H99NAI-LOCAL-HENREIYM (IDX)
                                          TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                          TO
                                   CLAIMS-LOCAL-PUBLIC-HENREIYM (IDX)
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       4
                                   OR      SPA-H99NAI-LOCAL-KOHID
                                                           (IDX IDY)
                                   =   ZERO
                       MOVE    SPA-H99NAI-LOCAL-KOHID  (IDX IDY)
                                      TO  CLAIMS-LOCAL-KOHID (IDX IDY)
                       MOVE    SPA-H99NAI-LOCAL-KOHNUM (IDX IDY)
                                      TO  CLAIMS-LOCAL-KOHNUM(IDX IDY)
                       MOVE    SPA-H99NAI-LOCAL-FTNJANUM  (IDX IDY)
                                      TO
                                      CLAIMS-LOCAL-FTNJANUM (IDX IDY)
                       MOVE    SPA-H99NAI-LOCAL-JKYSNUM (IDX IDY)
                                      TO
                                      CLAIMS-LOCAL-JKYSNUM   (IDX IDY)
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *####返戻情報一覧
          .
       20010-DETAIL-INF-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    レセプト情報チェック処理
      *****************************************************************
       20011-RECEIPT-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-RECEIPT-MAX
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       50
                   OR      CLAIMQ-DETAIL-INFORMATION (IDZZ)
                                   =       SPACE
               MOVE    SPACE               TO  SPA-ERRCD
                                               WRK-RECEIPT-MSG-AREA
      *
      *        レセプト情報チェック処理
               PERFORM 200111-RECEIPT-DETAIL-CHK-SEC
      *
      *        レセプト情報エラー処理
               PERFORM 200112-RECEIPT-DETAIL-ERR-SEC
           END-PERFORM
      *
           IF      WRK-RECEIPT-MAX >   ZERO
               GO  TO  20011-RECEIPT-CHK-EXT
           END-IF
      *
      *    レセプト情報と請求管理のチェック
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       50
                   OR      WRK-TEISYUTUSAKI (IDYY)
                                   =       ZERO
               MOVE    ZERO            TO  FLG-RECEIPT-CHK
      *
               PERFORM VARYING IDVV    FROM    1   BY  1
                       UNTIL   IDVV    >       SPA-H99GMN-MAX
                   MOVE    ZERO            TO  FLG-DETAIL-CHK
      *
                   IF      WRK-TEISYUTUSAKI (IDYY)
                               =   SPA-H99NAI-RECE10-TEISYUTUSAKI (IDVV)
                       CONTINUE
                   ELSE
                       MOVE    1            TO  FLG-DETAIL-CHK
                   END-IF
                   IF      WRK-NYUGAIKBN (IDYY)
                               =   SPA-H99NAI-RECE10-NYUGAIKBN (IDVV)
                       CONTINUE
                   ELSE
                       MOVE    1            TO  FLG-DETAIL-CHK
                   END-IF
                  IF      WRK-HKNID (IDYY)
                                       >   ZERO
                       IF      WRK-HKNID (IDYY)
                                   =    SPA-H99NAI-RECE10-HKNID (IDVV)
                            CONTINUE
                       ELSE
                           MOVE    1            TO  FLG-DETAIL-CHK
                       END-IF
                   END-IF
                   IF      WRK-HKNNUM (IDYY)
                                       NOT =   SPACE
                       IF      WRK-HKNNUM (IDYY)
                                   =   SPA-H99NAI-RECE10-HKNNUM (IDVV)
                           CONTINUE
                       ELSE
                           MOVE    1            TO  FLG-DETAIL-CHK
                       END-IF
                   END-IF
      *
                   IF      FLG-DETAIL-CHK      =   ZERO
                       EVALUATE    WRK-TEISYUTUSAKI (IDYY)
                         WHEN    "3"
                           IF      WRK-ACCIDENT (IDYY)
                                   =   SPA-H99NAI-RECE10-HKNKBN
                                                               (IDVV)
                               CONTINUE
                           ELSE
                               MOVE    1            TO  FLG-DETAIL-CHK
                           END-IF
                           EVALUATE    WRK-ACCIDENT (IDYY)
                               WHEN    "1"
                               WHEN    "2"
                                   IF      WRK-ACCIDENT-CLASS (IDYY)
                                       =   SPA-H99NAI-RECE10-SAIGAIKBN
                                                               (IDVV)
                                       CONTINUE
                                   ELSE
                                       MOVE    1   TO  FLG-DETAIL-CHK
                                   END-IF
                                   IF      WRK-ACCIDENT-NUM (IDYY)
                                                   NOT =   SPACE
                                       IF      WRK-ACCIDENT-NUM (IDYY)
                                                           =
                                           SPA-H99NAI-RECE10-KOFUNUM
                                                                 (IDVV)
                                           CONTINUE
                                       ELSE
                                           MOVE    1
                                                   TO  FLG-DETAIL-CHK
                                       END-IF
                                   END-IF
                               WHEN    "3"
                                   IF      WRK-RYOYO-DATE (IDYY)
                                                           =
                                           SPA-H99NAI-RECE10-TEKSTYMD
                                                               (IDVV)
                                       CONTINUE
                                   ELSE
                                       MOVE    1   TO  FLG-DETAIL-CHK
                                   END-IF
                                   IF      WRK-KANRITCHO-NUMBER (IDYY)
                                                   NOT =   SPACE
                                       IF      WRK-KANRITCHO-NUMBER
                                                                 (IDYY)
                                                           =
                                           SPA-H99NAI-RECE10-KOFUNUM
                                                                 (IDVV)
                                           CONTINUE
                                       ELSE
                                           MOVE    1
                                                   TO  FLG-DETAIL-CHK
                                       END-IF
                                   END-IF
                               WHEN    "4"
                                   IF      WRK-START-DATE (IDYY)
                                                   NOT =   SPACE
                                       IF      WRK-START-DATE (IDYY)
                                                           =
                                           SPA-H99NAI-RECE10-TEKSTYMD
                                                                 (IDVV)
                                           CONTINUE
                                       ELSE
                                           MOVE    1
                                                   TO  FLG-DETAIL-CHK
                                       END-IF
                                   END-IF
                           END-EVALUATE
                         WHEN    "4"
                           IF      WRK-FORM-STYLE (IDYY)
                                               NOT =   SPACE
                               IF      WRK-FORM-STYLE (IDYY)       =
                                           SPA-H99NAI-RECE10-RECEKBN
                                                                 (IDVV)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           IF      WRK-LI-START-DATE (IDYY)
                                               NOT =   SPACE
                               IF      WRK-LI-START-DATE (IDYY)    =
                                           SPA-H99NAI-RECE10-TEKSTYMD
                                                                 (IDVV)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                         WHEN    "5"
                           IF      WRK-POLLUTION-KIGO (IDYY)
                                               NOT =   SPACE
                               IF      WRK-POLLUTION-KIGO (IDYY)
                                                       =
                                           SPA-H99NAI-RECE10-KIGO (IDVV)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           IF      WRK-POLLUTION-NUM (IDYY)
                                               NOT =   SPACE
                               IF      WRK-POLLUTION-NUM (IDYY)
                                                       =
                                           SPA-H99NAI-RECE10-NUM (IDVV)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                         WHEN    "1"
                         WHEN    "2"
                         WHEN    "6"
                           IF      WRK-RECESYUBETU (IDYY)
                                           NOT =   SPACE
                               IF      WRK-RECESYUBETU (IDYY)
                                   =   SPA-H99NAI-RECE10-RECESYUBETU
                                                               (IDVV)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           IF      WRK-HKNJANUM (IDYY)
                                           NOT =   SPACE
                               IF      WRK-HKNJANUM (IDYY)
                                   =   SPA-H99GMN-THKNJANUM (IDVV)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           IF      WRK-HOJOKBN (IDYY)
                                           NOT =   SPACE
                               IF      WRK-HOJOKBN (IDYY)
                                   =   SPA-H99NAI-RECE10-HOJOKBN (IDVV)
                                   CONTINUE
                               ELSE
                                   MOVE    1       TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           EVALUATE    WRK-ACCIDENT-TARGET (IDYY)
                               WHEN    "True"
                                   IF      SPA-H99GMN-TDISASTER (IDVV)
                                             NOT =   SPACE
                                       CONTINUE
                                   ELSE
                                       MOVE    1   TO  FLG-DETAIL-CHK
                                   END-IF
                               WHEN    SPACE
                                 IF      SPA-H99GMN-TDISASTER (IDVV)
                                                 =   SPACE
                                     CONTINUE
                                 ELSE
                                     MOVE    1     TO  FLG-DETAIL-CHK
                                 END-IF
                           END-EVALUATE
                           EVALUATE    WRK-INCARE-CLASS (IDYY)
                               WHEN    "True"
                                   EVALUATE
                                       SPA-H99NAI-RECE10-RECEIPT-KBN
                                                           (IDVV) (1:1)
                                       WHEN    "1"
                                       WHEN    "2"
                                       WHEN    "3"
                                       WHEN    "4"
                                           CONTINUE
                                       WHEN    OTHER
                                           MOVE    1   TO FLG-DETAIL-CHK
                                   END-EVALUATE
                               WHEN    SPACE
                                   EVALUATE
                                       SPA-H99NAI-RECE10-RECEIPT-KBN
                                                           (IDVV) (1:1)
                                       WHEN    "1"
                                       WHEN    "2"
                                       WHEN    "3"
                                       WHEN    "4"
                                           MOVE    1   TO FLG-DETAIL-CHK
                                   END-EVALUATE
                           END-EVALUATE
                           PERFORM VARYING IDY     FROM    1   BY  1
                                   UNTIL   IDY     >       4
                                   OR     FLG-DETAIL-CHK   =   1
                               IF      WRK-KOHNUM (IDYY IDY)
                                                   NOT =   SPACE
                                   IF      WRK-KOHNUM (IDYY IDY)
                                       =   SPA-H99NAI-RECE10-KOHNUM
                                                           (IDVV IDY)
                                       CONTINUE
                                   ELSE
                                       MOVE    1   TO  FLG-DETAIL-CHK
                                   END-IF
                               END-IF
                               IF      WRK-KOHID (IDYY IDY)
                                                       >   ZERO
                                   IF      WRK-KOHID (IDYY IDY)
                                       =   SPA-H99NAI-RECE10-KOHID
                                                           (IDVV IDY)
                                       CONTINUE
                                   ELSE
                                       MOVE    1   TO  FLG-DETAIL-CHK
                                   END-IF
                               END-IF
                               IF      WRK-FTNJANUM (IDYY IDY)
                                                 NOT =   SPACE
                                   IF      WRK-FTNJANUM (IDYY IDY)
                                       =   SPA-H99NAI-RECE10-FTNJANUM
                                                           (IDVV IDY)
                                       CONTINUE
                                   ELSE
                                       MOVE    1   TO  FLG-DETAIL-CHK
                                   END-IF
                               END-IF
      ****                     IF      WRK-JKYSNUM (IDYY IDY)
      ****                                         NOT =   SPACE
      ****                         IF      WRK-JKYSNUM (IDYY IDY)
      ****                             =   SPA-H99NAI-RECE10-JKYSNUM
      ****                                                 (IDVV IDY)
      ****                             CONTINUE
      ****                         ELSE
      ****                             MOVE    1   TO  FLG-DETAIL-CHK
      ****                         END-IF
      ****                     END-IF
                           END-PERFORM
                     END-EVALUATE
                   END-IF
      *
      *??
      *            DISPLAY "FLG-DETAIL-CHK=" FLG-DETAIL-CHK "#" 
      *??
                   IF      FLG-DETAIL-CHK         =   ZERO
      *                リクエストに対する件数
                       ADD     1               TO  WRK-H99-CNT    (IDYY)
                       MOVE    IDVV            TO  WRK-H99-INDEX  (IDYY)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       50
                   OR      WRK-TEISYUTUSAKI (IDYY)
                                   =       ZERO
      *        個別対象−レセプト情報と一致するレセプトがないとき
               IF      WRK-H99-CNT (IDYY)  =   ZERO
                   MOVE    "E53"              TO  WRK-RECEIPT-ERRCD
                   MOVE    IDYY               TO  IDZZ
      *            レセプト情報エラー処理
                   PERFORM 200112-RECEIPT-DETAIL-ERR-SEC
               ELSE
      *            複数の個別対象−レセプト情報と一致するとき
                   IF      WRK-H99-CNT (IDYY)  >   1
                       MOVE    "E54"               TO  WRK-RECEIPT-ERRCD
                       MOVE    IDYY                TO  IDZZ
      *                レセプト情報エラー処理
                       PERFORM 200112-RECEIPT-DETAIL-ERR-SEC
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-RECEIPT-MAX     >   ZERO
               GO  TO  20011-RECEIPT-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       50
                   OR      WRK-TEISYUTUSAKI (IDYY)
                                   =       ZERO
               MOVE   WRK-H99-INDEX (IDYY)
                                       TO  IDVV
      *
               MOVE    SPACE           TO  WRK-RECEIPT-ERRCD
               MOVE    ZERO            TO  FLG-DETAIL-CHK
      *
      *        点検用レセ０点公費のとき入力不可
               IF      SPA-H99NAI-SKYKBN-RECEDEN  (IDVV)
                                       =   "1"
                   MOVE    "E35"           TO  WRK-RECEIPT-ERRCD
               END-IF
      *
      *        主科設定時の主科未設定のチェック（特別療養費を除く）
               IF     WRK-RECEIPT-ERRCD    =   SPACE
                   IF    ( SPA-H99NAI-RECE10-RECEKA (IDVV)
                                           =   "00"   )
                   AND   ( SPA-H99NAI-RECE10-RECESYUBETU (IDVV)
                                       NOT =   "6666" )
                       EVALUATE    SPA-H99NAI-RECE10-TEISYUTUSAKI (IDVV)
                           WHEN    "1"
                               IF     SPA-NAI-2009-SYUKA-SYAHO  =   "1"
                                   MOVE    "E36"  TO  WRK-RECEIPT-ERRCD
                               END-IF
                           WHEN    "2"
                               IF      SPA-NAI-2009-SYUKA-KOKUHO =   "1"
                                   MOVE    "E36"  TO  WRK-RECEIPT-ERRCD
                               END-IF
                           WHEN    "6"
                               IF      SPA-NAI-2009-SYUKA-KOUIKI =   "1"
                                   MOVE    "E36"  TO  WRK-RECEIPT-ERRCD
                               END-IF
                       END-EVALUATE
                   END-IF
               END-IF
      *
      *        レセ電データ存在チェック処理
               IF     WRK-RECEIPT-ERRCD    =   SPACE
                   IF      SPA-H99NAI-RECEDEN-FLG (IDVV)
                                               =   1
                       MOVE    "E37"               TO  WRK-RECEIPT-ERRCD
                   END-IF
               END-IF
      *
      *        返戻データ紐付けチェック処理
                IF     WRK-RECEIPT-ERRCD    =   SPACE
                   IF      SPA-H99NAI-RECE10-HENREI-SJ-FLG (IDVV)
                                               =   1
                       MOVE    "E3t"               TO  WRK-RECEIPT-ERRCD
                   END-IF
               END-IF
      *
      *        入力値と設定のチェック
               IF     WRK-RECEIPT-ERRCD    =   SPACE
                   IF    ( SPA-H99NAI-RECE10-SKYKBN (IDVV)
                                           NOT =   WRK-SKYKBN   )
                   OR    ( SPA-H99NAI-RECE10-SKYYM (IDVV)
                                           NOT =   WRK-SKYYM    )
                   OR    ( SPA-H99NAI-RECE10-HENREIYM (IDVV)
                                           NOT =   WRK-HENREIYM )
                       CONTINUE
                   ELSE
                       IF      SPA-H99NAI-LOCAL-KOHNUM (IDVV 1)
                                               NOT =   SPACE
                           IF    ( SPA-H99NAI-LOCAL-SKYYM    (IDVV)
                                           NOT =   WRK-KOHSKY-SKYYM    )
                           OR    ( SPA-H99NAI-LOCAL-HENREIYM (IDVV)
                                           NOT =   WRK-KOHSKY-HENREIYM )
      *                        地方公費の更新内容は違うが、地方公費は更新しないとき
                               IF   ( WRK-LOCAL-KOHNUM (IDYY 1)
                                               NOT =   SPACE )
                                   CONTINUE
                               ELSE
                                   MOVE    "E3s"   TO  WRK-RECEIPT-ERRCD
                               END-IF
                           ELSE
                               MOVE    "E39"       TO  WRK-RECEIPT-ERRCD
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               IF     WRK-RECEIPT-ERRCD
                                       NOT =   SPACE
                   MOVE    IDYY            TO  IDZZ
      *            レセプト情報エラー処理
                   PERFORM 200112-RECEIPT-DETAIL-ERR-SEC
               END-IF
      *
      *        レセプト情報と地方公費のチェック
               IF   ( WRK-LOCAL-KOHNUM (IDYY 1)
                                   NOT =   SPACE )
               AND  ( WRK-RECEIPT-ERRCD
                                       =   SPACE )
                   IF      SPA-H99NAI-LOCAL-KOHNUM (IDVV 1)
                                           =   SPACE
                       MOVE    "E3r"           TO  WRK-RECEIPT-ERRCD
                       MOVE    IDYY            TO  IDZZ
      *                レセプト情報エラー処理
                       PERFORM 200112-RECEIPT-DETAIL-ERR-SEC
                   ELSE
      *                レセプト情報と地方公費のチェック
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >       4
                               OR     FLG-DETAIL-CHK   =   1
                           IF      WRK-LOCAL-KOHNUM (IDYY IDY)
                                       NOT =   SPACE
                               IF      WRK-LOCAL-KOHNUM (IDYY IDY)
                                           =   SPA-H99NAI-LOCAL-KOHNUM
                                                           (IDVV IDY)
                                   CONTINUE
                               ELSE
                                   MOVE    1   TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
                           IF      WRK-LOCAL-KOHID (IDYY IDY)
                                           >   ZERO
                               IF      WRK-LOCAL-KOHID (IDYY IDY)
                                           =   SPA-H99NAI-LOCAL-KOHID
                                                           (IDVV IDY)
                                   CONTINUE
                               ELSE
                                   MOVE    1   TO  FLG-DETAIL-CHK
                               END-IF
                           END-IF
      ****                 IF      WRK-LOCAL-FTNJANUM (IDYY IDY)
      ****                             NOT =   SPACE
      ****                     IF      WRK-LOCAL-FTNJANUM (IDYY IDY)
      ****                                 =   SPA-H99NAI-LOCAL-FTNJANUM
      ****                                                 (IDVV IDY)
      ****                         CONTINUE
      ****                     ELSE
      ****                         MOVE    1   TO  FLG-DETAIL-CHK
      ****                     END-IF
      ****                 END-IF
      ****                 IF      WRK-LOCAL-JKYSNUM (IDYY IDY)
      ****                             NOT =   SPACE
      ****                     IF      WRK-LOCAL-JKYSNUM (IDYY IDY)
      ****                                 =   SPA-H99NAI-LOCAL-JKYSNUM
      ****                                                 (IDVV IDY)
      ****                         CONTINUE
      ****                     ELSE
      ****                         MOVE    1   TO  FLG-DETAIL-CHK
      ****                     END-IF
      ****                 END-IF
                       END-PERFORM
      *
                       IF      FLG-DETAIL-CHK  =   1
                           MOVE    "E34"           TO  WRK-RECEIPT-ERRCD
                           MOVE    IDYY            TO  IDZZ
      *                    レセプト情報エラー処理
                           PERFORM 200112-RECEIPT-DETAIL-ERR-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-RECEIPT-MAX     >   ZERO
               GO  TO  20011-RECEIPT-CHK-EXT
           END-IF
      *
      *    初画面情報の退避
           MOVE    SPA-H01KYOUTU   TO  TAIHI-SPA-WORK
           MOVE    SPA-AREA        TO  TAIHI-SPA-COMMON
           MOVE    SPA-H01FREE     TO  TAIHI-SPA-FREE
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       50
                   OR      WRK-TEISYUTUSAKI (IDYY)
                                   =       ZERO
               MOVE    SPACE           TO  WRK-RECEIPT-ERRCD
      *
               MOVE    TAIHI-SPA-WORK  TO  SPA-H01KYOUTU
               MOVE    TAIHI-SPA-COMMON
                                       TO  SPA-AREA
               MOVE    TAIHI-SPA-FREE  TO  SPA-H01FREE
      *
               INITIALIZE                  SPA-H01KYOUTU-H
               MOVE    SPACE           TO  LINKAREA
                                           SPA-ERRCD
                                           SPA-ERRMSG
      *??
      *        DISPLAY "WRK-H99-INDEX=" IDYY "#" WRK-H99-INDEX(IDYY) "#"
      *??
               MOVE   WRK-H99-INDEX (IDYY)
                                       TO  H99-SELNUM
               MOVE    SPACE           TO  MCP-STATUS
               MOVE    "SELNUM"        TO  MCP-WIDGET
               MOVE    "ACTIVATE"      TO  MCP-EVENT
      *
               PERFORM 800-ORCGH99-CALL-SEC
      *??
      *         DISPLAY "H99 ERRCD=" SPA-ERRCD "#" SPA-HIDCD "#"
      *??
      *
               IF      SPA-ERRCD   NOT =   SPACE
                   EVALUATE    SPA-ERRCD
                       WHEN    "0009"
                           MOVE    "E35"       TO  WRK-RECEIPT-ERRCD
                       WHEN    OTHER
                           MOVE    "E60"       TO  WRK-RECEIPT-ERRCD
                   END-EVALUATE
               ELSE
                   IF      SPA-HIDCD   NOT =   SPACE
                       EVALUATE    SPA-HIDCD
                           WHEN    "1006"
                               MOVE    "E36"       TO  WRK-RECEIPT-ERRCD
                       END-EVALUATE
                   END-IF
               END-IF
               IF      WRK-RECEIPT-ERRCD   NOT =   SPACE
      *            レセプト情報エラー処理
                    MOVE    IDYY        TO  IDZZ
                   PERFORM 200112-RECEIPT-DETAIL-ERR-SEC
               ELSE
                   MOVE    SPACE           TO  LINKAREA
                                               SPA-ERRCD
                                               SPA-ERRMSG
                   MOVE    "H99"           TO  SPA-MOTOPG
      *
                   MOVE    "LINK"          TO  MCP-STATUS
                   MOVE    SPACE           TO  MCP-EVENT
      *
                   PERFORM 800-ORCGH01-CALL-SEC
      *??
      *      DISPLAY "HENFLG=" SPA-HENFLG "#"
      * "WRK-SKYKBN=" WRK-SKYKBN "#" WRK-SKYYM "#" WRK-HENREIYM "#"
      *??
      *
                   INITIALIZE                  SPA-H01KYOUTU-H
                   MOVE    SPACE           TO  LINKAREA
                                               SPA-MOTOPG
                                               SPA-ERRCD
                                               SPA-ERRMSG
      *
      *??
      *      DISPLAY
      * "WRK-SKYKBN=" WRK-SKYKBN "#" WRK-SKYYM "#" WRK-HENREIYM "#"
      *??
                   MOVE    WRK-SKYKBN      TO  H01-SKYKBN
                   IF      WRK-SKYYM       =   ZERO
                       MOVE    SPACE           TO  H01-SKYYM
                   ELSE
                       MOVE    WRK-SKYYM       TO  H01-SKYYM
                   END-IF
                   IF      WRK-HENREIYM    =   ZERO
                       MOVE    SPACE           TO  H01-HENREIYM
                   ELSE
                       MOVE    WRK-HENREIYM    TO  H01-HENREIYM
                   END-IF
      *
                   MOVE    SPACE           TO  MCP-STATUS
                   MOVE    "ACTIVATE"      TO  MCP-EVENT
                   MOVE    SPACE           TO  MCP-WIDGET
      *
                   PERFORM 800-ORCGH01-CALL-SEC
      *
                   IF    ( SPA-ERRCD       =   SPACE )
                   AND   ( SPA-HIDCD       =   SPACE )
                   AND  (  SPA-H01NAI-KOHSKY-FLG
                                           =   1     )
                       INITIALIZE                  SPA-H01KYOUTU-H
                       MOVE    SPACE           TO  LINKAREA
                                                   SPA-MOTOPG
                                                   SPA-ERRCD
                                                   SPA-ERRMSG
      *
                       IF      WRK-KOHSKY-SKYYM
                                               =   ZERO
                           MOVE    SPACE           TO  H01-SKYYM-KOHSKY
                       ELSE
                           MOVE    WRK-KOHSKY-SKYYM
                                                   TO  H01-SKYYM-KOHSKY
                       END-IF
                       IF      WRK-KOHSKY-HENREIYM
                                               =   ZERO
                           MOVE    SPACE       TO  H01-HENREIYM-KOHSKY
                       ELSE
                           MOVE    WRK-KOHSKY-HENREIYM
                                               TO  H01-HENREIYM-KOHSKY
                       END-IF
      *
                       MOVE    SPACE           TO  MCP-STATUS
                       MOVE    "ACTIVATE"      TO  MCP-EVENT
                       MOVE    SPACE           TO  MCP-WIDGET
      *
                       PERFORM 800-ORCGH01-CALL-SEC
                   END-IF
                   IF      SPA-ERRCD   NOT =   SPACE
                       MOVE    "E60"       TO  WRK-RECEIPT-ERRCD
                   END-IF
               END-IF
      *
               IF      WRK-RECEIPT-ERRCD   NOT =   SPACE
      *            レセプト情報エラー処理
                    MOVE    IDYY        TO  IDZZ
                   PERFORM 200112-RECEIPT-DETAIL-ERR-SEC
               END-IF
           END-PERFORM
           .
       20011-RECEIPT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト情報チェック処理
      *****************************************************************
       200111-RECEIPT-DETAIL-CHK-SEC      SECTION.
      *
      *    レセプト情報の重複チェック
           COMPUTE IDZ =   IDZZ    +   1
           PERFORM VARYING IDYY    FROM    IDZ   BY  1
                   UNTIL   IDYY    >       50
                   OR      CLAIMQ-DETAIL-INFORMATION (IDZZ)
                                   =       SPACE
               IF      CLAIMQ-DETAIL-INFORMATION (IDZZ)
                               =   CLAIMQ-DETAIL-INFORMATION (IDYY)
                   MOVE    "E32"           TO  WRK-RECEIPT-ERRCD
               ELSE
                   IF    ( CLAIMQ-DETAI-TEISYUTUSAKI (IDZZ)
                                       =
                                   CLAIMQ-DETAI-TEISYUTUSAKI (IDYY)   )
                   AND   ( CLAIMQ-DETAIL-INOUT (IDZZ)
                                       =   CLAIMQ-DETAIL-INOUT (IDYY) )
                   AND   ( CLAIMQ-DETAIL-CODE (IDZZ)
                                       =   CLAIMQ-DETAIL-CODE (IDYY)  )
                       IF    ( CLAIMQ-HKN (IDZZ)
                                       NOT =   SPACE             )
                       AND   ( CLAIMQ-HKN (IDZZ)
                                           =   CLAIMQ-HKN (IDYY) )
                       AND   ( CLAIMQ-RYOYO-DATE (IDZZ)
                                           =
                                           CLAIMQ-RYOYO-DATE (IDYY) )
                           MOVE    "E32"       TO  WRK-RECEIPT-ERRCD
                       END-IF
                   END-IF
               END-IF
               IF      WRK-RECEIPT-ERRCD   NOT =   SPACE
                   GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
               END-IF
           END-PERFORM
      *
      *    提出先
           EVALUATE    CLAIMQ-DETAI-TEISYUTUSAKI (IDZZ)
               WHEN    "02"
                   MOVE    1           TO  WRK-TEISYUTUSAKI (IDZZ)
               WHEN    "03"
                   MOVE    2           TO  WRK-TEISYUTUSAKI (IDZZ)
               WHEN    "04"
                   MOVE    6           TO  WRK-TEISYUTUSAKI (IDZZ)
               WHEN    "05"
                   MOVE    3           TO  WRK-TEISYUTUSAKI (IDZZ)
               WHEN    "06"
                   MOVE    4           TO  WRK-TEISYUTUSAKI (IDZZ)
               WHEN    "09"
                   MOVE    5           TO  WRK-TEISYUTUSAKI (IDZZ)
               WHEN    OTHER
                   MOVE    "E3p"       TO  WRK-RECEIPT-ERRCD
                   GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
           END-EVALUATE
      *
      *    入外区分
           EVALUATE    CLAIMQ-DETAIL-INOUT (IDZZ)
               WHEN    "I"
                   MOVE    "1"         TO  WRK-NYUGAIKBN (IDZZ)
               WHEN    "O"
                   MOVE    "2"         TO  WRK-NYUGAIKBN (IDZZ)
               WHEN    OTHER
                   MOVE    "E3n"       TO  WRK-RECEIPT-ERRCD
                   GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
           END-EVALUATE
      *
           EVALUATE    CLAIMQ-DETAI-TEISYUTUSAKI (IDZZ)
               WHEN    "02"
               WHEN    "03"
               WHEN    "04"
      *            医保情報以外の入力の有無
                   IF    ( CLAIMQ-ACCIDENT-INSURANCE  (IDZZ)
                                           NOT =   SPACE )
                   OR    ( CLAIMQ-LIABILITY-INSURANCE (IDZZ)
                                           NOT =   SPACE )
                   OR    ( CLAIMQ-POLLUTION-INSURANCE (IDZZ)
                                           NOT =   SPACE )
                       MOVE    "E33"           TO  WRK-RECEIPT-ERRCD
                       GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                   END-IF
      *            種別コード
      *****        IF      CLAIMQ-DETAIL-CODE (IDZZ)
      *****                                    =   SPACE
      *****            MOVE    "E38"       TO  WRK-RECEIPT-ERRCD
      *****            GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
      *****        ELSE
                       INITIALIZE                  ORCSNUMAREA
                       MOVE    CLAIMQ-DETAIL-CODE (IDZZ)
                                                   TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF    ( SNUM-RC         =   ZERO )
                       AND   ( SNUM-SEISUU     =   4    )
                       AND   ( SNUM-SYOSUU     =   0    ) 
                           MOVE    CLAIMQ-DETAIL-CODE (IDZZ)
                                               TO  WRK-RECESYUBETU(IDZZ)
                       ELSE
                           MOVE    "E38"       TO  WRK-RECEIPT-ERRCD
                           GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                       END-IF
      *****        END-IF
      *            保険情報、公費情報
                   IF      CLAIMQ-DETAIL-CODE (IDZZ) (1:2)
                                               =   "12"
      *                公費単独の場合の公費情報の入力の有無
                       IF    ( CLAIMQ-KOHID  (IDZZ 1)
                                           NOT =   SPACE )
                       AND   ( CLAIMQ-KOHNUM (IDZZ 1)
                                           NOT =   SPACE )
                       AND   ( CLAIMQ-HKNID  (IDZZ)
                                               =   SPACE )
                       AND   ( CLAIMQ-HKNNUM (IDZZ)
                                               =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE    "E30"       TO  WRK-RECEIPT-ERRCD
                           GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                       END-IF
                   ELSE
      *                主保険ありの場合の保険情報の入力の有無
                       IF    ( CLAIMQ-HKNID  (IDZZ)
                                           NOT =   SPACE )
                       AND   ( CLAIMQ-HKNNUM (IDZZ)
                                           NOT =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE    "E31"       TO  WRK-RECEIPT-ERRCD
                           GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                       END-IF
                   END-IF
               WHEN    OTHER
      *            医保情報の入力の有無
                   IF    ( CLAIMQ-DETAIL-CODE      (IDZZ)
                                       NOT =   SPACE )
                   OR    ( CLAIMQ-MAIN-INSURANCE   (IDZZ)
                                       NOT =   SPACE )
                   OR    ( CLAIMQ-PUBLIC-INSURANCE (IDZZ)
                                       NOT =   SPACE )
                   OR    ( CLAIMQ-LOCAL-PUBLIC-INSURANCE (IDZZ)
                                        NOT =   SPACE )
                       MOVE    "E33"           TO  WRK-RECEIPT-ERRCD
                       GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                   END-IF
      *            保険情報の入力の有無
                   IF    ( CLAIMQ-HKNID  (IDZZ)
                                       NOT =   SPACE )
                   AND   ( CLAIMQ-HKNNUM (IDZZ)
                                       NOT =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    "E31"           TO  WRK-RECEIPT-ERRCD
                       GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                   END-IF
           END-EVALUATE
       *
       *   保険ＩＤ
           IF     CLAIMQ-HKNID  (IDZZ) NOT =   SPACE
              INITIALIZE                  ORCSNUMAREA
               MOVE    CLAIMQ-HKNID  (IDZZ)
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING   ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  WRK-HKNID (IDZZ)
               ELSE
                   MOVE    "E3c"           TO  WRK-RECEIPT-ERRCD
                   GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
               END-IF
           END-IF
      *    保険番号
           IF      CLAIMQ-HKNNUM (IDZZ)
                                       NOT =   SPACE
               INITIALIZE          ORCSKANACHKAREA
               MOVE    "1"         TO  KANACHK-SYORI 
               MOVE    3           TO  KANACHK-MAX-CNT
               MOVE    CLAIMQ-HKNNUM (IDZZ)
                                   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING
                                       ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
               AND   ( KANACHK-RC2     =   ZERO )
               AND   ( KANACHK-SYUBETU =   1    )
               AND   ( KANACHK-MAX     =   3    )
                   MOVE    CLAIMQ-HKNNUM (IDZZ)
                                           TO  WRK-HKNNUM (IDZZ)
               ELSE
                   MOVE    "E3d"           TO  WRK-RECEIPT-ERRCD
                   GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
               END-IF
           END-IF
      *
           EVALUATE    CLAIMQ-DETAI-TEISYUTUSAKI (IDZZ)
               WHEN    "02"
               WHEN    "03"
               WHEN    "04"
      *            保険者番号
                   IF      CLAIMQ-HKNJANUM (IDZZ)
                                       NOT =   SPACE
                       MOVE    CLAIMQ-HKNJANUM (IDZZ)
                                           TO  WRK-HKNJANUM (IDZZ)
                   END-IF
                   MOVE    CLAIMQ-HKNJANUM (IDZZ)
                                       TO  WRK-HKNJANUM (IDZZ)
      *            記号
                   MOVE    CLAIMQ-KIGO (IDZZ)
                                       TO  WRK-KIGO     (IDZZ)
      *            番号
                   MOVE    CLAIMQ-NUM  (IDZZ)
                                       TO  WRK-NUM      (IDZZ)
      *            補助区分
                   MOVE    CLAIMQ-HOJOKBN (IDZZ)
                                       TO  WRK-HOJOKBN  (IDZZ)
      *            災害該当の有無
                   EVALUATE   CLAIMQ-ACCIDENT-TARGET (IDZZ)
                       WHEN    SPACE
                       WHEN    "True"
                           MOVE    CLAIMQ-ACCIDENT-TARGET (IDZZ)
                                               TO
                                           WRK-ACCIDENT-TARGET (IDZZ)
                       WHEN    OTHER
                           MOVE    "E3g"       TO  WRK-RECEIPT-ERRCD
                           GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                   END-EVALUATE
      *            入所中の有無
                   EVALUATE   CLAIMQ-INCARE-CLASS (IDZZ)
                       WHEN    SPACE
                       WHEN    "True"
                           MOVE    CLAIMQ-INCARE-CLASS (IDZZ)
                                               TO
                                           WRK-INCARE-CLASS (IDZZ)
                       WHEN    OTHER
                           MOVE    "E3h"       TO  WRK-RECEIPT-ERRCD
                           GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                   END-EVALUATE
      *            公費情報
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       4
                           OR (    CLAIMQ-KOHID  (IDZZ IDXX)
                                           =   SPACE
                               AND CLAIMQ-KOHNUM (IDZZ IDXX)
                                           =   SPACE )
      *                公費ＩＤ
                       IF      CLAIMQ-KOHID (IDZZ IDXX)
                                                   =   SPACE
                           CONTINUE
                       ELSE
                           INITIALIZE              ORCSNUMAREA
                           MOVE    CLAIMQ-KOHID (IDZZ IDXX)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"       USING
                                                   ORCSNUMAREA
                           IF      SNUM-RC     =   ZERO
                              MOVE    SNUM-OUTNUM
                                           TO  WRK-KOHID (IDZZ IDXX)
                           ELSE
                               MOVE    "E3e"   TO  WRK-RECEIPT-ERRCD
                               GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                           END-IF
                       END-IF
      *                公費番号
                       IF      CLAIMQ-KOHNUM (IDZZ IDXX)
                                                   =   SPACE
                           CONTINUE
                       ELSE
                           INITIALIZE      ORCSKANACHKAREA
                           MOVE    "1"     TO  KANACHK-SYORI 
                           MOVE    3       TO  KANACHK-MAX-CNT
                           MOVE    CLAIMQ-KOHNUM (IDZZ IDXX)
                                           TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   1    )
                           AND   ( KANACHK-MAX     =   3    )
                               MOVE    CLAIMQ-KOHNUM (IDZZ IDXX)
                                               TO  WRK-KOHNUM(IDZZ IDXX)
                           ELSE
                               MOVE    "E3f"   TO  WRK-RECEIPT-ERRCD
                               GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                           END-IF
                       END-IF
      *                負担者番号
                       MOVE    CLAIMQ-FTNJANUM (IDZZ IDXX)
                                           TO  WRK-FTNJANUM (IDZZ IDXX)
      *                受給者番号
                       MOVE    CLAIMQ-JKYSNUM (IDZZ IDXX)
                                           TO  WRK-JKYSNUM  (IDZZ IDXX)
                   END-PERFORM
      *            地方公費情報
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       4
                           OR      CLAIMQ-LOCAL-PUBLIC-INS-O (IDZZ IDXX)
                                           =   SPACE
      *                公費ＩＤ
                       IF      CLAIMQ-LOCAL-KOHID (IDZZ IDXX)
                                                   =   SPACE
                           CONTINUE
                       ELSE
                           INITIALIZE              ORCSNUMAREA
                           MOVE    CLAIMQ-LOCAL-KOHID (IDZZ IDXX)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"       USING
                                                   ORCSNUMAREA
                           IF      SNUM-RC     =   ZERO
                               MOVE    SNUM-OUTNUM TO
                                           WRK-LOCAL-KOHID (IDZZ IDXX)
                           ELSE
                               MOVE    "E3q"       TO  WRK-RECEIPT-ERRCD
                               GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                           END-IF
                       END-IF
      *                公費番号
                       IF      CLAIMQ-LOCAL-KOHNUM (IDZZ IDXX)
                                                   =   SPACE
                           CONTINUE
                       ELSE
                           INITIALIZE      ORCSKANACHKAREA
                           MOVE    "1"     TO  KANACHK-SYORI 
                           MOVE    3       TO  KANACHK-MAX-CNT
                           MOVE    CLAIMQ-LOCAL-KOHNUM (IDZZ IDXX)
                                           TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                  ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   1    )
                           AND   ( KANACHK-MAX     =   3    )
                               MOVE    CLAIMQ-LOCAL-KOHNUM (IDZZ IDXX)
                                                   TO
                                           WRK-LOCAL-KOHNUM (IDZZ IDXX)
                           ELSE
                               MOVE    "E3o"   TO  WRK-RECEIPT-ERRCD
                               GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                           END-IF
                       END-IF
      *                負担者番号
                       MOVE    CLAIMQ-LOCAL-FTNJANUM (IDZZ IDXX)
                                           TO
                                       WRK-LOCAL-FTNJANUM (IDZZ IDXX)
      *                受給者番号
                       MOVE    CLAIMQ-LOCAL-JKYSNUM (IDZZ IDXX)
                                           TO
                                       WRK-LOCAL-JKYSNUM  (IDZZ IDXX)
                   END-PERFORM
               WHEN    "05"
      *            労災自賠保険区分
                   EVALUATE    CLAIMQ-ACCIDENT (IDZZ)
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "3"
                       WHEN    "4"
                           MOVE    CLAIMQ-ACCIDENT (IDZZ)
                                               TO WRK-ACCIDENT (IDZZ)
                       WHEN    OTHER
                           MOVE    "E3j"       TO  WRK-RECEIPT-ERRCD
                   END-EVALUATE
      *
                   EVALUATE    CLAIMQ-ACCIDENT (IDZZ)
                       WHEN    "1"
                       WHEN    "2"
      *                    労働保険番号と年金証書番号の重複
                           IF    ( CLAIMQ-ACCIDENT-NUM (IDZZ)
                                               NOT =   SPACE )
                           AND   ( CLAIMQ-PENSION-NUM  (IDZZ)
                                               NOT =   SPACE )
                               MOVE    "E3m"       TO  WRK-RECEIPT-ERRCD
                           END-IF
      *                    労働保険番号
                           MOVE    CLAIMQ-ACCIDENT-NUM (IDZZ)
                                               TO
                                               WRK-ACCIDENT-NUM (IDZZ)
      *                    年金証書番号
                           MOVE    CLAIMQ-PENSION-NUM (IDZZ)
                                               TO
                                               WRK-ACCIDENT-NUM (IDZZ)
      *                    災害区分
                           EVALUATE    CLAIMQ-ACCIDENT-CLASS (IDZZ)
                               WHEN    "1"
                               WHEN    "2"
                                   MOVE    CLAIMQ-ACCIDENT-CLASS (IDZZ)
                                                   TO
                                               WRK-ACCIDENT-CLASS (IDZZ)
                               WHEN    OTHER
                                   MOVE    "E3k"   TO  WRK-RECEIPT-ERRCD
                           END-EVALUATE
                       WHEN    "3"
      *                    健康管理手帳番号
                           MOVE    CLAIMQ-KANRITCHO-NUMBER (IDZZ)
                                               TO
                                           WRK-KANRITCHO-NUMBER (IDZZ)
      *                    損傷区分
                           MOVE    CLAIMQ-DAMAGE-CLASS (IDZZ)
                                               TO
                                           WRK-DAMAGE-CLASS     (IDZZ)
      *                    診療年月日
                           IF      CLAIMQ-RYOYO-DATE (IDZZ)
                                                       =   SPACE
                               MOVE    "E3a"       TO  WRK-RECEIPT-ERRCD
                               GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                           ELSE
                               MOVE    CLAIMQ-RYOYO-DATE (IDZZ)
                                                       TO  WRK-HEN-DATE
                               PERFORM 802-DAYHEN02-SEC
                               INITIALIZE                  ORCSGDAYAREA
                               MOVE    WRK-SYMD        TO  SGDAY-INDAY
                               CALL    "ORCSGDAY"          USING
                                                           ORCSGDAYAREA
                               IF      SGDAY-RC        =   ZERO
                                   MOVE    WRK-SYMD    TO
                                                   WRK-RYOYO-DATE (IDZZ)
                               ELSE
                                   MOVE    "E3a"       TO
                                                       WRK-RECEIPT-ERRCD
                                   GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                               END-IF
                           END-IF
                       WHEN    "4"
      *                    療養開始日
                           IF      CLAIMQ-START-DATE (IDZZ)
                                                       =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    CLAIMQ-START-DATE (IDZZ)
                                                       TO  WRK-HEN-DATE
                               PERFORM 802-DAYHEN02-SEC
                               INITIALIZE                  ORCSGDAYAREA
                               MOVE    WRK-SYMD        TO  SGDAY-INDAY
                               CALL    "ORCSGDAY"          USING
                                                           ORCSGDAYAREA
                               IF      SGDAY-RC        =   ZERO
                                   MOVE    WRK-SYMD    TO
                                                   WRK-START-DATE (IDZZ)
                               ELSE
                                   MOVE    "E3b"       TO
                                                       WRK-RECEIPT-ERRCD
                                   GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                               END-IF
                           END-IF
                   END-EVALUATE
               WHEN    "06"
      *            様式
                   EVALUATE    CLAIMQ-FORM-STYLE (IDZZ)
                       WHEN    "4"
                       WHEN    "5"
                       WHEN    "6"
                       WHEN    SPACE
                           MOVE    CLAIMQ-FORM-STYLE (IDZZ)
                                               TO  WRK-FORM-STYLE (IDZZ)
                       WHEN    OTHER
                           MOVE    "E3i"       TO  WRK-RECEIPT-ERRCD
                           GO  TO  200111-RECEIPT-DETAIL-CHK-EXT
                   END-EVALUATE
               WHEN    "09"
      *            記号
                   MOVE    CLAIMQ-POLLUTION-KIGO (IDZZ)
                                           TO  WRK-POLLUTION-KIGO (IDZZ)
      *            番号
                   MOVE    CLAIMQ-POLLUTION-NUM  (IDZZ)
                                           TO  WRK-POLLUTION-NUM  (IDZZ)
           END-EVALUATE
           .
       200111-RECEIPT-DETAIL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト情報エラー処理
      *****************************************************************
       200112-RECEIPT-DETAIL-ERR-SEC     SECTION.
      *??
      *         DISPLAY "200112-RECEIPT-DETAIL-ERR IDZZ=" 
      *          IDZZ "#" WRK-RECEIPT-ERRCD "#" 
      *??
      *
           IF    ( WRK-RECEIPT-ERRCD   =   SPACE )
           AND   ( SPA-ERRCD           =   SPACE )
               GO  TO  200112-RECEIPT-DETAIL-ERR-EXT
           END-IF
      *
           ADD     1               TO  WRK-RECEIPT-MAX
      *
           MOVE    CLAIMQ-DETAIL-INFORMATION (IDZZ)
                                   TO  CLAIMS-ERR-DETAIL-INFORMATION
                                               (WRK-RECEIPT-MAX)
           MOVE    WRK-RECEIPT-ERRCD
                                   TO  CLAIMS-ERR-ERRCD
                                               (WRK-RECEIPT-MAX)
           EVALUATE    WRK-RECEIPT-ERRCD
               WHEN    "E30"
                   MOVE    "保険情報、公費情報の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E31"
                   MOVE    "保険情報の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E32"
                   MOVE    "同じ内容で設定してあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E33"
                   MOVE    "不要な設定があります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E34"
                   MOVE
                       "地方公費情報の対象となる地方公費がありません。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E35"
                   MOVE    "０点公費記載のレセプトです。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E36"
                   MOVE    "主科未設定です。"  TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E37"
                   MOVE    "レセ電データがありません。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E38"
                   MOVE    "種別コードの設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E39"
               WHEN    "E3s"
                   MOVE   "設定項目と対象データの項目が同じです 。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3a"
                   MOVE    "診療年月日が暦日ではありません。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3b"
                   MOVE    "療養開始年月日が暦日ではありません。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3c"
                   MOVE    "保険ＩＤの設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3d"
                   MOVE    "保険の種類の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3e"
                   MOVE    "公費ＩＤの設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3f"
                   MOVE    "公費の種類の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3g"
                   MOVE    "災害該当の有無の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3h"
                   MOVE    "入所中の有無の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3i"
                   MOVE    "様式の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3j"
                   MOVE    "労災自賠保険区分の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3k"
                   MOVE    "災害区分の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3m"
                   STRING  "労働保険番号、年金証書番号の"
                                                   DELIMITED   BY  SIZE
                           "の両方に設定してあります。"
                                                   DELIMITED   BY  SIZE
                                               INTO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
                   END-STRING
               WHEN    "E3n"
                   MOVE    "入外区分の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3o"
                   MOVE
                       "公費の種類（地方公費）の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3p"
                   MOVE    "提出先の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3q"
                   MOVE
                       "公費ＩＤ（地方公費）の設定に誤りがあります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3r"
                   MOVE    "地方公費は存在しません。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E3t"
                   MOVE    "返戻データが紐付けされています。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E53"
                   MOVE
                        "レセプト情報の対象となるレセプトがありません。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
               WHEN    "E54"
                   MOVE
                     "レセプト情報から複数のレセプトが対象になります。"
                                               TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
      *
               WHEN    "E60"
                   MOVE    SPA-ERRMSG          TO
                                   CLAIMS-ERR-ERRMSG (WRK-RECEIPT-MAX)
           END-EVALUATE
           .
     *
       200112-RECEIPT-DETAIL-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       2002-CLAIMED-CHECK-SEC    SECTION.
      *
      *    対象レセプト情報の返却処理
           PERFORM 20023-DETAIL-INF-HENSYU-SEC
      *
      *    オルカＵＩＤ処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *****APIパラメタチェック処理
      *****PERFORM 20021-PARA-INIT-SEC
      *
      *    APIパラメタ登録処理
           PERFORM 20022-APIPARA-INS-SEC
      *
           IF      WRK-ERRCD           =   SPACE
      *        排他チェック & ロック処理
               PERFORM 990-LOCK-IN-SEC
               IF      SLOCK-RETURN    NOT =   ZERO
                   MOVE    "E90"               TO  WRK-ERRCD
      *************MOVE    SPACE               TO  WRK-WARNING
                   GO  TO  2002-CLAIMED-CHECK-EXT
               END-IF
           END-IF
           .
       2002-CLAIMED-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象レセプト情報の返却処理
      *****************************************************************
       20023-DETAIL-INF-HENSYU-SEC     SECTION.
      *
           INITIALIZE                  CLAIMS-RECEIPT-INFORMATION
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       50
                   OR      WRK-TEISYUTUSAKI (IDYY)
                                   =       ZERO
               MOVE   WRK-H99-INDEX (IDYY)
                                       TO  IDX
      *
               EVALUATE    SPA-H99NAI-RECE10-TEISYUTUSAKI (IDX)
                   WHEN    "1"
                       MOVE    "02"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "2"
                       MOVE    "03"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "3"
                       MOVE    "05"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "4"
                       MOVE    "06"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "5"
                       MOVE    "09"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
                   WHEN    "6"
                       MOVE    "04"        TO
                                   CLAIMS-DETAIL-TEISYUTUSAKI (IDX)
               END-EVALUATE
      *
               EVALUATE    SPA-H99NAI-RECE10-TEISYUTUSAKI (IDX)
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "6"
                       IF      SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                                           =   "6666"
                           MOVE    SPA-H99GMN-TRECESYUBETU (IDX)
                                       TO  CLAIMS-DETAIL-TYPE (IDX)
                       ELSE 
                           MOVE    SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                                       TO  CLAIMS-DETAIL-CODE (IDX)
                           MOVE    SPA-H99GMN-TRECESYUBETU (IDX) (5:)
                                       TO  CLAIMS-DETAIL-TYPE (IDX)
                       END-IF 
                   WHEN    OTHER
                       MOVE    SPA-H99GMN-TRECESYUBETU (IDX)
                                       TO  CLAIMS-DETAIL-TYPE (IDX)
               END-EVALUATE
      *
               EVALUATE    SPA-H99NAI-RECE10-NYUGAIKBN (IDX)
                   WHEN    "1"
                       MOVE    "I"     TO  CLAIMS-DETAIL-INOUT (IDX)
                   WHEN    "2"
                       MOVE    "O"     TO  CLAIMS-DETAIL-INOUT (IDX)
               END-EVALUATE
      *
               MOVE    SPA-H99NAI-RECE10-SKYKBN (IDX)
                                       TO  CLAIMS-DETAIL-SKYKBN (IDX)
      *
      *        レセプト情報
               MOVE    SPA-H99NAI-RECE10-SKYYM (IDX)
                                       TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE (1:7)
                                       TO  CLAIMS-RECEIPT-SKYYM  (IDX)
               MOVE    SPA-H99NAI-RECE10-HENREIYM (IDX)
                                       TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE (1:7)
                                       TO
                                       CLAIMS-RECEIPT-HENREIYM  (IDX)
      *
               IF      SPA-H99NAI-RECE10-HKNNUM (IDX)
                                   NOT =   SPACE
                   MOVE    SPA-H99NAI-RECE10-HKNID  (IDX)
                                           TO  CLAIMS-HKNID   (IDX)
                   MOVE    SPA-H99NAI-RECE10-HKNNUM (IDX)
                                           TO  CLAIMS-HKNNUM  (IDX)
               END-IF
      *
               EVALUATE    SPA-H99NAI-RECE10-TEISYUTUSAKI (IDX)
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "6"
                       MOVE    SPA-H99GMN-THKNJANUM     (IDX)
                                              TO  CLAIMS-HKNJANUM(IDX)
                       MOVE    SPA-H99NAI-RECE10-KIGO   (IDX)
                                              TO  CLAIMS-KIGO    (IDX)
                       MOVE    SPA-H99NAI-RECE10-NUM    (IDX)
                                              TO  CLAIMS-NUM     (IDX)
                       IF      SPA-H99NAI-RECE10-HOJOKBN (IDX)
                                          NOT =   "0"
                           MOVE    SPA-H99NAI-RECE10-HOJOKBN (IDX)
                                              TO  CLAIMS-HOJOKBN (IDX)
                       END-IF
      *
                       EVALUATE  SPA-H99NAI-RECE10-RECEIPT-KBN(IDX)(1:1)
                           WHEN    "1"
                           WHEN    "2"
                           WHEN    "3"
                           WHEN    "4"
                               MOVE    "True"          TO
                                           CLAIMS-INCARE-CLASS  (IDX)
                       END-EVALUATE
                       IF      SPA-H99GMN-TDISASTER (IDX) NOT =   SPACE
                               MOVE    "True"          TO
                                           CLAIMS-ACCIDENT-TARGET(IDX)
                       END-IF
      *
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY     >       4
                               OR      SPA-H99NAI-RECE10-KOHID(IDX IDY)
                                               =   ZERO
                           MOVE    SPA-H99NAI-RECE10-KOHID    (IDX IDY)
                                           TO  CLAIMS-KOHID (IDX IDY)
                           MOVE    SPA-H99NAI-RECE10-KOHNUM   (IDX IDY)
                                           TO  CLAIMS-KOHNUM (IDX IDY)
                           MOVE    SPA-H99NAI-RECE10-FTNJANUM (IDX IDY)
                                           TO  CLAIMS-FTNJANUM
                                                               (IDX IDY)
                           MOVE    SPA-H99NAI-RECE10-JKYSNUM  (IDX IDY)
                                           TO  CLAIMS-JKYSNUM  (IDX IDY)
                       END-PERFORM
                   WHEN    "3"
                       MOVE    SPA-H99NAI-RECE10-HKNKBN (IDX)
                                       TO   CLAIMS-ACCIDENT  (IDX)
                       EVALUATE    SPA-H99NAI-RECE10-RECESYUBETU (IDX)
                           WHEN    "7021"
      *                        アフターケア
                               MOVE    SPA-H99NAI-RECE10-KOFUNUM (IDX)
                                       TO
                                       CLAIMS-KANRITCHO-NUMBER (IDX)
                               MOVE    SPA-H99NAI-RECE10-DAMAGE-CLASS
                                                                 (IDX)
                                       TO  CLAIMS-DAMAGE-CLASS  (IDX)
                               MOVE    SPA-H99NAI-RECE10-TEKSTYMD (IDX)
                                       TO  WRK-SYMD
                               PERFORM 801-DAYHEN01-SEC
                               MOVE    WRK-HEN-DATE
                                       TO  CLAIMS-RYOYO-DATE (IDX)
                               MOVE    SPA-H99GMN-TKIGO(IDX) (9:)
                                       TO
                                       CLAIMS-DISEASE-LOCATION (IDX)
                           WHEN    "7051"
                           WHEN    "7061"
      *                        公務災害
                               MOVE    SPA-H99GMN-TKIGO(IDX)
                                       TO
                                       CLAIMS-DISEASE-LOCATION (IDX)
                               MOVE    SPA-H99NAI-RECE10-TEKSTYMD (IDX)
                                       TO  WRK-SYMD
                               PERFORM 801-DAYHEN01-SEC
                               MOVE    WRK-HEN-DATE
                                       TO  CLAIMS-START-DATE (IDX)
                           WHEN    OTHER
                               EVALUATE  SPA-H99NAI-RECE10-HKNKBN (IDX)
                                   WHEN    "1"
                                       MOVE
                                       SPA-H99NAI-RECE10-KOFUNUM (IDX)
                                           TO
                                           CLAIMS-ACCIDENT-NUM(IDX)
                                   WHEN    "2"
                                       MOVE
                                       SPA-H99NAI-RECE10-KOFUNUM (IDX)
                                           TO
                                           CLAIMS-PENSION-NUM (IDX)
                               END-EVALUATE
                               MOVE    SPA-H99NAI-RECE10-SAIGAIKBN(IDX)
                                           TO
                                           CLAIMS-ACCIDENT-CLASS (IDX)
                       END-EVALUATE
                   WHEN    "4"
                       MOVE    SPA-H99GMN-TKIGO (IDX)
                                       TO
                           CLAIMS-LI-DISEASE-LOCATION (IDX)
                       MOVE    SPA-H99NAI-RECE10-RECEKBN (IDX)
                                       TO  CLAIMS-FORM-STYLE (IDX)
                       MOVE    SPA-H99NAI-RECE10-TEKSTYMD (IDX)
                                       TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                       TO  CLAIMS-LI-START-DATE (IDX)
                   WHEN    "5"
                       MOVE    SPA-H99NAI-RECE10-KIGO  (IDX)
                                      TO  CLAIMS-POLLUTION-KIGO  (IDX)
                       MOVE    SPA-H99NAI-RECE10-NUM   (IDX) 
                                      TO  CLAIMS-POLLUTION-NUM   (IDX)
               END-EVALUATE
      *
      *        地方公費情報（存在する場合のみ返却）
               IF      SPA-H99NAI-LOCAL-KOHID (IDX 1)
                                      =   ZERO
                   CONTINUE
               ELSE
                   MOVE    SPA-H99NAI-LOCAL-SKYYM (IDX)
                                          TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                          TO
                                       CLAIMS-LOCAL-PUBLIC-SKYYM (IDX)
                   MOVE    SPA-H99NAI-LOCAL-HENREIYM (IDX)
                                          TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                          TO
                                   CLAIMS-LOCAL-PUBLIC-HENREIYM (IDX)
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       4
                                   OR      SPA-H99NAI-LOCAL-KOHID
                                                           (IDX IDY)
                                   =   ZERO
                       MOVE    SPA-H99NAI-LOCAL-KOHID  (IDX IDY)
                                      TO  CLAIMS-LOCAL-KOHID (IDX IDY)
                       MOVE    SPA-H99NAI-LOCAL-KOHNUM (IDX IDY)
                                      TO  CLAIMS-LOCAL-KOHNUM(IDX IDY)
                       MOVE    SPA-H99NAI-LOCAL-FTNJANUM  (IDX IDY)
                                      TO
                                      CLAIMS-LOCAL-FTNJANUM (IDX IDY)
                       MOVE    SPA-H99NAI-LOCAL-JKYSNUM (IDX IDY)
                                      TO
                                      CLAIMS-LOCAL-JKYSNUM   (IDX IDY)
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *####返戻情報一覧
          .
       20023-DETAIL-INF-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＵＩＤコード取得処理
      *****************************************************************
       20020-ORCAUID-SYORI-SEC        SECTION.
      *
           IF    ( CLAIMQ-REQUEST-NUMBER
                                       =   "01"  )
           AND   ( CLAIMQ-ORCA-UID     =   SPACE )
      *        UIDコード処理
               PERFORM 1001-UID-GET-SEC
               MOVE    C-UID               TO  WRK-ORCA-UID
           ELSE
               MOVE    CLAIMQ-ORCA-UID     TO  WRK-ORCA-UID
           END-IF
      * 
      *    UIDコードをTERMID
           MOVE    SPACE               TO  SPA-TERMID
           STRING  "API+"              DELIMITED   BY  SIZE
                   WRK-ORCA-UID        DELIMITED   BY  SPACE
                                       INTO    SPA-TERMID
           END-STRING
           .
       20020-ORCAUID-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    APIパラメタチェック処理
      *****************************************************************
       20021-PARA-INIT-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-API-PARA-FLG
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP43"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API43R1V3"         TO  APIPARA-FILEMEI
      *****MOVE    1                   TO  APIPARA-EDANUM
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 20021-PARA-READ-NEXT-SEC
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
      *
           PERFORM             UNTIL   FLG-APIPARA   =   1
               EVALUATE    APIPARA-EDANUM
                   WHEN    1
                       MOVE    APIPARA-REC     TO  WRK-APIPARA-REC
                       MOVE    APIPARA-DATA-REC
                                               TO  WRK-APIV3DATA-REC
                       ADD     1               TO  FLG-API-PARA-FLG
                   WHEN    2
                       MOVE    APIPARA-DATA-REC
                                               TO  WRK-APIV3DATA-REC1
                       ADD     1               TO  FLG-API-PARA-FLG
                   WHEN    3
                       MOVE    APIPARA-DATA-REC
                                               TO  WRK-APIV3DATA-REC2
                       ADD     1               TO  FLG-API-PARA-FLG
                   WHEN    4
                       MOVE    APIPARA-DATA-REC
                                               TO  WRK-APIV3DATA-REC3
                       ADD     1               TO  FLG-API-PARA-FLG
                   WHEN    5
                       MOVE    APIPARA-DATA-REC
                                               TO  WRK-APIV3DATA-REC4
                       ADD     1               TO  FLG-API-PARA-FLG
                   WHEN    6
                       MOVE    APIPARA-DATA-REC
                                               TO  WRK-APIV3DATA-REC5
                       ADD     1               TO  FLG-API-PARA-FLG
               END-EVALUATE
      *
               PERFORM 20021-PARA-READ-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-API-PARA-FLG    =   6
               MOVE    ZERO                TO  FLG-APIPARA
           ELSE
               MOVE    1                   TO  FLG-APIPARA
           END-IF
           .
       20021-PARA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    APIパラメタチェック処理
      *****************************************************************
       20021-PARA-READ-NEXT-SEC            SECTION.
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  APIPARA-REC
               MOVE    ZERO            TO  FLG-APIPARA
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           .
       20021-PARA-READ-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    APIパラメタ登録処理
      *****************************************************************
       20022-APIPARA-INS-SEC        SECTION.
      *
      *     APIパラメタ削除処理
           PERFORM 2902-PARA-ALLDEL-SEC
           IF      WRK-ERRCD       NOT = SPACE
               GO  TO  20022-APIPARA-INS-EXT
           END-IF
      *
      *    登録
           MOVE    SPACE               TO  WRK-APIV3DATA-REC
           INITIALIZE                      WRK-APIV3DATA-REC
      *
           MOVE    CLAIMQ-PATIENT-ID   TO  WRK-CLAIMQ-PATIENT-ID
           MOVE    CLAIMQ-PERFORM-MONTH
                                       TO  WRK-CLAIMQ-PERFORM-MONTH
           MOVE    CLAIMQ-SKYKBN       TO  WRK-CLAIMQ-SKYKBN
           MOVE    CLAIMQ-SKYYM        TO  WRK-CLAIMQ-SKYYM
           MOVE    CLAIMQ-HENREIYM     TO  WRK-CLAIMQ-HENREIYM
           MOVE    CLAIMQ-KOHSKY-SKYYM
                                       TO  WRK-CLAIMQ-KOHSKY-SKYYM
           MOVE    CLAIMQ-KOHSKY-HENREIYM
                                       TO  WRK-CLAIMQ-KOHSKY-HENREIYM
      *
           MOVE    WRK-SKYYM           TO  WRK-CLAIMQ-SKYYM-9
           MOVE    WRK-HENREIYM        TO  WRK-CLAIMQ-HENREIYM-9
           MOVE    WRK-KOHSKY-SKYYM    TO  WRK-CLAIMQ-KOHSKY-SKYYM-9
           MOVE    WRK-KOHSKY-HENREIYM TO  WRK-CLAIMQ-KOHSKY-HENREIYM-9
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       50
                   OR      CLAIMQ-DETAI-TEISYUTUSAKI (IDXX)
                                   =       SPACE
               MOVE   WRK-H99-INDEX (IDXX)
                                           TO  IDVV
               MOVE    SPA-H99NAI-RECE10-KEY (IDVV)
                                           TO  WRK-SPA-H99-KEY (IDXX)
      *
               MOVE    CLAIMQ-DETAI-TEISYUTUSAKI (IDXX)
                                           TO
                                   WRK-CLAIMQ-DETAI-TEISYUTUSAKI (IDXX)
               MOVE    CLAIMQ-DETAIL-INOUT (IDXX)
                                           TO
                                   WRK-CLAIMQ-DETAIL-INOUT (IDXX)
               MOVE    CLAIMQ-DETAIL-CODE  (IDXX)
                                           TO
                                   WRK-CLAIMQ-DETAIL-CODE (IDXX)
               MOVE    CLAIMQ-HKNID (IDXX) TO  WRK-CLAIMQ-HKNID   (IDXX)
               MOVE    CLAIMQ-HKNNUM (IDXX)
                                           TO  WRK-CLAIMQ-HKNNUM  (IDXX)
               MOVE    CLAIMQ-HKNJANUM (IDXX)
                                           TO  WRK-CLAIMQ-HKNJANUM(IDXX)
               MOVE    CLAIMQ-HOJOKBN (IDXX)
                                           TO  WRK-CLAIMQ-HOJOKBN (IDXX)
               MOVE    CLAIMQ-ACCIDENT-TARGET (IDXX)
                                           TO
                                   WRK-CLAIMQ-ACCIDENT-TARGET (IDXX)
               MOVE    CLAIMQ-INCARE-CLASS (IDXX)
                                           TO
                                   WRK-CLAIMQ-INCARE-CLASS (IDXX)
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       4
                   MOVE    CLAIMQ-KOHID  (IDXX IDYY)  TO
                                          WRK-CLAIMQ-KOHID  (IDXX IDYY)
                   MOVE    CLAIMQ-KOHNUM (IDXX IDYY)  TO
                                          WRK-CLAIMQ-KOHNUM (IDXX IDYY)
                   MOVE    CLAIMQ-FTNJANUM (IDXX IDYY)  TO
                                          WRK-CLAIMQ-FTNJANUM(IDXX IDYY)
               END-PERFORM
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       4
                   MOVE    CLAIMQ-LOCAL-KOHID  (IDXX IDYY)  TO
                                   WRK-CLAIMQ-LOCAL-KOHID  (IDXX IDYY)
                   MOVE    CLAIMQ-LOCAL-KOHNUM (IDXX IDYY)  TO
                                   WRK-CLAIMQ-LOCAL-KOHNUM (IDXX IDYY)
               END-PERFORM
               MOVE    CLAIMQ-ACCIDENT (IDXX)
                                           TO
                                   WRK-CLAIMQ-ACCIDENT (IDXX)
               MOVE    CLAIMQ-ACCIDENT-NUM (IDXX)
                                           TO
                                   WRK-CLAIMQ-ACCIDENT-NUM (IDXX)
               MOVE    CLAIMQ-PENSION-NUM (IDXX)
                                           TO
                                   WRK-CLAIMQ-PENSION-NUM (IDXX)
               MOVE    CLAIMQ-ACCIDENT-CLASS (IDXX)
                                           TO
                                   WRK-CLAIMQ-ACCIDENT-CLASS (IDXX)
               MOVE    CLAIMQ-KANRITCHO-NUMBER (IDXX)
                                           TO
                                   WRK-CLAIMQ-KANRITCHO-NUMBER (IDXX)
               MOVE    CLAIMQ-RYOYO-DATE (IDXX)
                                           TO
                                   WRK-CLAIMQ-RYOYO-DATE (IDXX)
               MOVE    CLAIMQ-START-DATE (IDXX)
                                           TO
                                   WRK-CLAIMQ-START-DATE (IDXX)
               MOVE    CLAIMQ-FORM-STYLE (IDXX)
                                           TO
                                   WRK-CLAIMQ-FORM-STYLE (IDXX)
               MOVE    CLAIMQ-LI-START-DATE (IDXX)
                                           TO
                                   WRK-CLAIMQ-LI-START-DATE (IDXX)
               MOVE    CLAIMQ-POLLUTION-KIGO (IDXX)
                                           TO
                                   WRK-CLAIMQ-POLLUTION-KIGO (IDXX)
               MOVE    CLAIMQ-POLLUTION-NUM (IDXX)
                                           TO
                                   WRK-CLAIMQ-POLLUTION-NUM (IDXX)
           END-PERFORM
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6 
               INITIALIZE                      APIPARA-REC
               MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
               MOVE    "AP43"              TO  APIPARA-GYOUMUID
               MOVE    SPA-TERMID          TO  APIPARA-TERMID
               MOVE    "API43R1V3"         TO  APIPARA-FILEMEI
               MOVE    IDXX                TO  APIPARA-EDANUM
               MOVE    CLAIMQ-KARTE-UID    TO  APIPARA-KARTE-UID
               MOVE    CLAIMQ-PATIENT-ID   TO  APIPARA-PTNUM
      *
               MOVE    SMCNDATE-YMD        TO  APIPARA-UPYMD
               MOVE    SMCNDATE-HMS        TO  APIPARA-UPHMS
      *
               EVALUATE    IDXX
                   WHEN    1
                       MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
                   WHEN    2
                       MOVE    WRK-APIV3DATA-REC1  TO  APIPARA-DATA-REC
                   WHEN    3
                       MOVE    WRK-APIV3DATA-REC2  TO  APIPARA-DATA-REC
                   WHEN    4
                       MOVE    WRK-APIV3DATA-REC3  TO  APIPARA-DATA-REC
                   WHEN    5
                       MOVE    WRK-APIV3DATA-REC4  TO  APIPARA-DATA-REC
                   WHEN    6
                       MOVE    WRK-APIV3DATA-REC5  TO  APIPARA-DATA-REC
               END-EVALUATE
      *
               MOVE    APIPARA-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E80"               TO  WRK-ERRCD
                   DISPLAY "API PARA INSERT ERR:"  MCP-RC
                           ",KEY:" APIPARA-KEY
                   GO  TO  20022-APIPARA-INS-EXT
               END-IF
           END-PERFORM
           .
       20022-APIPARA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理更新処理
      *****************************************************************
       2003-CLAIMED-UPDATE-SEC      SECTION.
      *
      *    オルカＵＩＤ処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *    APIパラメタチェック処理
           PERFORM 20021-PARA-INIT-SEC
      *
           IF      FLG-APIPARA     =   ZERO
               IF    ( CLAIMQ-KARTE-UID
                                   NOT =   WRK-APIPARA-KARTE-UID  )
                   MOVE    "E41"           TO  WRK-ERRCD
               ELSE 
                   IF    ( SPA-TERMID  NOT =   WRK-APIPARA-TERMID )
                       MOVE    "E43"           TO  WRK-ERRCD
                   ELSE 
                       IF    ( CLAIMQ-PERFORM-MONTH
                                   NOT =   WRK-CLAIMQ-PERFORM-MONTH   )
                       OR    ( CLAIMQ-PATIENT-ID
                                   NOT =   WRK-CLAIMQ-PATIENT-ID      )
                       OR    ( CLAIMQ-SKYKBN
                                   NOT =   WRK-CLAIMQ-SKYKBN          )
                       OR    ( CLAIMQ-SKYKBN
                                   NOT =   WRK-CLAIMQ-SKYKBN          )
                       OR    ( CLAIMQ-SKYYM
                                   NOT =   WRK-CLAIMQ-SKYYM           )
                       OR    ( CLAIMQ-HENREIYM
                                   NOT =   WRK-CLAIMQ-HENREIYM        )
                       OR    ( CLAIMQ-KOHSKY-SKYYM
                                   NOT =   WRK-CLAIMQ-KOHSKY-SKYYM    )
                       OR    ( CLAIMQ-KOHSKY-HENREIYM
                                   NOT =   WRK-CLAIMQ-KOHSKY-HENREIYM )
                           MOVE    "E42"           TO  WRK-ERRCD
                           GO  TO  2003-CLAIMED-UPDATE-EXT
                       ELSE
                           PERFORM VARYING IDXX   FROM    1   BY 1
                                   UNTIL   IDXX   >       50
                               IF    ( CLAIMQ-DETAI-TEISYUTUSAKI (IDXX)
                                                NOT =
                                       WRK-CLAIMQ-DETAI-TEISYUTUSAKI
                                                               (IDXX) )
                               OR    ( CLAIMQ-DETAIL-INOUT (IDXX)
                                               NOT =
                                       WRK-CLAIMQ-DETAIL-INOUT (IDXX) )
                               OR    ( CLAIMQ-DETAIL-CODE (IDXX)
                                               NOT =
                                       WRK-CLAIMQ-DETAIL-CODE (IDXX)  )
                               OR    ( CLAIMQ-HKNID (IDXX)
                                           NOT =
                                       WRK-CLAIMQ-HKNID (IDXX)        )
                               OR    ( CLAIMQ-HKNNUM (IDXX)
                                           NOT =
                                       WRK-CLAIMQ-HKNNUM (IDXX)       )
                               OR    ( CLAIMQ-HKNJANUM (IDXX)
                                           NOT =
                                       WRK-CLAIMQ-HKNJANUM (IDXX)     )
                               OR    ( CLAIMQ-HOJOKBN (IDXX)
                                           NOT =
                                       WRK-CLAIMQ-HOJOKBN (IDXX)      )
                               OR    ( CLAIMQ-ACCIDENT-TARGET (IDXX)
                                           NOT =
                                       WRK-CLAIMQ-ACCIDENT-TARGET
                                                               (IDXX) )
                               OR    ( CLAIMQ-INCARE-CLASS (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-INCARE-CLASS(IDXX) )
                                   MOVE    "E42"           TO  WRK-ERRCD
                                   GO  TO  2003-CLAIMED-UPDATE-EXT
                               END-IF
                               IF    ( CLAIMQ-ACCIDENT (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-ACCIDENT (IDXX) )
                               OR    ( CLAIMQ-ACCIDENT-NUM (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-ACCIDENT-NUM
                                                               (IDXX) )
                               OR    ( CLAIMQ-PENSION-NUM (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-PENSION-NUM(IDXX)  )
                               OR    ( CLAIMQ-ACCIDENT-CLASS (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-ACCIDENT-CLASS
                                                               (IDXX) )
                               OR    ( CLAIMQ-KANRITCHO-NUMBER (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-KANRITCHO-NUMBER
                                                               (IDXX) )
                               OR    ( CLAIMQ-RYOYO-DATE (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-RYOYO-DATE (IDXX)  )
                               OR    ( CLAIMQ-START-DATE (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-START-DATE (IDXX)  )
                                   MOVE    "E42"           TO  WRK-ERRCD
                                   GO  TO  2003-CLAIMED-UPDATE-EXT
                               END-IF
                               IF    ( CLAIMQ-FORM-STYLE (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-FORM-STYLE (IDXX)  )
                               OR    ( CLAIMQ-LI-START-DATE (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-LI-START-DATE
                                                               (IDXX) )
                               OR    ( CLAIMQ-POLLUTION-KIGO (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-POLLUTION-KIGO
                                                               (IDXX) )
                               OR    ( CLAIMQ-POLLUTION-NUM (IDXX)
                                           NOT =
                                        WRK-CLAIMQ-POLLUTION-NUM
                                                               (IDXX) )
                                   MOVE    "E42"           TO  WRK-ERRCD
                                   GO  TO  2003-CLAIMED-UPDATE-EXT
                               END-IF
                               PERFORM VARYING IDYY   FROM    1   BY 1
                                       UNTIL   IDYY   >       4
                                   IF     ( CLAIMQ-KOHID (IDXX IDYY)
                                               NOT =
                                       WRK-CLAIMQ-KOHID    (IDXX IDYY) )
                                   OR    ( CLAIMQ-KOHNUM (IDXX IDYY)
                                               NOT =
                                       WRK-CLAIMQ-KOHNUM   (IDXX IDYY) )
                                   OR    ( CLAIMQ-FTNJANUM (IDXX IDYY)
                                               NOT =
                                       WRK-CLAIMQ-FTNJANUM (IDXX IDYY) )
                                   OR    ( CLAIMQ-LOCAL-KOHID
                                                         (IDXX IDYY)
                                               NOT =
                                       WRK-CLAIMQ-LOCAL-KOHID
                                                           (IDXX IDYY) )
                                   OR    ( CLAIMQ-LOCAL-KOHNUM
                                                         (IDXX IDYY)
                                               NOT =
                                       WRK-CLAIMQ-LOCAL-KOHNUM
                                                           (IDXX IDYY) )
                                       MOVE    "E42"       TO  WRK-ERRCD
                                       GO  TO  2003-CLAIMED-UPDATE-EXT
                                   END-IF
                               END-PERFORM
                           END-PERFORM
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    "E08"           TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD      NOT =   SPACE
               GO  TO  2003-CLAIMED-UPDATE-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    請求管理の更新
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >       50
                   OR      WRK-CLAIMQ-DETAI-TEISYUTUSAKI (IDYY)
                                   =       SPACE
               MOVE    SPACE           TO  RECE10-REC
               INITIALIZE                  RECE10-REC
               MOVE    WRK-SPA-H99-KEY (IDYY)
                                       TO  RECE10-KEY
               PERFORM 900-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    "E61"               TO  WRK-ERRCD
                   GO  TO  2003-CLAIMED-UPDATE-EXT
               END-IF
      *
               MOVE    WRK-CLAIMQ-SKYYM-9  TO  RECE10-SKYYM
               MOVE    WRK-CLAIMQ-HENREIYM-9
                                           TO  RECE10-HENREIYM
               MOVE    WRK-CLAIMQ-SKYKBN   TO  RECE10-SKYKBN
               MOVE    "1"                 TO  RECE10-TEISEIKBN
      *     
               MOVE    SMCNDATE-YMD        TO  RECE10-UPDYMD
               MOVE    SMCNDATE-HMS        TO  RECE10-UPDHMS
      *
               MOVE    RECE10-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E62"               TO  WRK-ERRCD
                   GO  TO  2003-CLAIMED-UPDATE-EXT
               END-IF
      *
      *        公費請求書があれば更新する
               MOVE    SPACE           TO  KOHSKY-REC
               INITIALIZE                  KOHSKY-REC
               MOVE    WRK-SPA-H99-KEY (IDYY)
                                       TO  KOHSKY-KEY
               PERFORM 900-KOHSKY-READ-SEC
               IF      FLG-KOHSKY          =   ZERO
                   MOVE    MCPDATA-REC         TO  KOHSKY-REC
      *
                   IF    ( WRK-CLAIMQ-LOCAL-PUBLIC (IDYY)
                                               =   SPACE         )
                   AND   ( WRK-CLAIMQ-SKYKBN   =   KOHSKY-SKYKBN )
                       CONTINUE
                   ELSE
                       IF      WRK-CLAIMQ-LOCAL-PUBLIC (IDYY)
                                               NOT =   SPACE
                           MOVE    WRK-CLAIMQ-KOHSKY-SKYYM-9
                                                   TO  KOHSKY-SKYYM
                           MOVE    WRK-CLAIMQ-KOHSKY-HENREIYM-9
                                                   TO  KOHSKY-HENREIYM
                       END-IF
                       MOVE    WRK-CLAIMQ-SKYKBN   TO  KOHSKY-SKYKBN
                       MOVE    "1"                 TO  KOHSKY-TEISEIKBN
                       MOVE    SMCNDATE-YMD        TO  KOHSKY-UPDYMD
      *
                       MOVE    KOHSKY-REC          TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_kohsky"        TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
      *
                       IF      MCP-RC          NOT =   ZERO
                           MOVE    "E63"               TO  WRK-ERRCD
                           GO  TO  2003-CLAIMED-UPDATE-EXT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *     APIパラメタ削除処理
           PERFORM 2902-PARA-ALLDEL-SEC
           IF      WRK-ERRCD       NOT = SPACE
             GO  TO  2003-CLAIMED-UPDATE-EXT
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
           .
       2003-CLAIMED-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-JOBKANRI-CHK-SEC       SECTION.
       *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "E70"           TO  WRK-ERRCD
                   STRING  "レセプト作成処理中です【"
                                           DELIMITED  BY  SIZE
                           JOB-SHELLMSG    DELIMITED  BY  SPACE
                           "】"            DELIMITED  BY  SIZE
                                           INTO  WRK-ERRMSG
                   END-STRING
                   GO  TO  800-JOBKANRI-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID1
                                           TO  JOB-SHELLID
                   PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "E70"           TO  WRK-ERRCD
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-ERRMSG
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
           .
       800-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    UIDコード取得処理
      *****************************************************************
       1001-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           .
       1001-UID-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *     APIパラメタ削除処理
      *****************************************************************
       2902-PARA-ALLDEL-SEC          SECTION.
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP43"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API43R1V3"         TO  APIPARA-FILEMEI
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    "E81"               TO  WRK-ERRCD
           END-IF
      *
           .
       2902-PARA-ALLDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理処理
      *****************************************************************
       800-ORCGH01-CALL-SEC                SECTION.
      *
           MOVE    SPA-H01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-H01FREE     TO  SPA-FREE
      *
           CALL    "ORCGH01"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-H01FREE
           MOVE    SPA-WORK        TO  SPA-H01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGH01-CALL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    請求管理処理
      *****************************************************************
       800-ORCGH99-CALL-SEC                SECTION.
      *
           MOVE    SPA-H01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-H01FREE     TO  SPA-FREE
      *
           CALL    "ORCGH99"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-H01FREE
           MOVE    SPA-WORK        TO  SPA-H01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGH99-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE   OR  "000000"
               MOVE    SPACE               TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換処理
      *****************************************************************
       803-HIZUKE-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-HENYMDG3
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE OR
                                           ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
               EVALUATE    WRK-SYMD (1:6)
                   WHEN    "000000"
                       MOVE    "００００００００"  TO  WRK-HENYMDG3
                   WHEN    "999999"
                       MOVE    "９９９９９９９９"  TO  WRK-HENYMDG3
               END-EVALUATE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           END-IF
      *
           .
       803-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E70"
      *            レセプト作成の処理中
                   GO  TO  890-ERRCD-MSG-EXT
           END-EVALUATE
      * 
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E01"
                   MOVE    "患者番号が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E04"
                   MOVE    "電子カルテＵＩＤが未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E05"
                   MOVE    "オルカＵＩＤが未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E08"
                   STRING  "初回接続が完了していません。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    "E10"
                   MOVE    "該当の患者が存在しません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E11"
                   MOVE    "診療年月が暦日ではありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
                   MOVE    "入外区分の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E14"
                   MOVE    "診療年月が未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "対象のレセプトは存在しません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E16"
                   MOVE    "請求区分の設定に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E17"
                   MOVE    "返戻年月が暦日ではありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E18"
                   MOVE    "請求年月（地方公費）が暦日ではありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E19"
                   MOVE    "返戻年月（地方公費）が暦日ではありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E20"
                   MOVE    "レセプト情報の設定がありません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E21"
                   MOVE    "請求年月は診療年月以降で設定して下さい。"
                                                   TO  WRK-ERRMSG
               WHEN    "E22"
                   MOVE    "返戻年月は診療年月以降で設定して下さい。"
                                                   TO  WRK-ERRMSG
               WHEN    "E23"
                   MOVE
                  "請求年月（地方公費）は診療年月以降で設定して下さい。"
                                                   TO  WRK-ERRMSG
               WHEN    "E24"
                   MOVE
                 "返戻年月（地方公費）は診療年月以降で設定して下さい。"
                                                   TO  WRK-ERRMSG
               WHEN    "E25"
                   MOVE    "請求年月が暦日ではありません。"
                                                   TO  WRK-ERRMSG
       *
               WHEN    "E40"
               WHEN    "E41"
                   MOVE    "リクエスト０１の処理が行われていません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E42"
                   MOVE    "前回と設定内容が違います。"
                                                   TO  WRK-ERRMSG
               WHEN    "E43"
                   MOVE    "オルカＵＩＤが違います。"
                                                   TO  WRK-ERRMSG
      *
               WHEN    "E60"
      *            画面エラー
                   MOVE    SPA-ERRMSG              TO  WRK-ERRMSG
               WHEN    "E61"
                  MOVE    "対象の請求管理が存在しませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E62"
                  MOVE    "請求管理が更新できませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E63"
                  MOVE    "公費請求書が更新できませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E69"
                   MOVE    "設定内容に誤りがあります。"
                                                   TO  WRK-ERRMSG
       *
               WHEN    "E80"
                   MOVE    "一時データが更新できませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E81"
                  MOVE    "一時データが削除できませんでした。"
                                                   TO  WRK-ERRMSG
      *        共通エラー
               WHEN    "E89"
      *            ORCSCOMMON のエラー
                   EVALUATE    SPA-ERRCD
                       WHEN    "1001"
                           MOVE    "職員情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1002"
                           MOVE    "医療機関情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1003"
                           MOVE    "システム日付が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1005"
                           MOVE    "患者番号構成情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1015"
                           STRING  "グループ医療機関が不整合です。"
                               "処理を終了して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                           END-STRING
                       WHEN    OTHER
                           MOVE    "システム項目が設定できません。"
                                                   TO  WRK-ERRMSG
                   END-EVALUATE
               WHEN    "E90"
                   MOVE    "他端末で使用中です。"  TO  WRK-ERRMSG
               WHEN    "E91"
                   MOVE    "リクエスト番号が不正です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E97"
                   MOVE   "送信内容に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E98"
                   MOVE   "送信内容の読込ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E99"
                   MOVE    "ユーザＩＤが未登録です。"
                                                   TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスター読込
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kohsky"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-KOHSKY
                   MOVE    MCPDATA-REC         TO  KOHSKY-REC
               ELSE
                   MOVE    1                   TO  FLG-KOHSKY
               END-IF
           ELSE
               MOVE    1                   TO  FLG-KOHSKY
           END-IF
      *
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災患者保険情報読込
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（主キー）
      *****************************************************************
       900-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SEIKYU
               ELSE
                   MOVE    1               TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       900-APIPARA-READ-SEC         SECTION.
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBFETCH"           TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  APIPARA-REC
                   MOVE    ZERO            TO  FLG-APIPARA
               ELSE
                   INITIALIZE                  APIPARA-REC
                   MOVE    1               TO  FLG-APIPARA
               END-IF
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-APIPARA-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
           MOVE    "H01"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-H01NAI-PTID     TO  SLOCK-PTID
      *    電子カルテキー
           MOVE    CLAIMQ-KARTE-UID    TO  SLOCK-KARTE-UID
      *    API+オルカＵＩＤ をTERMID
           MOVE    SPA-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理(削除)
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    "H01"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
      *    API+オルカＵＩＤ をTERMID
           MOVE    SPA-TERMID          TO  MCP-TERM
      *
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MCP-TERM        TO  MCP-TERM
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
