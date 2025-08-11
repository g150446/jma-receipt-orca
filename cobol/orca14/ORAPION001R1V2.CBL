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
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.          ORAPION001R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : オンライン資格確認
      *  管理者            :
      *  作成日付    作業者        記述
      *  20/11/06    NACL−藤原    新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01     ORCAMO       21/06/10  資格確認：要求ファイル名修正
      *  05.00.02     ORCAMO       21/10/22  資格確認：uuid で最新の検索をする
      *  05.01.01     ORCAMO       22/01/07  資格確認：限度額適用認定証提
      *                                      供同意フラグの設定
      *  05.01.02     ORCAMO       22/01/07  資格確認：照会番号に患者番号
      *                                      を返却
      *  05.01.03    ORCAMO        22/07/15  資格確認：診療情報対応
      *  05.01.04    ORCAMO        22/08/22  保険証ＯＣＲ(アルメックス）対応
      *  05.01.05    ORCAMO        22/12/09  処方箋発行形態対応
      *  05.02.01    ORCAMO        24/05/31  医療機関コード設定を修正
      *  05.02.02    ORCAMO        24/11/15  訪問診療対応
      *  05.02.03    ORCAMO        25/06/09  旧医療機関コード対応
      ******************************************************************
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-ONSHI-KAKU          PIC 9(01).
           03  FLG-ONSHI-YKZ           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-ONSHI-HOUMON        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HOSPCD              PIC X(10).
           03  WRK-SIKIBETU            PIC X(50).
           03  WRK-PTID                PIC 9(10).
           03  WRK-PATIENT-ID          PIC X(20).
           03  WRK-OPID                PIC X(16).
           03  WRK-ONSKAKU-SIKAKU-YUKO PIC X(01).
      *         
           03  WRK-FILE-PATIENT-ID     PIC X(20).
       *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-TABLE           PIC X(62).
      *
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
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-WARNING             PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
      *
           03  WRK-SPA-SYSYMD-1        PIC X(08).     
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUAREQ1.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUARES1.INC".
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1001.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1051.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    資格確認テーブル
       01  ONSKAKU-REC.       
           COPY    "CPONSHI-KAKU.INC".
      *
      *    資格確認薬剤情報
       01  ONSHI-YAKUZAI-MAIN-REC.       
           COPY    "CPONSHI-YAKUZAI-MAIN.INC".
      *
      *    資格確認訪問診療
       01  ONSHI-HOUMON-REC.
           COPY    "CPONSHI-HOUMON.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA14SCRAREA.INC".
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
           DISPLAY   "**********************"
           DISPLAY   "*  onlinequa1 start  *"
           DISPLAY   "**********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *****INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
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
           DISPLAY   "**********************"
           DISPLAY   "*   onlinequa1 end   *"
           DISPLAY   "**********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      XML-ONLINEQUARES1
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      **** MOVE    "20281010"          TO  SMCNDATE-YMD
      *
           STRING  "A+"                DELIMITED  BY  SIZE
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    システム日付の前月      
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SPA-SYSYMD-1
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
               MOVE   "E89"            TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      *
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               MOVE    "E02"           TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      *     
      *    オンライン資格確認照会情報取得 
           INITIALIZE                  SYS-1051-REC
           MOVE    "1051"              TO  SYS-1051-KANRICD
           MOVE    "*"                 TO  SYS-1051-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1051-STYUKYMD
                                           SYS-1051-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1051-HOSPNUM
           MOVE    SYS-1051-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1051-REC
           ELSE
               INITIALIZE                  SYS-1051-REC
           END-IF
      *
      *    医療機関コード設定
           STRING  SPA-PREFNUM         DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN  DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD     DELIMITED  BY  SIZE
                                       INTO    WRK-HOSPCD
           END-STRING
      *    旧医療機関コードが設定されているとき
           IF      SYS-1051-HOSPCDF    =   SPACE
               CONTINUE
           ELSE
               MOVE    SYS-1051-HOSPCDF    TO  WRK-HOSPCD
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
           MOVE    SPACE           TO  WRK-ERRCD
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    資格確認依頼処理
           PERFORM 2002-SIKAKU-KAKUNIN-IRAI-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAREQ1
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinequareq1" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST-BODY         TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinequareq1"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAREQ1
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
               CONTINUE
           ELSE
               DISPLAY "300 WRK-ERRCD=" WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  QUARES1-API-RESULT
               MOVE    WRK-ERRMSG      TO  QUARES1-API-RESULT-MSG
            END-IF
      *
           IF      APILST-BODY        NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinequares1" TO  MCP-TABLE
           MOVE    1                   TO  QUARES1-MODE
           MOVE    "onlinequares1"     TO  QUARES1-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINEQUARES1
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  QUARES1-OBJECT    TO  APILST-BODY
               MOVE  "application/xml" TO  APILST-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           IF      ONLINEQUAREQ1-DATAREQ
                               =   SPACE 
               MOVE    "E15"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    実施日
           IF      ONLINEQUAREQ1-PERFORM-DATE
                               =   SPACE
               MOVE    "E17"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           ELSE
               MOVE    ONLINEQUAREQ1-PERFORM-DATE
                                           TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
               IF      STS-DAY-RC1         NOT =   ZERO
                  MOVE    "E16"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
      *    患者ＩＤに変換             
           IF      ONLINEQUAREQ1-PATIENT-ID
                                   =   SPACE
               MOVE    ZERO            TO  WRK-PTID
               MOVE    SPACE           TO  WRK-PATIENT-ID
           ELSE
               INITIALIZE                  ORCSPTNUMAREA
               MOVE    ONLINEQUAREQ1-PATIENT-ID
                                      TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"    USING   ORCSPTNUMAREA
                                              SPA-AREA
               IF      SPTNUM-RC       =   00
                   MOVE    SPTNUM-PTID     TO  WRK-PTID
                   MOVE    SPTNUM-PTNUM    TO  WRK-PATIENT-ID
               ELSE
                   DISPLAY "PTID ERROR=" ONLINEQUAREQ1-PATIENT-ID
                   MOVE    "E05"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           IF      ONLINEQUAREQ1-UUID  =   SPACE
               MOVE    "E13"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
      *****MOVE    WRK-PTID        TO  ONSKAKU-PTID
           MOVE    ONLINEQUAREQ1-UUID
                                   TO  ONSKAKU-TBL-UUID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           PERFORM 900-ONSHI-KAKU-READ-SEC
           IF      FLG-ONSHI-KAKU  =   ZERO
               CONTINUE
           ELSE
               MOVE    "E07"           TO  WRK-ERRCD    
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認依頼処理
      *****************************************************************
       2002-SIKAKU-KAKUNIN-IRAI-SEC    SECTION.
      * 
      *    任意のファイル識別子(医療機関固有項目)の設定 
           STRING  WRK-PTID                DELIMITED  BY  SPACE
                   ","                     DELIMITED  BY  SIZE  
                   ONLINEQUAREQ1-UUID      DELIMITED  BY  SIZE
                                           INTO    WRK-SIKIBETU
           END-STRING
      *
      *    保険証ＯＣＲのとき
           IF      ONSKAKU-ORCA-SHOKAI-KBN
                                   =   "2"
               EVALUATE    ONSKAKU-SIKAKU-YUKO
                   WHEN    "5"
                       MOVE    SPACE       TO  ONSKAKU-SIKAKU-YUKO
                       MOVE    "5"         TO  WRK-ONSKAKU-SIKAKU-YUKO
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
                       MOVE    ONSKAKU-SIKAKU-YUKO
                                           TO  WRK-ONSKAKU-SIKAKU-YUKO
               END-EVALUATE
           END-IF
      *
           EVALUATE    ONSKAKU-KAKU-ENDFLG
               WHEN    SPACE
                   PERFORM 20021-RES1-HENSYU-SEC
                   PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
               WHEN    "01"
                   CONTINUE
               WHEN    "02"
                   PERFORM 20021-RES1-HENSYU-SEC
                   PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
               WHEN    "03"
                   IF      ONSKAKU-SIKAKU-YUKO =   "1" OR  "2" OR  "3"
                       PERFORM 20021-RES1-HENSYU-SEC
                       PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
                   END-IF    
           END-EVALUATE
      *
           .
       2002-SIKAKU-KAKUNIN-IRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    顔認証資格確認登録、更新処理
      *****************************************************************
       20021-RES1-HENSYU-SEC           SECTION.   
      *
      *    要求ファイル用患者番号編集 
           MOVE    SPACE       TO  WRK-FILE-PATIENT-ID
           STRING  WRK-PATIENT-ID  DELIMITED  BY  SPACE
                   "XXXXXXXXXXXXXXXXXXXX"
                                   DELIMITED  BY  SIZE
                                   INTO    WRK-FILE-PATIENT-ID
           END-STRING                        
      *
           MOVE    SPACE               TO  QUARES1-QUARES1
           MOVE    ONLINEQUAREQ1-PATIENT-ID
                                       TO  QUARES1-PATIENT-ID
           MOVE    SPA-SYSYMD          TO  QUARES1-KAKUNINYMD
           MOVE    WRK-HOSPCD          TO  QUARES1-HOSPCD
           MOVE    WRK-SIKIBETU        TO  QUARES1-SIKIBETU
      *          
           EVALUATE    ONSKAKU-KAKU-ENDFLG
               WHEN    SPACE
               WHEN    "02"
                   MOVE    ONSKAKU-SHO-HKNJANUM
                                               TO  QUARES1-QUA-HKNJANUM
                   MOVE    ONSKAKU-SHO-KIGO    TO  QUARES1-QUA-KIGO
                   MOVE    ONSKAKU-SHO-NUM     TO  QUARES1-QUA-NUM
                   MOVE    ONSKAKU-SHO-EDABAN  TO  QUARES1-QUA-EDABAN
                   MOVE    ONSKAKU-SHO-BIRTHDAY
                                               TO  QUARES1-QUA-BIRTHDAY
                   IF      ONSKAKU-SHOKAI-KBN
                                               =   "2" OR  "3"
                     IF      ONSKAKU-SHOKAI-KBN = "2"
                           MOVE    "1"             TO   
                                               QUARES1-QUA-DOUIFLG
                     END-IF
                   ELSE    
                       MOVE    ONSKAKU-SHO-DOUIFLG
                                               TO  QUARES1-QUA-DOUIFLG
                   END-IF
                   MOVE    WRK-SIKIBETU        TO  QUARES1-QUA-SIKIBETU
                   MOVE    ONSKAKU-SHO-SHOHO-KEITAI
                                               TO
                                               QUARES1-QUA-SHOHO-KEITAI
                   MOVE    ONSKAKU-ONSHINRYO-KBN
                                               TO
                                               QUARES1-QUA-HOU-SRYKBN
      *                                         
                   MOVE    "01"                TO  ONSKAKU-KAKU-ENDFLG
               WHEN    "03"
      *            オンライン資格確認照会情報取得 
                   INITIALIZE                  SYS-1051-REC
                   MOVE    "1051"              TO  SYS-1051-KANRICD
                   MOVE    "*"                 TO  SYS-1051-KBNCD
                   MOVE    SPA-SYSYMD          TO  SYS-1051-STYUKYMD
                                                   SYS-1051-EDYUKYMD 
                   MOVE    SPA-HOSPNUM         TO  SYS-1051-HOSPNUM
                   MOVE    SYS-1051-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-READ-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1051-REC
                   ELSE
                       INITIALIZE                  SYS-1051-REC
                   END-IF
      *
                   IF      ONSKAKU-SIKAKU-YUKO =   "1" OR  "2" OR  "3"
      *                照会番号が空白                 
                       IF      ONSKAKU-SHOKAI-NUM  =   SPACE
                           MOVE    "01"        TO  ONSKAKU-SHOU-ENDFLG
      *                     
                           MOVE    "True"      TO  QUARES1-REF-CLASS
                           MOVE    WRK-HOSPCD  TO  QUARES1-REF-HOSPCD
                           MOVE    WRK-SIKIBETU
                                               TO  QUARES1-REF-SIKIBETU
                           MOVE    WRK-PATIENT-ID
                                               TO
                                               QUARES1-REF-SHOKAI-NUM
                           MOVE    ONSKAKU-RES-HKNJANUM
                                               TO  QUARES1-REF-HKNJANUM
                           MOVE    ONSKAKU-RES-KIGO
                                               TO  QUARES1-REF-KIGO
                           MOVE    ONSKAKU-RES-NUM
                                               TO  QUARES1-REF-NUM
                           MOVE    ONSKAKU-RES-EDABAN
                                               TO  QUARES1-REF-EDABAN
                       END-IF
      *                薬剤情報閲覧同意フラグが１（同意）で薬剤情報得依頼済フラグが空白
                       IF    ( ONSKAKU-YAKUZAI-DOUIFLG
                                                   =   "1"   )
                       AND   ( ONSKAKU-YAKUZAI-ENDFLG
                                                   =   SPACE )
                           MOVE    "01"        TO
                                               ONSKAKU-YAKUZAI-ENDFLG
      *                                         
                           MOVE    "True"      TO  QUARES1-DRG-CLASS
      *                    マシン日付から一年前を取得
                           INITIALIZE              STS-AREA-DAY
                           INITIALIZE              LNK-DAY6-AREA
                           MOVE    "61"        TO  LNK-DAY6-IRAI
                           MOVE    SPA-SYSYMD  TO  LNK-DAY6-KIJUN
                           MOVE    "2"         TO  LNK-DAY6-ZOGENPTN
                           MOVE    -12         TO  LNK-DAY6-ZOGEN
                           CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
      *??
          display "spaymd-12=" SPA-SYSYMD "#" LNK-DAY6-KEISAN "#"                                                  
      *                    資格薬剤情報を検索
                           INITIALIZE          ONSHI-YAKUZAI-MAIN-REC
                           MOVE    SPA-HOSPNUM TO  ONS-YKZ-M-HOSPNUM
                           MOVE    WRK-PTID    TO  ONS-YKZ-M-PTID
                           MOVE    ONSHI-YAKUZAI-MAIN-REC
                                               TO  MCPDATA-REC
                           MOVE    "key2"      TO  WRK-MCP-PATHNAME
                           PERFORM 900-ONSHI-YKZ-READ-SEC
                           IF      FLG-ONSHI-YKZ   =   ZERO
                               IF      ONS-YKZ-M-SRYYM
                                               <   LNK-DAY6-KEISAN(1:6)
                                   MOVE    1       TO  FLG-ONSHI-YKZ
                               END-IF
                           END-IF
      *                    資格薬剤情報が一年以内に存在しない場合
                           IF      FLG-ONSHI-YKZ
                                               =   1
                               MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO  ONS-YKZ-M-SRYYM
                           END-IF                    
      *??
          display "FLG-ONSHI-YKZ=" FLG-ONSHI-YKZ "#" ONS-YKZ-M-SRYYM "#"                                                  
      *
                           MOVE    ZERO    TO  IDX 
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       12
                               INITIALIZE          STS-AREA-DAY
                               INITIALIZE          LNK-DAY6-AREA
                               MOVE    "61"    TO  LNK-DAY6-IRAI
                               MOVE    SPA-SYSYMD (1:6)
                                               TO  LNK-DAY6-KIJUN (1:6)
                               MOVE    "01"    TO  LNK-DAY6-KIJUN (7:2)
                               MOVE    "2"     TO  LNK-DAY6-ZOGENPTN
                               COMPUTE LNK-DAY6-ZOGEN
                                               =   IDXX    *   (-1) 
                               CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                           LNK-DAY6-AREA
                               IF      LNK-DAY6-KEISAN(1:6)
                                               <   ONS-YKZ-M-SRYYM
                                   MOVE    13  TO  IDXX
                               ELSE
                                   ADD     1   TO  IDX
                                   MOVE    WRK-HOSPCD
                                           TO  QUARES1-DRG-HOSPCD  (IDX)
                                   MOVE    ONSKAKU-RES-HKNJANUM
                                           TO
                                           QUARES1-DRG-HKNJANUM    (IDX)
                                   MOVE    ONSKAKU-RES-KIGO
                                           TO  QUARES1-DRG-KIGO    (IDX)
                                   MOVE    ONSKAKU-RES-NUM
                                           TO  QUARES1-DRG-NUM     (IDX)
                                   MOVE    ONSKAKU-RES-EDABAN
                                           TO  QUARES1-DRG-EDABAN  (IDX)
                                   MOVE    WRK-SIKIBETU
                                           TO
                                           QUARES1-DRG-SIKIBETU    (IDX)
      *                            診療情報閲覧同意フラグ
                                   IF      ONSKAKU-SHINRYO-DOUIFLG
                                                       =   "1"
                                       MOVE    "4"         TO
                                           QUARES1-DRG-FILECATEGORY(IDX)
                                   ELSE
                                       MOVE    "2"         TO
                                           QUARES1-DRG-FILECATEGORY(IDX)
                                   END-IF
                                   MOVE    ONSKAKU-ONSHINRYO-KBN
                                                         TO
                                               QUARES1-DRG-SRYKBN  (IDX)
                                   MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO
                                           QUARES1-DRG-STARTDATE   (IDX)
                                           QUARES1-DRG-ENDDATE     (IDX)
                               END-IF
                           END-PERFORM    
                           IF    ( SYS-1051-ONSHIYAKUZAI
                                               =   "1" )
                           OR    ( SYS-1051-ONSHIYKZSINRYO
                                               =   "1" )
                               ADD     1   TO  IDX
                               MOVE    WRK-HOSPCD
                                           TO  QUARES1-DRG-HOSPCD(IDX)
                               MOVE    ONSKAKU-RES-HKNJANUM
                                           TO
                                           QUARES1-DRG-HKNJANUM  (IDX)
                               MOVE    ONSKAKU-RES-KIGO
                                           TO  QUARES1-DRG-KIGO  (IDX) 
                               MOVE    ONSKAKU-RES-NUM
                                           TO  QUARES1-DRG-NUM   (IDX)
                               MOVE    ONSKAKU-RES-EDABAN
                                           TO  QUARES1-DRG-EDABAN(IDX)
                               MOVE    WRK-SIKIBETU
                                           TO
                                           QUARES1-DRG-SIKIBETU  (IDX)
      *                        診療情報閲覧同意フラグ
                               IF    ( ONSKAKU-SHINRYO-DOUIFLG
                                           =   "1" )
                               AND   ( SYS-1051-ONSHIYKZSINRYO
                                           =   "1" )
                                   MOVE    "3"         TO
                                           QUARES1-DRG-FILECATEGORY(IDX)
                               ELSE
                                   MOVE    "1"         TO
                                           QUARES1-DRG-FILECATEGORY(IDX)
                               END-IF 
                               MOVE    ONSKAKU-ONSHINRYO-KBN
                                           TO  QUARES1-DRG-SRYKBN  (IDX)
                               STRING  "YZKsiquc01req_"
                                                   DELIMITED BY  SIZE
                                       WRK-FILE-PATIENT-ID
                                                   DELIMITED BY  SIZE
                                       ".xml"      DELIMITED BY  SIZE
                                       INTO    QUARES1-DRG-FILE (IDX)
                               END-STRING
                               INITIALIZE          STS-AREA-DAY
                               INITIALIZE          LNK-DAY6-AREA
                               MOVE    "61"
                                           TO  LNK-DAY6-IRAI
                               MOVE    SPA-SYSYMD
                                           TO  LNK-DAY6-KIJUN
                               MOVE    "2" TO  LNK-DAY6-ZOGENPTN
                               MOVE    -12 TO  LNK-DAY6-ZOGEN
                               CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                           LNK-DAY6-AREA
                               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO
                                           QUARES1-DRG-STARTDATE (IDX)
                               MOVE    WRK-SPA-SYSYMD-1
                                           TO
                                           QUARES1-DRG-ENDDATE   (IDX)
                           END-IF                        
                       END-IF                            
      *                特定検診情報閲覧同意フラグが１（同意）で特定検診情報取得依頼済フラグが空白
                       IF    ( ONSKAKU-KENSIN-DOUIFLG
                                                   =   "1"   )
                       AND   ( ONSKAKU-KENSIN-ENDFLG
                                                   =   SPACE )
                           MOVE    "01"        TO  ONSKAKU-KENSIN-ENDFLG
      *
                           MOVE    "True"      TO  QUARES1-SPEC-CLASS
                           MOVE    WRK-HOSPCD  TO
                                               QUARES1-SPEC-HOSPCD (1)
                           MOVE    ONSKAKU-RES-HKNJANUM
                                               TO
                                               QUARES1-SPEC-HKNJANUM (1)
                           MOVE    ONSKAKU-RES-KIGO
                                               TO  QUARES1-SPEC-KIGO (1)
                           MOVE    ONSKAKU-RES-NUM
                                               TO  QUARES1-SPEC-NUM  (1)
                           MOVE    ONSKAKU-RES-EDABAN
                                               TO
                                               QUARES1-SPEC-EDABAN (1)
                           MOVE    WRK-SIKIBETU
                                               TO
                                               QUARES1-SPEC-SIKIBETU (1)
                           MOVE    "2"         TO
                                           QUARES1-SPEC-FILECATEGORY (1)
                           MOVE    ONSKAKU-ONSHINRYO-KBN
                                               TO
                                               QUARES1-SPEC-SRYKBN (1)
                           IF      SYS-1051-ONSHIKENSIN 
                                               =   "1"                
                               MOVE    WRK-HOSPCD
                                               TO
                                               QUARES1-SPEC-HOSPCD (2)
                               MOVE    ONSKAKU-RES-HKNJANUM
                                               TO
                                               QUARES1-SPEC-HKNJANUM (2)
                               MOVE    ONSKAKU-RES-KIGO
                                               TO  QUARES1-SPEC-KIGO (2)
                               MOVE    ONSKAKU-RES-NUM
                                               TO  QUARES1-SPEC-NUM  (2)
                               MOVE    ONSKAKU-RES-EDABAN
                                               TO
                                               QUARES1-SPEC-EDABAN (2)
                               MOVE    WRK-SIKIBETU
                                               TO
                                               QUARES1-SPEC-SIKIBETU (2)
                               MOVE    "1"     TO
                                           QUARES1-SPEC-FILECATEGORY (2)
                               MOVE    ONSKAKU-ONSHINRYO-KBN
                                           TO  QUARES1-SPEC-SRYKBN (2)
                               STRING  "TKKsiquc01req_"
                                                   DELIMITED BY  SIZE
                                       WRK-FILE-PATIENT-ID
                                                   DELIMITED BY  SIZE
                                       ".xml"      DELIMITED BY  SIZE
                                           INTO    QUARES1-SPEC-FILE (2)
                           END-STRING
                           END-IF                
                       END-IF
                    END-IF
           END-EVALUATE
      *
      *    診療区分=2(訪問)の場合、医療情報閲覧要求をtbl_onshi_houmonの
      *    内容で設定
           IF      ONSKAKU-ONSHINRYO-KBN
                                   =   "2" OR  "3"
               INITIALIZE                  ONSHI-HOUMON-REC    
               MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
               MOVE    ONSKAKU-PTID    TO  ONS-HOUMON-PTID
               MOVE    ONSHI-HOUMON-REC
                                       TO  MCPDATA-REC
               PERFORM 900-ONSHI-HOUMON-READ-SEC
               IF      FLG-ONSHI-HOUMON    =   ZERO
                   IF      ONS-HOUMON-GENDO-SHUTOKUFLG
                                               =   "0" OR  "1"
                       MOVE    ONS-HOUMON-GENDO-SHUTOKUFLG
                                           TO  QUARES1-QUA-SYUTOKUFLG
                   ELSE
                       MOVE    "0"         TO  QUARES1-QUA-SYUTOKUFLG
                   END-IF    
                   IF      ONS-HOUMON-SIKKAN-SHUTOKUFLG
                                               =   "0" OR  "1"
                       MOVE    ONS-HOUMON-SIKKAN-SHUTOKUFLG
                                           TO  QUARES1-QUA-TOK-DOUIFLG
                   ELSE
                       MOVE    "0"         TO  QUARES1-QUA-TOK-DOUIFLG
                   END-IF    
               ELSE
                   MOVE    "E08"           TO  WRK-ERRCD    
               END-IF
           END-IF
      *
      *   保険証ＯＣＲのとき
           EVALUATE    WRK-ONSKAKU-SIKAKU-YUKO
               WHEN    "5"
                   MOVE    "01"            TO  ONSKAKU-KAKU-ENDFLG
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
                   MOVE    "02"            TO  ONSKAKU-KAKU-ENDFLG
           END-EVALUATE
           .
       20021-RES1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認レコードの更新
      *****************************************************************
       20022-ONSHI-KAKU-UPDATE-SEC SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-UPYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-UPHMS
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    "E12"           TO  WRK-ERRCD
           END-IF    
      *
           .
       20022-ONSHI-KAKU-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      * 
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E02"
                   MOVE    "医療機関情報が取得できません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E05"
                   MOVE    "患者ＩＤが取得できません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E07"
                   MOVE    "資格確認レコードが存在しません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E08"
                   MOVE    "資格確認訪問診療レコードが存在しません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
                   MOVE    "資格確認レコードの更新ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
                   MOVE    "ＵＩＤが未設定です"    TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "リクエストが空です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E16"
                   MOVE    "実施日が暦日ではありません"
                                                   TO  WRK-ERRMSG
               WHEN    "E17"
                   MOVE    "実施日が未設定です"
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
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
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
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認照会結果読込
      *****************************************************************
       900-ONSHI-KAKU-READ-SEC           SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-KAKU-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険証の資格確認読込（照会区分＝９以外）
      ******************************************************************
       900-ONSHI-KAKU-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSKAKU-REC
               MOVE    ZERO                TO  FLG-ONSHI-KAKU
               IF      ONSKAKU-SHOKAI-KBN
                                           =   "9" OR  "8"
                   GO  TO  900-ONSHI-KAKU-READ-N-SEC
               END-IF        
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           .
       900-ONSHI-KAKU-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格薬剤情報読込
      *****************************************************************
       900-ONSHI-YKZ-READ-SEC     SECTION.
      *
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_yakuzai_main"
                                           TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  ONSHI-YAKUZAI-MAIN-REC
                   MOVE    ZERO            TO  FLG-ONSHI-YKZ
               ELSE
                   INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
                   MOVE    1               TO  FLG-ONSHI-YKZ
               END-IF
           ELSE
               INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
               MOVE    1               TO  FLG-ONSHI-YKZ
           END-IF
      *
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .

       900-ONSHI-YKZ-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    オンライン資格確認訪問診療
      ******************************************************************
       900-ONSHI-HOUMON-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ONSHI-HOUMON-REC
                   MOVE    ZERO                TO  FLG-ONSHI-HOUMON
               ELSE
                   INITIALIZE                      ONSHI-HOUMON-REC
                   MOVE    1                   TO  FLG-ONSHI-HOUMON
               END-IF
           ELSE
               INITIALIZE                      ONSHI-HOUMON-REC
               MOVE    1                   TO  FLG-ONSHI-HOUMON
           END-IF
      *
           MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-HOUMON-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.

