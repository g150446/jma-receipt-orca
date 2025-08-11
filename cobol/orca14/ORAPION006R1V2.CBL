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
       PROGRAM-ID.          ORAPION006R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 照会番号一括登録
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/02/06    NMED−藤原    新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    ORCAMO       21/07/05  資格確認：照会番号に患者番号を設定
      *  05.02.01    ORCAMO       24/03/13  医療扶助資格確認対応
      *  05.02.02    ORCAMO       24/05/31  医療機関コード設定を修正
      *  05.02.04    ORCAMO       25/06/09  旧医療機関コード対応
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
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-MONTHLYNUM          PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXX                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-OPID                PIC X(16).
           03  WRK-PTHKN-PTID          PIC 9(10).
           03  WRK-REF-PTID            PIC 9(10).
           03  WRK-PATIENT-ID          PIC X(20).
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
           03  WRK-ERRMSG              PIC X(100).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-REF-MAX        PIC 9(04)   VALUE   5000.     
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEREFALLREQ1.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEREFALLRES1.INC".
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
           COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
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
       *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *     
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
           DISPLAY   "**************************"
           DISPLAY   "* onlinerefallreq1 start *"
           DISPLAY   "**************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
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
               MOVE   404                  TO  APILST06-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*************************"
           DISPLAY   "* onlinerefallreq1  end *"
           DISPLAY   "*************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      XML-ONLINEREFALLRES1
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           STRING  "A+"                DELIMITED  BY  SIZE
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  REFALLRES1-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  REFALLRES1-INFORMATION-TIME
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
      *    医療機関情報
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC    
           END-IF
      *
      *    オンライン資格確認照会情報取得 
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1051"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1051-REC
           ELSE
               INITIALIZE                  SYS-1051-REC
           END-IF
           IF      SYS-1051-ONSHIKAKU  =   SPACE
               MOVE    "0"                 TO  SYS-1051-ONSHIKAKU
                                               SYS-1051-FUJYO
           END-IF
           IF      SYS-1051-FUJYO      =   SPACE
               IF      SYS-1051-ONSHIKAKU  =   "1"
               MOVE    "1"                 TO  SYS-1051-FUJYO
               END-IF
           END-IF
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
      *    照会番号一括登録処理
           PERFORM 2002-PTHKNINF-REFALL-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST06-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEREFALLREQ1
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinerefallreq1"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST06-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinerefallreq1"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEREFALLREQ1
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
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "000"               TO  REFALLRES1-API-RESULT
               MOVE    "処理終了"          TO  REFALLRES1-API-RESULT-MSG
           ELSE
               DISPLAY "300 WRK-ERRCD=" WRK-ERRCD
               MOVE    WRK-ERRCD           TO  REFALLRES1-API-RESULT
               MOVE    WRK-ERRMSG          TO  REFALLRES1-API-RESULT-MSG
            END-IF
      *
           IF      APILST06-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinerefallres1"
                                       TO  MCP-TABLE
           MOVE    1                   TO  REFALLRES1-MODE
           MOVE    "onlinerefallres1"  TO  REFALLRES1-RECORDNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEREFALLRES1
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE    REFALLRES1-OBJECT   TO  APILST06-BODY
               MOVE    "application/xml"   TO  APILST06-CONTENT-TYPE
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
      *    照会番号チェック
           MOVE    ZERO        TO  WRK-REF-PTID
      *
           IF      REFALLREQ1-PTID
                           NOT =   SPACE
               INITIALIZE                  ORCSNUMAREA
               MOVE    REFALLREQ1-PTID TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                           ORCSNUMAREA
               IF    ( SNUM-RC         =   ZERO  )
               AND   ( SNUM-SEISUU     <=  10    )
               AND   ( SNUM-SYOSUU     =   0     )
               AND   ( SNUM-MINKBN     =   SPACE )
                   MOVE    SNUM-OUTNUM     TO  WRK-REF-PTID
               ELSE
                   DISPLAY "Last_ReferenceNumber=" REFALLREQ1-PTID
                   MOVE    "E11"           TO  WRK-ERRCD
                   MOVE    "照会番号の設定に誤りがあります"
                                           TO  WRK-ERRMSG
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF    
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    照会番号一括登録処理
      *****************************************************************
       2002-PTHKNINF-REFALL-SEC  SECTION.
      *
      *    最終照会番号の次から対象とする
           MOVE    ZERO            TO  IDXX
           MOVE    ZERO            TO  FLG-PTINF
      *     
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    WRK-REF-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key30"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key30"         TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           PERFORM         UNTIL   FLG-PTINF       =   1
                           OR      IDXX            >=  WRK-CONS-REF-MAX
               INITIALIZE                  PTHKNINF-REC
               MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
               MOVE    SMCNDATE-YMD    TO  PTHKN-TEKSTYMD
                                           PTHKN-TEKEDYMD
               MOVE    PTINF-PTID      TO  PTHKN-PTID 
               PERFORM 900-PTHKNINF-READ-SEC
               IF      FLG-PTHKNINF    =   ZERO
                   MOVE    PTHKN-PTID      TO  WRK-PTHKN-PTID
      *
                   MOVE    SPACE           TO  WRK-PATIENT-ID
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    PTINF-PTID      TO  SPTID-PTID
                   CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                               SPA-AREA
                   IF      SPTID-RC        =   00
                       MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
                   END-IF
      *         
                   ADD     1               TO  IDXX
      ********     MOVE    PTHKN-PTID      TO  REFALLRES1-SHOKAI-NUM
      ********                                                   (IDXX)
                   MOVE    WRK-PATIENT-ID  TO  REFALLRES1-SHOKAI-NUM
                                                                 (IDXX)
                   MOVE    PTHKN-HKNJANUM  TO  REFALLRES1-HKNJANUM
                                                                 (IDXX)
                   MOVE    PTHKN-KIGO      TO  REFALLRES1-KIGO   (IDXX)
                   MOVE    PTHKN-NUM       TO  REFALLRES1-NUM    (IDXX)
                   MOVE    PTHKN-EDABAN    TO  REFALLRES1-EDABAN (IDXX)
               ELSE
      *            医療扶助の資格確認をするとき
                   IF      SYS-1051-FUJYO  =   "1"
                       INITIALIZE                  PTKOHINF-REC
                       MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                       MOVE    PTINF-PTID      TO  PTKOH-PTID
                       MOVE    "012"           TO  PTKOH-KOHNUM
                       MOVE    SMCNDATE-YMD    TO  PTKOH-TEKSTYMD
                                                   PTKOH-TEKEDYMD
                       PERFORM 900-PTKOHINF-READ-SEC
                       IF      FLG-PTKOHINF    =   ZERO
                           MOVE    PTHKN-PTID      TO  WRK-PTHKN-PTID
      *                    有効な医療扶助があるとき受給者番号が空白のときは
      *                    最初に検索した医療扶助から月代わり受給者番号を検索
                           IF      PTKOH-JKYSNUM   =   SPACE
                               INITIALIZE              MONTHLYNUM-REC
                               MOVE    SPA-HOSPNUM TO
                                                   MONTHLYNUM-HOSPNUM
                               MOVE    PTKOH-PTID  TO  MONTHLYNUM-PTID
                               MOVE    PTKOH-KOHID TO  MONTHLYNUM-KOHID
                               MOVE    SMCNDATE-YMD (1:6)
                                                   TO  MONTHLYNUM-SRYYM
                               MOVE    "%"         TO
                                                   MONTHLYNUM-NYUGAIKBN
                               MOVE    MONTHLYNUM-REC
                                                   TO  MCPDATA-REC
                               PERFORM 900-MONTHLYNUM-READ-SEC
                               IF    ( FLG-MONTHLYNUM  =   ZERO  )
                                   MOVE    MONTHLYNUM-JKYSNUM 
                                                       TO  PTKOH-JKYSNUM
                               END-IF
                           END-IF
      *
                           MOVE    SPACE           TO  WRK-PATIENT-ID
                           INITIALIZE                  ORCSPTIDAREA
                           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                           MOVE    PTINF-PTID      TO  SPTID-PTID
                           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                           SPA-AREA
                           IF      SPTID-RC        =   00
                               MOVE    SPTID-PTNUM TO  WRK-PATIENT-ID
                           END-IF
      *         
                           ADD     1               TO  IDXX
      ********             MOVE    PTKOH-PTID      TO
      *                                    REFALLRES1-SHOKAI-NUM (IDXX)
                           MOVE    WRK-PATIENT-ID  TO
                                           REFALLRES1-SHOKAI-NUM (IDXX)
                           MOVE    PTKOH-FTNJANUM  TO
                                           REFALLRES1-HKNJANUM   (IDXX)
                           MOVE    PTKOH-JKYSNUM   TO
                                           REFALLRES1-NUM        (IDXX)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key30"         TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-N-SEC
           END-PERFORM
      *     
           IF      IDXX            =   ZERO
               MOVE    "E12"           TO  WRK-ERRCD
               MOVE    "保険情報なし"  TO  WRK-ERRMSG
           ELSE
               IF      FLG-PTINF       =   1
                   CONTINUE
               ELSE
                   MOVE    WRK-PTHKN-PTID  TO  REFALLRES1-PTID
                   MOVE    "Yes"           TO  REFALLRES1-REF-CONT
               END-IF
               STRING  SYS-1001-PREFNUM
                                       DELIMITED  BY  SIZE
                       SYS-1001-TENHYOKBN
                                       DELIMITED  BY  SIZE
                       SYS-1001-HOSPCD DELIMITED  BY  SIZE
                                       INTO       REFALLRES1-HOSPCD
               END-STRING
      *        旧医療機関コードが設定されているとき
               IF      SYS-1051-HOSPCDF    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    SYS-1051-HOSPCDF    TO  REFALLRES1-HOSPCD
               END-IF    
      *********MOVE                    TO  REFALLRES1-SIKIBETU
           END-IF    
           .        
       2002-PTHKNINF-REFALL-EXT.
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
      ******************************************************************
      *    患者マスター読込処理
      ******************************************************************
       900-PTINF-READ-N-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           .
       900-PTINF-READ-N-EXT.
           EXIT.
      *
       ******************************************************************
      *    患者保険マスター読込処理
      ******************************************************************
       900-PTHKNINF-READ-SEC     SECTION.
      *
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
          .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者保険情報読込
      ******************************************************************
       900-PTHKNINF-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTHKNINF-REC
      *        医保分以外         
               IF      PTHKN-HKNNUM (1:1)
                                       NOT =   "0"
                   GO  TO  900-PTHKNINF-READ-N-SEC
               END-IF
               IF      PTHKN-SAKJOKBN  =   "1"
                   GO  TO  900-PTHKNINF-READ-N-SEC
               END-IF
      *        職務上の補助区分が設定され重複する場合
               IF    ( PTHKN-HKNNUM    =   "002"   OR  "031"   OR
                                           "032"   OR  "033"   OR
                                           "034"                  )
               AND   ( PTHKN-HOJOKBN   =   "A" OR  "B" OR  "C" OR
                                           "D" OR  "E" OR  "F" OR
                                           "G" OR  "H" OR  "I" OR
                                           "1" OR  "2" OR  "3"    )
                   GO  TO  900-PTHKNINF-READ-N-SEC
               END-IF
      *        後期高齢者以外で枝番が設定されていない         
               IF    ( PTHKN-EDABAN        =   SPACE )
               AND   ( PTHKN-HKNNUM    NOT =   "039" )     
                   GO  TO  900-PTHKNINF-READ-N-SEC
               END-IF
               MOVE    ZERO                TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                      PTHKNINF-REC
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           .
       900-PTHKNINF-READ-N-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者公費マスタ読込処理
      ******************************************************************
       900-PTKOHINF-READ-SEC     SECTION.
      *
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   INITIALIZE                  PTKOHINF-REC
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
          .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       900-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_monthlynum"    TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    システム管理読込
      ******************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SYSKANRI-READ-EXT.
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

