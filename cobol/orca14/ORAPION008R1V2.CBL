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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPION008R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 予約患者一括資格確認照会処理
      *                     （資格確認依頼情報返却処理）
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/11/30    ORCAMO        新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.02.01    ORCAMO       24/05/31  医療機関コード設定を修正
      *  05.02.02    ORCAMO       24/06/26  医療扶助併用対応
      *  05.02.03    ORCAMO       24/11/15  訪問診療対応
      *  05.02.04    ORCAMO       25/03/07  オンライン診療対応
      *  05.02.05    ORCAMO       25/06/09  旧医療機関コード対応
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
           03  FLG-YYK                 PIC 9(01).
           03  FLG-JOBKANRI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-MONTHLYNUM          PIC 9(01).
      *
           03  FLG-PTHKNINF-OK         PIC 9(01).
           03  FLG-PTKOHINF-OK         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *    03  WRK-OPID                PIC X(16).
           03  WRK-PTID                PIC 9(10).
           03  WRK-LAST-YYKNO          PIC 9(10).
           03  WRK-HOSPCD              PIC X(10).
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
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
      *    ジョブ管理マスタ
       01  WRK-JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC"
                                   REPLACING   //JOB-//
                                   BY          //WRK-JOB-//.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値（予約）
           03  WRK-CONS-JOB-SHELLID
                                       PIC X(08)   VALUE   "yoyaku01".
      *    ジョブ管理ＤＢのキー値（訪問診療）
           03  WRK-CONS-JOB-SHELLID1
                                       PIC X(08)   VALUE   "P02V001".
      *    ジョブ管理ＤＢのキー値（オンライン診療）
           03  WRK-CONS-ONLINE-JOB-SHELLID
                                       PIC X(08)   VALUE   "P02O001".
           03  WRK-CONS-JOB-JOBID      PIC 9(07)   VALUE   ZERO.
           03  WRK-CONS-RES-MAX        PIC 9(04)   VALUE   300.
           03  WRK-CONS-RES-HEIYOU-MAX PIC 9(04)   VALUE   298.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
      *     COPY    "CPORCSPTID.INC".
      *    患者番号変換サブ
      *     COPY    "CPORCSPTNUM.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUAAPPREQ1.INC".
      *    API XML書き込み用定義体
           COPY    "CPONLINEQUAAPPRES1.INC".
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
           COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
       *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
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
      *    予約
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
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
           DISPLAY   "****************************"
           DISPLAY   "*   onlinequaapp1  start   *"
           DISPLAY   "****************************"
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
               MOVE   404                  TO  APILST08-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "****************************"
           DISPLAY   "*   onlinequaapp1    end   *"
           DISPLAY   "****************************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-ONLINEQUAAPPRES1
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    STRING  "A+"                DELIMITED  BY  SIZE
      *            MCP-USER            DELIMITED  BY  SIZE
      *                                INTO    WRK-OPID
      *    END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  QUAAPPRES1-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  QUAAPPRES1-INFORMATION-TIME
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
           INITIALIZE                      SYS-1051-REC
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
           IF      SYS-1051-ONSHIKAKU  =   SPACE
               MOVE    "0"                 TO  SYS-1051-ONSHIKAKU
                                               SYS-1051-FUJYO
           END-IF
           IF      SYS-1051-FUJYO      =   SPACE
               IF      SYS-1051-ONSHIKAKU  =   "1"
               MOVE    "1"                 TO  SYS-1051-FUJYO
               END-IF
           END-IF
      *
      *    医療機関コード設定
           STRING  SPA-PREFNUM         DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN  DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD     DELIMITED  BY  SIZE
                                       INTO    WRK-HOSPCD
           END-STRING
           IF      SYS-1051-HOSPCDF    =   SPACE
               CONTINUE
           ELSE
               MOVE    SYS-1051-HOSPCDF    TO  WRK-HOSPCD
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
           EVALUATE    QUAAPPREQ1-QUALIFICATION-MODE
               WHEN    "01"
      *            資格確認依頼情報返却処理
                   PERFORM 2002-SIKAKU-KAKUNIN-RES-SEC
               WHEN    "02"    
      *            資格確認結果登録処理
                   PERFORM 2003-JOBKANRI-UPDATE-SEC
               WHEN    "03"    
      *            未処理返却処理
                   PERFORM 2004-JOBKANRI-LIST-SEC
            END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST08-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAAPPREQ1
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinequaappreq1"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST08-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinequaappreq1"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAAPPREQ1
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
               MOVE    "000"               TO  QUAAPPRES1-API-RESULT
               MOVE    "処理終了"          TO  QUAAPPRES1-API-RESULT-MSG
           ELSE
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  QUAAPPRES1-API-RESULT
               MOVE    WRK-ERRMSG      TO  QUAAPPRES1-API-RESULT-MSG
           END-IF
      *
           IF      APILST08-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinequaappres1"
                                       TO  MCP-TABLE
           MOVE    1                   TO  QUAAPPRES1-MODE
           MOVE    "onlinequaappres1"  TO  QUAAPPRES1-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINEQUAAPPRES1
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  QUAAPPRES1-OBJECT    TO  APILST08-BODY
               MOVE  "application/xml" TO  APILST08-CONTENT-TYPE
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
           IF      QUAAPPREQ1-DATAREQ
                               =   SPACE 
               MOVE    "E15"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      QUAAPPREQ1-QUALIFICATION-MODE
                               =   "01"    OR  "02"    OR  "03" 
                CONTINUE
           ELSE
               MOVE    "E12"       TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      QUAAPPREQ1-DATE
                               =   SPACE
               MOVE    "E17"       TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           ELSE
               MOVE    QUAAPPREQ1-DATE
                                   TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE              ORCSGDAYAREA
               MOVE    "2"         TO  SGDAY-INTYPE
               MOVE    WRK-SYMD    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"  USING
                                       ORCSGDAYAREA
               IF      SGDAY-RC    =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "E16"       TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           EVALUATE    QUAAPPREQ1-QUALIFICATION-MODE
               WHEN    "01"
                   IF      QUAAPPREQ1-UUID
                                       =   SPACE
                       MOVE    "E13"       TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
      * 
                   MOVE    ZERO        TO  WRK-LAST-YYKNO
                   IF      QUAAPPREQ1-LAST-YYKNO
                                   NOT =   SPACE
                       INITIALIZE                  ORCSNUMAREA
                       MOVE    QUAAPPREQ1-LAST-YYKNO
                                           TO  SNUM-INX
                       CALL    "ORCSNUM"   USING
                                               ORCSNUMAREA
                       IF    ( SNUM-RC         =   ZERO  )
                       AND   ( SNUM-SEISUU     <=  10    )
                       AND   ( SNUM-SYOSUU     =   0     )
                       AND   ( SNUM-MINKBN     =   SPACE )
                           MOVE    SNUM-OUTNUM     TO  WRK-LAST-YYKNO
                       ELSE
                           DISPLAY "Last_AppointmentNumber="
                                                   QUAAPPREQ1-LAST-YYKNO
                           MOVE    "E11"           TO  WRK-ERRCD
                           GO  TO  2001-INPUT-CHK-EXT
                       END-IF    
                   END-IF
               WHEN    "02"
                   IF      QUAAPPREQ1-SIKIBETU
                                       =   SPACE
                       MOVE    "E18"       TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
      *
                   IF      QUAAPPREQ1-UKENO
                                       =   SPACE
                       MOVE    "E19"       TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               WHEN    "03"
                   CONTINUE
            END-EVALUATE
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認依頼情報返却処理
      *****************************************************************
       2002-SIKAKU-KAKUNIN-RES-SEC    SECTION.
      *
      *??
           display "QUAAPPREQ1-PROCESS-CLASS=" QUAAPPREQ1-PROCESS-CLASS
      *    訪問診療、オンライン診療(同意一括)のとき
           IF      QUAAPPREQ1-PROCESS-CLASS
                                   =   "V001"  OR  "O001"
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               EVALUATE    QUAAPPREQ1-PROCESS-CLASS
                   WHEN    "V001"
                       MOVE    WRK-CONS-JOB-SHELLID1
                                               TO  JOB-SHELLID
                   WHEN    "O001"
                       MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                               TO  JOB-SHELLID
               END-EVALUATE                        
               MOVE    QUAAPPREQ1-UUID TO  JOB-KARTE-UID
               MOVE    "key6"          TO  WRK-MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-SEC
               IF      FLG-JOBKANRI    =   1
                   MOVE    "E14"           TO  WRK-ERRCD
               ELSE
                   MOVE    QUAAPPREQ1-UUID TO
                                           QUAAPPRES1-IKKATU-SIKIBETU
                   MOVE    WRK-HOSPCD      TO  QUAAPPRES1-IKKATU-HOSPCD
                   MOVE    JOB-STARTYMD    TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  QUAAPPRES1-DOUI-STARYMD
                   MOVE    JOB-ENDYMD      TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE    TO  QUAAPPRES1-DOUI-ENDYMD
                  IF      QUAAPPREQ1-PROCESS-CLASS
                                           =   "V001"
                       MOVE    "2"             TO  QUAAPPRES1-SRYKBN
                   ELSE
                       MOVE    "3"             TO  QUAAPPRES1-SRYKBN
                   END-IF     
      * 
                   MOVE    "0001"          TO  JOB-ERRCD
                   MOVE    JOBKANRI-REC    TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_jobkanri"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   IF      MCP-RC          >   ZERO
                       DISPLAY "JOBKANRI UPDATE ERR=" MCP-RC
                       MOVE    "E22"           TO  WRK-ERRCD
                   END-IF
               END-IF
               GO  TO  2002-SIKAKU-KAKUNIN-RES-EXT
           END-IF
      *
           MOVE    QUAAPPREQ1-DATE TO  QUAAPPRES1-KAKUNINYMD
           MOVE    QUAAPPREQ1-UUID TO  QUAAPPRES1-SIKIBETU
           MOVE    WRK-HOSPCD      TO  QUAAPPRES1-HOSPCD
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    QUAAPPREQ1-UUID TO  JOB-KARTE-UID
           MOVE    "key6"          TO  WRK-MCP-PATHNAME
           PERFORM 900-JOBKANRI-READ-SEC
           IF      FLG-JOBKANRI    =   1
               MOVE    "E14"           TO  WRK-ERRCD
               GO  TO  2002-SIKAKU-KAKUNIN-RES-EXT
           ELSE
               MOVE    JOBKANRI-REC    TO  WRK-JOBKANRI-REC 
           END-IF
      *
           MOVE    ZERO            TO  IDXX
                                       WRK-PTID
      *
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM     TO  YYK-HOSPNUM
           MOVE    JOB-SRYYMD      TO  YYK-YYKYMD
           MOVE    WRK-LAST-YYKNO  TO  YYK-PTID
           MOVE    YYK-REC         TO  MCPDATA-REC
           MOVE    "key13"         TO  MCP-PATHNAME
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-YYK-READ-N-SEC
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1               TO  FLG-YYK
           END-IF
      *
           PERFORM                 UNTIL   FLG-YYK =   1
                                   OR      IDXX    >=  WRK-CONS-RES-MAX
               MOVE    ZERO            TO  FLG-PTHKNINF-OK
                                           FLG-PTKOHINF-OK
      *
      *        テスト患者は対象外
               INITIALIZE                  PTINF-REC
               MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
               MOVE    YYK-PTID        TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      PTINF-TSTPTNUMKBN   =   "1"
                 CONTINUE
               ELSE         
                 INITIALIZE                  PTHKNINF-REC
                 MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
                 MOVE    YYK-PTID        TO  PTHKN-PTID
                 MOVE    YYK-YYKYMD      TO  PTHKN-TEKSTYMD
                                             PTHKN-TEKEDYMD
                 PERFORM 900-PTHKNINF-READ-SEC
      *          医療扶助の資格確認をするとき
                 IF      SYS-1051-FUJYO  =   "1"
                     INITIALIZE                  PTKOHINF-REC
                     MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                     MOVE    YYK-PTID        TO  PTKOH-PTID
                     MOVE    "012"           TO  PTKOH-KOHNUM
                     MOVE    YYK-YYKYMD      TO  PTKOH-TEKSTYMD
                                                 PTKOH-TEKEDYMD
                     PERFORM 900-PTKOHINF-READ-SEC
                     IF      FLG-PTKOHINF    =   ZERO
      *                  有効な医療扶助があるとき受給者番号が空白のときは
      *                  最初に検索した医療扶助から月代わり受給者番号を検索
                         IF      PTKOH-JKYSNUM   =   SPACE
                           INITIALIZE              MONTHLYNUM-REC
                           MOVE    SPA-HOSPNUM TO  MONTHLYNUM-HOSPNUM
                           MOVE    PTKOH-PTID  TO  MONTHLYNUM-PTID
                           MOVE    PTKOH-KOHID TO  MONTHLYNUM-KOHID
                           MOVE    YYK-YYKYMD (1:6)
                                               TO  MONTHLYNUM-SRYYM
                           MOVE    "%"         TO  MONTHLYNUM-NYUGAIKBN
                           MOVE    MONTHLYNUM-REC
                                               TO  MCPDATA-REC
                           PERFORM 900-MONTHLYNUM-READ-SEC
                           IF    ( FLG-MONTHLYNUM  =   ZERO  )
                               MOVE    MONTHLYNUM-JKYSNUM 
                                               TO  PTKOH-JKYSNUM
                           END-IF
                         END-IF
                     END-IF
                 END-IF        
      *             
                 IF    ( FLG-PTHKNINF-OK  =   1 )
                 OR    ( FLG-PTKOHINF-OK  =   1 )
                   IF    ( FLG-PTHKNINF-OK  =   1 )
                   AND   ( FLG-PTKOHINF-OK  =   1 )
      *                併用で同じレスポンスに返却できないとき
                       IF      IDXX         >   WRK-CONS-RES-HEIYOU-MAX
                           MOVE    WRK-CONS-RES-MAX
                                               TO  IDXX
                       END-IF
                   END-IF
      *             
                   IF      IDXX    >=  WRK-CONS-RES-MAX
                       CONTINUE
                   ELSE    
                       PERFORM 20021-QUAAPPRES1-HENSYU-SEC
                   END-IF
                 END-IF
      *
                 MOVE    YYK-PTID        TO  WRK-PTID
      *          同じ患者の予約情報を読み飛ばす
                 PERFORM     UNTIL   FLG-YYK         =   1
                             OR      YYK-PTID    NOT =   WRK-PTID 
                     PERFORM 900-YYK-READ-N-SEC
                 END-PERFORM    
               END-IF  
           END-PERFORM
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key13"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-YYK         =   ZERO
               PERFORM 20021-JOBKANRI-SYORI-SEC
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "Yes"           TO  QUAAPPRES1-CONT-FLG
                   MOVE    WRK-PTID        TO  QUAAPPRES1-LAST-YYKNO
                   MOVE    C-UID           TO  QUAAPPRES1-SIKIBETU
               END-IF    
           END-IF
      *
           .        
       2002-SIKAKU-KAKUNIN-RES-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理作成処理
      *****************************************************************
       20021-QUAAPPRES1-HENSYU-SEC               SECTION.
      *
           IF    ( FLG-PTHKNINF-OK =   1 )
               ADD     1               TO  IDXX
      *        UIDコード取得処理
               PERFORM 800-UID-GET-SEC
      *
               MOVE    PTHKN-HKNJANUM  TO  QUAAPPRES1-HKNJANUM (IDXX)
               MOVE    PTHKN-KIGO      TO  QUAAPPRES1-KIGO     (IDXX)
               MOVE    PTHKN-NUM       TO  QUAAPPRES1-NUM      (IDXX)
               MOVE    PTHKN-EDABAN    TO  QUAAPPRES1-EDABAN   (IDXX)
      *
               MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  QUAAPPRES1-BIRTHDAY (IDXX)
               STRING    YYK-PTID      DELIMITED  BY  SIZE
                         ","           DELIMITED  BY  SIZE
                         C-UID         DELIMITED  BY  SPACE
                                       INTO  QUAAPPRES1-RES-SIKIBETU
                                                               (IDXX)
               END-STRING
           END-IF
      *
           IF    ( FLG-PTKOHINF-OK =   1 )
               ADD     1               TO  IDXX
      *            UIDコード取得処理
               PERFORM 800-UID-GET-SEC
      *
               MOVE    PTKOH-FTNJANUM  TO  QUAAPPRES1-HKNJANUM (IDXX)
               MOVE    PTKOH-JKYSNUM   TO  QUAAPPRES1-NUM      (IDXX)
      *
               MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  QUAAPPRES1-BIRTHDAY (IDXX)
               STRING  YYK-PTID        DELIMITED  BY  SIZE
                       ","             DELIMITED  BY  SIZE
                       C-UID           DELIMITED  BY  SPACE
                                       INTO  QUAAPPRES1-RES-SIKIBETU
                                                               (IDXX)
               END-STRING
           END-IF
           .        
       20021-QUAAPPRES1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理作成処理
      *****************************************************************
       20021-JOBKANRI-SYORI-SEC               SECTION.
      *
      *    マシン日付取得
           INITIALIZE              ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"   USING
                                   ORCSMCNDATEAREA
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
      *    JOBID最大値取得
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-JOBKANRI-READ-SEC
           MOVE    JOB-JOBID           TO  WRK-CONS-JOB-JOBID
           IF      WRK-CONS-JOB-JOBID  =   10
               MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
           END-IF    
           ADD     1                   TO  WRK-CONS-JOB-JOBID
      *    JOBID最大値+1のデータ削除
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
               IF      SJOBKANRI-RETURN    =   ZERO
                   CONTINUE
               ELSE
                   DISPLAY
                       "DELETE ERR KEY = [" SJOBKANRI-RETURN JOB-KEY "]"
                   MOVE    "E20"               TO  WRK-ERRCD
                   GO  TO  20021-JOBKANRI-SYORI-EXT
               END-IF
           END-IF    
      *    JOBID最大値+1のデータ作成
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           MOVE    WRK-JOB-SRYYMD      TO  JOB-SRYYMD
           MOVE    SMCNDATE-YMD        TO  JOB-STARTYMD
           MOVE    C-UID               TO  JOB-KARTE-UID
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              >   ZERO
               DISPLAY "INSERT ERR KEY = [" MCP-RC JOB-KEY "]"
               MOVE    "E21"               TO  WRK-ERRCD
               GO  TO  20021-JOBKANRI-SYORI-EXT
           END-IF
           .
       20021-JOBKANRI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理更新処理
      *****************************************************************
       2003-JOBKANRI-UPDATE-SEC  SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           EVALUATE    QUAAPPREQ1-PROCESS-CLASS
               WHEN    "V001"
                   MOVE    WRK-CONS-JOB-SHELLID1
                                           TO  JOB-SHELLID
               WHEN    "O001"
                   MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                           TO  JOB-SHELLID
               WHEN    OTHER                            
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
          END-EVALUATE                        
           MOVE    QUAAPPREQ1-SIKIBETU (1:36)
                                   TO  JOB-KARTE-UID
           MOVE    "key6"          TO  WRK-MCP-PATHNAME
           PERFORM 900-JOBKANRI-READ-SEC
           IF      FLG-JOBKANRI    =   1
               MOVE    "E14"           TO  WRK-ERRCD
           ELSE
      *        訪問診療(同意一括)のとき
               IF      QUAAPPREQ1-PROCESS-CLASS
                                       =   "V001"  OR  "O001"
                   MOVE    "0002"          TO  JOB-ERRCD
               END-IF
               MOVE    QUAAPPREQ1-UKENO
                                       TO  JOB-YOBI
               MOVE    JOBKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          >   ZERO
                   DISPLAY "JOBKANRI UPDATE ERR=" MCP-RC
                   MOVE    "E22"           TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2003-JOBKANRI-UPDATE-EXT.
           EXIT.
      *
      ******************************************************************
      *    未処理返却処理
      ******************************************************************
       2004-JOBKANRI-LIST-SEC      SECTION.
      *
           MOVE    WRK-HOSPCD      TO  QUAAPPRES1-HOSPCD
      *
           MOVE    ZERO            TO  IDX
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID1
                                   TO  JOB-SHELLID
           MOVE    "0002"          TO  JOB-ERRCD
           MOVE    "1"             TO  JOB-SYOKBN
           MOVE    JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"  TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
           END-IF
      *
           PERFORM         UNTIL ( FLG-JOBKANRI    =   1  )
                           OR    ( IDX             >=  10 )
               ADD     1               TO  IDX
               MOVE    JOB-YOBI        TO  QUAAPPRES1-YYKNO    (IDX)
               MOVE    "V001"          TO  QUAAPPRES1-BULK-PROCESS-CLASS
                                                               (IDX)
      *
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-N-SEC
           END-PERFORM
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "0002"          TO  JOB-ERRCD
           MOVE    "1"             TO  JOB-SYOKBN
           MOVE    JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"  TO  MCP-TABLE
           MOVE    "key9"          TO  WRK-MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
           END-IF
      *
           PERFORM         UNTIL ( FLG-JOBKANRI    =   1  )
                           OR    ( IDX             >=  10 )
               ADD     1               TO  IDX
               MOVE    JOB-YOBI        TO  QUAAPPRES1-YYKNO    (IDX)
               MOVE    "O001"          TO  QUAAPPRES1-BULK-PROCESS-CLASS
                                                               (IDX)
      *
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-N-SEC
           END-PERFORM
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
            .
       2004-JOBKANRI-LIST-EXT.
           EXIT.
      *
      ******************************************************************
      *    UIDコード取得処理
      ******************************************************************
       800-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
           .
       800-UID-GET-EXT.
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
           EVALUATE    WRK-ERRCD
               WHEN    "E02"
                   MOVE    "医療機関情報が取得できません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E11"
                   MOVE    "最終予約番号の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
                   MOVE    "処理区分の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
                   MOVE    "ＵＩＤが未設定です"    TO  WRK-ERRMSG
               WHEN    "E14"
                   MOVE    "ＵＩＤに一致する処理がありません"
                                                   TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "リクエストが空です。"  TO  WRK-ERRMSG
               WHEN    "E16"
                   MOVE    "資格確認日（予約日）が暦日ではありません"
                                                   TO  WRK-ERRMSG
               WHEN    "E17"
                   MOVE    "資格確認日（予約日）が未設定です"
                                                   TO  WRK-ERRMSG
               WHEN    "E18"
                   MOVE    "任意のファイル識別子が未設定です"
                                                   TO  WRK-ERRMSG
               WHEN    "E19"
                   MOVE    "受付番号が未設定です"TO  WRK-ERRMSG
               WHEN    "E20"
                   MOVE    "ジョブ管理の削除ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E21"
                   MOVE    "ジョブ管理の追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E22"
                   MOVE    "ジョブ管理の更新ができませんでした。"
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
      *    システム管理マスター読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
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
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-SEC       SECTION.
      * 
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  JOBKANRI-REC
                   MOVE    ZERO                TO  FLG-JOBKANRI
               ELSE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    1                   TO  FLG-JOBKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-JOBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-N-SEC          SECTION.
      * 
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JOBKANRI-REC
               MOVE    ZERO            TO  FLG-JOBKANRI
           ELSE
               INITIALIZE                  JOBKANRI-REC
               MOVE    1               TO  FLG-JOBKANRI
               END-IF
           .
       900-JOBKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YYK-READ-N-SEC          SECTION.
      * 
           MOVE    "tbl_yyk"       TO  MCP-TABLE
           MOVE    "key13"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  YYK-REC
               MOVE    ZERO                TO  FLG-YYK
           ELSE
               INITIALIZE                      YYK-REC
               MOVE    1                   TO  FLG-YYK
           END-IF
           .
       900-YYK-READ-N-EXT.
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
      *    患者保険マスター読込処理
      ******************************************************************
       900-PTHKNINF-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTHKNINF-REC
      *         
               IF      PTHKN-HKNNUM (1:1)
                                       NOT =   "0"
                   GO  TO  900-PTHKNINF-READ-N-SEC
               END-IF
               IF  (   PTHKN-SAKJOKBN      =   "1"                     )
               OR  ( ( PTHKN-HKNNUM        =   "002"   OR  "031"   OR
                                               "032"   OR  "033"   OR
                                               "034"                  )
                 AND ( PTHKN-HOJOKBN       =   "A" OR  "B" OR  "C" OR
                                               "D" OR  "E" OR  "F" OR
                                               "G" OR  "H" OR  "I" OR
                                               "1" OR  "2" OR  "3"    ))
                   GO  TO  900-PTHKNINF-READ-N-SEC
               ELSE    
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    1                   TO  FLG-PTHKNINF-OK
               END-IF    
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
      *    患者マスター読込処理
      ******************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                      PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
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
                   MOVE    1               TO  FLG-PTKOHINF-OK
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

