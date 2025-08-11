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
       PROGRAM-ID.             ORAPION014R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 訪問診療患者一括資格確認照会処理
      *                     （資格確認結果登録処理）
      *  管理者            :
      *  作成日付    作業者        記述
      *  25/02/05    ORCAMO        新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    xml格納用データ
           SELECT  XML-FILE        ASSIGN  XMLPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-XML.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    xml格納用データ
       FD  XML-FILE.
       01  XML-REC                     PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
      *    レセプト電算ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //XMLPARA//.
      *
       01  XMLPARA-BASENAME.
           03  XMLPARA-HOSPNUM         PIC 9(02).
           03  FILLER                  PIC X(10)   VALUE   "ONSHIDOUI_".
           03  XMLPARA-PROCESS-CLASS   PIC X(01).
           03  FILLER                  PIC X(04)   VALUE   ".TXT".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-XML                 PIC X(02).
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-JOBKANRI            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
            03  FLG-END                PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-CNT                 PIC 9(10).
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
           03  WRK-TEMPDIR-ROOT        PIC X(1024).
      *
      *    ジョブ管理マスタ
       01  WRK-JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC"
                                   REPLACING   //JOB-//
                                   BY          //WRK-JOB-//.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値（訪問診療）
           03  WRK-CONS-HOUMON-JOB-SHELLID
                                       PIC X(08)   VALUE   "P02V001".
      *    ジョブ管理ＤＢのキー値（オンライン診療）
           03  WRK-CONS-ONLINE-JOB-SHELLID
                                       PIC X(08)   VALUE   "P02O001".
      *
           03  WRK-CONS-JOB-SHELLID    PIC X(08)   VALUE   SPACE.
           03  WRK-CONS-JOB-JOBID      PIC 9(07)   VALUE   ZERO.
      *
           03  WRK-CONS-RESULT-MAX     PIC 9(04)   VALUE   2.
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID        PIC X(08)   VALUE   "OR071ERR".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUAAPPREQ3.INC".
      *    API XML書き込み用定義体
           COPY    "CPONLINEQUAAPPRES1.INC".
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1001.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPSHELLTBL.INC".
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
           COPY    "ORCA71SCRAREA.INC".
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
           DISPLAY   "*   onlinequaapp3  start   *"
           DISPLAY   "****************************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
           INITIALIZE                      WRK-JOBKANRI-REC 
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "999"
                                       OR  "E89"
               CONTINUE
           ELSE
      *        XML情報CLOSE
               PERFORM 3000-RGTXMLCLOSE-SEC
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
           DISPLAY   "****************************"
           DISPLAY   "*   onlinequaapp3    end   *"
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
      *    一時ディレクトリのルートディレクトリ取得
           ACCEPT  WRK-TEMPDIR-ROOT
                               FROM    ENVIRONMENT "MCP_TEMPDIR_ROOT"
      *    一時ディレクトリ名組み立て
           STRING  WRK-TEMPDIR-ROOT    DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   SPA-TERMID          DELIMITED   BY  SPACE
                   LOW-VALUE           DELIMITED   BY  SIZE
                                       INTO        MCP-TEMPDIR
           END-STRING
      *
      *    環境変数［MCP_TEMPDIR］を生成（MCP-TEMPDIRの値を設定）
           SET ENVIRONMENT "MCP_TEMPDIR"
                                   TO  MCP-TEMPDIR
      *    環境変数［ORCA_UID］を生成（SPA-TERMID値を設定）
           SET ENVIRONMENT "ORCA_UID"
                                   TO  SPA-TERMID
      *    一時ディレクトリ作成
           CALL    "CBL_CREATE_DIR"    USING   MCP-TEMPDIR
           IF    ( RETURN-CODE     NOT =   ZERO )
               MOVE    "E80"               TO  WRK-ERRCD
           END-IF
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLOPEN-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報OPEN
      *****************************************************************
       1000-RGTXMLOPEN-SEC         SECTION.
      *
           IF      APILST06-BODY   NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-ONLINEQUAAPPREQ3
      *
           MOVE    "MSGOPEN"           TO  MCP-FUNC
           MOVE    "xml_onlinequaappreq3"
                                       TO  MCP-TABLE
           MOVE    APILST06-BODY       TO  QUAAPPREQ3-OBJECT
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEQUAAPPREQ3
           IF     (MCP-RC              =   ZERO  OR  1  )
               DISPLAY "MSGOPEN OK = " MCP-RC
           ELSE
               DISPLAY "MSGOPEN ERR failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      FLG-END         =   1
      *        対象なし
               MOVE   "E98"             TO  WRK-ERRCD
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    １件目のみ入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
      *
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    QUAAPPREQ3-PROCESS-CLASS
                   WHEN    "2"
                       MOVE    WRK-CONS-HOUMON-JOB-SHELLID
                                           TO  WRK-CONS-JOB-SHELLID
                   WHEN    "3"
                       MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                           TO  WRK-CONS-JOB-SHELLID
               END-EVALUATE        
      *        資格確認結果登録処理
               PERFORM 2002-JOBKANRI-UPDATE-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   GO  TO  200-MAIN-EXT
               END-IF
      *        処理件数を設定         
               MOVE    WRK-JOB-UPDCNT  TO  WRK-CNT
      *     
               IF      QUAAPPREQ3-RESULT-KBN
                                       =   "1"
      *            ＸＭＬ情報からファイル作成処理
                   PERFORM 2003-XML-SYORI-SEC
      *            処理実施
                   PERFORM 2004-SIKAKU-KAKUNIN-SYORI-SEC
               END-IF
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC         SECTION.
      *
      *
           MOVE    "MSGREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinequaappreq3"
                                       TO  MCP-TABLE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEQUAAPPREQ3
           IF     (MCP-RC              =   ZERO )
               PERFORM 10001-XML-REMAKE-SEC
      *test
      *             DISPLAY '----'
      *             DISPLAY "QQUAAPPREQ3-PROCESS-CLASS = "
      *                      QUAAPPREQ3-PROCESS-CLASS
      *             DISPLAY "QUAAPPREQ3-RESULT-KBN="
      *                     QUAAPPREQ3-RESULT-KBN         
      *             DISPLAY '----'
      *test
           ELSE
               DISPLAY "MSGREAD end  = " MCP-RC
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      * 
      *****************************************************************
      *    読み込んだXMLのLOW-VALUEをSPACEに置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      QUAAPPREQ3-HO-DOUI-STARYMD  =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-DOUI-STARYMD
           END-IF
           IF      QUAAPPREQ3-HO-DOUI-ENDYMD   =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-DOUI-ENDYMD
           END-IF
           IF      QUAAPPREQ3-HO-SRYKBN        =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-SRYKBN
           END-IF
           IF      QUAAPPREQ3-HO-RESULT        =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-RESULT
           END-IF
           IF      QUAAPPREQ3-HO-RESULT-CODE   =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-RESULT-CODE
           END-IF
           IF      QUAAPPREQ3-SHO-HKNJANUM     =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-SHO-HKNJANUM
           END-IF  
           IF      QUAAPPREQ3-SHO-KIGO         =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-SHO-KIGO    
           END-IF  
           IF      QUAAPPREQ3-SHO-NUM          =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-SHO-NUM     
           END-IF  
           IF      QUAAPPREQ3-SHO-EDABAN       =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-SHO-EDABAN  
           END-IF  
           IF      QUAAPPREQ3-SHO-BIRTHDAY     =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-SHO-BIRTHDAY
           END-IF  
           IF      QUAAPPREQ3-GENDO-SHOKBN     =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-GENDO-SHOKBN
           END-IF  
           IF      QUAAPPREQ3-GENDO-TEKKBN     =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-GENDO-TEKKBN
           END-IF  
           IF      QUAAPPREQ3-SHO-SIKIBETU     =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-SHO-SIKIBETU
           END-IF  
           IF      QUAAPPREQ3-RESULT           =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-RESULT      
           END-IF  
           IF      QUAAPPREQ3-RESULT-CODE      =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-RESULT-CODE 
           END-IF  
           IF      QUAAPPREQ3-RESULT-MSG       =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-RESULT-MSG  
           END-IF
           IF      QUAAPPREQ3-SIKAKU-YUKO      =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-SIKAKU-YUKO 
           END-IF  
           IF      QUAAPPREQ3-HO-DOUIFLG       =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-DOUIFLG  
           END-IF  
           IF      QUAAPPREQ3-HO-TIME          =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-TIME     
           END-IF  
           IF      QUAAPPREQ3-HO-KIGEN         =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-KIGEN    
           END-IF  
           IF      QUAAPPREQ3-HO-RIYU          =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-HO-RIYU     
           END-IF  
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-CONS-RESULT-MAX
               IF      QUAAPPREQ3-CARD-CLASS (IDX)     =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-CARD-CLASS   (IDX)
               END-IF
               IF      QUAAPPREQ3-RES-HKNJANUM (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-HKNJANUM (IDX)
               END-IF  
               IF      QUAAPPREQ3-RES-KIGO (IDX)      =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-KIGO     (IDX)
               END-IF  
               IF      QUAAPPREQ3-RES-NUM (IDX)        =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-NUM      (IDX)
               END-IF  
               IF      QUAAPPREQ3-RES-EDABAN (IDX)     =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-EDABAN   (IDX)
               END-IF  
               IF      QUAAPPREQ3-RES-HONKZKKBN (IDX)  =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-HONKZKKBN(IDX)
               END-IF  
               IF      QUAAPPREQ3-RES-HIHKNJANAME (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-HIHKNJANAME
                                                                   (IDX)
               END-IF 
               IF      QUAAPPREQ3-RES-NAME (IDX)       =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-NAME   (IDX)
               END-IF 
               IF      QUAAPPREQ3-RES-NAME-ETC (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-NAME-ETC (IDX)
               END-IF 
               IF      QUAAPPREQ3-RES-KANANAME (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-KANANAME (IDX)
               END-IF 
               IF      QUAAPPREQ3-RES-KANANAME-ETC (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-KANANAME-ETC
                                                                   (IDX)
               END-IF 
               IF      QUAAPPREQ3-RES-SEX1 (IDX)       =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-SEX1     (IDX)
               END-IF  
               IF      QUAAPPREQ3-RES-SEX2 (IDX)       =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-SEX2     (IDX)
               END-IF  
               IF      QUAAPPREQ3-RES-BIRTHDAY (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-BIRTHDAY (IDX)
               END-IF  
               IF      QUAAPPREQ3-RES-ADRS  (IDX)      =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-ADRS     (IDX)
               END-IF 
               IF      QUAAPPREQ3-RES-POST  (IDX)      =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-RES-POST     (IDX)
               END-IF  
               IF      QUAAPPREQ3-JOIN-SKKGETYMD (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO
                                       QUAAPPREQ3-JOIN-SKKGETYMD   (IDX)
               END-IF  
               IF      QUAAPPREQ3-JOIN-SKKLOSSYMD (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO
                                       QUAAPPREQ3-JOIN-SKKLOSSYMD  (IDX)
               END-IF  
               IF      QUAAPPREQ3-SKKGETYMD(IDX)       =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-SKKGETYMD    (IDX)
               END-IF  
               IF      QUAAPPREQ3-TEKSTYMD (IDX)       =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-TEKSTYMD     (IDX)
               END-IF  
               IF      QUAAPPREQ3-TEKEDYMD (IDX)       =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-TEKEDYMD     (IDX)
               END-IF  
               IF      QUAAPPREQ3-FTNRATE  (IDX)       =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-FTNRATE      (IDX)
               END-IF  
               IF      QUAAPPREQ3-MISYUGAKU (IDX)      =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-MISYUGAKU    (IDX)
               END-IF  
               IF      QUAAPPREQ3-LOSS-JIYU (IDX)      =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-LOSS-JIYU    (IDX)
               END-IF  
               IF      QUAAPPREQ3-HKNJANUM-NAME (IDX)  =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HKNJANUM-NAME(IDX)
               END-IF 
               IF      QUAAPPREQ3-ELDER-SKKGETYMD (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-ELDER-SKKGETYMD
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-ELDER-TEKSTYMD (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-ELDER-TEKSTYMD
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-ELDER-TEKEDYMD (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-ELDER-TEKEDYMD
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-ELDER-FTNRATE (IDX)  =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-ELDER-FTNRATE
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-GENDO-HENKOU-UM (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-GENDO-HENKOU-UM
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-DOUIFLG (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-GENDO-DOUIFLG
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-TIME (IDX)  =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-GENDO-TIME
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-KIGEN (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-GENDO-KIGEN
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-SHOKBN (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-GENDO-SHOKBN
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-TEKKBN (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-GENDO-TEKKBN
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-SKKGETYMD (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-GENDO-SKKGETYMD
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-TEKSTYMD (IDX)  =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-GENDO-TEKSTYMD
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-TEKEDYMD (IDX)  =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-GENDO-TEKEDYMD
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-GENDO-SKJGNSTYMD (IDX)
                                                   =   LOW-VALUE
                   MOVE    SPACE       TO
                                   QUAAPPREQ3-HO-GENDO-SKJGNSTYMD  (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-SIKKAN-DOUIFLG (IDX)
                                                   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-SIKKAN-DOUIFLG
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-SIKKAN-TIME (IDX)     =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-SIKKAN-TIME
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-SIKKAN-KIGEN (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-SIKKAN-KIGEN
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-SIKKAN-RIYU (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-SIKKAN-RIYU
                                                                   (IDX)
               END-IF  
               PERFORM VARYING IDY    FROM    1   BY  1
                       UNTIL   IDY    >       3
                   IF      QUAAPPREQ3-HO-SIKKAN-SIPKBN (IDX IDY)
                                                  =   LOW-VALUE
                       MOVE    SPACE       TO
                               QUAAPPREQ3-HO-SIKKAN-SIPKBN     (IDX IDY)
                   END-IF  
                   IF      QUAAPPREQ3-HO-SIKKAN-SKJGNSTYMD (IDX IDY)
                                                  =   LOW-VALUE
                       MOVE    SPACE     TO
                               QUAAPPREQ3-HO-SIKKAN-SKJGNSTYMD (IDX IDY)
                   END-IF  
                   IF      QUAAPPREQ3-HO-SIKKAN-TEKSTYMD (IDX IDY)
                                                  =   LOW-VALUE
                       MOVE    SPACE       TO
                               QUAAPPREQ3-HO-SIKKAN-TEKSTYMD   (IDX IDY)
                   END-IF  
                   IF      QUAAPPREQ3-HO-SIKKAN-TEKEDYMD (IDX IDY)
                                                  =   LOW-VALUE
                       MOVE    SPACE       TO
                               QUAAPPREQ3-HO-SIKKAN-TEKEDYMD   (IDX IDY)
                   END-IF  
                   IF      QUAAPPREQ3-HO-SIKKAN-FTNMONEY (IDX IDY)
                                                  =   LOW-VALUE
                       MOVE    SPACE       TO
                               QUAAPPREQ3-HO-SIKKAN-FTNMONEY   (IDX IDY)
                   END-IF
               END-PERFORM  
               PERFORM VARYING IDY    FROM    1   BY  1
                       UNTIL   IDY    >       3
                   IF      QUAAPPREQ3-HO-FUJYO-KOFUNUM (IDX IDY)
                                              =   LOW-VALUE
                       MOVE    SPACE       TO
                               QUAAPPREQ3-HO-FUJYO-KOFUNUM     (IDX IDY)
                   END-IF
                   IF      QUAAPPREQ3-HO-FUJYO-SRYYM  (IDX IDY)
                                              =   LOW-VALUE
                       MOVE    SPACE       TO
                               QUAAPPREQ3-HO-FUJYO-SRYYM       (IDX IDY)
                   END-IF  
                   IF      QUAAPPREQ3-HO-TICKET-TYPE  (IDX IDY)
                                              =   LOW-VALUE
                       MOVE    SPACE       TO
                           QUAAPPREQ3-HO-TICKET-TYPE           (IDX IDY)
                   END-IF  
                   IF      QUAAPPREQ3-HO-MEDICAL-TYPE (IDX IDY)
                                              =   LOW-VALUE
                       MOVE    SPACE       TO
                           QUAAPPREQ3-HO-MEDICAL-TYPE          (IDX IDY)
                   END-IF  
               END-PERFORM
               IF      QUAAPPREQ3-HO-KENSIN-DOUIFLG (IDX)  =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-KENSIN-DOUIFLG
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-KENSIN-TIME (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-KENSIN-TIME
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-KENSIN-KIGENYMD (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-KENSIN-KIGENYMD
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-KENSIN-RIYU (IDX)     =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-KENSIN-RIYU
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-YAKUZAI-DOUIFLG (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-YAKUZAI-DOUIFLG
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-YAKUZAI-TIME (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-YAKUZAI-TIME
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-YAKUZAI-KIGEN (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-YAKUZAI-KIGEN
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-YAKUZAI-RIYU (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-YAKUZAI-RIYU
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-SHINRYO-DOUIFLG (IDX) =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-SHINRYO-DOUIFLG
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-SHINRYO-TIME (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-SHINRYO-TIME
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-SHINRYO-KIGEN (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-SHINRYO-KIGEN
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-SHINRYO-RIYU (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-SHINRYO-RIYU
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-OPE-DOUIFLG (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-OPE-DOUIFLG
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-OPE-TIME (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-OPE-TIME (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-OPE-KIGEN (IDX)   =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-OPE-KIGEN
                                                                   (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-OPE-RIYU (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-OPE-RIYU (IDX)
               END-IF  
               IF      QUAAPPREQ3-HO-ONSHI-CREYMD (IDX)    =   LOW-VALUE
                   MOVE    SPACE       TO  QUAAPPREQ3-HO-ONSHI-CREYMD
                                                                   (IDX)
               END-IF  
           END-PERFORM        
           IF      QUAAPPREQ3-SHOKAI-NUM   =   LOW-VALUE
               MOVE  SPACE             TO  QUAAPPREQ3-SHOKAI-NUM         
           END-IF  

      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC          SECTION.
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
           IF      APILST06-BODY       NOT =   LOW-VALUE
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
               MOVE  QUAAPPRES1-OBJECT    TO  APILST06-BODY
               MOVE  "application/xml" TO  APILST06-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報CLOSE
      *****************************************************************
       3000-RGTXMLCLOSE-SEC        SECTION.
      *
           MOVE    "MSGCLOSE"          TO  MCP-FUNC
           MOVE    "xml_onlinequaappreq3"
                                       TO  MCP-TABLE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEQUAAPPREQ3
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "MSGCLOSE ERR failure = " MCP-RC
           END-IF
      *
           .
       3000-RGTXMLCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC          SECTION.
      *
           IF      QUAAPPREQ3-RESULT-KBN
                               =   "1"    OR  "9" 
                CONTINUE
           ELSE
               MOVE    "E12"       TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      QUAAPPREQ3-PROCESS-CLASS
                               =   "2" OR  "3"
                CONTINUE
           ELSE
               MOVE    "E16"       TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理更新処理（総処理件数）
      *****************************************************************
       2002-JOBKANRI-UPDATE-SEC    SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    QUAAPPREQ3-FILE-SIKIBETU (1:36)
                                   TO  JOB-KARTE-UID
           PERFORM 900-JOBKANRI-READ-SEC
           IF      FLG-JOBKANRI    =   1
               MOVE    "E14"           TO  WRK-ERRCD
           ELSE
      *        訪問診療(同意一括)のとき
               IF      QUAAPPREQ3-PROCESS-CLASS
                                       =   "2" OR  "3"
                   MOVE    "0003"          TO  JOB-ERRCD
                   MOVE    "1"             TO  JOB-SYOKBN
               END-IF

               MOVE    QUAAPPREQ3-RESULT-KBN
                                       TO  JOB-SYOKBN
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ3-ALL-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-PID1
               END-IF    
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ3-OK-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-PID2
               END-IF
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAAPPREQ3-ERR-CNT
                                       TO  SNUM-INX
               CALL    "ORCSNUM"       USING
                                       ORCSNUMAREA
               IF      SNUM-RC         =   ZERO
                   MOVE    SNUM-OUTNUM     TO  JOB-PID3
               END-IF
      *
               IF      JOB-PID2        =   ZERO
                   MOVE    "E01 処理件数（(正常）が０件です。"
                                           TO  JOB-YOBI
                   MOVE    "E01"           TO  WRK-ERRCD
               END-IF
      *
               MOVE    JOBKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          >   ZERO
                   DISPLAY "JOBKANRI UPDATE ERR=" MCP-RC
                   MOVE    "E23"           TO  WRK-ERRCD
               ELSE    
      *            対象ＪＯＢＫＡＮＲＩ取得
      *            MOVE    JOB-JOBID       TO  WRK-CONS-JOB-JOBID
                   MOVE    JOBKANRI-REC    TO  WRK-JOBKANRI-REC     
               END-IF
           END-IF
          .
       2002-JOBKANRI-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＸＭＬ情報からファイル作成処理
      *****************************************************************
       2003-XML-SYORI-SEC          SECTION.
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    SPA-HOSPNUM     TO  XMLPARA-HOSPNUM
           MOVE    QUAAPPREQ3-PROCESS-CLASS
                                   TO  XMLPARA-PROCESS-CLASS
           MOVE    XMLPARA-BASENAME
                                   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  XMLPARA
      *
           OPEN    OUTPUT  XML-FILE
      * 
           PERFORM     UNTIL       FLG-END     =   1
               MOVE    XML-ONLINEQUAAPPREQ3
                                       TO  XML-REC
               WRITE   XML-REC
               ADD  1              TO  WRK-CNT
      *        XML情報取り出し
               PERFORM 1000-RGTXMLREAD-SEC
           END-PERFORM
      *
           CLOSE   XML-FILE     
      *
      *    読み込んだ件数が総処理件数以下の時     
           IF      WRK-CNT          <   WRK-JOB-PID1
                IF      WRK-ERRCD       =   SPACE
                   MOVE    "Yes"           TO  QUAAPPRES1-CONT-FLG
                   MOVE    WRK-CNT         TO  QUAAPPRES1-LAST-YYKNO
               END-IF    
           END-IF
      *
           .
       2003-XML-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理実施処理
      *****************************************************************
       2004-SIKAKU-KAKUNIN-SYORI-SEC           SECTION.
      *
      *    ジョブ管理チェック処理
      *****PERFORM 800-JOBKANRI-CHK-SEC
      *****IF      WRK-ERRCD       NOT =   SPACE
      *****    GO  TO  2004-SIKAKU-KAKUNIN-SYORI-EXT
      *****END-IF
      *     
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    エラーファイル名
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *
      *    処理スクリプト
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "onshi_doui01.sh"   TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    WRK-JOB-JOBID       TO  SHELLTBL-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-SHELLID
           MOVE    WRK-JOB-JOBID       TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG2
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG3
           MOVE    XMLPARA-BASENAME    TO  SHELLTBL-ARG4
           MOVE    ERRFLPARA-BASENAME  TO  SHELLTBL-ARG5
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       2004-SIKAKU-KAKUNIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-JOB-JOBID   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "E70"           TO  WRK-ERRCD
                   MOVE    SPACE           TO  WRK-ERRMSG
                   STRING  "処理中です【"  DELIMITED  BY  SIZE
                           JOB-SHELLMSG    DELIMITED  BY  SPACE
                           "】"            DELIMITED  BY  SIZE
                                           INTO  WRK-ERRMSG
                   END-STRING
                   GO  TO  800-JOBKANRI-CHK-EXT
               END-IF
           END-IF
           .
       800-JOBKANRI-CHK-EXT.
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
               WHEN    "E01"
                   MOVE    "処理件数（(正常）が０件です"
                                                   TO  WRK-ERRMSG
               WHEN    "E11"
                   MOVE    "最終予約番号の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
                   MOVE    "処理結果区分の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
                   MOVE    "ＵＩＤが未設定です"    TO  WRK-ERRMSG
               WHEN    "E14"
               WHEN    "E17"
                   MOVE    "ＵＩＤに一致する処理がありません"
                                                   TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "リクエストが空です。"  TO  WRK-ERRMSG
               WHEN    "E16"
                   MOVE    "プロセス識別の設定に誤りがあります"
                                                   TO  WRK-ERRMSG
               WHEN    "E20"
                   MOVE    "資格確認レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E21"
                    MOVE
                "医療扶助交付番号格納レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E22"
                   MOVE
                   "資格確認訪問診療レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E23"
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
      *****************************************************************
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-SEC       SECTION.
      * 
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
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
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-JOBKANRI-READ-EXT.
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

