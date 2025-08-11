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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI042R3V3.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 未請求設定
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/01/01    NMED−藤原    新規作成
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "R01SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SPASCR              PIC 9(01).
      *
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SEIKYU              PIC 9(01).
           03  FLG-SEIKYU-UMU          PIC 9(01).
           03  FLG-SYUKA               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-API-PARA-FLG        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
      *    一時領域
      *
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
           03  WRK-ENDYMD.
               05  WRK-ENDYY           PIC 9(04).
               05  WRK-ENDMM           PIC 9(02).
               05  WRK-ENDDD           PIC 9(02).
      *
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-WARNING             PIC X(03).
           03  WRK-WARNING-MSG         PIC X(100).
      *
           03  WRK-PATIENT-MAX         PIC 9(03).
      *
      *    オルカUID
           03  WRK-ORCA-UID            PIC X(36).
           03  WRK-OPID                PIC X(16).
      *
           03  HZN-SPA-OPID            PIC X(16).
      *
           03  WRK-ENDKBN              PIC X(01).
      *
      *    MCP-TERMID
           03  WRK-MCP-TERM            PIC X(64).
      *
           03  WRK-JOB-STARTYMD        PIC X(08).
           03  WRK-JOB-STARTTIME.
                05  WRK-JOB-START-HH   PIC 9(02).
                05  WRK-JOB-START-MM   PIC 9(02).
                05  FILLER             PIC X(02).
      *
       01  WRK-PATIENT-MSG-AREA.
           03  WRK-PATIENT-ERRCD       PIC X(03).
           03  WRK-PATIENT-ERRMSG      PIC X(100).
           03  WRK-PATIENT-WARNING     PIC X(03).
           03  WRK-PATIENT-WARNING-MSG PIC X(100).
      *
       01  WRK-XML-AREA.
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-SRYYM               PIC X(06).
      *
      *    共通パラメタ内容
       01  WRK-APIV3DATA-AREA.
      *    電子カルテUID
      *****03  WRK-KARTE-UID           PIC X(36).
           03  WRK-APIV3DATA-REC.
               05  WRK-UNCLAIMEDQ-PERFORM-MONTH
                                           PIC X(7).
               05  WRK-UNCLAIMEDQ-INOUT    PIC X(1).
               05  WRK-UNCLAIMEDQ-UNCLAIMED-INF.
                   07  WRK-UNCLAIMEDQ-UNCLAIMED-INF-O  OCCURS  100.
                       09  WRK-UNCLAIMED-PTNUM PIC X(20).
               05  WRK-UNCLAIMEDQ-CLAIMD-INF.
                   07  WRK-UNCLAIMEDQ-CLAIMD-INF-O     OCCURS  100.
                       09  WRK-CLAIMED-PTNUM   PIC X(20).
                       09  WRK-CLAIMED-SRYYM   PIC X(07).
               05  WRK-UNCLAIMEDQ-NYUGAIKBN
                                           PIC X(01).
               05  WRK-UNCLAIMEDQ-SRYYM    PIC X(06).
      *
           03  WRK-APIV3DATA-REC1.
               05  WRK-NAI-INF.
                   07  WRK-UNCLAIMED-NAI-INFORMATION   OCCURS  100.
                       09  WRK-UNCLAIMED-NAI-PTID
                                               PIC 9(10).
                       09  WRK-UNCLAIMED-NAI-WARNING
                                               PIC X(03).
                   07  WRK-CLAIMD-NAI-INFORMATION      OCCURS  100.
                       09  WRK-CLAIMED-NAI-PTID
                                               PIC 9(10).
                       09  WRK-CLAIMED-NAI-SRYYM
                                               PIC X(06).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
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
      *   
           COPY    "MCPAREA"       REPLACING  //MCP//
                                   BY         //TAIHI-MCP//.
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
           03  WRK-CONS-LINE-MISEIKYU-MAX
                                   PIC 9(03)   VALUE   500.
           03  WRK-CONS-LINE-KAIJYO-MAX
                                   PIC 9(03)   VALUE   500.
           03  WRK-CONS-LINE-PTNUM-MAX
                                   PIC 9(03)   VALUE   100.
      *
           03  WRK-CONS-SRYYM-200610
                                   PIC X(06)   VALUE   "200610".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY     "CPORCSGDAY.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    API XML読込用定義体
           COPY    "CPUNCLAIMEDV3REQ.INC".
      *    API XML出力用定義体
           COPY    "CPUNCLAIMEDV3RES.INC".
      *
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK3003.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPRCF010.INC".
      *
           COPY    "CPKOHSKY.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    APIパラメタテーブル
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA42SCRAREA.INC".
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
           DISPLAY   "*********************"
           DISPLAY   "*  unclaimed start  *"
           DISPLAY   "*********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
           INITIALIZE                      SPA-AREA
                                           SPA-R01KYOUTU
                                           SPA-R01FREE
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
               MOVE   404                  TO  APILST04-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "*  unclaimed end  *"
           DISPLAY   "*******************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-UNCLAIMEDRES
           INITIALIZE                      WRK-APIV3DATA-AREA
      *
      *    更新日取得
           INITIALIZE                      ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Unclaimed_Info"    TO  UNCLAIMEDS-RESKEY
      *
           STRING  "A+"
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    日付・時間出力編集
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  UNCLAIMEDS-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  UNCLAIMEDS-INFORMATION-TIME
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
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  UNCLAIMEDS-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
                                           WRK-WARNING
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    UNCLAIMEDQ-REQUEST-NUMBER
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
      *??
           DISPLAY "2001=" WRK-ERRCD "#" 
      *??
           IF    ( WRK-ERRCD           =   SPACE )
               CONTINUE
           ELSE
               GO  TO  200-MAIN-EXT
           END-IF
      *
           EVALUATE    UNCLAIMEDQ-REQUEST-NUMBER
               WHEN    "01"
      *            処理確認処理
                   PERFORM 2002-UNCLAIMED-CHECK-SEC
               WHEN    "02"
      *            処理実施処理
                   PERFORM 2003-UNCLAIMED-MAKE-SEC
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
           INITIALIZE                      XML-UNCLAIMEDREQ
           INITIALIZE                      XML-APIREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_unclaimedv3req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST04-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "unclaimedv3req"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-UNCLAIMEDREQ
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
                       MOVE    "01"    TO  UNCLAIMEDS-RESPONSE-NUMBER
                   WHEN    OTHER
                       MOVE    "02"    TO  UNCLAIMEDS-RESPONSE-NUMBER
               END-EVALUATE
       *
               EVALUATE    FLG-SYORI
                   WHEN    1
                       MOVE    "処理確認終了"
                                       TO  UNCLAIMEDS-API-RESULT-MSG
                   WHEN    2
                       MOVE    "処理実施終了"
                                       TO  UNCLAIMEDS-API-RESULT-MSG
                   WHEN    9
                       MOVE    "情報取得終了"
                                       TO  UNCLAIMEDS-API-RESULT-MSG
               END-EVALUATE
           ELSE
               EVALUATE    FLG-SYORI
                   WHEN    9
                       MOVE    "00"    TO  UNCLAIMEDS-RESPONSE-NUMBER
                   WHEN    OTHER
                       MOVE    "01"    TO  UNCLAIMEDS-RESPONSE-NUMBER
               END-EVALUATE
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  UNCLAIMEDS-API-RESULT
               MOVE    WRK-ERRMSG      TO  UNCLAIMEDS-API-RESULT-MSG
           END-IF
      *
      *    警告メッセージ
           IF      WRK-WARNING     NOT =   SPACE
               MOVE    WRK-WARNING     TO  UNCLAIMEDS-API-WARNING
               MOVE    WRK-WARNING-MSG TO  UNCLAIMEDS-API-WARNING-MSG
           END-IF
      *
           MOVE    UNCLAIMEDQ-KARTE-UID TO  UNCLAIMEDS-KARTE-UID
           IF      FLG-SYORI           =   9
               CONTINUE
           ELSE
               IF      FLG-SYORI           =   1
                   IF    ( WRK-ERRCD           =   SPACE )
                       MOVE    WRK-ORCA-UID    TO  UNCLAIMEDS-ORCA-UID
                   END-IF
               ELSE
                   MOVE    UNCLAIMEDQ-ORCA-UID TO  UNCLAIMEDS-ORCA-UID
               END-IF
           END-IF
           MOVE    UNCLAIMEDQ-REQUEST-NUMBER
                                       TO  UNCLAIMEDS-REQUEST-NUMBER
           MOVE    UNCLAIMEDQ-PERFORM-MONTH
                                       TO  UNCLAIMEDS-PERFORM-MONTH
           MOVE    UNCLAIMEDQ-INOUT    TO  UNCLAIMEDS-INOUT
      *
      *    未請求設定患者一覧、未請求設定解除患者一覧の返却処理
           IF    ( FLG-SYORI           =   2   OR  9 )
           AND   ( WRK-ERRCD           =   SPACE     )
               PERFORM 3001-UNCLAIMED-INF-HENSYU-SEC
           END-IF
      *
           IF      APILST04-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_unclaimedv3res" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  UNCLAIMEDS-MODE
           MOVE    "unclaimedv3res"  TO  UNCLAIMEDS-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-UNCLAIMEDRES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  UNCLAIMEDS-OBJECT     TO  APILST04-BODY
               MOVE  "application/xml"     TO  APILST04-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    未請求設定患者一覧、未請求設定解除患者一覧の返却処理
      *****************************************************************
       3001-UNCLAIMED-INF-HENSYU-SEC   SECTION.
      *
           IF      UNCLAIMEDQ-REQUEST-NUMBER
                                       =   "00"    OR  "02"
      *        未請求設定患者一覧
               MOVE    ZERO            TO  IDX
               INITIALIZE                  SYUKA-REC
               MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    WRK-SRYYM       TO  SYUKA-SRYYM
               MOVE    WRK-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
               MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
               MOVE    "01"            TO  SYUKA-MISEIKYU-FLG
               MOVE    SYUKA-REC       TO  MCPDATA-REC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key19"         TO  MCP-PATHNAME
               MOVE    "DBSELECT"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key19"         TO  MCP-PATHNAME
                   PERFORM 910-SYUKA-NEXT-SEC
               ELSE
                  MOVE    1                   TO  FLG-SYUKA
               END-IF
      *
               PERFORM UNTIL   FLG-SYUKA   =   1
                   IF     IDX          >=  WRK-CONS-LINE-MISEIKYU-MAX
                       MOVE    "True"          TO
                                           UNCLAIMEDS-UNCLAIMED-OVERFLOW
                       MOVE    1               TO  FLG-SYUKA
                   ELSE
                       ADD     1               TO  IDX
                       MOVE    SYUKA-SRYYM     TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE (1:7)
                                               TO
                                       UNCLAIMEDS-UNCLAIMED-MONTH  (IDX)
                       INITIALIZE              ORCSPTIDAREA
                       MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                       MOVE    SYUKA-PTID      TO  SPTID-PTID
                       CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                   SPA-AREA
                       MOVE    SPTID-PTNUM     TO
                                       UNCLAIMEDS-UNCLAIMED-PTNUM  (IDX)
      *
                       MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                       MOVE    SYUKA-PTID      TO  PTINF-PTID
                       PERFORM 900-PTINF-READ-SEC
                       MOVE    PTINF-NAME      TO
                                       UNCLAIMEDS-UNCLAIMED-NAME   (IDX)
      *                新規登録分のチェック
                       IF    ( UNCLAIMEDQ-REQUEST-NUMBER
                                               =   "02"      )
                       AND   ( SYUKA-SRYYM     =   WRK-SRYYM )
                            PERFORM VARYING IDY     FROM    1   BY  1
                                   UNTIL   IDY     >
                                               WRK-CONS-LINE-PTNUM-MAX
                                   OR      WRK-UNCLAIMED-PTNUM (IDY)
                                                   =   SPACE
                               IF      SYUKA-PTID      =
                                           WRK-UNCLAIMED-NAI-PTID (IDY)
                                   MOVE    "True"      TO
                                       UNCLAIMEDS-UNCLAIMED-CHECK (IDX)
                                   MOVE    WRK-CONS-LINE-PTNUM-MAX
                                                       TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF
      *
                       MOVE    "tbl_syuka"     TO  MCP-TABLE
                       MOVE    "key19"         TO  MCP-PATHNAME
                       PERFORM 910-SYUKA-NEXT-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key19"         TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
      *        未請求設定解除患者一覧
               MOVE    ZERO            TO  IDX
               INITIALIZE                  SYUKA-REC
               MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    WRK-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
               MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
               MOVE    "02"            TO  SYUKA-MISEIKYU-FLG
               MOVE    SYUKA-REC       TO  MCPDATA-REC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key14"         TO  MCP-PATHNAME
               MOVE    "DBSELECT"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key14"         TO  MCP-PATHNAME
                   PERFORM 910-SYUKA-NEXT-SEC
               ELSE
                  MOVE    1                   TO  FLG-SYUKA
               END-IF
      *
               PERFORM UNTIL   FLG-SYUKA   =   1
                       OR      IDX     >=  WRK-CONS-LINE-KAIJYO-MAX
                   ADD     1           TO  IDX
                   MOVE    SYUKA-SRYYM         TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                           TO
                                       UNCLAIMEDS-CLAIMED-MONTH   (IDX)
                   INITIALIZE              ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM TO  SPTID-HOSPNUM
                   MOVE    SYUKA-PTID  TO  SPTID-PTID
                   CALL    "ORCSPTID"  USING   ORCSPTIDAREA
                                               SPA-AREA
                   MOVE    SPTID-PTNUM     TO
                                       UNCLAIMEDS-CLAIMED-PTNUM   (IDX)
      *
                   MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                   MOVE    SYUKA-PTID      TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   MOVE    PTINF-NAME      TO
                                       UNCLAIMEDS-CLAIMED-NAME    (IDX)
      *            新規登録分のチェック
                   IF      UNCLAIMEDQ-REQUEST-NUMBER
                                           =   "02"
                       PERFORM VARYING IDY     FROM    1   BY  1
                               UNTIL   IDY >   WRK-CONS-LINE-PTNUM-MAX
                               OR      WRK-CLAIMED-PTNUM (IDY)
                                           =   SPACE
                           IF    ( SYUKA-PTID      =
                                           WRK-CLAIMED-NAI-PTID  (IDY) )
                           AND   ( SYUKA-SRYYM     =
                                           WRK-CLAIMED-NAI-SRYYM (IDY) )
                               MOVE    "True"      TO
                                       UNCLAIMEDS-CLAIMED-CHECK (IDX)
                               MOVE    WRK-CONS-LINE-PTNUM-MAX
                                               TO  IDY
                           END-IF
                       END-PERFORM
                   END-IF
      *
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key14"         TO  MCP-PATHNAME
                   PERFORM 910-SYUKA-NEXT-SEC
               END-PERFORM
      *
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key14"         TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
          .
       3001-UNCLAIMED-INF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC          SECTION.
      *
      *    カルテUID
           IF      UNCLAIMEDQ-KARTE-UID =   SPACE
               MOVE    "E04"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *   オルカUID
           IF      UNCLAIMEDQ-REQUEST-NUMBER
                                       =   "00"    OR  "01"
               CONTINUE
           ELSE
               IF      UNCLAIMEDQ-ORCA-UID  =   SPACE
                   MOVE    "E05"               TO  WRK-ERRCD
               END-IF
           END-IF
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    ジョブ管理チェック処理（レセプト作成処理中）
           PERFORM 800-JOBKANRI-CHK-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      UNCLAIMEDQ-REQUEST-NUMBER
                                       =   "02"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      UNCLAIMEDQ-REQUEST-NUMBER
                                       =   "01"
      *        カルテＵＩＤが違う時の１分以上のチェック
               PERFORM 20021-PARA-INIT-SEC
               IF      FLG-APIPARA     =   ZERO
                   IF      UNCLAIMEDQ-KARTE-UID
                                       NOT =   WRK-APIPARA-KARTE-UID
                       MOVE    WRK-APIPARA-UPHMS   TO  WRK-JOB-STARTTIME
                       MOVE    WRK-APIPARA-UPYMD   TO  WRK-JOB-STARTYMD
                       IF      WRK-JOB-START-MM    <   55
                           COMPUTE WRK-JOB-START-MM
                                                   =   WRK-JOB-START-MM
                                                   +   1
                       ELSE
                           COMPUTE WRK-JOB-START-MM
                                                   =   WRK-JOB-START-MM
                                                   -   55
                           IF      WRK-JOB-START-HH    <   23
                               COMPUTE WRK-JOB-START-HH
                                                   =   WRK-JOB-START-HH
                                                   +   1
                           ELSE
                               MOVE    0           TO  WRK-JOB-START-HH
                               INITIALIZE              STS-AREA-DAY
                               INITIALIZE              LNK-DAY6-AREA
                               MOVE    "61"        TO  LNK-DAY6-IRAI
                               MOVE    JOB-STARTYMD
                                                   TO  LNK-DAY6-KIJUN 
                               MOVE    "1"         TO  LNK-DAY6-ZOGENPTN
                               MOVE    1           TO  LNK-DAY6-ZOGEN
                               CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                               MOVE    LNK-DAY6-KEISAN
                                                   TO  WRK-JOB-STARTYMD
                           END-IF
                       END-IF
      *
                       IF      SMCNDATE-YMD    >   WRK-JOB-STARTYMD
                           CONTINUE
                       ELSE
                           IF      SMCNDATE-YMD    =   WRK-JOB-STARTYMD
                               IF      SMCNDATE-HMS
                                                  <=  WRK-JOB-STARTTIME
                                   MOVE    "E71"           TO  WRK-ERRCD
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               IF      WRK-ERRCD      NOT =   SPACE
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF 
      *
      *    入外区分
           EVALUATE    UNCLAIMEDQ-INOUT
               WHEN    "I"
                   IF      SPA-BEDSU       >   ZERO
                       MOVE    "1"             TO  WRK-NYUGAIKBN
                   ELSE
                       MOVE    "E12"           TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               WHEN    "O"
                   MOVE    "2"             TO  WRK-NYUGAIKBN
               WHEN    OTHER
                   MOVE    "E12"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
           END-EVALUATE
      *
      *    診療年月
           IF      UNCLAIMEDQ-PERFORM-MONTH
                                       =   SPACE
               IF      UNCLAIMEDQ-REQUEST-NUMBER
                                           =   "00"
                   MOVE    "999999"            TO  WRK-SRYYM
               ELSE
      *            未請求設定患者一覧設定時は必須
                   IF      UNCLAIMEDQ-UNCLAIMED-INF
                                           NOT =   SPACE
                       MOVE    "E14"               TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   ELSE
                       MOVE    "999999"            TO  WRK-SRYYM
                   END-IF
               END-IF
           ELSE
               MOVE    UNCLAIMEDQ-PERFORM-MONTH
                                           TO  WRK-HEN-DATE
               PERFORM 802-DAYHEN02-SEC
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-SYMD (1:6)      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    WRK-SYMD (1:6)  TO  WRK-SRYYM
                   IF      WRK-SRYYM       <   WRK-CONS-SRYYM-200610
                       MOVE    "E15"           TO  WRK-ERRCD
                       GO  TO  2001-INPUT-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "E11"               TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           IF      UNCLAIMEDQ-REQUEST-NUMBER
                                       =   "00"
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
      *    患者番号が未設定のとき
           IF     ( UNCLAIMEDQ-UNCLAIMED-INF
                                       =   SPACE )
           AND    ( UNCLAIMEDQ-CLAIMED-INF
                                       =   SPACE )
               MOVE    "E16"               TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
          INITIALIZE                   WRK-APIV3DATA-AREA
      *
      *    初画面情報
           MOVE    SPACE               TO  LINKAREA
                                           SPA-RIDCD-AREA
                                           SPA-RID1-AREA
           MOVE    "R03"               TO  SPA-MOTOPG
                                           SPA-R01-MOTOPG
           MOVE    "1"                 TO  SPA-R01-SYOKBN
           MOVE    "0"                 TO  SPA-R01-RECEKBN
           MOVE    "1"                 TO  SPA-R01-FILEKBN
           IF      WRK-SRYYM           =   "999999"
               MOVE    SMCNDATE-YMD (1:6)  TO  SPA-R01-SRYYM
           ELSE
               MOVE    WRK-SRYYM           TO  SPA-R01-SRYYM
           END-IF
           MOVE    "1"                 TO  SPA-R01-PRTKBN
      *
           MOVE    WRK-NYUGAIKBN       TO  SPA-R01-NYUGAIKBN
           MOVE    SPA-R01-SRYYM       TO  SPA-SYSYMD (1:6)
           MOVE    "20"                TO  SPA-SYSYMD (7:2)
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 803-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-SYSYMDWH
      *??
            display "spa=" spa-sysymd "#" spa-sysymdwh "#"
      *??
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           PERFORM 800-ORCGR96-CALL-SEC
      *
      *    未請求設定患者チェック
           MOVE    ZERO            TO  WRK-PATIENT-MAX
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       100
                   OR      UNCLAIMEDQ-UNCLAIMED-PTNUM (IDZZ)
                                   =       SPACE
      *
               ADD     1               TO  WRK-PATIENT-MAX
               MOVE    UNCLAIMEDQ-UNCLAIMED-PTNUM (IDZZ)
                                       TO  UNCLAIMEDS-UNCLAIMED-PTNUM
                                                       (WRK-PATIENT-MAX)
                                           WRK-UNCLAIMED-PTNUM
                                                       (WRK-PATIENT-MAX)
      *
               MOVE    SPACE               TO  SPA-ERRCD
                                               WRK-PATIENT-MSG-AREA
      *        患者番号の重複チェック
               IF      IDZZ            >   1
                   COMPUTE IDZ =   IDZZ    -   1
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       IDZ
                       IF      UNCLAIMEDQ-UNCLAIMED-PTNUM (IDZZ)
                                   =   UNCLAIMEDQ-UNCLAIMED-PTNUM (IDYY)
                           MOVE    "E29"           TO  WRK-PATIENT-ERRCD
                           MOVE    "同じ内容で設定してあります。"
                                                   TO  SPA-ERRMSG
                           MOVE    100             TO  IDYY
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF      WRK-PATIENT-ERRCD   =   SPACE
      *            患者存在チェック
                   MOVE    UNCLAIMEDQ-UNCLAIMED-PTNUM (IDZZ)
                                           TO  R96-PTNUM
                   MOVE    SPACE           TO  MCP-STATUS
                   MOVE    "ACTIVATE"      TO  MCP-EVENT
                   MOVE    "PTNUM"         TO  MCP-WIDGET
                   PERFORM 800-ORCGR96-CALL-SEC
      *
                   IF      SPA-ERRCD       =   SPACE
      *                未請求設定一覧に存在
                       IF      SPA-NAI-R96-SELNUM
                                               >   ZERO
                           MOVE    "E20"       TO  WRK-PATIENT-ERRCD
                       END-IF
      *                未請求解除一覧に存在
                       IF      SPA-NAI-R96-KAIJYO-SELNUM
                                               >   ZERO 
                           MOVE    "W21"       TO  WRK-PATIENT-WARNING
                       END-IF
                   ELSE
                       EVALUATE    SPA-ERRCD
                           WHEN    "0001"
                           WHEN    "0003"
                               MOVE    "E10"   TO  WRK-PATIENT-ERRCD
                           WHEN    "0006"
                               MOVE    "E17"   TO  WRK-PATIENT-ERRCD
                           WHEN    "0010"
                               MOVE    "E18"   TO  WRK-PATIENT-ERRCD
                           WHEN    OTHER
                               MOVE    "E50"   TO  WRK-PATIENT-ERRCD
                       END-EVALUATE
                       MOVE    SPA-ERRMSG      TO  WRK-PATIENT-ERRMSG
                   END-IF
      *
      *            未請求設定患者情報処理
                   PERFORM 20011-UNCLAIMED-PATIENT-SEC
      *
      *            APIパラメタ編集処理
                   MOVE    SPA-NAI-R96-PTID
                                       TO  WRK-UNCLAIMED-NAI-PTID
                                                   (WRK-PATIENT-MAX)
                   MOVE    WRK-PATIENT-WARNING
                                       TO  WRK-UNCLAIMED-NAI-WARNING
                                                   (WRK-PATIENT-MAX)
      *
      *            クリア処理
                   MOVE    "PUTG"          TO  MCP-STATUS
                   MOVE    "CLICKED"       TO  MCP-EVENT
                   MOVE    "B02"           TO  MCP-WIDGET
                   PERFORM 800-ORCGR96-CALL-SEC
               END-IF
      *
      *        未請求設定患者エラー処理
               PERFORM 20012-UNCLAIMED-ERR-SEC
           END-PERFORM
      *
      *
      *    未請求設定解除患者チェック
           MOVE    ZERO            TO  WRK-PATIENT-MAX
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       100
                   OR (    UNCLAIMEDQ-CLAIMED-PTNUM (IDZZ)
                                   =   SPACE
                       AND UNCLAIMEDQ-CLAIMED-MONTH (IDZZ)
                                   =   SPACE )
      *
               ADD     1               TO  WRK-PATIENT-MAX
               MOVE    UNCLAIMEDQ-CLAIMED-PTNUM (IDZZ)
                                       TO  UNCLAIMEDS-CLAIMED-PTNUM
                                                       (WRK-PATIENT-MAX)
                                           WRK-CLAIMED-PTNUM
                                                       (WRK-PATIENT-MAX)
               MOVE    UNCLAIMEDQ-CLAIMED-MONTH (IDZZ)
                                       TO  UNCLAIMEDS-CLAIMED-MONTH
                                                       (WRK-PATIENT-MAX)
                                           WRK-CLAIMED-SRYYM
                                                       (WRK-PATIENT-MAX)
      *
               MOVE    SPACE               TO  SPA-ERRCD
                                               WRK-PATIENT-MSG-AREA
      *        重複チェック
               IF      IDZZ            >   1
                   COMPUTE IDZ =   IDZZ    -   1
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       IDZ
                       IF    ( UNCLAIMEDQ-CLAIMED-PTNUM (IDZZ)
                                   =   UNCLAIMEDQ-CLAIMED-PTNUM (IDYY) )
                       AND   ( UNCLAIMEDQ-CLAIMED-MONTH (IDZZ)
                                   =   UNCLAIMEDQ-CLAIMED-MONTH (IDYY) )
                           MOVE    "E29"           TO  WRK-PATIENT-ERRCD
                           MOVE    "同じ内容で設定してあります。"
                                                   TO  SPA-ERRMSG
                           MOVE    100             TO  IDYY
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF      WRK-PATIENT-ERRCD   =   SPACE
      *            診療年月チェック
                   IF      UNCLAIMEDQ-CLAIMED-MONTH (IDZZ)
                                               =   SPACE
                       MOVE    "E14"               TO  WRK-PATIENT-ERRCD
                   ELSE
                       MOVE    UNCLAIMEDQ-CLAIMED-MONTH (IDZZ)
                                               TO  WRK-HEN-DATE
                       PERFORM 802-DAYHEN02-SEC
                       MOVE    WRK-SYMD (1:6)  TO  R96-SRYYM
                       MOVE    SPACE           TO  MCP-STATUS
                       MOVE    "ACTIVATE"      TO  MCP-EVENT
                       MOVE    "SRYYM"         TO  MCP-WIDGET
                       PERFORM 800-ORCGR96-CALL-SEC
      *
                       IF      SPA-ERRCD   NOT =   SPACE
                           EVALUATE    SPA-ERRCD
                               WHEN    "0007"
                                   MOVE    "E11"   TO  WRK-PATIENT-ERRCD
                               WHEN    "0008"
                                   MOVE    "E12"   TO  WRK-PATIENT-ERRCD
                               WHEN    "0009"
                                   MOVE    "E15"   TO  WRK-PATIENT-ERRCD
                               WHEN    OTHER
                                   MOVE    "E50"   TO  WRK-PATIENT-ERRCD
                           END-EVALUATE
                           MOVE    SPA-ERRMSG      TO WRK-PATIENT-ERRMSG
                       END-IF
                   END-IF
               END-IF
      *
               IF      WRK-PATIENT-ERRCD   =   SPACE
                   IF      UNCLAIMEDQ-CLAIMED-PTNUM (IDZZ)
                                               =   SPACE
                       MOVE    "E19"               TO  WRK-PATIENT-ERRCD
                   ELSE
      *                患者存在チェック
                       MOVE    UNCLAIMEDQ-CLAIMED-PTNUM (IDZZ)
                                               TO  R96-PTNUM
                       MOVE    SPACE           TO  MCP-STATUS
                       MOVE    "ACTIVATE"      TO  MCP-EVENT
                       MOVE    "PTNUM"         TO  MCP-WIDGET
                       PERFORM 800-ORCGR96-CALL-SEC
      *
                       IF      SPA-ERRCD       =   SPACE
      *                    未請求設定一覧に存在しない
                           IF      SPA-NAI-R96-SELNUM
                                                   >   ZERO
                               CONTINUE
                           ELSE
                               IF      SPA-NAI-R96-KAIJYO-SELNUM
                                                   >   ZERO
                                   MOVE    "E21"   TO  WRK-PATIENT-ERRCD
                               ELSE
                                   MOVE    "E22"   TO  WRK-PATIENT-ERRCD
                               END-IF
                           END-IF
                       ELSE
                           EVALUATE    SPA-ERRCD
                               WHEN    "0001"
                               WHEN    "0003"
                                   MOVE    "E10"   TO  WRK-PATIENT-ERRCD
                               WHEN    "0006"
                                   MOVE    "E17"   TO  WRK-PATIENT-ERRCD
                               WHEN    "0007"
                                   MOVE    "E11"   TO  WRK-PATIENT-ERRCD
                               WHEN    "0009"
                                   MOVE    "E15"   TO  WRK-PATIENT-ERRCD
                               WHEN    "0010"
                                   MOVE    "E18"   TO  WRK-PATIENT-ERRCD
                               WHEN    OTHER
                                   MOVE    "E50"   TO  WRK-PATIENT-ERRCD
                           END-EVALUATE
                           MOVE    SPA-ERRMSG      TO WRK-PATIENT-ERRMSG
                       END-IF
      *
      *                未請求解除患者情報処理
                       PERFORM 20013-CLAIMED-PATIENT-SEC
      *
                       IF    ( WRK-PATIENT-ERRCD
                                               =   SPACE )
                       AND   ( WRK-PATIENT-WARNING
                                               =   SPACE )
      *                    APIパラメタ編集処理
                           MOVE    SPA-NAI-R96-PTID
                                           TO   WRK-CLAIMED-NAI-PTID
                                                       (WRK-PATIENT-MAX)
                           MOVE    SPA-NAI-R96-SRYYM
                                           TO   WRK-CLAIMED-NAI-SRYYM
                                                       (WRK-PATIENT-MAX)
                       END-IF
      *
      *                クリア処理
                       MOVE    "PUTG"          TO  MCP-STATUS
                       MOVE    "CLICKED"       TO  MCP-EVENT
                       MOVE    "B02"           TO  MCP-WIDGET
                       PERFORM 800-ORCGR96-CALL-SEC
                   END-IF
               END-IF
      *
      *        未請求解除患者エラー処理
               PERFORM 20014-CLAIMED-ERR-SEC
           END-PERFORM
      *
          .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定患者情報処理
      *****************************************************************
       20011-UNCLAIMED-PATIENT-SEC     SECTION.
      *
           MOVE    SPA-GMN-R96-NAME    TO   UNCLAIMEDS-UNCLAIMED-NAME
                                                       (WRK-PATIENT-MAX)
      *
           IF    ( WRK-PATIENT-ERRCD   =   SPACE )
           AND   ( SPA-ERRCD           =   SPACE )
      *        レセプト一覧（未請求設定患者患者）編集処理
               PERFORM VARYING     IDX FROM    1   BY  1
                       UNTIL       IDX >       SPA-GMN-R96-PTNUM-TBL-MAX
                   MOVE    SPA-GMN-R96-PTNUM-TTEISYUTUSAKI (IDX)
                                       TO
                               UNCLAIMEDS-UNCLAIMED-TEISYUTU
                                                   (WRK-PATIENT-MAX IDX)
                   MOVE    SPA-GMN-R96-PTNUM-TRECESYUBETU(IDX)
                                       TO
                               UNCLAIMEDS-UNCLAIMED-CODE
                                                   (WRK-PATIENT-MAX IDX)
                   MOVE    SPA-GMN-R96-PTNUM-THKNJANUM   (IDX)
                                       TO
                               UNCLAIMEDS-UNCLAIMED-HKNJANUM
                                                   (WRK-PATIENT-MAX IDX)
                   MOVE    SPA-GMN-R96-PTNUM-THOJOKBN    (IDX)
                                       TO
                               UNCLAIMEDS-UNCLAIMED-HOJOKBN
                                                   (WRK-PATIENT-MAX IDX)
               END-PERFORM
      *
      *        請求管理テーブルの存在チェック
               INITIALIZE                  RECE10-REC
               MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
               MOVE    WRK-SRYYM       TO  RECE10-SRYYM
               MOVE    WRK-NYUGAIKBN   TO  RECE10-NYUGAIKBN
               MOVE    SPA-NAI-R96-PTID
                                       TO  RECE10-PTID
               PERFORM 900-SEIKYU-READ-SEC
               IF      FLG-SEIKYU-UMU  =   1
                   MOVE    "W20"           TO  WRK-PATIENT-WARNING
               END-IF
           END-IF
            .
       20011-UNCLAIMED-PATIENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定患者エラー処理
      *****************************************************************
       20012-UNCLAIMED-ERR-SEC        SECTION.
      *
           IF    ( WRK-PATIENT-ERRCD
                                   =   SPACE )
               IF      WRK-PATIENT-WARNING
                                   NOT =   SPACE
                   MOVE    WRK-PATIENT-WARNING
                                       TO  UNCLAIMEDS-UNCLAIMED-WARNING
                                                       (WRK-PATIENT-MAX)
                   EVALUATE    WRK-PATIENT-WARNING
                       WHEN    "W20"
                           MOVE    "請求データを削除して設定します。"
                                               TO
                                       UNCLAIMEDS-UNCLAIMED-WARNINGMSG
                                                       (WRK-PATIENT-MAX)
                       WHEN    "W21"
                           MOVE
                               "未請求設定解除の設定が行われています。"
                                               TO
                                       UNCLAIMEDS-UNCLAIMED-WARNINGMSG
                                                       (WRK-PATIENT-MAX)
                   END-EVALUATE
      *
      *??          IF      WRK-WARNING     =   SPACE
      *??              MOVE    UNCLAIMEDS-UNCLAIMED-WARNING
      *??                                              (WRK-PATIENT-MAX)
      *??                                      TO  WRK-WARNING
      *??              MOVE    UNCLAIMEDS-UNCLAIMED-WARNINGMSG
      *??                                              (WRK-PATIENT-MAX)
      *??                                      TO  WRK-WARNING-MSG
      *??          END-IF
               END-IF
           ELSE
               MOVE    WRK-PATIENT-ERRCD
                                   TO   UNCLAIMEDS-UNCLAIMED-ERRCD
                                                       (WRK-PATIENT-MAX)
               EVALUATE    WRK-PATIENT-ERRCD
                   WHEN    "E20"
                       MOVE    "未請求設定の設定が行われています。"
                                               TO
                                       UNCLAIMEDS-UNCLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E21"
                       MOVE
                               "未請求設定解除の設定が行われています。"
                                               TO
                                       UNCLAIMEDS-UNCLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E29"
                       MOVE    "同じ内容で設定してあります。"
                                               TO
                                       UNCLAIMEDS-UNCLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E10"
                       MOVE    "該当の患者が存在しません。"
                                               TO
                                       UNCLAIMEDS-UNCLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E17"
                   WHEN    "E18"
                   WHEN    "E50"
                       MOVE    WRK-PATIENT-ERRMSG
                                               TO
                                       UNCLAIMEDS-UNCLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
               END-EVALUATE
      *
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "E69"           TO  WRK-ERRCD
                   MOVE    "設定内容に誤りがあります。"
                                           TO  WRK-ERRMSG
               END-IF
           END-IF
            .
       20012-UNCLAIMED-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求解除患者情報処理
      *****************************************************************
       20013-CLAIMED-PATIENT-SEC     SECTION.
      *
           MOVE    SPA-GMN-R96-NAME    TO   UNCLAIMEDS-CLAIMED-NAME
                                                       (WRK-PATIENT-MAX)
      *
           IF    ( WRK-PATIENT-ERRCD   =   SPACE )
           AND   ( SPA-ERRCD           =   SPACE )
      *        レセプト一覧編集処理
               PERFORM VARYING     IDX     FROM    1   BY  1
               UNTIL       IDX     >       SPA-GMN-R96-PTNUM-TBL-MAX
                   MOVE    SPA-GMN-R96-PTNUM-TTEISYUTUSAKI (IDX)
                                       TO
                               UNCLAIMEDS-CLAIMED-TEISYUTU
                                                   (WRK-PATIENT-MAX IDX)
                   MOVE    SPA-GMN-R96-PTNUM-TRECESYUBETU(IDX)
                                       TO
                               UNCLAIMEDS-CLAIMED-CODE
                                                   (WRK-PATIENT-MAX IDX)
                   MOVE    SPA-GMN-R96-PTNUM-THKNJANUM   (IDX)
                                       TO
                               UNCLAIMEDS-CLAIMED-HKNJANUM
                                                   (WRK-PATIENT-MAX IDX)
                   MOVE    SPA-GMN-R96-PTNUM-THOJOKBN    (IDX)
                                       TO
                               UNCLAIMEDS-CLAIMED-HOJOKBN
                                                   (WRK-PATIENT-MAX IDX)
               END-PERFORM
            END-IF
            .
       20013-CLAIMED-PATIENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求解除患者エラー処理
      *****************************************************************
       20014-CLAIMED-ERR-SEC     SECTION.
      *
           IF    ( WRK-PATIENT-ERRCD   =   SPACE )
           AND   ( SPA-ERRCD           =   SPACE )
               CONTINUE
           ELSE
               MOVE    WRK-PATIENT-ERRCD
                                   TO   UNCLAIMEDS-CLAIMED-ERRCD
                                                       (WRK-PATIENT-MAX)
               EVALUATE    WRK-PATIENT-ERRCD
                   WHEN    "E21"
                       MOVE    "未請求設定解除の設定が行われています。"
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E22"
                       MOVE    "未請求設定の設定が行われていません。"
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E29"
                       MOVE    "同じ内容で設定してあります。"
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E10"
                       MOVE    "該当の患者が存在しません。"
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E11"
                       MOVE    "診療年月が暦日ではありません。"
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E14"
                       MOVE    "診療年月が未設定です。"
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E12"
                       MOVE    "入外区分の設定に誤りがあります。"
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E19"
                       MOVE    "患者番号が未設定です。"
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
                   WHEN    "E15"
                   WHEN    "E17"
                   WHEN    "E18"
                   WHEN    "E50"
                       MOVE    WRK-PATIENT-ERRMSG
                                               TO
                                       UNCLAIMEDS-CLAIMED-ERRMSG
                                                       (WRK-PATIENT-MAX)
               END-EVALUATE
      *
               IF      WRK-ERRCD       =   SPACE
                   MOVE    "E69"           TO  WRK-ERRCD
                   MOVE    "設定内容に誤りがあります。"
                                           TO  WRK-ERRMSG
               END-IF
           END-IF
            .
       20014-CLAIMED-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       2002-UNCLAIMED-CHECK-SEC    SECTION.
      *
      *    オルカＵＩＤ処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *    APIパラメタ登録処理
           PERFORM 20022-APIPARA-INS-SEC
           .
       2002-UNCLAIMED-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＵＩＤコード取得処理
      *****************************************************************
       20020-ORCAUID-SYORI-SEC        SECTION.
      *
           IF      UNCLAIMEDQ-REQUEST-NUMBER
                                       =   "01"
      *        UIDコード処理
               PERFORM 1001-UID-GET-SEC
               MOVE    C-UID               TO  WRK-ORCA-UID
           ELSE
               MOVE    UNCLAIMEDQ-ORCA-UID TO  WRK-ORCA-UID
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
           MOVE    "AP42"              TO  APIPARA-GYOUMUID
      *****MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API42R3V3"         TO  APIPARA-FILEMEI
      *****MOVE    1                   TO  APIPARA-EDANUM
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 20021-PARA-READ-NEXT-SEC
           ELSE
               INITIALIZE                  APIPARA-REC
               MOVE    1               TO  FLG-APIPARA
           END-IF
      *
           PERFORM             UNTIL   FLG-APIPARA   =   1
               IF      APIPARA-EDANUM      =   1
                   MOVE    APIPARA-REC         TO  WRK-APIPARA-REC
                   MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC
                   MOVE    WRK-UNCLAIMEDQ-NYUGAIKBN
                                               TO  WRK-NYUGAIKBN
                   MOVE    WRK-UNCLAIMEDQ-SRYYM
                                               TO  WRK-SRYYM
                   ADD     1                   TO  FLG-API-PARA-FLG
               END-IF
               IF      APIPARA-EDANUM      =   2
                   MOVE    APIPARA-DATA-REC    TO  WRK-APIV3DATA-REC1
                   ADD     1                   TO  FLG-API-PARA-FLG
               END-IF
      *
               PERFORM 20021-PARA-READ-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-API-PARA-FLG    =   2
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
           MOVE    "key8"              TO  MCP-PATHNAME
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
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP42"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API42R3V3"         TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
      *    レスポンス番号・電子カルテUID保存
           MOVE    UNCLAIMEDQ-KARTE-UID
                                       TO  APIPARA-KARTE-UID
           MOVE    UNCLAIMEDQ-PERFORM-MONTH
                                       TO  WRK-UNCLAIMEDQ-PERFORM-MONTH
           MOVE    UNCLAIMEDQ-INOUT    TO  WRK-UNCLAIMEDQ-INOUT
           MOVE    WRK-NYUGAIKBN       TO  WRK-UNCLAIMEDQ-NYUGAIKBN
           MOVE    WRK-SRYYM           TO  WRK-UNCLAIMEDQ-SRYYM
      *
           MOVE    WRK-APIV3DATA-REC   TO  APIPARA-DATA-REC
      *
           MOVE    SMCNDATE-YMD        TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  APIPARA-UPHMS
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
       *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "AP42"              TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API42R3V3"         TO  APIPARA-FILEMEI
           MOVE    2                   TO  APIPARA-EDANUM
      *
           MOVE    UNCLAIMEDQ-KARTE-UID
                                       TO  APIPARA-KARTE-UID
           MOVE    WRK-APIV3DATA-REC1  TO  APIPARA-DATA-REC
      *
           MOVE    SMCNDATE-YMD        TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  APIPARA-UPHMS
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
           END-IF
           .
       20022-APIPARA-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理実施処理
      *****************************************************************
       2003-UNCLAIMED-MAKE-SEC     SECTION.
      *
      *    オルカＵＩＤ処理
           PERFORM 20020-ORCAUID-SYORI-SEC
      *
      *    APIパラメタチェック処理
           PERFORM 20021-PARA-INIT-SEC
      *
           IF      FLG-APIPARA     =   ZERO
               IF    ( UNCLAIMEDQ-KARTE-UID
                                   NOT =   WRK-APIPARA-KARTE-UID  )
                   MOVE    "E41"           TO  WRK-ERRCD
               ELSE 
                   IF    ( SPA-TERMID  NOT =   WRK-APIPARA-TERMID     )
                       MOVE    "E43"           TO  WRK-ERRCD
                   ELSE 
                       IF    ( UNCLAIMEDQ-PERFORM-MONTH
                               NOT =   WRK-UNCLAIMEDQ-PERFORM-MONTH )
                       OR    ( UNCLAIMEDQ-INOUT
                               NOT =   WRK-UNCLAIMEDQ-INOUT         )
                       OR    ( UNCLAIMEDQ-UNCLAIMED-INF
                               NOT =   WRK-UNCLAIMEDQ-UNCLAIMED-INF )
                       OR    ( UNCLAIMEDQ-CLAIMED-INF
                               NOT =   WRK-UNCLAIMEDQ-CLAIMD-INF    )
                           MOVE    "E42"           TO  WRK-ERRCD
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE    "E40"           TO  WRK-ERRCD
           END-IF
      *
           IF      WRK-ERRCD      NOT =   SPACE
               GO  TO  2003-UNCLAIMED-MAKE-EXT
           END-IF
      *
           INITIALIZE                  UNCLAIMEDS-UNCLAIMED-INF
                                       UNCLAIMEDS-CLAIMED-INF
      *
      *??
            DISPLAY "NYUGAIKBN=" WRK-NYUGAIKBN "#" WRK-SRYYM "#"
      *??
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       100
                   OR      WRK-UNCLAIMED-PTNUM (IDZZ)
                                   =       SPACE
      *??
            DISPLAY "UNCLAIMED=" IDZZ "#" WRK-UNCLAIMED-PTNUM (IDZZ) "#"
                          WRK-UNCLAIMED-NAI-WARNING (IDZZ) "#"
                          WRK-UNCLAIMED-NAI-PTID (IDZZ) "#" 
      *??
               IF      WRK-UNCLAIMED-NAI-WARNING (IDZZ)
                                       =   "W20"
      *            請求管理削除
                   INITIALIZE                  RECE10-REC
                   MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
                   MOVE    WRK-SRYYM       TO  RECE10-SRYYM
                   MOVE    WRK-NYUGAIKBN   TO  RECE10-NYUGAIKBN
                   MOVE    WRK-UNCLAIMED-NAI-PTID (IDZZ)
                                           TO  RECE10-PTID
                   MOVE    1               TO  RECE10-TEISYUTUSAKI
                   MOVE    2               TO  RECE10-XXTEISYUTUSAKI 
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "del7"          TO  MCP-PATHNAME
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "E62"           TO  WRK-ERRCD
                       GO  TO  2003-UNCLAIMED-MAKE-EXT
                   END-IF
      *
                   INITIALIZE                  RECE10-REC
                   MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
                   MOVE    WRK-SRYYM       TO  RECE10-SRYYM
                   MOVE    WRK-NYUGAIKBN   TO  RECE10-NYUGAIKBN
                   MOVE    WRK-UNCLAIMED-NAI-PTID (IDZZ)
                                           TO  RECE10-PTID
                   MOVE    6               TO  RECE10-TEISYUTUSAKI
                   MOVE    6               TO  RECE10-XXTEISYUTUSAKI 
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "del7"          TO  MCP-PATHNAME
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "E63"           TO  WRK-ERRCD
                       GO  TO  2003-UNCLAIMED-MAKE-EXT
                   END-IF
      *            公費請求書削除
                   INITIALIZE                  KOHSKY-REC
                   MOVE    SPA-HOSPNUM     TO  KOHSKY-HOSPNUM
                   MOVE    SPA-NAI-R96-SRYYM
                                           TO  KOHSKY-SRYYM
                   MOVE    WRK-NYUGAIKBN
                                           TO  KOHSKY-NYUGAIKBN
                   MOVE    WRK-UNCLAIMED-NAI-PTID (IDZZ)
                                           TO  KOHSKY-PTID
                   MOVE    KOHSKY-REC      TO  MCPDATA-REC
                   MOVE    "tbl_kohsky"    TO  MCP-TABLE
                   MOVE    "del4"          TO  MCP-PATHNAME
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "E64"           TO  WRK-ERRCD
                       GO  TO  2003-UNCLAIMED-MAKE-EXT
                   END-IF
      *            レセ電削除
                   INITIALIZE                  RECEDEN-REC
                   MOVE    SPA-HOSPNUM     TO  RECEDEN-HOSPNUM
                   MOVE    WRK-SRYYM       TO  RECEDEN-SRYYM
                   MOVE    WRK-NYUGAIKBN   TO  RECEDEN-NYUGAIKBN
                   MOVE    WRK-UNCLAIMED-NAI-PTID (IDZZ)
                                           TO  RECEDEN-PTID
                   MOVE    1               TO  RECEDEN-TEISYUTUSAKI
                   MOVE    RECEDEN-REC     TO  MCPDATA-REC
                   MOVE    "tbl_receden"   TO  MCP-TABLE
                   MOVE    "del4"          TO  MCP-PATHNAME
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "E65"           TO  WRK-ERRCD
                       GO  TO  2003-UNCLAIMED-MAKE-EXT
                   END-IF
                   INITIALIZE                  RECEDEN-REC
                   MOVE    SPA-HOSPNUM     TO  RECEDEN-HOSPNUM
                   MOVE    WRK-SRYYM       TO  RECEDEN-SRYYM
                   MOVE    WRK-NYUGAIKBN   TO  RECEDEN-NYUGAIKBN
                   MOVE    WRK-UNCLAIMED-NAI-PTID (IDZZ)
                                           TO  RECEDEN-PTID
                   MOVE    2               TO  RECEDEN-TEISYUTUSAKI
                   MOVE    RECEDEN-REC     TO  MCPDATA-REC
                   MOVE    "tbl_receden"   TO  MCP-TABLE
                   MOVE    "del4"          TO  MCP-PATHNAME
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "E65"           TO  WRK-ERRCD
                       GO  TO  2003-UNCLAIMED-MAKE-EXT
                   END-IF
                   INITIALIZE                  RECEDEN-REC
                   MOVE    SPA-HOSPNUM     TO  RECEDEN-HOSPNUM
                   MOVE    WRK-SRYYM       TO  RECEDEN-SRYYM
                   MOVE    WRK-NYUGAIKBN   TO  RECEDEN-NYUGAIKBN
                   MOVE    WRK-UNCLAIMED-NAI-PTID (IDZZ)
                                           TO  RECEDEN-PTID
                   MOVE    6               TO  RECEDEN-TEISYUTUSAKI
                   MOVE    RECEDEN-REC     TO  MCPDATA-REC
                   MOVE    "tbl_receden"   TO  MCP-TABLE
                   MOVE    "del4"          TO  MCP-PATHNAME
                   MOVE    "DBDELETE"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "E65"           TO  WRK-ERRCD
                       GO  TO  2003-UNCLAIMED-MAKE-EXT
                   END-IF
               END-IF
      *
               INITIALIZE                  SYUKA-REC
               MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    WRK-UNCLAIMED-NAI-PTID (IDZZ)
                                       TO  SYUKA-PTID
               MOVE    WRK-SRYYM       TO  SYUKA-SRYYM
               MOVE    WRK-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
               MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
               MOVE    SYUKA-REC       TO  MCPDATA-REC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key15"         TO  MCP-PATHNAME
               PERFORM 900-SYUKA-READ-SEC
               IF      FLG-SYUKA       =   1
                   INITIALIZE                  SYUKA-REC
                   MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
                   MOVE    WRK-UNCLAIMED-NAI-PTID (IDZZ)
                                           TO  SYUKA-PTID
                   MOVE    WRK-SRYYM       TO  SYUKA-SRYYM
                   MOVE    WRK-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
                   MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
               END-IF
      *
               MOVE    "01"            TO  SYUKA-MISEIKYU-FLG
               MOVE    SPA-TERMID      TO  SYUKA-TERMID
               MOVE    WRK-OPID        TO  SYUKA-OPID
      *
               IF      FLG-SYUKA       =   1
                   MOVE    SMCNDATE-YMD        TO  SYUKA-CREYMD
                   MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
               ELSE
                   MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
               END-IF
               MOVE    SYUKA-REC       TO  MCPDATA-REC
      *
               IF      FLG-SYUKA       =   ZERO
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "upd4"          TO  MCP-PATHNAME
               ELSE
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_syuka"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
               END-IF
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "E66"           TO  WRK-ERRCD
                   GO  TO  2003-UNCLAIMED-MAKE-EXT
               END-IF
           END-PERFORM
      *
      *    未請求設定解除更新処理
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       100
                   OR (    WRK-CLAIMED-PTNUM (IDZZ)
                                   =   SPACE
                       AND WRK-CLAIMED-SRYYM (IDZZ)
                                   =   SPACE )
      *??
            DISPLAY "CLAIMED=" IDZZ "#" WRK-CLAIMED-PTNUM (IDZZ) "#"
                          WRK-CLAIMED-SRYYM (IDZZ) "#"
                          WRK-CLAIMED-NAI-SRYYM (IDZZ) "#"
                          WRK-CLAIMED-NAI-PTID (IDZZ) "#" 
      *??
               INITIALIZE                  SYUKA-REC
               MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
               MOVE    WRK-CLAIMED-NAI-PTID  (IDZZ)
                                       TO  SYUKA-PTID
               MOVE    WRK-CLAIMED-NAI-SRYYM (IDZZ)
                                       TO  SYUKA-SRYYM
               MOVE    WRK-NYUGAIKBN   TO  SYUKA-NYUGAIKBN
               MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
               MOVE    "02"            TO  SYUKA-MISEIKYU-FLG
               MOVE    SPA-TERMID      TO  SYUKA-TERMID
               MOVE    WRK-OPID        TO  SYUKA-OPID
               MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
               MOVE    SYUKA-REC       TO  MCPDATA-REC
      *
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "upd4"          TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "E67"           TO  SPA-ERRCD
                   GO  TO  2003-UNCLAIMED-MAKE-EXT
               END-IF
           END-PERFORM
      *
      *    ＡＰＩパラメタ削除処理
      *
      *     APIパラメタ削除処理
           PERFORM 2902-PARA-ALLDEL-SEC
           IF      WRK-ERRCD       NOT = SPACE
             GO  TO  2003-UNCLAIMED-MAKE-EXT
           END-IF
      *
           .
       2003-UNCLAIMED-MAKE-EXT.
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
           MOVE    "AP42"              TO  APIPARA-GYOUMUID
      *****MOVE    SPA-TERMID          TO  APIPARA-TERMID
           MOVE    "API42R3V3"         TO  APIPARA-FILEMEI
      *
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
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
      *    未請求設定処理
      *****************************************************************
       800-ORCGR96-CALL-SEC                SECTION.
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R96-AREA    TO  SPA-FREE
      *
           CALL    "ORCGR96"           USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R96-AREA
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    TAIHI-MCPAREA   TO  MCPAREA
           .
       800-ORCGR96-CALL-EXT.
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
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
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
      *    和暦西暦変換処理
      *****************************************************************
       803-HIZUKE-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE OR
                                           ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
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
      *            レセプト作成、定期請求の処理中
                   GO  TO  890-ERRCD-MSG-EXT
               WHEN    "E69"
      *            個別対象患者エラー
                   GO  TO  890-ERRCD-MSG-EXT
           END-EVALUATE
      * 
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E04"
                   MOVE    "電子カルテＵＩＤが未設定です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E05"
                   MOVE    "オルカＵＩＤが未設定です。"
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
                   MOVE    "平成１８年１０月以降のみ設定可能です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E16"
                   MOVE    "設定がありません。"    TO  WRK-ERRMSG
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
               WHEN    "E62"
               WHEN    "E63"
                  MOVE    "請求管理が削除できませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E64"
                  MOVE    "公費請求書が削除できませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E65"
                  MOVE    "レセ電データが削除できませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E66"
                   MOVE    "未請求設定ができませんでした"
                                                   TO  WRK-ERRMSG
               WHEN    "E67"
                   MOVE    "未請求設定の解除ができませんでした"
                                                   TO  WRK-ERRMSG
               WHEN    "E71"
                   MOVE    "他からアクセス中です。"
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
               PERFORM 920-DBFETCH-SEC
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
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
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
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key15"         TO  MCP-PATHNAME
          PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key15"         TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYUKA
           END-IF
      *
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key15"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       910-SYUKA-NEXT-SEC         SECTION.
      * 
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-SYUKA
               MOVE    MCPDATA-REC          TO  SYUKA-REC
           ELSE
               MOVE    1                    TO  FLG-SYUKA
               INITIALIZE                       SYUKA-REC
           END-IF
      *
           .
       910-SYUKA-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（医保の対象データの存在チェック）
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SEIKYU-UMU
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           PERFORM                 UNTIL   FLG-SEIKYU      =   1
                                   OR      FLG-SEIKYU-UMU  =   1
               IF      RECE10-TEISYUTUSAKI =   1   OR  2   OR  6
                   MOVE    1               TO  FLG-SEIKYU-UMU
               END-IF
      * 
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
               MOVE    MCPDATA-REC     TO  RECE10-REC
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＡＲＡデータ検索処理
      *****************************************************************
       9010-APIPARA-KEYREAD-SEC         SECTION.
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
       9010-APIPARA-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルCALL処理
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-ORCDBMAIN-EXT.
           EXIT.
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
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
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
