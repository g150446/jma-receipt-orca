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
       PROGRAM-ID.           ORAPION012R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 資格確認　歯科診療情報登録、更新処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  22/10/XX    ORCAMO        新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-ONS-SRY-MAIN        PIC 9(01).
           03  FLG-ONS-SRY-SUB         PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-ONSKAKU             PIC 9(01).
           03  FLG-SHINRYO-MAIN        PIC 9(01).
           03  FLG-END                 PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDS                     PIC 9(04).
           03  IDD                     PIC 9(02).
           03  IDX-D                   PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HOSPCD              PIC X(10).
           03  WRK-SIKIBETU            PIC X(50).
           03  WRK-OPID                PIC X(16).
           03  WRK-PTID-AREA.
               05  WRK-PTID            PIC 9(10).
               05  WRK-PATIENT-ID      PIC X(20).
      *
           03  WRK-FILE-PATIENT-ID     PIC X(20).
      *
           03  WRK-TBL-UUID            PIC X(36).
      *
           03  WRK-KIGO                PIC X(80).
           03  WRK-NUM                 PIC X(80).
      *
           03  WRK-DAY-AREA.
               05  WRK-DAY             PIC X(01)   OCCURS 31.
           03  WRK-DATE.
               05  WRK-YYMM            PIC X(06).
               05  WRK-DD              PIC X(02).
           03  WRK-RENNUM              PIC 9(03).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-SRYYM           PIC X(06).
               05  WRK-KEY-NYUGAIKBN       PIC X(01).
               05  WRK-KEY-METRDIHCD       PIC X(10).
      *
           03  WRK-UID                 PIC X(36).
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
           03  WRK-ERRMSG              PIC X(200).
      *    全角変換用 
           03  WRK-KANACHK-MAE-INPUT   PIC X(5000).
           03  WRK-KANACHK-OUT-INPUT   PIC X(5000).
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUAREQ8.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUARES1.INC".
      *    API XML読み込み共通定義
      ***  COPY    "CPAPIV2REQ.INC".
      ***  COPY    "CPAPI21V3REQ.INC".
      *  01  XML-APIREQ.
      *      03  APIREQ-CONTEXT          PIC S9(9)   BINARY.
      *      03  APIREQ-OBJECT           PIC X(36).
      *      03  APIREQ-MODE             PIC S9(9)   BINARY.
      *      03  APIREQ-RECORDNAME       PIC X(128).
      *      03  APIREQ-PATIENTINFOREQ   PIC X(55000000).
      *
      *    COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
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
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    オン資格確認情報
       01  ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC".
      *
      *    資格確認診療行為情報メイン
       01  ONSHI-SHINRYO-MAIN-REC.
           COPY    "CPONSHI-SHINRYO-MAIN.INC".
      *
      *    資格確認診療行為情報サブ
       01  ONSHI-SHINRYO-SUB-REC.
           COPY    "CPONSHI-SHINRYO-SUB.INC".
      *    診療確認歯科情報サブ
       01  ONSHI-SHIKA-SUB-REC.
           COPY    "CPONSHI-SHIKA-SUB.INC".
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
           DISPLAY   "**********************"
           DISPLAY   "*  onlinequa8 start  *"
           DISPLAY   "**********************"
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
                                       OR  "E89"
               CONTINUE
           ELSE
      *        XML情報CLOSE
               PERFORM 3000-RGTXMLCLOSE-SEC
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
           DISPLAY   "**********************"
           DISPLAY   "*   onlinequa8 end   *"
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
           MOVE    WRK-HEN-DATE        TO  QUARES1-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  QUARES1-INFORMATION-TIME
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
           IF      APILST04-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-ONLINEQUAREQ8
      *
           MOVE    "MSGOPEN"           TO  MCP-FUNC
           MOVE    "xml_onlinemedicalreq2"
                                       TO  MCP-TABLE
           MOVE    APILST04-BODY       TO  QUAREQ8-OBJECT
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEQUAREQ8
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
      *****************************************************************
      *    XML情報OPEN
      *****************************************************************
       3000-RGTXMLCLOSE-SEC         SECTION.
      *
           MOVE    "MSGCLOSE"          TO  MCP-FUNC
           MOVE    "xml_onlinemedicalreq2"
                                       TO  MCP-TABLE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEQUAREQ8
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "MSGCLOSE ERR failure = " MCP-RC
           END-IF
      *
           .
       3000-RGTXMLCLOSE-EXT.
           EXIT.
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
      *    資格確認診療情報処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-SIKAKU-SHINRYO-SYORI-SEC
           END-IF
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
      *
           MOVE    "MSGREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinemedicalreq2"
                                       TO  MCP-TABLE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEQUAREQ8
           IF     (MCP-RC              =   ZERO )
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               DISPLAY "MSGREAD end  = " MCP-RC
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    読み込んだXMLのLOW-VALUE を  SPACE に置換
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      QUAREQ8-REQ-HKNJANUM   =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-REQ-HKNJANUM
           END-IF
           IF      QUAREQ8-REQ-KIGO   =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-REQ-KIGO
           END-IF
           IF      QUAREQ8-REQ-NUM    =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-REQ-NUM
           END-IF
           IF      QUAREQ8-REQ-EDABAN     =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-REQ-EDABAN
           END-IF
           IF      QUAREQ8-FILE-SIKIBETU  =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-FILE-SIKIBETU
           END-IF
           IF      QUAREQ8-RESULTCODE     =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-RESULTCODE
           END-IF
           IF      QUAREQ8-SRYYM      =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-SRYYM
           END-IF
           IF      QUAREQ8-NYUGAIKBN   =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-NYUGAIKBN
           END-IF
           IF      QUAREQ8-GRPNUM     =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-GRPNUM
           END-IF
           IF      QUAREQ8-DIORG      =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-DIORG
           END-IF
           IF      QUAREQ8-GRPNUM2    =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-GRPNUM2
           END-IF
           IF      QUAREQ8-PR-ISORG     =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-PR-ISORG
           END-IF
           IF      QUAREQ8-PR-METRDIHCD   =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-PR-METRDIHCD
           END-IF
           IF      QUAREQ8-PR-METRDIHNM   =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-PR-METRDIHNM
           END-IF
           IF      QUAREQ8-PR-PRLSHCD     =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-PR-PRLSHCD
           END-IF
           IF      QUAREQ8-PR-PRLSHNM     =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-PR-PRLSHNM
           END-IF
      ***
      *診療日別情報群
           IF      QUAREQ8-DI-DATE    =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-DI-DATE
           END-IF
           IF      QUAREQ8-PR-DATE    =   LOW-VALUE
               MOVE  SPACE             TO  QUAREQ8-PR-DATE
           END-IF
      *診療情報群
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   100
               IF      QUAREQ8-SRYKBN (IDX)   =   LOW-VALUE
                   MOVE    SPACE           TO  QUAREQ8-SRYKBN (IDX)
               END-IF
               IF      QUAREQ8-SRYCD (IDX)   =   LOW-VALUE
                   MOVE    SPACE           TO  QUAREQ8-SRYCD (IDX)
               END-IF
               IF      QUAREQ8-SRYNAME (IDX)   =   LOW-VALUE
                   MOVE    SPACE           TO  QUAREQ8-SRYNAME (IDX)
               END-IF
               IF      QUAREQ8-UNIT (IDX)  =   LOW-VALUE
                   MOVE    SPACE           TO  QUAREQ8-UNIT (IDX)
               END-IF
               IF      QUAREQ8-QUA1 (IDX)  =   LOW-VALUE
                   MOVE    SPACE           TO  QUAREQ8-QUA1 (IDX)
               END-IF
               IF      QUAREQ8-QUA2 (IDX)  =   LOW-VALUE
                   MOVE    SPACE           TO  QUAREQ8-QUA2 (IDX)
               END-IF
               IF      QUAREQ8-TIMES (IDX) =   LOW-VALUE
                   MOVE    SPACE           TO  QUAREQ8-TIMES (IDX)
               END-IF
      *        歯科診療行為加算情報
               PERFORM VARYING     IDY     FROM    1  BY  1
                       UNTIL       IDY     >   30
                   IF      QUAREQ8-DEN-ADDCD (IDX IDY) =   LOW-VALUE
                       MOVE    SPACE           TO  QUAREQ8-DEN-ADDCD
                                                           (IDX IDY)
                   END-IF
                   IF      QUAREQ8-DEN-ADDNAME (IDX IDY) =   LOW-VALUE
                       MOVE    SPACE           TO  QUAREQ8-DEN-ADDNAME
                                                           (IDX IDY)
                   END-IF
                   IF      QUAREQ8-DEN-QUA (IDX IDY)   =   LOW-VALUE
                       MOVE    SPACE           TO  QUAREQ8-DEN-QUA
                                                           (IDX IDY)
                   END-IF
                   IF      QUAREQ8-DEN-UNIT (IDX IDY)  =   LOW-VALUE
                       MOVE    SPACE           TO  QUAREQ8-DEN-UNIT
                                                           (IDX IDY)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "000"               TO  QUARES1-API-RESULT
               MOVE    "処理終了"          TO  QUARES1-API-RESULT-MSG
           ELSE
               DISPLAY "300 WRK-ERRCD=" WRK-ERRCD
               MOVE    WRK-ERRCD           TO  QUARES1-API-RESULT
               MOVE    WRK-ERRMSG          TO  QUARES1-API-RESULT-MSG
            END-IF
      *
           IF      APILST04-BODY       NOT =   LOW-VALUE
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
               MOVE  QUARES1-OBJECT    TO  APILST04-BODY
               MOVE  "application/xml" TO  APILST04-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
          .
       300-RGTXMLMAKE-EXT.
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
      ****
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *     処理結果区分(薬剤)
           IF      QUAREQ8-RESULTCODE  =   "1"
               CONTINUE
           ELSE
               MOVE    "E01"               TO  WRK-ERRCD
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認薬剤情報登録、更新処理
      *****************************************************************
       2002-SIKAKU-SHINRYO-SYORI-SEC      SECTION.
      *
      *    TBL_UUID
           MOVE    QUAREQ8-FILE-SIKIBETU(12:36) TO  WRK-TBL-UUID
      *    患者ID判定(要求任意のファイル識別子)
           IF      QUAREQ8-FILE-SIKIBETU(1:10)  NUMERIC
               MOVE    QUAREQ8-FILE-SIKIBETU(1:10) TO  WRK-PTID
           ELSE
               MOVE    ZERO                TO  WRK-PTID
               MOVE    "E10"               TO  WRK-ERRCD
               DISPLAY "0006 PTID ERR:"  WRK-PTID
               GO  TO  2002-SIKAKU-SHINRYO-SYORI-EXT
           END-IF
      *
      *    患者情報検索
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    WRK-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        =   00
               MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
           ELSE
               MOVE    "E10"               TO  WRK-ERRCD
               DISPLAY "0006 PTID ERR:"  WRK-PTID
               GO  TO  2002-SIKAKU-SHINRYO-SYORI-EXT
           END-IF
      *
      *    要求ファイル用患者番号編集 
           MOVE    SPACE           TO  WRK-FILE-PATIENT-ID
           STRING  WRK-PATIENT-ID      DELIMITED  BY  SPACE
                   "XXXXXXXXXXXXXXXXXXXX"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-FILE-PATIENT-ID
           END-STRING
      *
           MOVE    WRK-PATIENT-ID  TO  QUARES1-PATIENT-ID
      *
      *    被保険者証記号　全角変換
           MOVE    QUAREQ8-REQ-KIGO    TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-KIGO
      *    被保険者証番号　全角変換
           MOVE    QUAREQ8-REQ-NUM     TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-NUM
      *
      *    医療機関・薬局コード、診療年月、入外等区分毎に処理
           MOVE    SPACE               TO  WRK-KEY-AREA
      *
           PERFORM UNTIL      FLG-END  =   1
      *
               IF      (QUAREQ8-SRYYM      =   WRK-KEY-SRYYM )
                  AND  (QUAREQ8-NYUGAIKBN  =   WRK-KEY-NYUGAIKBN)
                  AND  (QUAREQ8-PR-METRDIHCD   =   WRK-KEY-METRDIHCD)
                   CONTINUE
               ELSE
                   IF      WRK-KEY-AREA    NOT =   SPACE
      *                診療情報メイン登録処理
                       PERFORM 20022-ONSHI-SHINMAIN-INS-SEC
                       MOVE    SPACE               TO  WRK-KEY-AREA
                   END-IF
      *            初期処理
                   PERFORM 20022-ONSHI-INIT-SEC
               END-IF
      *        診療情報サブ登録処理
               PERFORM 20023-ONSHI-SHINSUB-SEC
      *
      *        XML情報取り出し
               PERFORM 1000-RGTXMLREAD-SEC
           END-PERFORM
      *
           IF      WRK-KEY-AREA    NOT =   SPACE
      *        診療情報メイン登録処理
               PERFORM 20022-ONSHI-SHINMAIN-INS-SEC
           END-IF
           .
       2002-SIKAKU-SHINRYO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤情報資格確認　初期処理
     *****************************************************************
       20022-ONSHI-INIT-SEC     SECTION.
      *
           MOVE    QUAREQ8-SRYYM           TO  WRK-KEY-SRYYM
           MOVE    QUAREQ8-NYUGAIKBN       TO  WRK-KEY-NYUGAIKBN
           MOVE    QUAREQ8-PR-METRDIHCD    TO  WRK-KEY-METRDIHCD
      *
      *    削除処理
           PERFORM 200221-ONSHI-SHINRYO-DEL-SEC
      *
      *    診療情報メイン初期設定
           PERFORM 20022-ONSHI-SHINMAIN-INIT-SEC
      *
           .
       20022-ONSHI-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤情報資格確認　削除処理
     *****************************************************************
       200221-ONSHI-SHINRYO-DEL-SEC     SECTION.
      *
      *    薬剤情報メイン削除
           INITIALIZE                  ONSHI-SHINRYO-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SNR-M-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-SNR-M-PTID
      *    診療年月
           MOVE    QUAREQ8-SRYYM       TO  ONS-SNR-M-SRYYM
           MOVE    QUAREQ8-NYUGAIKBN   TO  ONS-SNR-M-NYUGAIKBN
           MOVE    QUAREQ8-PR-METRDIHCD    TO  ONS-SNR-M-HOSPCD
      *
           MOVE    ONSHI-SHINRYO-MAIN-REC  TO  MCPDATA-REC
      *    検索
           MOVE    "tbl_onshi_shinryo_main"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"             TO  MCP-PATHNAME
               PERFORM 900-SHINRYO-MAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SHINRYO-MAIN
           END-IF
           PERFORM UNTIL       FLG-SHINRYO-MAIN    =   1
      *        診療情報サブ削除
               INITIALIZE                  ONSHI-SHINRYO-SUB-REC
               MOVE    ONS-SNR-M-HOSPNUM   TO  ONS-SNR-S-HOSPNUM
               MOVE    ONS-SNR-M-TBL-UUID  TO  ONS-SNR-S-TBL-UUID
               MOVE    ONS-SNR-M-PTID      TO  ONS-SNR-S-PTID
      *        診療年月
               MOVE    ONS-SNR-M-SRYYM     TO  ONS-SNR-S-SRYYM
      *
               MOVE    ONSHI-SHINRYO-SUB-REC   TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_onshi_shinryo_sub"
                                           TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        歯科情報削除
      *        診療情報歯科加算情報サブ削除
               INITIALIZE                  ONSHI-SHIKA-SUB-REC
               MOVE    SPA-HOSPNUM         TO  ONS-SIK-S-HOSPNUM
               MOVE    ONS-SNR-M-TBL-UUID  TO  ONS-SIK-S-TBL-UUID
               MOVE    ONS-SNR-M-PTID      TO  ONS-SIK-S-PTID
      *        診療年月
               MOVE    ONS-SNR-M-SRYYM     TO  ONS-SIK-S-SRYYM
      *
               MOVE    ONSHI-SHIKA-SUB-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_onshi_shika_sub"
                                           TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *       メイン削除
               MOVE    ONSHI-SHINRYO-MAIN-REC  TO  MCPDATA-REC
      *
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_onshi_shinryo_main"
                                           TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               MOVE    "tbl_onshi_shinryo_main"    TO  MCP-TABLE
               MOVE    "key6"             TO  MCP-PATHNAME
               PERFORM 900-SHINRYO-MAIN-READ-SEC
           END-PERFORM
           MOVE    "tbl_onshi_shinryo_main"    TO  MCP-TABLE
           MOVE    "key6"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200221-ONSHI-SHINRYO-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報　メイン編集処理
      *****************************************************************
       20022-ONSHI-SHINMAIN-INIT-SEC     SECTION.
      *
      *    UIDコード取得処理
           PERFORM 1001-UID-GET-SEC
           MOVE    C-UID               TO  WRK-UID
      *
           INITIALIZE                  ONSHI-SHINRYO-MAIN-REC
      *    メイン編集
           MOVE    SPA-HOSPNUM         TO  ONS-SNR-M-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-SNR-M-PTID
      *
      *    UID
           MOVE    WRK-UID             TO  ONS-SNR-M-TBL-UUID
      *
      *    診療年月
           MOVE    QUAREQ8-SRYYM       TO  ONS-SNR-M-SRYYM
      *    入外区分（４：調剤）
           MOVE    QUAREQ8-NYUGAIKBN
                                       TO  ONS-SNR-M-NYUGAIKBN
      *
      *    調剤機関毎連番
           MOVE    QUAREQ8-GRPNUM
                                       TO  ONS-SNR-M-CHOZAI-SEQNUM
      *    調剤機関区分
           MOVE    QUAREQ8-DIORG       TO  ONS-SNR-M-CHOZAI-KBN
      *    処方機関毎連番
           MOVE    QUAREQ8-GRPNUM2    TO  ONS-SNR-M-SHOHO-SEQNUM
      *    処方箋発行機関区分
           MOVE    QUAREQ8-PR-ISORG
                                       TO  ONS-SNR-M-SHOHO-KBN
      *    資格情報
           MOVE    QUAREQ8-REQ-HKNJANUM
                                       TO  ONS-SNR-M-HKNJANUM
      *    被保険者証記号
           MOVE    WRK-KIGO            TO  ONS-SNR-M-KIGO
      *    被保険者証番号
           MOVE    WRK-NUM             TO  ONS-SNR-M-NUM
      *    番号枝番
           MOVE    QUAREQ8-REQ-EDABAN  TO  ONS-SNR-M-EDABAN
      *
      *    医療機関・薬局コード
           MOVE    QUAREQ8-PR-METRDIHCD
                                       TO  ONS-SNR-M-HOSPCD
      *    医療機関・薬局名
           MOVE    QUAREQ8-PR-METRDIHNM
                                       TO  ONS-SNR-M-HOSPNAME
      *    処方箋発行医療機関コード
           MOVE    QUAREQ8-PR-PRLSHCD
                                       TO  ONS-SNR-M-CHOZAICD
      *    処方箋発行医療機関名
           MOVE    QUAREQ8-PR-PRLSHNM
                                       TO  ONS-SNR-M-CHOZAINAME
      ***
           .
       20022-ONSHI-SHINMAIN-INIT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     診療情報　サブ作成処理
      *****************************************************************
       20023-ONSHI-SHINSUB-SEC     SECTION.
      *
           MOVE    ZERO                TO  IDD
      *    診療日付
           MOVE    QUAREQ8-DI-DATE      TO  WRK-DATE
           IF      WRK-DD               >=  "01"   AND
                                        <=  "31"
               MOVE    WRK-DD              TO  IDD
      *        サブ登録
               PERFORM 200221-ONSHI-SHINRYO-SUB-SEC
           END-IF
      *
      *    メインの診療日設定
           IF      WRK-DD               >=  "01"   AND
                                        <=  "31"
               PERFORM VARYING     IDD     FROM    1   BY  1
                       UNTIL      (IDD         >   31    )
                   IF     ONS-SNR-M-SRYDD (IDD)    =   SPACE
                       MOVE    WRK-DD      TO  ONS-SNR-M-SRYDD
                                                          (IDD)
                   END-IF
                   IF     ONS-SNR-M-SRYDD (IDD)    =   WRK-DD
                       MOVE    31          TO  IDD
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       20023-ONSHI-SHINSUB-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認診療情報メイン登録処理
      *****************************************************************
       20022-ONSHI-SHINMAIN-INS-SEC     SECTION.
      *
           MOVE    SPA-OPID            TO  ONS-SNR-M-OPID
      *
           MOVE    SMCNDATE-YMD        TO  ONS-SNR-M-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONS-SNR-M-CREHMS
           MOVE    SMCNDATE-YMD        TO  ONS-SNR-M-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-SNR-M-UPHMS
      *
           MOVE    ONSHI-SHINRYO-MAIN-REC  TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_shinryo_main"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "006 ONSHI-SHINRYO-MAIN INSERT ERR:"  MCP-RC
                       ",KEY:" ONS-SNR-M-KEY
           END-IF
           .
       20022-ONSHI-SHINMAIN-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報資格確認サブ登録処理 
      *****************************************************************
       200221-ONSHI-SHINRYO-SUB-SEC    SECTION.
      *
           INITIALIZE                  ONSHI-SHINRYO-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SNR-S-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-SNR-S-PTID
      *    UID
           MOVE    WRK-UID             TO  ONS-SNR-S-TBL-UUID
      *    診療年月
           MOVE    QUAREQ8-SRYYM       TO  ONS-SNR-S-SRYYM
      *    入外区分
           MOVE    QUAREQ8-NYUGAIKBN
                                       TO  ONS-SNR-S-NYUGAIKBN
      *    診療日
           MOVE    WRK-DD              TO  ONS-SNR-S-SRYDD
      *    処方箋発行日
           MOVE    QUAREQ8-PR-DATE
                                       TO  ONS-SNR-S-HAKKOYMD
      *
           MOVE    ZERO                TO  WRK-RENNUM
      *
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ         >   100   )
                         OR   (WRK-ERRCD   NOT =   SPACE )
               IF      QUAREQ8-SRYKBN (IDZ)   NOT =   SPACE
                   ADD     1               TO  WRK-RENNUM
                   PERFORM 200221-ONSHI-SHINSUB-HEN-SEC
      *            歯科情報
                   IF      QUAREQ8-DEN-ADDCD (IDZ 1)
                                           NOT =   SPACE
                       PERFORM 200222-ONSHI-SHIKA-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200221-ONSHI-SINRYO-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報サブ登録処理 
      *****************************************************************
       200221-ONSHI-SHINSUB-HEN-SEC  SECTION.
      *
           INITIALIZE                  ONS-SNR-S-DATA
      *
           MOVE    WRK-RENNUM          TO  ONS-SNR-S-RENNUM
      *    診療識別等区分
           MOVE    QUAREQ8-SRYKBN(IDZ)
                                       TO  ONS-SNR-S-SRYKBN
      *    診療コード
           MOVE    QUAREQ8-SRYCD (IDZ)
                                       TO  ONS-SNR-S-SRYCD
      *    診療名
           MOVE    QUAREQ8-SRYNAME (IDZ)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-SNR-S-SRYNAME
      *    単位
           MOVE    QUAREQ8-UNIT   (IDZ)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-SNR-S-TANINAME
      *    使用量
           IF      QUAREQ8-QUA1 (IDZ)  =   SPACE
               MOVE    ZERO                TO  ONS-SNR-S-SURYO
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAREQ8-QUA1 (IDZ)  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      ( SNUM-RC           =   ZERO  )
                    MOVE    SNUM-OUTNUM    TO  ONS-SNR-S-SURYO
               END-IF
           END-IF
      *    数量２
           IF      QUAREQ8-QUA2 (IDZ)  =   SPACE
               MOVE    ZERO                TO  ONS-SNR-S-SURYO2
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAREQ8-QUA2 (IDZ)  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      ( SNUM-RC           =   ZERO  )
                    MOVE    SNUM-OUTNUM    TO  ONS-SNR-S-SURYO2
               END-IF
           END-IF
      *    回数
           IF      QUAREQ8-TIMES (IDZ) =   SPACE
               MOVE    ZERO                TO  ONS-SNR-S-KAISU
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    QUAREQ8-TIMES (IDZ) TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF      ( SNUM-RC           =   ZERO  )
                    MOVE    SNUM-OUTNUM    TO  ONS-SNR-S-KAISU
               END-IF
           END-IF
      *    歯科情報
           IF      QUAREQ8-DEN-ADDCD (IDZ 1)
                                       NOT =   SPACE
               MOVE    "1"                 TO  ONS-SNR-SHIKA-KBN
           END-IF
      *
      *    資格確認診療情報サブ登録処理
           MOVE    SPA-OPID            TO  ONS-SNR-S-OPID
      *
           MOVE    SMCNDATE-YMD        TO  ONS-SNR-S-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONS-SNR-S-CREHMS
           MOVE    SMCNDATE-YMD        TO  ONS-SNR-S-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-SNR-S-UPHMS
      *
           MOVE    ONSHI-SHINRYO-SUB-REC   TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_shinryo_sub"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "006 ONSHI-SHINRYO-SUB INSERT ERR:"  MCP-RC
                       ",KEY:" ONS-SNR-S-KEY
           END-IF
           .
       200221-ONSHI-SHINSUB-HEN-EXT.
           EXIT.
      *****************************************************************
      *    歯科診療行為情報処理
      *****************************************************************
       200222-ONSHI-SHIKA-SEC                SECTION.
      *
           PERFORM VARYING     IDS     FROM    1   BY  1
                   UNTIL      (IDS         >   30    )
                         OR   (WRK-ERRCD   NOT =   SPACE )
               IF      QUAREQ8-DEN-ADDCD (IDZ IDS)
                                               =   SPACE
                   MOVE    30                  TO  IDS
               ELSE
                   PERFORM 200222-ONSHI-SIKAINS-SEC
               END-IF
           END-PERFORM
           .
       200222-ONSHI-SHIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    歯科診療行為情報作成処理
      *****************************************************************
       200222-ONSHI-SIKAINS-SEC                SECTION.
      *
           INITIALIZE                  ONSHI-SHIKA-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SIK-S-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-SIK-S-PTID
      *    UID
           MOVE    WRK-UID             TO  ONS-SIK-S-TBL-UUID
      *    診療年月
           MOVE    QUAREQ8-SRYYM       TO  ONS-SIK-S-SRYYM
      *    入外区分（４：調剤）
           MOVE    QUAREQ8-NYUGAIKBN
                                       TO  ONS-SIK-S-NYUGAIKBN
      *    診療日
           MOVE    WRK-DD              TO  ONS-SIK-S-SRYDD
      *    処方箋発行日
           MOVE    QUAREQ8-PR-DATE
                                       TO  ONS-SIK-S-HAKKOYMD
      *    診療情報サブ連番
           MOVE    WRK-RENNUM          TO  ONS-SIK-S-RENNUM
      *    医科情報連番
           MOVE    IDS                 TO  ONS-SIK-S-RENNUM1
      *
      *    診療行為加算コード
           MOVE    QUAREQ8-DEN-ADDCD (IDZ IDS)
                                       TO  ONS-SIK-S-SRYKASANCD
      *    診療行為加算名
           MOVE    QUAREQ8-DEN-ADDNAME (IDZ IDS)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-SIK-S-SRYKASAN-NAME
      *    使用量
           INITIALIZE                  ORCSNUMAREA
           MOVE    QUAREQ8-DEN-QUA (IDZ IDS)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      ( SNUM-RC           =   ZERO  )
                MOVE    SNUM-OUTNUM    TO  ONS-SIK-S-SURYO
           END-IF
      *    単位
           MOVE    QUAREQ8-DEN-UNIT (IDZ IDS)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-SIK-S-TANINAME
      *
           MOVE    SPA-OPID            TO  ONS-SIK-S-OPID
      *
           MOVE    SMCNDATE-YMD        TO  ONS-SIK-S-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONS-SIK-S-CREHMS
           MOVE    SMCNDATE-YMD        TO  ONS-SIK-S-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-SIK-S-UPHMS
      *
           MOVE    ONSHI-SHIKA-SUB-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_shika_sub"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "006 ONSHI-SHIKA-SUB INSERT ERR:"  MCP-RC
                       ",KEY:" ONS-SIK-S-KEY
           END-IF
           .
       200222-ONSHI-SIKAINS-EXT.
           EXIT.
      *
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
      *     全角変換処理
      *****************************************************************
       800-KANACHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-KANACHK-OUT-INPUT
           IF      WRK-KANACHK-MAE-INPUT
                               NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   2    )
                   CONTINUE
               ELSE
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       800-KANACHK-EXT.
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
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認読込
      ******************************************************************
       900-SHINRYO-MAIN-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_shinryo_main"    TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-SHINRYO-MAIN-REC
               MOVE    ZERO                TO  FLG-SHINRYO-MAIN
           ELSE
               INITIALIZE                      ONSHI-SHINRYO-MAIN-REC
               MOVE    1                   TO  FLG-SHINRYO-MAIN
           END-IF
      *
           .
       900-SHINRYO-MAIN-READ-EXT.
           EXIT.
      ******************************************************************
      *    資格確認読込
      ******************************************************************
       900-ONSKAKU-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSKAKU-REC
               MOVE    ZERO                TO  FLG-ONSKAKU
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSKAKU
           END-IF
      *
           .
       900-ONSKAKU-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者読込
      ******************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *  
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
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
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
       990-DBCLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       990-DBCLOSE-EXT.
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

