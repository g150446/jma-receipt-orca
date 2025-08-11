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
       PROGRAM-ID.           ORAPION005R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 資格確認特定検診登録、更新処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/02/01    NMED−多々納  新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.02.00    ORCAMO       25/05/XX  単位外字対応
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
           03  FLG-ONS-KNS-MAIN        PIC 9(01).
           03  FLG-ONS-KNS-SUB         PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-ONSKAKU             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDD                     PIC 9(02).
           03  IDX-D                   PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HOSPCD              PIC X(10).
           03  WRK-OPID                PIC X(16).
           03  WRK-PTID-AREA.
               05  WRK-PTID            PIC 9(10).
               05  WRK-PATIENT-ID      PIC X(20).
      *
           03  WRK-TBL-UUID            PIC X(36).
      *
           03  WRK-KIGO                PIC X(80).
           03  WRK-NUM                 PIC X(80).
           03  WRK-RENNUM              PIC 9(03).
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
           COPY    "CPONLINEQUAREQ5.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUARES1.INC".
      *    API XML読み込み共通定義
      ***  COPY    "CPAPIV2REQ.INC".
      ***  COPY    "CPAPI21V3REQ.INC".
        01  XML-APIREQ.
            03  APIREQ-CONTEXT          PIC S9(9)   BINARY.
            03  APIREQ-OBJECT           PIC X(36).
            03  APIREQ-MODE             PIC S9(9)   BINARY.
            03  APIREQ-RECORDNAME       PIC X(128).
            03  APIREQ-PATIENTINFOREQ   PIC X(4000000).
      *
      *    COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    ＵＩＤ取得用エリア
      *01  UIDIO-AREA.
      *    03  C-RET               PIC X(2).
      *    03  C-UID               PIC X(36).
      *01  ST                      PIC 9(2).
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
      *    資格確認特定検診情報メイン
       01  ONSHI-KENSIN-MAIN-REC.
           COPY    "CPONSHI-KENSIN-MAIN.INC".
      *
      *    資格確認特定検診情報サブ
       01  ONSHI-KENSIN-SUB-REC.
           COPY    "CPONSHI-KENSIN-SUB.INC".
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
           DISPLAY   "*  onlinequa5 start  *"
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
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST05-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "*   onlinequa5 end   *"
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
      *
      *    資格確認特定検診情報処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-SIKAKU-KENSIN-SYORI-SEC
           END-IF
      *    資格確認処理更新処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2003-ONSHI-KAKU-SYORI-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST05-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAREQ5
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinespecreq1"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST05-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinespecreq1"    TO  APIREQ-RECORDNAME
      **   CALL    "MONFUNC"           USING
      *                                MCPAREA
      **                               XML-APIREQ
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAREQ5
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
               MOVE    "000"               TO  QUARES1-API-RESULT
               MOVE    "処理終了"          TO  QUARES1-API-RESULT-MSG
           ELSE
               DISPLAY "300 WRK-ERRCD=" WRK-ERRCD
               MOVE    WRK-ERRCD           TO  QUARES1-API-RESULT
               MOVE    WRK-ERRMSG          TO  QUARES1-API-RESULT-MSG
      *****    GO  TO  300-RGTXMLMAKE-EXT
            END-IF
      *
           IF      APILST05-BODY       NOT =   LOW-VALUE
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
               MOVE  QUARES1-OBJECT    TO  APILST05-BODY
               MOVE  "application/xml" TO  APILST05-CONTENT-TYPE
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
      *    処理結果区分(特定健診)
           IF      QUAREQ5-RESULTCODE  =   "1"
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
       2002-SIKAKU-KENSIN-SYORI-SEC      SECTION.
      *
      *    TBL_UUID
           MOVE    QUAREQ5-FILE-SIKIBETU(12:36) TO  WRK-TBL-UUID
      *    患者ID判定(要求任意のファイル識別子)
           IF      QUAREQ5-FILE-SIKIBETU(1:10)  NUMERIC
               MOVE    QUAREQ5-FILE-SIKIBETU(1:10) TO  WRK-PTID
           ELSE
               MOVE    ZERO                TO  WRK-PTID
               MOVE    "E10"               TO  WRK-ERRCD
               DISPLAY "0004 PTID ERR:"  WRK-PTID
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
               DISPLAY "0004 PTID ERR:"  WRK-PTID
           END-IF
      *
           MOVE    WRK-PATIENT-ID      TO  QUARES1-PATIENT-ID
      *
      *    被保険者証記号　全角変換
           MOVE    QUAREQ5-RES-KIGO    TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-KIGO
      *    被保険者証番号　全角変換
           MOVE    QUAREQ5-RES-NUM     TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-NUM
      *
      *    実施年月日毎に処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   50   )
                         OR   (WRK-ERRCD   NOT =   SPACE )
               IF     (QUAREQ5-JISSHIYMD (IDX) NOT =   SPACE)
      *            特定検診処理
                   PERFORM 20021-ONSHI-KENSIN-SYORI-SEC
               END-IF
           END-PERFORM
           .
       2002-SIKAKU-KENSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     特定検診処理
      *****************************************************************
       20021-ONSHI-KENSIN-SYORI-SEC     SECTION.
      *
      *    実施年月日の登録チェック
           INITIALIZE                  ONSHI-KENSIN-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-KNS-M-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-KNS-M-PTID
      *    実施年月日
           MOVE    QUAREQ5-JISSHIYMD(IDX)  TO  ONS-KNS-M-JISSHIYMD
      *
           MOVE    ONSHI-KENSIN-MAIN-REC   TO  MCPDATA-REC
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONS-KNS-MAIN
           END-IF
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ONS-KNS-MAIN    =   ZERO
      *        登録済みは保険情報更新
               PERFORM 200211-KENSIN-MAIN-UPD-SEC
           ELSE
      *        登録処理
               PERFORM 200212-KENSIN-TOROKU-SEC
           END-IF
           .
       20021-ONSHI-KENSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認特定検診メイン更新処理
      *****************************************************************
       200211-KENSIN-MAIN-UPD-SEC     SECTION.
      *
      *  保険情報の変更
      *    被保険者番号
           MOVE    QUAREQ5-RES-HKNJANUM
                                       TO  ONS-KNS-M-HKNJANUM
      *    被保険者証記号
           MOVE    WRK-KIGO            TO  ONS-KNS-M-KIGO
      *    被保険者証番号
           MOVE    WRK-NUM             TO  ONS-KNS-M-NUM
      *    番号枝番
           MOVE    QUAREQ5-RES-EDABAN  TO  ONS-KNS-M-EDABAN
      *
           MOVE    SPA-OPID            TO  ONS-KNS-M-OPID
           MOVE    SMCNDATE-YMD        TO  ONS-KNS-M-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-KNS-M-UPHMS
      *
           MOVE    ONSHI-KENSIN-MAIN-REC   TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "005 ONSHI-KENSIN-MAIN UPDATE ERR:"  MCP-RC
                       ",KEY:" ONS-KNS-M-KEY
           END-IF
           .
       200211-KENSIN-MAIN-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認特定検診登録処理
      *****************************************************************
       200212-KENSIN-TOROKU-SEC     SECTION.
      *
      *    メイン編集
           PERFORM 2002121-KENSIN-MAIN-HEN-SEC
      *
           MOVE    ZERO             TO  WRK-RENNUM
      *    実施年月日毎に処理
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY         >   200  )
                         OR   (WRK-ERRCD   NOT =   SPACE )
               IF     (QUAREQ5-ITEMCODE (IDX IDY)  NOT =   SPACE)
      *            特定検診サブ登録
                   PERFORM 2002122-KENSIN-SUB-SYORI-SEC
               END-IF
           END-PERFORM
      *
           IF      WRK-RENNUM          >   ZERO
      *        サブが１件でもあればメイン登録
               PERFORM 2002121-KENSIN-MAIN-INS-SEC
           END-IF
      *
           .
       200212-KENSIN-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *     特定検診資格確認　更新処理
      *****************************************************************
       2002121-KENSIN-MAIN-HEN-SEC     SECTION.
      *
           INITIALIZE                  ONSHI-KENSIN-MAIN-REC
      *    メイン編集
           MOVE    SPA-HOSPNUM         TO  ONS-KNS-M-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-KNS-M-PTID
      *    実施年月日
           MOVE    QUAREQ5-JISSHIYMD(IDX)  TO  ONS-KNS-M-JISSHIYMD
      *    被保険者番号
           MOVE    QUAREQ5-RES-HKNJANUM
                                       TO  ONS-KNS-M-HKNJANUM
      *    被保険者証記号
           MOVE    WRK-KIGO            TO  ONS-KNS-M-KIGO
      *    被保険者証番号
           MOVE    WRK-NUM             TO  ONS-KNS-M-NUM
      *    番号枝番
           MOVE    QUAREQ5-RES-EDABAN  TO  ONS-KNS-M-EDABAN
           .
       2002121-KENSIN-MAIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     特定検診資格確認サブ　登録理
      *****************************************************************
       2002122-KENSIN-SUB-SYORI-SEC     SECTION.
      *
           IF      WRK-RENNUM          =   999
               GO  TO  2002122-KENSIN-SUB-SYORI-EXT
           END-IF
           ADD     1                   TO  WRK-RENNUM
      *
           INITIALIZE                  ONSHI-KENSIN-SUB-REC
      *    メイン編集
           MOVE    SPA-HOSPNUM         TO  ONS-KNS-S-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-KNS-S-PTID
      *    実施年月日
           MOVE    QUAREQ5-JISSHIYMD(IDX)  TO  ONS-KNS-S-JISSHIYMD
           MOVE    WRK-RENNUM          TO  ONS-KNS-S-RENNUM
      *
      *    項目コード
           MOVE    QUAREQ5-ITEMCODE(IDX IDY)
                                       TO  ONS-KNS-S-ITEMCODE
      *    項目名
           MOVE    QUAREQ5-ITEMNAME (IDX IDY)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 801-KANACHK02-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-KNS-S-ITEMNAME
      *    データ型
           MOVE    QUAREQ5-DATA-TYPE(IDX IDY)
                                       TO  ONS-KNS-S-DATA-TYPE
      *    データ値
           MOVE    QUAREQ5-DATA-VALUE (IDX IDY)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 801-KANACHK02-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-KNS-S-DATA-VALUE
      *    単位
      *2025/05
           MOVE    QUAREQ5-DATA-TANNI(IDX IDY)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 801-KANACHK02-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-KNS-S-DATA-TANNI
      *
      *    資格確認特定検診情報サブ登録処理
           MOVE    SPA-OPID            TO  ONS-KNS-S-OPID
      *
           MOVE    SMCNDATE-YMD        TO  ONS-KNS-S-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONS-KNS-S-CREHMS
           MOVE    SMCNDATE-YMD        TO  ONS-KNS-S-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-KNS-S-UPHMS
      *
           MOVE    ONSHI-KENSIN-SUB-REC    TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_kenshin_sub"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "005 ONSHI-KENSIN-SUB INSERT ERR:"  MCP-RC
                       ",KEY:" ONS-KNS-S-KEY
           END-IF
      *
           .
       2002122-KENSIN-SUB-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認薬剤情報メイン登録処理
      *****************************************************************
       2002121-KENSIN-MAIN-INS-SEC     SECTION.
      *
           MOVE    SPA-OPID            TO  ONS-KNS-M-OPID
      *
           MOVE    SMCNDATE-YMD        TO  ONS-KNS-M-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONS-KNS-M-CREHMS
           MOVE    SMCNDATE-YMD        TO  ONS-KNS-M-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-KNS-M-UPHMS
      *
           MOVE    ONSHI-KENSIN-MAIN-REC   TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "005 ONSHI-KENSIN-MAIN INSERT ERR:"  MCP-RC
                       ",KEY:" ONS-KNS-M-KEY
           END-IF
           .
       2002121-KENSIN-MAIN-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    オン資格確認処理更新処理
      *****************************************************************
       2003-ONSHI-KAKU-SYORI-SEC                SECTION.
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    WRK-TBL-UUID        TO  ONSKAKU-TBL-UUID
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ONSKAKU         =   ZERO
      *        特定検診情報取得依頼済フラグ
               MOVE    "02"                TO  ONSKAKU-KENSIN-ENDFLG
      *
               MOVE    SPA-OPID            TO  ONSKAKU-OPID
               MOVE    SMCNDATE-YMD        TO  ONSKAKU-UPYMD
               MOVE    SMCNDATE-HMS        TO  ONSKAKU-UPHMS
      *
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "E60"               TO  WRK-ERRCD
                   DISPLAY "005  ONSKAKU KEY UPD ERR:"  MCP-RC
                       ",KEY:" ONSKAKU-KEY
               END-IF
           END-IF
           .
       2003-ONSHI-KAKU-SYORI-EXT.
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
      *     全角チェック変換処理
      *****************************************************************
       801-KANACHK02-SEC             SECTION.
      *
           MOVE    WRK-KANACHK-MAE-INPUT   TO  WRK-KANACHK-OUT-INPUT
      *2025/05
           IF      WRK-KANACHK-MAE-INPUT   =   SPACE
               GO  TO  801-KANACHK02-EXT
           END-IF
      *
      *    全角チェック
      *2025/05
           IF      SPA-ENCODING        =   2
      *        拡張漢字対応
               PERFORM 8011-ZENKANACHK02-SEC
           ELSE
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT   TO  KANACHK-MAE-INPUT
      **** MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-KANACHK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    KANACHK-MAE-INPUT   TO  WRK-KANACHK-OUT-INPUT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-KANACHK-OUT-INPUT
           END-IF
      *
           END-IF
           .
       801-KANACHK02-EXT.
           EXIT.
      *2025/02
      *****************************************************************
      *    全角チェック変換処理（拡張文字対応）
      *****************************************************************
       8011-ZENKANACHK02-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT   TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           MOVE    KANACHK2-MAE-INPUT  TO  WRK-KANACHK-MAE-INPUT
           IF     (KANACHK2-RC     NOT =   ZERO  )  OR
                  (KANACHK2-SYUBETU    =   1     )
               MOVE    KANACHK2-MAE-INPUT  TO  WRK-KANACHK-OUT-INPUT
           ELSE
               MOVE    KANACHK2-OUT-INPUT  TO  WRK-KANACHK-OUT-INPUT
           END-IF
           .
       8011-ZENKANACHK02-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
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
      *
      ******************************************************************
      *    資格確認読込
      ******************************************************************
       910-ONSHIMAIN-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-KENSIN-MAIN-REC
               MOVE    ZERO                TO  FLG-ONS-KNS-MAIN
           ELSE
               INITIALIZE                      ONSHI-KENSIN-MAIN-REC
               MOVE    1                   TO  FLG-ONS-KNS-MAIN
           END-IF
      *
           .
       910-ONSHIMAIN-READ-EXT.
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

