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
       PROGRAM-ID.           ORAPION004R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 資格確認薬剤情報登録、更新処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/01/26    NACL−多々納  新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.00    ORCAMO       22/08/XX  薬剤テーブルレイアウト変更対応
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
           03  FLG-ONS-YKZ-MAIN        PIC 9(01).
           03  FLG-ONS-YKZ-SUB         PIC 9(01).
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
           03  WRK-KEY-SRYYM           PIC X(06).
      *2022/08
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
           COPY    "CPONLINEQUAREQ4.INC".
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
            03  APIREQ-PATIENTINFOREQ   PIC X(8000000).
      *
      *    COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *2022/08
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
      *    資格確認薬剤情報メイン
       01  ONSHI-YAKUZAI-MAIN-REC.
           COPY    "CPONSHI-YAKUZAI-MAIN.INC".
      *
      *    資格確認薬剤情報サブ
       01  ONSHI-YAKUZAI-SUB-REC.
           COPY    "CPONSHI-YAKUZAI-SUB.INC".
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
           DISPLAY   "*  onlinequa4 start  *"
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
               MOVE   404                  TO  APILST04-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "*   onlinequa4 end   *"
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
      *    資格確認薬剤情報処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-SIKAKU-YAKUZAI-SYORI-SEC
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
           IF      APILST04-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAREQ4
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinedrugreq1"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST04-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinedrugreq1"    TO  APIREQ-RECORDNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      **** CALL    "ORCDBMAIN"         USING
      *                                MCPAREA
      *                                XML-APIREQ
      ****                             SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAREQ4
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
      *2022/08
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
           IF      QUAREQ4-RESULTCODE  =   "1"
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
       2002-SIKAKU-YAKUZAI-SYORI-SEC      SECTION.
      *
      *    TBL_UUID
           MOVE    QUAREQ4-FILE-SIKIBETU(12:36) TO  WRK-TBL-UUID
      *    患者ID判定(要求任意のファイル識別子)
           IF      QUAREQ4-FILE-SIKIBETU(1:10)  NUMERIC
               MOVE    QUAREQ4-FILE-SIKIBETU(1:10) TO  WRK-PTID
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
           MOVE    QUAREQ4-RES-KIGO    TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-KIGO
      *    被保険者証番号　全角変換
           MOVE    QUAREQ4-RES-NUM     TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-NUM
      *
      *    診療年月毎に処理
           MOVE    SPACE               TO  WRK-KEY-SRYYM
      *    削除処理
           PERFORM VARYING     IDX     FROM    1   BY  1
      *2022/02/24  UNTIL      (IDX         >   3 )
                   UNTIL      (IDX         >   6 )
                         OR   ( WRK-ERRCD  NOT =   SPACE )
               IF     (QUAREQ4-SRYYM (IDX) NOT =   SPACE )
      *            削除処理
                   IF      WRK-KEY-SRYYM   NOT =   QUAREQ4-SRYYM (IDX)
                       PERFORM 20021-ONSHI-YAKUZAI-DEL-SEC
                   END-IF
                   MOVE    QUAREQ4-SRYYM (IDX) TO  WRK-KEY-SRYYM
               END-IF
           END-PERFORM
      *
      *    診療年月毎に処理
           PERFORM VARYING     IDX     FROM    1   BY  1
      *2022/02/24  UNTIL      (IDX         >   3 )
                   UNTIL      (IDX         >   6 )
                         OR   ( WRK-ERRCD  NOT =   SPACE )
               IF     (QUAREQ4-SRYYM (IDX) NOT =   SPACE)
      *            追加処理
                   PERFORM 20022-ONSHI-YAKUZAI-ADD-SEC
               END-IF
           END-PERFORM
           .
       2002-SIKAKU-YAKUZAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤情報資格確認　削除処理
     *****************************************************************
       20021-ONSHI-YAKUZAI-DEL-SEC     SECTION.
      *
      *    薬剤情報メイン削除
           INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-YKZ-M-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-YKZ-M-PTID
      *    診療年月
           MOVE    QUAREQ4-SRYYM(IDX)  TO  ONS-YKZ-M-SRYYM
      *
           MOVE    ONSHI-YAKUZAI-MAIN-REC  TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    薬剤情報サブ削除
           INITIALIZE                  ONSHI-YAKUZAI-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-YKZ-S-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-YKZ-S-PTID
      *    診療年月
           MOVE    QUAREQ4-SRYYM(IDX)  TO  ONS-YKZ-S-SRYYM
      *
           MOVE    ONSHI-YAKUZAI-SUB-REC   TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_onshi_yakuzai_sub"
                                       TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       20021-ONSHI-YAKUZAI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤情報資格確認　更新処理
      *****************************************************************
       20022-ONSHI-YAKUZAI-ADD-SEC     SECTION.
      *
      *    UIDコード取得処理
           PERFORM 1001-UID-GET-SEC
           MOVE    C-UID               TO  WRK-UID
      *
           INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
      *    メイン編集
           MOVE    SPA-HOSPNUM         TO  ONS-YKZ-M-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-YKZ-M-PTID
      *2022/08
      *    UID
           MOVE    WRK-UID             TO  ONS-YKZ-M-TBL-UUID
      *
      *    診療年月
           MOVE    QUAREQ4-SRYYM(IDX)  TO  ONS-YKZ-M-SRYYM
      *    入外区分（４：調剤）
           MOVE    QUAREQ4-NYUGAIKBN(IDX)
                                       TO  ONS-YKZ-M-NYUGAIKBN
      *
      *    調剤機関毎連番
           MOVE    QUAREQ4-GRPNUM  (IDX)
                                       TO  ONS-YKZ-M-CHOZAI-SEQNUM
      *    調剤機関区分
           MOVE    QUAREQ4-DIORG  (IDX)
                                       TO  ONS-YKZ-M-CHOZAI-KBN
      *    処方機関毎連番
           MOVE    QUAREQ4-GRPNUM2  (IDX)
                                       TO  ONS-YKZ-M-SHOHO-SEQNUM
      *    処方箋発行機関区分
           MOVE    QUAREQ4-PR-ISORG (IDX)
                                       TO  ONS-YKZ-M-SHOHO-KBN
      *    資格情報
           MOVE    QUAREQ4-RES-HKNJANUM
                                       TO  ONS-YKZ-M-HKNJANUM
      *    被保険者証記号
           MOVE    WRK-KIGO            TO  ONS-YKZ-M-KIGO
      *    被保険者証番号
           MOVE    WRK-NUM             TO  ONS-YKZ-M-NUM
      *    番号枝番
           MOVE    QUAREQ4-RES-EDABAN  TO  ONS-YKZ-M-EDABAN
      *2022/08
      *    医療機関・薬局コード
           MOVE    QUAREQ4-PR-METRDIHCD (IDX)
                                       TO  ONS-YKZ-M-HOSPCD
      *    医療機関・薬局名
           MOVE    QUAREQ4-PR-METRDIHNM  (IDX)
                                       TO  ONS-YKZ-M-HOSPNAME
      *    処方箋発行医療機関コード
           MOVE    QUAREQ4-PR-PRLSHCD (IDX)
                                       TO  ONS-YKZ-M-CHOZAICD
      *    処方箋発行医療機関名
           MOVE    QUAREQ4-PR-PRLSHNM (IDX)
                                       TO  ONS-YKZ-M-CHOZAINAME
      ***
      *
           INITIALIZE                  WRK-DAY-AREA
           MOVE    ZERO                TO  IDD
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY         >   31    )
                         OR   (WRK-ERRCD   NOT =   SPACE )
               IF      QUAREQ4-DI-DATE (IDX IDY)   NOT =   SPACE
      *            調剤日付をキーとする
                   MOVE    QUAREQ4-DI-DATE(IDX IDY)    TO  WRK-DATE
                   IF      WRK-DD              >=  "01"   AND
                                               <=  "31"
                       MOVE    WRK-DD              TO  IDD
                       MOVE    "1"                 TO  WRK-DAY (IDD)
      *                サブ登録
                       PERFORM 200221-ONSHI-YAKUZAI-SUB-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    算定のある日に「１」を設定する
           MOVE    ZERO                TO  IDX-D
           PERFORM VARYING     IDD     FROM    1   BY  1
                   UNTIL      (IDD         >   31    )
               IF      WRK-DAY (IDD)       =   "1"
                   ADD     1               TO  IDX-D
                   MOVE    IDD             TO  ONS-YKZ-M-SRYDD
                                                      (IDX-D)
               END-IF
           END-PERFORM
      *
           PERFORM 20022-ONSHI-YAKUMAIN-INS-SEC
           .
       20022-ONSHI-YAKUZAI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *     資格確認薬剤情報メイン登録処理
      *****************************************************************
       20022-ONSHI-YAKUMAIN-INS-SEC     SECTION.
      *
           MOVE    SPA-OPID            TO  ONS-YKZ-M-OPID
      *
           MOVE    SMCNDATE-YMD        TO  ONS-YKZ-M-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONS-YKZ-M-CREHMS
           MOVE    SMCNDATE-YMD        TO  ONS-YKZ-M-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-YKZ-M-UPHMS
      *
           MOVE    ONSHI-YAKUZAI-MAIN-REC  TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "004 ONSHI-YAKUZAI-MAIN INSERT ERR:"  MCP-RC
                       ",KEY:" ONS-YKZ-M-KEY
           END-IF
           .
       20022-ONSHI-YAKUMAIN-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報資格確認サブ登録処理 
      *****************************************************************
       200221-ONSHI-YAKUZAI-SUB-SEC  SECTION.
      *
           INITIALIZE                  ONSHI-YAKUZAI-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-YKZ-S-HOSPNUM
           MOVE    WRK-PTID            TO  ONS-YKZ-S-PTID
      *2022/08
      *    UID
           MOVE    WRK-UID             TO  ONS-YKZ-S-TBL-UUID
      *    診療年月
           MOVE    QUAREQ4-SRYYM(IDX)  TO  ONS-YKZ-S-SRYYM
      *    入外区分（４：調剤）
           MOVE    QUAREQ4-NYUGAIKBN(IDX)
                                       TO  ONS-YKZ-S-NYUGAIKBN
      *2022/02/24
      *    調剤機関毎連番
           MOVE    QUAREQ4-GRPNUM  (IDX)
                                       TO  ONS-YKZ-S-CHOZAI-SEQNUM
      *    処方機関毎連番
           MOVE    QUAREQ4-GRPNUM2  (IDX)
                                       TO  ONS-YKZ-S-SHOHO-SEQNUM
      *****
      *    診療日
           MOVE    WRK-DD              TO  ONS-YKZ-S-SRYDD
      *    処方箋発行日
           MOVE    QUAREQ4-PR-DATE (IDX IDY)
                                       TO  ONS-YKZ-S-HAKKOYMD
      *
           MOVE    ZERO                TO  WRK-RENNUM
      *
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ         >   100   )
                         OR   (WRK-ERRCD   NOT =   SPACE )
               IF      QUAREQ4-SRYKBN (IDX IDY IDZ)    NOT =   SPACE
                   ADD     1               TO  WRK-RENNUM
                   PERFORM 200221-ONSHI-YAKUSUB-HEN-SEC
               END-IF
           END-PERFORM
      *
           .
       200221-ONSHI-YAKUZAI-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報資格確認サブ登録処理 
      *****************************************************************
       200221-ONSHI-YAKUSUB-HEN-SEC  SECTION.
      *
           INITIALIZE                  ONS-YKZ-S-DATA
      *
           MOVE    WRK-RENNUM          TO  ONS-YKZ-S-RENNUM
      *    診療識別等区分
           MOVE    QUAREQ4-SRYKBN(IDX IDY IDZ)
                                       TO  ONS-YKZ-S-SRYKBN
      *    用法コード
           MOVE    QUAREQ4-USAGEC(IDX IDY IDZ)
                                       TO  ONS-YKZ-S-YOHOCD
      *    用法名称
           MOVE    QUAREQ4-USAGEN(IDX IDY IDZ)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-YKZ-S-YOHONAME
      *    特別指示
           MOVE    QUAREQ4-SPINST(IDX IDY IDZ)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-YKZ-S-SHIJI
      *    医薬品コード
           MOVE    QUAREQ4-DRUGCD(IDX IDY IDZ)
                                       TO  ONS-YKZ-S-SRYCD
      *    薬剤名
           MOVE    QUAREQ4-DRUGNAME(IDX IDY IDZ)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-YKZ-S-YAKUZAINAME
      *    成分名
           MOVE    QUAREQ4-INGRENAME(IDX IDY IDZ)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-YKZ-S-SEIBUNNAME
      *    単位
           MOVE    QUAREQ4-UNIT     (IDX IDY IDZ)
                                       TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  ONS-YKZ-S-TANINAME
      *    使用量
           INITIALIZE                  ORCSNUMAREA
           MOVE    QUAREQ4-QUAL (IDX IDY IDZ)  TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      ( SNUM-RC           =   ZERO  )
                MOVE    SNUM-OUTNUM    TO  ONS-YKZ-S-SURYO
           END-IF
      *   １回用量
           INITIALIZE                  ORCSNUMAREA
           MOVE    QUAREQ4-DNDOSE (IDX IDY IDZ)  TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      ( SNUM-RC           =   ZERO  )
                MOVE    SNUM-OUTNUM    TO  ONS-YKZ-S-YORYO
           END-IF
      *    回数
           INITIALIZE                  ORCSNUMAREA
           MOVE    QUAREQ4-TIMES (IDX IDY IDZ)  TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      ( SNUM-RC           =   ZERO  )
                MOVE    SNUM-OUTNUM    TO  ONS-YKZ-S-KAISU
           END-IF
      *
      *    資格確認薬剤情報サブ登録処理
           MOVE    SPA-OPID            TO  ONS-YKZ-S-OPID
      *
           MOVE    SMCNDATE-YMD        TO  ONS-YKZ-S-CREYMD
           MOVE    SMCNDATE-HMS        TO  ONS-YKZ-S-CREHMS
           MOVE    SMCNDATE-YMD        TO  ONS-YKZ-S-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONS-YKZ-S-UPHMS
      *
           MOVE    ONSHI-YAKUZAI-SUB-REC   TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_onshi_yakuzai_sub"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "80"                TO  WRK-ERRCD
               DISPLAY "004 ONSHI-YAKUZAI-SUB INSERT ERR:"  MCP-RC
                       ",KEY:" ONS-YKZ-S-KEY
           END-IF
           .
       200221-ONSHI-YAKUSUB-HEN-EXT.
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
      *        薬剤情報取得依頼済フラグ
               MOVE    "02"                TO  ONSKAKU-YAKUZAI-ENDFLG
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
                   DISPLAY "004  ONSKAKU KEY UPD ERR:"  MCP-RC
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

