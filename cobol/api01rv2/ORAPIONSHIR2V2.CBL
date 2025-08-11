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
       PROGRAM-ID.         ORAPIONSHIR2V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : API 資格確認特定検診情報取得処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/02/03    NMED−多々納  新規作成
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
           03  FLG-ONSHI-MAIN          PIC 9(01).
           03  FLG-ONSHI-SUB           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-ONSKAKU             PIC 9(01).
      *
           03  FLG-SP                  PIC 9(01).
      *
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
      *
           03  IDXM                    PIC 9(04).
           03  IDXR                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-OPID                PIC X(16).
      *
           03  WRK-JISSIYMD            PIC X(08).
           03  WRK-PTNUM               PIC X(20).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRCD1          PIC X(01).
               05  WRK-ERRCD           PIC X(02).
           03  WRK-ERRMSG              PIC X(200).
           03  WRK-ERRCD2              PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(30).
      *
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
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
      *    COPY    "CPORCSKANACHK.INC".
      *    拡張漢字チェックサブ
      *    COPY    "CPORCSKANACHK2.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINESPECGETREQ.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINESPECGETRES.INC".
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *    COPY    "MCPDATA2.INC".
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
      *    COPY    "CPSK1051.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    オン資格確認情報
      *01  ONSKAKU-REC.
      *    COPY    "CPONSHI-KAKU.INC".
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
           COPY    "API01RV2SCRAREA.INC".
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
           DISPLAY   "*  onlinespecgetreq start  *"
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
           IF      WRK-ERRCD-G         =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD-G         =   "999"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST49-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "****************************"
           DISPLAY   "*   onlinespecgetreq end   *"
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
           INITIALIZE                      XML-ONLINESPECGETRES
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
           MOVE    WRK-HEN-DATE        TO  SPECRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  SPECRES-INFORMATION-TIME
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "999"               TO  WRK-ERRCD-G
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
               MOVE   "89"             TO  WRK-ERRCD
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
           MOVE    SPACE           TO  WRK-ERRCD-G
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
      *
      *    資格確認特定検診処理
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    SPECREQ-REQUEST-NUMBER
               WHEN    "01"
                   PERFORM 2002-SIKAKU-SYORI01-SEC
               WHEN    "02"
                   PERFORM 2002-SIKAKU-SYORI02-SEC
               WHEN    OTHER
                   MOVE    "91"            TO  WRK-ERRCD
               END-EVALUATE
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
           IF      APILST49-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINESPECGETREQ
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinespecgetv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST49-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinespecgetreq"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINESPECGETREQ
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "98"              TO  WRK-ERRCD
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
               IF      WRK-ERRCD2      NOT =   SPACE
                   MOVE    "W"             TO  WRK-ERRCD1
                   MOVE    WRK-ERRCD2      TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "000"               TO  SPECRES-API-RESULT
               MOVE    "処理終了"          TO  SPECRES-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    "E"                 TO  WRK-ERRCD1
               MOVE    WRK-ERRCD-G         TO  SPECRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  SPECRES-API-RESULT-MSG
           END-IF
      *
      *
           IF      APILST49-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinespecgetv2res" TO  MCP-TABLE
           MOVE    1                   TO  SPECRES-MODE
           MOVE    "onlinespecgetres"  TO  SPECRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINESPECGETRES
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  SPECRES-OBJECT    TO  APILST49-BODY
               MOVE  "application/xml" TO  APILST49-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
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
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
      *    編集処理
           PERFORM 20010-INPUT-HEN-SEC
      *
      *    患者番号チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-PTNUM-CHK-SEC
           END-IF
      *    実施年月日チェック
           IF    ( WRK-ERRCD           =   SPACE )
            AND  ( WRK-JISSIYMD      NOT =   SPACE )
               PERFORM 20011-JISSIYMD-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目変換編集処理
      *****************************************************************
       20010-INPUT-HEN-SEC     SECTION.
      *
      *    患者番号
           MOVE    SPECREQ-PATIENTID       TO  SPECRES-PATIENTID
           MOVE    SPECREQ-PATIENTID       TO  WRK-PTNUM
      *    基準日
           MOVE    SPECREQ-JISSIYMD        TO  SPECRES-IN-JISSIYMD
      *    数字のみに編集
           IF     (SPECREQ-JISSIYMD    NOT =   SPACE )
               MOVE    SPECREQ-JISSIYMD        TO  WRK-HEN-DATE
               MOVE    SPACE                   TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD                TO  WRK-JISSIYMD
           ELSE
               MOVE    SPACE                   TO  WRK-JISSIYMD
           END-IF
      *
      *    必須入力チェック
      *    患者番号
           IF     (WRK-PTNUM           =   SPACE )
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *
           .
       20010-INPUT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       20011-PTNUM-CHK-SEC     SECTION.
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
               GO      TO                  20011-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPTID-PTNUM         TO  SPECRES-PATIENTID
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           MOVE    SPTID-PTID          TO  SPA-PTID
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPTID-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "10"                TO  WRK-ERRCD
           ELSE
      *        患者情報検索
               PERFORM 200212-PTINF-HEN-SEC
           END-IF
           .
       20011-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報内容編集処理
      *****************************************************************
       200212-PTINF-HEN-SEC        SECTION.
      *
           MOVE    PTINF-NAME          TO  SPECRES-NAME
           MOVE    PTINF-KANANAME      TO  SPECRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  SPECRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  SPECRES-SEX
           .
       200212-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認年月チェック
      *****************************************************************
       20011-JISSIYMD-CHK-SEC          SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-JISSIYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
           .
       20011-JISSIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認特定検診情報実施年月日取得処理
      *****************************************************************
       2002-SIKAKU-SYORI01-SEC     SECTION.
      *
           INITIALIZE                  SPECRES-DATA01-AREA
           INITIALIZE                  SPECRES-DATA02-AREA
      *
      *    特定検診情報メイン検索
           INITIALIZE                  ONSHI-KENSIN-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-KNS-M-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-KNS-M-PTID
      *
           MOVE    ONSHI-KENSIN-MAIN-REC   TO  MCPDATA-REC
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-MAIN
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   ( FLG-ONSHI-MAIN    =   1 )
                        OR ( IDX               >   50)
      *
               ADD     1                   TO  IDX
               IF      IDX                 >   50
                   CONTINUE
               ELSE
                   MOVE    ONS-KNS-M-JISSHIYMD  TO  WRK-SYMD
                   MOVE    SPACE                TO  WRK-THMS
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE     TO  SPECRES-TOR-JISSIYMD
                                                      (IDX)
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    対象なし
           IF      IDX                 =   ZERO
               MOVE    "50"            TO  WRK-ERRCD
           END-IF
      *    ５０件以上あり
           IF      IDX                 >   50
               MOVE    "51"            TO  WRK-ERRCD2
           END-IF
      *
           .
       2002-SIKAKU-SYORI01-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認特定検診情報編集処理
      *****************************************************************
       2002-SIKAKU-SYORI02-SEC      SECTION.
      *
           INITIALIZE                  SPECRES-DATA02-AREA
      *
      *    特定検診情報メイン検索
           INITIALIZE                  ONSHI-KENSIN-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-KNS-M-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-KNS-M-PTID
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           IF      WRK-JISSIYMD    NOT =   SPACE
               MOVE    WRK-JISSIYMD        TO  ONS-KNS-M-JISSHIYMD
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    ONSHI-KENSIN-MAIN-REC   TO  MCPDATA-REC
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-MAIN
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   ( FLG-ONSHI-MAIN    =   1 )
                        OR ( IDX               >   10)
      *
               ADD     1                   TO  IDX
               IF      IDX                 >   10
                   CONTINUE
               ELSE
                   PERFORM 20021-ONSHI-MEIN-HEN-SEC
                   PERFORM 20021-ONSHI-SUB-SYORI-SEC
               END-IF
      *
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_kenshin_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    対象なし
           IF      IDX                 =   ZERO
               MOVE    "50"            TO  WRK-ERRCD
           END-IF
      *    １０件以上あり
           IF      IDX                 >   10
               MOVE    "52"            TO  WRK-ERRCD2
           END-IF
           .
       2002-SIKAKU-SYORI02-EXT.
           EXIT.
      *
      *****************************************************************
      *     特定検診情報資格メイン編集処理
      *****************************************************************
       20021-ONSHI-MEIN-HEN-SEC       SECTION.
      *
           IF      IDX                =   1
      *        最初の保険情報
               MOVE    ONS-KNS-M-HKNJANUM
                                       TO  SPECRES-HKNJANUM
               MOVE    ONS-KNS-M-KIGO
                                       TO  SPECRES-KIGO
               MOVE    ONS-KNS-M-NUM
                                       TO  SPECRES-NUM
               MOVE    ONS-KNS-M-EDABAN
                                       TO  SPECRES-EDABAN
           END-IF
      *
           MOVE    ONS-KNS-M-JISSHIYMD  TO  WRK-SYMD
           MOVE    SPACE                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE         TO  SPECRES-JISSIYMD
                                                      (IDX)
           .
       20021-ONSHI-MEIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     特定検診情報資格サブ編集処理
      *****************************************************************
       20021-ONSHI-SUB-SYORI-SEC       SECTION.
      *
      *    特定検診情報サブ検索
           INITIALIZE                  ONSHI-KENSIN-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-KNS-S-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-KNS-S-PTID
      *    診療年月
           MOVE    ONS-KNS-M-JISSHIYMD TO  ONS-KNS-S-JISSHIYMD
      *
           MOVE    ONSHI-KENSIN-SUB-REC    TO  MCPDATA-REC
           MOVE    "tbl_onshi_kenshin_sub"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-ONSHISUB-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-SUB
           END-IF
      *
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL   (FLG-ONSHI-SUB  =   1  )
      *
               PERFORM 200211-ONSHI-MEISEI-SEC
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-ONSHISUB-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_onshi_kenshin_sub"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       20021-ONSHI-SUB-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報資格確認サブ編集処理 
      *****************************************************************
       200211-ONSHI-MEISEI-SEC  SECTION.
      *
           ADD     1                   TO  IDY
           IF      IDY                 >   100
               GO  TO  200211-ONSHI-MEISEI-EXT
           END-IF
      *
      *    項目コード
           MOVE    ONS-KNS-S-ITEMCODE      TO  SPECRES-ITEMCODE
                                                         (IDX IDY)
      *    項目名
           MOVE    ONS-KNS-S-ITEMNAME      TO  SPECRES-ITEMNAME
                                                         (IDX IDY)
      *    データ型
           MOVE    ONS-KNS-S-DATA-TYPE     TO  SPECRES-DATA-TYPE
                                                         (IDX IDY)
      *    データ値
           MOVE    ONS-KNS-S-DATA-VALUE    TO  SPECRES-DATA-VALUE
                                                         (IDX IDY)
      *    単位
           MOVE    ONS-KNS-S-DATA-TANNI    TO  SPECRES-DATA-TANNI
                                                         (IDX IDY)
           .
       200211-ONSHI-MEISEI-EXT.
           EXIT.
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
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
           EVALUATE    WRK-ERRCD
           WHEN    "01"
               MOVE    "患者番号の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "実施年月日が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "50"
               MOVE    "対象がありませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "51"
               MOVE   "実施年月日が５０件以上あります。"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE   "実施年月日が１０件以上あります。"
                                               TO  WRK-ERRMSG
      *共通エラー
           WHEN    "89"
      *        ORCSCOMMON のエラー
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "職員情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "医療機関情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "システム日付が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号構成情報が取得できません"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "グループ医療機関が不整合です。"
                           "処理を終了して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "システム項目が設定できません"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "他端末使用中"          TO  WRK-ERRMSG
           WHEN    "91"
               MOVE    "処理区分未設定"        TO  WRK-ERRMSG
           WHEN    "97"
               MOVE    "送信内容に誤りがあります。"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE    "送信内容の読込ができませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "ユーザＩＤ未登録。"
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
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認特定検診情報メイン読込
      ******************************************************************
       910-ONSHIMAIN-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kenshin_main"    TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-KENSIN-MAIN-REC
               MOVE    ZERO                TO  FLG-ONSHI-MAIN
           ELSE
               INITIALIZE                      ONSHI-KENSIN-MAIN-REC
               MOVE    1                   TO  FLG-ONSHI-MAIN
           END-IF
      *
           .
       910-ONSHIMAIN-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認特定検診情報サブ読込
      ******************************************************************
       920-ONSHISUB-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kenshin_sub"  TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-KENSIN-SUB-REC
               MOVE    ZERO                TO  FLG-ONSHI-SUB
           ELSE
               INITIALIZE                      ONSHI-KENSIN-SUB-REC
               MOVE    1                   TO  FLG-ONSHI-SUB
           END-IF
      *
           .
       920-ONSHISUB-READ-EXT.
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

