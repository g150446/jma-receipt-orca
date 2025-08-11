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
       PROGRAM-ID.         ORAPIONSHIR3V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : API 資格確認診療情報取得処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  22/08/XX    ORCAMO        新規作成
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
           03  FLG-ONSHIKA-SUB         PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-ONSKAKU             PIC 9(01).
      *
           03  FLG-SP                  PIC 9(01).
           03  FLG-NASI                PIC 9(01).
           03  FLG-MSGWRITE            PIC 9(01).
      *
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXM                    PIC 9(04).
           03  IDXR                    PIC 9(04).
           03  IDR                     PIC 9(04).
           03  IDD                     PIC 9(02).
           03  IDS                     PIC 9(04).
           03  IDXS                    PIC 9(04).
           03  IDX-MAX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-OPID                PIC X(16).
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
      *
           03  WRK-PTNUM               PIC X(20).
      *    エラーコード
           03  WRK-ERRCD-G.
               05  WRK-ERRCD1          PIC X(01).
               05  WRK-ERRCD           PIC X(02).
           03  WRK-ERRMSG              PIC X(200).
           03  WRK-ERR2                PIC X(02).
           03  WRK-ERR3                PIC X(02).
           03  WRK-ERRCD3              PIC X(02).
           03  WRK-ERRCD4              PIC X(02).
           03  WRK-ERRCD5              PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(30).
      *
           03  WRK-DAY                 PIC X(02).
           03  WRK-HAKKOYMD            PIC X(08).
      *
           03  WRK-MSGCNT              PIC 9(03).
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
           03  WRK-STRYMD.
               05  WRK-STRYM.
                   07  WRK-STRYY           PIC 9(04).
                   07  WRK-STRMM           PIC 9(02).
               05  WRK-STRDD           PIC 9(02).
           03  WRK-CHK-SRYYM           PIC X(06).
      *
           03  WRK-MED-SRYYMD.
               05  WRK-MED-SRYYM       PIC X(06).
               05  WRK-MED-SRYDD       PIC X(02).
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
       01  WRK-MEDRES1-REC.
           03  WRK-MEDRES1-MEISA-AREA.
      *         05  WRK-MEDRES1-MEI-TBL           OCCURS   3.
      *        入外等の別
                   07  WRK-MEDRES1-NYUGAIKBN       PIC X(01).
      *
      *調剤日別情報
                   07  WRK-MEDRES1-DI-DATE-OCC     OCCURS   100.
      *処理済み区分
                       09  WRK-MEDRES1-CHK         PIC X(01).
      *調剤日
                       09  WRK-MEDRES1-DI-DAY      PIC X(02).
      *処方箋発行日
                       09  WRK-MEDRES1-PR-DATE     PIC X(10).
      *                医療機関コード
                       09  WRK-MEDRES1-HOSPCD      PIC  X(10).
      *                医療機関名
                       09  WRK-MEDRES1-HOSPNAME    PIC  X(80).
      *                調剤薬局コード
                       09  WRK-MEDRES1-CHOZAICD    PIC  X(10).
      *                調剤薬局名
                       09  WRK-MEDRES1-CHOZAINAME  PIC  X(80).
      *薬剤情報群
                       09  WRK-MEDRES1-PR-DATA-INFG.
      *薬剤情報
                           11  WRK-MEDRES1-PR-DATA-INF
                                                         OCCURS  100.
      *診療識別等区分
                               13  WRK-MEDRES1-SRYKBN
                                                          PIC X(02).
      *医薬品コード
                               13  WRK-MEDRES1-SRYCD      PIC X(09).
      *薬剤名
                               13  WRK-MEDRES1-SRYNAME    PIC X(64).
      *単位
                               13  WRK-MEDRES1-TANINAME
                                                            PIC X(12).
      *使用量
                               13  WRK-MEDRES1-SURYO
                                                           PIC X(11).
      *1回用量
                               13  WRK-MEDRES1-SURYO2
                                                            PIC X(11).
      *回数
                               13  WRK-MEDRES1-KAISU 
                                                           PIC X(03).
      *歯科加算区分
                               13  WRK-MEDRES1-SHIKA-KBN   PIC X(01).
                               13  WRK-MEDRES1-S-KEY.
      *医療機関識別番号
                                  15  WRK-SNR-S-HOSPNUM       PIC 9(02).
      *TBL_UUID
                                  15  WRK-SNR-S-TBL-UUID      PIC X(36).
      *患者ＩＤ
                                  15  WRK-SNR-S-PTID          PIC 9(10).
      *診療年月
                                  15  WRK-SNR-S-SRYYM         PIC X(06).
      *入外等区分
                                  15  WRK-SNR-S-NYUGAIKBN     PIC X(01).
      *診療日
                                  15  WRK-SNR-S-SRYDD         PIC X(02).
      *処方箋発行日
                                  15  WRK-SNR-S-HAKKOYMD      PIC X(08).
      *明細番号
                                  15  WRK-SNR-S-RENNUM        PIC 9(03).
      ***
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
           COPY    "CPONLINEMEDGETREQ1.INC".
      *    API XML書き込み用定義体
           COPY    "CPONLINEMEDGETRES1.INC".
      *    API XML書き込み用定義体
           COPY    "CPONLINEMEDGETRES2.INC".
      *    API XML書き込み用定義体(歯科)
           COPY    "CPONLINEMEDGETRES3.INC".
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
      *     COPY    "CPSK1051.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    オン資格確認情報
      * 01  ONSKAKU-REC.
      *     COPY    "CPONSHI-KAKU.INC".
      *
      *    資格確認診療行為情報メイン
       01  ONSHI-SHINRYO-MAIN-REC.
           COPY    "CPONSHI-SHINRYO-MAIN.INC".
      *
      *    資格確認診療行為情報サブ
       01  ONSHI-SHINRYO-SUB-REC.
           COPY    "CPONSHI-SHINRYO-SUB.INC".
      *
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
           DISPLAY   "*  onlinemegetreq1  start  *"
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
               MOVE   404                  TO  APILST51-HTTPSTATUS
           ELSE
      *        返却領域編集
               IF      FLG-MSGWRITE        =   ZERO
                   PERFORM 300-RGTXMLMAKE-SEC
               END-IF
           END-IF
      *
           DISPLAY   "****************************"
           DISPLAY   "*   onlinemegetreq1  end   *"
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
           INITIALIZE                      XML-ONLINEMEDGETRES1
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
           MOVE    WRK-HEN-DATE        TO  MEDRES1-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  MEDRES1-INFORMATION-TIME
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
      *    資格確認薬剤情報処理
           IF      WRK-ERRCD           =   SPACE
               EVALUATE    MEDREQ1-REQUEST-NUMBER
               WHEN    "01"
                   PERFORM 2002-SIKAKU-SYORI01-SEC
               WHEN    "02"
                   IF      MEDREQ1-HOSPCD(3:1)     =   "3"
      *                歯科
                       PERFORM 2003-SIKAKU-SHIKA-SYORI-SEC
                   ELSE
      *                医科
                       PERFORM 2002-SIKAKU-SHINRYO-SYORI-SEC
                   END-IF
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
           IF      APILST51-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEMEDGETREQ1
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinemedgetv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST51-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinemedgetreq"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEMEDGETREQ1
               PERFORM 10001-XML-REMAKE-SEC
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
               IF      WRK-ERR2        NOT =   SPACE
                   MOVE    "W"             TO  WRK-ERRCD1
                   MOVE    WRK-ERR2        TO  WRK-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               MOVE    "000"               TO  MEDRES1-API-RESULT
               MOVE    "処理終了"          TO  MEDRES1-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               IF      WRK-ERRCD1          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRCD1
               END-IF
               MOVE    WRK-ERRCD-G         TO  MEDRES1-API-RESULT
               MOVE    WRK-ERRMSG          TO  MEDRES1-API-RESULT-MSG
           END-IF
      *
      *
           IF      APILST51-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinemedgetv2res" TO  MCP-TABLE
           MOVE    1                   TO  MEDRES1-MODE
           MOVE    "onlinemedgetres"   TO  MEDRES1-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINEMEDGETRES1
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  MEDRES1-OBJECT    TO  APILST51-BODY
               MOVE  "application/xml" TO  APILST51-CONTENT-TYPE
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
           IF      MEDREQ1-HOSPCD      =   LOW-VALUE
               MOVE    SPACE           TO  MEDREQ1-HOSPCD
           END-IF
           IF      MEDREQ1-CHOZAICD    =   LOW-VALUE
               MOVE    SPACE           TO  MEDREQ1-CHOZAICD
           END-IF
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
      *    確認年月チェック
           IF    ( WRK-ERRCD           =   SPACE )
            AND  ( WRK-SRYYMD      NOT =   SPACE )
               PERFORM 20011-SRYYM-CHK-SEC
           END-IF
      *    入外等区分チェック
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20011-NYUGAIKBN-CHK-SEC
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
           MOVE    MEDREQ1-PATIENTID       TO  MEDRES1-PATIENTID
           MOVE    MEDREQ1-PATIENTID       TO  WRK-PTNUM
      *    基準日
           MOVE    MEDREQ1-SRYYM           TO  MEDRES1-SRYYM
      *    数字のみに編集
           IF     (MEDREQ1-SRYYM       NOT =   SPACE )
               MOVE    MEDREQ1-SRYYM           TO  WRK-HEN-DATE
               MOVE    SPACE                   TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD                TO  WRK-SRYYMD
           ELSE
               MOVE    SPACE                   TO  WRK-SRYYMD
           END-IF
      *
      *    必須入力チェック
      *    患者番号
           IF     (WRK-PTNUM           =   SPACE )
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *
      *    リクエスト＝０２の必須
           IF      MEDREQ1-REQUEST-NUMBER  =   "02"
      *        確認年月
               IF     (MEDREQ1-SRYYM       =   SPACE )
                   MOVE    "02"            TO  WRK-ERRCD
               END-IF
      *        入外区分
      *         IF     (MEDREQ1-NYUGAIKBN   =   SPACE )
      *           AND  (SPA-ERRCD           =   SPACE )
      *             MOVE    "13"            TO  WRK-ERRCD
      *         END-IF
      ******
      *        医療機関・薬局コード
               IF     (MEDREQ1-HOSPCD      =   SPACE )
                 AND  (WRK-ERRCD           =   SPACE )
                   MOVE    "14"            TO  WRK-ERRCD
               END-IF
           END-IF
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
           MOVE    SPTID-PTNUM         TO  MEDRES1-PATIENTID
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
           MOVE    PTINF-NAME          TO  MEDRES1-NAME
           MOVE    PTINF-KANANAME      TO  MEDRES1-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MEDRES1-BIRTHDAY
           MOVE    PTINF-SEX           TO  MEDRES1-SEX
           .
       200212-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認年月チェック
      *****************************************************************
       20011-SRYYM-CHK-SEC             SECTION.
      *
      *    日付チェックサブ
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
      *    １日とする
           MOVE    "01"                TO  WRK-SRYDD
           MOVE    WRK-SRYYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "11"                TO  WRK-ERRCD
           END-IF
           .
       20011-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外等区分チェック
      *****************************************************************
       20011-NYUGAIKBN-CHK-SEC             SECTION.
      *
           EVALUATE    MEDREQ1-NYUGAIKBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "4"
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   MOVE    "12"                TO  WRK-ERRCD
           END-EVALUATE
           .
       20011-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認薬剤情報算定年月処理
      *****************************************************************
       2002-SIKAKU-SYORI01-SEC     SECTION.
      *
           INITIALIZE                  MEDRES1-DATA01-AREA
      *
           IF      WRK-SRYYMD          =   SPACE
      *        システム日の前月からする
               MOVE    SPA-SYSYMD      TO  WRK-STRYMD
               IF      WRK-STRMM       =   01
                   MOVE    12          TO  WRK-STRMM
                   COMPUTE WRK-STRYY   =   WRK-STRYY   -  1
               ELSE
                   COMPUTE WRK-STRMM   =   WRK-STRMM   -  1
               END-IF
           ELSE
               MOVE    WRK-SRYYMD       TO  WRK-STRYMD
           END-IF
      *
      *    診療情報メイン検索
           INITIALIZE                  ONSHI-SHINRYO-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SNR-M-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-SNR-M-PTID
      *    基準年月
           MOVE    WRK-STRYM           TO  ONS-SNR-M-SRYYM
      *
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
      *
           MOVE    ONSHI-SHINRYO-MAIN-REC  TO  MCPDATA-REC
           MOVE    "tbl_onshi_shinryo_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-MAIN
           END-IF
      *    基準日 から ６か月分を検索する
           MOVE    ZERO                TO  IDX
           MOVE    SPACE               TO  WRK-CHK-SRYYM
           PERFORM UNTIL   ( FLG-ONSHI-MAIN    =   1 )
                      OR   ( IDX               >=  6 )
      *
               IF      (WRK-CHK-SRYYM       =   ONS-SNR-M-SRYYM)
                   CONTINUE
               ELSE
                   ADD     1                TO  IDX
                   MOVE    ONS-SNR-M-SRYYM  TO  WRK-SYMD(1:6)
                   MOVE    SPACE            TO  WRK-THMS
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE(1:7)
                                            TO  MEDRES1-TOR-SRYYM
                                                      (IDX)
               END-IF
      *        医療機関コード・調剤薬局コード
               PERFORM VARYING     IDY      FROM    1   BY  1
                       UNTIL     ( IDY      >   20 )
                   IF     (MEDRES1-TOR-HOSPCD (IDX IDY)
                                            =   ONS-SNR-M-HOSPCD )
                     AND  (MEDRES1-TOR-CHOZAICD (IDX IDY)
                                            =   ONS-SNR-M-CHOZAICD)
                     AND  (MEDRES1-TOR-NYUGAIKBN (IDX IDY)
                                            =   ONS-SNR-M-NYUGAIKBN)
                       MOVE    20               TO  IDY
                   ELSE
                       IF     (MEDRES1-TOR-HOSPCD (IDX IDY)
                                                =   SPACE )
                         AND  (MEDRES1-TOR-CHOZAICD (IDX IDY)
                                                =   SPACE )
                           MOVE    ONS-SNR-M-HOSPCD    TO
                                         MEDRES1-TOR-HOSPCD (IDX IDY)
                           MOVE    ONS-SNR-M-HOSPNAME  TO
                                         MEDRES1-TOR-HOSPNAME(IDX IDY)
                           MOVE    ONS-SNR-M-CHOZAICD    TO
                                         MEDRES1-TOR-CHOZAICD (IDX IDY)
                           MOVE    ONS-SNR-M-CHOZAINAME TO
                                         MEDRES1-TOR-CHOZAINAME
                                                            (IDX IDY)
                           MOVE    ONS-SNR-M-NYUGAIKBN TO
                                         MEDRES1-TOR-NYUGAIKBN
                                                            (IDX IDY)
                           MOVE    20            TO  IDY
                        END-IF
                   END-IF
      *
               END-PERFORM
      *
               MOVE    ONS-SNR-M-SRYYM      TO  WRK-CHK-SRYYM
      *
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_shinryo_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    対象なし
           IF      IDX                 =   ZERO
               MOVE    "50"            TO  WRK-ERRCD
           END-IF
      *
           .
       2002-SIKAKU-SYORI01-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認　医科診療情報編集処理
      *****************************************************************
       2002-SIKAKU-SHINRYO-SYORI-SEC      SECTION.
      *
           INITIALIZE                  WRK-MEDRES1-REC
      *    XMLOPEN
           PERFORM 1000-RGTXMLOPEN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
              GO  TO  2002-SIKAKU-SHINRYO-SYORI-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-MSGWRITE
      *    患者情報等編集
           PERFORM 20021-PTINF-HEN02-SEC
      *
      *    診療情報メイン検索
           INITIALIZE                  ONSHI-SHINRYO-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SNR-M-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-SNR-M-PTID
      *    診療年月
           MOVE    WRK-SRYYM           TO  ONS-SNR-M-SRYYM
      *    医療機関・薬局コード
           MOVE    MEDREQ1-HOSPCD      TO  ONS-SNR-M-HOSPCD
      *
           MOVE    "key5"              TO  WRK-MCP-PATHNAME
      *    入外等区分
           IF      MEDREQ1-NYUGAIKBN   NOT =   SPACE
               MOVE    MEDREQ1-NYUGAIKBN
                                           TO  ONS-SNR-M-NYUGAIKBN
               MOVE    "key6"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    ONSHI-SHINRYO-MAIN-REC  TO  MCPDATA-REC
           MOVE    "tbl_onshi_shinryo_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-MAIN
           END-IF
           MOVE    ZERO                TO  WRK-MSGCNT
           PERFORM UNTIL     FLG-ONSHI-MAIN    =   1
      *
      *        医療機関コード判定
               MOVE    ZERO                TO  FLG-NASI
      *        医療機関・調剤コード、処方箋発行機関コード判定
               IF     (MEDREQ1-CHOZAICD    NOT =   SPACE )
                   IF     (MEDREQ1-CHOZAICD    =   ONS-SNR-M-CHOZAICD)
                      MOVE    ZERO             TO  FLG-NASI
                   ELSE
                      MOVE    1                TO  FLG-NASI
                   END-IF
               END-IF
      *
               IF      FLG-NASI            =   ZERO
                   PERFORM 20021-ONSHI-SUB-SYORI-SEC
                END-IF
      *
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_shinryo_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-MSGCNT             >   ZERO
               CONTINUE
           ELSE
      *        対象なし
               MOVE    "50"                TO  WRK-ERRCD
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               IF      WRK-ERRCD1          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRCD1
               END-IF
               MOVE    WRK-ERRCD-G         TO  MEDRES2-API-RESULT
               MOVE    WRK-ERRMSG          TO  MEDRES2-API-RESULT-MSG
               MOVE    MEDREQ1-NYUGAIKBN   TO  MEDRES2-NYUGAIKBN
               MOVE    MEDREQ1-HOSPCD      TO  MEDRES2-HOSPCD
               MOVE    MEDREQ1-CHOZAICD    TO  MEDRES2-CHOZAICD
               PERFORM 2000-RGTXMLWRITE-SEC
           END-IF
      *
      *    XML情報CLOSE
           PERFORM 3000-RGTXMLCLOSE-SEC
           .
       2002-SIKAKU-SHINRYO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報内容編集処理
      *****************************************************************
       20021-PTINF-HEN02-SEC        SECTION.
      *
           MOVE    "000"               TO  MEDRES2-API-RESULT
           MOVE    "処理終了"          TO  MEDRES2-API-RESULT-MSG
      *
           MOVE    MEDRES1-INFORMATION-DATE
                                       TO  MEDRES2-INFORMATION-DATE
           MOVE    MEDRES1-INFORMATION-TIME
                                       TO  MEDRES2-INFORMATION-TIME
      *
           MOVE    MEDRES1-PATIENTID   TO  MEDRES2-PATIENTID
           MOVE    MEDRES1-NAME        TO  MEDRES2-NAME
           MOVE    MEDRES1-KANANAME    TO  MEDRES2-KANANAME
           MOVE    MEDRES1-BIRTHDAY    TO  MEDRES2-BIRTHDAY
           MOVE    MEDRES1-SEX         TO  MEDRES2-SEX
           .
       20021-PTINF-HEN02-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     診療情報資格サブ編集処理
      *****************************************************************
       20021-ONSHI-SUB-SYORI-SEC       SECTION.
      *
      *    診療情報サブ検索
           INITIALIZE                  ONSHI-SHINRYO-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SNR-S-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-SNR-S-PTID
      *    UID
           MOVE    ONS-SNR-M-TBL-UUID  TO  ONS-SNR-S-TBL-UUID
      *    診療年月
           MOVE    ONS-SNR-M-SRYYM     TO  ONS-SNR-S-SRYYM
      *    入外区分
           MOVE    ONS-SNR-M-NYUGAIKBN
                                       TO  ONS-SNR-S-NYUGAIKBN
      *
           MOVE    ONSHI-SHINRYO-SUB-REC   TO  MCPDATA-REC
           MOVE    "tbl_onshi_shinryo_sub"
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
      **** MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDZ
           MOVE    SPACE               TO  WRK-DAY   
           MOVE    SPACE               TO  WRK-HAKKOYMD
           MOVE    SPACE               TO  WRK-ERRCD3
           PERFORM UNTIL   (FLG-ONSHI-SUB  =   1  )
      *        診療日毎に編集
               IF     (WRK-DAY         =   ONS-SNR-S-SRYDD)
                  AND (WRK-HAKKOYMD    =   ONS-SNR-S-HAKKOYMD)
                   CONTINUE
               ELSE
      *            診療日毎に出力
                   IF      IDZ                 >   ZERO
                       PERFORM 2000-RGTXMLWRITE-SEC
                   END-IF
      *            診療ヘッド
                   PERFORM 200211-ONSHI-DAY-SEC
      *
                   MOVE    ONS-SNR-S-SRYDD     TO  WRK-DAY
                   MOVE    ONS-SNR-S-HAKKOYMD  TO  WRK-HAKKOYMD
                   MOVE    ZERO                TO  IDZ
               END-IF
               IF      WRK-ERRCD3          =   SPACE
      *            診療明細
                   PERFORM 200211-ONSHI-MEISEI-SEC
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-ONSHISUB-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_onshi_shinryo_sub"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療日毎に出力
           IF      IDZ                 >   ZERO
               PERFORM 2000-RGTXMLWRITE-SEC
           END-IF
           .
       20021-ONSHI-SUB-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報資格確認　編集処理 
      *****************************************************************
       200211-ONSHI-DAY-SEC            SECTION.
      *
      *****
      *    最初のみ保険情報
           IF      WRK-MSGCNT          =   ZERO
               MOVE    ONS-SNR-M-HKNJANUM
                                       TO  MEDRES2-HKNJANUM
               MOVE    ONS-SNR-M-KIGO
                                       TO  MEDRES2-KIGO
               MOVE    ONS-SNR-M-NUM
                                       TO  MEDRES2-NUM
               MOVE    ONS-SNR-M-EDABAN
                                       TO  MEDRES2-EDABAN
           END-IF
      *****
      *
           INITIALIZE                  MEDRES2-DATA02-AREA
      *    診療日
           MOVE    ONS-SNR-S-SRYYM     TO  WRK-MED-SRYYM
           MOVE    ONS-SNR-S-SRYDD     TO  WRK-MED-SRYDD
           MOVE    WRK-MED-SRYYMD      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MEDRES2-DI-DATE
      *    処方箋発行日
           IF      ONS-SNR-S-HAKKOYMD  NOT =   SPACE
               MOVE    ONS-SNR-S-HAKKOYMD  TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MEDRES2-PR-DATE
           END-IF
           MOVE    ONS-SNR-M-NYUGAIKBN TO  MEDRES2-NYUGAIKBN
      *医療機関・薬局コード
           MOVE    ONS-SNR-M-HOSPCD    TO  MEDRES2-HOSPCD
      *医療機関コード
           MOVE    ONS-SNR-M-HOSPNAME  TO  MEDRES2-HOSPNAME
      *調剤薬局コード
           MOVE    ONS-SNR-M-CHOZAICD  TO  MEDRES2-CHOZAICD
      *調剤薬局名
           MOVE    ONS-SNR-M-CHOZAINAME    TO  MEDRES2-CHOZAINAME
           .
       200211-ONSHI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報資格確認サブ編集処理 
      *****************************************************************
       200211-ONSHI-MEISEI-SEC         SECTION.
      *
           ADD     1                   TO  IDZ
           IF      IDZ                 >   200
               GO  TO  200211-ONSHI-MEISEI-EXT
           END-IF
      *
      *    診療識別等区分
           MOVE    ONS-SNR-S-SRYKBN    TO  MEDRES2-SRYKBN
                                                   (IDZ)
      *    診療行為コード
           MOVE    ONS-SNR-S-SRYCD     TO  MEDRES2-SRYCD
                                                    (IDZ)
      *    診療行為名
           MOVE    ONS-SNR-S-SRYNAME   TO  MEDRES2-SRYNAME
                                                    (IDZ)
      *    単位
           MOVE    ONS-SNR-S-TANINAME  TO  MEDRES2-TANINAME
                                                    (IDZ)
      *    使用量
           MOVE    ONS-SNR-S-SURYO     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO MEDRES2-SURYO
                                                    (IDZ)
      *    数量２
           INITIALIZE                  ORCSNUMAREA
           MOVE    ONS-SNR-S-SURYO2    TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MEDRES2-SURYO2
                                                    (IDZ)
      *    回数
           INITIALIZE                  ORCSNUMAREA
           MOVE    ONS-SNR-S-KAISU     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MEDRES2-KAISU
                                                    (IDZ)
           .
       200211-ONSHI-MEISEI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    XML情報OPEN
      *****************************************************************
       1000-RGTXMLOPEN-SEC         SECTION.
      *
           IF      APILST51-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-ONLINEMEDGETRES2
      *
           MOVE    "MSGOPEN"           TO  MCP-FUNC
           MOVE    "xml_onlinemedgetv2res2"
                                       TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           MOVE    2                   TO  MEDRES2-MODE
           MOVE    APILST51-BODY       TO  MEDRES2-OBJECT
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEMEDGETRES2
           IF     (MCP-RC              =   ZERO  OR  1  )
               DISPLAY "MSGOPEN OK = " MCP-RC
           ELSE
               DISPLAY "MSGOPEN ERR failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLOPEN-EXT.
           EXIT.
      *****************************************************************
      *    XML情報書き込み
      *****************************************************************
       2000-RGTXMLWRITE-SEC         SECTION.
      *
           ADD     1                   TO  WRK-MSGCNT
      *
           MOVE    "MSGWRITE"          TO  MCP-FUNC
           MOVE    "xml_onlinemedgetv2res2"
                                       TO  MCP-TABLE
           MOVE    2                   TO  MEDRES2-MODE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEMEDGETRES2
           IF     (MCP-RC              =   ZERO  OR  1  )
               DISPLAY "MSGWRITE OK = " MCP-RC
           ELSE
               DISPLAY "MSGWRITE ERR failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
           .
       2000-RGTXMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報CLOSE
      *****************************************************************
       3000-RGTXMLCLOSE-SEC         SECTION.
      *
           MOVE    "MSGCLOSE"          TO  MCP-FUNC
           MOVE    "xml_onlinemedgetv2res2"
                                       TO  MCP-TABLE
           MOVE    2                   TO  MEDRES2-MODE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEMEDGETRES2
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MEDRES2-OBJECT    TO  APILST51-BODY
      **         MOVE  "application/xml" TO  APILST51-CONTENT-TYPE
           ELSE
               DISPLAY "MSGCLOSE ERR failure = " MCP-RC
           END-IF
      *
           .
       3000-RGTXMLCLOSE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    資格確認　歯科診療情報編集処理
      *****************************************************************
       2003-SIKAKU-SHIKA-SYORI-SEC      SECTION.
      *
      *    XMLOPEN
           PERFORM 1000-RGTXML03OPEN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
              GO  TO  2003-SIKAKU-SHIKA-SYORI-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-MSGWRITE
      *    患者情報等編集
           PERFORM 20021-PTINF-HEN03-SEC
      *
      *    薬剤情報メイン検索
           INITIALIZE                  ONSHI-SHINRYO-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SNR-M-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-SNR-M-PTID
      *    診療年月
           MOVE    WRK-SRYYM           TO  ONS-SNR-M-SRYYM
      *    医療機関・薬局コード
           MOVE    MEDREQ1-HOSPCD      TO  ONS-SNR-M-HOSPCD
      *
           MOVE    "key5"              TO  WRK-MCP-PATHNAME
      *    入外等区分
           IF      MEDREQ1-NYUGAIKBN   NOT =   SPACE
               MOVE    MEDREQ1-NYUGAIKBN
                                           TO  ONS-SNR-M-NYUGAIKBN
               MOVE    "key6"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    ONSHI-SHINRYO-MAIN-REC  TO  MCPDATA-REC
           MOVE    "tbl_onshi_shinryo_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-MAIN
           END-IF
           MOVE    ZERO                TO  WRK-MSGCNT
           PERFORM UNTIL     FLG-ONSHI-MAIN    =   1
      *
      *        医療機関コード判定
               MOVE    ZERO                TO  FLG-NASI
      *        医療機関・調剤コード、処方箋発行機関コード判定
               IF     (MEDREQ1-CHOZAICD    NOT =   SPACE )
                   IF     (MEDREQ1-CHOZAICD    =   ONS-SNR-M-CHOZAICD)
                      MOVE    ZERO             TO  FLG-NASI
                   ELSE
                      MOVE    1                TO  FLG-NASI
                   END-IF
               END-IF
      *
               IF      FLG-NASI            =   ZERO
                   PERFORM 20031-ONSHI-SUB-SYORI03-SEC
                END-IF
      *
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_shinryo_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-MSGCNT             >   ZERO
               CONTINUE
           ELSE
      *        対象なし
               MOVE    "50"                TO  WRK-ERRCD
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               IF      WRK-ERRCD1          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRCD1
               END-IF
               MOVE    WRK-ERRCD-G         TO  MEDRES3-API-RESULT
               MOVE    WRK-ERRMSG          TO  MEDRES3-API-RESULT-MSG
               MOVE    MEDREQ1-NYUGAIKBN   TO  MEDRES3-NYUGAIKBN
               MOVE    MEDREQ1-HOSPCD      TO  MEDRES3-HOSPCD
               MOVE    MEDREQ1-CHOZAICD    TO  MEDRES3-CHOZAICD
               PERFORM 2000-RGTXML03WRITE-SEC
           END-IF
      *
      *    XML情報CLOSE
           PERFORM 3000-RGTXML03CLOSE-SEC
           .
       2003-SIKAKU-SHIKA-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報内容編集処理
      *****************************************************************
       20021-PTINF-HEN03-SEC        SECTION.
      *
           MOVE    "000"               TO  MEDRES3-API-RESULT
           MOVE    "処理終了"          TO  MEDRES3-API-RESULT-MSG
      *
           MOVE    MEDRES1-INFORMATION-DATE
                                       TO  MEDRES3-INFORMATION-DATE
           MOVE    MEDRES1-INFORMATION-TIME
                                       TO  MEDRES3-INFORMATION-TIME
      *
           MOVE    MEDRES1-PATIENTID   TO  MEDRES3-PATIENTID
           MOVE    MEDRES1-NAME        TO  MEDRES3-NAME
           MOVE    MEDRES1-KANANAME    TO  MEDRES3-KANANAME
           MOVE    MEDRES1-BIRTHDAY    TO  MEDRES3-BIRTHDAY
           MOVE    MEDRES1-SEX         TO  MEDRES3-SEX
           .
       20021-PTINF-HEN03-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *     診療情報資格サブ編集処理
      *****************************************************************
       20031-ONSHI-SUB-SYORI03-SEC       SECTION.
      *
      *    診療情報サブ検索
           INITIALIZE                  ONSHI-SHINRYO-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SNR-S-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-SNR-S-PTID
      *    UID
           MOVE    ONS-SNR-M-TBL-UUID  TO  ONS-SNR-S-TBL-UUID
      *    診療年月
           MOVE    ONS-SNR-M-SRYYM     TO  ONS-SNR-S-SRYYM
      *    入外区分
           MOVE    ONS-SNR-M-NYUGAIKBN
                                       TO  ONS-SNR-S-NYUGAIKBN
      *
           MOVE    ONSHI-SHINRYO-SUB-REC   TO  MCPDATA-REC
           MOVE    "tbl_onshi_shinryo_sub"
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
      **** MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDZ
           MOVE    SPACE               TO  WRK-DAY
           MOVE    SPACE               TO  WRK-HAKKOYMD
           PERFORM UNTIL   (FLG-ONSHI-SUB  =   1  )
      *        診療日毎に編集
               IF     (WRK-DAY         =   ONS-SNR-S-SRYDD)
                  AND (WRK-HAKKOYMD    =   ONS-SNR-S-HAKKOYMD)
                   CONTINUE
               ELSE
      *            診療日毎に出力
                   IF      IDZ                 >   ZERO
                       PERFORM 2000-RGTXML03WRITE-SEC
                   END-IF
      *            診療ヘッド
                   PERFORM 200211-ONSHI-SIKA-DAY-SEC
      *
                   MOVE    ONS-SNR-S-SRYDD     TO  WRK-DAY
                   MOVE    ONS-SNR-S-HAKKOYMD  TO  WRK-HAKKOYMD
                   MOVE    ZERO                TO  IDZ
               END-IF
               IF      WRK-ERRCD3          =   SPACE
      *            診療明細
                   PERFORM 200211-ONSHI-SIKAMEISEI-SEC
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-ONSHISUB-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_onshi_shinryo_sub"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療日毎に出力
           IF      IDZ                 >   ZERO
               PERFORM 2000-RGTXML03WRITE-SEC
           END-IF
           .
       20031-ONSHI-SUB-SYORI03-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報　歯科　編集処理 
      *****************************************************************
       200211-ONSHI-SIKA-DAY-SEC            SECTION.
      *
      *****
      *    最初のみ保険情報
           IF      WRK-MSGCNT          =   ZERO
               MOVE    ONS-SNR-M-HKNJANUM
                                       TO  MEDRES3-HKNJANUM
               MOVE    ONS-SNR-M-KIGO
                                       TO  MEDRES3-KIGO
               MOVE    ONS-SNR-M-NUM
                                       TO  MEDRES3-NUM
               MOVE    ONS-SNR-M-EDABAN
                                       TO  MEDRES3-EDABAN
           END-IF
      *****
      *
           MOVE    SPACE               TO  MEDRES3-DATA02-AREA
           INITIALIZE                  MEDRES3-DATA02-AREA
      *    診療日
           MOVE    ONS-SNR-S-SRYYM     TO  WRK-MED-SRYYM
           MOVE    ONS-SNR-S-SRYDD     TO  WRK-MED-SRYDD
           MOVE    WRK-MED-SRYYMD      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MEDRES3-DI-DATE
      *    処方箋発行日
           IF      ONS-SNR-S-HAKKOYMD  NOT =   SPACE
               MOVE    ONS-SNR-S-HAKKOYMD  TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  MEDRES3-PR-DATE
           END-IF
           MOVE    ONS-SNR-M-NYUGAIKBN TO  MEDRES3-NYUGAIKBN
      *医療機関コード
           MOVE    ONS-SNR-M-HOSPCD    TO  MEDRES3-HOSPCD
      *医療機関コード
           MOVE    ONS-SNR-M-HOSPNAME  TO  MEDRES3-HOSPNAME
      *調剤薬局コード
           MOVE    ONS-SNR-M-CHOZAICD  TO  MEDRES3-CHOZAICD
      *調剤薬局名
           MOVE    ONS-SNR-M-CHOZAINAME    TO  MEDRES3-CHOZAINAME
           .
       200211-ONSHI-SIKA-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報資格確認サブ編集処理 
      *****************************************************************
       200211-ONSHI-SIKAMEISEI-SEC    SECTION.
      *
           ADD     1                   TO  IDZ
           IF      IDZ                 >   100
               GO  TO  200211-ONSHI-SIKAMEISEI-EXT
           END-IF
      *
      *    診療識別等区分
           MOVE    ONS-SNR-S-SRYKBN    TO  MEDRES3-SRYKBN
                                                   (IDZ)
      *    診療行為コード
           MOVE    ONS-SNR-S-SRYCD     TO  MEDRES3-SRYCD
                                                    (IDZ)
      *    診療行為名
           MOVE    ONS-SNR-S-SRYNAME   TO  MEDRES3-SRYNAME
                                                    (IDZ)
      *    単位
           MOVE    ONS-SNR-S-TANINAME  TO  MEDRES3-TANINAME
                                                    (IDZ)
      *    使用量
           MOVE    ONS-SNR-S-SURYO     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO MEDRES3-SURYO
                                                    (IDZ)
      *    数量２
           INITIALIZE                  ORCSNUMAREA
           MOVE    ONS-SNR-S-SURYO2    TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MEDRES3-SURYO2
                                                    (IDZ)
      *    回数
           INITIALIZE                  ORCSNUMAREA
           MOVE    ONS-SNR-S-KAISU     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MEDRES3-KAISU
                                                    (IDZ)
      *
      *    歯科加算区分
           IF      ONS-SNR-SHIKA-KBN    =   "1"
      *         歯科情報編集
                PERFORM 200291-SHIKA-HEN-SEC
           END-IF
           .
       200211-ONSHI-SIKAMEISEI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    歯科情報編集処理
      *****************************************************************
       200291-SHIKA-HEN-SEC             SECTION.
      *
      *    歯科加算情報サブ検索
           INITIALIZE                  ONSHI-SHIKA-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-SIK-S-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-SIK-S-PTID
      *    UID
           MOVE    ONS-SNR-M-TBL-UUID  TO  ONS-SIK-S-TBL-UUID
      *    診療年月
           MOVE    ONS-SNR-M-SRYYM     TO  ONS-SIK-S-SRYYM
      *    入外区分
           MOVE    ONS-SNR-M-NYUGAIKBN
                                       TO  ONS-SIK-S-NYUGAIKBN
      *    診療日
           MOVE    ONS-SNR-S-SRYDD
                                       TO  ONS-SIK-S-SRYDD
      *    処方箋発行日
           MOVE    ONS-SNR-S-HAKKOYMD
                                       TO  ONS-SIK-S-HAKKOYMD
      *    連番
           MOVE    ONS-SNR-S-RENNUM
                                       TO  ONS-SIK-S-RENNUM
      *
           MOVE    ONSHI-SHIKA-SUB-REC     TO  MCPDATA-REC
           MOVE    "tbl_onshi_shika_sub"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-ONSHISIKASUB-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHIKA-SUB
           END-IF
      *
           MOVE    ZERO                TO  IDS
           MOVE    SPACE               TO  WRK-ERRCD5
           PERFORM UNTIL   (FLG-ONSHIKA-SUB    =   1  )
                        OR (WRK-ERRCD5       NOT =   SPACE )
               ADD     1                   TO  IDS
               IF      IDS                 >   30
                   MOVE    "54"                TO  WRK-ERRCD5
               END-IF
      *
               IF      WRK-ERRCD5            =   SPACE
      *            歯科情報
                   PERFORM 2002911-ONSHI-SHIKA-MEI-SEC
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-ONSHISIKASUB-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_onshi_shika_sub"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (WRK-ERRCD5      NOT =   SPACE)
              AND (WRK-ERRCD4          =   SPACE)
               MOVE    WRK-ERRCD5          TO  WRK-ERRCD4
           END-IF
           .
       200291-SHIKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    歯科情報編集処理
      *****************************************************************
       2002911-ONSHI-SHIKA-MEI-SEC             SECTION.
      *
      *    診療行為加算コード
           MOVE    ONS-SIK-S-SRYKASANCD    TO  MEDRES3-SRYKASANCD
                                                  (IDZ IDS)
      *    診療行為加算名
           MOVE    ONS-SIK-S-SRYKASAN-NAME TO  MEDRES3-SRYKASAN-NAME
                                                  (IDZ IDS)
      *    単位
           MOVE    ONS-SIK-S-TANINAME      TO  MEDRES3-SHIKA-TANINAME
                                                  (IDZ IDS)
      *    使用量
           MOVE    ONS-SIK-S-SURYO     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MEDRES3-SHIKA-SURYO
                                                  (IDZ IDS)
           .
       2002911-ONSHI-SHIKA-MEI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    XML情報OPEN
      *****************************************************************
       1000-RGTXML03OPEN-SEC         SECTION.
      *
           IF      APILST51-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-ONLINEMEDGETRES3
      *
           MOVE    "MSGOPEN"           TO  MCP-FUNC
           MOVE    "xml_onlinemedgetv2res3"
                                       TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           MOVE    2                   TO  MEDRES3-MODE
           MOVE    APILST51-BODY       TO  MEDRES3-OBJECT
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEMEDGETRES3
           IF     (MCP-RC              =   ZERO  OR  1  )
               DISPLAY "MSGOPEN OK = " MCP-RC
           ELSE
               DISPLAY "MSGOPEN ERR failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLOPEN-EXT.
           EXIT.
      *****************************************************************
      *    XML情報書き込み
      *****************************************************************
       2000-RGTXML03WRITE-SEC         SECTION.
      *
           ADD     1                   TO  WRK-MSGCNT
      *
           MOVE    "MSGWRITE"          TO  MCP-FUNC
           MOVE    "xml_onlinemedgetv2res3"
                                       TO  MCP-TABLE
           MOVE    2                   TO  MEDRES3-MODE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEMEDGETRES3
           IF     (MCP-RC              =   ZERO  OR  1  )
               DISPLAY "MSGWRITE OK = " MCP-RC
           ELSE
               DISPLAY "MSGWRITE ERR failure = " MCP-RC
               MOVE   "98"             TO  WRK-ERRCD
           END-IF
           .
       2000-RGTXML03WRITE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報CLOSE
      *****************************************************************
       3000-RGTXML03CLOSE-SEC         SECTION.
      *
           MOVE    "MSGCLOSE"          TO  MCP-FUNC
           MOVE    "xml_onlinemedgetv2res3"
                                       TO  MCP-TABLE
           MOVE    2                   TO  MEDRES3-MODE
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-ONLINEMEDGETRES3
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE  MEDRES3-OBJECT    TO  APILST51-BODY
      **         MOVE  "application/xml" TO  APILST51-CONTENT-TYPE
           ELSE
               DISPLAY "MSGCLOSE ERR failure = " MCP-RC
           END-IF
      *
           .
       3000-RGTXML03CLOSE-EXT.
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
           WHEN    "02"
               MOVE    "確認年月の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "患者番号に該当する患者が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "11"
               MOVE    "確認年月が暦日ではありません"
                                               TO  WRK-ERRMSG
           WHEN    "12"
               MOVE    "入外等区分が存在しません"
                                               TO  WRK-ERRMSG
           WHEN    "13"
               MOVE    "入外等区分の設定がありません"
                                               TO  WRK-ERRMSG
           WHEN    "14"
               STRING  "医療機関・薬局コードの設定がありません"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
               END-STRING
           WHEN    "50"
               MOVE    "対象がありませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE   "対象が返却最大数を超えています。"
                                               TO  WRK-ERRMSG
           WHEN    "53"
               MOVE   "診療情報の対象が返却最大数を超えています。"
                                               TO  WRK-ERRMSG
           WHEN    "54"
               MOVE   "歯科情報の対象が返却最大数を超えています。"
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
      *    資格確認薬剤情報メイン読込
      ******************************************************************
       910-ONSHIMAIN-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_shinryo_main"    TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-SHINRYO-MAIN-REC
               MOVE    ZERO                TO  FLG-ONSHI-MAIN
           ELSE
               INITIALIZE                      ONSHI-SHINRYO-MAIN-REC
               MOVE    1                   TO  FLG-ONSHI-MAIN
           END-IF
      *
           .
       910-ONSHIMAIN-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認薬剤情報サブ読込
      ******************************************************************
       920-ONSHISUB-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_shinryo_sub"    TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-SHINRYO-SUB-REC
               MOVE    ZERO                TO  FLG-ONSHI-SUB
           ELSE
               INITIALIZE                      ONSHI-SHINRYO-SUB-REC
               MOVE    1                   TO  FLG-ONSHI-SUB
           END-IF
      *
           .
       920-ONSHISUB-READ-EXT.
           EXIT.
           
      ******************************************************************
      *    資格確認歯科情報サブ読込
      ******************************************************************
       920-ONSHISIKASUB-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_shika_sub"    TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-SHIKA-SUB-REC
               MOVE    ZERO                TO  FLG-ONSHIKA-SUB
           ELSE
               INITIALIZE                      ONSHI-SHIKA-SUB-REC
               MOVE    1                   TO  FLG-ONSHIKA-SUB
           END-IF
      *
           .
       920-ONSHISIKASUB-READ-EXT.
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

