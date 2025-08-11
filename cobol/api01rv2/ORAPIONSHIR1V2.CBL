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
       PROGRAM-ID.         ORAPIONSHIR1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : API 資格確認薬剤情報取得処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/01/26    NMED−多々納  新規作成
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
           03  FLG-ONSHI-MAIN          PIC 9(01).
           03  FLG-ONSHI-SUB           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-ONSKAKU             PIC 9(01).
      *
           03  FLG-SP                  PIC 9(01).
           03  FLG-NASI                PIC 9(01).
      *
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXM                    PIC 9(04).
           03  IDXR                    PIC 9(04).
           03  IDXS                    PIC 9(04).
           03  IDR                     PIC 9(04).
           03  IDD                     PIC 9(02).
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
           03  WRK-ERRCD3              PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(30).
      *
           03  WRK-DAY                 PIC X(02).
           03  WRK-HAKKOYMD            PIC X(08).
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
       01  WRK-DRGRES1-REC.
           03  WRK-DRGRES1-MEISA-AREA.
               05  WRK-DRGRES1-MEI-TBL           OCCURS   3.
      *        入外等の別
                   07  WRK-DRGRES1-NYUGAIKBN       PIC X(01).
      *
      *調剤日別情報
                   07  WRK-DRGRES1-DI-DATE-OCC     OCCURS   100.
      *処理済み区分
                       09  WRK-DRGRES1-CHK         PIC X(01).
      *調剤日
                       09  WRK-DRGRES1-DI-DAY      PIC X(02).
      *処方箋発行日
                       09  WRK-DRGRES1-PR-DATE     PIC X(10).
      *                医療機関コード
                       09  WRK-DRGRES1-HOSPCD      PIC  X(10).
      *                医療機関名
                       09  WRK-DRGRES1-HOSPNAME    PIC  X(80).
      *                調剤薬局コード
                       09  WRK-DRGRES1-CHOZAICD    PIC  X(10).
      *                調剤薬局名
                       09  WRK-DRGRES1-CHOZAINAME  PIC  X(80).
      *薬剤情報群
                       09  WRK-DRGRES1-PR-DATA-INFG.
      *薬剤情報
                           11  WRK-DRGRES1-PR-DATA-INF
      ******                                             OCCURS  100.
                                                         OCCURS  60.
      *診療識別等区分
                                       17  WRK-DRGRES1-SRYKBN
                                                          PIC X(02).
      *用法コード
                                       17  WRK-DRGRES1-YOHOCD
                                                         PIC X(03).
      *用法名称
                                       17  WRK-DRGRES1-YOHONAME
                                                            PIC X(30).
      *特別指示
                                       17  WRK-DRGRES1-SHIJI  PIC X(80).
      *医薬品コード
                                       17  WRK-DRGRES1-SRYCD  PIC X(09).
      *薬剤名
                                       17  WRK-DRGRES1-YAKUZAINAME
                                                            PIC X(64).
      *成分名
                                       17  WRK-DRGRES1-SEIBUNNAME
                                                            PIC X(200).
      *単位
                                       17  WRK-DRGRES1-TANINAME
                                                            PIC X(12).
      *使用量
                                       17  WRK-DRGRES1-SURYO
                                                           PIC X(11).
      *1回用量
                                       17  WRK-DRGRES1-YORYO
                                                            PIC X(11).
      *回数
                                       17  WRK-DRGRES1-KAISU 
                                                           PIC X(03).
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
           COPY    "CPONLINEDRUGGETREQ1.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEDRUGGETRES1.INC".
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
           DISPLAY   "*  onlinedruggetreq start  *"
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
               MOVE   404                  TO  APILST48-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "****************************"
           DISPLAY   "*   onlinedruggetreq end   *"
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
           INITIALIZE                      XML-ONLINEDRUGGETRES1
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
           MOVE    WRK-HEN-DATE        TO  DRGRES1-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  DRGRES1-INFORMATION-TIME
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
               EVALUATE    DRGREQ1-REQUEST-NUMBER
               WHEN    "01"
                   PERFORM 2002-SIKAKU-SYORI01-SEC
               WHEN    "02"
                   PERFORM 2002-SIKAKU-YAKUZAI-SYORI-SEC
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
           IF      APILST48-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEDRUGGETREQ1
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinedruggetv2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST48-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinedruggetreq"  TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEDRUGGETREQ1
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
               MOVE    "000"               TO  DRGRES1-API-RESULT
               MOVE    "処理終了"          TO  DRGRES1-API-RESULT-MSG
           ELSE
      *        エラー・警告メッセージ
               PERFORM 890-ERRCD-MSG-SEC
               IF      WRK-ERRCD1          =   SPACE
                   MOVE    "E"                 TO  WRK-ERRCD1
               END-IF
               MOVE    WRK-ERRCD-G         TO  DRGRES1-API-RESULT
               MOVE    WRK-ERRMSG          TO  DRGRES1-API-RESULT-MSG
           END-IF
      *
      *
           IF      APILST48-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinedruggetv2res" TO  MCP-TABLE
           MOVE    1                   TO  DRGRES1-MODE
           MOVE    "onlinedruggetres"  TO  DRGRES1-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINEDRUGGETRES1
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  DRGRES1-OBJECT    TO  APILST48-BODY
               MOVE  "application/xml" TO  APILST48-CONTENT-TYPE
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
           MOVE    DRGREQ1-PATIENTID       TO  DRGRES1-PATIENTID
           MOVE    DRGREQ1-PATIENTID       TO  WRK-PTNUM
      *    基準日
           MOVE    DRGREQ1-SRYYM           TO  DRGRES1-SRYYM
      *    数字のみに編集
           IF     (DRGREQ1-SRYYM       NOT =   SPACE )
               MOVE    DRGREQ1-SRYYM           TO  WRK-HEN-DATE
               MOVE    SPACE                   TO  WRK-HEN-TIME
               PERFORM 802-DAYHEN02-SEC
               MOVE    WRK-SYMD                TO  WRK-SRYYMD
           ELSE
               MOVE    SPACE                   TO  WRK-SRYYMD
           END-IF
      *    入外等区分
           MOVE    DRGREQ1-NYUGAIKBN   TO  DRGRES1-NYUGAIKBN (1)
      *
      *    必須入力チェック
      *    患者番号
           IF     (WRK-PTNUM           =   SPACE )
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *
      *    確認年月
           IF      DRGREQ1-REQUEST-NUMBER  =   "02"
               IF     (DRGREQ1-SRYYM       =   SPACE )
                   MOVE    "02"            TO  WRK-ERRCD
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
           MOVE    SPTID-PTNUM         TO  DRGRES1-PATIENTID
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
           MOVE    PTINF-NAME          TO  DRGRES1-NAME
           MOVE    PTINF-KANANAME      TO  DRGRES1-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  DRGRES1-BIRTHDAY
           MOVE    PTINF-SEX           TO  DRGRES1-SEX
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
           EVALUATE    DRGREQ1-NYUGAIKBN
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
           INITIALIZE                  DRGRES1-DATA01-AREA
           INITIALIZE                  DRGRES1-DATA02-AREA
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
      *    薬剤情報メイン検索
           INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-YKZ-M-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-YKZ-M-PTID
      *    基準年月
           MOVE    WRK-STRYM           TO  ONS-YKZ-M-SRYYM
      *
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
      *
           MOVE    ONSHI-YAKUZAI-MAIN-REC  TO  MCPDATA-REC
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-MAIN
           END-IF
      *     基準日 から ６か月分を検索する
           MOVE    ZERO                TO  IDX
           MOVE    SPACE               TO  WRK-CHK-SRYYM
           PERFORM UNTIL   ( FLG-ONSHI-MAIN    =   1 )
                      OR   ( IDX               >=  6 )
      *
               IF      (WRK-CHK-SRYYM       =   ONS-YKZ-M-SRYYM)
                   CONTINUE
               ELSE
                   ADD     1                TO  IDX
                   MOVE    ONS-YKZ-M-SRYYM  TO  WRK-SYMD(1:6)
                   MOVE    SPACE            TO  WRK-THMS
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE(1:7)
                                            TO  DRGRES1-TOR-SRYYM
                                                      (IDX)
               END-IF
      *        医療機関コード・調剤薬局コード
               PERFORM VARYING     IDY      FROM    1   BY  1
                       UNTIL     ( IDY      >   20 )
                   IF     (DRGRES1-TOR-HOSPCD (IDX IDY)
                                            =   ONS-YKZ-M-HOSPCD )
                     AND  (DRGRES1-TOR-CHOZAICD (IDX IDY)
                                            =   ONS-YKZ-M-CHOZAICD)
                       MOVE    20               TO  IDY
                   ELSE
                       IF     (DRGRES1-TOR-HOSPCD (IDX IDY)
                                                =   SPACE )
                         AND  (DRGRES1-TOR-CHOZAICD (IDX IDY)
                                                =   SPACE )
                           MOVE    ONS-YKZ-M-HOSPCD    TO
                                         DRGRES1-TOR-HOSPCD (IDX IDY)
                           MOVE    ONS-YKZ-M-HOSPNAME  TO
                                         DRGRES1-TOR-HOSPNAME(IDX IDY)
                           MOVE    ONS-YKZ-M-CHOZAICD    TO
                                         DRGRES1-TOR-CHOZAICD (IDX IDY)
                           MOVE    ONS-YKZ-M-CHOZAINAME TO
                                         DRGRES1-TOR-CHOZAINAME
                                                            (IDX IDY)
                           MOVE    20            TO  IDY
                        END-IF
                   END-IF
      *
               END-PERFORM
      *
               MOVE    ONS-YKZ-M-SRYYM      TO  WRK-CHK-SRYYM
      *
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_yakuzai_main"
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
      *    資格確認薬剤情報編集処理
      *****************************************************************
       2002-SIKAKU-YAKUZAI-SYORI-SEC      SECTION.
      *
           INITIALIZE                  WRK-DRGRES1-REC
           INITIALIZE                  DRGRES1-DATA02-AREA
      *
      *    薬剤情報メイン検索
           INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
           MOVE    SPA-HOSPNUM         TO  ONS-YKZ-M-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-YKZ-M-PTID
      *    診療年月
           MOVE    WRK-SRYYM           TO  ONS-YKZ-M-SRYYM
      *
           MOVE    "key3"              TO  WRK-MCP-PATHNAME
      *    入外等区分
           IF      DRGREQ1-NYUGAIKBN   NOT =   SPACE
               MOVE    DRGREQ1-NYUGAIKBN
                                           TO  ONS-YKZ-M-NYUGAIKBN
      *2022/08 MOVE    "key"               TO  WRK-MCP-PATHNAME
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    ONSHI-YAKUZAI-MAIN-REC  TO  MCPDATA-REC
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-MAIN
           END-IF
           MOVE    ZERO                TO  IDX-MAX
           PERFORM UNTIL     FLG-ONSHI-MAIN    =   1
      *
      *        医療機関コード判定
               MOVE    ZERO                TO  FLG-NASI
               IF     (DRGREQ1-HOSPCD      NOT =   SPACE )
                 AND  (DRGREQ1-HOSPCD      NOT =   ONS-YKZ-M-HOSPCD )
                   MOVE    1               TO  FLG-NASI
               END-IF
      *        調剤薬局コード
               IF     (DRGREQ1-CHOZAICD    NOT =   SPACE )
                 AND  (DRGREQ1-CHOZAICD    NOT =   ONS-YKZ-M-CHOZAICD)
                   MOVE    1               TO  FLG-NASI
               END-IF
      *
               IF      FLG-NASI            =   ZERO
                   PERFORM 20021-ONSHI-MEIN-HEN-SEC
                   PERFORM 20021-ONSHI-SUB-SYORI-SEC
                END-IF
      *
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-ONSHIMAIN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-MAX             >   ZERO
               MOVE    IDX-MAX             TO  IDX
               PERFORM 20029-DRGRES1-DATA02-SEC
           ELSE
      *        対象なし
               MOVE    "50"            TO  WRK-ERRCD
      *        入外等区分
               MOVE    DRGREQ1-NYUGAIKBN   TO  DRGRES1-NYUGAIKBN (1)
               MOVE    DRGREQ1-HOSPCD      TO  DRGRES1-HOSPCD (1 1)
               MOVE    DRGREQ1-CHOZAICD    TO  DRGRES1-CHOZAICD (1 1)
           END-IF
           .
       2002-SIKAKU-YAKUZAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤情報資格メイン編集処理
      *****************************************************************
       20021-ONSHI-MEIN-HEN-SEC       SECTION.
      *
           IF      IDX-MAX             =   ZERO
      *        最初の保険情報
               MOVE    ONS-YKZ-M-HKNJANUM
                                       TO  DRGRES1-HKNJANUM
               MOVE    ONS-YKZ-M-KIGO
                                       TO  DRGRES1-KIGO
               MOVE    ONS-YKZ-M-NUM
                                       TO  DRGRES1-NUM
               MOVE    ONS-YKZ-M-EDABAN
                                       TO  DRGRES1-EDABAN
           END-IF
      *
      *    入外区分順に検索する
           IF     (IDX-MAX             =   ZERO )
             OR  ((IDX-MAX             >   ZERO )
             AND  (DRGRES1-NYUGAIKBN (IDX-MAX)
                                   NOT =   ONS-YKZ-M-NYUGAIKBN ))
      *        入外区分毎に編集を行う
               IF      IDX-MAX             >   ZERO
                   MOVE    IDX-MAX             TO  IDX
                   PERFORM 20029-DRGRES1-DATA02-SEC
                   IF      IDX-MAX             >=  3
                       MOVE    "52"                TO  WRK-ERR2
                       GO  TO  20021-ONSHI-MEIN-HEN-EXT
                   END-IF
               END-IF
               ADD     1                   TO  IDX-MAX
               MOVE    IDX-MAX             TO  IDX
               MOVE    ONS-YKZ-M-NYUGAIKBN TO  DRGRES1-NYUGAIKBN (IDX)
      *        明細添字クリア
               MOVE    ZERO                TO  IDY
           END-IF
           MOVE    IDX-MAX             TO  IDX
      *
           .
       20021-ONSHI-MEIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤情報資格サブ編集処理
      *****************************************************************
       20021-ONSHI-SUB-SYORI-SEC       SECTION.
      *
      *    薬剤情報サブ検索
           INITIALIZE                  ONSHI-YAKUZAI-SUB-REC
           MOVE    SPA-HOSPNUM         TO  ONS-YKZ-S-HOSPNUM
           MOVE    SPA-PTID            TO  ONS-YKZ-S-PTID
      *    UID
           MOVE    ONS-YKZ-M-TBL-UUID  TO  ONS-YKZ-S-TBL-UUID
      *    診療年月
           MOVE    ONS-YKZ-M-SRYYM     TO  ONS-YKZ-S-SRYYM
      *    入外区分
           MOVE    ONS-YKZ-M-NYUGAIKBN
                                       TO  ONS-YKZ-S-NYUGAIKBN
      *
           MOVE    ONSHI-YAKUZAI-SUB-REC   TO  MCPDATA-REC
           MOVE    "tbl_onshi_yakuzai_sub"
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
                        OR (WRK-ERRCD3     NOT =   SPACE )
      *        診療日毎に編集
               IF     (WRK-DAY         =   ONS-YKZ-S-SRYDD)
                  AND (WRK-HAKKOYMD    =   ONS-YKZ-S-HAKKOYMD)
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDY
      *****        IF      IDY                 >   31
                   IF      IDY                 >   100
                       MOVE    "52"                TO  WRK-ERRCD3
                   END-IF
                   IF      WRK-ERRCD3          =   SPACE
      *                薬剤ヘッド
                       PERFORM 200211-ONSHI-DAY-SEC
                   END-IF
      *
                   MOVE    ONS-YKZ-S-SRYDD     TO  WRK-DAY
                   MOVE    ONS-YKZ-S-HAKKOYMD  TO  WRK-HAKKOYMD
                   MOVE    ZERO                TO  IDZ
               END-IF
               IF      WRK-ERRCD3          =   SPACE
      *            薬剤明細
                   PERFORM 200211-ONSHI-MEISEI-SEC
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-ONSHISUB-READ-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_onshi_yakuzai_sub"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    ( WRK-ERRCD3      NOT =   SPACE )
             AND ( WRK-ERR2            =   SPACE )
               MOVE    WRK-ERRCD3          TO  WRK-ERR2
           END-IF
           .
       20021-ONSHI-SUB-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報資格確認　編集処理 
      *****************************************************************
       200211-ONSHI-DAY-SEC  SECTION.
      *
      *    診療日
           MOVE    ONS-YKZ-S-SRYDD     TO  WRK-DRGRES1-DI-DAY
                                                       (IDX IDY)
      *    処方箋発行日
           IF      ONS-YKZ-S-HAKKOYMD  NOT =   SPACE
               MOVE    ONS-YKZ-S-HAKKOYMD  TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-DRGRES1-PR-DATE
                                                      (IDX IDY)
           END-IF
      *****
      *医療機関コード
           MOVE    ONS-YKZ-M-HOSPCD    TO  WRK-DRGRES1-HOSPCD
                                              (IDX IDY)
      *医療機関コード
           MOVE    ONS-YKZ-M-HOSPNAME  TO  WRK-DRGRES1-HOSPNAME
                                              (IDX IDY)
      *調剤薬局コード
           MOVE    ONS-YKZ-M-CHOZAICD  TO  WRK-DRGRES1-CHOZAICD
                                              (IDX IDY)
      *調剤薬局名
           MOVE    ONS-YKZ-M-CHOZAINAME    TO  WRK-DRGRES1-CHOZAINAME
                                              (IDX IDY)
           .
       200211-ONSHI-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報資格確認サブ編集処理 
      *****************************************************************
       200211-ONSHI-MEISEI-SEC  SECTION.
      *
           ADD     1                   TO  IDZ
      **** IF      IDZ                 >   100
           IF      IDZ                 >   60
               GO  TO  200211-ONSHI-MEISEI-EXT
           END-IF
      *
      *    診療識別等区分
           MOVE    ONS-YKZ-S-SRYKBN    TO  WRK-DRGRES1-SRYKBN
                                                  (IDX IDY IDZ)
      *    用法コード
           MOVE    ONS-YKZ-S-YOHOCD    TO  WRK-DRGRES1-YOHOCD
                                                    (IDX IDY IDZ)
      *    用法名称
           MOVE    ONS-YKZ-S-YOHONAME  TO  WRK-DRGRES1-YOHONAME
                                                    (IDX IDY IDZ)
      *    特別指示
           MOVE    ONS-YKZ-S-SHIJI     TO  WRK-DRGRES1-SHIJI
                                                    (IDX IDY IDZ)
      *    医薬品コード
           MOVE    ONS-YKZ-S-SRYCD     TO  WRK-DRGRES1-SRYCD
                                                    (IDX IDY IDZ)
      *    薬剤名
           MOVE    ONS-YKZ-S-YAKUZAINAME   TO  WRK-DRGRES1-YAKUZAINAME
                                                    (IDX IDY IDZ)
      *    成分名
           MOVE    ONS-YKZ-S-SEIBUNNAME    TO  WRK-DRGRES1-SEIBUNNAME
                                                    (IDX IDY IDZ)
      *    単位
           MOVE    ONS-YKZ-S-TANINAME  TO  WRK-DRGRES1-TANINAME
                                                    (IDX IDY IDZ)
      *    使用量
           MOVE    ONS-YKZ-S-SURYO     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO WRK-DRGRES1-SURYO
                                                    (IDX IDY IDZ)
      *   １回用量
           INITIALIZE                  ORCSNUMAREA
           MOVE    ONS-YKZ-S-YORYO     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  WRK-DRGRES1-YORYO
                                                    (IDX IDY IDZ)
      *    回数
           INITIALIZE                  ORCSNUMAREA
           MOVE    ONS-YKZ-S-KAISU     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  WRK-DRGRES1-KAISU
                                                    (IDX IDY IDZ)
           .
       200211-ONSHI-MEISEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     薬剤情報資格日付毎に編集処理
      *****************************************************************
       20029-DRGRES1-DATA02-SEC       SECTION.
      *
           MOVE    SPACE              TO  WRK-DAY
           MOVE    SPACE              TO  WRK-HAKKOYMD

           MOVE    ZERO               TO  IDXS
      *
           PERFORM VARYING     IDY    FROM    1   BY  1
                   UNTIL      (IDY        >   100     )
               MOVE    SPACE              TO  WRK-DAY
               MOVE    ZERO               TO  IDXS
               PERFORM VARYING     IDR    FROM    1   BY  1
                       UNTIL      (IDR    >   100   )
                               OR (WRK-DRGRES1-DI-DAY(IDX IDR)
                                          =   SPACE  )
                   IF     (WRK-DRGRES1-CHK (IDX IDR) =   "1" )
                       CONTINUE
                   ELSE
                       IF     (WRK-DAY         =   SPACE )
                         OR   (WRK-DRGRES1-DI-DAY(IDX IDR)
                                               <   WRK-DAY )
                           MOVE    IDR         TO  IDXS
                           MOVE    WRK-DRGRES1-DI-DAY(IDX IDR)
                                               TO  WRK-DAY
                       END-IF
                   END-IF
               END-PERFORM
               IF      IDXS                =   ZERO
      *            処理終了
                   MOVE    100             TO  IDY
               ELSE
                   PERFORM 20029-DRGRES1-DATA02-HEN-SEC
                   MOVE    "1"             TO  WRK-DRGRES1-CHK
                                                  (IDX IDXS)
               END-IF
           END-PERFORM
           .
       20029-DRGRES1-DATA02-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報編集処理
      *****************************************************************
       20029-DRGRES1-DATA02-HEN-SEC             SECTION.
      *
      *    調剤日
           MOVE    WRK-DRGRES1-DI-DAY  (IDX IDXS)
                                   TO  DRGRES1-DI-DAY (IDX IDY)
      *    処方箋発行日
           MOVE   WRK-DRGRES1-PR-DATE  (IDX IDXS)
                                   TO  DRGRES1-PR-DATE  (IDX IDY)
      *    医療機関コード
           MOVE    WRK-DRGRES1-HOSPCD   (IDX IDXS)
                                   TO  DRGRES1-HOSPCD  (IDX IDY)
      *    医療機関名
           MOVE    WRK-DRGRES1-HOSPNAME  (IDX IDXS)
                                   TO  DRGRES1-HOSPNAME  (IDX IDY)
      *    調剤薬局コード
           MOVE    WRK-DRGRES1-CHOZAICD  (IDX IDXS)
                                   TO  DRGRES1-CHOZAICD  (IDX IDY)
      *    調剤薬局名
           MOVE    WRK-DRGRES1-CHOZAINAME (IDX IDXS)
                                   TO  DRGRES1-CHOZAINAME (IDX IDY)
      *    薬剤情報群
           MOVE    WRK-DRGRES1-PR-DATA-INFG (IDX IDXS)
                                   TO  DRGRES1-PR-DATA-INFG (IDX IDY)
           .
       20029-DRGRES1-DATA02-HEN-EXT.
           EXIT.
      *
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
           WHEN    "50"
               MOVE    "対象がありませんでした"
                                               TO  WRK-ERRMSG
           WHEN    "52"
               MOVE   "対象が返却最大数を超えています。"
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
           MOVE    "tbl_onshi_yakuzai_main"    TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-YAKUZAI-MAIN-REC
               MOVE    ZERO                TO  FLG-ONSHI-MAIN
           ELSE
               INITIALIZE                      ONSHI-YAKUZAI-MAIN-REC
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
           MOVE    "tbl_onshi_yakuzai_sub"    TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-YAKUZAI-SUB-REC
               MOVE    ZERO                TO  FLG-ONSHI-SUB
           ELSE
               INITIALIZE                      ONSHI-YAKUZAI-SUB-REC
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

