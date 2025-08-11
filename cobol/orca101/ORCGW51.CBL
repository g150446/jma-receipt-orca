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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGW51.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : オンライン資格確認情報（Ｗ５１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  20/12/XX    NMED−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.00    ORCAMO       22/01/XX  診療行為資格確認有無対応
      *  05.01.01    ORCAMO       22/07/15  診療情報対応
      *  05.01.02    ORCAMO       22/12/19  電子処方箋対応
      *  05.02.00    ORCAMO       23/10/XX  電子処方箋対応処方箋対応
      *  05.02.00    ORCAMO       24/03/XX  訪問診療・オンライン診療対応
      *                                     医療扶助、交付番号転記対応
      *  05.02.00    ORCAMO       24/10/XX  診療行為通知時間対応
      *  05.02.00    ORCAMO       25/05/XX  医療機関コード対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W51.
           03  SPA-W51-AREA.
               05  SPA-GMN-CUR             PIC 9(02).
               05  SPA-GMN-SYORI           PIC 9(01).
      *2023/10
               05  SPA-NAI-WEBORCA         PIC 9(01).
      *
               05  SPA-GMN-AREA.
      *            オンライン資格確認処理区分
                   07  SPA-GMN-ONSHIKAKU-COMB-G.
                       09  SPA-GMN-ONSHIKAKU-COMB
                                               PIC X(64)   OCCURS  5.
                   07  SPA-GMN-ONSHIKAKU-MAX   PIC 9(02).
                   07  SPA-GMN-ONSHIKAKU-G.
                       09  SPA-GMN-ONSHIKAKU           PIC X(01).
                       09  SPA-GMN-ONSHIKAKU-F         PIC X(01).
                       09  SPA-GMN-ONSHIKAKU-MEI       PIC X(62).
      *            資格確認通知時間
                   07  SPA-GMN-POP-TIME        PIC  9(03).
      *            資格確認状態通知時間
                   07  SPA-GMN-POP-COND-TIME   PIC  9(03).
      *2024/10
      *            診療行為状態通知時間
                   07  SPA-GMN-POP-KGMN-TIME   PIC  9(03).
                   07  SPA-GMN-POP-KGMN-TIME-X REDEFINES
                       SPA-GMN-POP-KGMN-TIME   PIC  X(03).
      *
      *            診療行為資格確認有無
                   07  SPA-GMN-ONSHISINRYO-COMB-G.
                       09  SPA-GMN-ONSHISINRYO-COMB
                                               PIC X(62)   OCCURS  5.
                   07  SPA-GMN-ONSHISINRYO-MAX   PIC 9(02).
                   07  SPA-GMN-ONSHISINRYO-G.
                       09  SPA-GMN-ONSHISINRYO         PIC X(01).
                       09  SPA-GMN-ONSHISINRYO-F       PIC X(01).
                       09  SPA-GMN-ONSHISINRYO-MEI     PIC X(60).
      *
      *            薬剤情報PDF区分
                   07  SPA-GMN-ONSHIYAKUZAI-COMB-G.
                       09  SPA-GMN-ONSHIYAKUZAI-COMB
                                               PIC X(64)   OCCURS  5.
                   07  SPA-GMN-ONSHIYAKUZAI-MAX   PIC 9(02).
                   07  SPA-GMN-ONSHIYAKUZAI-G.
                       09  SPA-GMN-ONSHIYAKUZAI           PIC X(01).
                       09  SPA-GMN-ONSHIYAKUZAI-F         PIC X(01).
                       09  SPA-GMN-ONSHIYAKUZAI-MEI       PIC X(62).
      *
      *            特定検診情報PDF区分
                   07  SPA-GMN-ONSHIKENSIN-COMB-G.
                       09  SPA-GMN-ONSHIKENSIN-COMB
                                               PIC X(64)   OCCURS  5.
                   07  SPA-GMN-ONSHIKENSIN-MAX   PIC 9(02).
                   07  SPA-GMN-ONSHIKENSIN-G.
                       09  SPA-GMN-ONSHIKENSIN           PIC X(01).
                       09  SPA-GMN-ONSHIKENSIN-F         PIC X(01).
                       09  SPA-GMN-ONSHIKENSIN-MEI       PIC X(62).
      *
      *            薬剤・診療情報PDF区分
                   07  SPA-GMN-ONSHIYKZSINRYO-COMB-G.
                       09  SPA-GMN-ONSHIYKZSINRYO-COMB
                                               PIC X(64)   OCCURS  5.
                   07  SPA-GMN-ONSHIYKZSINRYO-MAX  PIC 9(02).
                   07  SPA-GMN-ONSHIYKZSINRYO-G.
                       09  SPA-GMN-ONSHIYKZSINRYO        PIC X(01).
                       09  SPA-GMN-ONSHIYKZSINRYO-F      PIC X(01).
                       09  SPA-GMN-ONSHIYKZSINRYO-MEI    PIC X(62).
      *
      *            電子処方箋区分
                   07  SPA-GMN-DENSHISHOHO-COMB-G.
                       09  SPA-GMN-DENSHISHOHO-COMB
                                               PIC X(64)   OCCURS  5.
                   07  SPA-GMN-DENSHISHOHO-MAX  PIC 9(02).
                   07  SPA-GMN-DENSHISHOHO-G.
                       09  SPA-GMN-DENSHISHOHO        PIC X(01).
                       09  SPA-GMN-DENSHISHOHO-F      PIC X(01).
                       09  SPA-GMN-DENSHISHOHO-MEI    PIC X(62).
      *2023/10
      *            用法置換え区分
                   07  SPA-GMN-YOHOCHGKBN-COMB-G.
                       09  SPA-GMN-YOHOCHGKBN-COMB
                                               PIC X(32)   OCCURS  5.
                   07  SPA-GMN-YOHOCHGKBN-MAX  PIC 9(02).
                   07  SPA-GMN-YOHOCHGKBN-G.
                       09  SPA-GMN-YOHOCHGKBN      PIC X(01).
                       09  SPA-GMN-YOHOCHGKBN-F    PIC X(01).
                       09  SPA-GMN-YOHOCHGKBN-MEI  PIC X(30).
      *            電子処方箋（紙）標準用法名称区分
                   07  SPA-GMN-YOUHOUNAME-COMB-G.
                       09  SPA-GMN-YOUHOUNAME-COMB
                                               PIC X(32)   OCCURS  5.
                   07  SPA-GMN-YOUHOUNAME-MAX  PIC 9(02).
                   07  SPA-GMN-YOUHOUNAME-G.
                       09  SPA-GMN-YOUHOUNAME      PIC X(01).
                       09  SPA-GMN-YOUHOUNAME-F    PIC X(01).
                       09  SPA-GMN-YOUHOUNAME-MEI  PIC X(30).
      *            ＣＳＶディレクトリ場所
                   07  SPA-GMN-CSVFILEDIR      PIC  X(200).
      *R06/03
      *            訪問診療
                   07  SPA-GMN-HOUMON-COMB-G.
                       09  SPA-GMN-HOUMON-COMB
                                               PIC X(62)   OCCURS  2.
                   07  SPA-GMN-HOUMON-MAX      PIC 9(02).
                   07  SPA-GMN-HOUMON-G.
                       09  SPA-GMN-HOUMON      PIC X(01).
                       09  SPA-GMN-HOUMON-F    PIC X(01).
                       09  SPA-GMN-HOUMON-MEI  PIC X(60).
      *            オンライン診療
                   07  SPA-GMN-ONLINE-COMB-G.
                       09  SPA-GMN-ONLINE-COMB
                                               PIC X(62)   OCCURS  2.
                   07  SPA-GMN-ONLINE-MAX      PIC 9(02).
                   07  SPA-GMN-ONLINE-G.
                       09  SPA-GMN-ONLINE      PIC X(01).
                       09  SPA-GMN-ONLINE-F    PIC X(01).
                       09  SPA-GMN-ONLINE-MEI  PIC X(60).
      *
      *            医療扶助
                   07  SPA-GMN-FUJYO-COMB-G.
                       09  SPA-GMN-FUJYO-COMB
                                               PIC X(62)   OCCURS  2.
                   07  SPA-GMN-FUJYO-MAX      PIC 9(02).
                   07  SPA-GMN-FUJYO-G.
                       09  SPA-GMN-FUJYO      PIC X(01).
                       09  SPA-GMN-FUJYO-F    PIC X(01).
                       09  SPA-GMN-FUJYO-MEI  PIC X(60).
      *
      *            交付番号の転記
                   07  SPA-GMN-KOFUTNK-COMB-G.
                       09  SPA-GMN-KOFUTNK-COMB
                                               PIC X(62)   OCCURS  2.
                   07  SPA-GMN-KOFUTNK-MAX        PIC 9(02).
                   07  SPA-GMN-KOFUTNK-G.
                       09  SPA-GMN-KOFUTNK        PIC X(01).
                       09  SPA-GMN-KOFUTNK-F      PIC X(01).
                       09  SPA-GMN-KOFUTNK-MEI    PIC X(60).
      *2025/05
      *            継承元医療機関コード
                   07  SPA-GMN-HOSPCDF         PIC X(10).
                   07  SPA-GMN-HOSPCDF-R       REDEFINES
                                               SPA-GMN-HOSPCDF.
                       09  SPA-GMN-PREFNUM     PIC  X(02).
      *点数表区分
                       09  SPA-GMN-TENHYOKBN   PIC  X(01).
      *医療機関コード
                       09  SYS-GMN-HOSPCD      PIC  X(07).
      *
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                            PIC 9(01).
           03  FLG-SYSKANRI                       PIC 9(01).
      *
           03  FLG-CHK                            PIC 9(01).
           03  FLG-OK                             PIC 9(01).
           03  FLG-SYORI-END                      PIC 9(01).
           03  FLG-TOUROKU                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(80).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-YMD             PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *R04.1
           03  WRK-MAE-ONSHIKAKU   PIC X(01).
           03  WRK-MIDDLEWARE-MODE PIC X(10).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *2025/05
      *    医療機関情報  
           COPY  "CPSK1001.INC".
      *    オンライン資格確認照会情報  
           COPY  "CPSK1051.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 LINKAREA
                                                 SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
           MOVE    SPA-FREE            TO  SPA-W51
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W51             TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK 
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "W51"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      * 
           ACCEPT  WRK-MIDDLEWARE-MODE
                   FROM ENVIRONMENT    "MCP_MIDDLEWARE_MODE"
      *
           MOVE    SPACE               TO  SPA-W51
           INITIALIZE                  SPA-W51-AREA
      *2023/10
      *    weborca設定
           IF    ( WRK-MIDDLEWARE-MODE =   "weborca" )
               MOVE    1               TO  SPA-NAI-WEBORCA
           ELSE
               MOVE    ZERO            TO  SPA-NAI-WEBORCA
           END-IF
      *
           MOVE    "0 照会しない"      TO  SPA-GMN-ONSHIKAKU-COMB
                                                             (1)
           MOVE    "1 照会する"        TO  SPA-GMN-ONSHIKAKU-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-ONSHIKAKU-MAX
      *
      *R04.1
      *    診療行為資格照会有無
           MOVE    "0 無し"            TO  SPA-GMN-ONSHISINRYO-COMB
                                                             (1)
           MOVE    "1 有り"            TO  SPA-GMN-ONSHISINRYO-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-ONSHISINRYO-MAX
      *
           MOVE    "0 出力しない"      TO  SPA-GMN-ONSHIYAKUZAI-COMB
                                                             (1)
           MOVE    "1 出力する"        TO  SPA-GMN-ONSHIYAKUZAI-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-ONSHIYAKUZAI-MAX
      *
           MOVE    "0 出力しない"      TO  SPA-GMN-ONSHIKENSIN-COMB
                                                             (1)
           MOVE    "1 出力する"        TO  SPA-GMN-ONSHIKENSIN-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-ONSHIKENSIN-MAX
      *
           MOVE    "0 出力しない"      TO  SPA-GMN-ONSHIYKZSINRYO-COMB
                                                             (1)
           MOVE    "1 出力する"        TO  SPA-GMN-ONSHIYKZSINRYO-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-ONSHIYKZSINRYO-MAX
      *
           MOVE    "0 しない"          TO  SPA-GMN-DENSHISHOHO-COMB
                                                             (1)
           MOVE    "1 する"            TO  SPA-GMN-DENSHISHOHO-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-DENSHISHOHO-MAX
      *2023/10
           MOVE    "0 しない"          TO  SPA-GMN-YOHOCHGKBN-COMB
                                                             (1)
           MOVE    "1 する"            TO  SPA-GMN-YOHOCHGKBN-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-YOHOCHGKBN-MAX
      *
      *    電子処方箋（紙）標準用法名称
           MOVE    "0 しない"          TO  SPA-GMN-YOUHOUNAME-COMB
                                                             (1)
           MOVE    "1 する"            TO  SPA-GMN-YOUHOUNAME-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-YOUHOUNAME-MAX
      *
      *R06/02
      *    訪問診療
           MOVE    "0 照会しない"      TO  SPA-GMN-HOUMON-COMB
                                                             (1)
           MOVE    "1 照会する"        TO  SPA-GMN-HOUMON-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-HOUMON-MAX
      *    オンライン診療
           MOVE    "0 照会しない"      TO  SPA-GMN-ONLINE-COMB
                                                             (1)
           MOVE    "1 照会する"        TO  SPA-GMN-ONLINE-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-ONLINE-MAX
      *
      *    医療扶助
           MOVE    "0 照会しない"      TO  SPA-GMN-FUJYO-COMB
                                                             (1)
           MOVE    "1 照会する"        TO  SPA-GMN-FUJYO-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-FUJYO-MAX
      *    交付番号の転記
           MOVE    "0 しない"          TO  SPA-GMN-KOFUTNK-COMB
                                                             (1)
           MOVE    "1 する"            TO  SPA-GMN-KOFUTNK-COMB
                                                             (2)
           MOVE    2                   TO  SPA-GMN-KOFUTNK-MAX
      *    患者情報通知情報
           MOVE    SPACE               TO  SYS-1051-REC
           INITIALIZE                      SYS-1051-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1051-HOSPNUM
           MOVE    "1051"              TO  SYS-1051-KANRICD
           MOVE    "*"                 TO  SYS-1051-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1051-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1051-EDYUKYMD
           MOVE    SYS-1051-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1051-REC
           ELSE
               INITIALIZE                      SYS-1051-REC
           END-IF
      *
      *    オンライン資格確認処理区分
           MOVE    SYS-1051-ONSHIKAKU  TO  SPA-GMN-ONSHIKAKU
           MOVE    SYS-1051-POPUP-TIMEOUT  TO  SPA-GMN-POP-TIME
           MOVE    SYS-1051-POPUP-COND-TIMEOUT
                                           TO  SPA-GMN-POP-COND-TIME
      *2024/10
      *    診療行為状態通知時間
           IF      SYS-1051-POPUP-KGMN-TIMEOUT =   SPACE
      *        未設定の時は、資格確認情報通知時間
               MOVE    SYS-1051-POPUP-TIMEOUT
                                           TO  SPA-GMN-POP-KGMN-TIME
           ELSE
               MOVE    SYS-1051-POPUP-KGMN-TIMEOUT
                                           TO  SPA-GMN-POP-KGMN-TIME
           END-IF
      *
           MOVE    SYS-1051-ONSHIYAKUZAI   TO  SPA-GMN-ONSHIYAKUZAI
           MOVE    SYS-1051-ONSHIKENSIN    TO  SPA-GMN-ONSHIKENSIN
      *R04.1
           MOVE    SYS-1051-ONSHISINRYO    TO  SPA-GMN-ONSHISINRYO
           MOVE    SYS-1051-ONSHIYKZSINRYO TO  SPA-GMN-ONSHIYKZSINRYO
      *R06/02
      *    訪問診療
           MOVE    SYS-1051-HOUMON         TO  SPA-GMN-HOUMON
      *    オンライン診療
           MOVE    SYS-1051-ONLINE         TO  SPA-GMN-ONLINE
      *    医療扶助
           MOVE    SYS-1051-FUJYO          TO  SPA-GMN-FUJYO
      *    医療扶助
           MOVE    SYS-1051-FUJYO          TO  SPA-GMN-FUJYO
      *    交付番号の転記
           MOVE    SYS-1051-KOFUTNK        TO  SPA-GMN-KOFUTNK
      *
      **** IF    ( WRK-MIDDLEWARE-MODE =   "weborca" )
           IF    ( SPA-NAI-WEBORCA     =   1    )
               MOVE    SYS-1051-DENSHISHOHO    TO  SPA-GMN-DENSHISHOHO
      *用法置換え区分
               MOVE    SYS-1051-YOHOCHGKBN     TO  SPA-GMN-YOHOCHGKBN
      *電子処方箋（紙）標準用法名称
               MOVE    SYS-1051-YOUHOUNAME     TO  SPA-GMN-YOUHOUNAME
      *ＣＳＶディレクトリ
               MOVE    SYS-1051-CSVFILEDIR     TO  SPA-GMN-CSVFILEDIR
           END-IF
      *
      *    オンライン資格確認処理区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHIKAKU-MAX )
               IF      SPA-GMN-ONSHIKAKU   =   SPA-GMN-ONSHIKAKU-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHIKAKU-COMB(IDX)
                                       TO  SPA-GMN-ONSHIKAKU-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-ONSHIKAKU   =   SPACE
      *        処理しない、が初期値
               MOVE    SPA-GMN-ONSHIKAKU-COMB(1)
                                       TO  SPA-GMN-ONSHIKAKU-G
           END-IF
      *R04.1
      *    診療行為資格確認有無区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHISINRYO-MAX )
               IF      SPA-GMN-ONSHISINRYO =   SPA-GMN-ONSHISINRYO-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHISINRYO-COMB(IDX)
                                       TO  SPA-GMN-ONSHISINRYO-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-ONSHISINRYO  =   SPACE
               IF      SPA-GMN-ONSHIKAKU   =   "0"
      *             照会なし
                    MOVE    SPA-GMN-ONSHISINRYO-COMB(1)
                                       TO  SPA-GMN-ONSHISINRYO-G
               ELSE
      *             照会あり
                    MOVE    SPA-GMN-ONSHISINRYO-COMB(2)
                                       TO  SPA-GMN-ONSHISINRYO-G
               END-IF
           END-IF
      *
      *    薬剤情報PDF区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHIYAKUZAI-MAX )
               IF      SPA-GMN-ONSHIYAKUZAI
                                       =   SPA-GMN-ONSHIYAKUZAI-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHIYAKUZAI-COMB(IDX)
                                       TO  SPA-GMN-ONSHIYAKUZAI-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-ONSHIYAKUZAI   =   SPACE
      *        出力しないが初期値
               MOVE    SPA-GMN-ONSHIYAKUZAI-COMB(1)
                                       TO  SPA-GMN-ONSHIYAKUZAI-G
           END-IF
      *    特定検診情報PDF区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHIKENSIN-MAX )
               IF      SPA-GMN-ONSHIKENSIN
                                       =   SPA-GMN-ONSHIKENSIN-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHIKENSIN-COMB(IDX)
                                       TO  SPA-GMN-ONSHIKENSIN-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-ONSHIKENSIN   =   SPACE
      *        出力しないが初期値
               MOVE    SPA-GMN-ONSHIKENSIN-COMB(1)
                                       TO  SPA-GMN-ONSHIKENSIN-G
           END-IF
      *    薬剤・診療情報PDF区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHIYKZSINRYO-MAX )
               IF      SPA-GMN-ONSHIYKZSINRYO
                                       =   SPA-GMN-ONSHIYKZSINRYO-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHIYKZSINRYO-COMB(IDX)
                                       TO  SPA-GMN-ONSHIYKZSINRYO-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-ONSHIYKZSINRYO   =   SPACE
      *        出力しないが初期値
               MOVE    SPA-GMN-ONSHIYKZSINRYO-COMB(1)
                                       TO  SPA-GMN-ONSHIYKZSINRYO-G
           END-IF
      *R06/02
      *    訪問診療
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-HOUMON-MAX )
               IF      SPA-GMN-HOUMON
                                       =   SPA-GMN-HOUMON-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-HOUMON-COMB(IDX)
                                       TO  SPA-GMN-HOUMON-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-HOUMON      =   SPACE
      *        しないが初期値
               MOVE    SPA-GMN-HOUMON-COMB(1)
                                       TO  SPA-GMN-HOUMON-G
           END-IF
      *R06/02
      *    オンライン診療
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONLINE-MAX )
               IF      SPA-GMN-ONLINE
                                       =   SPA-GMN-ONLINE-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONLINE-COMB(IDX)
                                       TO  SPA-GMN-ONLINE-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-ONLINE      =   SPACE
      *        しないが初期値
               MOVE    SPA-GMN-ONLINE-COMB(1)
                                       TO  SPA-GMN-ONLINE-G
           END-IF
      *R06/03
      *    医療扶助
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-FUJYO-MAX )
               IF      SPA-GMN-FUJYO
                                       =   SPA-GMN-FUJYO-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-FUJYO-COMB(IDX)
                                       TO  SPA-GMN-FUJYO-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-FUJYO       =   SPACE
      *        オン資格照会あり
               IF      SPA-GMN-ONSHIKAKU   =   "1"
      *            する
                   MOVE    SPA-GMN-FUJYO-COMB(2)
                                       TO  SPA-GMN-FUJYO-G
               ELSE
      *            しない
                   MOVE    SPA-GMN-FUJYO-COMB(1)
                                       TO  SPA-GMN-FUJYO-G
               END-IF
           END-IF
      *R06/03
      *    交付番号の転記
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-KOFUTNK-MAX )
               IF      SPA-GMN-KOFUTNK
                                       =   SPA-GMN-KOFUTNK-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-KOFUTNK-COMB(IDX)
                                       TO  SPA-GMN-KOFUTNK-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-KOFUTNK      =   SPACE
      *        しないが初期値
               MOVE    SPA-GMN-KOFUTNK-COMB(1)
                                       TO  SPA-GMN-KOFUTNK-G
           END-IF
      *
      *    電子処方箋区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-DENSHISHOHO-MAX )
               IF      SPA-GMN-DENSHISHOHO
                                       =   SPA-GMN-DENSHISHOHO-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-DENSHISHOHO-COMB(IDX)
                                       TO  SPA-GMN-DENSHISHOHO-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-DENSHISHOHO   =   SPACE
      *        しないが初期値
               MOVE    SPA-GMN-DENSHISHOHO-COMB(1)
                                       TO  SPA-GMN-DENSHISHOHO-G
           END-IF
      *2023/10
      *    用法置換え区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-YOHOCHGKBN-MAX )
               IF      SPA-GMN-YOHOCHGKBN   =   SPA-GMN-YOHOCHGKBN-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-YOHOCHGKBN-COMB(IDX)
                                       TO  SPA-GMN-YOHOCHGKBN-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-YOHOCHGKBN  =   SPACE
      *        処理しない、が初期値
               MOVE    SPA-GMN-YOHOCHGKBN-COMB(1)
                                       TO  SPA-GMN-YOHOCHGKBN-G
           END-IF
      *    電子処方箋（紙）標準用法名称区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-YOUHOUNAME-MAX )
               IF      SPA-GMN-YOUHOUNAME  =   SPA-GMN-YOUHOUNAME-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-YOUHOUNAME-COMB(IDX)
                                       TO  SPA-GMN-YOUHOUNAME-G
               END-IF
           END-PERFORM
           IF      SPA-GMN-YOUHOUNAME  =   SPACE
      *        処理しない、が初期値
               MOVE    SPA-GMN-YOUHOUNAME-COMB(1)
                                       TO  SPA-GMN-YOUHOUNAME-G
           END-IF
      *2025/05
      *    継承元医療機関コード
           MOVE    SYS-1051-HOSPCDF    TO  SPA-GMN-HOSPCDF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
      *        登録
               WHEN    "1001"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
      *        削除
               WHEN    "1002"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   END-IF
      *2025/05
      *        複写
               WHEN    "1005"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4051-HOSPCDCOPY-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM  210-BACK
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  402-CLEAR-SEC
      *        医療機関ＣＤ複写
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM  405-HOSPNUM-COPY-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    SPA-GMN-ONSHIKAKU   TO  WRK-MAE-ONSHIKAKU
      *
           MOVE    W51-ONSHIKAKU       TO  SPA-GMN-ONSHIKAKU-G
           MOVE    W51-ONSHIYAKUZAI    TO  SPA-GMN-ONSHIYAKUZAI-G
           MOVE    W51-ONSHIKENSIN     TO  SPA-GMN-ONSHIKENSIN-G
           MOVE    W51-POP-TIME        TO  SPA-GMN-POP-TIME
           MOVE    W51-POP-COND-TIME   TO  SPA-GMN-POP-COND-TIME
      *2024/10
           MOVE    W51-POP-KGMN-TIME   TO  SPA-GMN-POP-KGMN-TIME
      *
           MOVE    W51-ONSHISINRYO     TO  SPA-GMN-ONSHISINRYO-G
           MOVE    W51-ONSHIYKZSINRYO  TO  SPA-GMN-ONSHIYKZSINRYO-G
           MOVE    W51-DENSHISHOHO     TO  SPA-GMN-DENSHISHOHO-G
      *R06/02
           MOVE    W51-HOUMON          TO  SPA-GMN-HOUMON-G
           MOVE    W51-ONLINE          TO  SPA-GMN-ONLINE-G
           MOVE    W51-FUJYO           TO  SPA-GMN-FUJYO-G
           MOVE    W51-KOFUTNK         TO  SPA-GMN-KOFUTNK-G
      *2023/10
           MOVE    W51-YOHOCHGKBN      TO  SPA-GMN-YOHOCHGKBN-G
           MOVE    W51-YOUHOUNAME      TO  SPA-GMN-YOUHOUNAME-G
           MOVE    W51-CSVFILEDIR-DATA TO  SPA-GMN-CSVFILEDIR
      *2025/05
      *    継承元医療機関コード
           MOVE    W51-HOSPCDF         TO  SPA-GMN-HOSPCDF
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      * 
      *     ACCEPT  WRK-MIDDLEWARE-MODE
      *             FROM ENVIRONMENT    "MCP_MIDDLEWARE_MODE"
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-ONSHIKAKU-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-POP-TIME-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-POP-COND-TIME-CHK-SEC
           END-IF
      *R04.1
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-ONSHISINRYO-CHK-SEC
           END-IF
      **
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-ONSHIYAKUZAI-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-ONSHIKENSIN-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-ONSHIYKZSINRYO-CHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      ****     IF    ( WRK-MIDDLEWARE-MODE =   "weborca" )
               IF      SPA-NAI-WEBORCA     =   1
                  PERFORM 4100-DENSHISHOHO-CHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-WEBORCA     =   1
                   PERFORM 4100-YOHOCHGKBN-CHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-WEBORCA     =   1
                   PERFORM 4100-YOUHOUNAME-CHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-WEBORCA     =   1
                   PERFORM 4100-CSVFILEDIR-CHK-SEC
               END-IF
           END-IF
      *R06/03
      *    医療扶助
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-FUJYO-CHK-SEC
           END-IF
      *    交付番号の転記
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-KOFUTNK-CHK-SEC
           END-IF
      *    訪問診療
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-HOUMON-CHK-SEC
           END-IF
      *    オンライン診療
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-ONLINE-CHK-SEC
           END-IF
      *2025/05
      *    継承元医療機関コード
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4100-HOSPCDF-CHK-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
           .
      *****************************************************************
      *    オンライン資格確認チェック処理
      *****************************************************************
       4100-ONSHIKAKU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *    オンライン資格確認処理区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHIKAKU-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-ONSHIKAKU   =   SPA-GMN-ONSHIKAKU-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHIKAKU-COMB(IDX)
                                       TO  SPA-GMN-ONSHIKAKU-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      WRK-MAE-ONSHIKAKU   =   "0"
               IF      SPA-GMN-ONSHIKAKU   =   "1"
      *            0 から 1へ変更で、診療行為資格照会も「1」へ
                   MOVE    "1"             TO  SPA-GMN-ONSHISINRYO
      *            医療扶助「1」へ
                   MOVE    "1"             TO  SPA-GMN-FUJYO
               END-IF
           END-IF
           .
       4100-ONSHIKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    資格確認通知時間チェック処理
      *****************************************************************
       4100-POP-TIME-CHK-SEC       SECTION.
      *
      *
           .
       4100-POP-TIME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    資格確認状態通知時間チェック処理
      *****************************************************************
       4100-POP-COND-TIME-CHK-SEC  SECTION.
      *
      *
           .
       4100-POP-COND-TIME-CHK-EXT.
           EXIT.
      *R04.1
      *****************************************************************
      *    診療行為資格確認有無チェエック処理
      *****************************************************************
       4100-ONSHISINRYO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *    オンライン資格確認処理区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHISINRYO-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-ONSHISINRYO =   SPA-GMN-ONSHISINRYO-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHISINRYO-COMB(IDX)
                                       TO  SPA-GMN-ONSHISINRYO-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *    オンライン資格確認しない時
           IF      SPA-GMN-ONSHIKAKU   =   "0"
               IF      SPA-GMN-ONSHISINRYO     =   "1"
                   MOVE    SPA-GMN-ONSHISINRYO-COMB(1)
                                       TO  SPA-GMN-ONSHISINRYO-G
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4100-ONSHIKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報ＰＤＦチェック処理
      *****************************************************************
       4100-ONSHIYAKUZAI-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *    薬剤情報ＰＤＦ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHIYAKUZAI-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-ONSHIYAKUZAI
                                       =   SPA-GMN-ONSHIYAKUZAI-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHIYAKUZAI-COMB(IDX)
                                       TO  SPA-GMN-ONSHIYAKUZAI-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-ONSHIYAKUZAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *     特定検診情報ＰＤＦチェック処理
      *****************************************************************
       4100-ONSHIKENSIN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *    特定検診情報ＰＤＦ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHIKENSIN-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-ONSHIKENSIN
                                       =   SPA-GMN-ONSHIKENSIN-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHIKENSIN-COMB(IDX)
                                       TO  SPA-GMN-ONSHIKENSIN-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-ONSHIYAKUZAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *     薬剤・診療情報ＰＤＦチェック処理
      *****************************************************************
       4100-ONSHIYKZSINRYO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *    特定検診情報ＰＤＦ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONSHIYKZSINRYO-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-ONSHIYKZSINRYO
                                       =   SPA-GMN-ONSHIYKZSINRYO-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONSHIYKZSINRYO-COMB(IDX)
                                       TO  SPA-GMN-ONSHIYKZSINRYO-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-ONSHIYAKUZAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *     電子処方箋チェック処理
      *****************************************************************
       4100-DENSHISHOHO-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *    特定検診情報ＰＤＦ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-DENSHISHOHO-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-DENSHISHOHO
                                       =   SPA-GMN-DENSHISHOHO-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-DENSHISHOHO-COMB(IDX)
                                       TO  SPA-GMN-DENSHISHOHO-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-ONSHIYAKUZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法置換え区分チェック処理
      *****************************************************************
       4100-YOHOCHGKBN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *    用法置換え区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-YOHOCHGKBN-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-YOHOCHGKBN =   SPA-GMN-YOHOCHGKBN-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-YOHOCHGKBN-COMB(IDX)
                                       TO  SPA-GMN-YOHOCHGKBN-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-YOHOCHGKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電子処方箋（紙）標準用法名称区分チェック処理
      *****************************************************************
       4100-YOUHOUNAME-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *    用法置換え区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-YOUHOUNAME-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-YOUHOUNAME =   SPA-GMN-YOUHOUNAME-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-YOUHOUNAME-COMB(IDX)
                                       TO  SPA-GMN-YOUHOUNAME-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-YOUHOUNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶディレクトリチェック処理
      *****************************************************************
       4100-CSVFILEDIR-CHK-SEC       SECTION.
      *
      *
           .
       4100-CSVFILEDIR-CHK-EXT.
           EXIT.
      *R06/02
      *****************************************************************
      *    訪問診療区分チェック処理
      *****************************************************************
       4100-HOUMON-CHK-SEC             SECTION.
      *
      *    オンライン資格確認しない時
           IF      SPA-GMN-ONSHIKAKU   =   "0"
      *        照会なしとする
               MOVE    SPA-GMN-HOUMON-COMB(1)
                                       TO  SPA-GMN-HOUMON-G
               GO  TO  4100-HOUMON-CHK-EXT
           END-IF
           MOVE    ZERO                TO  FLG-CHK
      *    訪問診療区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-HOUMON-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-HOUMON      =   SPA-GMN-HOUMON-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-HOUMON-COMB(IDX)
                                       TO  SPA-GMN-HOUMON-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    14                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-HOUMON-CHK-EXT.
           EXIT.
      *R06/02
      *****************************************************************
      *    オンライン診療　チェック処理
      *****************************************************************
       4100-ONLINE-CHK-SEC             SECTION.
      *
      *    オンライン資格確認しない時
           IF      SPA-GMN-ONSHIKAKU   =   "0"
      *        照会なしとする
               MOVE    SPA-GMN-ONLINE-COMB(1)
                                       TO  SPA-GMN-ONLINE-G
               GO  TO  4100-ONLINE-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
      *    オンライン診療
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-ONLINE-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-ONLINE      =   SPA-GMN-ONLINE-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-ONLINE-COMB(IDX)
                                       TO  SPA-GMN-ONLINE-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    15                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-ONLINE-CHK-EXT.
           EXIT.
      *R06/03
      *****************************************************************
      *    医療扶助　チェック処理
      *****************************************************************
       4100-FUJYO-CHK-SEC             SECTION.
      *
      *    オンライン資格確認しない時
           IF      SPA-GMN-ONSHIKAKU   =   "0"
      *        照会なしとする
               MOVE    SPA-GMN-FUJYO-COMB(1)
                                       TO  SPA-GMN-FUJYO-G
               GO  TO  4100-FUJYO-CHK-EXT
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-CHK
      *    医療扶助
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-FUJYO-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-FUJYO       =   SPA-GMN-FUJYO-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-FUJYO-COMB(IDX)
                                       TO  SPA-GMN-FUJYO-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN-CUR
           END-IF
      *
            .
       4100-FUJYO-CHK-EXT.
           EXIT.
      *R06/03
      *****************************************************************
      *    交付番号の転記　チェック処理
      *****************************************************************
       4100-KOFUTNK-CHK-SEC             SECTION.
      *
      *    医療扶助がするの時のみ有効
           IF      SPA-GMN-FUJYO       =   "1"
               CONTINUE
           ELSE
               MOVE    SPA-GMN-KOFUTNK-COMB(1)
                                       TO  SPA-GMN-KOFUTNK-G
               GO  TO  4100-KOFUTNK-CHK-EXT
           END-IF
           MOVE    ZERO                TO  FLG-CHK
      *    交付番号の転記
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-GMN-KOFUTNK-MAX )
                         OR  ( FLG-CHK     =   1       )
               IF      SPA-GMN-KOFUTNK    =   SPA-GMN-KOFUTNK-COMB
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-KOFUTNK-COMB(IDX)
                                       TO  SPA-GMN-KOFUTNK-G
                   MOVE    1           TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4100-KOFUTNK-CHK-EXT.
           EXIT.
      *
      *2025/05
      *****************************************************************
      *    継承元医療機関コード　チェック処理
      *****************************************************************
       4100-HOSPCDF-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-HOSPCDF      =   SPACE
               GO  TO  4100-HOSPCDF-CHK-EXT
           END-IF
      *
      *    半角チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-HOSPCDF     TO  KANACHK-MAE-INPUT
           MOVE    10                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
      *        半角であること
               CONTINUE
           ELSE
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
               GO  TO  4100-HOSPCDF-CHK-EXT
           END-IF
      *    県
           IF    (SPA-GMN-PREFNUM      >=  "01"
                                   AND <=  "47"  )
               CONTINUE
           ELSE
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
      *    点数表区分
           IF     (SPA-ERRCD           =   SPACE )
               IF      SPA-GMN-TENHYOKBN   =   "1"
                                           OR  "7"
                   CONTINUE
               ELSE
                   MOVE    "0016"              TO  SPA-ERRCD
                   MOVE    17                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    医療機関コード
           IF     (SPA-ERRCD           =   SPACE )
               IF      SYS-GMN-HOSPCD  NOT NUMERIC
                   MOVE    "0017"              TO  SPA-ERRCD
                   MOVE    17                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4100-HOSPCDF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W51"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           PERFORM 310-SPASET-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       402-CLEAR-EXT.
           EXIT.
      *2025/05
      *****************************************************************
      *    継承元医療機関コード複写処理
      *****************************************************************
       405-HOSPNUM-COPY-SEC            SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
              GO      TO      405-HOSPNUM-COPY-EXT
           END-IF
      *
           MOVE    "1005"              TO  SPA-WIDCD
           .
       405-HOSPNUM-COPY-EXT.
           EXIT.
      *****************************************************************
      *    継承元医療機関コード複写処理
      *****************************************************************
       4051-HOSPCDCOPY-SEC            SECTION.
      *
      *    医療機関情報検索
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
      
               IF      SYS-1001-HOSPCD     =   SPACE
                                           OR  ZERO
                   MOVE    "0018"              TO  SPA-ERRCD
                   MOVE    17                  TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        県番号
               MOVE    SYS-1001-PREFNUM    TO  SPA-GMN-PREFNUM
      *        点数表区分
               MOVE    SYS-1001-TENHYOKBN  TO  SPA-GMN-TENHYOKBN
      *        医療機関コード
               MOVE    SYS-1001-HOSPCD     TO  SYS-GMN-HOSPCD
           END-IF
      *
           .
       4051-HOSPCDCOPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録確認画面表示処理
      *****************************************************************
       412-TOUROKU-SEC                               SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
              GO      TO      412-TOUROKU-EXT
           END-IF
      *
      *    入力項目の関連チェック処理
           PERFORM 4901-KANRENCHK-SEC
      *
           IF    ( SPA-ERRCD        =   SPACE )
               MOVE    "1001"              TO  SPA-WIDCD
           END-IF
      *
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      * 
      *     ACCEPT  WRK-MIDDLEWARE-MODE
      *             FROM ENVIRONMENT    "MCP_MIDDLEWARE_MODE"
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      * 
           MOVE    SPACE               TO  SYS-1051-REC
           INITIALIZE                      SYS-1051-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1051-HOSPNUM
           MOVE    "1051"              TO  SYS-1051-KANRICD
           MOVE    "*"                 TO  SYS-1051-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1051-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1051-EDYUKYMD
           MOVE    SYS-1051-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1051-REC
           END-IF
      *
      *    新規
           IF      FLG-SYSKANRI        =   1
               MOVE    SPACE               TO  SYS-1051-REC
               INITIALIZE                      SYS-1051-REC
               MOVE    SPA-HOSPNUM         TO  SYS-1051-HOSPNUM
               MOVE    "1051"              TO  SYS-1051-KANRICD
               MOVE    "*"                 TO  SYS-1051-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1051-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD    TO  SYS-1051-EDYUKYMD
           END-IF
      *
           MOVE    SPA-GMN-ONSHIKAKU   TO  SYS-1051-ONSHIKAKU
           MOVE    SPA-GMN-POP-TIME    TO  SYS-1051-POPUP-TIMEOUT
           MOVE    SPA-GMN-POP-COND-TIME
                                       TO  SYS-1051-POPUP-COND-TIMEOUT
      *2024/10
           MOVE    SPA-GMN-POP-KGMN-TIME
                                       TO  SYS-1051-POPUP-KGMN-TIMEOUT
           MOVE    SPA-GMN-ONSHIYAKUZAI    TO  SYS-1051-ONSHIYAKUZAI
           MOVE    SPA-GMN-ONSHIKENSIN TO  SYS-1051-ONSHIKENSIN
           MOVE    SPA-GMN-ONSHISINRYO TO  SYS-1051-ONSHISINRYO
           MOVE    SPA-GMN-ONSHIYKZSINRYO  TO  SYS-1051-ONSHIYKZSINRYO
      *R06/02
      *    訪問診療
           MOVE    SPA-GMN-HOUMON      TO  SYS-1051-HOUMON
           MOVE    SPA-GMN-ONLINE      TO  SYS-1051-ONLINE
           MOVE    SPA-GMN-FUJYO       TO  SYS-1051-FUJYO
           MOVE    SPA-GMN-KOFUTNK     TO  SYS-1051-KOFUTNK
      *2025/05
      *    継承元医療機関コード
           MOVE    SPA-GMN-HOSPCDF     TO  SYS-1051-HOSPCDF
      *
      **** IF    ( WRK-MIDDLEWARE-MODE =   "weborca" )
           IF      SPA-NAI-WEBORCA     =   1
               MOVE    SPA-GMN-DENSHISHOHO TO  SYS-1051-DENSHISHOHO
      *用法置換え区分
               MOVE    SPA-GMN-YOHOCHGKBN  TO  SYS-1051-YOHOCHGKBN
      *電子処方箋（紙）標準用法名称
               MOVE    SPA-GMN-YOUHOUNAME  TO  SYS-1051-YOUHOUNAME
      *ＣＳＶディレクトリ
               MOVE    SPA-GMN-CSVFILEDIR  TO  SYS-1051-CSVFILEDIR
           ELSE
               MOVE    SPACE               TO  SYS-1051-DENSHISHOHO
               MOVE    SPACE               TO  SYS-1051-YOHOCHGKBN
               MOVE    SPACE               TO  SYS-1051-YOUHOUNAME
               MOVE    SPACE               TO  SYS-1051-CSVFILEDIR
           END-IF
      *
           IF      FLG-SYSKANRI            =   ZERO
      *        更新
               MOVE    SPA-HOSPNUM         TO  SYS-1051-UPD-HOSPNUM
               MOVE    SYS-KANRICD         TO  SYS-1051-UPD-KANRICD
               MOVE    SYS-KBNCD           TO  SYS-1051-UPD-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1051-UPD-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD    TO  SYS-1051-UPD-EDYUKYMD
      *
               MOVE    SPA-OPID            TO  SYS-1051-OPID
               MOVE    SMCNDATE-YMD        TO  SYS-1051-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1051-UPHMS
               MOVE    SYS-1051-REC        TO  MCPDATA-REC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "W51 SYSKANRI UPD ERR:" MCP-RC
                            ",KEY:" SYS-1051-KEY
                   MOVE    "1002"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPA-OPID            TO  SYS-1051-OPID
               MOVE    SMCNDATE-YMD        TO  SYS-1051-UPYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1051-CREYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1051-UPHMS
               MOVE    SYS-1051-REC        TO  MCPDATA-REC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "W51 SYSKANRI INS ERR:" MCP-RC
                            ",KEY:" SYS-1051-KEY
                   MOVE    "1001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK 
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4901-KANRENCHK-SEC           SECTION.
      *
      *
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .
       403-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1051-REC
           INITIALIZE                      SYS-1051-REC
           MOVE    "1051"              TO  SYS-1051-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1051-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1051-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1051-HOSPNUM
           MOVE    SYS-1051-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1051-KEY 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG  
               WHEN    OTHER
                   IF      WRK-YMD  (1:7)  NOT NUMERIC
                       INSPECT     WRK-YMD      REPLACING   ALL " "
                                                            BY  "0"
                       MOVE    WRK-YMD(1:1)      TO  WRK-WGO
                       MOVE    WRK-YMD(2:2)      TO  WRK-WYY
                       MOVE    WRK-YMD(5:2)      TO  WRK-WMM
                       MOVE    WRK-YMD(8:2)      TO  WRK-WDD
                   ELSE
                       MOVE    WRK-YMD(1:7)      TO  WRK-WYMD 
                   END-IF
      *
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "12"                TO  LNK-DAY1-IRAI
                   MOVE    ZERO                TO  LNK-DAY1-YMD
                   MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
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
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W51    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      * 
      *     ACCEPT  WRK-MIDDLEWARE-MODE
      *             FROM ENVIRONMENT    "MCP_MIDDLEWARE_MODE"
      *
           MOVE    SPACE               TO  W51
           INITIALIZE                      W51
      *
           MOVE    SPA-GMN-ONSHIKAKU-G
                                       TO  W51-ONSHIKAKU
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-ONSHIKAKU-MAX )
               MOVE    SPA-GMN-ONSHIKAKU-COMB (IDX)
                                       TO  W51-ONSHIKAKU-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-ONSHIKAKU-MAX   TO  W51-ONSHIKAKU-COUNT
           MOVE    SPA-GMN-POP-TIME
                                       TO  W51-POP-TIME
           MOVE    SPA-GMN-POP-COND-TIME
                                       TO  W51-POP-COND-TIME
      *2024/10
           MOVE    SPA-GMN-POP-KGMN-TIME
                                       TO  W51-POP-KGMN-TIME
      *
      *R04.1
           MOVE    SPA-GMN-ONSHISINRYO-G
                                       TO  W51-ONSHISINRYO
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-ONSHISINRYO-MAX )
               MOVE    SPA-GMN-ONSHISINRYO-COMB (IDX)
                                       TO  W51-ONSHISINRYO-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-ONSHISINRYO-MAX TO  W51-ONSHISINRYO-COUNT
      *
           MOVE    SPA-GMN-ONSHIYAKUZAI-G
                                       TO  W51-ONSHIYAKUZAI
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-ONSHIYAKUZAI-MAX )
               MOVE    SPA-GMN-ONSHIYAKUZAI-COMB (IDX)
                                       TO  W51-ONSHIYAKUZAI-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-ONSHIYAKUZAI-MAX
                                       TO  W51-ONSHIYAKUZAI-COUNT
      *
           MOVE    SPA-GMN-ONSHIKENSIN-G
                                       TO  W51-ONSHIKENSIN
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-ONSHIKENSIN-MAX )
               MOVE    SPA-GMN-ONSHIKENSIN-COMB (IDX)
                                       TO  W51-ONSHIKENSIN-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-ONSHIKENSIN-MAX
                                       TO  W51-ONSHIKENSIN-COUNT
      *
           MOVE    SPA-GMN-ONSHIYKZSINRYO-G
                                       TO  W51-ONSHIYKZSINRYO
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-ONSHIYKZSINRYO-MAX )
               MOVE    SPA-GMN-ONSHIYKZSINRYO-COMB (IDX)
                                       TO  W51-ONSHIYKZSINRYO-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-ONSHIYKZSINRYO-MAX
                                       TO  W51-ONSHIYKZSINRYO-COUNT
      *R06/02
      *    訪問診療
           MOVE    SPA-GMN-HOUMON-G    TO  W51-HOUMON
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-HOUMON-MAX )
               MOVE    SPA-GMN-HOUMON-COMB (IDX)
                                       TO  W51-HOUMON-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HOUMON-MAX  TO  W51-HOUMON-COUNT
      *
           MOVE    SPA-GMN-ONLINE-G    TO  W51-ONLINE
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-ONLINE-MAX )
               MOVE    SPA-GMN-ONLINE-COMB (IDX)
                                       TO  W51-ONLINE-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-ONLINE-MAX  TO  W51-ONLINE-COUNT
      *
           MOVE    SPA-GMN-FUJYO-G     TO  W51-FUJYO
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-FUJYO-MAX )
               MOVE    SPA-GMN-FUJYO-COMB (IDX)
                                       TO  W51-FUJYO-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-FUJYO-MAX   TO  W51-FUJYO-COUNT
      *
      *
           MOVE    SPA-GMN-KOFUTNK-G    TO  W51-KOFUTNK
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-KOFUTNK-MAX )
               MOVE    SPA-GMN-KOFUTNK-COMB (IDX)
                                       TO  W51-KOFUTNK-ITEM
                                                     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-KOFUTNK-MAX  TO  W51-KOFUTNK-COUNT
      *2025/05
           MOVE    SPA-GMN-HOSPCDF      TO  W51-HOSPCDF
      *
      **** IF    ( WRK-MIDDLEWARE-MODE =   "weborca" )
           IF      SPA-NAI-WEBORCA     =   1
               MOVE    "T"             TO  W51-DENSHISHOHO-VISIBLE
                                           W51-DENSHISHOHO-TITLE-VISIBLE
               MOVE    SPA-GMN-DENSHISHOHO-G
                                       TO  W51-DENSHISHOHO
               PERFORM VARYING IDX         FROM    1   BY  1
                       UNTIL ( IDX         >   SPA-GMN-DENSHISHOHO-MAX )
                   MOVE    SPA-GMN-DENSHISHOHO-COMB (IDX)
                                           TO  W51-DENSHISHOHO-ITEM
                                                         (IDX)
               END-PERFORM
               MOVE    SPA-GMN-DENSHISHOHO-MAX
                                       TO  W51-DENSHISHOHO-COUNT
      *2023/10
               MOVE    "T"             TO  W51-YOHOCHGKBN-VISIBLE
               MOVE    "T"             TO  W51-YOUHOUNAME-VISIBLE
               MOVE    "T"             TO  W51-CSVFILEDIR-VISIBLE
               MOVE    "T"             TO  W51-YOHOCHGKBN-TITLE-VISIBLE
               MOVE    "T"             TO  W51-YOUHOUNAME-TITLE-VISIBLE
               MOVE    "T"             TO  W51-CSVFILEDIR-T1-VISIBLE
               MOVE    "T"             TO  W51-CSVFILEDIR-T2-VISIBLE
               MOVE    "T"             TO  W51-CSVFILEDIR-T3-VISIBLE
               MOVE    SPA-GMN-YOHOCHGKBN-G
                                       TO  W51-YOHOCHGKBN
               PERFORM VARYING IDX         FROM    1   BY  1
                       UNTIL ( IDX         >   SPA-GMN-YOHOCHGKBN-MAX )
                   MOVE    SPA-GMN-YOHOCHGKBN-COMB (IDX)
                                           TO  W51-YOHOCHGKBN-ITEM
                                                     (IDX)
               END-PERFORM
               MOVE    SPA-GMN-YOHOCHGKBN-MAX  TO  W51-YOHOCHGKBN-COUNT
      *
               MOVE    SPA-GMN-YOUHOUNAME-G
                                           TO  W51-YOUHOUNAME
               PERFORM VARYING IDX         FROM    1   BY  1
                       UNTIL ( IDX         >   SPA-GMN-YOUHOUNAME-MAX )
                   MOVE    SPA-GMN-YOUHOUNAME-COMB (IDX)
                                           TO  W51-YOUHOUNAME-ITEM
                                                     (IDX)
               END-PERFORM
               MOVE    SPA-GMN-YOUHOUNAME-MAX  TO  W51-YOUHOUNAME-COUNT
      *
               MOVE    SPA-GMN-CSVFILEDIR  TO  W51-CSVFILEDIR-DATA
           ELSE
               MOVE    "F"             TO  W51-DENSHISHOHO-VISIBLE
                                           W51-DENSHISHOHO-TITLE-VISIBLE
               MOVE    "F"             TO  W51-YOHOCHGKBN-VISIBLE
               MOVE    "F"             TO  W51-YOUHOUNAME-VISIBLE
               MOVE    "F"             TO  W51-CSVFILEDIR-VISIBLE
               MOVE    "F"             TO  W51-YOHOCHGKBN-TITLE-VISIBLE
               MOVE    "F"             TO  W51-YOUHOUNAME-TITLE-VISIBLE
               MOVE    "F"             TO  W51-CSVFILEDIR-T1-VISIBLE
               MOVE    "F"             TO  W51-CSVFILEDIR-T2-VISIBLE
               MOVE    "F"             TO  W51-CSVFILEDIR-T3-VISIBLE
           END-IF
      *
      *    業務権限非活性処理
      **** PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      * 
      *     ACCEPT  WRK-MIDDLEWARE-MODE
      *             FROM ENVIRONMENT    "MCP_MIDDLEWARE_MODE"
      *
           IF      SPA-GMN-CUR         =   ZERO
      *   基本情報
               EVALUATE    WRK-MCP-WIDGET
               WHEN    "ONSHIKAKU"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "POP_TIME"
      *R04.1       MOVE    3               TO  SPA-GMN-CUR
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "POP_COND_TIME"
      *2024/10     MOVE    5               TO  SPA-GMN-CUR
                   MOVE    16             TO  SPA-GMN-CUR
      *2024/10
               WHEN    "POP_KGMN_TIME"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "ONSHISINRYO"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "ONSHIYAKUZAI"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "ONSHIKENSIN"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "ONSHIYKZSINRYO"
      ****         IF    ( WRK-MIDDLEWARE-MODE =   "weborca" )
                   IF      SPA-NAI-WEBORCA     =   1
                       MOVE    7               TO  SPA-GMN-CUR
                   ELSE    
      *R06/02          MOVE    99              TO  SPA-GMN-CUR
                       MOVE    12              TO  SPA-GMN-CUR
                   END-IF    
               WHEN    "DENSHISHOHO"
      *2023/10     MOVE    99              TO  SPA-GMN-CUR
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "YOHOCHGKBN"
                   MOVE    10              TO  SPA-GMN-CUR
               WHEN    "YOUHOUNAME"
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "CSVFILEDIR"
      *R06/02      MOVE    99              TO  SPA-GMN-CUR
                   MOVE    12              TO  SPA-GMN-CUR
      *R06/03
               WHEN    "FUJYO"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN    "KOFUTNK"
                   MOVE    14              TO  SPA-GMN-CUR
               WHEN    "HOUMON"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN    "ONLINE"
                   MOVE    99              TO  SPA-GMN-CUR
      *
      *2025/05
                   MOVE    17              TO  SPA-GMN-CUR
               WHEN    "HOSPCDF"
                   MOVE    99              TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    1               TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN   1
                    MOVE  "ONSHIKAKU"          TO  MCP-WIDGET
                WHEN   2
                    MOVE  "POP_TIME"           TO  MCP-WIDGET
                WHEN   3
                    MOVE  "ONSHIYAKUZAI"       TO  MCP-WIDGET
                WHEN   4
                    MOVE  "ONSHIKENSIN"        TO  MCP-WIDGET
                WHEN   5
                    MOVE  "ONSHISINRYO"        TO  MCP-WIDGET
                WHEN   6
                    MOVE  "ONSHIYKZSINRYO"     TO  MCP-WIDGET
                WHEN   7
                    MOVE  "DENSHISHOHO"        TO  MCP-WIDGET
                WHEN   8
                    MOVE  "POP_COND_TIME"      TO  MCP-WIDGET
                WHEN   9
                    MOVE  "YOHOCHGKBN"         TO  MCP-WIDGET
                WHEN   10
                    MOVE  "YOUHOUNAME"         TO  MCP-WIDGET
                WHEN   11
                    MOVE  "CSVFILEDIR"         TO  MCP-WIDGET
                WHEN   12
                    MOVE  "FUJYO"             TO  MCP-WIDGET
                WHEN   13
                    MOVE  "KOFUTNK"             TO  MCP-WIDGET
                WHEN   14
                    MOVE  "HOUMON"             TO  MCP-WIDGET
                WHEN   15
                    MOVE  "ONLINE"             TO  MCP-WIDGET
                WHEN   16
                    MOVE  "POP_KGMN_TIME"      TO  MCP-WIDGET
                WHEN   17
                    MOVE  "HOSPCDF"            TO  MCP-WIDGET
                 WHEN   99
                    MOVE  "B12"                TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE     "オンライン資格確認照会区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE     "薬剤情報ＰＤＦ区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE     "特定検診情報ＰＤＦ区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE     "診療行為資格確認有無区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   STRING   "オンライン資格確認を照会するでは"
                            "ありません。照会はできません。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   MOVE     "薬剤・診療情報ＰＤＦ区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE     "電子処方箋区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   STRING  "ＱＲデータ用法の標準用法置換え区分"
                           "入力エラー"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0009"
                   STRING  "電子処方箋（紙）の標準用法名称出力区分"
                           "入力エラー"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
      *
               WHEN    "0010"
                   STRING  "訪問診療区分入力エラー"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0011"
                   STRING  "オンライン診療区分入力エラー"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0012"
                   STRING  "医療扶助区分入力エラー"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0013"
                   STRING  "交付番号の転記区分入力エラー"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
      *2025/05
               WHEN    "0014"
                   STRING  "承継元医療機関コードが半角ではありません"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0015"
                   STRING  "承継元医療機関コードの県番号が違います"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0016"
                   STRING  "承継元医療機関コードの点数表番号が違います"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0017"
                   STRING  "承継元医療機関コードの医療機関コードは"
                           "数値７桁で入力してください"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0018"
                   STRING  "当日有効な医療機関情報ー基本の"
                           "医療機関コードがありません。"
                           "複写できません。"
                                               DELIMITED BY  SIZE
                                               INTO  SPA-ERRMSG
                    END-STRING
      *
                WHEN    "1001"
                   MOVE    "登録処理ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "更新処理ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "削除処理ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W51"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録処理をします。よろしいですか？"
                                               TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "削除処理をします。よろしいですか？"
                                               TO  WRK-WIDMSG
      *2025/05
               WHEN     "1005"
                   STRING    "システム管理の医療機関コードを"
                             "承継元医療機関コードに複写します。"
                             "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                               INTO  WRK-WIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
           EVALUATE    SPA-WIDCD
               WHEN    "1002"
                   MOVE    "B01"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "W51"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
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
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.

