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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGP032.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : オンライン資格確認一覧画面（Ｐ０３２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  20/09/XX    NMED-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     ORCAMO       21/05/XX  強制確認対応
      * 05.00.00     ORCAMO       21/08/XX  資格有効性の背景色対応
      * 05.01.00     ORCAMO       21/11/XX  オン資格確認
      *                                     公費照会対応
      * 05.01.00     ORCAMO       22/02/XX  変更無し、削除対応
      * 05.01.00     ORCAMO       22/07/XX  照会番号表示対応
      * 05.01.00     ORCAMO       23/03/XX  薬剤情報など同意表示対応
      * 05.02.00     ORCAMO       23/11/XX  本人家族・負担割など表示対応
      * 05.02.00     ORCAMO       24/12/XX  訪問診療・オン診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-ONSKAKU         PIC 9(01).
           03  FLG-PTHKNINF         PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-DATAOK          PIC 9(01).
      *
           03  FLG-CHKKBN          PIC 9(01).
           03  FLG-FUJYO-ARI       PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *    03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(06).
           03  IDY                     PIC 9(06).
           03  IDY2                    PIC 9(04).
      *
           03  IDX-STR                 PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TIME.
               05  WRK-THH             PIC 9(02).
               05  WRK-TMM             PIC 9(02).
               05  WRK-TSS             PIC 9(02).
           03  WRK-THKNID              PIC 9(10).
      *
           03  WRK-PIDMSG              PIC X(80).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *    処理保存
           03  WRK-SYORI           PIC 9(01).
      *
           03  WRK-PTID            PIC 9(10).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-OYA-UUID            PIC X(36).
           03  WRK-TBL-UUID            PIC X(36).
      *
           03  WRK-FUJYO-UUID          PIC X(36).
           03  WRK-KAKU-KAKUNINYMD     PIC X(08).
      *
           03  WRK-ON-RATE             PIC 9(03).
      *
      *     処理実行日時
           03  WRK-SHORI-TIME-IN.
               05  WRK-SHORI-IN-YY     PIC X(04).
               05  WRK-SHORI-IN-MM     PIC X(02).
               05  WRK-SHORI-IN-DD     PIC X(02).
               05  WRK-SHORI-IN-THH    PIC X(02).
               05  WRK-SHORI-IN-TMM    PIC X(02).
      *
      * 処理実行日時編集
       01  WRK-SHORI-TIME-H.
           03  WRK-SHORI-YY            PIC X(04).
           03  WRK-SHORI-F1            PIC X(01)   VALUE    ".".
           03  WRK-SHORI-MM            PIC X(02).
           03  WRK-SHORI-F2            PIC X(01)   VALUE    ".".
           03  WRK-SHORI-DD            PIC X(02).
           03  WRK-SHORI-F3            PIC X(01)   VALUE    "-".
           03  WRK-SHORI-THH           PIC X(02).
           03  WRK-SHORI-F4            PIC X(01)   VALUE    ":".
           03  WRK-SHORI-TMM           PIC X(02).
      *R05.3
       01  WRK-SHORI-TIME-HS.
           03  WRK-SHORI-THHS           PIC X(02).
           03  WRK-SHORI-F5            PIC X(01)   VALUE    ":".
           03  WRK-SHORI-TMMS           PIC X(02).
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
           03  WRK-HENTIME.
               05  WRK-THH-H       PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM-H       PIC Z9.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    オンライン資格確認照会結果テーブル
       01  ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    オンライン資格確認照会結果テーブル
       01  HZN-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"  REPLACING
                                      //ONSKAKU-// BY //HZN-ONSKAKU-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    検索文字追加
           COPY    "CPORCSADDSIGN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *R03.5
      *    資格確認照会パラメタ
           COPY    "CPORCSONSHI001.INC".
      *
      *R05.11
      *    患者保険情報とのチェック
           COPY    "CPORCSONSHI003.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA12SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           END-EVALUATE
      *
      *    画面編集後、表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU     TO  SPA-WORK
           MOVE    SPA-AREA          TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA    TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P032"              TO  MCP-WINDOW
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
           EVALUATE    SPA-MOTOPG
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *R04.2
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                   AND   ( SPA-PIDCD       NOT =   SPACE )
                       PERFORM 520-JIDSET-SEC
                   END-IF
      *******
               WHEN    "P033"
                   PERFORM 3002-P033-INIT-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者確認画面より
      *****************************************************************
       3002-P033-INIT-SEC            SECTION.
      *
           IF      SPA-ONSKAKU-SYORI   =   SPACE
      *        オンライン資格選択中の解除
               IF      (SPA-P031-SEL-UUID  NOT =   SPACE )
                   PERFORM 30009-ONSHIKAKU-CR-SEC
               END-IF
      *
               MOVE    SPA-P032-SYSYMD     TO  WRK-SYMD
               MOVE    SPA-P032-SYORI      TO  WRK-SYORI
      *        オンライン資格確認照会編集処理
               PERFORM 30001-P032-INIT-SEC
               MOVE    1                   TO  SPA-P032-CUR
           ELSE
               PERFORM 210-BACK
           END-IF
           .
       3002-P032-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
      *    選択中の解除
           IF      (SPA-P031-SEL-UUID  NOT =   SPACE )
               PERFORM 30009-ONSHIKAKU-CR-SEC
           END-IF
      *
           MOVE    ZERO                TO  WRK-SYORI
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P032-INIT-SEC
           IF      SPA-P032-MAX        =   ZERO
               MOVE    99                  TO  SPA-P032-CUR
           ELSE
               MOVE    1                   TO  SPA-P032-CUR
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認照会結果編集処理
      *****************************************************************
       30001-P032-INIT-SEC              SECTION.
      *
      *    ＳＰＡ編集
           INITIALIZE                      SPA-P032-AREA
           INITIALIZE                      SPA-ONSKAKU-AREA
           INITIALIZE                      SPA-ONSKAKU-KOHTEI-AREA
      *    システム日付
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           STRING  "（"
                   LNK-DAY2-EDTYMD3
                   "）"               DELIMITED BY  SIZE
                                      INTO  SPA-P032-MIDASI
           END-STRING
      *
           MOVE    WRK-SYMD            TO  SPA-P032-SYSYMD
           MOVE    WRK-SYORI           TO  SPA-P032-SYORI
           MOVE    1                   TO  SPA-P032-PAGE
           PERFORM 3001-ONSKAK-SYORI-SEC
           IF      SPA-P032-MAX        =   ZERO
               MOVE    99                  TO  SPA-P032-CUR
           ELSE
               MOVE    1                   TO  SPA-P032-CUR
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認照会結果編集処理
      *****************************************************************
       3001-ONSKAK-SYORI-SEC              SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-P032-SYSYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  SPA-P032-HSYSYMD
      *
           INITIALIZE                  SPA-P032-LIST-G
      *    オンライン資格確認照会結果検索
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-PTID            TO  ONSKAKU-PTID
      *2022/09
      *    予約照会以外を対象
           MOVE    "1"                 TO  ONSKAKU-ORCA-SHOKAI-KBN
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
      *2023/11  予約照会は対象外へ戻す
           MOVE    "key17"             TO  WRK-MCP-PATHNAME
      ***  MOVE    "key22"             TO  WRK-MCP-PATHNAME
      *    全部対象
           IF     (SPA-P032-SYORI      =   1    )
      *        削除分も対象
               MOVE    "key15"             TO  WRK-MCP-PATHNAME
      *予約も込の時
      *******  MOVE    "key23"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    WRK-MCP-PATHNAME     TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME     TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           MOVE    ZERO                TO  SPA-P032-NEXT
           IF      SPA-P032-PAGE       =   ZERO
               MOVE    1               TO  SPA-P032-PAGE
           END-IF
           COMPUTE IDX-STR             =  (SPA-P032-PAGE   -  1)
                                       *   50
                                       +   1
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDY2
           PERFORM UNTIL   (FLG-ONSKAKU        =    1   )
                       OR  (SPA-P032-NEXT      =    1   )
               MOVE    ZERO                TO  FLG-DATAOK
      *        処理結果区分
               IF     (ONSKAKU-RESULT-KBN  =   "1"  )
      *            当日の未処理のみ対象
                   IF     (SPA-P032-SYORI      =   ZERO )
                     AND  (ONSKAKU-MOD-FLG     =   SPACE)
                     AND  (ONSKAKU-KAKUNINYMD  =   SPA-P032-SYSYMD)
                       MOVE    1               TO  FLG-DATAOK
                   END-IF
      *            全部対象
                   IF     (SPA-P032-SYORI      =   1    )
                       MOVE    1               TO  FLG-DATAOK
                   END-IF
      *2024/12
      *            訪問診療同意は対象外
                   IF      (ONSKAKU-SHOKAI-KBN     =   SPACE )
                     AND   (ONSKAKU-ONSHINRYO-KBN  =   "2"   )
                     AND   (ONSKAKU-PROCESS-CLASS  =   "2"   )
                       MOVE    ZERO            TO  FLG-DATAOK
                   END-IF
               END-IF
      *
               IF      FLG-DATAOK          =   1
                   ADD     1                    TO  IDY
      *2023/01/XX
      *            RENNUM は３桁
                   ADD     1                   TO  IDY2
                   IF      IDY2                >   999
                       MOVE    1               TO  IDY2
                   END-IF
      *
                   IF      IDX-STR              >   IDY
                       CONTINUE
                   ELSE
                       PERFORM 3002-ONSAKA-HEN-SEC
                   END-IF
               END-IF
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
           MOVE    "tbl_onshi_kaku"     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME     TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3001-ONSKAK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認照会結果編集処理
      *****************************************************************
       3002-ONSAKA-HEN-SEC              SECTION.
      *
           ADD     1                   TO  IDX
           IF      IDX                 >   50
               MOVE    1               TO  SPA-P032-NEXT
               GO      TO  3002-ONSAKA-HEN-EXT
           END-IF
      *
      **** MOVE    IDY                 TO  SPA-P032-RENNUM  (IDX)
           MOVE    IDY2                TO  SPA-P032-RENNUM  (IDX)
      *    ＵＩＤ
           MOVE    ONSKAKU-TBL-UUID    TO  SPA-P032-TBL-UUID(IDX)
           MOVE    ONSKAKU-SHOKAI-KBN  TO  SPA-P032-SHOKAI-KBN(IDX)
           EVALUATE    ONSKAKU-SHOKAI-KBN
               WHEN    "1"
      *            顔認証
                   MOVE    "★"        TO  SPA-P032-SHOKAI-MEI(IDX)
               WHEN    "2"
      *            保険証
                   MOVE    "◎"        TO  SPA-P032-SHOKAI-MEI(IDX)
      *2022/09
                   IF      ONSKAKU-ORCA-SHOKAI-KBN     =  "2"
                                                       OR "3"
      *                保険証ＯＣＲ
                       MOVE    "◆"       TO  SPA-P032-SHOKAI-MEI(IDX)
                   END-IF
      *2023/04
                   IF      ONSKAKU-ORCA-SHOKAI-KBN     =  "1"
      *                予約照会
                       MOVE    "■"       TO  SPA-P032-SHOKAI-MEI(IDX)
                   END-IF
      *R03.11
               WHEN    "9"
      *            地方公費
                   MOVE    "公"        TO  SPA-P032-SHOKAI-MEI(IDX)
      *2022/09
               WHEN    "8"
      *            地方公費(保険証ＯＣＲ)
                   MOVE    "公"        TO  SPA-P032-SHOKAI-MEI(IDX)
      *2024/12
               WHEN    SPACE
                   MOVE    SPACE       TO  SPA-P032-SHOKAI-MEI(IDX)
                   IF      ONSKAKU-ONSHINRYO-KBN   =   "2"
      *                訪問診療
                       MOVE    "訪"    TO  SPA-P032-SHOKAI-MEI(IDX)
                   END-IF
                   IF      ONSKAKU-ONSHINRYO-KBN   =   "3"
      *                オンライン診療
                       MOVE    "オ"    TO  SPA-P032-SHOKAI-MEI(IDX)
                   END-IF
               WHEN    OTHER
                   MOVE    SPACE       TO  SPA-P032-SHOKAI-MEI(IDX)
           END-EVALUATE
      *2024/04
      *    保険証OCR
           MOVE    ONSKAKU-ORCA-SHOKAI-KBN
                                       TO  SPA-P032-ORCA-SHOKAI-KBN
                                                              (IDX)
      *    処理実行日時
           MOVE    ONSKAKU-SHORI-TIME  TO  WRK-SHORI-TIME-IN
           MOVE    WRK-SHORI-IN-YY     TO  WRK-SHORI-YY
           MOVE    WRK-SHORI-IN-MM     TO  WRK-SHORI-MM
           MOVE    WRK-SHORI-IN-DD     TO  WRK-SHORI-DD
           MOVE    WRK-SHORI-IN-THH    TO  WRK-SHORI-THH
           MOVE    WRK-SHORI-IN-TMM    TO  WRK-SHORI-TMM
      *R05.3  時間のみへ
           MOVE    WRK-SHORI-IN-THH    TO  WRK-SHORI-THHS
           MOVE    WRK-SHORI-IN-TMM    TO  WRK-SHORI-TMMS
      *
           MOVE    WRK-SHORI-TIME-HS   TO  SPA-P032-SHORITIME(IDX)
      *    確認日付
           MOVE    ONSKAKU-KAKUNINYMD  TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P032-KAKUNINYMD (IDX)
      *    ＵＩＤ
           MOVE    ONSKAKU-TBL-UUID    TO  SPA-P032-TBL-UUID(IDX)
      *R04.7
      *    照会番号
           MOVE    ONSKAKU-SHOKAI-NUM  TO  SPA-P032-SHOKAI-NUM(IDX)
      *
           IF     (ONSKAKU-SHOKAI-KBN      =   "2"   )
             AND  (ONSKAKU-RES-HKNJANUM    =   SPACE )
      *        保険証照会 確認結果なし
               PERFORM 30022-SHOKAI-HEN-SEC
           ELSE
      *        確認結果編集
               PERFORM 30021-RESULT-HEN-SEC
           END-IF
      *    資格喪失事由
      **** MOVE    ONSKAKU-LOSS-JIYU   TO  SPA-P032-LOSS-JIYU (IDX)
      *R03.4
      *    保険証照会
           IF     (ONSKAKU-SHOKAI-KBN      =   "2"   )
      *R03.8    有効の時のみ
             AND  (ONSKAKU-SIKAKU-YUKO     =   "1"   )
      *        枝番不一致
               IF      ONSKAKU-SHO-EDABAN  NOT =  ONSKAKU-RES-EDABAN
      *****        MOVE    1               TO  SPA-P032-CHKEDABAN(IDX)
                   MOVE    1               TO  SPA-P032-TJYOTAIFLG(IDX)
               END-IF
           END-IF
      *2023/11
      *    患者保険情報との一致
           IF      (ONSKAKU-SIKAKU-YUKO    =   "1"   )
              AND ((ONSKAKU-SHOKAI-KBN     =   "1"
                                           OR  "2"   )
      *2024/12     訪問診療
               OR ((ONSKAKU-SHOKAI-KBN     =   SPACE )  AND
                   (ONSKAKU-ONSHINRYO-KBN  =   "2"
                                           OR  "3"  )))
      *        有効で、患者番号が決定済み
               PERFORM 30023-HKNINF-CHKHEN-SEC
           END-IF
           MOVE    ONSKAKU-CARD-CLASS  TO  SPA-P032-CARD-CLASS (IDX)
      *2024/04    扶助併用
           MOVE    ONSKAKU-FUJYO-HEIYO TO  SPA-P032-FUJYO-HEIYO (IDX)
      *2024/02
           IF     (ONSKAKU-CARD-CLASS   =   "A1" )
      *       FUJYO_UUID 空白は選択エラー
              IF       ONSKAKU-FUJYO-UUID  =   SPACE
                  MOVE    "9"              TO  SPA-P032-FUJYO-KOH(IDX)
      *2025/03
      *           「資格無」は選択可とする
                  IF     (ONSKAKU-SIKAKU-YUKO  =   "4"  )
                      MOVE    "0"          TO  SPA-P032-FUJYO-KOH(IDX)
                  END-IF
              ELSE
                  IF  (ONSKAKU-KOUHI-UUID  NOT =   SPACE )
      *           保険の有無をFUJYO_UUID で判定
                  AND (ONSKAKU-KOUHI-UUID  NOT =   ONSKAKU-FUJYO-UUID)
      *                保険あり
                       MOVE    "1"         TO  SPA-P032-FUJYO-KOH (IDX)
                   END-IF
      *2024/04
      *           保険扶助併用
                  IF   (ONSKAKU-FUJYO-HEIYO    =   "1"   )
      *                保険あり
                       MOVE    "1"         TO  SPA-P032-FUJYO-KOH (IDX)
                   END-IF
               END-IF
           END-IF
      *
      *    資格有効
           EVALUATE    ONSKAKU-SIKAKU-YUKO
               WHEN    "1"
                   MOVE    "有効"      TO  SPA-P032-SIKAKU-YUKO(IDX)
               WHEN    "2"
                   MOVE    "無効"      TO  SPA-P032-SIKAKU-YUKO(IDX)
                   MOVE    2           TO  SPA-P032-TJYOTAIFLG(IDX)
               WHEN    "3"
                   MOVE    "無効（新）"
                                       TO  SPA-P032-SIKAKU-YUKO(IDX)
                   MOVE    3           TO  SPA-P032-TJYOTAIFLG(IDX)
               WHEN    "4"
                   MOVE    "該当無"    TO  SPA-P032-SIKAKU-YUKO(IDX)
      *R03.4
                   MOVE    4           TO  SPA-P032-TJYOTAIFLG(IDX)
               WHEN    "5"
                   MOVE    "複数該当"  TO  SPA-P032-SIKAKU-YUKO(IDX)
      *            患者確認必要
                   MOVE    5           TO  SPA-P032-TJYOTAIFLG(IDX)
      *2024/12
               WHEN    "6"
                   MOVE    "有効（未）"
                                       TO  SPA-P032-SIKAKU-YUKO(IDX)
           END-EVALUATE
      *
      *    状態
           MOVE    ONSKAKU-LOCK-KBN    TO  SPA-P032-LOCK-KBN(IDX)
           MOVE    ONSKAKU-MOD-FLG     TO  SPA-P032-MOD-FLG (IDX)
           IF      ONSKAKU-MOD-FLG NOT =   SPACE
      *        処理済み
               MOVE    "済"            TO  SPA-P032-SHOKAI-MEI(IDX)
                                                          (3:2)
           END-IF
           IF      ONSKAKU-LOCK-KBN    NOT =   SPACE
      *        処理中
               MOVE    "他"            TO  SPA-P032-SHOKAI-MEI(IDX)
                                                          (3:2)
           END-IF
      *
      *R04.2
           MOVE    ONSKAKU-SCR-DLTFLG   TO  SPA-P032-SCR-DLTFLG(IDX)
           IF      ONSKAKU-SCR-DLTFLG   =   "1"
             AND  (SPA-P032-SHOKAI-MEI(IDX) (3:2) =   SPACE )
               MOVE    "×"            TO  SPA-P032-SHOKAI-MEI(IDX)
                                                          (3:2)
           END-IF
      *
      *    限度額適用認定証
           IF     (ONSKAKU-GENDO-DOUIFLG   =   "1" )
               IF     (ONSKAKU-GENDO-SHOKBN    =   SPACE
                                               OR  ZERO  )
                   MOVE    "無"                TO  SPA-P032-GENDO-ARI
                                                                (IDX)
               ELSE
                   MOVE    "有"                TO  SPA-P032-GENDO-ARI
                                                                (IDX)
               END-IF
           END-IF
      *    特定疾病療養受療証提供
           IF     (ONSKAKU-SIKKAN-DOUIFLG  =   "1" )
               IF      (ONSKAKU-SIKKAN-SIPKBN (1)   =   SPACE )
                   AND (ONSKAKU-SIKKAN-SIPKBN (2)   =   SPACE )
                   AND (ONSKAKU-SIKKAN-SIPKBN (3)   =   SPACE )
                   MOVE    "無"                TO  SPA-P032-SIKKAN-ARI
                                                                (IDX)
               ELSE
                   MOVE    "有"                TO  SPA-P032-SIKKAN-ARI
                                                                (IDX)
               END-IF
           END-IF
      *R05.3
      *    同意フラグのみ判定に変更する
           IF    (ONSKAKU-SHOKAI-KBN       =   "2"   )
      *** 保険証OCR同意対応に
            AND  (ONSKAKU-ORCA-SHOKAI-KBN  =   SPACE
                                           OR  "1"  )
      *        保険証照会、予約照会は変更なし
               CONTINUE
           ELSE
      *
      *    限度額適用認定証
           EVALUATE    ONSKAKU-GENDO-DOUIFLG
               WHEN    "1"
                   MOVE    "有"                TO  SPA-P032-GENDO-ARI
                                                                (IDX)
               WHEN    "0"
                   MOVE    "無"                TO  SPA-P032-GENDO-ARI
                                                                (IDX)
           END-EVALUATE
      *    特定疾病療養受療証提供
           EVALUATE    ONSKAKU-SIKKAN-DOUIFLG
               WHEN    "1"
                   MOVE    "有"                TO  SPA-P032-SIKKAN-ARI
                                                                (IDX)
               WHEN    "0"
                   MOVE    "無"                TO  SPA-P032-SIKKAN-ARI
                                                                (IDX)
           END-EVALUATE
      *
           END-IF
      *****
      *
      *    手術情報閲覧同意
           EVALUATE    ONSKAKU-OPE-DOUIFLG
               WHEN   "1"
                   MOVE   "有"             TO  SPA-P032-OPE-DOUIFLG
                                                              (IDX)
               WHEN   "0"
                   MOVE   "無"             TO  SPA-P032-OPE-DOUIFLG
                                                              (IDX)
           END-EVALUATE
      *    特定健診情報閲覧同意
           EVALUATE    ONSKAKU-KENSIN-DOUIFLG
               WHEN   "1"
                   MOVE   "有"             TO  SPA-P032-KENSIN-DOUIFLG
                                                              (IDX)
               WHEN    "0"
                   MOVE   "無"             TO  SPA-P032-KENSIN-DOUIFLG
                                                              (IDX)
           END-EVALUATE
      *    薬剤情報閲覧同意
           EVALUATE    ONSKAKU-YAKUZAI-DOUIFLG
               WHEN   "1"
                   MOVE   "有"             TO  SPA-P032-YAKUZAI-DOUIFLG
                                                              (IDX)
               WHEN    "0"
                   MOVE   "無"             TO  SPA-P032-YAKUZAI-DOUIFLG
                                                              (IDX)
           END-EVALUATE
      *    診療情報閲覧同意
           EVALUATE    ONSKAKU-SHINRYO-DOUIFLG
               WHEN   "1"
                   MOVE   "有"             TO  SPA-P032-SHINRYO-DOUIFLG
                                                              (IDX)
               WHEN    "0"
                   MOVE   "無"             TO  SPA-P032-SHINRYO-DOUIFLG
                                                              (IDX)
           END-EVALUATE
      *
      *
      *
           MOVE    IDX                 TO  SPA-P032-MAX
           .
       3002-ONSAKA-HEN-EXT.
           EXIT.
      *2023/10
      *****************************************************************
      *    患者保険情報とのチェック処理
      *****************************************************************
       30023-HKNINF-CHKHEN-SEC              SECTION.
      *
           INITIALIZE                      ORCSONSHI003AREA
           MOVE    "01"                TO  LNK-ONSHI003-SYOKBN
           MOVE    ONSKAKU-PTID        TO  LNK-ONSHI003-PTID
           MOVE    SPA-P032-SYSYMD     TO  LNK-ONSHI003-SYSYMD
           CALL    "ORCSONSHI003"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSONSHI003AREA
                                       ONSKAKU-REC
           IF      LNK-ONSHI003-CHK-KBN    =   "99"
      *        対象の患者保険登録なし
               GO  TO  30023-HKNINF-CHKHEN-EXT
           END-IF
           IF      LNK-ONSHI003-CHK-HONKZKKBN  =   "1"
               MOVE    "1"                 TO  SPA-P032-HKNCHK (IDX)
           END-IF
           IF      LNK-ONSHI003-CHK-FTNRATE =   "1"
               MOVE    "2"                 TO  SPA-P032-HKNCHK (IDX)
           END-IF
           .
       30023-HKNINF-CHKHEN-EXT.
           EXIT.
      *****************************************************************
      *    確認結果編集処理
      *****************************************************************
       30021-RESULT-HEN-SEC              SECTION.
      *
      *    氏名
           MOVE    ONSKAKU-RES-NAME    TO  SPA-P032-NAME (IDX)
           MOVE    ONSKAKU-RES-KANANAME    TO  SPA-P032-KANANAME (IDX)
      *    生年月日
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
      *    MOVE    WRK-HENYMD          TO  SPA-P032-BIRTHDAY (IDX)
      *    住所
           MOVE    ONSKAKU-RES-POST    TO  SPA-P032-POST (IDX)
           MOVE    ONSKAKU-RES-ADRS    TO  SPA-P032-ADRS (IDX)
      *
      *    保険者番号
           MOVE    ONSKAKU-RES-HKNJANUM
                                       TO  SPA-P032-HKNJANUM(IDX)
           IF     (ONSKAKU-RES-KIGO    =   SPACE )
              AND (ONSKAKU-RES-NUM     =   SPACE )
               MOVE    SPACE           TO  SPA-P032-KIGONUM (IDX)
           ELSE
               STRING  ONSKAKU-RES-KIGO
                   "・"
                   ONSKAKU-RES-NUM     DELIMITED  BY  SPACE
                                       INTO   SPA-P032-KIGONUM (IDX)
               END-STRING
           END-IF
      *    公費照会
           IF      ONSKAKU-SHOKAI-KBN      =   "9"
                                           OR  "8"
               MOVE     ONSKAKU-RES-NUM
                                       TO   SPA-P032-KIGONUM (IDX)
           END-IF
      *
           MOVE    ONSKAKU-RES-EDABAN
                                       TO  SPA-P032-EDABAN (IDX)
      *    被保険者証有効終了年月日
           IF      ONSKAKU-TEKEDYMD    NOT =   SPACE
               MOVE    ONSKAKU-TEKEDYMD    TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P032-TEKEDYMD (IDX)
           END-IF
      *2023/11
      *    本人家族
           EVALUATE    ONSKAKU-RES-HONKZKKBN
               WHEN    "1"
                   MOVE    "本"            TO  SPA-P032-HONKZKKBN (IDX)
               WHEN    "2"
                   MOVE    "家"            TO  SPA-P032-HONKZKKBN (IDX)
           END-EVALUATE
      *    負担割合
      *    補助区分（高齢者の負担割合）
           IF      ONSKAKU-FTNRATE         =   SPACE
                                           OR   ZERO
               MOVE    ZERO                TO  WRK-ON-RATE
           ELSE
               MOVE    ONSKAKU-FTNRATE     TO  WRK-ON-RATE
           END-IF
      *    高齢受給者証があれば優先（前期高齢者）
           IF      ONSKAKU-ELDER-SKKGETYMD     NOT =   SPACE
      *        高齢受給者一部負担金割合
               IF      ONSKAKU-ELDER-FTNRATE   =   SPACE
                                               OR  ZERO
                   MOVE    ZERO                TO  WRK-ON-RATE
               ELSE
                   MOVE    ONSKAKU-ELDER-FTNRATE   TO  WRK-ON-RATE
               END-IF
           END-IF
           MOVE    WRK-ON-RATE         TO  WRK-ZZZ
           MOVE    WRK-ZZZ             TO  SPA-P032-FTNRATE (IDX)
      *
           .
       30021-RESULT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認結果なし編集処理
      *****************************************************************
       30022-SHOKAI-HEN-SEC              SECTION.
      *
      *    生年月日
      *    MOVE    ONSKAKU-SHO-BIRTHDAY    TO  WRK-SYMD
      *    PERFORM 4001-SEIWA-YMDEDIT-SEC
      *    MOVE    WRK-HENYMD          TO  SPA-P032-BIRTHDAY (IDX)
      *
      *    保険者番号
           MOVE    ONSKAKU-SHO-HKNJANUM
                                       TO  SPA-P032-HKNJANUM(IDX)
           IF     (ONSKAKU-SHO-KIGO    =   SPACE )
              AND (ONSKAKU-SHO-NUM     =   SPACE )
               MOVE    SPACE           TO  SPA-P032-KIGONUM (IDX)
           ELSE
               STRING  ONSKAKU-SHO-KIGO
                   "・"
                   ONSKAKU-SHO-NUM     DELIMITED  BY  SPACE
                                       INTO   SPA-P032-KIGONUM (IDX)
               END-STRING
           END-IF
           MOVE    ONSKAKU-SHO-EDABAN
                                       TO  SPA-P032-EDABAN (IDX)
      *    患者確認なし
      *R03.4
      *****MOVE    2                   TO  SPA-P032-TJYOTAIFLG(IDX)
           .
       30022-SHOKAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認解除処理
      *****************************************************************
       30009-ONSHIKAKU-CR-SEC              SECTION.
      *
      *    オンライン資格確認UID検索処理
           PERFORM 4103-ONSKAKU-KEY-SEC
      *
           IF      FLG-ONSKAKU         =   ZERO
      *
               INITIALIZE                  ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"       USING
                                           ORCSMCNDATEAREA
      *
               MOVE    SPACE               TO  ONSKAKU-LOCK-KBN
      *
               MOVE    SPA-OPID            TO  ONSKAKU-OPID
               MOVE    SMCNDATE-YMD        TO  ONSKAKU-UPYMD
               MOVE    SMCNDATE-HMS        TO  ONSKAKU-UPHMS
      *
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "P032 ONSKAKU KEY UPD ERR:"  MCP-RC
                       ",KEY:" ONSKAKU-KEY
               END-IF
           END-IF
           .
       30009-ONSHIKAKU-CR-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
      *****
      *R04.1
      *        限度額承認
               WHEN    "0105"
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    "1"         TO  SPA-P031-GENDO-DOUIFLG
                   ELSE
                       MOVE    SPACE       TO  SPA-P031-GENDO-DOUIFLG
                   END-IF
                   MOVE    SPACE               TO  SPA-PIDCD
      *            P033へ
                   PERFORM 4101-P033-CALL-SEC
      *R04.2
              WHEN    "0101"
      *            削除
                  IF      SPA-PID1-FLG        =   "OK"
                      PERFORM 4031-DELETE-SEC
                  END-IF
                  MOVE    SPACE               TO  SPA-PIDCD
      *R04.2
      *       変更なし処理
               WHEN    "0106"
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4110-HENKOUNASI-SYORI-SEC
                   END-IF
                   MOVE    SPACE          TO  SPA-PIDCD
      *
           END-EVALUATE
      *
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P032
           INITIALIZE                      P032
           MOVE    SPA-P032-HSYSYMD    TO  P032-KAKUNINYMD
           MOVE    SPA-P032-MIDASI     TO  P032-MIDASI-TEXTDATA
      *
      *
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-P032-MAX
               MOVE    SPA-P032-RENNUM (IDX)   TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  P032-RENNUM  (IDX)
               MOVE    SPA-P032-SHOKAI-MEI(IDX)
                                               TO  P032-SHOKAI (IDX)
               MOVE    SPA-P032-SHORITIME(IDX) TO  P032-SHORITIME(IDX)
               MOVE    SPA-P032-KAKUNINYMD(IDX)
                                               TO  P032-KAKUNYMD (IDX)
               MOVE    SPA-P032-NAME     (IDX) TO  P032-NAME (IDX)
               MOVE    SPA-P032-KANANAME (IDX)
                                               TO  P032-KANANAME (IDX)
               MOVE    SPA-P032-HKNJANUM (IDX) TO  P032-HKNJANUM (IDX)
               MOVE    SPA-P032-KIGONUM  (IDX) TO  P032-KIGONUM (IDX)
               MOVE    SPA-P032-EDABAN   (IDX) TO  P032-EDABAN (IDX)
      *2023/11
               MOVE    SPA-P032-HONKZKKBN (IDX)
                                               TO  P032-HONKZKKBN (IDX)
               MOVE    SPA-P032-FTNRATE (IDX)  TO  P032-FTNRATE (IDX)
      *
               MOVE    SPA-P032-TEKEDYMD (IDX)
                                               TO  P032-TEKEDYMD (IDX)
               MOVE    SPA-P032-SIKAKU-YUKO(IDX)
                                               TO  P032-SIKAKU-YUKO(IDX)
               MOVE    SPA-P032-GENDO-ARI (IDX)
                                               TO  P032-GENDO  (IDX)
               MOVE    SPA-P032-SIKKAN-ARI (IDX)
                                               TO  P032-SIKKAN (IDX)
      *R05.3
               MOVE    SPA-P032-OPE-DOUIFLG (IDX)
                                               TO  P032-SYUJYU (IDX)
               MOVE    SPA-P032-KENSIN-DOUIFLG (IDX)
                                               TO  P032-KENSIN (IDX)
               MOVE    SPA-P032-YAKUZAI-DOUIFLG (IDX)
                                               TO  P032-YAKUZAI (IDX)
               MOVE    SPA-P032-SHINRYO-DOUIFLG (IDX)
                                               TO  P032-SHINRYO (IDX)
      *
      *        MOVE    SPA-P032-PTNUM (IDX)
      *                                        TO  P032-PTNUM (IDX)
               MOVE    SPA-P032-POST  (IDX)
                                               TO  P032-POST (IDX)
               MOVE    SPA-P032-ADRS   (IDX)   TO  P032-ADRS   (IDX)
      *R04.7
               MOVE    SPA-P032-SHOKAI-NUM(IDX)
                                               TO  P032-SHOKAI-NUM(IDX)
      *
               IF      SPA-P032-RENNUM (IDX)   =   SPA-P032-SELNUM
                   MOVE    "T"                 TO  P032-SELECTDATA
                                                               (IDX)
                   MOVE    IDX                 TO  P032-ROW
               ELSE
                   MOVE    "F"                 TO  P032-SELECTDATA
                                                               (IDX)
               END-IF
      *
      *R03.8   受付と合わせる
      *        枝番変更（青緑?
      *         IF      SPA-P032-CHKEDABAN (IDX)
      *                                     =   1
      *             MOVE    "#C0E7E0"       TO  P032-BGCOLOR (IDX)
      *         END-IF
      *        患者確認必要（複数該当）（黄緑)
      *         IF      SPA-P032-TJYOTAIFLG (IDX)
      *                                     =   3
      *             MOVE    "#EFFF99"       TO  P032-BGCOLOR (IDX)
      *         END-IF
      *        資格結果なし（淡い赤）?
      *         IF      SPA-P032-TJYOTAIFLG (IDX)
      *                                     =   2
      *             MOVE    "#FAEBD7"       TO  P032-BGCOLOR (IDX)
      ******   END-IF
      *2023/11
      *        被保険者資格証明書（優先）
               IF      SPA-P032-CARD-CLASS (IDX)   =   "05"
      ****                                         OR  "A1"
      *           患者確認必要（シェルピンク）
                  MOVE    "#fbdac8"       TO   P032-BGCOLOR    (IDX)
               END-IF
      *2024/02
      *        医療扶助
               IF      SPA-P032-CARD-CLASS (IDX)   =   "A1"
      *           患者確認必要（ 灰緑）
                  MOVE    "#95CC95"       TO   P032-BGCOLOR    (IDX)
               END-IF
      *2023/11
      *        患者保険情報とのチェック（優先）
               IF      SPA-P032-HKNCHK (IDX)   NOT =   SPACE
      *           患者確認必要（淡香 うすこう）
                  MOVE    "#f3bf88"       TO   P032-BGCOLOR    (IDX)
               END-IF
      *R03.8
      *        資格有効性による背景色
               EVALUATE     SPA-P032-TJYOTAIFLG(IDX)
                   WHEN     1
      *            枝番確認（ベビーブルー）
                       MOVE    "#bbe2f1"       TO  P032-BGCOLOR (IDX)
                   WHEN     2
      *            無効（サーモンピンク）
                       MOVE    "#f3a68c"       TO   P032-BGCOLOR
                                                                  (IDX)
                   WHEN    3
      *            無効（新）（ラベンダー）
                       MOVE    "#cab8d9"       TO   P032-BGCOLOR
                                                                  (IDX)
                   WHEN    4
      *            資格結果なし（ベビーピンク
                       MOVE    "#f6e5cc"       TO   P032-BGCOLOR
                                                                  (IDX)
                   WHEN    5
      *            患者確認必要（複数該当）（淡い黄緑色）
                      MOVE    "#EFFF99"       TO   P032-BGCOLOR
                                                                  (IDX)
               END-EVALUATE
      *****
      *******
      *        削除分（やや灰色い無彩色）
               IF      SPA-P032-SCR-DLTFLG(IDX)   =    "1"
                      MOVE    "#dcdcdc"       TO   P032-BGCOLOR
                                                                  (IDX)
               END-IF
      ********
           END-PERFORM
           MOVE    SPA-P032-MAX        TO  P032-ONSHILIST-COUNT
      *
           MOVE    SPA-P032-SELNUM     TO  P032-SELNUM
      *
           IF      SPA-P032-SYORI      =   ZERO
               MOVE    "F10 全対象"
                                       TO  P032-B10-LABEL
               MOVE    "【当日未処理分】"
                                       TO  P032-MIDASI2-TEXTDATA
           ELSE
               MOVE    "F10 当日分"
                                       TO  P032-B10-LABEL
               MOVE    "【全対象分】"
                                       TO  P032-MIDASI2-TEXTDATA
           END-IF
      *
           IF      SPA-P032-NEXT       =   1
               IF      SPA-P032-PAGE       >   1
                   MOVE    "前次頁あり"  TO  P032-MIDASINEXT-TEXTDATA
                   MOVE    "red"         TO  P032-MIDASINEXT-STYLE
               ELSE
                   MOVE    "次頁あり"    TO  P032-MIDASINEXT-TEXTDATA
                   MOVE    "red"         TO  P032-MIDASINEXT-STYLE
               END-IF
           ELSE
               IF      SPA-P032-PAGE       >   1
                   MOVE    "前頁あり"    TO  P032-MIDASINEXT-TEXTDATA
                   MOVE    "red"         TO  P032-MIDASINEXT-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-P032-CUR
               WHEN    01
                   MOVE    "SELNUM"    TO  MCP-WIDGET
               WHEN    08
                   MOVE    "B08"       TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B10"       TO  MCP-WIDGET
               WHEN    12
                   MOVE    "B12"       TO  MCP-WIDGET
               WHEN    03
                   MOVE    "B03"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *R04.2
      *     削除
              WHEN    "CLICKED"       ALSO    "B03"
                  PERFORM 403-DELETE-SYORI-SEC
      *    前日
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 405-BACKDAY-SEC
      *    次日
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 406-NEXTDAY-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 406-BACKPAGE-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 407-NEXTPAGE-SEC
      *R03.5
      *    強制資格確認
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 408-KAKUNIN-SEC
      *    表示切替
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-LISTCHNG-SEC
      *R04.2
      *       変更なし
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 411-B11-SYORI-SEC
      *****
      *    患者確認
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 410-P032-SYORI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
           MOVE    ZERO                TO  SPA-P032-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        リスト選択
               WHEN    ANY             ALSO    "ONSHILIST"
                   PERFORM 4001-ONSHILIST-SEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4001-ONSHILIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-P032-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P032-MAX
               IF      P032-SELECTDATA (IDX) =   "T"
                   MOVE    SPA-P032-RENNUM (IDX)
                                           TO  SPA-P032-SELNUM
     ****          MOVE    IDX             TO  SPA-P032-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-P032-SELNUM    NOT =   ZERO
               PERFORM 4102-SELNUM-CHK-SEC
           END-IF
      *
           .
       4001-ONSHILIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4102-SELNUM-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P032-SELNUM         TO  SPA-P032-SELNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       4102-SELNUM-CHK-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-P032-CUR
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    SPA-P032-MAX )
                          OR  (IDX          >   ZERO  )
               IF      SPA-P032-SELNUM      =   SPA-P032-RENNUM
                                                             (IDY)
                   MOVE    IDY              TO  IDX
               END-IF
           END-PERFORM
           IF     (SPA-P032-SELNUM     =   ZERO )  OR
                  (IDX                 =   ZERO )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P032-CUR
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *R04.2
      *    削除分
           IF      SPA-P032-SCR-DLTFLG(IDX)    =   "1"
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P032-CUR
               MOVE    SPACE               TO  SPA-P032-SEL-UUID
               MOVE    ZERO                TO  SPA-P032-SELNUM
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *R03.11
      *    公費選択不可
           IF      SPA-P032-SHOKAI-KBN (IDX)   =   "9"
                                               OR  "8"
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P032-CUR
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *2024/02
      *    医療扶助対応前
           IF     (SPA-P032-CARD-CLASS (IDX)   =  "A1" )
            AND   (SPA-P032-FUJYO-KOH  (IDX)   =   "9")
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-P032-SEL-UUID
               MOVE    ZERO                TO  SPA-P032-SELNUM
               MOVE    1                   TO  SPA-P032-CUR
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *2024/02
      *    医療扶助で公費（保険あり）
           IF      SPA-P032-CARD-CLASS (IDX)   =  "A1"
            AND   (SPA-P032-FUJYO-KOH  (IDX)   =   "1")
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P032-CUR
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-P032-TBL-UUID (IDX) TO  SPA-P031-SEL-UUID
      *    オンライン資格確認照会検索
           PERFORM 4103-ONSKAKU-KEY-SEC
           IF      FLG-ONSKAKU         =   1
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE
               IF     (ONSKAKU-LOCK-KBN    =   "1"  )
      *            他端末で処理
                   MOVE    "0003"          TO  SPA-ERRCD
               END-IF
           END-IF
      *2024/04
      *    保険証照会で医療扶助併用
           IF     (SPA-P032-SHOKAI-KBN  (IDX)   =   "2")
            AND   (SPA-P032-ORCA-SHOKAI-KBN (IDX)
                                                =   SPACE)
            AND   (SPA-P032-FUJYO-HEIYO (IDX)   =   "1" )
            AND   (ONSKAKU-FUJYO-UUID  NOT =   SPACE )
            AND   (SPA-ERRCD           =   SPACE )
      *        医療扶助返却チェック
               PERFORM 410210-FUJYO-HEIYO-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE
                   MOVE    1                   TO  SPA-P032-CUR
                   GO  TO  4102-SELNUM-CHK-EXT
               END-IF
           END-IF
      *
      *    状態の詳細
      *    IF      SPA-ERRCD           =   SPACE
      *        IF     (ONSKAKU-SIKAKU-YUKO NOT =   "1"   )
      *            OR (ONSKAKU-LOSS-JIYU   NOT =   SPACE )
      *            PERFORM 41021-FOTTMSG-SEC
      *        END-IF
      *    END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    12                  TO  SPA-P032-CUR
           ELSE
               MOVE    1                   TO  SPA-P032-CUR
           END-IF
           .
      *
       4102-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認UID検索処理
      *****************************************************************
       4103-ONSKAKU-KEY-SEC         SECTION.
      *
      *    オンライン資格確認照会結果検索
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P031-SEL-UUID   TO  ONSKAKU-TBL-UUID
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
      *
       4103-ONSKAKU-KEY-EXT.
           EXIT.
      *
      *2024/04
      *****************************************************************
      *    医療扶助併用返却チェック処理
      *****************************************************************
       410210-FUJYO-HEIYO-CHK-SEC         SECTION.
      *
      *    オンライン資格確認照会結果検索
           MOVE    ONSKAKU-REC         TO  HZN-ONSKAKU-REC
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    HZN-ONSKAKU-FUJYO-UUID
                                       TO  ONSKAKU-FUJYO-UUID
           MOVE    HZN-ONSKAKU-KAKU-KAKUNINYMD
                                       TO  ONSKAKU-KAKU-KAKUNINYMD
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           MOVE    ZERO                TO  FLG-FUJYO-ARI
           PERFORM UNTIL       FLG-ONSKAKU     =   1
      *        処理結果区分
               IF     (ONSKAKU-RESULT-KBN  =   "1"  )
      *          医療扶助のみ
                 AND  (ONSKAKU-CARD-CLASS  =   "A1" )
                 AND  (ONSKAKU-FUJYO-HEIYO =   "1"  )
                   MOVE    1               TO  FLG-FUJYO-ARI
               END-IF
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    HZN-ONSKAKU-REC     TO  ONSKAKU-REC
           IF      FLG-FUJYO-ARI       =   ZERO
               MOVE    "0021"              TO  SPA-ERRCD
           END-IF
           .
       410210-FUJYO-HEIYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           IF      SPA-P032-SELNUM     =   ZERO
      *        選択なし
      *        IF      SPA-SYSYMD      NOT =   SPA-P032-SYSYMD
      *            初期表示へ
                   PERFORM 3000-SCREEN-INIT-SEC
      *         END-IF
           ELSE
               MOVE    ZERO                TO  SPA-P032-SELNUM
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
               MOVE    1                   TO  SPA-P032-CUR
      *
               MOVE    SPA-P032-SYSYMD     TO  WRK-SYMD
               MOVE    SPA-P032-SYORI      TO  WRK-SYORI
      *        オンライン資格確認照会編集処理
               PERFORM 30001-P032-INIT-SEC
           END-IF
           .
       402-CLEAR-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    表示切替処理
      *****************************************************************
       410-LISTCHNG-SEC             SECTION.
      *
           IF      SPA-P032-SYORI      =   ZERO
               MOVE    1               TO  SPA-P032-SYORI
           ELSE
               MOVE    ZERO            TO  SPA-P032-SYORI
           END-IF
      *
           MOVE    SPA-P032-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P032-SYORI      TO  WRK-SYORI
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P032-INIT-SEC
           IF      SPA-P032-MAX        =   ZERO
               MOVE    10                  TO  SPA-P032-CUR
           ELSE
               MOVE    1                   TO  SPA-P032-CUR
           END-IF
           .
       410-LISTCHNG-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    「変更なし」処理
      *****************************************************************
       411-B11-SYORI-SEC             SECTION.
      *
           IF     (SPA-P032-SELNUM     =   ZERO )
      ****        (SPA-P032-SELNUM     >   SPA-P032-MAX)
              OR  (SPA-P031-SEL-UUID   =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO   411-B11-SYORI-EXT
           END-IF
      *
      *    排他中などのチェック
           MOVE    1                   TO  FLG-CHKKBN
           PERFORM 4080-UPD-CHECK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0106"              TO  SPA-PIDCD
           END-IF
           .
      *
       411-B11-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    「変更なし」処理
      *****************************************************************
       4110-HENKOUNASI-SYORI-SEC             SECTION.
      *
      *    排他中などのチェック
           MOVE    1                   TO  FLG-CHKKBN
           PERFORM 4080-UPD-CHECK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4110-HENKOUNASI-SYORI-EXT
           END-IF
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM 4103-ONSKAKU-KEY-SEC
      *
           MOVE    SPACE               TO  WRK-OYA-UUID
           MOVE    SPACE               TO  WRK-TBL-UUID
           IF      FLG-ONSKAKU         =   ZERO
      *
      *        親ＩＤがあればすべて対象
               MOVE    ONSKAKU-OYA-UUID    TO  WRK-OYA-UUID
      *        公費照会をすべて対象
               MOVE    ONSKAKU-TBL-UUID    TO  WRK-TBL-UUID
      *        処理済み区分更新処理
               MOVE    "1"                 TO  ONSKAKU-MOD-FLG
               PERFORM 40811-ONSIKAKU-UPDATE-SEC
      *        公費照会の処理済み更新処理
               PERFORM 41102-KOUHI-MODSYORI-SEC
           END-IF
      *
           IF     (WRK-OYA-UUID    NOT =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
      *        親UUIDで検索
               INITIALIZE                      ONSKAKU-REC
               MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
               MOVE    WRK-OYA-UUID        TO  ONSKAKU-OYA-UUID
      *
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-ONSKAKU
               END-IF
               PERFORM UNTIL    FLG-ONSKAKU    =   1
      *
                    IF      ONSKAKU-MOD-FLG     NOT =   "1"
      *                処理済み区分更新処理
                       MOVE    "1"             TO  ONSKAKU-MOD-FLG
                       PERFORM 40811-ONSIKAKU-UPDATE-SEC
      *                公費照会を処理済みとする
                       MOVE    ONSKAKU-TBL-UUID    TO  WRK-TBL-UUID
      *                公費照会の処理済み更新処理
                       PERFORM 41102-KOUHI-MODSYORI-SEC
                   END-IF
      *
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               END-PERFORM
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *       一覧表最新表示
               MOVE    SPA-P032-SYSYMD     TO  WRK-SYMD
               MOVE    SPA-P032-SYORI      TO  WRK-SYORI
      *        オンライン資格確認照会編集処理
               PERFORM 30001-P032-INIT-SEC
           END-IF
      *
           .
       4110-HENKOUNASI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    オンライン資格確認　公費照会削除処理
      *****************************************************************
       41102-KOUHI-MODSYORI-SEC         SECTION.
      *
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    WRK-TBL-UUID        TO  ONSKAKU-KOUHI-UUID
      *    照会区分＝９　が地方公費
      ***  MOVE    "9"                 TO  ONSKAKU-SHOKAI-KBN
      *    kouhi-uuidでのみ検索へ
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
      ***  MOVE    "key12"             TO  MCP-PATHNAME
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           PERFORM UNTIL    FLG-ONSKAKU    =   1
      *      公費照会のみ
             IF       (ONSKAKU-SHOKAI-KBN  =   "8"
                                           OR  "9"  )
      *2024/02 医療扶助
             OR       (ONSKAKU-CARD-CLASS  =   "A1" )
      *        処理済み区分更新処理
               MOVE    "1"             TO  ONSKAKU-MOD-FLG
               PERFORM 40811-ONSIKAKU-UPDATE-SEC
             END-IF
      *
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       41102-KOUHI-MODSYORI-EXT.
           EXIT.
      **
      *****************************************************************
      *    変更なし・削除対象チェック処理
      *****************************************************************
       4080-UPD-CHECK-SEC             SECTION.
      *
      *    オンライン資格確認照会検索
           PERFORM 4103-ONSKAKU-KEY-SEC
           IF      FLG-ONSKAKU         =   1
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE
               IF     (ONSKAKU-LOCK-KBN    =   "1"  )
      *            他端末で処理
                   MOVE    "0003"          TO  SPA-ERRCD
               ELSE
      *            処理済み
                   IF      ONSKAKU-MOD-FLG     =   "1"
                       IF      FLG-CHKKBN          =   1
      *                    変更なし
                           MOVE    "0011"          TO  SPA-ERRCD
                       ELSE
      *                    削除
                           MOVE    "0010"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *        親ＩＤがあれば処理中でないことを確認
               IF     (SPA-ERRCD           NOT  =   SPACE)
                  OR  (ONSKAKU-OYA-UUID         =   SPACE)
                   GO  TO  4080-UPD-CHECK-EXT
               END-IF
      *
      *        親UUIDのチェック
               MOVE    ONSKAKU-OYA-UUID        TO  WRK-OYA-UUID
               MOVE    ONSKAKU-TBL-UUID        TO  WRK-TBL-UUID
      *        親UUIDで検索
               INITIALIZE                      ONSKAKU-REC
               MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
               MOVE    WRK-OYA-UUID        TO  ONSKAKU-OYA-UUID
      *
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-ONSKAKU
               END-IF
               PERFORM UNTIL  ( FLG-ONSKAKU    =   1  )
                          OR  ( SPA-ERRCD  NOT =   SPACE)
      *
                   IF     (ONSKAKU-LOCK-KBN    =   "1"  )
      *                他端末で処理
                       MOVE    "0003"          TO  SPA-ERRCD
                   END-IF
      *
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               END-PERFORM
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       4080-UPD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       403-DELETE-SYORI-SEC             SECTION.
      *
           IF     (SPA-P032-SELNUM     =   ZERO )
      ******      (SPA-P032-SELNUM     >   SPA-P032-MAX)
              OR  (SPA-P031-SEL-UUID   =   SPACE )
               MOVE    "0001"          TO  SPA-ERRCD
               GO      TO   403-DELETE-SYORI-EXT
           END-IF
      *
      *    排他中などのチェック
           MOVE    2                   TO  FLG-CHKKBN
           PERFORM 4080-UPD-CHECK-SEC
      *
      *  
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-PIDCD
           END-IF
           .
      *
       403-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DELETE-SEC             SECTION.
      *
      *    排他中などのチェック
           MOVE    2                   TO  FLG-CHKKBN
           PERFORM 4080-UPD-CHECK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4031-DELETE-EXT
           END-IF
      *
           PERFORM 4103-ONSKAKU-KEY-SEC
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SPACE               TO  WRK-OYA-UUID
           MOVE    SPACE               TO  WRK-TBL-UUID
           MOVE    SPACE               TO  WRK-FUJYO-UUID
           MOVE    SPACE               TO  WRK-KAKU-KAKUNINYMD
           IF      FLG-ONSKAKU         =   ZERO
      *
      *        親ＩＤがあればすべて対象
               MOVE    ONSKAKU-OYA-UUID        TO  WRK-OYA-UUID
      *        公費照会をすべて対象
               MOVE    ONSKAKU-TBL-UUID        TO  WRK-TBL-UUID
      *2024/02 医療扶助
               IF      (ONSKAKU-CARD-CLASS  =   "A1" )
                   MOVE    ONSKAKU-FUJYO-UUID   TO  WRK-FUJYO-UUID
                   MOVE    ONSKAKU-KAKU-KAKUNINYMD
                                                TO  WRK-KAKU-KAKUNINYMD
               END-IF
      *
      *        削除区分更新処理
               PERFORM 40811-ONSKAKU-DELETE-SEC
      *        公費照会の削除処理
               PERFORM 40812-KOUHI-DELSYORI-SEC
      *2024/02
      *        医療扶助をすべて削除処理
               IF     (WRK-FUJYO-UUID  NOT =   SPACE )
                  AND (SPA-ERRCD           =   SPACE )
                   PERFORM 48013-KOUHI-DELSYORI-SEC
               END-IF
           END-IF
      *
           IF     (WRK-OYA-UUID    NOT =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
      *        親UUIDで検索
               INITIALIZE                      ONSKAKU-REC
               MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
               MOVE    WRK-OYA-UUID        TO  ONSKAKU-OYA-UUID
      *
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-ONSKAKU
               END-IF
               PERFORM UNTIL    FLG-ONSKAKU    =   1
      *
                   IF      ONSKAKU-MOD-FLG     NOT =   "1"
      *                公費照会を処理済みとする
                       MOVE    ONSKAKU-TBL-UUID    TO  WRK-TBL-UUID
      *                処理中解除と更新処理
                       PERFORM 40811-ONSKAKU-DELETE-SEC
      *                公費照会の削除処理
                       PERFORM 40812-KOUHI-DELSYORI-SEC
                   END-IF
      *
                   MOVE    "key7"              TO  MCP-PATHNAME
                   PERFORM 900-ONSKAKU-READ-SEC
               END-PERFORM
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *       一覧表最新表示
               MOVE    SPA-P032-SYSYMD     TO  WRK-SYMD
               MOVE    SPA-P032-SYORI      TO  WRK-SYORI
      *        オンライン資格確認照会編集処理
               PERFORM 30001-P032-INIT-SEC
           END-IF
           .
      *
       4031-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       40811-ONSKAKU-DELETE-SEC        SECTION.
      *
           MOVE    "1"                 TO  ONSKAKU-SCR-DLTFLG
      *
           PERFORM 40811-ONSIKAKU-UPDATE-SEC
           .
       40811-ONSKAKU-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル更新処理
      *****************************************************************
       40811-ONSIKAKU-UPDATE-SEC        SECTION.
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
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "P032 ONSKAKU KEY UPD ERR:"  MCP-RC
                   ",KEY:" ONSKAKU-KEY
           END-IF
           .
       40811-ONSIKAKU-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    オンライン資格確認　公費照会削除処理
      *****************************************************************
       40812-KOUHI-DELSYORI-SEC         SECTION.
      *
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    WRK-TBL-UUID        TO  ONSKAKU-KOUHI-UUID
      *    照会区分＝９　が地方公費
      ***  MOVE    "9"                 TO  ONSKAKU-SHOKAI-KBN
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           PERFORM UNTIL    FLG-ONSKAKU    =   1
      *      公費照会のみ
             IF       (ONSKAKU-SHOKAI-KBN  =   "8"
                                           OR  "9"  )
      *        削除区分更新処理
               PERFORM 40811-ONSKAKU-DELETE-SEC
            END-IF
      *
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       40812-KOUHI-DELSYORI-EXT.
           EXIT.
      *
      **2024/02
      *****************************************************************
      *    オンライン資格確認　医療扶助削除処理
      *****************************************************************
       48013-KOUHI-DELSYORI-SEC         SECTION.
      *
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    WRK-FUJYO-UUID      TO  ONSKAKU-FUJYO-UUID
           MOVE    WRK-KAKU-KAKUNINYMD
                                       TO  ONSKAKU-KAKU-KAKUNINYMD
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           PERFORM UNTIL    FLG-ONSKAKU    =   1
      *      医療扶助 公費照会のみ
               IF       (ONSKAKU-SHOKAI-KBN  =   "8"
                                             OR  "9"  )
               OR       (ONSKAKU-CARD-CLASS  =   "A1" )
      *            削除区分更新処理
                   PERFORM 40811-ONSKAKU-DELETE-SEC
               END-IF
      *
               MOVE    "key27"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       40812-KOUHI-DELSYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       406-BACKPAGE-SEC            SECTION.
      *
           IF      SPA-P032-PAGE       >   1
               COMPUTE SPA-P032-PAGE   =   SPA-P032-PAGE
                                       -   1
      *        オンライン資格確認照会結果編集
               PERFORM 3001-ONSKAK-SYORI-SEC
           ELSE
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P032-CUR
           END-IF
           .
       406-BACKPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       407-NEXTPAGE-SEC           SECTION.
      *
           IF      SPA-P032-NEXT       =   1
               ADD     1               TO  SPA-P032-PAGE
      *        オンライン資格確認照会結果編集
               PERFORM 3001-ONSKAK-SYORI-SEC
           ELSE
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P032-CUR
           END-IF
           .
       407-NEXTPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日処理
      *****************************************************************
        405-BACKDAY-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-P032-SELNUM
           MOVE    1                   TO  SPA-P032-CUR
      *    １日前
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    SPA-P032-SYSYMD  TO  LNK-DAY6-KIJUN
           MOVE    "1"              TO  LNK-DAY6-ZOGENPTN
           MOVE    -1               TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF      STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  SPA-P032-SYSYMD
           END-IF
      *    オンライン資格確認照会結果編集
           MOVE    1                   TO  SPA-P032-PAGE
           PERFORM 3001-ONSKAK-SYORI-SEC
           IF      SPA-P032-MAX        =   ZERO
               MOVE    99                  TO  SPA-P032-CUR
           ELSE
               MOVE    1                   TO  SPA-P032-CUR
           END-IF
           .
      *
        405-BACKDAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    次日処理
      *****************************************************************
        406-NEXTDAY-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-P032-SELNUM
           MOVE    1                   TO  SPA-P032-CUR
      *    １日前
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    SPA-P032-SYSYMD  TO  LNK-DAY6-KIJUN
           MOVE    "1"              TO  LNK-DAY6-ZOGENPTN
           MOVE    1                TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF      STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  SPA-P032-SYSYMD
           END-IF
      *    オンライン資格確認照会結果編集
           MOVE    1                   TO  SPA-P032-PAGE
           PERFORM 3001-ONSKAK-SYORI-SEC
           IF      SPA-P032-MAX        =   ZERO
               MOVE    99                  TO  SPA-P032-CUR
           ELSE
               MOVE    1                   TO  SPA-P032-CUR
           END-IF
           .
        406-NEXTDAY-EXT.
           EXIT.
      *
      *****************************************************************
      *     患者確認画面処理
      *****************************************************************
       410-P032-SYORI-SEC             SECTION.
      *
      *
           IF      SPA-P031-SEL-UUID   =   SPACE
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
      *        オンライン資格確認照会検索
               PERFORM 4103-ONSKAKU-KEY-SEC
               IF      FLG-ONSKAKU         =   1
                   MOVE    "0002"          TO  SPA-ERRCD
               ELSE
                   IF     (ONSKAKU-LOCK-KBN    =   "1" )
      *                他端末で処理
                       MOVE    "0003"          TO  SPA-ERRCD
                   ELSE
      *                該当なしの場合、エラー
                       IF    (ONSKAKU-SIKAKU-YUKO      =   "4"   )
                        OR   (ONSKAKU-RES-HKNJANUM     =   SPACE )
      *2025/03
      *****                MOVE    "0006"          TO  SPA-ERRCD
                           MOVE    "0007"          TO  SPA-ERRCD
                       END-IF
      *2024/2
      *                医療扶助該当なしの場合、エラー
                       IF    (ONSKAKU-SIKAKU-YUKO      =   "6"   )
                        AND  (ONSKAKU-CARD-CLASS       =   "A1"  )
                           MOVE    "0019"          TO  SPA-ERRCD
                       END-IF
      *2024/05
      *                顔認証で生年月日空白
                       IF    (SPA-ERRCD            =   SPACE )
                         AND (ONSKAKU-SHOKAI-KBN   =   "1"   )
                         AND (ONSKAKU-RES-BIRTHDAY =   SPACE )
                           MOVE    "0006"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
      *2023/11     予約照会はエラー
                   IF     (ONSKAKU-SHOKAI-KBN      =   "2" )
                      AND (ONSKAKU-ORCA-SHOKAI-KBN =   "1" )
                       MOVE    "0012"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1               TO  SPA-P032-CUR
               GO      TO  410-P032-SYORI-EXT
           END-IF
      *
      *    処理中の設定
           IF      FLG-ONSKAKU         =   ZERO
      *
               INITIALIZE                  ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"       USING
                                           ORCSMCNDATEAREA
               PERFORM 4103-ONSKAKU-KEY-SEC
      *
               MOVE    "1"                 TO  ONSKAKU-LOCK-KBN
               MOVE    SPA-OPID            TO  ONSKAKU-OPID
               MOVE    SMCNDATE-YMD        TO  ONSKAKU-UPYMD
               MOVE    SMCNDATE-HMS        TO  ONSKAKU-UPHMS
      *
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "P032 ONSKAKU KEY UPD ERR:"  MCP-RC
                       ",KEY:" ONSKAKU-KEY
                   GO      TO  410-P032-SYORI-EXT
               END-IF
           END-IF
      *TEST
      *****
      *R04.1
      *    保険証確認で限度額認定証情報があるとき
           IF     (ONSKAKU-SHOKAI-KBN      =   "2"  )
      *R05.2
      *        保険証OCRでないこと
             AND  (ONSKAKU-ORCA-SHOKAI-KBN NOT =   "2" )
               IF     (ONSKAKU-GENDO-SHOKBN    =   SPACE
                                               OR  ZERO  )
                   CONTINUE
               ELSE
                   MOVE    "0105"              TO  SPA-PIDCD
                   GO      TO  410-P032-SYORI-EXT
               END-IF
           END-IF
      *    P031へ
           MOVE    SPACE       TO  SPA-P031-GENDO-DOUIFLG
           PERFORM 4101-P033-CALL-SEC
      *
           .
       410-P032-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     強制資格確認処理
      *****************************************************************
       4101-P033-CALL-SEC            SECTION.
      *
      *    初期画面編集
           INITIALIZE                      SPA-P033-AREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    "P032"              TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP033"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    "P032"              TO  SPA-MOTOPG
           MOVE    "P033"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P033"              TO  MCP-WINDOW
      *
           IF      SPA-P033-MAX        =   ZERO
               MOVE    "B08"               TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
               IF      SPA-P033-CUR        =   10
                   MOVE    "B10"               TO  MCP-WIDGET
               END-IF
               IF      SPA-P033-CUR        =   12
                   MOVE    "B12"               TO  MCP-WIDGET
               END-IF
           END-IF
      *
           PERFORM 900-PUT-WINDOW
      *
      *
           MOVE    1                   TO  FLG-END
           .
       4101-P033-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *     強制資格確認処理
      *****************************************************************
       408-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                  ORCSONSHI001AREA
           MOVE    SPA-PTID            TO  LNK-ONSHI001-PTID
           MOVE    "03"                TO  LNK-ONSHI001-SYOKBN
           MOVE    "12"                TO  LNK-ONSHI001-GYOUMUCD
           CALL    "ORCSONSHI001"      USING   ORCSONSHI001AREA
                                               SPA-AREA
           IF      LNK-ONSHI001-KAKUNIN-FLG    =   "Yes"
               MOVE    "0100"              TO  SPA-ERRCD
           ELSE
      *R05.7
               IF      LNK-ONSHI001-KAKUNIN-FLG    =   "Ng"
                   MOVE    "0102"              TO  SPA-ERRCD
               ELSE
                   MOVE    "0101"              TO  SPA-ERRCD
               END-IF
           END-IF
      * 
      *    初期表示へ
           PERFORM 3000-SCREEN-INIT-SEC
           .
       408-KAKUNIN-EXT.
           EXIT. 
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       4001-SEIWA-YMDEDIT-SEC           SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE           TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "0000000"       TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "9999999"       TO  WRK-HENYMD
               WHEN    OTHER
      *
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   ELSE
                       MOVE    SPACE               TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
           .
       4001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P032"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    ZERO                TO  SPA-FLG-END
           MOVE    "P02"               TO  SPA-P02-GAMEN
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P032"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING  "対象のオンライン資格確認データが"
                           "存在しません。再度一覧表示をして下さい"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "他端末で処理中です。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "前頁はありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "次頁はありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   STRING  "資格確認エラーです。選択できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *2025/03
               WHEN    "0007"
                   STRING  "資格で「該当なし」となっています。"
                           "選択できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *R03.11
               WHEN    "0008"
                   STRING  "公費照会のデータです。選択できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING 
      *R04.02
               WHEN    "0009"
                   STRING  "削除済みのデータです。選択できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "処理済みです。削除できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1001"
                   STRING  "オンライン資格確認データ更新が"
                           "できませんでした。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0011"
                   STRING  "既に処理済みです。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *2023/11
               WHEN    "0012"
                   STRING  "予約照会データです。処理できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *2024/02
               WHEN    "0018"
                   MOVE    "医療扶助対応前の情報です。選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "医療扶助未登録のため、選択できません。"
                                               TO  SPA-ERRMSG
      *R03.5 
               WHEN    "0100"
                   STRING  "資格確認を照会しました。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0101"
                   STRING  "資格対象外の保険のみ、"
                           "または当日資格確認済みの返却があります。"
                           "照会しません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *R05.7
               WHEN    "0102"
                   STRING  "保険の番号が未入力です。"
                           "照会しません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *2024/04
               WHEN    "0021"
                   STRING  "保険併用の医療扶助照会中です。"
                           "選択できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P032"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PIDMSG
           EVALUATE    SPA-PIDCD
      ****
      *R04.1
               WHEN    "0105"
                   STRING  "限度額情報が設定されています。"
                           "患者さんの同意が得られていますか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *
      *
               WHEN    "0101"
                   STRING  "選択した情報と関係するデータをすべて"
                           "削除します。"
                           "よろしいですか？"  DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *R04.2
               WHEN    "0106"
                   STRING  "選択した情報を処理済みとします。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "戻る"              TO  PID1-B01
           MOVE    "ＯＫ"              TO  PID1-B12
           IF      SPA-PIDCD           =   "0105"
               MOVE    "ＮＯ"              TO  PID1-B01
           END-IF
      *
           MOVE    "B12"               TO  MCP-WIDGET
           IF      SPA-PIDCD           =   "0105"
                                       OR  "0101"
              MOVE    "B01"            TO  MCP-WIDGET
           END-IF
      *
           MOVE    "P032"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認照会結果読込
      *****************************************************************
       900-ONSKAKU-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ONSKAKU-REC
               MOVE    ZERO            TO  FLG-ONSKAKU
           ELSE
               INITIALIZE                  ONSKAKU-REC
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           .
       900-ONSKAKU-READ-EXT.
           EXIT.
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
      *    ＤＢクローズ処理
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
      *
