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
       PROGRAM-ID.             ORCGP031.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : オンライン資格確認一覧画面（Ｐ０３１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  20/09/XX    NMED-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     ORCAMO       21/05/XX  該当なし対応
      *                                     端末処理中解除対応他
      * 05.00.00     ORCAMO       21/08/XX  資格有効性の背景色対応
      * 05.01.00     ORCAMO       21/11/XX  オン資格確
      *                                     公費照会対応
      * 05.01.00     ORCAMO       22/01/XX  資格確認限度額承認対応
      * 05.01.00     ORCAMO       22/02/XX  変更無し、削除対応
      * 05.01.00     ORCAMO       22/07/XX  照会番号表示対応
      * 05.01.00     ORCAMO       23/03/XX  薬剤情報など同意表示対応
      * 05.02.00     ORCAMO       23/11/XX  本人家族・負担割など表示対応
      * 05.02.00     ORCAMO       24/02/XX  医療扶助対応
      * 05.02.00     ORCAMO       24/12/XX  訪問診療・オン診療対応
      * 05.02.00     ORCAMO       25/05/XX  一覧更新タイマー対応
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
           03  FLG-TERR            PIC 9(01).
      *R04.2
           03  FLG-CHKKBN          PIC 9(01).
      *????
           03  FLG-INCHK           PIC 9(01).
           03  FLG-CHNGKBN         PIC 9(01).
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
           03  WRK-SYORI           PIC 9(02).
           03  WRK-SYORI2          PIC 9(01).
      *
           03  WRK-CHNGKBN         PIC X(30).
           03  WRK-CHNGKBN-KBN     PIC X(02).
      *
           03  WRK-PTID            PIC 9(10).
      *
      *     処理実行日時
           03  WRK-SHORI-TIME-IN.
               05  WRK-SHORI-IN-YY     PIC X(04).
               05  WRK-SHORI-IN-MM     PIC X(02).
               05  WRK-SHORI-IN-DD     PIC X(02).
               05  WRK-SHORI-IN-THH    PIC X(02).
               05  WRK-SHORI-IN-TMM    PIC X(02).
           03  WRK-BIRTHDAY        PIC X(08).
      *
           03  WRK-FOOTMSG1            PIC X(100).
           03  WRK-FOOTMSG2            PIC X(100).
           03  WRK-MIDASI2             PIC X(100).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *R03.05
           03   WRK-CHK-TIME.
                05  WRK-CHH             PIC 9(02).
                05  WRK-CMM             PIC 9(02).
                05  WRK-CSS             PIC 9(02).
      *
           03  WRK-OYA-UUID            PIC X(36).
           03  WRK-TBL-UUID            PIC X(36).
      *
           03  WRK-SEL-UUID            PIC X(36).
           03  WRK-ON-RATE             PIC 9(03).
           03  WRK-FUJYO-UUID          PIC X(36).
           03  WRK-KAKU-KAKUNINYMD     PIC X(08).
      *
           03  WRK-IKTKBN          PIC X(01).
      *
      *   年齢表示
       01  WRK-NENREI-AREA.
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(04).
               05  WRK-NENREI-MM       PIC 9(02).
               05  WRK-NENREI-DD       PIC 9(02).
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(03).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(04).
      *
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
      *2024/04
           COPY    "ENUM-VALUE".
      *
      *    オンライン資格確認照会結果テーブル
       01  HZN-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"  REPLACING
                                      //ONSKAKU-// BY //HZN-ONSKAKU-//.
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
      *R05.11
      *    患者保険情報とのチェック
           COPY    "CPORCSONSHI003.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
               MOVE    "P031"              TO  MCP-WINDOW
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
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
      *R03.5
                   IF    ( SPA-ERRCD           =   SPACE )
                   AND   ( SPA-PIDCD       NOT =   SPACE )
                       PERFORM 520-JIDSET-SEC
                   END-IF
      ******
      *
               WHEN    "P033"
                   PERFORM 3002-P033-INIT-SEC
      *
               WHEN    "P035"
                   PERFORM 3002-P035-INIT-SEC
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
      *R03.5
                   IF    ( SPA-ERRCD           =   SPACE )
                   AND   ( SPA-PIDCD       NOT =   SPACE )
                       PERFORM 520-JIDSET-SEC
                   END-IF
      ******
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
               MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
               MOVE    SPA-P031-SYORI      TO  WRK-SYORI
               MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
      *        オンライン資格確認照会編集処理
               PERFORM 30001-P031-INIT-SEC
               MOVE    1                   TO  SPA-P031-CUR
           ELSE
               PERFORM 210-BACK
           END-IF
           .
       3002-P033-INIT-EXT.
           EXIT.
      *****************************************************************
      *    保険証ＯＣＲ照会画面より
      *****************************************************************
       3002-P035-INIT-SEC            SECTION.
      *
      *????
      *    選択を継続
           MOVE    SPACE              TO  WRK-SEL-UUID
           IF     (SPA-P035-NEXTFLG   =   "1"       )
               MOVE    SPA-P031-SEL-UUID   TO  WRK-SEL-UUID
           END-IF
      *????
      *    オンライン資格選択中の解除
           IF      (SPA-P031-SEL-UUID  NOT =   SPACE )
               PERFORM 30009-ONSHIKAKU-CR-SEC
           END-IF
      *
           MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P031-SYORI      TO  WRK-SYORI
           MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC
           MOVE    1                   TO  SPA-P031-CUR
      *
      *????
      *????
           IF      WRK-SEL-UUID        =   SPACE
               GO  TO  3002-P035-INIT-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-P031-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >    SPA-P031-MAX )
                          OR  (SPA-P031-SELNUM     >   ZERO  )
               IF      SPA-P031-TBL-UUID (IDX)
                                            =   WRK-SEL-UUID
                   MOVE    SPA-P031-RENNUM (IDX)
                                            TO  SPA-P031-SELNUM
               END-IF
           END-PERFORM
           IF     (SPA-P031-SELNUM     NOT =   ZERO )
               PERFORM 4102-SELNUM-CHK-SEC
               PERFORM 410-P033-SYORI-SEC
           END-IF
           .
       3002-P035-INIT-EXT.
           EXIT.
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
           MOVE    ZERO                TO  WRK-SYORI2
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC
           IF      SPA-P031-MAX        =   ZERO
               MOVE    99                  TO  SPA-P031-CUR
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認照会結果編集処理
      *****************************************************************
       30001-P031-INIT-SEC              SECTION.
      *
      *    ＳＰＡ編集
           INITIALIZE                      SPA-P031-AREA
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
                                      INTO  SPA-P031-MIDASI
           END-STRING
      *2023/11
           PERFORM 30002-P031-CHNGLST-SEC
      *
           MOVE    WRK-SYMD            TO  SPA-P031-SYSYMD
           MOVE    WRK-SYORI           TO  SPA-P031-SYORI
           MOVE    WRK-SYORI2          TO  SPA-P031-SYORI2
           MOVE    1                   TO  SPA-P031-PAGE
           PERFORM 3001-ONSKAK-SYORI-SEC
           IF      SPA-P031-MAX        =   ZERO
               MOVE    99                  TO  SPA-P031-CUR
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認照会結果編集処理
      *****************************************************************
       30002-P031-CHNGLST-SEC              SECTION.
      *
      *    表示内容リスト
           MOVE    "★ 顔認証照会"         TO  SPA-P031-CHNGKBN-L-MEI
                                                              (1)
           MOVE    "01"                     TO  SPA-P031-CHNGKBN-L-KBN
                                                              (1)
           MOVE    "◎ 登録済保険情報"     TO  SPA-P031-CHNGKBN-L-MEI
                                                              (2)
           MOVE    "04"                     TO  SPA-P031-CHNGKBN-L-KBN
                                                              (2)
           MOVE    "◆ 保険証ＯＣＲ照会"   TO  SPA-P031-CHNGKBN-L-MEI
                                                              (3)
           MOVE    "05"                     TO  SPA-P031-CHNGKBN-L-KBN
                                                              (3)
           MOVE    "公 公費ＯＣＲ照会"     TO  SPA-P031-CHNGKBN-L-MEI
                                                              (4)
           MOVE    "03"                     TO  SPA-P031-CHNGKBN-L-KBN
                                                              (4)
           MOVE    4                   TO  SPA-P031-CHNGKBN-MAX
      *2024/12
           IF     (SPA-ONSHI-HOUMON    =   "1"  )
               ADD    1                    TO  SPA-P031-CHNGKBN-MAX
               MOVE    "訪 訪問診療"       TO  SPA-P031-CHNGKBN-L-MEI
                                               (SPA-P031-CHNGKBN-MAX)
               MOVE    "07"                TO  SPA-P031-CHNGKBN-L-KBN
                                               (SPA-P031-CHNGKBN-MAX)
      *
               ADD    1                    TO  SPA-P031-CHNGKBN-MAX
               MOVE    "訪 訪問診療同意"   TO  SPA-P031-CHNGKBN-L-MEI
                                               (SPA-P031-CHNGKBN-MAX)
               MOVE    "08"                TO  SPA-P031-CHNGKBN-L-KBN
                                               (SPA-P031-CHNGKBN-MAX)
           END-IF
           IF     (SPA-ONSHI-ONLINE    =   "1"  )
               ADD    1                    TO  SPA-P031-CHNGKBN-MAX
               MOVE    "オ オンライン診療"
                                           TO  SPA-P031-CHNGKBN-L-MEI
                                               (SPA-P031-CHNGKBN-MAX)
               MOVE    "09"                TO  SPA-P031-CHNGKBN-L-KBN
                                               (SPA-P031-CHNGKBN-MAX)
      *
               ADD    1                    TO  SPA-P031-CHNGKBN-MAX
               MOVE    "オ オンライン診療同意"
                                           TO  SPA-P031-CHNGKBN-L-MEI
                                               (SPA-P031-CHNGKBN-MAX)
               MOVE    "10"                TO  SPA-P031-CHNGKBN-L-KBN
                                               (SPA-P031-CHNGKBN-MAX)
           END-IF
      ***  MOVE    "■ 予約照会"           TO  SPA-P031-CHNGKBN-L-MEI
      *                                                        (5)
      *    MOVE    "06"                    TO  SPA-P031-CHNGKBN-L-KBN
      *                                                        (5)
      **** MOVE    5                   TO  SPA-P031-CHNGKBN-MAX
      *
           MOVE    SPACE               TO  SPA-P031-CHNGKBN-G
           MOVE    SPACE               TO  SPA-P031-CHNGKBN-KBN
      *
           MOVE    WRK-CHNGKBN         TO  SPA-P031-CHNGKBN-G
           MOVE    WRK-CHNGKBN-KBN     TO  SPA-P031-CHNGKBN-KBN
           .
       30002-P031-CHNGLST-EXT.
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
           MOVE    SPA-P031-SYSYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  SPA-P031-HSYSYMD
      *
           INITIALIZE                  SPA-P031-LIST-G
      *
      *    オンライン資格確認照会結果検索
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P031-SYSYMD     TO  ONSKAKU-KAKUNINYMD
      *2022/09
      *    予約照会以外を対象
           MOVE    "1"                 TO  ONSKAKU-ORCA-SHOKAI-KBN
      *
      *
           EVALUATE    SPA-P031-SYORI
               WHEN    0
      *            全体
                   MOVE    "key6"          TO  WRK-MCP-PATHNAME
      *            全体の時、削除分も表示する
                   IF      SPA-P031-SYORI2     =   1
                       MOVE    "key16"         TO  WRK-MCP-PATHNAME
                   END-IF
               WHEN    1
      *            マイナンバーカード照会
                   MOVE    "1"             TO  ONSKAKU-SHOKAI-KBN
                   MOVE    "key2"          TO  WRK-MCP-PATHNAME
               WHEN    2
      *            被保険者証照会・保険証OCR
                   MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
                   MOVE    "key2"          TO  WRK-MCP-PATHNAME
               WHEN    3
      *            公費照会
      ***          MOVE    "9"             TO  ONSKAKU-SHOKAI-KBN
      ***          MOVE    "key2"          TO  WRK-MCP-PATHNAME
      *            9 と 8 を照会する
                   MOVE    "key18"          TO  WRK-MCP-PATHNAME
      *2023/11 リストで選択
               WHEN    4
      *            被保険者証照会
                   MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
                   MOVE    "key2"          TO  WRK-MCP-PATHNAME
               WHEN    5
      *            保険証OCR
                   MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
                   MOVE    "key2"          TO  WRK-MCP-PATHNAME
      *2024/12
               WHEN    7
      *            訪問診療
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-KBN
      *            診療区分
                   MOVE    "2"             TO  ONSKAKU-ONSHINRYO-KBN
      *            一括同意識別
                   MOVE    SPACE           TO  ONSKAKU-PROCESS-CLASS
                   MOVE    "key31"         TO  WRK-MCP-PATHNAME
               WHEN    8
      *            訪問診療同意
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-KBN
      *            診療区分
                   MOVE    "2"             TO  ONSKAKU-ONSHINRYO-KBN
      *            一括同意識別
                   MOVE    "2"             TO  ONSKAKU-PROCESS-CLASS
                   MOVE    "key31"         TO  WRK-MCP-PATHNAME
               WHEN    9
      *            オンライン診療
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-KBN
      *            診療区分
                   MOVE    "3"             TO  ONSKAKU-ONSHINRYO-KBN
      *            一括同意識別
                   MOVE    SPACE           TO  ONSKAKU-PROCESS-CLASS
                   MOVE    "key31"         TO  WRK-MCP-PATHNAME
               WHEN    10
      *            オンライン診療
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-KBN
      *            診療区分
                   MOVE    "3"             TO  ONSKAKU-ONSHINRYO-KBN
      *            一括同意識別
                   MOVE    "3"             TO  ONSKAKU-PROCESS-CLASS
                   MOVE    "key31"         TO  WRK-MCP-PATHNAME
      *??      WHEN    6
      *            予約照会
      *             MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
      *             MOVE    "key25"         TO  WRK-MCP-PATHNAME
      *??           MOVE    "1"             TO  ONSKAKU-ORCA-SHOKAI-KBN
           END-EVALUATE
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           MOVE    ZERO                TO  SPA-P031-NEXT
           IF      SPA-P031-PAGE       =   ZERO
               MOVE    1               TO  SPA-P031-PAGE
           END-IF
           COMPUTE IDX-STR             =  (SPA-P031-PAGE   -  1)
                                       *   50
                                       +   1
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDY2
      *
           PERFORM UNTIL   (FLG-ONSKAKU        =    1   )
                       OR  (SPA-P031-NEXT      =    1   )
               MOVE    ZERO                TO  FLG-DATAOK
      *        処理結果区分
               IF     (ONSKAKU-RESULT-KBN  =   "1"  )
      *            未処理のみ対象
                   IF     (SPA-P031-SYORI2     =   ZERO )
                     AND  (ONSKAKU-MOD-FLG     =   SPACE)
                       MOVE    1               TO  FLG-DATAOK
                   END-IF
      *            全部対象
                   IF     (SPA-P031-SYORI2     =   1    )
                       MOVE    1               TO  FLG-DATAOK
                   END-IF
      *2023/11
      *            被保険証照会対象
                   IF     SPA-P031-SYORI       =   4
                       IF      ONSKAKU-ORCA-SHOKAI-KBN   =   SPACE
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-DATAOK
                       END-IF
                   END-IF
      *            保険証OCR対象
                   IF     SPA-P031-SYORI       =   5
                       IF      ONSKAKU-ORCA-SHOKAI-KBN   =   "2"
                                                         OR  "3"
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-DATAOK
                       END-IF
                   END-IF
      *2024/12
      *            全体
                   IF     SPA-P031-SYORI       =   0
      *                訪問診療同意は対象外
                       IF      (ONSKAKU-SHOKAI-KBN     =   SPACE )
                         AND   (ONSKAKU-ONSHINRYO-KBN  =   "2"   )
                         AND   (ONSKAKU-PROCESS-CLASS  =   "2"   )
                           MOVE    ZERO            TO  FLG-DATAOK
                       END-IF
      *                オンライン診療同意は対象外
                       IF      (ONSKAKU-SHOKAI-KBN     =   SPACE )
                         AND   (ONSKAKU-ONSHINRYO-KBN  =   "3"   )
                         AND   (ONSKAKU-PROCESS-CLASS  =   "3"   )
                           MOVE    ZERO            TO  FLG-DATAOK
                       END-IF
                   END-IF
      *
      *            訪問診療
                   IF     SPA-P031-SYORI       =   07
                       IF      (ONSKAKU-SHOKAI-KBN     =   SPACE )
                         AND   (ONSKAKU-ONSHINRYO-KBN  =   "2"   )
                         AND   (ONSKAKU-PROCESS-CLASS  =   SPACE )
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-DATAOK
                       END-IF
                   END-IF
      *            訪問診療同意
                   IF     SPA-P031-SYORI       =   08
                       IF      (ONSKAKU-SHOKAI-KBN     =   SPACE )
                         AND   (ONSKAKU-ONSHINRYO-KBN  =   "2"   )
                         AND   (ONSKAKU-PROCESS-CLASS  =   "2"   )
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-DATAOK
                       END-IF
                   END-IF
      *            オンライン診療
                   IF     SPA-P031-SYORI       =   09
                       IF      (ONSKAKU-SHOKAI-KBN     =   SPACE )
                         AND   (ONSKAKU-ONSHINRYO-KBN  =   "3"   )
                         AND   (ONSKAKU-PROCESS-CLASS  =   SPACE  )
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-DATAOK
                       END-IF
                   END-IF
      *            オンライン診療同意
                   IF     SPA-P031-SYORI       =   10
                       IF      (ONSKAKU-SHOKAI-KBN     =   SPACE )
                         AND   (ONSKAKU-ONSHINRYO-KBN  =   "3"   )
                         AND   (ONSKAKU-PROCESS-CLASS  =   "3"   )
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  FLG-DATAOK
                       END-IF
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
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    見出し
           MOVE    SPACE                TO  SPA-P031-MIDASI2
           EVALUATE    SPA-P031-SYORI
               WHEN    0
                   MOVE    "【全　　体】"
                                           TO  WRK-MIDASI2
               WHEN    1
                   MOVE    "【顔認証照会】"
                                           TO  WRK-MIDASI2
               WHEN    2
                   MOVE    "【被保険者証照会】"
                                           TO  WRK-MIDASI2
               WHEN    3
                   MOVE    "【公費照会】"
                                           TO  WRK-MIDASI2
      *2023/11
               WHEN    4
                   MOVE    "【登録済保険情報】"
                                           TO  WRK-MIDASI2
               WHEN    5
                   MOVE    "【保険証ＯＣＲ照会】"
                                           TO  WRK-MIDASI2
      *2024/12
               WHEN    7
                   MOVE    "【訪問診療】"
                                           TO  WRK-MIDASI2
               WHEN    8
                   MOVE    "【訪問診療同意】"
                                           TO  WRK-MIDASI2
               WHEN    9
                   MOVE    "【オンライン診療】"
                                           TO  WRK-MIDASI2
               WHEN    10
                   MOVE    "【オンライン診療同意】"
                                           TO  WRK-MIDASI2
      *
               WHEN    6
                   MOVE    "【予約照会】"
                                           TO  WRK-MIDASI2
           END-EVALUATE
           IF      SPA-P031-SYORI2     =   1
               STRING  WRK-MIDASI2
                       "（全対象）"        DELIMITED BY  SPACE
                                           INTO  SPA-P031-MIDASI2
               END-STRING
           ELSE
               MOVE     WRK-MIDASI2        TO  SPA-P031-MIDASI2
           END-IF
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
               MOVE    1               TO  SPA-P031-NEXT
               GO      TO  3002-ONSAKA-HEN-EXT
           END-IF
      *2023/01
      ***  MOVE    IDY                 TO  SPA-P031-RENNUM  (IDX)
           MOVE    IDY2                TO  SPA-P031-RENNUM  (IDX)
      *    ＵＩＤ
           MOVE    ONSKAKU-TBL-UUID    TO  SPA-P031-TBL-UUID(IDX)
           MOVE    ONSKAKU-SHOKAI-KBN  TO  SPA-P031-SHOKAI-KBN(IDX)
           EVALUATE    ONSKAKU-SHOKAI-KBN
               WHEN    "1"
      *            顔認証
                   MOVE    "★"        TO  SPA-P031-SHOKAI-MEI(IDX)
               WHEN    "2"
      *            保険証
                   MOVE    "◎"        TO  SPA-P031-SHOKAI-MEI(IDX)
      *2022/09
                   IF      ONSKAKU-ORCA-SHOKAI-KBN     =  "2"
                                                       OR "3"
      *                保険証ＯＣＲ
                       MOVE    "◆"       TO  SPA-P031-SHOKAI-MEI(IDX)
                   END-IF
      *2023/11
                   IF      ONSKAKU-ORCA-SHOKAI-KBN     =  "1"
      *                予約照会
                       MOVE    "■"       TO  SPA-P031-SHOKAI-MEI(IDX)
                   END-IF
               WHEN    "9"
      *            地方公費
                   MOVE    "公"        TO  SPA-P031-SHOKAI-MEI(IDX)
      *2022/09
               WHEN    "8"
      *            地方公費(保険証ＯＣＲ)
                   MOVE    "公"        TO  SPA-P031-SHOKAI-MEI(IDX)
      *2024/12
               WHEN    SPACE
                   MOVE    SPACE       TO  SPA-P031-SHOKAI-MEI(IDX)
                   IF      ONSKAKU-ONSHINRYO-KBN   =   "2"
      *                訪問診療
                       MOVE    "訪"    TO  SPA-P031-SHOKAI-MEI(IDX)
                   END-IF
                   IF      ONSKAKU-ONSHINRYO-KBN   =   "3"
      *                オンライン診療
                       MOVE    "オ"    TO  SPA-P031-SHOKAI-MEI(IDX)
                   END-IF
               WHEN    OTHER
                   MOVE    SPACE       TO  SPA-P031-SHOKAI-MEI(IDX)
           END-EVALUATE
      *    保険証OCR
           MOVE    ONSKAKU-ORCA-SHOKAI-KBN
                                       TO  SPA-P031-ORCA-SHOKAI-KBN
                                                              (IDX)
      *2024/12
      *    診療区分
           MOVE    ONSKAKU-ONSHINRYO-KBN
                                       TO  SPA-P031-ONSHINRYO-KBN
                                                              (IDX)
      *
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
           MOVE    WRK-SHORI-TIME-HS   TO  SPA-P031-SHORITIME(IDX)
      *    ＵＩＤ
           MOVE    ONSKAKU-TBL-UUID    TO  SPA-P031-TBL-UUID(IDX)
      *R04.7
      *    照会番号
           MOVE    ONSKAKU-SHOKAI-NUM  TO  SPA-P031-SHOKAI-NUM(IDX)
      *
           IF     (ONSKAKU-SHOKAI-KBN      =   "2"   )
             AND  (ONSKAKU-RES-HKNJANUM    =   SPACE )
      *        保険証照会 確認結果なし
               PERFORM 30022-SHOKAI-HEN-SEC
           ELSE
      *        確認結果編集
               PERFORM 30021-RESULT-HEN-SEC
           END-IF
      *
      *    保険証照会
           IF     (ONSKAKU-SHOKAI-KBN      =   "2"   )
      *R03.8    有効の時のみ
             AND  (ONSKAKU-SIKAKU-YUKO     =   "1"   )
      *R05.2   医療扶助以外
             AND  (ONSKAKU-CARD-CLASS  NOT =   "A1" )
      *        枝番不一致
               IF      ONSKAKU-SHO-EDABAN  NOT =  ONSKAKU-RES-EDABAN
      *******      MOVE    1               TO  SPA-P031-CHKEDABAN(IDX)
                   MOVE    1               TO  SPA-P031-TJYOTAIFLG(IDX)
               END-IF
           END-IF
      *2023/11
      *    患者保険情報との一致
           IF      (ONSKAKU-SIKAKU-YUKO    =   "1"   )
              AND  (ONSKAKU-SHOKAI-KBN     =   "1"
                                           OR  "2"   )
              AND  (ONSKAKU-PTID       NOT =   ZERO  )
              AND  (ONSKAKU-ORCA-SHOKAI-KBN
                                       NOT =  "1"    )
      *        有効で、患者番号が決定済み（予約照会以外）
               PERFORM 30023-HKNINF-CHKHEN-SEC
           END-IF
      *2024/12
      *    訪問診療・オン診療　患者保険情報との一致
           IF      (ONSKAKU-SIKAKU-YUKO    =   "1"   )
              AND ((ONSKAKU-SHOKAI-KBN     =   SPACE )  AND
                   (ONSKAKU-ONSHINRYO-KBN  =   "2"
                                           OR  "3"  ))
              AND  (ONSKAKU-PTID       NOT =   ZERO  )
              AND  (ONSKAKU-ORCA-SHOKAI-KBN
                                       NOT =  "1"    )
      *        有効で、患者番号が決定済み（予約照会以外）
               PERFORM 30023-HKNINF-CHKHEN-SEC
           END-IF
      *    
      *
           MOVE    ONSKAKU-CARD-CLASS  TO  SPA-P031-CARD-CLASS (IDX)
      *2024/04    扶助併用
           MOVE    ONSKAKU-FUJYO-HEIYO TO  SPA-P031-FUJYO-HEIYO (IDX)
      *2024/02
           IF     (ONSKAKU-CARD-CLASS   =   "A1" )
      *       FUJYO_UUID 空白は選択エラー
              IF       ONSKAKU-FUJYO-UUID  =   SPACE
                  MOVE    "9"              TO  SPA-P031-FUJYO-KOH(IDX)
      *2025/03
      *           「資格無」は選択可とする
                  IF     (ONSKAKU-SIKAKU-YUKO  =   "4"  )
                      MOVE    "0"          TO  SPA-P031-FUJYO-KOH(IDX)
                  END-IF
              ELSE
                  IF  (ONSKAKU-KOUHI-UUID  NOT =   SPACE )
      *           保険の有無をFUJYO_UUID で判定
                  AND (ONSKAKU-KOUHI-UUID  NOT =   ONSKAKU-FUJYO-UUID)
      *                保険あり
                       MOVE    "1"         TO  SPA-P031-FUJYO-KOH (IDX)
                  END-IF
      *2024/04
      *           保険扶助併用
                  IF   (ONSKAKU-FUJYO-HEIYO    =   "1"   )
      *                保険あり
                       MOVE    "1"         TO  SPA-P031-FUJYO-KOH (IDX)
                   END-IF
               END-IF
           END-IF
      *
      *    予約照会　患者保険情報との一致
           IF      (ONSKAKU-SIKAKU-YUKO    =   "1"   )
              AND  (ONSKAKU-SHOKAI-KBN     =   "2"   )
              AND  (ONSKAKU-ORCA-SHOKAI-KBN
                                           =  "1"    )
              AND  (ONSKAKU-PTID       NOT =   ZERO  )
      *        有効の時枝番一致のチェック
               PERFORM 30024-PTHKNINF-YYKHEN-SEC
      *        「被保険者資格証明書」の背景色表示なし
               MOVE    SPACE               TO  SPA-P031-CARD-CLASS (IDX)
      *
           END-IF
      *
      *
      *    資格有効
           EVALUATE    ONSKAKU-SIKAKU-YUKO
               WHEN    "1"
                   MOVE    "有効"      TO  SPA-P031-SIKAKU-YUKO(IDX)
               WHEN    "2"
                   MOVE    "無効"      TO  SPA-P031-SIKAKU-YUKO(IDX)
                   MOVE    2           TO  SPA-P031-TJYOTAIFLG (IDX)
               WHEN    "3"
                   MOVE    "無効（新）"
                                       TO  SPA-P031-SIKAKU-YUKO(IDX)
                   MOVE    3           TO  SPA-P031-TJYOTAIFLG (IDX)
               WHEN    "4"
                   MOVE    "該当無"    TO  SPA-P031-SIKAKU-YUKO(IDX)
                   MOVE    4           TO  SPA-P031-TJYOTAIFLG (IDX)
               WHEN    "5"
                   MOVE    "複数該当"  TO  SPA-P031-SIKAKU-YUKO(IDX)
      *            患者確認必要
                   MOVE    5           TO  SPA-P031-TJYOTAIFLG (IDX)
      *2024/12
               WHEN    "6"
                   MOVE    "有効（未）"
                                       TO  SPA-P031-SIKAKU-YUKO(IDX)
           END-EVALUATE
      *
      *    限度額適用認定証
           IF     (ONSKAKU-GENDO-DOUIFLG   =   "1" )
               IF     (ONSKAKU-GENDO-SHOKBN    =   SPACE
                                               OR  ZERO  )
                   MOVE    "無"                TO  SPA-P031-GENDO-ARI
                                                                (IDX)
               ELSE
                   MOVE    "有"                TO  SPA-P031-GENDO-ARI
                                                                (IDX)
               END-IF
           END-IF
      *    特定疾病療養受療証提供
           IF     (ONSKAKU-SIKKAN-DOUIFLG  =   "1" )
               IF      (ONSKAKU-SIKKAN-SIPKBN (1)   =   SPACE )
                   AND (ONSKAKU-SIKKAN-SIPKBN (2)   =   SPACE )
                   AND (ONSKAKU-SIKKAN-SIPKBN (3)   =   SPACE )
                   MOVE    "無"                TO  SPA-P031-SIKKAN-ARI
                                                                (IDX)
               ELSE
                   MOVE    "有"                TO  SPA-P031-SIKKAN-ARI
                                                                (IDX)
               END-IF
           END-IF
      *R05.3
      *    同意フラグのみ判定に変更する
           IF    (ONSKAKU-SHOKAI-KBN       =   "2"   )
            AND  (ONSKAKU-ORCA-SHOKAI-KBN  =   SPACE )
      *        保険証照会は変更なし
               CONTINUE
           ELSE
      *
      *    限度額適用認定証
           EVALUATE    ONSKAKU-GENDO-DOUIFLG
               WHEN    "1"
                   MOVE    "有"                TO  SPA-P031-GENDO-ARI
                                                                (IDX)
               WHEN    "0"
                   MOVE    "無"                TO  SPA-P031-GENDO-ARI
                                                                (IDX)
           END-EVALUATE
      *    特定疾病療養受療証提供
           EVALUATE    ONSKAKU-SIKKAN-DOUIFLG
               WHEN    "1"
                   MOVE    "有"                TO  SPA-P031-SIKKAN-ARI
                                                                (IDX)
               WHEN    "0"
                   MOVE    "無"                TO  SPA-P031-SIKKAN-ARI
                                                                (IDX)
           END-EVALUATE
           END-IF
      ***
      *    手術情報閲覧同意
           EVALUATE    ONSKAKU-OPE-DOUIFLG
               WHEN   "1"
                   MOVE   "有"             TO  SPA-P031-OPE-DOUIFLG
                                                              (IDX)
               WHEN   "0"
                   MOVE   "無"             TO  SPA-P031-OPE-DOUIFLG
                                                              (IDX)
           END-EVALUATE
      *    特定健診情報閲覧同意
           EVALUATE    ONSKAKU-KENSIN-DOUIFLG
               WHEN   "1"
                   MOVE   "有"             TO  SPA-P031-KENSIN-DOUIFLG
                                                              (IDX)
               WHEN    "0"
                   MOVE   "無"             TO  SPA-P031-KENSIN-DOUIFLG
                                                              (IDX)
           END-EVALUATE
      *    薬剤情報閲覧同意
           EVALUATE    ONSKAKU-YAKUZAI-DOUIFLG
               WHEN   "1"
                   MOVE   "有"             TO  SPA-P031-YAKUZAI-DOUIFLG
                                                              (IDX)
               WHEN    "0"
                   MOVE   "無"             TO  SPA-P031-YAKUZAI-DOUIFLG
                                                              (IDX)
           END-EVALUATE
      *    診療情報閲覧同意
           EVALUATE    ONSKAKU-SHINRYO-DOUIFLG
               WHEN   "1"
                   MOVE   "有"             TO  SPA-P031-SHINRYO-DOUIFLG
                                                              (IDX)
               WHEN    "0"
                   MOVE   "無"             TO  SPA-P031-SHINRYO-DOUIFLG
                                                              (IDX)
           END-EVALUATE
      *
      *
      *    状態
           MOVE    ONSKAKU-LOCK-KBN    TO  SPA-P031-LOCK-KBN(IDX)
           MOVE    ONSKAKU-MOD-FLG     TO  SPA-P031-MOD-FLG (IDX)
           IF      ONSKAKU-MOD-FLG NOT =   SPACE
      *        処理済み
               MOVE    "済"            TO  SPA-P031-SHOKAI-MEI(IDX)
                                                          (3:2)
           END-IF
           IF      ONSKAKU-LOCK-KBN    NOT =   SPACE
      *        処理中
               MOVE    "他"            TO  SPA-P031-SHOKAI-MEI(IDX)
                                                          (3:2)
           END-IF
      *
      *R04.2
           MOVE    ONSKAKU-SCR-DLTFLG   TO  SPA-P031-SCR-DLTFLG(IDX)
           IF      ONSKAKU-SCR-DLTFLG   =   "1"
             AND  (SPA-P031-SHOKAI-MEI(IDX) (3:2) =   SPACE )
               MOVE    "×"            TO  SPA-P031-SHOKAI-MEI(IDX)
                                                          (3:2)
           END-IF
      *
           MOVE    IDX                 TO  SPA-P031-MAX
           .
       3002-ONSAKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認結果編集処理
      *****************************************************************
       30021-RESULT-HEN-SEC              SECTION.
      *
      *    氏名
           MOVE    ONSKAKU-RES-NAME        TO  SPA-P031-NAME (IDX)
           MOVE    ONSKAKU-RES-KANANAME    TO  SPA-P031-KANANAME (IDX)
           EVALUATE    ONSKAKU-RES-SEX1
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P031-SEX  (IDX)
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P031-SEX  (IDX)
               WHEN    "3"
                   MOVE    "　"            TO  SPA-P031-SEX  (IDX)
           END-EVALUATE
      *2024/02
           IF     (ONSKAKU-CARD-CLASS      =   "A1" )
             AND  (ONSKAKU-RES-BIRTHDAY    =   "19000101")
      *        医療補助で「19000101」は生年月日不明
               MOVE    ALL "*"             TO  SPA-P031-BIRTHDAY (IDX)
               MOVE    ALL "*"             TO  SPA-P031-NENREI (IDX)
           ELSE
      *
      *    生年月日
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P031-BIRTHDAY (IDX)
      *    年齢
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  WRK-BIRTHDAY
           PERFORM 300111-NENREI-SET-SEC
      *
           END-IF
      *
      *    住所
           MOVE    ONSKAKU-RES-POST    TO  SPA-P031-POST (IDX)
           MOVE    ONSKAKU-RES-ADRS    TO  SPA-P031-ADRS (IDX)
      *    患者番号
           IF      ONSKAKU-PTID    NOT =   ZERO
               PERFORM 300112-PTNUM-HEN-SEC
           END-IF
      *
      *    保険者番号
           MOVE    ONSKAKU-RES-HKNJANUM
                                       TO  SPA-P031-HKNJANUM(IDX)
           IF     (ONSKAKU-RES-KIGO    =   SPACE )
              AND (ONSKAKU-RES-NUM     =   SPACE )
               MOVE    SPACE           TO  SPA-P031-KIGONUM (IDX)
           ELSE
               STRING  ONSKAKU-RES-KIGO
                       "・"
                       ONSKAKU-RES-NUM     DELIMITED  BY  SPACE
                                    INTO   SPA-P031-KIGONUM (IDX)
               END-STRING
      *        公費照会
               IF      ONSKAKU-SHOKAI-KBN      =   "9"
                                               OR  "8"
                   MOVE     ONSKAKU-RES-NUM
                                       TO   SPA-P031-KIGONUM (IDX)
               END-IF
           END-IF
           MOVE    ONSKAKU-RES-EDABAN
                                       TO  SPA-P031-EDABAN (IDX)
      *    被保険者証有効終了年月日
           IF      ONSKAKU-TEKEDYMD    NOT =   SPACE
               MOVE    ONSKAKU-TEKEDYMD    TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P031-TEKEDYMD (IDX)
           END-IF
      *2023/11
      *    本人家族
           EVALUATE    ONSKAKU-RES-HONKZKKBN
               WHEN    "1"
                   MOVE    "本"            TO  SPA-P031-HONKZKKBN (IDX)
               WHEN    "2"
                   MOVE    "家"            TO  SPA-P031-HONKZKKBN (IDX)
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
           MOVE    WRK-ZZZ             TO  SPA-P031-FTNRATE (IDX)
      *
      *    資格喪失事由
      **** MOVE    ONSKAKU-LOSS-JIYU   TO  SPA-P031-LOSS-JIYU (IDX)
           .
       30021-RESULT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認結果なし編集処理
      *****************************************************************
       30022-SHOKAI-HEN-SEC              SECTION.
      *
      *    生年月日
           MOVE    ONSKAKU-SHO-BIRTHDAY    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P031-BIRTHDAY (IDX)
      *    年齢
           MOVE    ONSKAKU-SHO-BIRTHDAY    TO  WRK-BIRTHDAY
           PERFORM 300111-NENREI-SET-SEC
      *    患者番号
           IF      ONSKAKU-PTID    NOT =   ZERO
               PERFORM 300112-PTNUM-HEN-SEC
      *        患者情報より編集
               MOVE    PTINF-NAME          TO  SPA-P031-NAME (IDX)
               MOVE    PTINF-KANANAME      TO  SPA-P031-KANANAME(IDX)
      *        性別
               EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P031-SEX  (IDX)
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P031-SEX  (IDX)
               WHEN    "3"
                   MOVE    "　"            TO  SPA-P031-SEX  (IDX)
               END-EVALUATE
           END-IF
      *
      *    保険者番号
           MOVE    ONSKAKU-SHO-HKNJANUM
                                       TO  SPA-P031-HKNJANUM(IDX)
           IF     (ONSKAKU-SHO-KIGO    =   SPACE )
              AND (ONSKAKU-SHO-NUM     =   SPACE )
               MOVE    SPACE           TO  SPA-P031-KIGONUM (IDX)
           ELSE
               STRING  ONSKAKU-SHO-KIGO
                       "・"
                       ONSKAKU-SHO-NUM     DELIMITED  BY  SPACE
                                    INTO   SPA-P031-KIGONUM (IDX)
               END-STRING
           END-IF
           MOVE    ONSKAKU-SHO-EDABAN
                                       TO  SPA-P031-EDABAN (IDX)
      *    患者確認なし
      *R03.4
      *****MOVE    2                   TO  SPA-P031-TJYOTAIFLG(IDX)
           .
       30022-SHOKAI-HEN-EXT.
           EXIT.
      *2023/11
      *****************************************************************
      *    患者保険情報とのチェック処理
      *****************************************************************
       30023-HKNINF-CHKHEN-SEC              SECTION.
      *
           INITIALIZE                      ORCSONSHI003AREA
           MOVE    "01"                TO  LNK-ONSHI003-SYOKBN
           MOVE    ONSKAKU-PTID        TO  LNK-ONSHI003-PTID
           MOVE    SPA-P031-SYSYMD     TO  LNK-ONSHI003-SYSYMD
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
               MOVE    "1"                 TO  SPA-P031-HKNCHK (IDX)
           END-IF
           IF      LNK-ONSHI003-CHK-FTNRATE =   "1"
               MOVE    "2"                 TO  SPA-P031-HKNCHK (IDX)
           END-IF
           .
       30023-HKNINF-CHKHEN-EXT.
           EXIT.
      *
      *2023/11
      *****************************************************************
      *    予約照会　患者保険情報とのチェック処理
      *****************************************************************
       30024-PTHKNINF-YYKHEN-SEC              SECTION.
      *
      *    枝番不一致をクリア
           MOVE    ZERO                TO  SPA-P031-TJYOTAIFLG(IDX)
      *
           INITIALIZE                      ORCSONSHI003AREA
           MOVE    "03"                TO  LNK-ONSHI003-SYOKBN
           MOVE    "CHK"               TO  LNK-ONSHI003-SYOKBN2
           MOVE    ONSKAKU-PTID        TO  LNK-ONSHI003-PTID
           MOVE    SPA-P031-SYSYMD     TO  LNK-ONSHI003-SYSYMD
           CALL    "ORCSONSHI003"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSONSHI003AREA
                                       ONSKAKU-REC
           IF     (LNK-ONSHI003-RC         =   ZERO )
               CONTINUE
           ELSE
               IF      LNK-ONSHI003-CHK-KBN    =   "99"
      *        対象の患者保険登録なし
                   MOVE    "9"             TO  SPA-P031-YYKCHK (IDX)
               END-IF
               GO  TO  30024-PTHKNINF-YYKHEN-EXT
           END-IF
      *
           IF      LNK-ONSHI003-YYKERR     =   "00"
      *        枝番一致
               MOVE    "0"                 TO  SPA-P031-YYKCHK (IDX)
           ELSE
               MOVE    "2"                 TO  SPA-P031-YYKCHK (IDX)
           END-IF
      *    枝番不一致（更新可）
           IF      LNK-ONSHI003-CHK-EDABAN     =   "1"
               MOVE    1                   TO  SPA-P031-TJYOTAIFLG(IDX)
               MOVE    "1"                 TO  SPA-P031-YYKCHK (IDX)
           END-IF
           .
       30024-PTHKNINF-YYKHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号編集処理
      *****************************************************************
       300112-PTNUM-HEN-SEC              SECTION.
      *
      *    患者番号変換(管理）を検索し、患者IDより患者番号を取得
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    ONSKAKU-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  SPA-P031-PTNUM (IDX)
               MOVE    ONSKAKU-PTID    TO  SPA-P031-PTID  (IDX)
      *        患者情報検索
               MOVE    ONSKAKU-PTID    TO  WRK-PTID
               PERFORM 9010-PTINF-KEY-READ-SEC
               IF     (FLG-PTINF       =   ZERO)
                  AND (ONSKAKU-RES-SEX1    =   PTINF-SEX)
                  AND (ONSKAKU-RES-BIRTHDAY    =   PTINF-BIRTHDAY)
                   CONTINUE
               ELSE
      *            患者情報と性別・生年月日不一致
      *R03.8 変更（未使用）
      *            MOVE    1               TO  SPA-P031-TJYOTAIFLG(IDX)
                   MOVE    1               TO  SPA-P031-CHKEDABAN (IDX)
               END-IF
           ELSE
      *        患者なし
               MOVE    "******"        TO  SPA-P031-PTNUM (IDX)
               MOVE    ONSKAKU-PTID    TO  SPA-P031-PTID  (IDX)
      *R03.8 変更(未使用)
      *        MOVE    1               TO  SPA-P031-TJYOTAIFLG(IDX)
               MOVE    1               TO  SPA-P031-CHKEDABAN (IDX)
               INITIALIZE                  PTINF-REC
           END-IF
           .
       300112-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       300111-NENREI-SET-SEC             SECTION.
      *
           INITIALIZE                  WRK-NENREI-AREA
           IF      WRK-BIRTHDAY        =   SPACE
                                       OR  ZERO
               GO  TO  300111-NENREI-SET-EXT
           END-IF
      *
           MOVE    WRK-BIRTHDAY        TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
      *    画面表示用
           MOVE    SPACE               TO  WRK-NENREI-G
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-P031-NENREI (IDX)
      *
           .
       300111-NENREI-SET-EXT.
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
           IF     (FLG-ONSKAKU         =   ZERO )
             AND  (ONSKAKU-LOCK-KBN    NOT =   SPACE)
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
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "P031 ONSKAKU KEY UPD ERR:"  MCP-RC
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
               WHEN    "0101"
      *            削除
                   IF      SPA-PID1-FLG        =   "OK"
                      PERFORM 4081-DELETE-SEC
                   END-IF
                   MOVE    SPACE               TO  SPA-PIDCD
               WHEN    "0102"
      *            新規患者
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    SPA-P031-SELNUM     TO  IDX
      *                INITIALIZE              SPA-QRDATA-AREA
      *                MOVE    "1"             TO  SPA-QR-SYORI
      *                PERFORM 490-KAKUTEI-SYORI-SEC
                   END-IF
                   MOVE    SPACE               TO  SPA-PIDCD
      *R03.5
      *        排他解除
               WHEN    "0103"
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    "0104"              TO  SPA-PIDCD
                   ELSE
                       MOVE    SPACE               TO  SPA-PIDCD
                   END-IF
               WHEN    "0104"
                   IF      SPA-PID1-FLG        =   "OK"
                        MOVE    SPA-P031-SELNUM     TO  IDX
                        PERFORM 4031-LOCK-DELETE-SEC
                   END-IF
                   MOVE    SPACE               TO  SPA-PIDCD
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
      *****
      *R04.2
      *       変更なし処理
               WHEN    "0106"
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4110-HENKOUNASI-SYORI-SEC
                   END-IF
                   MOVE    SPACE          TO  SPA-PIDCD
      *2023/11
      *        予約照会から枝番更新
               WHEN    "0200"
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4120-EDABANN-UPDSYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-P031-CUR
                   END-IF
                   MOVE    SPACE          TO  SPA-PIDCD
           END-EVALUATE
      *
      ***     MOVE    SPACE               TO  SPA-PIDCD
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
           MOVE    SPACE               TO  P031
           INITIALIZE                      P031
      *
           MOVE    SPA-P031-HSYSYMD    TO  P031-KAKUNINYMD
           MOVE    SPA-P031-MIDASI     TO  P031-MIDASI-TEXTDATA
           MOVE    SPA-P031-MIDASI2    TO  P031-MIDASI2-TEXTDATA
      *
           EVALUATE    SPA-P031-SYORI
               WHEN    0
                   MOVE    "F10 保険証"
                                       TO  P031-B10-LABEL
               WHEN    1
                   MOVE    "F10 公費照"
                                       TO  P031-B10-LABEL
               WHEN    2
                   MOVE    "F10 顔認証"
                                       TO  P031-B10-LABEL
               WHEN    3
      ****         MOVE    "F10 全体"
                   MOVE    "F10 保険証"
                                       TO  P031-B10-LABEL
               WHEN    4
                   MOVE    "F10 全体"
                                       TO  P031-B10-LABEL
           END-EVALUATE
      *2023/10
           IF      SPA-P031-CHNGKBN     NOT =   SPACE
               MOVE    "F10 全体"
                                       TO  P031-B10-LABEL
           END-IF
      **** IF      SPA-P031-SYORI      =   6
      *        予約照会
      *         MOVE    "S+F8 枝番更新"
      *                                 TO  P031-B08S-LABEL
      ***  END-IF
      *
           IF      SPA-P031-SYORI2     =   0
               MOVE    "F9 全対象"
                                       TO  P031-B09-LABEL
           ELSE
               MOVE    "F9 未処理分"
                                       TO  P031-B09-LABEL
           END-IF
      *
           IF      SPA-P031-LOCK1      =   1
                                       OR  2
               MOVE    "F3 処理解除"
                                       TO  P031-B03-LABEL
           ELSE
               IF      SPA-P031-LOCK-SEL   =   ZERO
                   MOVE    "F3 削除"       TO  P031-B03-LABEL
               ELSE
                   MOVE    SPACE           TO  P031-B03-LABEL
               END-IF
           END-IF
      *
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-P031-MAX
               MOVE    SPA-P031-RENNUM (IDX)   TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  P031-RENNUM  (IDX)
               MOVE    SPA-P031-SHOKAI-MEI(IDX)
                                               TO  P031-SHOKAI (IDX)
               MOVE    SPA-P031-SHORITIME(IDX) TO  P031-SHORITIME(IDX)
               MOVE    SPA-P031-NAME     (IDX) TO  P031-NAME (IDX)
               MOVE    SPA-P031-KANANAME (IDX)
                                               TO  P031-KANANAME (IDX)
               MOVE    SPA-P031-SEX      (IDX) TO  P031-SEX (IDX)
               MOVE    SPA-P031-BIRTHDAY (IDX) TO  P031-BIRTHDAY (IDX)
               MOVE    SPA-P031-NENREI   (IDX) TO  P031-NENREI   (IDX)
               MOVE    SPA-P031-HKNJANUM (IDX) TO  P031-HKNJANUM (IDX)
               MOVE    SPA-P031-KIGONUM  (IDX) TO  P031-KIGONUM (IDX)
               MOVE    SPA-P031-EDABAN   (IDX) TO  P031-EDABAN (IDX)
               MOVE    SPA-P031-TEKEDYMD (IDX)
                                               TO  P031-TEKEDYMD (IDX)
      *2023/11
               MOVE    SPA-P031-HONKZKKBN (IDX)
                                               TO  P031-HONKZKKBN (IDX)
               MOVE    SPA-P031-FTNRATE (IDX)
                                               TO  P031-FTNRATE (IDX)
               MOVE    SPA-P031-SIKAKU-YUKO(IDX)
                                               TO  P031-SIKAKU-YUKO(IDX)
      *
               MOVE    SPA-P031-GENDO-ARI (IDX)
                                               TO  P031-GENDO  (IDX)
               MOVE    SPA-P031-SIKKAN-ARI (IDX)
                                               TO  P031-SIKKAN (IDX)
      *R05.3
               MOVE    SPA-P031-OPE-DOUIFLG (IDX)
                                               TO  P031-SYUJYU (IDX)
               MOVE    SPA-P031-KENSIN-DOUIFLG (IDX)
                                               TO  P031-KENSIN (IDX)
               MOVE    SPA-P031-YAKUZAI-DOUIFLG (IDX)
                                               TO  P031-YAKUZAI (IDX)
               MOVE    SPA-P031-SHINRYO-DOUIFLG (IDX)
                                               TO  P031-SHINRYO (IDX)
      *
               MOVE    SPA-P031-PTNUM (IDX)
                                               TO  P031-PTNUM (IDX)
               MOVE    SPA-P031-POST  (IDX)
                                               TO  P031-POST (IDX)
               MOVE    SPA-P031-ADRS   (IDX)   TO  P031-ADRS   (IDX)
      *R04.7
               MOVE    SPA-P031-SHOKAI-NUM(IDX)
                                               TO  P031-SHOKAI-NUM(IDX)
      *
               IF      SPA-P031-RENNUM (IDX)   =   SPA-P031-SELNUM
                   MOVE    "T"                 TO  P031-ONSHILIST-SELECT
                                                               (IDX)
      *2023/08
                   MOVE    IDX                 TO  P031-ONSHILIST-ROW
               ELSE
                   MOVE    "F"                 TO  P031-ONSHILIST-SELECT
                                                               (IDX)
               END-IF
      *R03.8   受付と合わせる
      ******   枝番変更（青緑?
      *         IF      SPA-P031-CHKEDABAN (IDX)
      *                                     =   1
      *           MOVE    "#C0E7E0"       TO  P031-BGCOLOR (IDX)
      *         END-IF
      *        患者確認必要（複数該当）（黄緑)
      *         IF      SPA-P031-TJYOTAIFLG (IDX)
      *                                     =   3
      *             MOVE    "#EFFF99"       TO  P031-BGCOLOR (IDX)
      *         END-IF
      *        資格結果なし（淡い赤）?
      *         IF      SPA-P031-TJYOTAIFLG (IDX)
      *                                     =   2
      *             MOVE    "#FAEBD7"       TO  P031-BGCOLOR (IDX)
      ******   END-IF
      *2023/11
      *        被保険者資格証明書
               IF      SPA-P031-CARD-CLASS (IDX)   =   "05"
      ****                                         OR  "A1"
      *           患者確認必要（シェルピンク）
                  MOVE    "#fbdac8"       TO   P031-BGCOLOR    (IDX)
               END-IF
      *2024/02
      *        医療扶助
               IF      SPA-P031-CARD-CLASS (IDX)   =   "A1"
      *           患者確認必要（ 灰緑）
                  MOVE    "#95CC95"       TO   P031-BGCOLOR    (IDX)
               END-IF
      *2023/11
      *        患者保険情報とのチェック（優先）
               IF      SPA-P031-HKNCHK (IDX)   NOT =   SPACE
      *           患者確認必要（淡香 うすこう）
                  MOVE    "#f3bf88"       TO   P031-BGCOLOR    (IDX)
               END-IF
      *****
      *R03.8
      *        資格有効性による背景色
               EVALUATE     SPA-P031-TJYOTAIFLG(IDX)
                   WHEN     1
      *            枝番確認（ベビーブルー）
                       MOVE    "#bbe2f1"       TO  P031-BGCOLOR (IDX)
                   WHEN     2
      *            無効（サーモンピンク）
                       MOVE    "#f3a68c"       TO   P031-BGCOLOR
                                                                  (IDX)
                   WHEN    3
      *            無効（新）（ラベンダー）
                       MOVE    "#cab8d9"       TO   P031-BGCOLOR
                                                                  (IDX)
                   WHEN    4
      *            資格結果なし（薄赤
      *                ベビーピンク
                       MOVE    "#f6e5cc"       TO   P031-BGCOLOR
                                                                  (IDX)
                   WHEN    5
      *            患者確認必要（複数該当）（淡い黄緑色）
                      MOVE    "#EFFF99"       TO   P031-BGCOLOR
                                                                  (IDX)
               END-EVALUATE
      *****
      *        削除分（やや灰色い無彩色）
               IF      SPA-P031-SCR-DLTFLG(IDX)   =    "1"
                      MOVE    "#dcdcdc"       TO   P031-BGCOLOR
                                                                  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-P031-MAX        TO  P031-ONSHILIST-COUNT
      *
           MOVE    SPA-P031-SELNUM     TO  P031-SELNUM
           MOVE    SPA-P031-FOOTMSG    TO  P031-FOOTMSG-TEXTDATA
      *
      *2023/11
           MOVE    SPA-P031-CHNGKBN-G  TO  P031-CHNGKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P031-CHNGKBN-MAX
               MOVE    SPA-P031-CHNGKBN-L-MEI (IDX)
                                       TO  P031-CHNGKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-P031-CHNGKBN-MAX    TO  P031-CHNGKBN-COUNT
      *
           IF      SPA-P031-NEXT       =   1
               IF      SPA-P031-PAGE       >   1
                   MOVE    "前次頁あり"  TO  P031-MIDASINEXT-TEXTDATA
                   MOVE    "red"         TO  P031-MIDASINEXT-STYLE
               ELSE
                   MOVE    "次頁あり"    TO  P031-MIDASINEXT-TEXTDATA
                   MOVE    "red"         TO  P031-MIDASINEXT-STYLE
               END-IF
           ELSE
               IF      SPA-P031-PAGE       >   1
                   MOVE    "前頁あり"    TO  P031-MIDASINEXT-TEXTDATA
                   MOVE    "red"         TO  P031-MIDASINEXT-STYLE
               END-IF
           END-IF
      *2024/12
      *    訪問一括
           IF     (SPA-ONSHI-HOUMON    =   "1"  )
             OR   (SPA-ONSHI-ONLINE    =   "1"  )
               MOVE    WIDGET-NORMAL       TO  P031-B05S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  P031-B05S-STATE
           END-IF
      *
      *2025/05
      *    一覧更新時間（３０秒）
           MOVE    30                  TO  P031-DURATION
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
           EVALUATE    SPA-P031-CUR
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
               WHEN    02
                   MOVE    "CHNGKBN"   TO  MCP-WIDGET
               WHEN    13
                   MOVE    "B02"       TO  MCP-WIDGET
               WHEN    18
                   MOVE    "B08S"      TO  MCP-WIDGET
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
      *2023/11     選択をクリア
                   MOVE    SPACE               TO  SPA-P031-SEL-UUID
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *    解除削除
               WHEN    "CLICKED"       ALSO    "B03"
                   IF      SPA-P031-LOCK-SEL   =   1
      *                排他処理中解除
                       PERFORM 403-LOCKDELETE-SYORI-SEC
                   ELSE
      *R04.2
      *                削除処理
                       PERFORM 408-DELETE-SYORI-SEC
                   END-IF
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
      *    表示切替
               WHEN    "CLICKED"       ALSO    "B10"
                   MOVE    ZERO            TO  FLG-CHNGKBN
                   PERFORM 410-LISTCHNG-SEC
      *    表示切替（全体）
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-ALLHYOJI-SEC
      *R04.2
      *       変更なし
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 411-B11-SYORI-SEC
      *2022/09
      *       保険証ＯＣＲ再確認
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 411-B08-SYORI-SEC
      *****
      *       患者確認
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 410-P033-SYORI-SEC
      *       一覧更新
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4110-B11S-SYORI-SEC
      *2025/05
      *       一覧更新（タイマー）
               WHEN    "CLICKED"       ALSO    "pandatimer1"
                   PERFORM 4110-B11S-TIMER-SEC
      *2024/12
      *        同意一括照会
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 4110-B05S-SYORI-SEC
      ******
      *2023/11
      *       予約照会　枝番更新
      *         WHEN    "CLICKED"       ALSO    "B08S"
      *             PERFORM 4110-B08S-SYORI-SEC
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
           MOVE    ZERO                TO  SPA-P031-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        リスト選択
               WHEN    ANY             ALSO    "ONSHILIST"
                   PERFORM 4001-ONSHILIST-SEL-SEC
      *        表示リスト入力
               WHEN    "ACTIVATE"      ALSO    "CHNGKBN"
                   PERFORM 4002-CHNGKBN-SYORI-SEC
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
           MOVE    ZERO                TO  SPA-P031-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P031-MAX
               IF      P031-ONSHILIST-SELECT (IDX) =   "T"
                   MOVE    SPA-P031-RENNUM (IDX)
                                           TO  SPA-P031-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-P031-SELNUM    NOT =   ZERO
               PERFORM 4102-SELNUM-CHK-SEC
           END-IF
      *
           .
       4001-ONSHILIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表表示リスト選択処理
      *****************************************************************
       4002-CHNGKBN-SYORI-SEC          SECTION.
      *
           MOVE    P031-CHNGKBN        TO  SPA-P031-CHNGKBN-G
           MOVE    SPACE               TO  SPA-P031-CHNGKBN-KBN
      *    空白選択
           IF      SPA-P031-CHNGKBN    =   SPACE
      *        全体表示へ 
               MOVE    1               TO  FLG-CHNGKBN
               PERFORM 410-LISTCHNG-SEC
               GO  TO  4002-CHNGKBN-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P031-CHNGKBN-MAX
               IF      SPA-P031-CHNGKBN    =   SPA-P031-CHNGKBN-L-MEI
                                                            (IDX)
                   MOVE    SPA-P031-CHNGKBN-L-MEI (IDX)
                                           TO  SPA-P031-CHNGKBN
                   MOVE    SPA-P031-CHNGKBN-L-KBN (IDX)
                                           TO  SPA-P031-CHNGKBN-KBN
                   MOVE    SPA-P031-CHNGKBN-MAX    TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    02                  TO  SPA-P031-CUR
               GO  TO  4002-CHNGKBN-SYORI-EXT
           END-IF
      *
           IF      SPA-P031-CHNGKBN-KBN NOT =   SPACE
               MOVE    SPA-P031-CHNGKBN-KBN     TO  SPA-P031-SYORI
           END-IF
      *
      *    選択をクリアし、一覧内容を最新にする
           MOVE    ZERO                TO  SPA-P031-SELNUM
           MOVE    SPACE               TO  SPA-P031-SEL-UUID
           MOVE    1                   TO  SPA-P031-CUR
      *
           MOVE    ZERO                TO  SPA-P031-LOCK1
      *
           MOVE    ZERO                TO  SPA-P031-SYORI2
           MOVE    SPA-P031-SYORI      TO  WRK-SYORI
           MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
           MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P031-CHNGKBN-KBN    TO  WRK-CHNGKBN-KBN
           MOVE    SPA-P031-CHNGKBN-G      TO  WRK-CHNGKBN
      *
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC
           IF      SPA-P031-MAX        =   ZERO
      *        対象なし
               MOVE    2                   TO  SPA-P031-CUR
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
           END-IF
           .
       4002-CHNGKBN-SYORI-EXT.
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
      *    選択番号を空白で「ENTER」
           IF      SPA-MCP-WIDGET      =   "SELNUM"
               IF     (P031-SELNUM     =   SPA-P031-SELNUM )
                 AND  (SPA-P031-SELNUM =   ZERO      )
                  MOVE    1                TO  FLG-INCHK
               END-IF
           END-IF
      *
           MOVE    P031-SELNUM         TO  SPA-P031-SELNUM
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
           MOVE    1                   TO  SPA-P031-CUR
      *
           MOVE    ZERO                TO  SPA-P031-LOCK1
           MOVE    ZERO                TO  SPA-P031-LOCK-SEL
      *2023/11
           MOVE    SPACE               TO  SPA-P031-FOOTMSG
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    SPA-P031-MAX )
                          OR  (IDX          >   ZERO  )
               IF      SPA-P031-SELNUM      =   SPA-P031-RENNUM
                                                             (IDY)
                   MOVE    IDY              TO  IDX
               END-IF
           END-PERFORM
           IF     (SPA-P031-SELNUM     =   ZERO )
               MOVE    1                   TO  SPA-P031-CUR
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
      *???
      *        空白入力で、表示コンボへ
               IF      FLG-INCHK           =   1
                  MOVE    02                  TO  SPA-P031-CUR
               END-IF
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
           IF      (IDX                 =   ZERO )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P031-CUR
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *R04.2
      *    削除分
           IF      SPA-P031-SCR-DLTFLG(IDX)    =   "1"
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P031-CUR
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
               MOVE    ZERO                TO  SPA-P031-SELNUM
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *R03.10
      *    公費選択不可
           IF      SPA-P031-SHOKAI-KBN (IDX)   =   "9"
                                               OR  "8"
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P031-CUR
               MOVE    ZERO                TO  SPA-P031-SELNUM
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *2024/02
      *    医療扶助対応前
           IF     (SPA-P031-CARD-CLASS (IDX)   =   "A1")
            AND   (SPA-P031-FUJYO-KOH  (IDX)   =   "9")
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P031-CUR
               MOVE    ZERO                TO  SPA-P031-SELNUM
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *    医療扶助で公費（保険あり）
           IF     (SPA-P031-CARD-CLASS (IDX)   =   "A1")
            AND   (SPA-P031-FUJYO-KOH  (IDX)   =   "1")
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P031-CUR
               MOVE    ZERO                TO  SPA-P031-SELNUM
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *2024/02
      *    医療扶助で生年月日不明
           IF      SPA-P031-CARD-CLASS (IDX)   =  "A1"
            AND   (SPA-P031-BIRTHDAY  (IDX)    =   ALL "9")
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P031-CUR
               MOVE    ZERO                TO  SPA-P031-SELNUM
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
               GO  TO  4102-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-P031-TBL-UUID (IDX) TO  SPA-P031-SEL-UUID
      *    オンライン資格確認照会検索
           PERFORM 4103-ONSKAKU-KEY-SEC
           IF      FLG-ONSKAKU         =   1
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE
               IF     (ONSKAKU-LOCK-KBN    =   "1"  )
      *            他端末で処理
                   PERFORM 40211-LOCK-MSG-SEC
                   MOVE    "0003"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-P031-LOCK-SEL
               END-IF
           END-IF
      *2024/04
      *    保険証照会で医療扶助併用
           IF     (SPA-P031-SHOKAI-KBN  (IDX)   =   "2")
            AND   (SPA-P031-ORCA-SHOKAI-KBN (IDX)
                                                =   SPACE)
            AND   (SPA-P031-FUJYO-HEIYO (IDX)   =   "1" )
            AND   (ONSKAKU-FUJYO-UUID  NOT =   SPACE )
            AND   (SPA-ERRCD           =   SPACE )
      *        医療扶助返却チェック
               PERFORM 410210-FUJYO-HEIYO-CHK-SEC
               IF      SPA-ERRCD           NOT =   SPACE
                   MOVE    1                   TO  SPA-P031-CUR
                   GO  TO  4102-SELNUM-CHK-EXT
               END-IF
           END-IF
      *
      *    状態の詳細
           IF      SPA-ERRCD           =   SPACE
               IF     (ONSKAKU-SIKAKU-YUKO NOT =   "1"   )
                   OR (ONSKAKU-LOSS-JIYU   NOT =   SPACE )
                   PERFORM 41021-FOTTMSG-SEC
               END-IF
           END-IF
      *2022/09
      *    保険証OCR
           IF      SPA-ERRCD           =   SPACE
               IF      ONSKAKU-ORCA-SHOKAI-KBN   =   "3"
      *            保険証OCRでエラーの時、B08 へ
                   MOVE    8                     TO  SPA-P031-CUR
                   GO      TO   4102-SELNUM-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    12                  TO  SPA-P031-CUR
      *2023/11 予約照会
               IF      (SPA-P031-SYORI    =   6  )
                   MOVE    18              TO  SPA-P031-CUR
               END-IF
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
           END-IF
           .
       4102-SELNUM-CHK-EXT.
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
      *
           IF      FLG-FUJYO-ARI       =   ZERO
               MOVE    "0021"              TO  SPA-ERRCD
           END-IF
           .
       410210-FUJYO-HEIYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    オンライン資格確認　状態編集処理
      *****************************************************************
       41021-FOTTMSG-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-P031-FOOTMSG
           MOVE    SPACE               TO  WRK-FOOTMSG1
           MOVE    SPACE               TO  WRK-FOOTMSG2
      *    喪失事由
           EVALUATE    ONSKAKU-LOSS-JIYU
               WHEN    "01"
                   MOVE    "（喪失事由：死亡）"    TO  WRK-FOOTMSG2
               WHEN    "02"
                   MOVE    "（喪失事由：生活保護受給）"
                                                   TO  WRK-FOOTMSG2
           END-EVALUATE
      *
           EVALUATE    ONSKAKU-SIKAKU-YUKO
               WHEN    "2"
                   MOVE    "【無効】"
                                                 TO  WRK-FOOTMSG1
               WHEN    "3"
                   MOVE    "【無効（新しい資格あり）】"
                                                 TO  WRK-FOOTMSG1
               WHEN    "4"
                   MOVE    "【資格なし】"
                                                 TO  WRK-FOOTMSG1
           END-EVALUATE
           STRING    WRK-FOOTMSG1
                     WRK-FOOTMSG2
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-P031-FOOTMSG
           END-STRING
           .
       41021-FOTTMSG-EXT.
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
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           IF      SPA-P031-SELNUM     =   ZERO
      *        選択なし
      *         IF      SPA-SYSYMD      NOT =   SPA-P031-SYSYMD
      *            初期表示へ
                   PERFORM 3000-SCREEN-INIT-SEC
      *         END-IF
           ELSE
               MOVE    ZERO                TO  SPA-P031-SELNUM
               MOVE    SPACE               TO  SPA-P031-SEL-UUID
               MOVE    1                   TO  SPA-P031-CUR
      *R03.5
               MOVE    ZERO                TO  SPA-P031-LOCK1
      *        一覧表最新表示
               MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
               MOVE    SPA-P031-SYORI      TO  WRK-SYORI
               MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
               MOVE    SPA-P031-CHNGKBN-KBN    TO  WRK-CHNGKBN-KBN
               MOVE    SPA-P031-CHNGKBN-G      TO  WRK-CHNGKBN
      *        オンライン資格確認照会編集処理
               PERFORM 30001-P031-INIT-SEC
           END-IF
           .
       402-CLEAR-EXT.
           EXIT.
      *
      *
      *R03.5
      *****************************************************************
      *    処理中解除チェック処理
      *****************************************************************
       4021-LOCK-CHEK-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-PIDCD
           PERFORM 4103-ONSKAKU-KEY-SEC
           IF      FLG-ONSKAKU         =   1
               MOVE    ZERO                TO  SPA-P031-LOCK1
           ELSE
               IF     (ONSKAKU-LOCK-KBN    =   "1"  )
      *            他端末で処理
                   PERFORM 40211-LOCK-MSG-SEC
                   IF      SPA-P031-LOCK1          =   1
                       MOVE    "0103"              TO  SPA-PIDCD
                       MOVE    2                   TO  SPA-P031-LOCK1
                       GO      TO       4021-LOCK-CHEK-EXT
                   ELSE
                       MOVE    "0007"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0007"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    一覧表最新表示
           MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P031-SYORI      TO  WRK-SYORI
           MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC
           .
       4021-LOCK-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    処理中解除チェック処理
      *****************************************************************
       40211-LOCK-MSG-SEC             SECTION.
      *
           MOVE    SPACE           TO  SPA-P031-LOCK-MSG
            STRING "OPID:["
                   ONSKAKU-OPID         DELIMITED BY SPACE
                   "]"
                   "("
                   ONSKAKU-UPYMD(1:4)
                   "-"
                   ONSKAKU-UPYMD(5:2)
                   "-"
                   ONSKAKU-UPYMD(7:2)
                   " "
                   ONSKAKU-UPHMS(1:2)
                   ":"
                   ONSKAKU-UPHMS(3:2)
                   ":"
                   ONSKAKU-UPHMS(5:2)
                   ")"
                                       DELIMITED BY SIZE
                                       INTO    SPA-P031-LOCK-MSG
           END-STRING
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    現在の時間から１分前とする
           MOVE    SMCNDATE-HMS        TO  WRK-CHK-TIME
           IF      WRK-CMM             >=  1
               COMPUTE WRK-CMM             =   WRK-CMM    -  1
           ELSE
               MOVE    59                  TO  WRK-CMM
               COMPUTE WRK-CHH             =   WRK-CHH    -  1
           END-IF
           MOVE    ZERO                TO  SPA-P031-LOCK1
      *    更新日が当日で、１分以内は、解除不可とする
           IF      (ONSKAKU-UPYMD      =   SMCNDATE-YMD)
                AND (ONSKAKU-UPHMS     >   WRK-CHK-TIME )
               MOVE    ZERO            TO  SPA-P031-LOCK1
           ELSE
               MOVE    1               TO  SPA-P031-LOCK1
           END-IF
           .
       40211-LOCK-MSG-EXT.
           EXIT.
      ******
      *****************************************************************
      *    表示切替処理
      *****************************************************************
       410-LISTCHNG-SEC             SECTION.
      *
           EVALUATE    SPA-P031-SYORI
               WHEN    0
      *            被保険者証へ
                   MOVE    2               TO  SPA-P031-SYORI
               WHEN    1
      *            全体へ
      ****         MOVE    0               TO  SPA-P031-SYORI
      *            公費照会へ
                   MOVE    3               TO  SPA-P031-SYORI
               WHEN    2
      *            顔認証へ
                   MOVE    1               TO  SPA-P031-SYORI
                WHEN    3
      *            全体へ
      *2023/11     MOVE    0               TO  SPA-P031-SYORI
      *            保険証ＯＣＲ
                   MOVE    4               TO  SPA-P031-SYORI
                WHEN    4
      *            全体へ
                   MOVE    0               TO  SPA-P031-SYORI
                WHEN    5
      *            全体へ
                   MOVE    0               TO  SPA-P031-SYORI
           END-EVALUATE
      *2023/11
           IF      FLG-CHNGKBN         =   1
      *        全体へ
               MOVE    0               TO  SPA-P031-SYORI
           END-IF
           IF      SPA-P031-CHNGKBN-KBN    NOT =   SPACE
      *        全体へ
               MOVE    0               TO  SPA-P031-SYORI
               MOVE    SPACE           TO  SPA-P031-CHNGKBN-KBN
               MOVE    SPACE           TO  SPA-P031-CHNGKBN-G
           END-IF
      *
           MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P031-SYORI      TO  WRK-SYORI
           MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC
           IF      SPA-P031-MAX        =   ZERO
               MOVE    10                  TO  SPA-P031-CUR
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
           END-IF
           .
       410-LISTCHNG-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示切替（全体）処理
      *****************************************************************
       409-ALLHYOJI-SEC             SECTION.
      *
      *
           IF      SPA-P031-SYORI2     =   0
               MOVE    1               TO  SPA-P031-SYORI2
           ELSE
               MOVE    0               TO  SPA-P031-SYORI2
           END-IF
      *
           MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P031-SYORI      TO  WRK-SYORI
           MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
           MOVE    SPA-P031-CHNGKBN-KBN    TO  WRK-CHNGKBN-KBN
           MOVE    SPA-P031-CHNGKBN-G      TO  WRK-CHNGKBN
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC
           IF      SPA-P031-MAX        =   ZERO
               MOVE    10                  TO  SPA-P031-CUR
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
           END-IF
           .
       409-ALLHYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    「変更なし」処理
      *****************************************************************
       411-B11-SYORI-SEC             SECTION.
      *
      *
           IF     (SPA-P031-SELNUM     =   ZERO )
      ****        (SPA-P031-SELNUM     >   SPA-P031-MAX)
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
               MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
               MOVE    SPA-P031-SYORI      TO  WRK-SYORI
               MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
               MOVE    SPA-P031-CHNGKBN-KBN    TO  WRK-CHNGKBN-KBN
               MOVE    SPA-P031-CHNGKBN-G      TO  WRK-CHNGKBN
      *        オンライン資格確認照会編集処理
               PERFORM 30001-P031-INIT-SEC
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
      **** MOVE    "9"                 TO  ONSKAKU-SHOKAI-KBN
      *    kouhi-uuidでのみ検索へ
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
      *    削除処理
      *****************************************************************
       408-DELETE-SYORI-SEC             SECTION.
      *
           IF     (SPA-P031-SELNUM     =   ZERO )
      ***        (SPA-P031-SELNUM     >   SPA-P031-MAX)
              OR  (SPA-P031-SEL-UUID   =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO   408-DELETE-SYORI-EXT
           END-IF
      *
      *    排他中などのチェック
           MOVE    2                   TO  FLG-CHKKBN
           PERFORM 4080-UPD-CHECK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-PIDCD
           END-IF
           .
      *
       408-DELETE-SYORI-EXT.
           EXIT.
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
                   PERFORM 40211-LOCK-MSG-SEC
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
                       PERFORM 40211-LOCK-MSG-SEC
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
       4081-DELETE-SEC             SECTION.
      *
      *    排他中などのチェック
           MOVE    2                   TO  FLG-CHKKBN
           PERFORM 4080-UPD-CHECK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4081-DELETE-EXT
           END-IF
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *
           PERFORM 4103-ONSKAKU-KEY-SEC
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
                   IF      ONSKAKU-SCR-DLTFLG  =   SPACE
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
               MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
               MOVE    SPA-P031-SYORI      TO  WRK-SYORI
               MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
               MOVE    SPA-P031-CHNGKBN-KBN    TO  WRK-CHNGKBN-KBN
               MOVE    SPA-P031-CHNGKBN-G      TO  WRK-CHNGKBN
      *        オンライン資格確認照会編集処理
               PERFORM 30001-P031-INIT-SEC
           END-IF
           .
       4081-DELETE-EXT.
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
               DISPLAY "P031 ONSKAKU KEY UPD ERR:"  MCP-RC
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
      *    kouhi-uuidでのみ検索へ
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
      **** MOVE    "key12"             TO  MCP-PATHNAME
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
      *****************************************************************
      *    排他の解除処理
      *****************************************************************
       403-LOCKDELETE-SYORI-SEC             SECTION.
      *
           IF      SPA-P031-LOCK1      =   1
      *        排他中のチェック
               PERFORM  4021-LOCK-CHEK-SEC
           END-IF
           .
      *
       403-LOCKDELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       406-BACKPAGE-SEC            SECTION.
      *
           IF      SPA-P031-PAGE       >   1
               COMPUTE SPA-P031-PAGE   =   SPA-P031-PAGE
                                       -   1
      *        オンライン資格確認照会結果編集
               PERFORM 3001-ONSKAK-SYORI-SEC
           ELSE
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P031-CUR
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
           IF      SPA-P031-NEXT       =   1
               ADD     1               TO  SPA-P031-PAGE
      *        オンライン資格確認照会結果編集
               PERFORM 3001-ONSKAK-SYORI-SEC
           ELSE
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P031-CUR
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
           MOVE    ZERO                TO  SPA-P031-SELNUM
           MOVE    1                   TO  SPA-P031-CUR
      *    １日前
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    SPA-P031-SYSYMD  TO  LNK-DAY6-KIJUN
           MOVE    "1"              TO  LNK-DAY6-ZOGENPTN
           MOVE    -1               TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF      STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  SPA-P031-SYSYMD
           END-IF
      *    オンライン資格確認照会結果編集
           MOVE    1                   TO  SPA-P031-PAGE
           PERFORM 3001-ONSKAK-SYORI-SEC
           IF      SPA-P031-MAX        =   ZERO
               MOVE    99                  TO  SPA-P031-CUR
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
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
           MOVE    ZERO                TO  SPA-P031-SELNUM
           MOVE    1                   TO  SPA-P031-CUR
      *    １日前
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    SPA-P031-SYSYMD  TO  LNK-DAY6-KIJUN
           MOVE    "1"              TO  LNK-DAY6-ZOGENPTN
           MOVE    1                TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF      STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  SPA-P031-SYSYMD
           END-IF
      *    オンライン資格確認照会結果編集
           MOVE    1                   TO  SPA-P031-PAGE
           PERFORM 3001-ONSKAK-SYORI-SEC
           IF      SPA-P031-MAX        =   ZERO
               MOVE    99                  TO  SPA-P031-CUR
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
           END-IF
           .
      *
        406-NEXTDAY-EXT.
           EXIT.
      *****************************************************************
      *     患者確認画面処理
      *****************************************************************
       410-P033-SYORI-SEC             SECTION.
      *
      *    予約照会表示中は選択不可
           IF      SPA-P031-SYORI      =   6
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P031-CUR
               GO      TO  410-P033-SYORI-EXT
           END-IF
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
                   IF     (ONSKAKU-LOCK-KBN    =   "1"  )
      *                他端末で処理
                       MOVE    "0003"          TO  SPA-ERRCD
      *R03.5
      *****            他端末情報編集
                       PERFORM  40211-LOCK-MSG-SEC
                   ELSE
      *                該当なしで患者番号がない場合、エラー
                       IF    (ONSKAKU-SIKAKU-YUKO      =   "4"   )
                        AND  (ONSKAKU-RES-HKNJANUM     =   SPACE )
                        AND  (ONSKAKU-PTID             =   ZERO  )
                           MOVE    "0006"          TO  SPA-ERRCD
                       END-IF
      *2022/09
      *                保険証ＯＣＲでエラーデータのみ
                       IF    (ONSKAKU-SHOKAI-KBN       =   "2"   )
                        AND  (ONSKAKU-ORCA-SHOKAI-KBN  =   "3"  )
                           MOVE    "0012"          TO  SPA-ERRCD
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
      *R03.4
      *****            患者ID登録確認（保険証認証）
                       IF    (SPA-ERRCD            =   SPACE)
                         AND (ONSKAKU-SHOKAI-KBN   =   "2"  )
                         AND (ONSKAKU-PTID     NOT =   ZERO )
                           PERFORM 4102-KAKUTEI-PTID-SEC
                       END-IF
      ***** 
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1               TO  SPA-P031-CUR
               GO      TO  410-P033-SYORI-EXT
           END-IF
      *2024/04
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    医療扶助であれば対象をすべて処理中とする
           IF      (ONSKAKU-CARD-CLASS  =   "A1" )
               PERFORM 41010-FUJYO-SYORI-SEC
           END-IF
      *
      *    処理中の設定
      ***  IF      FLG-ONSKAKU         =   ZERO
      *
      *         INITIALIZE                  ORCSMCNDATEAREA
      *         CALL    "ORCSMCNDATE"       USING
      *                                     ORCSMCNDATEAREA
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
                   DISPLAY "P031 ONSKAKU KEY UPD ERR:"  MCP-RC
                       ",KEY:" ONSKAKU-KEY
                   GO      TO  410-P033-SYORI-EXT
               END-IF
      **   END-IF
      *
      *    被保険者証確認でエラーの時、患者登録画面へ
           IF     (ONSKAKU-SHOKAI-KBN      =   "2"   )
      *R03.4
             AND ((ONSKAKU-RES-HKNJANUM    =   SPACE )
              OR  (ONSKAKU-SIKAKU-YUKO     =   "4"  ))
      ****
             AND  (ONSKAKU-PTID        NOT =   ZERO  )
               PERFORM 410-P02-BACK-SEC
               GO      TO  410-P033-SYORI-EXT
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
                   GO      TO  410-P033-SYORI-EXT
               END-IF
           END-IF
      *    P031へ
           MOVE    SPACE       TO  SPA-P031-GENDO-DOUIFLG
           PERFORM 4101-P033-CALL-SEC
           .
       410-P033-SYORI-EXT.
           EXIT.
      *2024/02
      *****************************************************************
      *    医療扶助をすべて処理中へ
      *****************************************************************
       41010-FUJYO-SYORI-SEC             SECTION.
      *
           MOVE    ONSKAKU-REC         TO  HZN-ONSKAKU-REC
      *
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    HZN-ONSKAKU-FUJYO-UUID
                                       TO  ONSKAKU-FUJYO-UUID
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key27"               TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           PERFORM UNTIL      (FLG-ONSKAKU    =   1 )
                           OR (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-P031-SEL-UUID   NOT =   ONSKAKU-TBL-UUID )
                 AND  (ONSKAKU-KAKUNINYMD  =   HZN-ONSKAKU-KAKUNINYMD)
                   PERFORM 41011-ONSKAKU-LOCK-SEC
               END-IF
               MOVE    "key27"               TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key27"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *2025/03
           MOVE    HZN-ONSKAKU-REC     TO  ONSKAKU-REC
           .
       41010-FUJYO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    オン資格　処理中設定
      *****************************************************************
       41011-ONSKAKU-LOCK-SEC             SECTION.
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "P031 ONSKAKU KEY UPD ERR:"  MCP-RC
                   ",KEY:" ONSKAKU-KEY
           END-IF
           .
       41011-ONSKAKU-LOCK-EXT.
           EXIT.
      *
      *****************************************************************
      *    P033オン資格確認画面へ
      *****************************************************************
       4101-P033-CALL-SEC             SECTION.
      *
      *    初期画面編集
           INITIALIZE                      SPA-P033-AREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    "P031"              TO  SPA-MOTOPG
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
           MOVE    "P031"              TO  SPA-MOTOPG
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
           MOVE    1                   TO  FLG-END
           .
       4101-P033-CALL-EXT.
           EXIT.
      *2022/09
      *****************************************************************
      *     保険証ＯＣＲ　再確認画面処理
      *****************************************************************
       411-B08-SYORI-SEC             SECTION.
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
                   IF     (ONSKAKU-LOCK-KBN    =   "1"  )
      *                他端末で処理
                       MOVE    "0003"          TO  SPA-ERRCD
      *R03.5
      *****            他端末情報編集
                       PERFORM  40211-LOCK-MSG-SEC
                   ELSE
      ******           保険証ＯＣＲでエラーデータのみ
      *2022/12         保険証ＯＣＲ　に変更
                       IF    (ONSKAKU-SHOKAI-KBN       =   "2"   )
                        AND  (ONSKAKU-ORCA-SHOKAI-KBN  =   "3"
                                                       OR  "2"   )
                           CONTINUE
                       ELSE
                           MOVE    "0013"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1               TO  SPA-P031-CUR
               GO      TO  411-B08-SYORI-EXT
           END-IF
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
                   DISPLAY "P031 ONSKAKU KEY UPD ERR:"  MCP-RC
                       ",KEY:" ONSKAKU-KEY
                   GO      TO  411-B08-SYORI-EXT
               END-IF
           END-IF
      *    P035へ
           PERFORM 4111-P035-CALL-SEC
           .
       411-B08-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    P035 保険証ＯＣＲ再確認画面へ
      *****************************************************************
       4111-P035-CALL-SEC             SECTION.
      *
      *    初期画面編集
           INITIALIZE                      SPA-P035-AREA
           MOVE    SPA-P031-SEL-UUID   TO  SPA-P035-SEL-UUID 
           MOVE    "P031"              TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP035"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
      *
           MOVE    "P031"              TO  SPA-MOTOPG
           MOVE    "P035"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P035"              TO  MCP-WINDOW
      *
            MOVE    "BIRTHDAY"         TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4111-P035-CALL-EXT.
           EXIT.
      *****************************************************************
      *    一覧更新処理
      *****************************************************************
       4110-B11S-SYORI-SEC             SECTION.
      *
      *    選択をクリアし、一覧内容を最新にする
           MOVE    ZERO                TO  SPA-P031-SELNUM
           MOVE    SPACE               TO  SPA-P031-SEL-UUID
           MOVE    1                   TO  SPA-P031-CUR
      *R03.5
           MOVE    ZERO                TO  SPA-P031-LOCK1
      *    一覧表最新表示
           MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P031-SYORI      TO  WRK-SYORI
           MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
           MOVE    SPA-P031-CHNGKBN-KBN    TO  WRK-CHNGKBN-KBN
           MOVE    SPA-P031-CHNGKBN-G      TO  WRK-CHNGKBN
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC
           .
       4110-B11S-SYORI-EXT.
           EXIT.
      *2025/05
      *****************************************************************
      *    一覧更新処理（タイマー）
      *****************************************************************
       4110-B11S-TIMER-SEC             SECTION.
      *
      *    選択中は更新なし
           IF     (SPA-P031-SELNUM     =   ZERO  )
              AND (SPA-P031-SEL-UUID   =   SPACE )
               CONTINUE
           ELSE
               GO  TO  4110-B11S-TIMER-EXT
           END-IF
      *    一覧更新
           PERFORM 4110-B11S-SYORI-SEC
           .
       4110-B11S-TIMER-EXT.
           EXIT.
      *2024/12
      *****************************************************************
      *    同意一括照会処理
      *****************************************************************
       4110-B05S-SYORI-SEC             SECTION.
      *
      *    選択中はエラー
           IF     (SPA-P031-SELNUM     NOT =   ZERO )
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-P031-CUR
               GO      TO   4110-B05S-SYORI-EXT
           END-IF
           IF     (SPA-ONSHI-HOUMON    =   "1"  )
              OR  (SPA-ONSHI-ONLINE    =   "1"  )
               PERFORM 41101-P0311-SYORI-SEC
           END-IF
           .
       4110-B05S-SYORI-EXT.
           EXIT.
      *2024/12
      *****************************************************************
      *    同意一括照会処理
      *****************************************************************
       41101-P0311-SYORI-SEC        SECTION.
      *
      *    初期画面編集
           INITIALIZE                      SPA-P0311-AREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    "P031"              TO  SPA-MOTOPG
           MOVE    "P031"              TO  SPA-P0311-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP0311"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    "P031"              TO  SPA-MOTOPG
           MOVE    "P0311"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P0311"             TO  MCP-WINDOW
      *
           IF      SPA-P0311-CUR       =   2
               MOVE    "STRYMD"            TO  MCP-WIDGET
           END-IF
           IF      SPA-P0311-CUR       =   10
               MOVE    "B10"               TO  MCP-WIDGET
           END-IF
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       41101-P0311-SYORI-EXT.
           EXIT.
      *
      *2023/11
      *****************************************************************
      *    予約照会　枝番更新処理
      *****************************************************************
       4110-B08S-SYORI-SEC             SECTION.
      *
           IF      SPA-P031-SYORI  NOT =   6
               MOVE    2                   TO  SPA-P031-CUR
      *        予約照会一覧へ
      *         MOVE    SPA-P031-CHNGKBN-L-MEI (5)
      *                                 TO  P031-CHNGKBN
      *         PERFORM 4002-CHNGKBN-SYORI-SEC
               GO      TO   4110-B08S-SYORI-EXT
           END-IF
      *
           IF     (SPA-P031-SELNUM     =   ZERO )
              OR  (SPA-P031-SEL-UUID   =   SPACE )
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P031-CUR
               GO      TO   4110-B08S-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >    SPA-P031-MAX )
                          OR  (IDX          >   ZERO  )
               IF      SPA-P031-SELNUM      =   SPA-P031-RENNUM
                                                             (IDY)
                   MOVE    IDY              TO  IDX
               END-IF
           END-PERFORM
           IF      IDX                 =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO   4110-B08S-SYORI-EXT
           END-IF
      *
      *    排他中などのチェック
           MOVE    1                   TO  FLG-CHKKBN
           PERFORM 4080-UPD-CHECK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *        枝番変更可であること
               IF     SPA-P031-YYKCHK(IDX) =   "1"
                   CONTINUE
               ELSE
                   MOVE    "0100"          TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0200"              TO  SPA-PIDCD
           END-IF
      *
           .
       4110-B08S-SYORI-EXT.
           EXIT.
      *2023/11
      *****************************************************************
      *    予約照会　枝番更新処理
      *****************************************************************
       4120-EDABANN-UPDSYORI-SEC             SECTION.
      *
      *    排他中などのチェック
           MOVE    1                   TO  FLG-CHKKBN
           PERFORM 4080-UPD-CHECK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4120-EDABANN-UPDSYORI-EXT
           END-IF
      *
      *    処理中の設定
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "P031 ONSKAKU KEY UPD ERR:"  MCP-RC
                   ",KEY:" ONSKAKU-KEY
               GO      TO  4120-EDABANN-UPDSYORI-EXT
           END-IF
      *
           INITIALIZE                      ORCSONSHI003AREA
           MOVE    "03"                TO  LNK-ONSHI003-SYOKBN
           MOVE    "UPD"               TO  LNK-ONSHI003-SYOKBN2
           MOVE    SPA-P031-SYSYMD     TO  LNK-ONSHI003-SYSYMD
           CALL    "ORCSONSHI003"      USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSONSHI003AREA
                                       ONSKAKU-REC
      *
      *    オン資格処理中解除
           PERFORM 30009-ONSHIKAKU-CR-SEC
      *
           EVALUATE    LNK-ONSHI003-YYKERR
               WHEN    "00"
      *        枝番対応済み
                   CONTINUE
               WHEN    "10"
      *        更新終了
                   CONTINUE
               WHEN    "19"
                   MOVE    "1001"              TO  SPA-ERRCD
               WHEN    "09"
      *        更新エラー
                   MOVE    "0109"              TO  SPA-ERRCD
               WHEN    "99"
      *        患者ID排他中
                   MOVE    "0199"              TO  SPA-ERRCD
                   MOVE    LNK-ONSHI003-YYKERRMSG
                                               TO  SPA-P031-LOCK-MSG
           END-EVALUATE
      *    排他中、更新エラー以外は、リスト更新
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4120-EDABANN-UPDSYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  SPA-P031-SELNUM
      *    一覧表最新表示
           MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P031-SYORI      TO  WRK-SYORI
           MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
           MOVE    SPA-P031-CHNGKBN-KBN    TO  WRK-CHNGKBN-KBN
           MOVE    SPA-P031-CHNGKBN-G      TO  WRK-CHNGKBN
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC 
           IF      SPA-P031-MAX        =   ZERO
               MOVE    08                  TO  SPA-P031-CUR
           ELSE
               MOVE    1                   TO  SPA-P031-CUR
           END-IF 
           .
       4120-EDABANN-UPDSYORI-EXT.
           EXIT.
      **
      *****************************************************************
      *    排他処理中解除処理
      *****************************************************************
       4031-LOCK-DELETE-SEC             SECTION.
      * 
           IF     (SPA-P031-LOCK1     =   2 )
             AND  (SPA-P031-SEL-UUID  NOT =   SPACE)
               PERFORM 30009-ONSHIKAKU-CR-SEC
           END-IF
           MOVE    ZERO                TO  SPA-P031-LOCK1
           MOVE    ZERO                TO  SPA-P031-SELNUM
           MOVE    ZERO                TO  SPA-P031-LOCK-SEL
      *
      *    一覧表最新表示
           MOVE    SPA-P031-SYSYMD     TO  WRK-SYMD
           MOVE    SPA-P031-SYORI      TO  WRK-SYORI
           MOVE    SPA-P031-SYORI2     TO  WRK-SYORI2
      *    オンライン資格確認照会編集処理
           PERFORM 30001-P031-INIT-SEC  
           .
       4031-LOCK-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    患者確定処理
      *****************************************************************
       412-KAKUTEI-SYORI-SEC             SECTION.
           .
      *
       412-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者ID確認処理
      *****************************************************************
       4102-KAKUTEI-PTID-SEC             SECTION.
      * 
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    ONSKAKU-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0006"          TO  SPA-ERRCD
           END-IF
           .
       4102-KAKUTEI-PTID-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録画面へ確定処理
      *****************************************************************
       410-P02-BACK-SEC             SECTION.
      *
      *    確認エラー
           INITIALIZE                      SPA-ONSKAKU-AREA
           INITIALIZE                      SPA-ONSKAKU-KOHTEI-AREA
           MOVE    "9"                    TO  SPA-ONSKAKU-SYORI
           MOVE    ONSKAKU-PTID           TO  SPA-ONSKAKU-PTID
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    ONSKAKU-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  SPA-ONSKAKU-PTNUM
           END-IF
           MOVE    SPA-P031-SEL-UUID      TO  SPA-ONSKAKU-UID
      *
           PERFORM 210-BACK
           .
      *
       410-P02-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者確定処理
      *****************************************************************
       490-KAKUTEI-SYORI-SEC           SECTION.
      *
           .
       490-KAKUTEI-SYORI-EXT.
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
           MOVE    "P031"              TO  SPA-MOTOPG
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
           MOVE    "P031"              TO  MCP-WINDOW.
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
                           SPA-P031-LOCK-MSG
                                               DELIMITED  BY   SIZE
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
      *R03.5
               WHEN    "0007"
                   STRING  "他端末処理中が解除となっています。"
                           "再度選択して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *R03.10
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
               WHEN    "0011"
                   STRING  "既に処理済みです。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0012"
                   STRING  "保険証ＯＣＲの不正データです。"
                           "処理できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "保険証ＯＣＲのデータではありません。"
                           "処理できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0014"
                   STRING  "予約照会です。"
                           "患者確認はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0015"
                   STRING  "表示リスト入力エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0016"
                   MOVE    "選択番号を入力して下さい。"
                                               TO  SPA-ERRMSG
      *2024/02
               WHEN    "0017"
                   MOVE    "生年月日が不明です。選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "医療扶助対応前の情報です。選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "医療扶助未登録のため、選択できません。"
                                               TO  SPA-ERRMSG
      *2024/04
               WHEN    "0020"
                   MOVE    "選択中です。解除して下さい。"
                                               TO  SPA-ERRMSG
      *2024/04
               WHEN    "0021"
                   STRING  "保険併用の医療扶助照会中です。"
                           "選択できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *2023/11
               WHEN    "0199"
                   STRING  "選択した患者は他端末で処理中です。"
                           "処理できません。"
                                               DELIMITED  BY  SIZE
                           SPA-P031-LOCK-MSG   DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0100"
                   STRING  "枝番更新対象ではありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0109"
                   STRING  "患者保険情報更新エラー"
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
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P031"              TO  SPA-MOTOPG
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
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PIDMSG
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   STRING  "選択した情報と関係するデータをすべて"
                           "削除します。"
                           "よろしいですか？"  DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "保険情報・性別・生年月日で同一の患者が"
                           "あります。新規患者でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *R03.5
               WHEN    "0103"
                   STRING  "他端末で処理中でないことを確認して下さい。"
                                               DELIMITED  BY  SIZE
                           SPA-P031-LOCK-MSG
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "他端末で処理中でないことを確認して下さい。"
                           "「ＯＫ」で他端末処理中を解除します。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      ****
      *R04.1
               WHEN    "0105"
                   STRING  "限度額情報が設定されています。"
                           "患者さんの同意が得られていますか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      ****
      *R04.2
               WHEN    "0106"
                   STRING  "選択した情報を処理済みとします。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *2023/11
               WHEN    "0200"
                   STRING  "患者保険情報の枝番を更新します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *
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
           IF      SPA-PIDCD           =   "0105"
               MOVE    "ＮＯ"              TO  PID1-B01
           END-IF
           MOVE    "ＯＫ"              TO  PID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
           IF      SPA-PIDCD           =   "0103"
                                       OR  "0104"
                                       OR  "0105"
                                       OR  "0101"
              MOVE    "B01"            TO  MCP-WIDGET
           END-IF
      *
           MOVE    "P031"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
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
      **    患者マスター読み込み
      *****************************************************************
       9010-PTINF-KEY-READ-SEC        SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       9010-PTINF-KEY-READ-EXT.
           EXIT.
      *****************************************************************
      **    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
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
