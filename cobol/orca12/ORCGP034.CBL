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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGP034.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : オンライン資格確認　公費・低所得者表示）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  21/11/01    ORCAMO        新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.02.00    ORCAMO       23/04/XX  「946」受給者番号半角対応
      *  05.02.00    ORCAMO       24/02/XX  医療扶助対応
      *  05.02.00    ORCAMO       25/05/XX  公費開始日01日チェック対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
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
      *    Ｐ０２画面スパ領域
           COPY    "P02SCR-SPA".
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ONSKAKU         PIC 9(01).
      *
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-TSYRRK          PIC 9(01).
           03  FLG-TNKRRK          PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-MONTHLYNUM      PIC 9(01).
      *
           03  FLG-KOHARI          PIC 9(01).
           03  FLG-KOHOK           PIC 9(01).
           03  FLG-ADDOK           PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-IN-FTNJANUM     PIC 9(01).
           03  FLG-CLCHK           PIC 9(01).
      *
           03  FLG-JYUFUKU         PIC 9(01).
           03  FLG-FUJYO           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-OSK             PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SK              PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-CK              PIC 9(04).
           03  IDX2                PIC 9(04).
      *
           03  IDY-2               PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-ARI             PIC 9(04).
           03  IDX-SK2             PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDM                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KOHMEI              PIC X(10).
           03  WRK-KOHNUM-MEI          PIC X(14).
           03  WRK-TEKSTYMD            PIC X(10).
           03  WRK-TEKEDYMD            PIC X(10).
           03  WRK-JKYSNUM             PIC X(20).
           03  WRK-TNKKYOUKAI          PIC X(30).
           03  WRK-TNKTEKKBN           PIC X(30).
           03  WRK-FUKUSHIMEI          PIC X(30).
           03  WRK-TSYKYOUKAI          PIC X(30).
           03  WRK-TSYTEKKBN           PIC X(30).
           03  WRK-SKJGNSTYMD          PIC X(10).
           03  WRK-KOHCHK              PIC X(20).
           03  WRK-CHKMSG              pic x(06).
      *
           03  WRK-FTNJANUM            PIC X(08).
           03  WRK-IDX                 PIC 9(04).
           03  WRK-KOHCNT              PIC 9(03).
      *
           03  WRK-PIDMSG              PIC X(100).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    保険公費種別
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNKOHSBTKBN        PIC X(01).
           03  WRK-TANSEIDONAME        PIC X(20).
      *
      *    認定範囲区分編集処理
           03  WRK-HKNTEKKBN-MEI       PIC X(12).
           03  WRK-HKNTEKKBN           PIC X(01).
           03  IDX-TEK                 PIC 9(04).
           03  WRK-KYOUKAI             PIC X(01).
           03  WRK-KYOUKAI-MEI         PIC X(20).
      *
      *    法別チェック
           03  WRK-HBTNUM              PIC X(02).
           03  WRK-CHK-KOHNUM          PIC X(03).
           03  WRK-MAE-KOHNUM          PIC X(03).
           03  WRK-HBTCNT              PIC 9(02).
           03  WRK-MAE-HBTNUM          PIC X(02).
           03  WRK-MAE-FTNJANUM        PIC X(08).
      *
           03  WRK-CHK-JKYUSNUM        PIC X(20).
      *
           03  WRK-ZZ                  PIC ZZ.
      *2023/12
           03  WRK-KOHMAX              PIC 9(03).
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-SELECT              PIC 9(04).
      *
           03  WRK-NYUGAIKBN-MED       PIC X(06).
           03  WRK-MONTHLYNUM-SRYYM    PIC X(06).
      *
      *ORCGP024.CBLと同じ。
      *    認定範囲一覧表示
       01  TBL-HKNTEKKBN-AREA.
           03  FILLER              PIC X(14)   VALUE   "0 全対象".
           03  FILLER              PIC X(14)   VALUE   "1 地対象外".
           03  FILLER              PIC X(14)   VALUE   "2 地のみ対象".
           03  FILLER              PIC X(14)   VALUE   "3 食のみ対象".
           03  FILLER              PIC X(14)   VALUE   "4 地＋食対象".
       01  TBL-HKNTEKKBN-AREAR     REDEFINES   TBL-HKNTEKKBN-AREA.
           03  TBL-HKNTEKKBN-G     PIC X(14)   OCCURS   5.
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
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    オンライン資格確認照会結果テーブル
       01  ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    月代り受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    限度額適用認定証など編集パラメタ
           COPY    "CPORCSONSHI002.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *   患者番号発生サブ
      *     COPY    "CPORCSP01.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
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
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      FLG-END             =   ZERO
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "P034"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
      *
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
               WHEN    "P02H"
                   PERFORM 3010-P02H-HEN-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
      *            公費追加
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 41201-KOHIADD-SYORI-SEC
                   ELSE
                       MOVE    11              TO  SPA-P034-CUR
                   END-IF
               WHEN    "0102"
      *            初期表示状態に戻す
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 3000-SCREEN-INIT-SEC
                   ELSE
                       MOVE    1               TO  SPA-P034-CUR
                   END-IF
      *2024/02
               WHEN    "0104"
                   PERFORM 41109-KOUHI-MONTHUPD-SEC
      *2024/02
               WHEN    "0103"
               WHEN    "0105"
                   IF      SPA-PID1-FLG        =   "OK"
      *                追加
                       PERFORM 41201-KOHIADD-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-P034-CUR
                   END-IF
            END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費種類選択より処理
      *****************************************************************
       3010-P02H-HEN-SEC              SECTION.
      *
           IF      SPA-P02H-KOHNUM     NOT =   SPACE
               MOVE    SPA-P02H-KOHNUM     TO  P034-KOHNUM
               MOVE    "KOHNUM"            TO  MCP-WIDGET
               INITIALIZE                  SPA-P02H-AREA
               PERFORM 410-INPUT-CHK-SEC
           ELSE
               INITIALIZE                  SPA-P02H-AREA
           END-IF
      *
           .
       3010-P02H-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-PIDCD
           INITIALIZE                  SPA-P034-GMN-AREA
      *    オンライン資格確認UID検索処理
           PERFORM 4103-ONSKAKU-KEY-SEC
      *
      *    提供同意の判定（地方公費があれば追加）
           IF     (ONSKAKU-GENDO-DOUIFLG   =   "1"  )
              OR  (ONSKAKU-SIKKAN-DOUIFLG  =   "1"  )
      *       保険証確認で同意あり
              IF   (ONSKAKU-SHOKAI-KBN         =   "2")
              AND  (SPA-GENDO-DOUIFLG      NOT =   "1")
      *        保険証確認で同意なし
                   MOVE    "【提供同意なし】"
                                       TO  SPA-P034-MIDASHI
      *R05.1       保険証OCR
                   IF     (ONSKAKU-ORCA-SHOKAI-KBN    =   "2" )
                       MOVE    SPACE       TO  SPA-P034-MIDASHI
                   END-IF
               END-IF
           ELSE
      *        対象なし
               MOVE    "【提供同意なし】"
                                       TO  SPA-P034-MIDASHI
      *2024/01
      *        医療扶助
               IF      ONSKAKU-CARD-CLASS  =   "A1"
                   MOVE    SPACE           TO  SPA-P034-MIDASHI
               END-IF
           END-IF
      *
      *    限度額認定証などから公費等編集
           INITIALIZE                      ORCSONSHI002AREA
           MOVE    "03"                TO  LNK-ONSHI002-SYOKBN
           MOVE    SPA-GENDO-DOUIFLG   TO  LNK-ONSHI002-DOUIFLG
      *    交付番号の転記
           MOVE    SPA-ONSHI-KOFUTNK   TO  LNK-ONSHI002-KFTNKKBN
      *
           CALL    "ORCSONSHI002"      USING
                                           SPA-AREA
                                           ORCSONSHI002AREA
                                           ONSKAKU-REC
      *    名称
      *R05.2
      *    限度額適用認定証
           MOVE    LNK-ONSHI002-GENDO-MSG
                                       TO  SPA-P034-GENDO-MSG
      *test
      *    限度額適用認定証エラー
      *     MOVE    LNK-ONSHI002-GENDO-KEIMSG
      *test                              TO  SPA-P034-GENDO-KEIMSG
      *    特定疾病療養受療証提供
           PERFORM VARYING    IDX-SK    FROM    1   BY  1
                   UNTIL      IDX-SK    >   3
               IF      LNK-ONSHI002-SIKKAN-MSG(IDX-SK)
                                         NOT =   SPACE
                   MOVE    LNK-ONSHI002-SIKKAN-MSG(IDX-SK)
                                             TO  SPA-P034-SIKKAN-MSG
                                                           (IDX-SK)
               END-IF
           END-PERFORM
      *2024/02
      *    医療扶助情報
           MOVE    ZERO                TO  SPA-P034-FUJARI
           MOVE    ZERO                TO  SPA-ONSHI002-FUJYO-MAX
           PERFORM VARYING IDX-SK      FROM    1   BY  1
                   UNTIL   (IDX-SK     >   10   )
                       OR  (LNK-ONSHI002-FUJYO-MSG(IDX-SK)
                                           =   SPACE )
               MOVE    LNK-ONSHI002-FUJYO-MSG(IDX-SK)
                                           TO  SPA-ONSHI002-FUJYO-MSG
                                                           (IDX-SK)
               MOVE    IDX-SK              TO  SPA-ONSHI002-FUJYO-MAX
      *        交付番号あり
               IF      LNK-ONSHI002-KOFUNUM (IDX-SK 1)
                                           NOT =   SPACE
                   MOVE    1                   TO  SPA-P034-FUJARI
               END-IF
      *
           END-PERFORM
      *2024/01
      *    医療扶助（限度額などはなし）
           MOVE    ONSKAKU-CARD-CLASS  TO  SPA-P034-CARD-CLASS
           IF      ONSKAKU-CARD-CLASS  =   "A1"
               MOVE    ZERO                TO  SPA-P034-FUJYOIDX
               MOVE    "2"                 TO  SPA-P034-ONSMSG-KBN
               PERFORM 30005-FUJYOMSG-HEN-SEC
               MOVE    ZERO                TO  SPA-P034-FUJARI
           ELSE
               MOVE    "1"                 TO  SPA-P034-ONSMSG-KBN
               PERFORM 30004-GEDPMSG-HEN-SEC
               MOVE    "限度額認定証"      TO  SPA-P034-LABEL01
               MOVE    "特定疾受給証"      TO  SPA-P034-LABEL02
           END-IF
      *
      *    患者未設定
           IF     (SPA-GMN-PTNUM       =   SPACE )
             AND  (SPA-ONSKAKU-PTID    NOT =   ZERO)
      *        公費・低所得者のSPAを設定
               PERFORM 30004-PTKOHHEN-SEC
           END-IF
      *
      *
           MOVE    ZERO                TO  IDX-K
      *    低所得１（低年金）
           IF      SPA-ONSKAKU-TNKSTYMD    NOT =   SPACE
      *        日付編集
               MOVE    SPA-ONSKAKU-TNKSTYMD    TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  WRK-TEKSTYMD
      *
               MOVE    SPA-ONSKAKU-TNKEDYMD    TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  WRK-TEKEDYMD
      *        老齢福祉年金
               MOVE    SPACE               TO  WRK-FUKUSHIMEI
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-GMN24-FUKUSHI-MAX)
                         OR   (WRK-FUKUSHIMEI  NOT =   SPACE )
                   IF      SPA-ONSKAKU-FUKUSHI
                                           =   SPA-GMN24-FUKUSHI-LST
                                                      (IDY)(1:1)
                       MOVE    SPA-GMN24-FUKUSHI-LST (IDY)(3:)
                                           TO  WRK-FUKUSHIMEI
                   END-IF
               END-PERFORM
      *        認定範囲
               MOVE    SPACE               TO  WRK-TNKTEKKBN
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-GMN24-TNKTEKKBN-MAX)
                         OR   (WRK-TNKTEKKBN    NOT =   SPACE )
                   IF      SPA-ONSKAKU-TNKTEKKBN
                                           =   SPA-GMN24-TNKTEKKBN-LST
                                                      (IDY)(1:1)
                       MOVE    SPA-GMN24-TNKTEKKBN-LST (IDY)(3:)
                                           TO  WRK-TNKTEKKBN
                   END-IF
               END-PERFORM
      *        境界層該当区分
               MOVE    SPACE               TO  WRK-TNKKYOUKAI
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-GMN24-TNKKYOUKAI-MAX)
                         OR   (WRK-TNKKYOUKAI    NOT =   SPACE )
                   IF      SPA-ONSKAKU-TNKKYOUKAI
                                           =   SPA-GMN24-TNKKYOUKAI-LST
                                                      (IDY)(1:1)
                       MOVE    SPA-GMN24-TNKKYOUKAI-LST (IDY)(3:)
                                           TO  WRK-TNKKYOUKAI
                   END-IF
               END-PERFORM
               MOVE    ZERO                TO  FLG-KOHARI
      *
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY         >   SPA-TNK-MAX)
                            OR    (FLG-KOHARI  =   1  )
                   IF     (SPA-GMN24-TNKDELKBN (IDY)   =   SPACE)
                     AND  (SPA-NAI24-TNKLHKNKBN(IDY)
                                       =   SPA-ONSKAKU-TNKTEKKBN )
                     AND  (SPA-GMN24-TFUKUSHI-KBN (IDY)
                                       =   SPA-ONSKAKU-FUKUSHI   )
                     AND  (SPA-NAI24-TNKLKYOUKAI (IDY)
                                       =   SPA-ONSKAKU-TNKKYOUKAI )
      *              終了日が期間内
                       IF     (SPA-GMN24-TNKDPSTYMD(IDY)
                                       <=  SPA-ONSKAKU-TNKEDYMD )
                         AND  (SPA-GMN24-TNKDPEDYMD(IDY)
                                       >=  SPA-ONSKAKU-TNKEDYMD )
                           IF     (SPA-GMN24-TNKDPSTYMD(IDY)
                                       =  SPA-ONSKAKU-TNKSTYMD )
                           AND    (SPA-GMN24-TNKDPEDYMD(IDY)
                                       =  SPA-ONSKAKU-TNKEDYMD  )
                               MOVE    1           TO  FLG-KOHARI
                           ELSE
      *2025/05
      *                        開始日を１日対応反映
                               IF     (SPA-GMN24-TNKDPSTYMD(IDY)(1:6)
                                       =   SPA-ONSKAKU-TNKSTYMD(1:6) )
                               AND    (SPA-GMN24-TNKDPSTYMD(IDY)(7:2)
                                       =   "01"                    )
                               AND    (SPA-GMN24-TNKDPEDYMD(IDY)
                                       =   SPA-ONSKAKU-TNKEDYMD  )
                               MOVE    1           TO  FLG-KOHARI
                           ELSE
                               MOVE    2           TO  FLG-KOHARI
                           END-IF
      *
                           END-IF
                       ELSE
                           MOVE    3           TO  FLG-KOHARI
                           MOVE    "9"         TO  SPA-ONSKAKU-TNKCHK
                       END-IF
                   END-IF
               END-PERFORM
      *
               ADD     1                   TO  IDX-K
               STRING  "低所得１　"
                       WRK-TEKSTYMD
                       "〜"
                       WRK-TEKEDYMD        DELIMITED  BY  SIZE
                       "　認定範囲："  
                       WRK-TNKTEKKBN       DELIMITED  BY  SPACE
                       "　減額開始日："
                       WRK-TEKSTYMD         DELIMITED  BY  SIZE
                       "　老齢福祉年金："
                       WRK-FUKUSHIMEI      DELIMITED BY  SPACE
      *                 "　境界層該当："
                       "　"
                       WRK-TNKKYOUKAI
                                           DELIMITED BY  SPACE
                                       INTO   SPA-P034-KOHMSG
                                                    (IDX-K)
               END-STRING
      *         IF      SPA-GMN-PTNUM   NOT =   SPACE
                   MOVE    FLG-KOHARI      TO  SPA-P034-KOHCHK(IDX-K)
      *         END-IF
               MOVE    "2"                 TO  SPA-P034-DATAKBN
                                                            (IDX-K)
            END-IF
      * 
      *    低所得者２（低所得）
      **** IF      LNK-ONSHI002-STK2-TEKSTYMD NOT =   SPACE
           IF      SPA-ONSKAKU-TSYSTYMD    NOT =   SPACE
      *        日付編集
               MOVE    SPA-ONSKAKU-TSYSTYMD    TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  WRK-TEKSTYMD
      *
               MOVE    SPA-ONSKAKU-TSYEDYMD    TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  WRK-TEKEDYMD
      *
               IF      SPA-ONSKAKU-TSYSKJGNSTYMD  =   SPACE
                   MOVE    SPACE           TO  WRK-SKJGNSTYMD
               ELSE
                   MOVE    SPA-ONSKAKU-TSYSKJGNSTYMD  TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  WRK-SKJGNSTYMD
               END-IF
      *        認定範囲
               MOVE    SPACE               TO  WRK-TSYTEKKBN
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-GMN24-TSYTEKKBN-MAX)
                         OR   (WRK-TSYTEKKBN    NOT =   SPACE )
                   IF      SPA-ONSKAKU-TSYTEKKBN
                                           =   SPA-GMN24-TSYTEKKBN-LST
                                                      (IDY)(1:1)
                       MOVE    SPA-GMN24-TSYTEKKBN-LST (IDY)(3:)
                                           TO  WRK-TSYTEKKBN
                   END-IF
               END-PERFORM
      *        境界層該当区分
               MOVE    SPACE               TO  WRK-TSYKYOUKAI
               PERFORM VARYING IDY         FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-GMN24-TSYKYOUKAI-MAX)
                         OR   (WRK-TSYKYOUKAI    NOT =   SPACE )
                   IF      SPA-ONSKAKU-TSYKYOUKAI
                                           =   SPA-GMN24-TSYKYOUKAI-LST
                                                      (IDY)(1:1)
                       MOVE    SPA-GMN24-TSYKYOUKAI-LST (IDY)(3:)
                                           TO  WRK-TSYKYOUKAI
                   END-IF
               END-PERFORM
      *
               MOVE    ZERO                TO  FLG-KOHARI
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY         >   SPA-TSY-MAX)
                            OR    (FLG-KOHARI  =   1  )
                   IF     (SPA-GMN24-TSYDELKBN (IDY)   =   SPACE)
                     AND  (SPA-NAI24-TSYLHKNKBN(IDY)
                                       =   SPA-ONSKAKU-TSYTEKKBN )
                     AND  (SPA-GMN24-TSYLSKJGNSTYMD(IDY)
                                       =   SPA-ONSKAKU-TSYSKJGNSTYMD )
                     AND  (SPA-NAI24-TSYLKYOUKAI (IDY)
                                       =   SPA-ONSKAKU-TSYKYOUKAI )
      *              終了日が期間内
                       IF     (SPA-GMN24-TSYDPSTYMD(IDY)
                                      <=  SPA-ONSKAKU-TSYEDYMD)
                         AND  (SPA-GMN24-TSYDPEDYMD(IDY)
                                       >=  SPA-ONSKAKU-TSYEDYMD)
                           IF     (SPA-GMN24-TSYDPSTYMD(IDY)
                                         =   SPA-ONSKAKU-TSYSTYMD )
                            AND   (SPA-GMN24-TSYDPEDYMD(IDY)
                                         =   SPA-ONSKAKU-TSYEDYMD)
                               MOVE    1           TO  FLG-KOHARI
                           ELSE
      *2025/05
      *                    開始日を１日対応反映
                           IF     (SPA-GMN24-TSYDPSTYMD(IDY)(1:6)
                                         =   SPA-ONSKAKU-TSYSTYMD(1:6))
                            AND   (SPA-GMN24-TSYDPSTYMD(IDY)(7:2)
                                         =   "01"               )
                            AND   (SPA-GMN24-TSYDPEDYMD(IDY)
                                         =   SPA-ONSKAKU-TSYEDYMD)
                               MOVE    1           TO  FLG-KOHARI
                           ELSE
                               MOVE    2           TO  FLG-KOHARI
                           END-IF
      *
                           END-IF
                       ELSE
                           MOVE    3           TO  FLG-KOHARI
                       END-IF
                   END-IF
               END-PERFORM
      *
               ADD     1                   TO  IDX-K
               STRING  "低所得２　"
                       WRK-TEKSTYMD
                       "〜"
                       WRK-TEKEDYMD        DELIMITED  BY  SIZE
                       "　認定範囲："  
                       WRK-TSYTEKKBN       DELIMITED  BY  SPACE
                       "　減額開始日："
                       WRK-TEKSTYMD         DELIMITED  BY  SIZE
                       "　長期入院該当日："
                       WRK-SKJGNSTYMD      DELIMITED BY  SIZE
      *                "　境界層該当："
                       "　"
                       WRK-TSYKYOUKAI
                                           DELIMITED BY  SPACE
                                       INTO   SPA-P034-KOHMSG
                                                    (IDX-K)
               END-STRING
      *         IF      SPA-GMN-PTNUM   NOT =   SPACE
                   MOVE    FLG-KOHARI      TO  SPA-P034-KOHCHK(IDX-K)
      *         END-IF
               MOVE    "2"                 TO  SPA-P034-DATAKBN
                                                            (IDX-K)
           END-IF
      *
      *    公費
      *    （SPA-ONSKAKU-PTKOH-G）をSPA-P034-KOH へ編集
           PERFORM VARYING  IDX-OSK        FROM     1   BY  1
                   UNTIL         ( IDX-OSK     >   7   )
                           OR    ( IDX-OSK     >   SPA-ONSKAKU-KOHMAX)
                           OR    ( IDX-K       >=  7  )
      *
               ADD     1                   TO  IDX-K
      *        SPA-ONSKAKU-KOH の位置
               MOVE    IDX-OSK             TO  SPA-P034-KOHIDX
                                                        (IDX-K)
      *
               MOVE    SPA-ONSKAKU-KOHNUM(IDX-OSK)
                                           TO  SPA-P034-STKOHNUM
                                                          (IDX-K)
               MOVE    SPA-ONSKAKU-FTNJANUM (IDX-OSK)
                                           TO  SPA-P034-STFTNJANUM
                                                          (IDX-K)
               MOVE    SPA-ONSKAKU-JKYSNUM (IDX-OSK)
                                           TO  SPA-P034-STJKYSNUM
                                                          (IDX-K)
               MOVE    SPA-ONSKAKU-TEKSTYMD(IDX-OSK)
                                           TO  SPA-P034-STTEKSTYMD
                                                          (IDX-K)
               MOVE    SPA-ONSKAKU-TEKEDYMD(IDX-OSK)
                                           TO  SPA-P034-STTEKEDYMD
                                                          (IDX-K)
               MOVE    SPA-ONSKAKU-CHIKOU (IDX-OSK)
                                           TO  SPA-P034-CHIKOU
                                                          (IDX-K)
      *        処理済み
               IF      SPA-ONSKAKU-SYORIFLG (IDX-OSK)
                                           =   "1"
                                           OR  "9"
                   MOVE    SPA-ONSKAKU-SYORIFLG (IDX-OSK)
                                           TO  SPA-P034-TSYORIKBN
                                                          (IDX-K)
               END-IF
      *        処理済み
               IF      SPA-ONSKAKU-KOHCHK (IDX-OSK)  =   "1"
                   MOVE    SPA-ONSKAKU-KOHCHK (IDX-OSK)
                                           TO  SPA-P034-TSYORIKBN
                                                          (IDX-K)
               END-IF
      *
                   
      *        公費のデータ
               MOVE    "1"                 TO  SPA-P034-DATAKBN
                                                            (IDX-K)
           END-PERFORM
      *
           MOVE    IDX-K               TO  SPA-P034-KOHMSG-MAX
      *
      *    公費画面編集
           PERFORM 30005-P034KOHGMN-HEN-SEC
      *
      *   患者登録内容
      *    公費対象分のみ
           MOVE    ZERO                TO  IDH
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-PTKOH-MAX )
                        OR    (IDH     >=  40      )
               IF     (SPA-PTKOH-SAKJOKBN (IDY)    =   SPACE)
                   MOVE    ZERO                TO  FLG-KOHOK
                   IF     (SPA-PTKOH-KOHNUM   (IDY)
                                                   =   "966"
                                                   OR  "967"
                                                   OR  "972"
                                                   OR  "974" )
      *            高額・長期
                       MOVE    1               TO  FLG-KOHOK
                   END-IF
      *            同じ公費の種類
                   PERFORM VARYING     IDX-K2  FROM    1   BY  1
                           UNTIL      (IDX-K2  >   SPA-ONSKAKU-KOHMAX)
                                OR    (FLG-KOHOK   =   1  )
                       IF      (SPA-PTKOH-KOHNUM   (IDY)
                                               =   SPA-ONSKAKU-KOHNUM
                                                           (IDX-K2))
                           MOVE    1               TO  FLG-KOHOK
                       END-IF
      *
      *2023/04
      *                公費「946」の受給者番号は半角を全角で判断する
                       MOVE    SPA-PTKOH-JKYSNUM (IDY)
                                               TO  WRK-CHK-JKYUSNUM
                       IF      (SPA-PTKOH-KOHNUM(IDY)  =   "946" )
                           IF      WRK-CHK-JKYUSNUM        =   "1"
                               MOVE    "１"        TO  WRK-CHK-JKYUSNUM
                           END-IF
                           IF      WRK-CHK-JKYUSNUM        =   "2"
                               MOVE    "２"        TO  WRK-CHK-JKYUSNUM
                          END-IF
                       END-IF
      *
                       IF     (FLG-KOHOK           =   ZERO )
                         AND  (SPA-ONSKAKU-KOHNUM (IDX-K2)
                                                   =   SPACE )
                         AND  (SPA-ONSKAKU-JKYSNUM(IDX-K2)
                                           =   WRK-CHK-JKYUSNUM )
                         AND (SPA-ONSKAKU-FTNJANUM(IDX-K2)
                                           =   SPA-PTKOH-FTNJANUM
                                                        (IDY) )
                           MOVE    1           TO  FLG-KOHOK
                       END-IF
                   END-PERFORM
      *            公費表示
                   IF      FLG-KOHOK           =   1
                       ADD     1               TO  IDH
      *                公費名称
                       MOVE    SPA-PTKOH-KOHNUM (IDY)
                                           TO  SPA-P034-TKOHNUM
                                                                (IDH)
                       MOVE    SPA-PTKOH-KOHNUM-MEI (IDY)
                                           TO  SPA-P034-TKOHNUM-MEI
                                                                (IDH)
                       MOVE    SPA-PTKOH-JKYSNUM  (IDY)
                                               TO  SPA-P034-TJKYUSNUM
                                                                (IDH)
                       MOVE    SPA-PTKOH-FTNJANUM  (IDY)
                                               TO  SPA-P034-TFTNJANUM
                                                                (IDH)
                       MOVE    SPA-PTKOH-TEKSTYMDW (IDY)
                                                TO  SPA-P034-TTEKSTYMD
                                                                (IDH)
                       MOVE    SPA-PTKOH-TEKEDYMDW (IDY)
                                               TO  SPA-P034-TTEKEDYMD
                                                                (IDH)
      *
                       MOVE    SPA-PTKOH-KOHID(IDY)
                                               TO  SPA-P034-TKOHID
                                                                (IDH)
                       MOVE    SPA-PTKOH-TEKSTYMD(IDY)
                                               TO  SPA-P034-NTEKSTYMD
                                                                (IDH)
                       MOVE    SPA-PTKOH-TEKEDYMD(IDY)
                                               TO  SPA-P034-NTEKEDYMD
                                                                (IDH)
                       MOVE    IDH             TO  SPA-P034-KOHMAX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    低所得者２（低所得）の患者登録分
           IF      SPA-ONSKAKU-TSYSTYMD    NOT =   SPACE
               PERFORM 30002-TSTK2-HEN-SEC
           END-IF
      *
      *    低所得１（低年金）の患者登録分
           IF      SPA-ONSKAKU-TNKSTYMD    NOT =   SPACE
               PERFORM 30001-TSTK1-HEN-SEC
           END-IF
      *    今回どちらもない場合、登録がある２，１順で表示
           IF     (SPA-ONSKAKU-TSYSTYMD  =   SPACE)
              AND (SPA-ONSKAKU-TNKSTYMD  =   SPACE)
      *        低所得２の対応
               PERFORM 30002-TSTK2-HEN-SEC
               IF      SPA-P034-TEISYO-MAX     =   ZERO
      *            低所得１の対応
                   PERFORM 30001-TSTK1-HEN-SEC
               END-IF
           END-IF
      *
      *
           MOVE    1                   TO  SPA-P034-CUR
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療扶助情報編集処理
      *****************************************************************
       30004-GEDPMSG-HEN-SEC              SECTION.
      * 
           MOVE    ZERO                TO  IDX-SK
           INITIALIZE                  SPA-P034-ONSGMN-MSG-G
           MOVE    1                   TO  IDX-SK
           MOVE    SPA-P034-GENDO-MSG  TO  SPA-P034-ONSGMN-MSG (IDX-SK)
      *    特定疾病療養受療証提供
           PERFORM VARYING    IDX-SK2   FROM    1   BY  1
                   UNTIL      IDX-SK2   >   3
               IF      SPA-P034-SIKKAN-MSG(IDX-SK2)
                                        NOT =   SPACE
                   ADD     1               TO  IDX-SK 
                   MOVE    SPA-P034-SIKKAN-MSG(IDX-SK2)
                                           TO  SPA-P034-ONSGMN-MSG
                                                           (IDX-SK)
               END-IF
           END-PERFORM
           MOVE    "限度額認定証"      TO  SPA-P034-LABEL01
           MOVE    "特定疾受給証"      TO  SPA-P034-LABEL02
           .
       30004-GEDPMSG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    医療扶助情報編集処理
      *****************************************************************
       30005-FUJYOMSG-HEN-SEC              SECTION.
      * 
           MOVE    ZERO                TO  IDX-SK
           INITIALIZE                  SPA-P034-ONSGMN-MSG-G
           IF      SPA-P034-FUJYOIDX   >=  SPA-ONSHI002-FUJYO-MAX
      *        １件目から
               MOVE    ZERO                TO  IDX-STR
           ELSE
               MOVE    SPA-P034-FUJYOIDX   TO  IDX-STR
           END-IF
           ADD     1                   TO  IDX-STR
           PERFORM VARYING     IDXS    FROM    IDX-STR  BY  1
                   UNTIL      (IDXS        >       10   )
                         OR   (IDXS        >   SPA-ONSHI002-FUJYO-MAX)
                         OR   (IDX-SK      >=  4     )
               ADD     1                   TO  IDX-SK
               MOVE    SPA-ONSHI002-FUJYO-MSG (IDXS)
                                           TO  SPA-P034-ONSGMN-MSG
                                                           (IDX-SK)
               MOVE    IDXS                TO  SPA-P034-FUJYOIDX
           END-PERFORM
      *
           MOVE    "医療扶助"          TO  SPA-P034-LABEL01
           MOVE    SPACE               TO  SPA-P034-LABEL02
           .
       30005-FUJYOMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       30004-PTKOHHEN-SEC              SECTION.
      *
           INITIALIZE                   SPA-PTKOHINF-AREA
           INITIALIZE                   SPA-GMN24-TSYTBL
           INITIALIZE                   SPA-GMN24-TNKTBL
      *    公費編集
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-ONSKAKU-PTID    TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM     UNTIL     FLG-PTKOHINF  =   1
      *
               IF      PTKOH-SAKJOKBN        =   SPACE
                   ADD     1                     TO  IDX
      *            ＳＰＡに編集
                   IF      IDX               >  60
                       CONTINUE
                   ELSE
                       PERFORM 300041-PTKOHINF-SPA-SEC
                   END-IF
             END-IF
      *
             MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 920-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      
      *    低所得より
           MOVE    ZERO                TO  SPA-NAI-TSYRRK
           INITIALIZE                      TSYRRK-REC
           MOVE    SPA-HOSPNUM         TO  TSYRRK-HOSPNUM
           MOVE    SPA-ONSKAKU-PTID    TO  TSYRRK-PTID
      *
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-TSYRRK-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-TSYRRK
           END-IF
           MOVE    ZERO                TO  IDX
      *
           PERFORM   UNTIL   (FLG-TSYRRK    =   1  )
                       OR    (IDX           >=  20 )
      *
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-TSY-MAX
               MOVE    TSYRRK-NINSTYMD
                                         TO  SPA-GMN24-TSYDPSTYMD (IDX)
               MOVE    TSYRRK-NINSTYMD   TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD        TO  SPA-GMN24-TSYDPSTYMDW(IDX)
      *
               MOVE    TSYRRK-NINEDYMD 
                                         TO  SPA-GMN24-TSYDPEDYMD (IDX)
               MOVE    TSYRRK-NINEDYMD   TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD        TO  SPA-GMN24-TSYDPEDYMDW(IDX)
      *
               MOVE    TSYRRK-SKJGNSTYMD
                                         TO  SPA-GMN24-TSYLSKJGNSTYMD
                                                              (IDX)
               MOVE    TSYRRK-SKJGNSTYMD TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD        TO  SPA-GMN24-TSYLSKJGNSTYMDW
                                                                (IDX)
      *        標準負担額減額開始日
               MOVE    TSYRRK-SKJNINSTYMD
                                         TO  SPA-NAI24-TSYLNINYMD
                                                                  (IDX)
               MOVE    TSYRRK-SKJNINSTYMD  TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD      TO  SPA-GMN24-TSYLNINYMD
                                                                (IDX)
      *        認定範囲区分
               IF      TSYRRK-HKNTEKKBN    =   SPACE
                   MOVE    "0"             TO  TSYRRK-HKNTEKKBN
               END-IF
               MOVE    TSYRRK-HKNTEKKBN    TO  SPA-NAI24-TSYLHKNKBN
                                                                (IDX)
               MOVE    SPA-NAI24-TSYLHKNKBN(IDX)   TO  WRK-HKNTEKKBN
               PERFORM 41002-HKNTEKKBN-HEN-SEC
               MOVE    WRK-HKNTEKKBN-MEI       TO  SPA-GMN24-TSYLHKNKBN
                                                                (IDX)
      *        境界層該当区分
               IF      TSYRRK-KYOUKAISOUKBN   =   SPACE
                   MOVE    "0"         TO  TSYRRK-KYOUKAISOUKBN
               END-IF
               MOVE    TSYRRK-KYOUKAISOUKBN
                                       TO  SPA-NAI24-TSYLKYOUKAI(IDX)
               MOVE    TSYRRK-KYOUKAISOUKBN    TO  WRK-KYOUKAI
               PERFORM 41003-KYOUKAI-HEN-SEC
               MOVE    WRK-KYOUKAI-MEI TO  SPA-GMN24-TSYLKYOUKAI(IDX)
      *
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-TSYRRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    年金編集（低所得１）
           MOVE    ZERO                TO  SPA-NAI-TNKRRK
           INITIALIZE                      TNKRRK-REC
           MOVE    SPA-HOSPNUM         TO  TNKRRK-HOSPNUM
           MOVE    SPA-ONSKAKU-PTID    TO  TNKRRK-PTID
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-TNKRRK-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-TNKRRK
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM   UNTIL    FLG-TNKRRK   =   1
               ADD     1                   TO  IDX
      *
               MOVE    IDX                 TO  SPA-TNK-MAX
               MOVE    TNKRRK-STYMD 
                                       TO  SPA-GMN24-TNKDPSTYMD (IDX)
               MOVE    TNKRRK-STYMD    TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD      TO  SPA-GMN24-TNKDPSTYMDW(IDX)
      *
               MOVE    TNKRRK-EDYMD
                                       TO  SPA-GMN24-TNKDPEDYMD (IDX)
               MOVE    TNKRRK-EDYMD    TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD      TO  SPA-GMN24-TNKDPEDYMDW(IDX)
      *        高齢者福祉
               MOVE    TNKRRK-RRI-FUKUSHI-KBN
                                       TO  SPA-GMN24-TFUKUSHI-KBN(IDX)
               EVALUATE    TNKRRK-RRI-FUKUSHI-KBN
               WHEN    "0"
                   MOVE    "無し"      TO  SPA-GMN24-TFUKUSHI-MEI
                                                                (IDX)
               WHEN    "1"
                   MOVE    "有り"      TO  SPA-GMN24-TFUKUSHI-MEI
                                                                (IDX)
               END-EVALUATE
      *        標準負担額減額開始日
               MOVE    TNKRRK-SKJNINSTYMD
                                          TO  SPA-NAI24-TNKLNINYMD
                                                                  (IDX)
               MOVE    TNKRRK-SKJNINSTYMD TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD      TO  SPA-GMN24-TNKLNINYMD
                                                                (IDX)
      *        認定範囲区分
               IF      TNKRRK-HKNTEKKBN    =   SPACE
                   MOVE    "0"             TO  TNKRRK-HKNTEKKBN
               END-IF
               MOVE    TNKRRK-HKNTEKKBN    TO  SPA-NAI24-TNKLHKNKBN
                                                                (IDX)
               MOVE    SPA-NAI24-TNKLHKNKBN(IDX)   TO  WRK-HKNTEKKBN
               PERFORM 41002-HKNTEKKBN-HEN-SEC
               MOVE    WRK-HKNTEKKBN-MEI       TO  SPA-GMN24-TNKLHKNKBN
                                                                (IDX)
      *        境界層該当区分
               IF      TNKRRK-KYOUKAISOUKBN =   SPACE
                   MOVE    "0"         TO  TNKRRK-KYOUKAISOUKBN
               END-IF
               MOVE    TNKRRK-KYOUKAISOUKBN
                                       TO  SPA-NAI24-TNKLKYOUKAI(IDX)
               MOVE    TNKRRK-KYOUKAISOUKBN    TO  WRK-KYOUKAI
               PERFORM 41003-KYOUKAI-HEN-SEC
               MOVE    WRK-KYOUKAI-MEI TO  SPA-GMN24-TNKLKYOUKAI(IDX)
      *
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-TNKRRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
            .
       30004-PTKOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報ＳＰＡ編集処理
      *****************************************************************
       300041-PTKOHINF-SPA-SEC           SECTION.
      *
           MOVE    PTKOH-KOHID         TO  SPA-PTKOH-KOHID      (IDX)
           MOVE    PTKOH-KOHNUM        TO  SPA-PTKOH-KOHNUM     (IDX)
           MOVE    PTKOH-PAYKBN        TO  SPA-PTKOH-PAYKBN     (IDX)
           MOVE    PTKOH-FTNJANUM      TO  SPA-PTKOH-FTNJANUM   (IDX)
           MOVE    PTKOH-JKYSNUM       TO  SPA-PTKOH-JKYSNUM    (IDX)
           MOVE    PTKOH-TEKSTYMD      TO  SPA-PTKOH-TEKSTYMD   (IDX)
           MOVE    PTKOH-TEKEDYMD      TO  SPA-PTKOH-TEKEDYMD   (IDX)
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-PTKOH-KAKUNINYMD (IDX)
           MOVE    PTKOH-SAKJOKBN      TO  SPA-PTKOH-SAKJOKBN   (IDX)
           MOVE    PTKOH-TEKEDCHKKBN   TO  SPA-PTKOH-TEKEDCHKKBN (IDX)
      *
           MOVE    PTKOH-TEKSTYMD      TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-TEKSTYMDW (IDX)
      *
           MOVE    PTKOH-TEKEDYMD      TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-TEKEDYMDW (IDX)
      *
           MOVE    PTKOH-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-KAKUNINYMDW (IDX)
      *
      *    保険公費区分編集
           MOVE    PTKOH-KOHNUM        TO  WRK-HKNNUM
           MOVE    PTKOH-PAYKBN        TO  WRK-PAYKBN
           MOVE    PTKOH-TEKSTYMD      TO  WRK-TEKSTYMD
           MOVE    PTKOH-TEKEDYMD      TO  WRK-TEKEDYMD
      *    保険番号マスタ検索
           PERFORM 930-HKNNUM-KEYREAD-SEC
           MOVE    SPACE               TO  WRK-HKNKOHSBTKBN
           MOVE    SPACE               TO  WRK-TANSEIDONAME
           IF      FLG-HKNNUM          =   ZERO
               IF      WRK-TEKSTYMD        >=  HKN-TEKSTYMD
                   MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
                   MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
               END-IF
           END-IF     
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-PTKOH-HKN-SBTKBN (IDX)
           MOVE    WRK-TANSEIDONAME    TO  SPA-PTKOH-KOHNUM-MEI (IDX)
      *
           MOVE    IDX                 TO  SPA-PTKOH-MAX
      *
           .
       300041-PTKOHINF-SPA-EXT.
           EXIT.
      *****************************************************************
      *    認定範囲区分編集処理
      *****************************************************************
       41002-HKNTEKKBN-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNTEKKBN-MEI
           PERFORM VARYING     IDX-TEK     FROM    1   BY  1
                   UNTIL       IDX-TEK     >   5
               IF      WRK-HKNTEKKBN       =   TBL-HKNTEKKBN-G(IDX-TEK)
                                                              (1:1)
                   MOVE    TBL-HKNTEKKBN-G (IDX-TEK)
                                           TO  WRK-HKNTEKKBN-MEI
               END-IF
           END-PERFORM
           .
       41002-HKNTEKKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    境界層該当区分編集処理
      *****************************************************************
       41003-KYOUKAI-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KYOUKAI-MEI
           EVALUATE    WRK-KYOUKAI
               WHEN    "0"
                   MOVE    "境界層非該当"  TO  WRK-KYOUKAI-MEI
               WHEN    "1"
                   MOVE    "境界層該当"    TO  WRK-KYOUKAI-MEI
           END-EVALUATE
           .
       41003-KYOUKAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費画面編集処理
      *****************************************************************
       30005-P034KOHGMN-HEN-SEC              SECTION.
      *
      *
           PERFORM VARYING     IDX-K        FROM     1   BY  1
                   UNTIL     ( IDX-K     >   7   )
                       OR    ( IDX-K     >   SPA-P034-KOHMSG-MAX)
      *選択済みを解除
               MOVE    SPACE             TO  SPA-P034-BKCHK(IDX-K)
               IF      SPA-P034-DATAKBN (IDX-K)    =   "1"
                   PERFORM 3009-ONSHIKOH-HEN-SEC
      **
      *             IF      SPA-GMN-PTNUM   NOT =   SPACE
                       MOVE    FLG-KOHARI      TO  SPA-P034-KOHCHK
                                                             (IDX-K)
      *2024/01         医療扶助で受給者番号不一致あり
                       IF     (FLG-KOHARI      NOT =   ZERO )
                         AND  (FLG-FUJYO           =   1    )
                           MOVE    "2"             TO  SPA-P034-JYUCHK
                                                             (IDX-K)
                       END-IF
      *
                       IF      FLG-KOHARI      =   ZERO
                         AND  (SPA-P034-TSYORIKBN (IDX-K)
                                               =   SPACE )
                           MOVE    "A"         TO  SPA-P034-TSYORIKBN
                                                             (IDX-K)
                       END-IF
      *             END-IF
                   MOVE    SPACE           TO  WRK-CHKMSG
                   EVALUATE    SPA-P034-TSYORIKBN(IDX-K)
                   WHEN    "1"
                       MOVE    "【済】"    TO  WRK-CHKMSG
                   WHEN    "9"
                       MOVE    "【外】"    TO  WRK-CHKMSG
                   WHEN    "A"
                       MOVE    "（追）"    TO  WRK-CHKMSG
                   WHEN    "U"
                       MOVE    "（更）"    TO  WRK-CHKMSG
                   END-EVALUATE
      *
                   MOVE    SPACE               TO  WRK-KOHNUM-MEI
                   IF      SPA-P034-STKOHNUM(IDX-K)    =   SPACE
                       MOVE    "＊＊＊＊＊＊"      TO  WRK-KOHNUM-MEI
                   ELSE
                       STRING  SPA-P034-STKOHNUM(IDX-K)
                               ":"
                                WRK-KOHMEI          DELIMITED  BY  SIZE
                                               INTO  WRK-KOHNUM-MEI
                       END-STRING
                   END-IF
                   STRING   WRK-CHKMSG   DELIMITED  BY  SIZE
                       WRK-KOHNUM-MEI  DELIMITED  BY  SIZE
                       "　負担者番号："
                       WRK-FTNJANUM
                                           DELIMITED  BY  SIZE
                       "　受給者番号："
                       WRK-JKYSNUM
                                           DELIMITED  BY  SIZE
                       "　"
                       WRK-TEKSTYMD
                       "〜"
                       WRK-TEKEDYMD        DELIMITED  BY  SIZE
                                       INTO   SPA-P034-KOHMSG
                                                    (IDX-K)
                   END-STRING
               END-IF
           END-PERFORM
           .
       30005-P034KOHGMN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       3009-ONSHIKOH-HEN-SEC              SECTION.
      *
      *    公費名称
           MOVE    SPACE               TO  WRK-KOHMEI
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-KOHMAX)
                     OR   (WRK-KOHMEI  NOT =   SPACE )
                IF       SPA-P034-STKOHNUM (IDX-K)
                                           =   SPA-GMN-KOHNUML (IDY)
      *
                   MOVE    SPA-GMN-KOHNUMLST (IDY)
                                           TO  SPA-P034-OSKOHNUM-G
                                                            (IDX-K)
      *
                   MOVE    SPA-GMN-KOHNUMMEIL (IDY)
                                           TO  WRK-KOHMEI
      *            半角空白を全角へ
                   INSPECT WRK-KOHMEI
                           REPLACING   ALL "  "  BY  "　"
               END-IF
           END-PERFORM
      *
      *    日付編集
           MOVE    SPA-P034-STTEKSTYMD (IDX-K) TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-TEKSTYMD
      *
           MOVE    SPA-P034-STTEKEDYMD (IDX-K) TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-TEKEDYMD
      *    受給者番号
           IF      SPA-P034-STJKYSNUM(IDX-K)   =   SPACE
      *        限度額認定証からは１文字設定
               MOVE    SPACE              TO  WRK-JKYSNUM
           ELSE
               MOVE    SPA-P034-STJKYSNUM(IDX-K) 
                                           TO  WRK-JKYSNUM
           END-IF
      *    負担者番号
           MOVE    SPA-P034-STFTNJANUM(IDX-K)
                                           TO  WRK-FTNJANUM
      *    登録済みの有無
           MOVE    ZERO                TO  FLG-KOHARI
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-PTKOH-MAX )
      *2023/04
      *        公費「946」の受給者番号は半角を全角で判断する
               MOVE    SPA-PTKOH-JKYSNUM (IDY) TO  WRK-CHK-JKYUSNUM
               IF      (SPA-PTKOH-KOHNUM(IDY)  =   "946" )
                   IF      WRK-CHK-JKYUSNUM        =   "1"
                       MOVE    "１"            TO  WRK-CHK-JKYUSNUM
                   END-IF
                   IF      WRK-CHK-JKYUSNUM        =   "2"
                       MOVE    "２"            TO  WRK-CHK-JKYUSNUM
                   END-IF
               END-IF
      *
               IF     (SPA-PTKOH-SAKJOKBN (IDY)    =   SPACE)
                AND  ((SPA-PTKOH-KOHNUM   (IDY)    = 
                                          SPA-P034-STKOHNUM(IDX-K))
                 OR   (SPA-P034-STKOHNUM(IDX-K)  =  SPACE ))
      ******    AND   (SPA-PTKOH-JKYSNUM  (IDY)    =
                AND   (WRK-CHK-JKYUSNUM            =
                                          SPA-P034-STJKYSNUM(IDX-K))
                AND   (SPA-PTKOH-FTNJANUM (IDY)    = 
                                          SPA-P034-STFTNJANUM(IDX-K))
      *            終了日が有効期間内
                   IF    (SPA-PTKOH-TEKSTYMD (IDY)
                                               <=  SPA-P034-STTEKEDYMD
                                                          (IDX-K))
                      AND (SPA-PTKOH-TEKEDYMD (IDY) 
                                               >=  SPA-P034-STTEKEDYMD
                                                           (IDX-K))
      *                期間が一致
                       IF     (SPA-PTKOH-TEKSTYMD(IDY)
                                               >=  SPA-P034-STTEKSTYMD
                                                           (IDX-K))
                         AND  (SPA-PTKOH-TEKEDYMD(IDY)
                                               =   SPA-P034-STTEKEDYMD
                                                           (IDX-K))
                           MOVE    1           TO  FLG-KOHARI
                       ELSE
      *2025/05
      *                    開始年月、終了日が一致
                           IF     (SPA-PTKOH-TEKSTYMD(IDY)(1:6)
                                               =  SPA-P034-STTEKSTYMD
                                                       (IDX-K)(1:6))
                             AND  (SPA-PTKOH-TEKEDYMD(IDY)
                                               =   SPA-P034-STTEKEDYMD
                                                           (IDX-K))
                             AND  (SPA-PTKOH-TEKSTYMD(IDY)(7:2)
                                               =   "01"   )
                             AND  (SPA-PTKOH-KOHNUM (IDY)
                                               =   "946"
                                               OR  "966"
                                               OR  "967")
                               MOVE    1           TO  FLG-KOHARI
                           ELSE
      *
                           MOVE    2           TO  FLG-KOHARI
                           END-IF
                       END-IF
                       MOVE    SPA-PTKOH-MAX   TO  IDY
                   ELSE
                       MOVE    3           TO  FLG-KOHARI
                   END-IF
               END-IF 
           END-PERFORM
      *
      *2024/02
      *    医療扶助（生活保護）
           MOVE    ZERO                TO  FLG-FUJYO
           IF     (FLG-KOHARI          =   ZERO )
             AND  (SPA-P034-STKOHNUM (IDX-K)    =   "012")
      *        受給者番号不一致分
               PERFORM 30091-ONSHIKOH-012HEN-SEC
           END-IF
           .
       3009-ONSHIKOH-HEN-EXT.
           EXIT.
      **
      *****************************************************************
      *    医療扶助（生活保護）登録済みチェック
      *****************************************************************
       30091-ONSHIKOH-012HEN-SEC       SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-PTKOH-MAX )
               IF     (SPA-PTKOH-SAKJOKBN (IDY)    =   SPACE)
                AND   (SPA-PTKOH-KOHNUM   (IDY)    = 
                                          SPA-P034-STKOHNUM(IDX-K))
                AND   (SPA-PTKOH-FTNJANUM (IDY)    = 
                                          SPA-P034-STFTNJANUM(IDX-K))
                AND   (SPA-PTKOH-JKYSNUM  (IDY)    =   SPACE  )
      *
      *        負担者番号のみ一致
      *            終了日が有効期間内
                   IF    (SPA-PTKOH-TEKSTYMD (IDY)
                                               <=  SPA-P034-STTEKEDYMD
                                                          (IDX-K))
                      AND (SPA-PTKOH-TEKEDYMD (IDY) 
                                               >=  SPA-P034-STTEKEDYMD
                                                           (IDX-K))
      *                期間が一致
                       IF     (SPA-PTKOH-TEKSTYMD(IDY)
                                               >=  SPA-P034-STTEKSTYMD
                                                           (IDX-K))
                         AND  (SPA-PTKOH-TEKEDYMD(IDY)
                                               =   SPA-P034-STTEKEDYMD
                                                           (IDX-K))
                           MOVE    1           TO  FLG-KOHARI
                           MOVE    1           TO  FLG-FUJYO
                       ELSE
                           MOVE    2           TO  FLG-KOHARI
                           MOVE    1           TO  FLG-FUJYO
                       END-IF
                       MOVE    SPA-PTKOH-MAX   TO  IDY
                   ELSE
      *
                       IF     (SPA-PTKOH-TEKEDYMD (IDY) 
                                               >=  SPA-P034-STTEKSTYMD
                                                           (IDX-K))
                           MOVE    3           TO  FLG-KOHARI
                       END-IF
                   END-IF
               END-IF 
           END-PERFORM
           .
       30091-ONSHIKOH-012HEN-EXT.
           EXIT.
      **
      *****************************************************************
      *    低所得者情報２編集処理
      *****************************************************************
       30002-TSTK2-HEN-SEC              SECTION.
      *
           INITIALIZE                  SPA-P034-TEISYO-G
           MOVE    ZERO                TO  IDX
           MOVE    "低所得者２"        TO  SPA-P034-MIDASHI2
           MOVE    "2"                 TO  SPA-P034-TEISYO-KBN
           PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-TSY-MAX)
                           OR (IDX         >=  10    )
               IF     (SPA-GMN24-TSYDELKBN (IDY)   =   SPACE)
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN24-TSYDPSTYMDW(IDY)
                                       TO  SPA-P034-TSYLSTYMD (IDX)
                   MOVE    SPA-GMN24-TSYDPEDYMDW(IDY)
                                       TO  SPA-P034-TSYLEDYMD (IDX)
                   MOVE    SPA-GMN24-TSYLHKNKBN (IDY)
                                       TO  SPA-P034-TSYLHKNKBN(IDX)
                   MOVE    SPA-GMN24-TSYLNINYMD (IDY)
                                       TO  SPA-P034-TSYLNINYMD (IDX)
                   MOVE    SPA-GMN24-TSYLSKJGNSTYMDW (IDY)
                                       TO  SPA-P034-TSYLSKJGNSTYMD(IDX)
                   MOVE    SPA-GMN24-TSYLKYOUKAI (IDY)
                                       TO  SPA-P034-TSYLKYOUKAI (IDX)
                   MOVE    IDX         TO  SPA-P034-TEISYO-MAX 
               END-IF
           END-PERFORM
           .
       30002-TSTK2-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得者情報１編集処理
      *****************************************************************
       30001-TSTK1-HEN-SEC              SECTION.
      *
           INITIALIZE                  SPA-P034-TEISYO-G
           MOVE    ZERO                TO  IDX
           MOVE    "低所得者１"        TO  SPA-P034-MIDASHI2
           MOVE    "1"                 TO  SPA-P034-TEISYO-KBN
           PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL  (IDY         >   SPA-TNK-MAX)
                          OR  (IDX         >=  10   )
               IF     (SPA-GMN24-TNKDELKBN (IDY)   =   SPACE)
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN24-TNKDPSTYMDW (IDY)
                                       TO  SPA-P034-TSYLSTYMD (IDX)
                   MOVE    SPA-GMN24-TNKDPEDYMDW (IDY)
                                       TO  SPA-P034-TSYLEDYMD (IDX)
                   MOVE    SPA-GMN24-TNKLHKNKBN (IDY)
                                       TO  SPA-P034-TSYLHKNKBN(IDX)
                   MOVE    SPA-GMN24-TNKLNINYMD (IDY)
                                       TO  SPA-P034-TSYLNINYMD (IDX)
                   MOVE    SPA-GMN24-TFUKUSHI-MEI (IDY)
                                       TO  SPA-P034-TSYLSKJGNSTYMD(IDX)
                   MOVE    SPA-GMN24-TNKLKYOUKAI (IDY)
                                       TO  SPA-P034-TSYLKYOUKAI (IDX)
                   MOVE    IDX         TO  SPA-P034-TEISYO-MAX 
               END-IF
           END-PERFORM
           .
       30002-TSTK2-HEN-EXT.
           EXIT.
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P034
           INITIALIZE                      P034
      *
           MOVE    SPA-P034-MIDASHI    TO  P034-ON-MIDASHI
      *TEST
      *     MOVE    SPA-P034-GENDO-KEIMSG   TO  P034-ON-MIDASHI1
      *TEST
      *     MOVE    SPA-P034-GENDO-MSG  TO  P034-GENDO-MSG
      *     MOVE    SPA-P034-SIKKAN-MSG(1)
      *                                 TO  P034-SIKKAN-MSG1
      *     MOVE    SPA-P034-SIKKAN-MSG(2)
      *                                 TO  P034-SIKKAN-MSG2
      *     MOVE    SPA-P034-SIKKAN-MSG(3)
      ****                                 TO  P034-SIKKAN-MSG3
           MOVE    SPA-P034-ONSGMN-MSG (1)
                                       TO  P034-GENDO-MSG
           MOVE    SPA-P034-ONSGMN-MSG (2)
                                       TO  P034-SIKKAN-MSG1
           MOVE    SPA-P034-ONSGMN-MSG (3)
                                       TO  P034-SIKKAN-MSG2
           MOVE    SPA-P034-ONSGMN-MSG (4)
                                       TO  P034-SIKKAN-MSG3
      *2024/01
           MOVE    SPA-P034-LABEL01    TO  P034-LABEL01
           MOVE    SPA-P034-LABEL02    TO  P034-LABEL02
      *2024/03
           IF      SPA-P034-FUJARI     =   1
               MOVE    "[交付番号があります]"
                                       TO  P034-ON-MIDASHI2
               MOVE    "blue"          TO  P034-ON-MIDASHI2-STYLE
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   7
      *
               IF      SPA-P034-BKCHK (IDX)  =   "1"
                   MOVE    "T"        TO   P034-CHK-KOHMSG-ISACTIVE
                                                             (IDX)
                ELSE
                   MOVE    "F"        TO   P034-CHK-KOHMSG-ISACTIVE
                                                             (IDX)
               END-IF
      *        公費の連番
               IF      SPA-P034-KOHIDX (IDX)   >   ZERO
                   MOVE    SPA-P034-KOHIDX(IDX)
                                             TO  P034-CHK-KOHMSG (IDX)
               END-IF
      *
               MOVE    SPA-P034-KOHMSG (IDX) TO  P034-ON-KOHMSG(IDX)
               EVALUATE    SPA-P034-KOHCHK (IDX)
                   WHEN    "0"
      *                未登録
                       MOVE    "red"          TO  P034-ON-KOHMSG-STYLE
                                                               (IDX)
                   WHEN    "1"
      *                登録済み
                       MOVE    "black"        TO  P034-ON-KOHMSG-STYLE
                                                               (IDX)
                   WHEN    "2"
      *                期間不一致
                       MOVE    "darkgreen"    TO  P034-ON-KOHMSG-STYLE
                                                               (IDX)
                   WHEN    "3"
      *                期間外
                       MOVE    "blue"         TO  P034-ON-KOHMSG-STYLE
                                                               (IDX)
                    WHEN    OTHER
                       MOVE    "black"        TO  P034-ON-KOHMSG-STYLE
                                                               (IDX)
               END-EVALUATE
               IF      SPA-P034-TSYORIKBN (IDX)   =   "9"
                   MOVE    "black"        TO  P034-ON-KOHMSG-STYLE
                                                               (IDX)
               END-IF
           END-PERFORM
      *    入力
           MOVE    SPA-P034-IN-FTNJANUM    TO  P034-FTNJANUM
           MOVE    SPA-P034-IN-JKYSNUM     TO  P034-JKYSNUM
           MOVE    SPA-P034-IN-TEKSTYMD    TO  P034-TEKSTYMD
           MOVE    SPA-P034-IN-TEKEDYMD    TO  P034-TEKEDYMD
           MOVE    SPA-P034-IN-KOHNUM-G    TO  P034-KOHNUM
           MOVE    SPA-P034-GMN-SELNUM     TO  P034-SELNUM
      *2023/12
           IF      SPA-GMN-KOHMAX      >   200
              MOVE    200              TO  WRK-KOHMAX
           ELSE
              MOVE    SPA-GMN-KOHMAX   TO  WRK-KOHMAX
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY  1
      *2023/12     UNTIL       IDY     >   SPA-GMN-KOHMAX
                   UNTIL       IDY     >   WRK-KOHMAX
               MOVE    SPA-GMN-KOHNUMLST (IDY) TO  P034-KOHNUM-ITEM
                                                                (IDY)
           END-PERFORM
      **** MOVE    SPA-GMN-KOHMAX      TO  P034-KOHNUM-COUNT
           MOVE    WRK-KOHMAX          TO  P034-KOHNUM-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   40 )
                         OR   (IDX     >   SPA-P034-KOHMAX )
                         OR   ( SPA-P034-TKOHNUM-G(IDX)  =  SPACE)
      *         MOVE    IDX                 TO  WRK-Z9
      *         MOVE    WRK-Z9              TO  P034-TRENNUM (IDX)
               MOVE    SPA-P034-TKOHNUM-G (IDX)
                                           TO  P034-TKOHNUM (IDX)
               MOVE    SPA-P034-TFTNJANUM (IDX)
                                           TO  P034-TFTNJANUM (IDX)
               MOVE    SPA-P034-TJKYUSNUM (IDX)
                                           TO  P034-TJKYSNUM (IDX)
               MOVE    SPA-P034-TTEKSTYMD (IDX)
                                           TO  P034-TKTEKSTYMD (IDX)
               MOVE    SPA-P034-TTEKEDYMD (IDX)
                                           TO  P034-TKTEKEDYMD (IDX)
      *
               IF     (SPA-P034-KOHSEL     =   IDX )
                   MOVE     "#FFCC99"      TO  P034-KOHLIST-BGCOLOR
                                                               (IDX)
                   MOVE    IDX             TO  P034-KOHLIST-ROW
               END-IF
      *        複数該当
               IF     (SPA-P034-KOH1       =   IDX )
                 OR   (SPA-P034-KOH2       =   IDX )
                   MOVE     "#FFCC99"      TO  P034-KOHLIST-BGCOLOR
                                                               (IDX)
                   IF     (SPA-P034-KOH1   =   IDX )
                       MOVE    IDX             TO  P034-KOHLIST-ROW
                   END-IF
               END-IF
               MOVE    IDX                 TO  P034-KOHLIST-COUNT
           END-PERFORM
      *2024/02
           MOVE    SPA-P034-MONTHMSG   TO  P034-MONTHMSG
           MOVE    SPA-P034-LABEL03    TO  P034-LABEL03
           MOVE    SPA-P034-MONTHMSG2  TO  P034-MONTHMSG1
           MOVE    SPA-P034-MONTHMSG3  TO  P034-MONTHMSG2
           MOVE    SPA-P034-MONTHMSG4  TO  P034-MONTHMSG3
           MOVE    SPA-P034-MONTHMSG5  TO  P034-MONTHMSG4
      *
           MOVE    SPA-P034-MIDASHI2   TO  P034-MIDASHI2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >    SPA-P034-TEISYO-MAX )
               MOVE     SPA-P034-TSYLSTYMD(IDX)
                                      TO  P034-TSYLSTYMD (IDX)
               MOVE     SPA-P034-TSYLEDYMD (IDX)
                                      TO  P034-TSYLEDYMD (IDX)   
               MOVE     SPA-P034-TSYLHKNKBN(IDX)
                                      TO  P034-TSYLHKNKBN (IDX)   
               MOVE     SPA-P034-TSYLNINYMD(IDX)
                                      TO  P034-TSYLNINYMD (IDX)
               MOVE     SPA-P034-TSYLSKJGNSTYMD(IDX)
                                      TO  P034-TSYLSKJGNSTYMD (IDX)
               MOVE     SPA-P034-TSYLKYOUKAI(IDX)
                                      TO  P034-TSYLKYOUKAI (IDX)
           END-PERFORM
           MOVE    SPA-P034-TEISYO-MAX    TO  P034-TEISYOLIST-COUNT
      *
           IF      SPA-P034-TEISYO-KBN    =   "2"
               MOVE    "長期入院該当"         TO  P034-LABL1
           ELSE
               MOVE    "年金受給者証"         TO  P034-LABL1
           END-IF
      * 
           MOVE    "F6 資格切替"       TO  P034-B06-LABEL
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
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-P034-CUR        =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-P034-CUR
              WHEN    01
                  MOVE    "B01"         TO  MCP-WIDGET
              WHEN    21
                  MOVE    "SELNUM"      TO  MCP-WIDGET
              WHEN    22
                  MOVE    "FTNJANUM"      TO  MCP-WIDGET
              WHEN    23
                  MOVE    "KOHNUM"        TO  MCP-WIDGET
              WHEN    24
                  MOVE    "JKYSNUM"       TO  MCP-WIDGET
              WHEN    25
                  MOVE    "TEKSTYMD"      TO  MCP-WIDGET
              WHEN    26
                  MOVE    "TEKEDYMD"      TO  MCP-WIDGET
              WHEN    27
                  MOVE    "B08"           TO  MCP-WIDGET
              WHEN    10
                  MOVE    "B10"         TO  MCP-WIDGET
              WHEN    11
                  MOVE    "B11"         TO  MCP-WIDGET
              WHEN    12
                  MOVE    "B12"         TO  MCP-WIDGET
              WHEN    05
                  MOVE    "B05"         TO  MCP-WIDGET
              WHEN    09
                  MOVE    "B09"         TO  MCP-WIDGET
            END-EVALUATE
      *
            .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    SPA-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    22          TO  SPA-P034-CUR
               WHEN    "FTNJANUM"
                   MOVE    23          TO  SPA-P034-CUR
               WHEN    "KOHNUM"
                   MOVE    24          TO  SPA-P034-CUR
               WHEN    "JKYSNUM"
                   MOVE    25          TO  SPA-P034-CUR
               WHEN    "TEKSTYMD"
                   MOVE    27          TO  SPA-P034-CUR
                   IF      SPA-P034-NAI-CHIKOU     =   "1"
                       MOVE    26          TO  SPA-P034-CUR
                   END-IF
               WHEN    "TEKEDYMD"
                   MOVE    27          TO  SPA-P034-CUR
           END-EVALUATE
           .
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    ZERO                TO  SPA-P034-SYORIKBN
                   PERFORM 210-BACK
      *        低所得切り替え
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-TEISYO-CNG-SEC
      *        公費確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4120-KOH-KAKUNIN-SEC
      *
      *        公費更新
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 4110-KOUHI-UPD-SEC
      *        公費追加
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 4120-KOUHI-ADD-SEC
      *        公費更新開始
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 4005-B05-SYORI-SEC
      **        公費更新
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 4100-KOUHI-HENUPD-SEC
      *        クリア処理
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 4002-CLEAR-SYORI-SEC
      *        公費対象外
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 4003-SEL-DELETE-SEC
      *2024/02
      *        オン資格情報切替
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 4005-B06-SYORI-SEC
      *
      *        公費選択
               WHEN    "CLICKED"       ALSO    "CHK_KOHMSG1"
                   MOVE    1               TO  IDX-CK
                   PERFORM 4001-KOHCHK-SEC
               WHEN    "CLICKED"       ALSO    "CHK_KOHMSG2"
                   MOVE    2               TO  IDX-CK
                   PERFORM 4001-KOHCHK-SEC
               WHEN    "CLICKED"       ALSO    "CHK_KOHMSG3"
                   MOVE    3               TO  IDX-CK
                   PERFORM 4001-KOHCHK-SEC
               WHEN    "CLICKED"       ALSO    "CHK_KOHMSG4"
                   MOVE    4               TO  IDX-CK
                   PERFORM 4001-KOHCHK-SEC
               WHEN    "CLICKED"       ALSO    "CHK_KOHMSG5"
                   MOVE    5               TO  IDX-CK
                   PERFORM 4001-KOHCHK-SEC
               WHEN    "CLICKED"       ALSO    "CHK_KOHMSG6"
                   MOVE    6               TO  IDX-CK
                   PERFORM 4001-KOHCHK-SEC
               WHEN    "CLICKED"       ALSO    "CHK_KOHMSG7"
                   MOVE    7               TO  IDX-CK
                   PERFORM 4001-KOHCHK-SEC
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
           MOVE    ZERO                TO  SPA-P034-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *2024/02
      *        リスト選択
               WHEN    ANY             ALSO    "KOHLIST"
                   PERFORM 4001-LIST-SEL-SEC
      *
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4102-SELNUM-CHK-SEC
      *
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      **
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4001-LIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELECT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P034-KOHMAX
               IF      P034-KOHLIST-SELECT (IDX)   =   "T"
                   IF      IDX             =   SPA-P034-KOHSEL
                                           OR  SPA-P034-KOH2
                       CONTINUE
                   ELSE
                       MOVE    IDX             TO  WRK-SELECT
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-P034-GMNSEL     =   ZERO  )
             OR   (WRK-SELECT          =   ZERO  )
               GO  TO  4001-LIST-SEL-EXT
           END-IF
           MOVE    SPA-P034-GMNSEL     TO  IDX2
      *
           IF     (SPA-P034-STKOHNUM (IDX2)    =   "012" )
      *     AND    (SPA-P034-JYUCHK (IDX2)      =   "2"   )
      *        生活保護で、受給者番号が一致する公費がない場合のみ
               CONTINUE
           ELSE
      *        負担者番号不一致
               MOVE    "0025"              TO  SPA-ERRCD
               GO  TO  4001-LIST-SEL-EXT
           END-IF
      *
      *    医療扶助選択中
           IF     (SPA-P034-TFTNJANUM (WRK-SELECT)
                                       =   SPA-P034-STFTNJANUM(IDX2))
             AND  (SPA-P034-TJKYUSNUM (WRK-SELECT)
                                       =   SPACE
                                       OR  SPA-P034-STJKYSNUM (IDX2))
               CONTINUE
           ELSE
      *        負担者番号・受給者番号不一致
               MOVE    "0026"              TO  SPA-ERRCD
               GO  TO  4001-LIST-SEL-EXT
           END-IF
      *
      *     MOVE    SPA-P034-KOH1        TO  SPA-P034-KOH1-B
      *     MOVE    SPA-P034-KOH2        TO  SPA-P034-KOH2-B
      *    MOVE    SPA-P034-KOHSEL      TO  SPA-P034-KOHSEL2
           MOVE    WRK-SELECT           TO  SPA-P034-KOHSEL
           MOVE    ZERO                 TO  SPA-P034-KOH2
           MOVE    ZERO                 TO  SPA-P034-KOH1
      *    月代わり受給者番号検索
           MOVE    SPA-P034-KOHSEL     TO  IDX
           PERFORM 4001121-MONTHLYNUM-HEN-SEC
           IF      SPA-P034-TJKYUSNUM (WRK-SELECT) =   SPACE
      *        受給者番号で登録なし
               MOVE    "2"             TO  SPA-P034-JYUCHK (IDX2)
           END-IF
           MOVE    10                  TO  SPA-P034-CUR
      *
           .
       4001-LIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      **    公費選択有り
           IF      SPA-P034-SELIDX     >   ZERO
      *        画面をＳＰＡセット
               PERFORM 4101-GMN-SPA-SET-SEC
      *
      *        入力チェック処理
               PERFORM 4102-INPUT-CHK-SEC
           END-IF
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-IN-FTNJANUM
           MOVE    SPA-P034-IN-FTNJANUM    TO  WRK-MAE-FTNJANUM
           IF      P034-FTNJANUM   NOT =   SPA-P034-IN-FTNJANUM
               MOVE    1               TO  FLG-IN-FTNJANUM
           END-IF
      *    負担者番号入力
           IF      SPA-MCP-WIDGET      =   "FTNJANUM"
               MOVE    1               TO  FLG-IN-FTNJANUM
           END-IF
      *
      *    地方公費のみ入力可
           IF      SPA-P034-NAI-CHIKOU     =   "1"
               MOVE    P034-FTNJANUM       TO  SPA-P034-IN-FTNJANUM
               MOVE    P034-KOHNUM         TO  SPA-P034-IN-KOHNUM-G
               MOVE    P034-JKYSNUM        TO  SPA-P034-IN-JKYSNUM
      *R05.4   終了日変更
               MOVE    P034-TEKEDYMD       TO  SPA-P034-IN-TEKEDYMD
           END-IF
           MOVE    P034-TEKSTYMD       TO  SPA-P034-IN-TEKSTYMD
      *    終了日変更不可
      **   MOVE    P034-TEKEDYMD       TO  SPA-P034-IN-TEKEDYMD
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
           MOVE    P034-SELNUM         TO  SPA-P034-GMN-SELNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    P034-SELNUM         TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    21                  TO  SPA-P034-CUR
               MOVE    ZERO                TO  SPA-P034-SELNUM
               GO      TO      4102-SELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-P034-SELNUM
               MOVE    SPA-P034-SELNUM     TO  WRK-ZZ
               MOVE    WRK-ZZ              TO  SPA-P034-GMN-SELNUM
           END-IF
      *
           MOVE    ZERO                TO  SPA-P034-SELIDX
           INITIALIZE                  SPA-P034-INPUT-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-P034-KOHMSG-MAX )
                          OR  (SPA-P034-SELIDX     >   ZERO )
               IF      SPA-P034-KOHIDX (IDX)
                                           =   SPA-P034-SELNUM
                   MOVE    IDX             TO  SPA-P034-SELIDX
               END-IF
           END-PERFORM
           IF      SPA-P034-SELIDX     =   ZERO
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-P034-CUR
               GO      TO  4102-SELNUM-CHK-EXT
           END-IF
      *    処理済みでないこと
           MOVE    SPA-P034-SELIDX     TO  IDX
           IF      (SPA-P034-TSYORIKBN(IDX)
                                       =   "1"   )
      *        OR   (SPA-P034-KOHCHK (IDX)
      *                                 =   "1"   )
               MOVE    "0014"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-P034-CUR
               MOVE    ZERO            TO  SPA-P034-SELIDX
               GO      TO  4102-SELNUM-CHK-EXT
           END-IF
      *
           IF      (SPA-P034-TSYORIKBN(IDX)
                                       =   "9"   )
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-P034-CUR
               MOVE    ZERO            TO  SPA-P034-SELIDX
               GO      TO  4102-SELNUM-CHK-EXT
           END-IF
      *
      *    公費編集
           MOVE    SPA-P034-STFTNJANUM(IDX)
                                        TO  SPA-P034-IN-FTNJANUM
           MOVE    SPA-P034-OSKOHNUM-G (IDX)
                                        TO  SPA-P034-IN-KOHNUM-G
           MOVE    SPA-P034-STJKYSNUM (IDX)
                                        TO  SPA-P034-IN-JKYSNUM
           MOVE    SPA-P034-STTEKSTYMD (IDX)
                                        TO  SPA-P034-NAI-TEKSTYMD
           MOVE    SPA-P034-STTEKEDYMD (IDX)
                                        TO  SPA-P034-NAI-TEKEDYMD
           MOVE    SPA-P034-CHIKOU (IDX)
                                       TO  SPA-P034-NAI-CHIKOU
      *
           MOVE    SPA-P034-STTEKSTYMD (IDX)
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P034-IN-TEKSTYMD
           MOVE    SPA-P034-STTEKEDYMD (IDX)
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P034-IN-TEKEDYMD
      *
           .
       4102-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       4102-INPUT-CHK-SEC          SECTION.
      *
      *    負担者番号
           IF      FLG-IN-FTNJANUM     =   1
               PERFORM 4101-FTNJANUM-CHK-SEC
           END-IF
           IF      FLG-END             =   1
               GO  TO  4102-INPUT-CHK-EXT
           END-IF
      *    公費の種類
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-KOHNUM-CHK-SEC
           END-IF
           IF      FLG-END             =   1
               GO  TO  4102-INPUT-CHK-EXT
           END-IF
      *    受給者番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1001-JKYSNUM-CHK-SEC
           END-IF
      *    有効開始日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-TEKSTYMD-CHK-SEC
           END-IF
      *    有効終了日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-TEKEDYMD-CHK-SEC
           END-IF
           .
       4102-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担者番号チェック処理
      *****************************************************************
       4101-FTNJANUM-CHK-SEC                   SECTION.
      *
      *    地方公費のみ入力可
           IF      SPA-P034-NAI-CHIKOU NOT =   "1"
      ******   IF      SPA-P034-IN-FTNJANUM    NOT =   SPACE
      ******       MOVE    "0013"              TO  SPA-ERRCD
      ******       MOVE    SPACE               TO  SPA-P034-IN-FTNJANUM
      ******   END-IF
               GO      TO      4101-FTNJANUM-CHK-EXT
           END-IF
      *
           IF      SPA-P034-IN-FTNJANUM    =   SPACE
               GO      TO      4101-FTNJANUM-CHK-EXT
           END-IF
      *
      *    桁数チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-P034-IN-FTNJANUM
                                       TO  KANACHK-MAE-INPUT
           MOVE    8                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-P034-IN-FTNJANUM
      *
           IF     (KANACHK-RC      NOT =   ZERO  )
             OR   (KANACHK-RC2     NOT =   ZERO  )
      *        間に空白がないこと
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0071"              TO  SPA-ERRCD
                   MOVE    22                  TO  SPA-P034-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 10011-FTNJANUM-DB-CHK-SEC
           END-IF
      *
           .
       4101-FTNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    法別番号チェック区分編集処理
      *****************************************************************
       10011-FTNJANUM-DB-CHK-SEC           SECTION.
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    SPA-P034-IN-FTNJANUM  TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HKNJAINF        =   ZERO
      *        保険公費区分編集
               MOVE    HKNJA-HKNNUM        TO  WRK-HKNNUM
               MOVE    ZERO                TO  WRK-PAYKBN
               MOVE    SPA-P034-NAI-TEKSTYMD   TO  WRK-TEKSTYMD
               MOVE    SPA-P034-NAI-TEKSTYMD   TO  WRK-TEKEDYMD
      *        保険番号マスタ検索
               PERFORM 930-HKNNUM-KEYREAD-SEC
               IF      FLG-HKNNUM          =   ZERO
      *        保険公費種別区分
                   IF      HKN-HKNKOHSBTKBN    =   "1"  OR  "4"
                                               OR  "8"  OR  "9"
      *                保険の種類は保険者マスタなしとする
                       MOVE    1               TO  FLG-HKNJAINF
                   END-IF
               END-IF
           END-IF
      *    保険者番号あり
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    SPACE               TO  SPA-P034-IN-KOHNUM-G
               MOVE    HKNJA-HKNNUM        TO  SPA-P034-IN-KOHNUM
           ELSE
      *        法別番号から公費の種類を決定する
               PERFORM 10011-HKNNUM-SET-SEC
           END-IF
      *
           .
       10011-FTNJANUM-DB-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    法別番号より公費種類処理
      *****************************************************************
       10011-HKNNUM-SET-SEC           SECTION.
      *
      *    負担者番号の法別番号から保険番号マスタの公費番号の判定
           MOVE    ZERO                TO  WRK-HBTCNT
           MOVE    SPACE               TO  WRK-CHK-KOHNUM
           MOVE    SPA-P034-IN-FTNJANUM (1:2)
                                       TO  WRK-HBTNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHMAX
               IF      WRK-HBTNUM      =   SPA-NAI-HBTNUM (IDX)
                   ADD     1               TO  WRK-HBTCNT
                   IF      WRK-HBTCNT      =   1
                       MOVE    SPA-GMN-KOHNUML (IDX)
                                               TO  WRK-CHK-KOHNUM
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-P034-IN-KOHNUM  =   SPACE
               MOVE    WRK-CHK-KOHNUM      TO  SPA-P034-IN-KOHNUM
      *        公費法別一覧編集区分
               IF     (SPA-NAI-KOHHBTKBN-FLG   =   "1"   )
                   IF     (WRK-HBTCNT          >   1   )
      *                同じ制度が複数ある
                       MOVE    SPACE           TO  SPA-P034-IN-KOHNUM
                       PERFORM 41021-KOHHBTCHEK-SEC
                   END-IF
               END-IF
      *        公費種類の変更なし
               IF      WRK-CHK-KOHNUM      =   SPA-P034-IN-KOHNUM
                   MOVE    SPACE           TO  WRK-MAE-KOHNUM
               ELSE
      *            法別番号変更時
                   IF      WRK-MAE-FTNJANUM(1:2)
                                           NOT =   WRK-HBTNUM
                       MOVE    WRK-CHK-KOHNUM  TO  SPA-P034-IN-KOHNUM
                   END-IF
               END-IF
           END-IF
      *
           .
       10011-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費の種類チェック処理
      *****************************************************************
       4101-KOHNUM-CHK-SEC                   SECTION.
      *
      *
           IF      SPA-P034-IN-KOHNUM(1:1)     =   "/"
               IF      SPA-P034-IN-FTNJANUM    NOT =   SPACE
                   PERFORM 41021-KOHHBTCHEK-SEC
                   IF      FLG-END             =   1
                       GO      TO      4101-KOHNUM-CHK-EXT
                   END-IF
                END-IF
           END-IF
      *
      *    公費名称
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY         FROM    1   BY  1
                   UNTIL  (IDY         >   SPA-GMN-KOHMAX)
                     OR   (FLG-OK      NOT =   ZERO )
               IF       SPA-P034-IN-KOHNUM
                                           =   SPA-GMN-KOHNUML (IDY)
                   MOVE    SPA-GMN-KOHNUMLST (IDY)
                                           TO  SPA-P034-IN-KOHNUM-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
              IF      SPA-P034-IN-KOHNUM   =   SPACE
                  MOVE    SPACE            TO  SPA-P034-IN-KOHNUM-G
              ELSE
                  MOVE    "0015"           TO  SPA-ERRCD
                  MOVE    23               TO  SPA-P034-CUR
              END-IF
           END-IF
      * 
           .
       4101-KOHNUM-CHK-EXT.
           EXIT.
      ****************************************************************
      *    新規の時一覧表示処理
      ****************************************************************
       41021-KOHHBTCHEK-SEC      SECTION.
      *
      *    新規の時一覧表示
           INITIALIZE                  SPA-P02H-AREA
           MOVE    SPA-P034-IN-FTNJANUM(1:2)
                                       TO  SPA-P02H-HBTNUM
           PERFORM 41021-P02H-SYORI-SEC
      *  
           IF      SPA-P02H-MAX        <=  1
               IF      SPA-P02H-MAX        =   1
                   MOVE    SPA-P02H-TKOHNUM (1)
                                           TO  SPA-P034-IN-KOHNUM
               END-IF
               INITIALIZE                  SPA-P02H-AREA
               MOVE    ZERO                TO  SPA-FLG-END
               MOVE    ZERO                TO  FLG-END
           END-IF
           .
      *
       41021-KOHHBTCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費法別一覧編集画面処理
      *****************************************************************
       41021-P02H-SYORI-SEC   SECTION.
      *
           MOVE    "P034"              TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
      *
           CALL    "ORCGP02H"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           IF      SPA-P02H-MAX        >   1
               MOVE    "SELNUM"            TO  MCP-WIDGET
               MOVE    "P02H"              TO  SPA-SAKIPG
               MOVE    "P034"              TO  SPA-MOTOPG
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02H"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       41021-P02H-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    受給者番号入力　処理
      *****************************************************************
       1001-JKYSNUM-CHK-SEC           SECTION.
      *
           IF      SPA-P034-IN-JKYSNUM     =   SPACE
               GO      TO      1001-JKYSNUM-CHK-EXT
           END-IF
      *
      *    半角・全角のチェックと桁数判定
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-P034-IN-JKYSNUM
                                       TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-P034-IN-JKYSNUM
           IF      KANACHK-RC      NOT =   ZERO
      *        半角・全角混在エラー
               MOVE    "0023"              TO  SPA-ERRCD
               MOVE    24                  TO  SPA-P034-CUR
               GO      TO          1001-JKYSNUM-CHK-EXT
           END-IF
           IF      KANACHK-RC2         =   8
      *        入力文字エラー
               MOVE    "0024"             TO  SPA-ERRCD
               MOVE    24                 TO  SPA-P034-CUR
           END-IF
      *
           .
       1001-JKYSNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    開始日チェック処理
      *****************************************************************
       4101-TEKSTYMD-CHK-SEC           SECTION.
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P034-IN-TEKSTYMD
                                       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-P034-IN-TEKSTYMD
               MOVE    SGDAY-SDAY          TO  SPA-P034-NAI-TEKSTYMD
           ELSE
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    25                  TO  SPA-P034-CUR
           END-IF
      *    
           IF     (SPA-P034-NAI-TEKEDYMD   =    SPACE
                                           OR  "99999999" )
             OR   (SPA-ERRCD           NOT =   SPACE   )
               CONTINUE
           ELSE
               IF      (SPA-P034-NAI-TEKSTYMD
                                           >=  SPA-P034-NAI-TEKEDYMD)
                   MOVE    "0021"              TO  SPA-ERRCD
                   MOVE    25                  TO  SPA-P034-CUR
               END-IF
           END-IF
           .
       4101-TEKSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了日チェック処理
      *****************************************************************
       4101-TEKEDYMD-CHK-SEC           SECTION.
      *
           IF      SPA-P034-IN-TEKEDYMD    =   SPACE
                                           OR  "99999999"
               MOVE    "99999999"          TO  SPA-P034-IN-TEKEDYMD
               MOVE    ALL "9"             TO  SPA-P034-NAI-TEKEDYMD
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-P034-IN-TEKEDYMD
                                           TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-P034-IN-TEKEDYMD
                   MOVE    SGDAY-SDAY      TO  SPA-P034-NAI-TEKEDYMD
               ELSE
                   MOVE    "0022"              TO  SPA-ERRCD
                   MOVE    26                  TO  SPA-P034-CUR
               END-IF
           END-IF
           .
       4101-TEKEDYMD-CHK-EXT.
           EXIT.
      **
      *****************************************************************
      *    公費変更開始処理
      *****************************************************************
       4005-B05-SYORI-SEC              SECTION.
      *
           IF     (SPA-P034-GMNSEL NOT =   ZERO  )
      *        画面選択あり
               MOVE    SPA-P034-GMNSEL
                                           TO  IDX
               IF      SPA-P034-KOHIDX (IDX)   >   ZERO
                   MOVE    SPA-P034-KOHIDX (IDX)
                                          TO  P034-SELNUM
                   PERFORM 4102-SELNUM-CHK-SEC
                   IF      SPA-ERRCD          =   SPACE
                       MOVE    22             TO  SPA-P034-CUR
                   END-IF
               ELSE
                   MOVE    "0012"         TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    21                 TO  SPA-P034-CUR
           END-IF
           .
       4005-B05-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    オン資格情報　切替処理
      *****************************************************************
       4005-B06-SYORI-SEC           SECTION.
      *
           IF      SPA-P034-ONSMSG-KBN =   "1"
      *        医療扶助画面へ
               IF      SPA-ONSHI002-FUJYO-MAX  >   ZERO
                   MOVE    ZERO                TO  SPA-P034-FUJYOIDX
                   MOVE    "2"                 TO  SPA-P034-ONSMSG-KBN
                   PERFORM 30005-FUJYOMSG-HEN-SEC
               END-IF
           ELSE
               IF     (SPA-P034-FUJYOIDX   <   SPA-ONSHI002-FUJYO-MAX)
                 OR   (SPA-P034-CARD-CLASS  =   "A1"   )
                   PERFORM 30005-FUJYOMSG-HEN-SEC
               ELSE
      *            限度額等
                   MOVE    "1"                 TO  SPA-P034-ONSMSG-KBN
                   PERFORM 30004-GEDPMSG-HEN-SEC
               END-IF
           END-IF
           .
       4005-B06-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    公費変更保存処理
      *****************************************************************
       4100-KOUHI-HENUPD-SEC           SECTION.
      *
           IF      SPA-P034-SELIDX     =   ZERO
               GO  TO  4100-KOUHI-HENUPD-EXT
           END-IF
      *
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4100-KOUHI-HENUPD-EXT
           END-IF
      *
           MOVE    SPA-P034-SELIDX     TO  IDX
           MOVE    SPA-P034-IN-FTNJANUM
                                   TO  SPA-P034-STFTNJANUM(IDX)
           MOVE    SPA-P034-IN-KOHNUM-G
                                   TO  SPA-P034-OSKOHNUM-G (IDX)
           MOVE    SPA-P034-IN-JKYSNUM
                                   TO  SPA-P034-STJKYSNUM (IDX)
           MOVE    SPA-P034-NAI-TEKSTYMD
                                   TO  SPA-P034-STTEKSTYMD(IDX)
           MOVE    SPA-P034-NAI-TEKEDYMD
                                   TO  SPA-P034-STTEKEDYMD(IDX)
      *  
      *    公費画面編集
           PERFORM 30005-P034KOHGMN-HEN-SEC
      *
           INITIALIZE                  SPA-P034-INPUT-G
           INITIALIZE                  SPA-P034-MONTHLYNUM-G
           MOVE    SPACE               TO  SPA-P034-GMN-SELNUM
           MOVE    ZERO                TO  SPA-P034-SELNUM
           MOVE    ZERO                TO  SPA-P034-SELIDX
           MOVE    ZERO                TO  SPA-P034-KOHSEL
           MOVE    ZERO                TO  SPA-P034-KOH1
           MOVE    ZERO                TO  SPA-P034-KOH2
      *
           MOVE    12                  TO  SPA-P034-CUR
           .
       4100-KOUHI-HENUPD-EXT.
           EXIT.
      **
      *****************************************************************
      *    クリア処理
      *****************************************************************
       4002-CLEAR-SYORI-SEC                   SECTION.
      *
           MOVE    ZERO                TO  FLG-CLCHK
           IF      SPA-P034-SELNUM NOT =   ZERO
               INITIALIZE                  SPA-P034-INPUT-G
               INITIALIZE                  SPA-P034-MONTHLYNUM-G
               MOVE    ZERO                TO  SPA-P034-SELNUM
               MOVE    SPACE               TO  SPA-P034-GMN-SELNUM
               MOVE    ZERO                TO  SPA-P034-SELIDX
               MOVE    1                   TO  FLG-CLCHK
      *
               MOVE    21                  TO  SPA-P034-CUR
           END-IF
      *    選択済みを解除
           MOVE    ZERO                TO  SPA-P034-GMNSEL
           MOVE    ZERO                TO  SPA-P034-KOHSEL
           MOVE    ZERO                TO  SPA-P034-KOH1
           MOVE    ZERO                TO  SPA-P034-KOH2
           INITIALIZE                  SPA-P034-MONTHLYNUM-G
           PERFORM VARYING         IDX    FROM    1  BY  1
                   UNTIL           IDX    >   7
               IF      SPA-P034-BKCHK (IDX)   NOT =   SPACE
                   MOVE    1                   TO  FLG-CLCHK
               END-IF
               MOVE    SPACE              TO  SPA-P034-BKCHK (IDX)
           END-PERFORM
      *
      *    初期表示に戻すかの確認(なし)
           IF      FLG-CLCHK           =   ZERO
              MOVE    "0102"              TO  SPA-PIDCD
           END-IF
           .
       4002-CLEAR-SYORI-EXT.
           EXIT.
      **
      *****************************************************************
      *    削除処理
      *****************************************************************
       4003-SEL-DELETE-SEC                   SECTION.
      *
           MOVE    SPA-P034-GMNSEL     TO  IDX
      *
           IF      IDX                 >   ZERO
               IF      SPA-P034-TSYORIKBN(IDX) NOT =   "1"
                   IF      SPA-P034-TSYORIKBN (IDX)    =   "9"
                       MOVE    SPACE       TO  SPA-P034-TSYORIKBN
                                                             (IDX)
                   ELSE
                       MOVE    "9"         TO  SPA-P034-TSYORIKBN
                                                             (IDX)
                   END-IF
      *            公費画面編集
                   PERFORM 30005-P034KOHGMN-HEN-SEC
               END-IF
      *
               INITIALIZE                   SPA-P034-INPUT-G
               MOVE    ZERO                TO  SPA-P034-SELNUM
               MOVE    SPACE               TO  SPA-P034-GMN-SELNUM
               MOVE    ZERO                TO  SPA-P034-SELIDX
               MOVE    ZERO                TO  SPA-P034-KOHSEL
               MOVE    ZERO                TO  SPA-P034-KOH1
               MOVE    ZERO                TO  SPA-P034-KOH2
               INITIALIZE                  SPA-P034-MONTHLYNUM-G
           END-IF
           MOVE    01                  TO  SPA-P034-CUR
           .
       4003-SEL-DELETE-EXT.
           EXIT.
      **
      *****************************************************************
      *    低所得者１，２切り替え処理
      *****************************************************************
       440-TEISYO-CNG-SEC                   SECTION.
      *
           IF      SPA-P034-TEISYO-KBN     =   "1"
               MOVE    "2"                 TO  SPA-P034-TEISYO-KBN
           ELSE
               MOVE    "1"                 TO  SPA-P034-TEISYO-KBN
           END-IF
      *
           IF      SPA-P034-TEISYO-KBN     =   "1"
               PERFORM 30001-TSTK1-HEN-SEC
           ELSE
               PERFORM 30002-TSTK2-HEN-SEC
           END-IF
      *
           .
       440-TEISYO-CNG-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費追加　処理
      *****************************************************************
       4120-KOUHI-ADD-SEC          SECTION.
      *
           IF     (SPA-P034-GMNSEL     =   ZERO  )
               MOVE    "0012"              TO  SPA-ERRCD
               GO      TO     4120-KOUHI-ADD-EXT
           END-IF
      *    画面選択あり
           MOVE    SPA-P034-GMNSEL     TO  IDX2
           IF      (SPA-P034-TSYORIKBN(IDX2)    =   "1" )
              OR   (SPA-P034-KOHCHK   (IDX2)    =   "1" )
      *        登録済み
      *2024/02
               IF     (SPA-P034-KOHCHK (IDX2)   =   "1")
                 AND  (SPA-P034-JYUCHK (IDX2)   =   "2")
                   CONTINUE
               ELSE
      *
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P034-CUR
               GO      TO     4120-KOUHI-ADD-EXT
      *
               END-IF
           END-IF
      *    対象外
           IF      (SPA-P034-TSYORIKBN(IDX2)    =   "9" )
      *        登録済み
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P034-CUR
               GO      TO     4120-KOUHI-ADD-EXT
           END-IF
      *    
           MOVE    SPA-P034-KOHIDX (IDX2)
                                            TO  IDX
      *2024/02
      *    医療扶助（生活保護）受給者番号一致なし
           IF    ( SPA-P034-JYUCHK (IDX2)  =   "2" )
            AND  ( SPA-ERRCD           =   SPACE )
               IF      (SPA-P034-KOHSEL    >   ZERO )
                 AND   (SPA-P034-NTEKEDYMD(SPA-P034-KOHSEL)
                                           >=  SPA-P034-STTEKEDYMD
                                                           (IDX2))
                   MOVE    "0103"          TO  SPA-PIDCD
               ELSE
                   MOVE    "0105"          TO  SPA-PIDCD
               END-IF
               MOVE    10              TO  SPA-P034-CUR
               GO      TO     4120-KOUHI-ADD-EXT
           END-IF
      *
           IF      SPA-P034-KOHCHK (IDX2)
      *            期間不一致は、開始日と終了日で期間変更可能である場合のみ
                                           =   "2"
                                           OR  "3"
               IF      SPA-P034-KOHSEL     =   ZERO
      *                対象の公費が決定できない
                   MOVE    "0006"              TO  SPA-ERRCD
               ELSE
      *
                   IF     (SPA-P034-NTEKSTYMD(SPA-P034-KOHSEL)
                                           <   SPA-P034-STTEKSTYMD
                                                           (IDX2))
                      AND (SPA-P034-NTEKEDYMD(SPA-P034-KOHSEL)
                                           <   SPA-P034-STTEKEDYMD
                                                           (IDX2))
      *           今回追加で登録済の公費が期間終了できること
      *                期間終了するときは、警告
                       IF      SPA-P034-NTEKEDYMD(SPA-P034-KOHSEL)
                                               >=  SPA-P034-STTEKSTYMD
                                                             (IDX2)
                           MOVE    "0101"          TO  SPA-PIDCD
                       END-IF
                   ELSE
                       MOVE    "0007"          TO  SPA-ERRCD
                       MOVE    11              TO  SPA-P034-CUR
                   END-IF
               END-IF
           END-IF
      *2024/02
      *    生活保護で追加不可の時
           IF    ( SPA-P034-JYUCHK (IDX2)  =   "1" )
            AND  ( SPA-ERRCD               =   SPACE )
               MOVE    "0027"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-PIDCD
               MOVE    10              TO  SPA-P034-CUR
           END-IF
      *
           IF      (SPA-ERRCD       NOT =   SPACE)
               OR  (SPA-PIDCD       NOT =   SPACE)
               GO      TO     4120-KOUHI-ADD-EXT
           END-IF
      *
      *    追加
           PERFORM 41201-KOHIADD-SYORI-SEC
            .
       4120-KOUHI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費追加確定処理
      *****************************************************************
       41201-KOHIADD-SYORI-SEC          SECTION.
      * 
           MOVE    SPA-P034-GMNSEL     TO  IDX
           MOVE    "A"                 TO  SPA-P034-TSYORIKBN (IDX)
      *
           PERFORM 30005-P034KOHGMN-HEN-SEC
           MOVE    ZERO               TO  SPA-P034-GMNSEL
           MOVE    ZERO               TO  SPA-P034-KOHSEL
           MOVE    ZERO               TO  SPA-P034-KOH1
           MOVE    ZERO               TO  SPA-P034-KOH2
           INITIALIZE                 SPA-P034-MONTHLYNUM-G
      *
      *
           MOVE    12                  TO  SPA-P034-CUR
           .
       41201-KOHIADD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費更新確定処理
      *****************************************************************
       4120-KOH-KAKUNIN-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-P034-SELIDX
           INITIALIZE                  SPA-P034-INPUT-G
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY         >   SPA-P034-KOHMSG-MAX )
      *
               IF      SPA-P034-KOHIDX (IDY)   NOT =   ZERO
      *            公費の変更内容をSPA-ONSIKAKU　へ
                   MOVE    SPA-P034-KOHIDX(IDY)    TO  IDX
      * 
                   MOVE    SPA-P034-STFTNJANUM(IDY)
                                       TO  SPA-ONSKAKU-FTNJANUM (IDX)
                   MOVE    SPA-P034-STKOHNUM (IDY)
                                       TO  SPA-ONSKAKU-KOHNUM (IDX)
                   MOVE    SPA-P034-STJKYSNUM (IDY)
                                       TO  SPA-ONSKAKU-JKYSNUM (IDX)
                   MOVE    SPA-P034-STTEKSTYMD (IDY)
                                       TO  SPA-ONSKAKU-TEKSTYMD (IDX)
                   MOVE    SPA-P034-STTEKEDYMD (IDY)
                                       TO  SPA-ONSKAKU-TEKEDYMD (IDX)
                   MOVE    SPA-P034-CHIKOU (IDY)
                                       TO  SPA-ONSKAKU-CHIKOU (IDX)
      *
      *
                   IF      SPA-P034-TSYORIKBN (IDY)    =   "9"
      *                削除
                       MOVE    "9"     TO  SPA-ONSKAKU-SYORIFLG(IDX)
                   ELSE
                       IF      SPA-ONSKAKU-SYORIFLG(IDX)   =   "9"
      *                削除解除
                           MOVE    SPACE
                                       TO  SPA-ONSKAKU-SYORIFLG(IDX)
      *
                       END-IF
      *
      *                更新
                       IF      SPA-P034-TSYORIKBN(IDY)  =  "U"
                           MOVE    "2"    TO  SPA-ONSKAKU-ADD(IDX)
      *
                           MOVE    SPA-P034-TSKOHID (IDY)
                                          TO  SPA-ONSKAKU-UPKOHID(IDX)
      *2024/02
      *                    月代わり受給者番号
                           MOVE    SPA-P034-JKYSNUMADD (IDY)
                                          TO  SPA-ONSKAKU-JKYSNUMADD
                                                                 (IDX)
      *
                       END-IF
      *                追加
                       IF      SPA-P034-TSYORIKBN(IDY)  =  "A"
                           MOVE    "1"    TO  SPA-ONSKAKU-ADD(IDX)
                       END-IF
                   END-IF
      *
                   IF      SPA-ONSKAKU-ADD (IDX)  =   "1"
                                                  OR  "2"
                        ADD     1          TO  SPA-P034-KOHSEL
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      (SPA-ERRCD          =   SPACE  )
             AND   (SPA-PIDCD          =   SPACE  )
               MOVE    1               TO  SPA-P034-SYORIKBN
      *        変更対象なし
               IF      SPA-P034-KOHSEL     =   ZERO
                   MOVE    ZERO            TO  SPA-P034-SYORIKBN
               END-IF
               PERFORM 210-BACK
           END-IF
           .
       4120-KOH-KAKUNIN-EXT.
           EXIT.
      * 
      *
      *****************************************************************
      *    公費更新選択　処理
      *****************************************************************
       4001-KOHCHK-SEC          SECTION.
      *
           IF      SPA-P034-BKCHK (IDX-CK)    =   "1"
               MOVE    SPACE           TO  SPA-P034-BKCHK (IDX-CK)
               MOVE    ZERO            TO  SPA-P034-KOHSEL
               MOVE    ZERO            TO  SPA-P034-GMNSEL
               MOVE    ZERO            TO  SPA-P034-KOH1
               MOVE    ZERO            TO  SPA-P034-KOH2
               INITIALIZE                  SPA-P034-MONTHLYNUM-G
           ELSE
               MOVE    "1"             TO  SPA-P034-BKCHK (IDX-CK)
           END-IF
      *
           IF      SPA-P034-BKCHK(IDX-CK)      =   "1"
      *        公費以外は選択不可
               IF      SPA-P034-DATAKBN (IDX-CK)  =  "2"
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-P034-BKCHK (IDX-CK)
               END-IF
      *        公費以外は選択不可
               IF      SPA-P034-DATAKBN (IDX-CK)  =  SPACE
                   MOVE    SPACE           TO  SPA-P034-BKCHK (IDX-CK)
                   GO  TO  4001-KOHCHK-EXT
               END-IF
           END-IF
      *
      *    選択可能な公費であること
           IF      SPA-P034-BKCHK(IDX-CK)      =   "1"
      *
               IF      (SPA-P034-TSYORIKBN (IDX-CK)  =   "1")
      *            処理済みはエラー
                   MOVE    "0001"             TO  SPA-ERRCD
                   MOVE    SPACE              TO  SPA-P034-BKCHK
                                                          (IDX-CK)
               END-IF
           END-IF
      *
            IF     (SPA-P034-BKCHK(IDX-CK)  =   "1" )
              AND  (SPA-ERRCD          =   SPACE   )
      *    選択済みを解除
               MOVE    ZERO            TO  SPA-P034-GMNSEL
               MOVE    ZERO            TO  SPA-P034-KOHSEL
               PERFORM VARYING         IDX    FROM    1  BY  1
                       UNTIL           IDX    >   7
                   IF      IDX         NOT =   IDX-CK
                       MOVE    SPACE           TO  SPA-P034-BKCHK (IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE 
               GO  TO  4001-KOHCHK-EXT
           END-IF
      *    公費更新対象を決定
           IF     (SPA-P034-BKCHK  (IDX-CK)    =   "1" ) 
                MOVE    IDX-CK              TO  SPA-P034-GMNSEL
           END-IF
      *
      *****
      *    公費更新対象を決定
           IF     (SPA-P034-BKCHK  (IDX-CK)    =   "1" )
               IF     (SPA-P034-KOHCHK (IDX-CK)    =    "2"
                                                   OR   "1"
                                                   OR   "3" )
                   IF      SPA-P034-KOHSEL     =   ZERO
                       PERFORM 40011-KOHTBL-CHK-SEC
                   ELSE
                       MOVE    "0002"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    IDX-CK              TO  SPA-P034-GMNSEL
               END-IF
      *
               IF     (SPA-ERRCD           =   SPACE)
                   IF      SPA-P034-KOHCHK (IDX-CK)    =   "0"
                       MOVE    11              TO  SPA-P034-CUR
                   ELSE
                       MOVE    10              TO  SPA-P034-CUR
                   END-IF
      *            公費の種類なしは、F5へ
                   IF      SPA-P034-STKOHNUM (IDX-CK)   =   SPACE
                       MOVE    05              TO  SPA-P034-CUR
                   END-IF
      *            対象外は解除へ
                   IF     SPA-P034-TSYORIKBN (IDX-CK)  =   "9"
                       MOVE    09              TO  SPA-P034-CUR
                   END-IF
               END-IF
           END-IF
           .
       4001-KOHCHK-EXT.
           EXIT.
      *****************************************************************
      *    公費更新　処理
      *****************************************************************
       40011-KOHTBL-CHK-SEC          SECTION.
      *
           MOVE    SPA-P034-GMNSEL    TO  IDX2
      *
           IF     (SPA-P034-DATAKBN (IDX2) NOT =   "1" )
               MOVE    "0003"          TO  SPA-ERRCD
               GO   TO  40011-KOHTBL-CHK-EXT
          END-IF
      *
           MOVE    ZERO                TO  WRK-IDX
           MOVE    ZERO                TO  SPA-P034-KOHSEL
           MOVE    ZERO                TO  SPA-P034-KOH1
           MOVE    ZERO                TO  SPA-P034-KOH2
           MOVE    ZERO                TO  WRK-KOHCNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   40 )
                        OR    (IDX     >   SPA-P034-KOHMAX)
      **                OR   ( SPA-P034-KOHSEL     NOT =   ZERO )
      *2023/04
      *        公費「946」の受給者番号は半角を全角で判断する
               MOVE    SPA-P034-TJKYUSNUM(IDX) TO  WRK-CHK-JKYUSNUM
               IF      (SPA-P034-TKOHNUM(IDX)  =   "946" )
                   IF      WRK-CHK-JKYUSNUM        =   "1"
                       MOVE    "１"            TO  WRK-CHK-JKYUSNUM
                   END-IF
                   IF      WRK-CHK-JKYUSNUM        =   "2"
                       MOVE    "２"            TO  WRK-CHK-JKYUSNUM
                   END-IF
               END-IF
      *
               IF    ( ( SPA-P034-STKOHNUM (IDX2)
                                           =   SPACE    )  OR
                      ((SPA-P034-STKOHNUM (IDX2)
                                       NOT =   SPACE  )  AND
                       (SPA-P034-TKOHNUM (IDX)
                                           =   SPA-P034-STKOHNUM
                                                           (IDX2))) )
                 AND  (SPA-P034-TFTNJANUM(IDX)
                                           =   SPA-P034-STFTNJANUM
                                                            (IDX2))
      ******     AND  (SPA-P034-TJKYUSNUM(IDX)
                 AND  (WRK-CHK-JKYUSNUM 
                                           =   SPA-P034-STJKYSNUM
                                                            (IDX2))
                   IF    ((SPA-P034-NTEKEDYMD(IDX)
                                           >   SPA-P034-STTEKSTYMD
                                                            (IDX2))
                     AND  (SPA-P034-NTEKEDYMD(IDX)
                                       NOT =  SPA-P034-STTEKEDYMD
                                                            (IDX2)))
                   OR    (SPA-P034-NTEKEDYMD(IDX)
                                           =   SPA-P034-STTEKEDYMD
                                                            (IDX2) )
      *                複数対象
                       IF      SPA-P034-KOHSEL     =   ZERO
                           MOVE    IDX         TO  SPA-P034-KOHSEL
                       ELSE
      *R05.4               最初を対象
                           IF      SPA-P034-KOH2   =   ZERO
                               MOVE    IDX         TO  SPA-P034-KOH2
                           END-IF
                       END-IF
                       ADD     1               TO  WRK-KOHCNT
                   ELSE
      *R05.4           最初を対象
                       IF      WRK-IDX         =   ZERO
                           MOVE    IDX         TO  WRK-IDX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *2024/01
           IF     (SPA-P034-KOHSEL     NOT =   ZERO )
             AND  (SPA-P034-CHIKOU (IDX2)  =   "4" )
             AND  (SPA-P034-STKOHNUM(IDX2) =   "012")
               MOVE    SPA-P034-KOHSEL     TO  IDX
      *        月代わり受給者番号検索
               PERFORM 4001121-MONTHLYNUM-HEN-SEC
           END-IF
      *
      *    医療扶助で該当なし
           IF     (SPA-P034-KOHSEL     =   ZERO )
             AND  (SPA-P034-CHIKOU (IDX2)  =   "4" )
             AND  (SPA-P034-STKOHNUM(IDX2) =   "012")
               IF      WRK-IDX             NOT =   ZERO
      *            期間継続かの判定
                   PERFORM 400111-FUJYO-KIKANCHK-SEC
                   IF      FLG-JYUFUKU    =   1
      *                追加不可
                       MOVE    "1"             TO  SPA-P034-JYUCHK
                                                              (IDX2)
                   END-IF
               ELSE
      *            受給者番号不一致の検索
                   PERFORM 400112-FUJYO-KOHCHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-P034-KOHSEL     =   ZERO
              IF      WRK-IDX              =   ZERO
                  MOVE    "0003"           TO  SPA-ERRCD
                  MOVE    1                TO  SPA-P034-CUR
              ELSE
                  MOVE    "0005"           TO  SPA-ERRCD
                  MOVE    WRK-IDX          TO  SPA-P034-KOHSEL
              END-IF
      *
           ELSE
      *2024/02
               IF     (SPA-P034-STKOHNUM(IDX2) =   "012")
                   CONTINUE
               ELSE
      *
               IF      WRK-KOHCNT          >   1
      *            対象の公費が複数
                   MOVE    SPA-P034-KOHSEL TO  SPA-P034-KOH1
                   MOVE    ZERO            TO  SPA-P034-KOHSEL
      *            F5 公費変更へ
                   MOVE    5               TO  SPA-P034-CUR
               END-IF
      *
               END-IF
           END-IF
      *    オン資格公費の表示位置
           IF      SPA-P034-KOHSEL     NOT =   ZERO
               MOVE    IDX-CK              TO  SPA-P034-GMNSEL
           END-IF
           .
       40011-KOHTBL-CHK-EXT.
           EXIT.
      *2024/02
      *****************************************************************
      *    医療扶助（生活保護）期間継続チェック処理
      *****************************************************************
       400111-FUJYO-KIKANCHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-JYUFUKU
           IF      SPA-P034-NTEKEDYMD(WRK-IDX)
                                       <   SPA-P034-STTEKSTYMD
                                                            (IDX2)
      *        有効終了日＋１日
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-P034-NTEKEDYMD(WRK-IDX)
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
               ELSE
                   MOVE    SPA-P034-NTEKEDYMD(WRK-IDX) TO  WRK-SYMD
               END-IF
           ELSE
               MOVE    SPA-P034-NTEKEDYMD(WRK-IDX) TO  WRK-SYMD
           END-IF
      *
           IF      WRK-SYMD            =   SPA-P034-STTEKSTYMD
                                                              (IDX2)
               MOVE    WRK-IDX         TO  SPA-P034-KOHSEL
               MOVE    1               TO  FLG-JYUFUKU
           END-IF
           .
       400111-FUJYO-KIKANCHK-EXT.
           EXIT.
      *2024/02
      *****************************************************************
      *    医療扶助（生活保護）受給者番号不一致分チェック処理
      *****************************************************************
       400112-FUJYO-KOHCHK-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   40 )
                        OR    (IDX     >   SPA-P034-KOHMAX)
               IF     (SPA-P034-TKOHNUM (IDX)
                                           =   SPA-P034-STKOHNUM
                                                           (IDX2) )
                 AND  (SPA-P034-TFTNJANUM(IDX)
                                           =   SPA-P034-STFTNJANUM
                                                            (IDX2))
      *          受給者番号が空白のものを対象
                 AND   (SPA-P034-TJKYUSNUM(IDX) =   SPACE   )
      * 
                   IF    ((SPA-P034-NTEKEDYMD(IDX)
                                           >   SPA-P034-STTEKSTYMD
                                                            (IDX2))
                     AND  (SPA-P034-NTEKEDYMD(IDX)
                                       NOT =  SPA-P034-STTEKEDYMD
                                                            (IDX2)))
                   OR    (SPA-P034-NTEKEDYMD(IDX)
                                           =   SPA-P034-STTEKEDYMD
                                                            (IDX2) )
      *                複数対象
                       IF      SPA-P034-KOHSEL     =   ZERO
                           MOVE    IDX         TO  SPA-P034-KOHSEL
                       ELSE
      *                    最初を対象
                           IF      SPA-P034-KOH2   =   ZERO
                               MOVE    IDX         TO  SPA-P034-KOH2
                           END-IF
                       END-IF
                       ADD     1               TO  WRK-KOHCNT
                   END-IF
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  FLG-JYUFUKU
           IF      SPA-P034-KOHSEL     >   ZERO
      *        受給者番号で登録なし
               MOVE    "2"                 TO  SPA-P034-JYUCHK (IDX2)
               MOVE    SPA-P034-KOHSEL     TO  IDX
      *        月代わり受給者番号検索
               PERFORM 4001121-MONTHLYNUM-HEN-SEC
           END-IF
           .
       400111-FUJYO-KOHCHK-EXT.
           EXIT.
      *****************************************************************
      *    月代わり受給者番号検索処理
      *****************************************************************
       4001121-MONTHLYNUM-HEN-SEC          SECTION.
      *
           INITIALIZE                      SPA-P034-MONTHLYNUM-G
      *    編集
           MOVE    SPACE               TO  MONTHLYNUM-REC
           INITIALIZE                      MONTHLYNUM-REC
           MOVE    SPA-HOSPNUM         TO  MONTHLYNUM-HOSPNUM
           MOVE    SPA-ONSKAKU-PTID    TO  MONTHLYNUM-PTID
           MOVE    SPA-P034-TKOHID(IDX)
                                       TO  MONTHLYNUM-KOHID
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
      *
           MOVE    MONTHLYNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 925-MONTHLYNUM-READ-SEC
           ELSE
               INITIALIZE                      MONTHLYNUM-REC
               MOVE    1               TO  FLG-MONTHLYNUM
           END-IF
           MOVE    SPACE               TO  WRK-MONTHLYNUM-SRYYM
           MOVE    ZERO                TO  IDM
           PERFORM UNTIL   FLG-MONTHLYNUM  =   1
      *    最新履歴を対象とする
             IF    (WRK-MONTHLYNUM-SRYYM =   SPACE )
      *      １件目
      *        受給者番号で登録済
               IF     (MONTHLYNUM-JKYSNUM  =   SPA-P034-STJKYSNUM
                                                            (IDX2))
                 AND  (MONTHLYNUM-SRYYM    =   SPA-SYSYMD(1:6) )
                   CONTINUE
               ELSE
                   MOVE    SPA-P034-STJKYSNUM (IDX2)
                                           TO  SPA-P034-MON-JKYSNUM
               END-IF
               MOVE    SPACE               TO  SPA-P034-MONTHMSG
               MOVE    SPACE               TO  SPA-P034-MONTHMSG2
               MOVE    "月代わり受給者番号"    TO  SPA-P034-LABEL03
      *
               MOVE    MONTHLYNUM-SRYYM    TO  WRK-SYMD(1:6)
               MOVE    "01"                TO  WRK-SYMD(7:2)
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               STRING   WRK-HENYMD(1:6)    DELIMITED BY  SIZE
                       "　"
                       MONTHLYNUM-JKYSNUM DELIMITED BY  SPACE
                                           INTO  SPA-P034-MONTHMSG
               END-STRING
      *        診療年月保存
               MOVE    MONTHLYNUM-SRYYM    TO  WRK-MONTHLYNUM-SRYYM
      *
             END-IF
             IF     (MONTHLYNUM-SRYYM      =    WRK-MONTHLYNUM-SRYYM)
              AND   (SPA-ONSHI-KOFUTNK     =   "1"  )
      *        交付番号
               IF     (MONTHLYNUM-KOFUNUM  NOT =   SPACE)
                   ADD      1                  TO  IDM
                   IF       IDM                =   1
                       MOVE     "交付番号"     TO  SPA-P034-MONTHMSG2
                   END-IF
                   EVALUATE    MONTHLYNUM-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入院："        TO  WRK-NYUGAIKBN-MED
                   WHEN    "2"
                       MOVE    "外来："        TO  WRK-NYUGAIKBN-MED
                   END-EVALUATE
                   IF      IDM             =   1
                       STRING   WRK-NYUGAIKBN-MED
                                MONTHLYNUM-KOFUNUM DELIMITED BY  SPACE
                                           INTO  SPA-P034-MONTHMSG3
                       END-STRING
                   ELSE
                       STRING   WRK-NYUGAIKBN-MED
                                MONTHLYNUM-KOFUNUM DELIMITED BY  SPACE
                                           INTO  SPA-P034-MONTHMSG4
                       END-STRING
                   END-IF
               END-IF
             END-IF
      *
               IF     (MONTHLYNUM-SRYYM   NOT =   WRK-MONTHLYNUM-SRYYM)
                  OR  (IDM                =   2    )
                   MOVE    1               TO  FLG-MONTHLYNUM
               ELSE
      *
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 925-MONTHLYNUM-READ-SEC
               END-IF
           END-PERFORM
      *         
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    登録なし
           IF      WRK-MONTHLYNUM-SRYYM    =   SPACE
               MOVE    SPA-P034-STJKYSNUM (IDX2)
                                           TO  SPA-P034-MON-JKYSNUM
           END-IF
           .
       4001121-MONTHLYNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    月代わり受給者番号読み込み
      *****************************************************************
       925-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-MONTHLYNUM
               MOVE    MCPDATA-REC     TO  MONTHLYNUM-REC
           ELSE
              INITIALIZE                  MONTHLYNUM-REC
              MOVE    1               TO  FLG-MONTHLYNUM
           END-IF
      *
           .
       925-MONTHLYNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費更新　処理
      *****************************************************************
       4110-KOUHI-UPD-SEC          SECTION.
      **
      *    更新の設定チェック
           IF     (SPA-P034-GMNSEL     =   ZERO )
              MOVE    "0006"           TO  SPA-ERRCD
              GO      TO   4110-KOUHI-UPD-EXT
           END-IF
      *
      *    対象外
           IF      (SPA-P034-TSYORIKBN(IDX2)    =   "9" )
      *        登録済み
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P034-CUR
               GO      TO     4110-KOUHI-UPD-EXT
           END-IF
      *    複数の対象
           IF     (SPA-P034-GMNSEL NOT =   ZERO )
              AND (SPA-P034-KOHSEL     =   ZERO )
              AND (SPA-P034-KOH1   NOT =   ZERO )
              AND (SPA-P034-KOH2   NOT =   ZERO )
              MOVE    "0010"           TO  SPA-ERRCD
              GO      TO   4110-KOUHI-UPD-EXT
           END-IF
      *    対象外
           IF      (SPA-P034-TSYORIKBN(IDX2)    =   "9" )
      *        登録済み
               MOVE    "0016"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P034-CUR
               GO      TO     4120-KOUHI-ADD-EXT
           END-IF
      *
      *    更新の設定チェック
           IF     (SPA-P034-KOHSEL     =   ZERO )
              MOVE    "0006"           TO  SPA-ERRCD
              GO      TO   4110-KOUHI-UPD-EXT
           END-IF
      *
      *    選択画面の位置
           MOVE    SPA-P034-GMNSEL         TO  IDX-CK
      *    公費更新対象を決定
           IF     (SPA-P034-TSYORIKBN (IDX-CK) =   SPACE)
             AND  (SPA-P034-DATAKBN (IDX-CK)   =   "1" )
               IF     (SPA-P034-KOHCHK (IDX-CK)    =    "2"
                                                   OR   "1"
                                                   OR   "3" )
                   CONTINUE
               ELSE
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4110-KOUHI-UPD-EXT
           END-IF
      *
      *    終了日が同じは更新なし
           MOVE    SPA-P034-KOHIDX(IDX-CK)     TO  IDX
           IF     (SPA-P034-NTEKEDYMD(SPA-P034-KOHSEL)
                                       =   SPA-P034-STTEKEDYMD
                                                           (IDX-CK))
      *2024/02
      *        医療扶助（生活保護）は受給者番号
               IF      SPA-P034-MON-JKYSNUM  NOT =   SPACE
               OR     (SPA-P034-STKOHNUM (IDX-CK)    =   "012")
                   CONTINUE
               ELSE
      *
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P034-CUR
               GO      TO   4110-KOUHI-UPD-EXT
      *
               END-IF
           END-IF
      *
           IF     (SPA-P034-KOHSEL     NOT =   ZERO)
             AND  (SPA-P034-TKOHID(SPA-P034-KOHSEL)
                                       NOT =   ZERO)
               MOVE    SPA-P034-TKOHID(SPA-P034-KOHSEL)
                                       TO  SPA-P034-TSKOHID
                                                      (IDX-CK)
               MOVE    "U"             TO  SPA-P034-TSYORIKBN
                                                      (IDX-CK)
      *2024/02
      *        医療扶助　月代わり受給者番号追加
               IF      SPA-P034-MON-JKYSNUM  NOT =   SPACE
                AND   (SPA-ONSHI-KOFUTNK     =   "1"  )
                   MOVE    "0104"       TO  SPA-PIDCD
                   GO      TO      4110-KOUHI-UPD-EXT
               END-IF
           ELSE
               MOVE    "0006"           TO  SPA-ERRCD 
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
      *        公費画面編集
                PERFORM 30005-P034KOHGMN-HEN-SEC
                MOVE    ZERO               TO  SPA-P034-GMNSEL
                MOVE    ZERO               TO  SPA-P034-KOHSEL
                MOVE    ZERO               TO  SPA-P034-KOH1
                MOVE    ZERO               TO  SPA-P034-KOH2
                INITIALIZE                 SPA-P034-MONTHLYNUM-G
      *
               MOVE    12                  TO  SPA-P034-CUR
           END-IF
           .
       4110-KOUHI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    医療扶助　月代わり受給者番号追加処理
      *****************************************************************
       41109-KOUHI-MONTHUPD-SEC              SECTION.
      *
           MOVE    SPA-P034-GMNSEL         TO  IDX-CK
      *
           IF      SPA-PID1-FLG        =   "OK"
               MOVE    "1"             TO  SPA-P034-JKYSNUMADD (IDX-CK)
           ELSE
               MOVE    SPACE           TO  SPA-P034-JKYSNUMADD (IDX-CK)
           END-IF
      *    公費画面編集
            PERFORM 30005-P034KOHGMN-HEN-SEC
            MOVE    ZERO               TO  SPA-P034-GMNSEL
            MOVE    ZERO               TO  SPA-P034-KOHSEL
            MOVE    ZERO               TO  SPA-P034-KOH1
            MOVE    ZERO               TO  SPA-P034-KOH2
            INITIALIZE                 SPA-P034-MONTHLYNUM-G
      *
           MOVE    12                  TO  SPA-P034-CUR
           .
       41109-KOUHI-MONTHUPD-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P034"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
                   STRING  "処理済みの公費です。"
                           "選択できません。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0017"
                   STRING  "公費選択番号入力エラー"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0002"
                   STRING  "既に公費を選択済みです。"
                           "１件選択して下さい。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING    "更新対象の公費を決定できません。"
                             "選択できません。"
                                       DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0004"
                   STRING    "低所得者情報は選択できません。"
                                       DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0005"
                   STRING  "更新対象の公費の有効期間外の"
                           "公費です。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0006"
                   MOVE    "更新対象の公費の選択がありません。"
                                       TO  SPA-ERRMSG
                WHEN    "0007"
                   STRING  "期間が重複するので追加できません。"
                           "更新して下さい。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0008"
                   STRING  "登録済みの公費です。追加できません。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0009"
                    STRING  "登録済みの公費です。更新できません。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0010"
                    STRING  "対象の公費が複数あります。"
                      "基本情報タブで対象の公費を直接更新して下さい。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0011"
                    STRING  "追加できる公費はありません。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0012"
                    STRING  "公費の選択がありません。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0013"
                    STRING  "負担割番号の入力はできません。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0014"
                    STRING  "登録済みの公費です。変更できません。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0015"
                    STRING  "公費の種類がありません。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0016"
                    STRING  "対象外の公費です。解除して下さい。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
      *
                WHEN    "0020"
                    STRING  "公費の適用開始日が入力エラーです"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0021"
                    STRING  "公費有効開始日≦有効終了日で"
                            "入力して下さい。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
                WHEN    "0022"
                    STRING  "公費の適用終了日が入力エラーです。"
                                             DELIMITED BY  SPACE
                                       INTO  SPA-ERRMSG
                    END-STRING
               WHEN    "0023"
                   MOVE    "受給者番号は全角・半角の混在はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "受給者番号が入力エラーです"
                                               TO  SPA-ERRMSG
      *2024/02
               WHEN    "0025"
                   STRING  "生活保護ではありません。"
                           "選択できません。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0026"
                   STRING  "負担者番号・受給者番号が一致しません。"
                           "選択できません。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0027"
                   STRING  "負担者番号・受給者番号が一致する公費と"
                           "期間が継続します。更新して下さい。"
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P034"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
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
                   STRING  "公費追加で、同じ内容の公費が期間終了と"
                           "なります。"
                           "公費更新でなくてよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "今回の処理をクリアしますか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
      *2024/02
               WHEN    "0103"
                   STRING  "公費追加で生活保護が重複します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0105"
                   STRING  "生活保護の登録があります。"
                           "公費追加でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "「OK」で登録時に"
                        "受給者番号を"
                        "月代わり受給者番号に追加します。"
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
           EVALUATE    SPA-PIDCD
               WHEN    "0104"
                   MOVE    "ＮＯ"              TO  PID1-B01
           END-EVALUATE
           MOVE    "ＯＫ"              TO  PID1-B12
      *
           MOVE    "B01"               TO  MCP-WIDGET
           EVALUATE    SPA-PIDCD
               WHEN    "0104"
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "P034"              TO  SPA-MOTOPG
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
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-P034-SYORIKBN   =   ZERO
      *        選択クリア
               MOVE    ZERO                TO  SPA-P034-KOHSEL
               MOVE    ZERO                TO  SPA-P034-KOH1
               MOVE    ZERO                TO  SPA-P034-KOH2
               MOVE    ZERO                TO  SPA-P034-GMNSEL
           END-IF
           IF     (SPA-GMN-PTNUM       =   SPACE)
               INITIALIZE                   SPA-PTKOHINF-AREA
           END-IF
      *
           MOVE    "P034"              TO  SPA-MOTOPG
           MOVE    SPA-P034-MOTOPG     TO  SPA-SAKIPG
           IF      SPA-P034-MOTOPG     =   "P033"
               MOVE    "P033"          TO  SPA-MOTOPG
           END-IF 
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
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
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       8001-SEIWA-YMDEDIT-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
                                       OR  ZERO
                                       OR  ALL "9"
               MOVE    WRK-SYMD            TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               IF      STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
               ELSE
                   MOVE    SPACE               TO  WRK-HENYMD
               END-IF
           END-IF
           .
       8001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *****************************************************************
      *    オンライン資格確認UID検索処理
      *****************************************************************
       4103-ONSKAKU-KEY-SEC         SECTION.
      *
      *    オンライン資格確認照会結果検索
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P034-TBL-UUID   TO  ONSKAKU-TBL-UUID
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  ONSKAKU-REC
                   MOVE    ZERO            TO  FLG-ONSKAKU
               ELSE
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    1               TO  FLG-ONSKAKU
               END-IF
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       4103-ONSKAKU-KEY-EXT.
           EXIT.
      *****************************************************************
      *    公費履歴読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険番号マスタ検索処理
      *****************************************************************
       930-HKNNUM-KEYREAD-SEC           SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKSTYMD
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   INITIALIZE              HKNNUM-REC
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       930-HKNNUM-KEYREAD-EXT.
           EXIT. 
      *
      *****************************************************************
      *    低所得マスタ読み込み
      *****************************************************************
       990-TSYRRK-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       990-TSYRRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金マスタ読み込み
      *****************************************************************
       991-TNKRRK-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       991-TNKRRK-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO                TO  FLG-HKNJAINF
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
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
