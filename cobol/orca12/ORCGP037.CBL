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
       PROGRAM-ID.             ORCGP037.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : オンライン資格公費画像確認画面（Ｐ０３７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  22/11/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
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
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
           03  FLG-KOUSIN          PIC 9(01).
           03  FLG-CHKKBN          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-NO              PIC 9(01).
           03  FLG-KIKAN-OK        PIC 9(01).
           03  FLG-HKNHEN          PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-K               PIC 9(04).
      *
           03  IDX-SK              PIC 9(04).
           03  IDX-SK2             PIC 9(04).
           03  IDX-OSK             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNJANUM            PIC X(08).
           03  WRK-HKNJANAME           PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HKNNUMMEI           PIC X(20).
      *
           03  WRK-KIKAN.
               05  WRK-STYMD           PIC X(09).
               05  WRK-K1              PIC X(02).
               05  WRK-EDYMD           PIC X(09).
      *
           03  WRK-PIDMSG              PIC X(100).
           03  WRK-ERRCD               PIC X(04).
      *
           03  WRK-MIDASHI             PIC X(100).
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
           03  WRK-ZZ              PIC ZZ.
      *
           03  WRK-OYA-UUID            PIC X(36).
           03  WRK-TBL-UUID            PIC X(36).
      *    検証番号チェック
           03  WRK-MOJ-AREA.
               05  WRK-MOJ-MAX         PIC 9(02).
               05  WRK-MOJ-NXT         PIC 9(02).
      *
           03  WRK-ERRMSG              PIC X(80).
      *
           03  WRK-HEN-KIKAN.
               05  WRK-HEN-KIKAN1          PIC X(10).
               05  WRK-HEN-K1              PIC X(02).
               05  WRK-HEN-KIKAN2          PIC X(10).
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
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT       PIC X(100).
           03  WRK-ATO-INPUT       PIC X(100).
           03  WRK-MAX             PIC 9(04).
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
      *    患者情報
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *    保険マスタ
      * 01  PTHKNINF-REC.
      *     COPY    "CPPTHKNINF.INC".
      *    患者公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    検索文字追加
      *     COPY    "CPORCSADDSIGN.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    補助区分変換パラメタ（API用）
      *     COPY    "CPORAPI012SUB1.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      * 
      *    排他制御サブパラメタ
      *     COPY    "CPORCSLOCK.INC".
      *
      *    保険証画像編集パラメタ
           COPY    "CPORCSONSHIIMG.INC".
      *
      *    資格確認照会パラメタ
      *     COPY    "CPORCSONSHI001.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPA-ERRCD       TO  WRK-ERRCD
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
               MOVE    "P037"              TO  MCP-WINDOW
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
                   IF     (SPA-ERRCD           =   SPACE )
                   AND    (SPA-PIDCD       NOT =   SPACE )
                       PERFORM 520-PIDSET-SEC
                   END-IF
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
      *** INITIALIZE                      SPA-P037-AREA
      *
           MOVE    SPA-MOTOPG          TO  SPA-P037-MOTOPG
      *    オンライン資格確認編集
           PERFORM 3001-ONSHIKAKU-SYORI-SEC
      *
           MOVE    1                   TO  SPA-P037-CUR
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認編集処理
      *****************************************************************
       3001-ONSHIKAKU-SYORI-SEC              SECTION.
      *
           INITIALIZE                      SPA-P037-HKN-SKAKU-G
           INITIALIZE                      SPA-P037-ONSKAKU-G
           INITIALIZE                      SPA-P037-KOHLIST-G
           MOVE    ZERO                TO  SPA-P037-SELNUM
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P037-HKN-TBL-UUID 
                                       TO  ONSKAKU-TBL-UUID
      *
           PERFORM 9002-ONSKAKU-KEYREAD-SEC
      *
           IF      FLG-ONSKAKU         =   ZERO
      *        オンライン資格確認保険内容
               PERFORM 30011-ONSHIKAKU-HEN-SEC
      *        公費医療券編集
      *        生年月日有効のみ
               MOVE    "1"             TO  SPA-P037-SYORIKBN
               PERFORM 30012-KOUHI-SHIKAKU-HEN-SEC
      *        対象がない時、全部
               IF      SPA-P037-LSTMAX     =   ZERO
                   MOVE    "2"             TO  SPA-P037-SYORIKBN
                   PERFORM 30012-KOUHI-SHIKAKU-HEN-SEC
               END-IF
               IF      SPA-P037-LSTMAX     >   ZERO
                   MOVE    1               TO  SPA-P037-SELNUM
                   PERFORM 30013-KOUHI-SEL-SEC
               END-IF
           END-IF
      *
           .
       3001-ONSHIKAKU-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    オンライン資格確認情報編集処理
      *****************************************************************
       30011-ONSHIKAKU-HEN-SEC              SECTION.
      *
      *    生年月日
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  SPA-P037-HBIRTHDAY
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD              TO  SPA-P037-HBIRTHDAYW
      *    年齢計算
           PERFORM 300111-NENREI-SET-SEC
      *    氏名
           MOVE    ONSKAKU-RES-NAME        TO  SPA-P037-NAME
      *    氏名
           MOVE    ONSKAKU-RES-KANANAME    TO  SPA-P037-KANANAME
      *    性別
           EVALUATE    ONSKAKU-RES-SEX1
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P037-SEX
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P037-SEX
           END-EVALUATE
      *
      *    患者番号
           IF     (ONSKAKU-PTID    NOT =   ZERO )
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    ONSKAKU-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM     TO  SPA-P037-PTNUM
               END-IF
           END-IF
           MOVE    ONSKAKU-PTID        TO  SPA-P037-PTID
      *
            .
       30011-ONSHIKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費医療券画像編集処理
      *****************************************************************
       30012-KOUHI-SHIKAKU-HEN-SEC              SECTION.
      *
           INITIALIZE                      SPA-P037-ONSKAKU-G
           MOVE    ZERO                TO  SPA-P037-SELNUM
           INITIALIZE                      SPA-P037-KOHLIST-G
           MOVE    ZERO                TO  IDX-K
      *
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P037-HKN-TBL-UUID 
                                       TO  ONSKAKU-KOUHI-UUID
      *
      **    kouhi-uuidでのみ検索へ
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
           PERFORM UNTIL       (FLG-ONSKAKU   =   1  )
                         OR    (IDX-K         >=  10 )
               MOVE    ZERO               TO  FLG-CHKKBN
      *        処理結果区分
               IF     (ONSKAKU-RESULT-KBN  =   "1"  )
      *          公費照会のみ対象
                AND   (ONSKAKU-SHOKAI-KBN  =   "8"
                                           OR  "9"  )
                   MOVE    1               TO  FLG-CHKKBN
               ELSE
                   MOVE    ZERO            TO  FLG-CHKKBN
               END-IF
      *
      *        対象のみ　
               IF      (SPA-P037-SYORIKBN     =   "1")
      *            保険の生年月日と同じこと
                   IF      (SPA-P037-HBIRTHDAY
                                       NOT =   ONSKAKU-RES-BIRTHDAY)
                   OR      (ONSKAKU-RES-HONKZKKBN
                                           =   "9"    )
                       MOVE    ZERO            TO  FLG-CHKKBN
                   END-IF
               END-IF
               IF      FLG-CHKKBN          =   1
                   ADD     1               TO  IDX-K
                   MOVE    IDX-K           TO  SPA-P037-LSTMAX
      *
                   MOVE    ONSKAKU-RES-BIRTHDAY    TO  WRK-SYMD
                   PERFORM 8001-SEIWA-YMDEDIT-SEC
                   MOVE    WRK-HENYMD      TO  SPA-P037-TBIRTHDAYW
                                                            (IDX-K)
      *            負担者番号
                   MOVE     ONSKAKU-RES-HKNJANUM
                                           TO   SPA-P037-TFTNJANUM
                                                               (IDX-K)
      *            負担者番号から公費の種類決定
      *            PERFORM 2301-FTNJANUM-KOHNUM-SEC
      *            受給者番号
                   MOVE     ONSKAKU-RES-NUM
                                           TO  SPA-P037-TJKYSNUM
                                                             (IDX-K)
      *            入院　有効期間
                   IF      ONSKAKU-KOH-TEKSTYMD (1)    NOT =   SPACE
                       MOVE    SPACE           TO  WRK-HEN-KIKAN
                       MOVE    ONSKAKU-KOH-TEKSTYMD (1)
                                               TO  WRK-SYMD
                       PERFORM 8001-SEIWA-YMDEDIT-SEC
                       MOVE    WRK-HENYMD      TO  WRK-HEN-KIKAN1
                       MOVE    ONSKAKU-KOH-TEKEDYMD (1)
                                               TO  WRK-SYMD
                       PERFORM 8001-SEIWA-YMDEDIT-SEC
                       MOVE    WRK-HENYMD      TO  WRK-HEN-KIKAN2
                       MOVE    "〜"            TO  WRK-HEN-K1
                       MOVE    WRK-HEN-KIKAN
                                               TO  SPA-P037-TTEKYMD1
                                                              (IDX-K)
                   END-IF
      *            外来　有効期間
                   IF      ONSKAKU-KOH-TEKSTYMD (2)    NOT =   SPACE
                       MOVE    SPACE           TO  WRK-HEN-KIKAN
                       MOVE    ONSKAKU-KOH-TEKSTYMD (2)
                                               TO  WRK-SYMD
                       PERFORM 8001-SEIWA-YMDEDIT-SEC
                       MOVE    WRK-HENYMD      TO  WRK-HEN-KIKAN1
                       MOVE    ONSKAKU-KOH-TEKEDYMD (2)
                                               TO  WRK-SYMD
                       PERFORM 8001-SEIWA-YMDEDIT-SEC
                       MOVE    WRK-HENYMD      TO  WRK-HEN-KIKAN2
                       MOVE    "〜"            TO  WRK-HEN-K1
                       MOVE    WRK-HEN-KIKAN
                                               TO  SPA-P037-TTEKYMD2
                                                              (IDX-K)
                   END-IF
      *            区分
      *            生年月日不一致
                   IF      ONSKAKU-RES-BIRTHDAY
                                       NOT =   SPA-P037-HBIRTHDAY
                       MOVE    "1"             TO  SPA-P037-TKUBUN
                                                              (IDX-K)
                   END-IF
                   IF      ONSKAKU-RES-HONKZKKBN   =   "9"
      *                対象外分
                       MOVE    "2"             TO  SPA-P037-TKUBUN
                                                              (IDX-K)
                   END-IF
      *            UUID
                   MOVE    ONSKAKU-TBL-UUID
                                           TO  SPA-P037-TBL-UUID
                                                             (IDX-K)
                END-IF
      *
      *
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           END-PERFORM
      * 
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
            .
       30012-KOUHI-SHIKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    公費医療券編集処理
      *****************************************************************
       30013-KOUHI-SEL-SEC              SECTION.
      *
           INITIALIZE                  SPA-P037-ONSKAKU-G
      *
           IF      SPA-P037-SELNUM     <   1  OR
                                       >   SPA-P037-LSTMAX
               MOVE    "0001"              TO  SPA-ERRCD
               GO  TO  30013-KOUHI-SEL-EXT
           END-IF
      *
           MOVE    SPA-P037-SELNUM     TO  IDX
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P037-TBL-UUID (IDX)
                                       TO  ONSKAKU-TBL-UUID
      *
           PERFORM 9002-ONSKAKU-KEYREAD-SEC
      *
           IF      FLG-ONSKAKU     NOT =   ZERO
               MOVE    "0100"              TO  SPA-ERRCD
               GO  TO  30013-KOUHI-SEL-EXT
           END-IF
      *
           MOVE    ONSKAKU-RES-BIRTHDAY
                                       TO  SPA-P037-BIRTHDAY
           MOVE    ONSKAKU-RES-BIRTHDAY    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P037-BIRTHDAYW
      *    負担者番号
           MOVE     ONSKAKU-RES-HKNJANUM
                                       TO   SPA-P037-FTNJANUM
      *    負担者番号から公費の種類決定
      *    PERFORM 2301-FTNJANUM-KOHNUM-SEC
      *    受給者番号
           MOVE     ONSKAKU-RES-NUM    TO  SPA-P037-JKYSNUM
      *    入院　有効期間
           IF      ONSKAKU-KOH-TEKSTYMD (1)    NOT =   SPACE
               MOVE    ONSKAKU-KOH-TEKSTYMD (1)
                                           TO  SPA-P037-TEKSTYMD1
               MOVE    ONSKAKU-KOH-TEKSTYMD (1)
                                           TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P037-TEKSTYMD1W
      *
               MOVE    ONSKAKU-KOH-TEKEDYMD (1)
                                           TO  SPA-P037-TEKEDYMD1
               MOVE    ONSKAKU-KOH-TEKEDYMD (1)
                                           TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P037-TEKEDYMD1W
           END-IF
           IF      ONSKAKU-KOH-TEKSTYMD (2)    NOT =   SPACE
               MOVE    ONSKAKU-KOH-TEKSTYMD (2)
                                           TO  SPA-P037-TEKSTYMD2
               MOVE    ONSKAKU-KOH-TEKSTYMD (2)
                                           TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P037-TEKSTYMD2W
      *
               MOVE    ONSKAKU-KOH-TEKEDYMD (2)
                                           TO  SPA-P037-TEKEDYMD2
               MOVE    ONSKAKU-KOH-TEKEDYMD (2)
                                           TO  WRK-SYMD
               PERFORM 8001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P037-TEKEDYMD2W
           END-IF
      *    画像ファイル名
           MOVE    ONSKAKU-HKNOCR-FILENAME   TO  SPA-P037-HKNOCR-FILE
      *
      *    保険の生年月日と一致
           IF      ONSKAKU-RES-BIRTHDAY    NOT =   SPA-P037-HBIRTHDAY
               MOVE    "1"             TO   SPA-P037-KUBUN
           END-IF
      *    削除分
           IF      ONSKAKU-RES-HONKZKKBN   =   "9"
               MOVE    "2"             TO   SPA-P037-KUBUN
           END-IF
           MOVE    2                   TO  SPA-P037-CUR
            .
       30013-KOUHI-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号編集処理
      *****************************************************************
       300111-HKNJAINF-HEN-SEC              SECTION.
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM          TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    HKNJA-HKNJANAME     TO  WRK-HKNJANAME
           MOVE    SPACE               TO  WRK-HKNNUMMEI
      *
      *
           .
       300111-HKNJAINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       300111-NENREI-SET-SEC             SECTION.
      *
           INITIALIZE                  WRK-NENREI-AREA
           IF      SPA-P037-HBIRTHDAY  =   SPACE
                                       OR  ZERO
                                       OR  ALL "9"
               GO  TO  300111-NENREI-SET-EXT
           END-IF
      *
           MOVE    SPA-P037-HBIRTHDAY  TO  WRK-YMDS1
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
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-P037-NENREI
      *
           .
       300111-NENREI-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
               WHEN    "0102"
      *            公費対象外
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4041-TAISYOGAI-SYORI-SEC
                   END-IF
                   MOVE    SPACE               TO  SPA-PIDCD
               WHEN    "0103"
      *            公費更新
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4121-KOH-KOUSIN-SYORI-SEC
                   END-IF
                   MOVE    SPACE               TO  SPA-PIDCD
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
           MOVE    SPACE               TO  P037
           INITIALIZE                      P037
           MOVE    SPA-P037-HBIRTHDAYW  TO  P037-HBIRTHDAY
           MOVE    SPA-P037-NENREI      TO  P037-NENREI
      *
           MOVE    SPA-P037-PTNUM       TO  P037-PTNUM
           MOVE    SPA-P037-NAME        TO  P037-NAME
           MOVE    SPA-P037-KANANAME    TO  P037-KANANAME
           MOVE    SPA-P037-SEX         TO  P037-SEX
      *
           MOVE    SPA-P037-BIRTHDAYW   TO  P037-BIRTHDAY
           MOVE    SPA-P037-JKYSNUM     TO  P037-JKYSNUM
           MOVE    SPA-P037-FTNJANUM    TO  P037-FTNJANUM
           MOVE    SPA-P037-TEKSTYMD1W  TO  P037-TEKSTYMD1
           MOVE    SPA-P037-TEKEDYMD1W  TO  P037-TEKEDYMD1
           MOVE    SPA-P037-TEKSTYMD2W  TO  P037-TEKSTYMD2
           MOVE    SPA-P037-TEKEDYMD2W  TO  P037-TEKEDYMD2
      * 
           EVALUATE    SPA-P037-KUBUN
               WHEN    "1"
                   MOVE    "〔生年月日不一致〕"
                                           TO  P037-KOHMSG-DATA
               WHEN    "2"
                   MOVE    "〔対象外設定〕"
                                           TO  P037-KOHMSG-DATA
           END-EVALUATE
      *
           MOVE    SPA-P037-SELNUM      TO  P037-SELNUM
      *
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-P037-LSTMAX
               MOVE    IDX                     TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  P037-RENNUM  (IDX)
               MOVE    SPA-P037-TBIRTHDAYW (IDX)
                                               TO  P037-TBIRTHDAY (IDX)
               MOVE    SPA-P037-TFTNJANUM (IDX)
                                               TO  P037-TFTNJANUM (IDX)
               MOVE    SPA-P037-TJKYSNUM (IDX)
                                               TO  P037-TJKYSNUM (IDX)
               MOVE    SPA-P037-TTEKYMD1 (IDX)
                                               TO  P037-TTEKYMD1 (IDX)
               MOVE    SPA-P037-TTEKYMD2 (IDX)
                                               TO  P037-TTEKYMD2 (IDX)
      *
               EVALUATE    SPA-P037-TKUBUN (IDX)
                   WHEN    "1"
                       MOVE    "×"            TO  P037-TKBN (IDX)
                   WHEN    "2"
                       MOVE    "×"            TO  P037-TKBN (IDX)
               END-EVALUATE
      *
               IF      IDX                 =   SPA-P037-SELNUM
                   MOVE    "T"             TO  P037-KOHLIST-SELECT
                                                               (IDX)
                   MOVE    IDX             TO  P037-KOHLIST-ROW
               ELSE
                   MOVE    "F"             TO  P037-KOHLIST-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-P037-LSTMAX     TO  P037-KOHLIST-COUNT
      *
           IF      SPA-P037-SYORIKBN   =   "2"
               MOVE    "F10 公費対象分"        TO  P037-B10-LABEL
           ELSE
               MOVE    "F10 全公費医療券"      TO  P037-B10-LABEL
           END-IF
           IF      SPA-P037-KUBUN      =   "2"
      *        対象外設定分
               MOVE    "F9 対象外解除"        TO  P037-B09-LABEL
           ELSE
               MOVE    "F9 公費対象外"        TO  P037-B09-LABEL
           END-IF
      *
      *    保険証ＯＣＲ画面表示
           IF      SPA-P037-HKNOCR-FILE    NOT =   SPACE
               PERFORM 50010-IMAGE-HEN-SEC
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
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-P037-CUR        =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-P037-CUR
               WHEN    01
                   MOVE    "SELNUM"      TO  MCP-WIDGET
               WHEN    02
                   MOVE    "BIRTHDAY"    TO  MCP-WIDGET
               WHEN    03
                   MOVE    "FTNJANUM"    TO  MCP-WIDGET
               WHEN    04
                   MOVE    "JKYSNUM"     TO  MCP-WIDGET
               WHEN    05
                   MOVE    "TEKSTYMD1"     TO  MCP-WIDGET
               WHEN    06
                   MOVE    "TEKEDYMD1"     TO  MCP-WIDGET
               WHEN    07
                   MOVE    "TEKSTYMD2"     TO  MCP-WIDGET
               WHEN    08
                   MOVE    "TEKEDYMD2"     TO  MCP-WIDGET
               WHEN    11
                   MOVE    "B04"         TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B10"         TO  MCP-WIDGET
               WHEN    12
                   MOVE    "B12"         TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.

      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    SPA-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2               TO  SPA-P037-CUR
               WHEN    "BIRTHDAY"
                   MOVE    3               TO  SPA-P037-CUR
               WHEN    "FTNJANUM"
                   MOVE    4               TO  SPA-P037-CUR
               WHEN    "JKYSNUM"
                   MOVE    5               TO  SPA-P037-CUR
               WHEN    "TEKSTYMD1"
                   MOVE    6               TO  SPA-P037-CUR
               WHEN    "TEKEDYMD1"
                   MOVE    7               TO  SPA-P037-CUR
               WHEN    "TEKSTYMD2"
                   MOVE    8               TO  SPA-P037-CUR
               WHEN    "TEKEDYMD2"
                   MOVE    12              TO  SPA-P037-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険証画像編集処理
      *****************************************************************
       50010-IMAGE-HEN-SEC              SECTION.
      *
           INITIALIZE                  ORCSONSHIIMGAREA
      * 
           MOVE    "01"                TO  LNK-ONSHIIMG-SYOKBN
           MOVE    "01"                TO  LNK-ONSHIIMG-IMGKBN
      *    画像ファイル名
           MOVE    SPA-P037-HKNOCR-FILE
                                     TO  LNK-ONSHIIMG-FILE-NAME
           CALL   "ORCSONSHIIMG"     USING
                                     ORCSONSHIIMGAREA
                                     SPA-AREA
                                     MCPAREA
           IF       LNK-ONSHIIMG-RC    =   1
      *        異常終了
               MOVE    LNK-ONSHIIMG-IMG-MSG    TO  P037-MIDASHI
               GO  TO  50010-IMAGE-HEN-EXT
           END-IF
           MOVE    LNK-ONSHIIMG-FILE-FOLDER
                                       TO  ORCA-BLOB-FILE
      *    画像イメージを表示
           MOVE    "BLOBIMPORT"        TO  MCP-FUNC
           MOVE    "blob"              TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
      *                                   
           MOVE    ORCA-BLOB-OBJECT    TO  P037-IMGAREA
           .
       50010-IMAGE-HEN-EXT.
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
      *    前↑
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 405-BACK-SYORI-SEC
      *    次↓
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 406-NEXT-SYORI-SEC
      *    患者対象外
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-DELSYORI-SEC
      *    表示切替
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-LIST-CNG-SEC
      *    更新処理
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 412-ONSHI-KOUSIN-SEC
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
           MOVE    ZERO                TO  SPA-P037-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 210-SELNUM-CHK-SEC
      *        リスト選択
               WHEN    ANY             ALSO    "KOHLIST"
                   PERFORM 4001-KOHLIST-SEL-SEC
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
      *    選択番号選択処理
      *****************************************************************
       210-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    P037-SELNUM         TO  SPA-P037-SELNUM
      *
           IF      SPA-P037-SELNUM     =   ZERO
               INITIALIZE                  SPA-P037-ONSKAKU-G
               MOVE    1               TO  SPA-P037-CUR
           ELSE
               PERFORM 30013-KOUHI-SEL-SEC
           END-IF
           .
       210-SELNUM-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4001-KOHLIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-P037-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P037-LSTMAX
               IF      P037-KOHLIST-SELECT (IDX) =   "T"
                   MOVE    IDX             TO  SPA-P037-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-P037-SELNUM    NOT =   ZERO
               PERFORM 30013-KOUHI-SEL-SEC
           END-IF
      *
           .
       4001-KOHLIST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック
           PERFORM 4102-KIHON-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P037-BIRTHDAY     TO  SPA-P037-BIRTHDAYW
           MOVE    P037-FTNJANUM     TO  SPA-P037-FTNJANUM
           MOVE    P037-JKYSNUM      TO  SPA-P037-JKYSNUM
           MOVE    P037-TEKSTYMD1    TO  SPA-P037-TEKSTYMD1W
           MOVE    P037-TEKEDYMD1    TO  SPA-P037-TEKEDYMD1W
           MOVE    P037-TEKSTYMD2    TO  SPA-P037-TEKSTYMD2W
           MOVE    P037-TEKEDYMD2    TO  SPA-P037-TEKEDYMD2W
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    生年月日
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-BIRTHDAY-CHK-SEC
           END-IF
      *    負担者番号
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-FTNJANUM-CHK-SEC
           END-IF
      *    受給者番号
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-JKYSNUM-CHK-SEC
           END-IF
      *    入院　開始日
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-TEKSTYMD1-CHK-SEC
           END-IF
      *    入院　終了日
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-TEKEDYMD1-CHK-SEC
           END-IF
      *    外来　開始日
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-TEKSTYMD2-CHK-SEC
           END-IF
      *    外来　終了日
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-TEKEDYMD2-CHK-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    生年月日チェック処理
      ****************************************************************
       4101-BIRTHDAY-CHK-SEC           SECTION.
      *
           IF      SPA-P037-BIRTHDAYW      =   SPACE
               MOVE    SPACE               TO  SPA-P037-BIRTHDAY
               GO  TO  4101-BIRTHDAY-CHK-EXT
           END-IF
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P037-BIRTHDAYW  TO  SGDAY-INDAY
           MOVE    SPA-SYSYMD          TO  SGDAY-SYSYMD
           MOVE    "2"                 TO  SGDAY-SYSKBN
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
      *
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-P037-BIRTHDAYW
               MOVE    SGDAY-SDAY          TO  SPA-P037-BIRTHDAY
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-P037-CUR
               MOVE    SPACE               TO  SPA-P037-BIRTHDAY
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
      *    システム日付とのチェック
           IF      SPA-P037-BIRTHDAY   >   SPA-SYSYMD
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-P037-CUR
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *    保険証の生年月日と違う
           IF      SPA-P037-BIRTHDAY   NOT =   SPA-P037-HBIRTHDAY
               MOVE    "0018"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-P037-CUR
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
      *    年齢計算
      **   PERFORM 300111-NENREI-SET-SEC
           .
       4101-BIRTHDAY-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    負担者番号チェック処理
      ****************************************************************
       4101-FTNJANUM-CHK-SEC     SECTION.
      *
      *    未入力判定
           IF      SPA-P037-FTNJANUM   =   SPACE
               GO      TO      4101-FTNJANUM-CHK-EXT
           END-IF
      *
      *    桁数チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-P037-FTNJANUM   TO  KANACHK-MAE-INPUT
           MOVE    8                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-P037-FTNJANUM
      *    間に空白がないこと
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-RC2     NOT =   ZERO  )
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-P037-CUR
           END-IF
           .
       4101-FTNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受給者番号入力　処理
      *****************************************************************
       4101-JKYSNUM-CHK-SEC           SECTION.
      *
           IF      SPA-P037-JKYSNUM     =   SPACE
               GO      TO      4101-JKYSNUM-CHK-EXT
           END-IF
      *
      *    半角・全角のチェックと桁数判定
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-P037-JKYSNUM    TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-P037-JKYSNUM
           IF      KANACHK-RC      NOT =   ZERO
      *        半角・全角混在エラー
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-P037-CUR
               GO      TO          4101-JKYSNUM-CHK-EXT
           END-IF
           IF      KANACHK-RC2         =   8
      *        入力文字エラー
               MOVE    "0005"             TO  SPA-ERRCD
               MOVE    4                  TO  SPA-P037-CUR
           END-IF
      *
           .
       4101-JKYSNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院　開始日チェック処理
      *****************************************************************
       4101-TEKSTYMD1-CHK-SEC           SECTION.
      *
           IF      SPA-P037-TEKSTYMD1W     =   SPACE
               MOVE    SPACE               TO  SPA-P037-TEKSTYMD1
               GO      TO   4101-TEKSTYMD1-CHK-EXT
           END-IF
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P037-TEKSTYMD1W
                                       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-P037-TEKSTYMD1W
               MOVE    SGDAY-SDAY          TO  SPA-P037-TEKSTYMD1
           ELSE
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-P037-CUR
           END-IF
      *    
           .
       4101-TEKSTYMD1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院　終了日チェック処理
      *****************************************************************
       4101-TEKEDYMD1-CHK-SEC           SECTION.
      *
           IF      SPA-P037-TEKEDYMD1W     =   SPACE
               IF      SPA-P037-TEKSTYMD1      =   SPACE
                   MOVE    SPACE               TO  SPA-P037-TEKEDYMD1
                   GO      TO   4101-TEKEDYMD1-CHK-EXT
               END-IF
      *        開始日入力あり
               IF     (SPA-MCP-WIDGET      =   "TEKEDYMD1" )
                 OR   (FLG-KOUSIN          =   1    )
                   MOVE    "99999999"      TO  SPA-P037-TEKEDYMD1W
               ELSE
                   GO      TO   4101-TEKEDYMD1-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-P037-TEKEDYMD1W     =   SPACE
                                           OR  "99999999"
               MOVE    "99999999"          TO  SPA-P037-TEKEDYMD1W
               MOVE    ALL "9"             TO  SPA-P037-TEKEDYMD1
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-P037-TEKEDYMD1W
                                           TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-P037-TEKEDYMD1W
                   MOVE    SGDAY-SDAY      TO  SPA-P037-TEKEDYMD1
               ELSE
                   MOVE    "0007"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-P037-CUR
               END-IF
           END-IF
      *
           IF      SPA-P037-TEKSTYMD1  =   SPACE
      *        終了日のみの入力
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-P037-CUR
           END-IF
      * 
           IF      (SPA-P037-TEKSTYMD1    >=  SPA-P037-TEKEDYMD1)
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-P037-CUR
           END-IF
           .
       4101-TEKEDYMD1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来  開始日チェック処理
      *****************************************************************
       4101-TEKSTYMD2-CHK-SEC           SECTION.
      *
           IF      SPA-P037-TEKSTYMD2W     =   SPACE
               MOVE    SPACE               TO  SPA-P037-TEKSTYMD2
               GO      TO   4101-TEKSTYMD2-CHK-EXT
           END-IF
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P037-TEKSTYMD2W
                                       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-P037-TEKSTYMD2W
               MOVE    SGDAY-SDAY          TO  SPA-P037-TEKSTYMD2
           ELSE
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-P037-CUR
           END-IF
      *    
           .
       4101-TEKSTYMD2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    外来　終了日チェック処理
      *****************************************************************
       4101-TEKEDYMD2-CHK-SEC           SECTION.
      *
           IF      SPA-P037-TEKEDYMD2W     =   SPACE
               IF      SPA-P037-TEKSTYMD2      =   SPACE
                   MOVE    SPACE               TO  SPA-P037-TEKEDYMD2
                   GO      TO   4101-TEKEDYMD2-CHK-EXT
               END-IF
      *        開始日入力あり
               IF     (SPA-MCP-WIDGET      =   "TEKEDYMD2" )
                 OR   (FLG-KOUSIN          =   1    )
                   MOVE    "99999999"      TO  SPA-P037-TEKEDYMD2W
               ELSE
                   GO      TO   4101-TEKEDYMD2-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-P037-TEKEDYMD2W     =   SPACE
                                           OR  "99999999"
               MOVE    "99999999"          TO  SPA-P037-TEKEDYMD2W
               MOVE    ALL "9"             TO  SPA-P037-TEKEDYMD2
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-P037-TEKEDYMD2W
                                           TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-P037-TEKEDYMD2W
                   MOVE    SGDAY-SDAY      TO  SPA-P037-TEKEDYMD2
               ELSE
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-P037-CUR
               END-IF
           END-IF
      *
           IF      SPA-P037-TEKSTYMD2  =   SPACE
      *        終了日のみの入力
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-P037-CUR
           END-IF
           IF      (SPA-P037-TEKSTYMD2    >=  SPA-P037-TEKEDYMD2)
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-P037-CUR
           END-IF
           .
       4101-TEKEDYMD2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
      *    オンライン資格確認編集
           PERFORM 3001-ONSHIKAKU-SYORI-SEC
      *
           MOVE    1                   TO  SPA-P037-CUR
           .
       402-CLEAR-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    前　選択処理
      *****************************************************************
       405-BACK-SYORI-SEC             SECTION.
      *
           IF      SPA-P037-LSTMAX    =   1
               MOVE    1                  TO  SPA-P037-SELNUM
           END-IF
           IF      SPA-P037-LSTMAX    >=  1
               IF      SPA-P037-SELNUM    =   1
                                          OR  ZERO
                   MOVE    SPA-P037-LSTMAX    TO  SPA-P037-SELNUM
               ELSE
                   COMPUTE SPA-P037-SELNUM    =   SPA-P037-SELNUM
                                              -   1
               END-IF
               PERFORM 30013-KOUHI-SEL-SEC
               MOVE    2                   TO  SPA-P037-CUR
           END-IF
      *
           .
       405-BACK-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次　選択処理
      *****************************************************************
       406-NEXT-SYORI-SEC             SECTION.
      *
           IF      SPA-P037-LSTMAX    =   1
               MOVE    1                  TO  SPA-P037-SELNUM
           END-IF
           IF      SPA-P037-LSTMAX    >=  1
               IF      SPA-P037-SELNUM    =   SPA-P037-LSTMAX
                   MOVE    1                  TO  SPA-P037-SELNUM
               ELSE
                   COMPUTE SPA-P037-SELNUM    =   SPA-P037-SELNUM
                                              +   1
               END-IF
               PERFORM 30013-KOUHI-SEL-SEC
               MOVE    2                   TO  SPA-P037-CUR
           END-IF
      *
           .
       406-NEXT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    公費対象外設定
      *****************************************************************
       409-DELSYORI-SEC             SECTION.
      *
           IF      SPA-P037-SELNUM     =   ZERO
               GO      TO      409-DELSYORI-EXT
           END-IF
      *
           IF      SPA-P037-SELNUM     <   1  OR
                                       >   SPA-P037-LSTMAX
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P037-CUR
               GO  TO  409-DELSYORI-EXT
           END-IF
      *
      *    対象外済みは解除
           MOVE    SPA-P037-SELNUM     TO  IDX
           IF      SPA-P037-TKUBUN (IDX)   =   "2"
               MOVE    "0102"              TO  SPA-PIDCD
           ELSE
               MOVE    "0101"              TO  SPA-PIDCD
           END-IF
           .
      *
       409-DELSYORI-EXT.
           EXIT.
      *****************************************************************
      *    公費対象外設定処理
      *****************************************************************
       4041-TAISYOGAI-SYORI-SEC        SECTION.
      *
      *
           MOVE    SPA-P037-SELNUM     TO  IDX
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P037-TBL-UUID (IDX)
                                       TO  ONSKAKU-TBL-UUID
      *
           PERFORM 9002-ONSKAKU-KEYREAD-SEC
      *
           IF      FLG-ONSKAKU     NOT =   ZERO
               MOVE    "0100"              TO  SPA-ERRCD
               GO  TO  4041-TAISYOGAI-SYORI-EXT
           END-IF
      *    対象外設定は解除
           IF      ONSKAKU-RES-HONKZKKBN   =   "9"
               MOVE    SPACE           TO  ONSKAKU-RES-HONKZKKBN
           ELSE
               MOVE    "9"             TO  ONSKAKU-RES-HONKZKKBN
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM 49901-ONSKAKU-MODEUPDATE-SEC
      *    公費リスト表示
           PERFORM 30012-KOUHI-SHIKAKU-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-P037-SELNUM
           INITIALIZE                  SPA-P037-ONSKAKU-G
           IF      SPA-P037-LSTMAX     >   ZERO
               MOVE    1                   TO  SPA-P037-SELNUM
               PERFORM 30013-KOUHI-SEL-SEC
           END-IF
           .
       4041-TAISYOGAI-SYORI-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費対象外設定
      *****************************************************************
       410-LIST-CNG-SEC             SECTION.
      *
           IF      SPA-P037-SYORIKBN   =   "2"
      *        生年月日有効のみ
               MOVE    "1"             TO  SPA-P037-SYORIKBN
           ELSE
               MOVE    "2"             TO  SPA-P037-SYORIKBN
           END-IF
           PERFORM 30012-KOUHI-SHIKAKU-HEN-SEC
           IF      SPA-P037-LSTMAX     =   ZERO
               MOVE    "0019"          TO  SPA-ERRCD
               MOVE   10               TO  SPA-P037-CUR
           END-IF 
      *
           IF      SPA-P037-LSTMAX     >   ZERO
               MOVE    1                   TO  SPA-P037-SELNUM
               PERFORM 30013-KOUHI-SEL-SEC
           END-IF
           .
       410-LIST-CNG-EXT.
           EXIT.
      *****************************************************************
      *    公費　更新処理
      *****************************************************************
       412-ONSHI-KOUSIN-SEC             SECTION.
      *
      *    選択中であること
           IF      SPA-P037-SELNUM     =   ZERO
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P037-CUR
               GO  TO  412-ONSHI-KOUSIN-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-KOUSIN
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
               OR (SPA-PIDCD       NOT =   SPACE )
               GO  TO  412-ONSHI-KOUSIN-EXT
           END-IF
      *
      *    更新前チェック
           IF      SPA-P037-BIRTHDAYW      =   SPACE
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-P037-CUR
               GO  TO  412-ONSHI-KOUSIN-EXT
           END-IF
      *    負担者番号、受給者番号はどちらが必須
           IF    ( SPA-P037-FTNJANUM       =   SPACE )
            AND  ( SPA-P037-JKYSNUM        =   SPACE )
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-P037-CUR
               GO  TO  412-ONSHI-KOUSIN-EXT
           END-IF
      *    有効期間の入力なし
           IF      (SPA-P037-TEKSTYMD1      =   SPACE)
             AND   (SPA-P037-TEKEDYMD1      =   SPACE)
             AND   (SPA-P037-TEKSTYMD2      =   SPACE)
             AND   (SPA-P037-TEKEDYMD2      =   SPACE)
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-P037-CUR
               GO  TO  412-ONSHI-KOUSIN-EXT
           END-IF
      *
           IF      (SPA-P037-TEKSTYMD1      =   SPACE)
             AND   (SPA-P037-TEKEDYMD1      =   SPACE)
               CONTINUE
           ELSE
               IF      (SPA-P037-TEKSTYMD1      =   SPACE)
                   MOVE    "0016"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-P037-CUR
                   GO  TO  412-ONSHI-KOUSIN-EXT
               END-IF
           END-IF
      *
           IF      (SPA-P037-TEKSTYMD2      =   SPACE)
             AND   (SPA-P037-TEKEDYMD2      =   SPACE)
               CONTINUE
           ELSE
               IF      (SPA-P037-TEKSTYMD2      =   SPACE)
                 OR    (SPA-P037-TEKEDYMD2      =   SPACE)
                   MOVE    "0017"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-P037-CUR
                   GO  TO  412-ONSHI-KOUSIN-EXT
               END-IF
           END-IF
      *
           MOVE    "0103"              TO  SPA-PIDCD
      * 
            .
       412-ONSHI-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報更新処理
      *****************************************************************
       4121-KOH-KOUSIN-SYORI-SEC     SECTION.
      *
           MOVE    SPA-P037-SELNUM     TO  IDX
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P037-TBL-UUID (IDX)
                                       TO  ONSKAKU-TBL-UUID
      *
           PERFORM 9002-ONSKAKU-KEYREAD-SEC
           IF      FLG-ONSKAKU     NOT =   ZERO
               MOVE    "0100"              TO  SPA-ERRCD
               GO  TO  4121-KOH-KOUSIN-SYORI-EXT
           END-IF
      *
           MOVE    SPA-P037-BIRTHDAY   TO  ONSKAKU-RES-BIRTHDAY
      *    負担者番号
           MOVE    SPA-P037-FTNJANUM   TO  ONSKAKU-RES-HKNJANUM
      *    受給者番号
           MOVE    SPA-P037-JKYSNUM    TO  ONSKAKU-RES-NUM
      *    入院　有効期間
           MOVE    SPA-P037-TEKSTYMD1  TO  ONSKAKU-KOH-TEKSTYMD (1)
           MOVE    SPA-P037-TEKEDYMD1  TO  ONSKAKU-KOH-TEKEDYMD (1)
           MOVE    SPA-P037-TEKSTYMD2  TO  ONSKAKU-KOH-TEKSTYMD (2)
           MOVE    SPA-P037-TEKEDYMD2  TO  ONSKAKU-KOH-TEKEDYMD (2)
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM 49901-ONSKAKU-MODEUPDATE-SEC
      *
      *    公費リスト表示
           PERFORM 30012-KOUHI-SHIKAKU-HEN-SEC
      *
           MOVE    ZERO                TO  SPA-P037-SELNUM
           INITIALIZE                  SPA-P037-ONSKAKU-G
      *
           IF      SPA-P037-LSTMAX     >   ZERO
               MOVE    1                   TO  SPA-P037-SELNUM
               PERFORM 30013-KOUHI-SEL-SEC
           END-IF
            .
       4121-KOH-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画像削除処理
      *****************************************************************
       412013-IMAGE-DEL-SEC        SECTION.
      *
           INITIALIZE                  ORCSONSHIIMGAREA
      *    画像削除
           MOVE    "02"                TO  LNK-ONSHIIMG-SYOKBN
           MOVE    "01"                TO  LNK-ONSHIIMG-IMGKBN
      *    画像ファイル名
           MOVE    SPA-P037-HKNOCR-FILE
                                     TO  LNK-ONSHIIMG-FILE-NAME
           CALL   "ORCSONSHIIMG"     USING
                                     ORCSONSHIIMGAREA
                                     SPA-AREA
                                     MCPAREA
           IF       LNK-ONSHIIMG-RC    =   1
      *        異常終了
               MOVE    "1012"                  TO  SPA-ERRCD
               MOVE    LNK-ONSHIIMG-IMG-MSG    TO  WRK-ERRMSG
           END-IF
            .
       412013-IMAGE-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認解除処理
      *****************************************************************
       49901-ONSKAKU-MODEUPDATE-SEC         SECTION.
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
               DISPLAY "P033 ONSKAKU KEY UPD ERR:"  MCP-RC
                   ",KEY:" ONSKAKU-KEY
           END-IF
           .
       49901-ONSKAKU-MODEUPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P037"              TO  SPA-MOTOPG
           MOVE    SPA-P037-MOTOPG     TO  SPA-SAKIPG
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
               PERFORM 520-PIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P037"              TO  MCP-WINDOW
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
                   STRING  "生年月日暦日エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "生年月日＜システム日付です。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "負担者番号が正しくありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "受給者番号が正しくありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   MOVE    "（入院）有効開始日暦日エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "（入院）有効終了日暦日エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   STRING  "（入院）有効開始日≧有効終了日です。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0009"
                   MOVE    "（外来）有効開始日暦日エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "（外来）有効終了日暦日エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   STRING  "（外来）有効開始日≧有効終了日です。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   STRING  "公費の選択がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "生年月日の入力がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0014"
                   STRING  "負担者番号または受給者番号を"
                           "入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0015"
                   STRING  "有効期間の入力がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0016"
                   STRING  "（入院）有効開始日の入力がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0017"
                   STRING  "（入院）有効開始日の入力がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0018"
                   STRING  "保険証の生年月日と一致しません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0019"
                   STRING  "対象の公費情報はありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *         WHEN    "0009"
      *            STRING  "患者番号で登録されている生年月日は"
      *                     SPA-P037-HEN-BIRTHDAYW
      *                     "です。画面の生年月日と一致しません。"
      *                                         DELIMITED  BY  SIZE
      *                                         INTO  SPA-ERRMSG
      *             END-STRING
      *
               WHEN    "0100"
                   STRING  "オン資格確認データが存在しません。"
                           "最初から処理を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1011"
                   STRING  "保険証資格確認が異常終了しました"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1012"
                   MOVE    WRK-ERRMSG          TO  SPA-ERRMSG
      *   
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P037"              TO  SPA-MOTOPG
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
      *    警告メッセージ表示理
      *****************************************************************
       520-PIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PIDMSG
           EVALUATE    SPA-PIDCD
      *         WHEN    "0100"
      *             STRING  "患者番号で登録されている生年月日："
      *                     SPA-P037-HEN-BIRTHDAYW
      *                     "に変更します。よろしいですか？"
      *                                         DELIMITED  BY  SIZE
      *                                         INTO  WRK-PIDMSG
      *             END-STRING
               WHEN    "0101"
                   STRING  "選択した公費医療券情報を対象外とします。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "選択した公費医療券情報の対象外を"
                           "解除します。よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "公費医療券情報を更新します。"
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
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P037"              TO  SPA-MOTOPG
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
      *    半角を全角へ変換処理
      *****************************************************************
       890-HALF-ZENC-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-MAX             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZENC-EXT.
           EXIT.
      *
      *****************************************************************
      **    オンライン資格確認キー読み込み
      *****************************************************************
       9002-ONSKAKU-KEYREAD-SEC          SECTION.
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
       9002-ONSKAKU-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      *    オンライン資格確認読込
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
      *
      *****************************************************************
      **    患者マスターキー読み込み
      *****************************************************************
       9001-PTINF-KEYREAD-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
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
       9001-PTINF-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      **    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
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
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO                TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
           .
       921-HKNNUM-READ-EXT.
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
