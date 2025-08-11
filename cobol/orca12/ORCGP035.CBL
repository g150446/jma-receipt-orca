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
       PROGRAM-ID.             ORCGP035.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : オンライン資格患者確認画面（Ｐ０３５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  22/09/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.01.00     ORCAMO       22/12/XX  公費医療券画像確認対応
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
           03  FLG-HKNNUM          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
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
           03  WRK-CHK-IDX         PIC 9(03).
           03  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-FTNRATE-9           PIC 9(03).
           03  WRK-Z9                  PIC Z9.
           03  WRK-FTNRATE-G.
               05  WRK-FTNRATE-X           PIC X(02).
               05  WRK-FTNRATE-M           PIC X(02).
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
      *R05.3
      *
       01  COMS-MSG1.
           03  FILLER              PIC X(46)  VALUE
           "※保険証画像と資格確認結果が一致していることを".
           03  FILLER              PIC X(20)   VALUE
           "ご確認下さい。".
       01  COMS-MSG2.
           03  FILLER              PIC X(40)  VALUE
           "一致していない場合は、入力内容を修正後、".
           03  FILLER              PIC X(40)   VALUE
           "再度資格確認を行って下さい。".
      *
      *    保険マスタ
       01  HZN-PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC"  REPLACING
                                     //PTHKN-// BY //HZN-PTHKN-//.
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
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
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
           COPY    "CPORAPI012SUB1.INC".
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
           COPY    "CPORCSLOCK.INC".
      *
      *    保険証画像編集パラメタ
           COPY    "CPORCSONSHIIMG.INC".
      *
      *    資格確認照会パラメタ
           COPY    "CPORCSONSHI001.INC".
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
      *     INITIALIZE                  FLG-AREA
      *     INITIALIZE                  CNT-AREA
      *     INITIALIZE                  WRK-AREA
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
               MOVE    "P035"              TO  MCP-WINDOW
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
               WHEN    "P036"
                   PERFORM 3036-P036-SYORI-SEC
               WHEN    "P037"
      *            公費画像確認
                   MOVE    1                   TO  SPA-P035-CUR
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
      *** INITIALIZE                      SPA-P035-AREA
      *
           MOVE    SPA-MOTOPG          TO  SPA-P035-MOTOPG
      *    オンライン資格確認編集
           PERFORM 3001-ONSHIKAKU-SYORI-SEC
      *
           MOVE    1                   TO  SPA-P035-CUR
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認編集処理
      *****************************************************************
       3001-ONSHIKAKU-SYORI-SEC              SECTION.
      *
           INITIALIZE                      SPA-P035-ONSKAKU-G
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P035-SEL-UUID   TO  ONSKAKU-TBL-UUID
      *
           PERFORM 9002-ONSKAKU-KEYREAD-SEC
      *
           IF      FLG-ONSKAKU         =   ZERO
      *        オンライン資格確認情報編集処理
               PERFORM 30011-ONSHIKAKU-HEN-SEC
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
      *    画像ファイル名
           MOVE  ONSKAKU-HKNOCR-FILENAME   TO  SPA-P035-HKNOCR-FILE
      *    保険証ＯＣＲであること
           IF     (ONSKAKU-SHOKAI-KBN      =   "2"  )  AND
                  (ONSKAKU-ORCA-SHOKAI-KBN =   "2"
                                           OR  "3"  )
               CONTINUE
           ELSE
               MOVE    "XXX"               TO  SPA-ERRCD
           END-IF
      *    照会情報
           MOVE    ONSKAKU-ORCA-SHOKAI-KBN     TO  SPA-P035-SHOKAI-KBN
      *    保険者番号
           MOVE    ONSKAKU-SHO-HKNJANUM    TO  SPA-P035-HKNJANUM
           MOVE    ONSKAKU-SHO-HKNJANUM    TO  WRK-HKNJANUM
           PERFORM 300111-HKNJAINF-HEN-SEC
           MOVE    WRK-HKNJANAME       TO  SPA-P035-HKNJANAME
      *     MOVE    WRK-HKNNUMMEI       TO  SPA-P035-HKNNUMMEI
      *     MOVE    WRK-HKNNUM          TO  SPA-P035-HKNNUM
      *    記号番号
           MOVE    ONSKAKU-SHO-KIGO    TO  SPA-P035-KIGO
           MOVE    ONSKAKU-SHO-NUM     TO  SPA-P035-NUM 
           MOVE    ONSKAKU-SHO-EDABAN  TO  SPA-P035-EDABAN
      *    生年月日
           MOVE    ONSKAKU-SHO-BIRTHDAY    TO  SPA-P035-BIRTHDAY
           MOVE    ONSKAKU-SHO-BIRTHDAY    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P035-BIRTHDAYW
      *    年齢計算
           PERFORM 300111-NENREI-SET-SEC
      *
      *2022/12
           EVALUATE    ONSKAKU-SIKAKU-YUKO
                WHEN    "1"
                    MOVE    "【有効】"     TO  SPA-P035-YUKO-MSG
               WHEN    "2"
                   MOVE     "【無効】"     TO  SPA-P035-YUKO-MSG
               WHEN    "3"
                   MOVE    "【無効（新しい資格あり）】"
                                           TO  SPA-P035-YUKO-MSG
               WHEN    "4"
                   MOVE    "【該当資格なし】"
                                           TO  SPA-P035-YUKO-MSG
               WHEN    "5"
                   MOVE    "【複数該当】"  TO  SPA-P035-YUKO-MSG
            END-EVALUATE
            .
       30011-ONSHIKAKU-HEN-EXT.
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
           IF      SPA-P035-BIRTHDAY   =   SPACE
                                       OR  ZERO
                                       OR  ALL "9"
               GO  TO  300111-NENREI-SET-EXT
           END-IF
      *
           MOVE    SPA-P035-BIRTHDAY   TO  WRK-YMDS1
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
           MOVE    WRK-NENREI-G        TO  SPA-P035-NENREI
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
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    SPA-P035-HEN-BIRTHDAYW
                                               TO  SPA-P035-BIRTHDAYW
                       MOVE    SPA-P035-HEN-BIRTHDAY
                                               TO  SPA-P035-BIRTHDAY
                   END-IF
                   MOVE    SPACE           TO  SPA-P035-HEN-BIRTHDAYW
                   MOVE    SPACE           TO  SPA-P035-HEN-BIRTHDAY
                   PERFORM 4101-PTNUM-CHK-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
               WHEN    "0102"
                   MOVE    SPACE           TO  SPA-PIDCD
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 406-P037-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-P035-CUR
                   END-IF
           END-EVALUATE
      *
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者一覧画面より処理
      *****************************************************************
       3036-P036-SYORI-SEC              SECTION.
      *
           IF      SPA-P036-S-PTNUM    =   SPACE
               INITIALIZE                      SPA-P036-AREA
               MOVE    1                   TO  SPA-P035-CUR
           ELSE
               IF      SPA-P036-S-HKNJANUM NOT =   SPACE
                   MOVE    SPA-P036-S-HKNJANUM
                                               TO  SPA-P035-HKNJANUM
                   MOVE    SPA-P036-S-KIGO     TO  SPA-P035-KIGO
                   MOVE    SPA-P036-S-NUM      TO  SPA-P035-NUM
                   MOVE    SPA-P036-S-EDABAN   TO  SPA-P035-EDABAN
      *            保険者番号　検索チェック
                   MOVE    SPA-P035-HKNJANUM   TO  WRK-HKNJANUM
                   PERFORM 300111-HKNJAINF-HEN-SEC
                   MOVE    WRK-HKNJANAME       TO  SPA-P035-HKNJANAME
               END-IF
               MOVE    SPA-P036-S-PTNUM    TO  SPA-P035-PTNUM
               PERFORM 4101-PTNUM-CHK-SEC
               MOVE    12                  TO  SPA-P035-CUR
           END-IF
      *
           .
       3036-P036-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P035
           INITIALIZE                      P035
           MOVE    SPA-P035-HKNJANUM    TO  P035-HKNJANUM
           MOVE    SPA-P035-HKNJANAME   TO  P035-HKNJANAME
           MOVE    SPA-P035-KIGO        TO  P035-KIGO
           MOVE    SPA-P035-NUM         TO  P035-NUM
           MOVE    SPA-P035-EDABAN      TO  P035-EDABAN
      *
           MOVE    SPA-P035-BIRTHDAYW   TO  P035-BIRTHDAY
           MOVE    SPA-P035-NENREI      TO  P035-NENREI
      *
           MOVE    SPA-P035-PTNUM       TO  P035-PTNUM
           MOVE    SPA-P035-NAME        TO  P035-NAME
           MOVE    SPA-P035-KANANAME    TO  P035-KANANAME
           MOVE    SPA-P035-SEX         TO  P035-SEX
      *
           MOVE    SPA-P035-YUKO-MSG    TO  P035-YUKO-MSG
      *R05.4
           MOVE    COMS-MSG1            TO  P035-CHUIMSG1
           MOVE    "red"                TO  P035-CHUIMSG1-STYLE
           MOVE    COMS-MSG2            TO  P035-CHUIMSG2
           MOVE    "red"                TO  P035-CHUIMSG2-STYLE
      *
      *    保険証ＯＣＲ画面表示
           PERFORM 50010-IMAGE-HEN-SEC
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
                  (SPA-P035-CUR        =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-P035-CUR
               WHEN    01
                   MOVE    "BIRTHDAY"    TO  MCP-WIDGET
               WHEN    02
                   MOVE    "HKNJANUM"    TO  MCP-WIDGET
               WHEN    03
                   MOVE    "KIGO"        TO  MCP-WIDGET
               WHEN    04
                   MOVE    "NUM"         TO  MCP-WIDGET
               WHEN    05
                   MOVE    "EDABAN"      TO  MCP-WIDGET
               WHEN    06
                   MOVE    "PTNUM"       TO  MCP-WIDGET
               WHEN    11
                   MOVE    "B04"         TO  MCP-WIDGET
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
               WHEN    "BIRTHDAY"
                   MOVE    2               TO  SPA-P035-CUR
               WHEN    "HKNJANUM"
                   MOVE    3               TO  SPA-P035-CUR
               WHEN    "KIGO"
                   MOVE    4               TO  SPA-P035-CUR
               WHEN    "NUM"
                   MOVE    5               TO  SPA-P035-CUR
               WHEN    "EDABAN"
                   MOVE    6               TO  SPA-P035-CUR
               WHEN    "PTNUM"
                   MOVE    12              TO  SPA-P035-CUR
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
           MOVE    SPA-P035-HKNOCR-FILE
                                     TO  LNK-ONSHIIMG-FILE-NAME
           CALL   "ORCSONSHIIMG"     USING
                                     ORCSONSHIIMGAREA
                                     SPA-AREA
                                     MCPAREA
           IF       LNK-ONSHIIMG-RC    =   1
      *        異常終了
               MOVE    LNK-ONSHIIMG-IMG-MSG    TO  P035-MIDASHI
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
           MOVE    ORCA-BLOB-OBJECT    TO  P035-IMGAREA
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
      *    患者一覧へ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 404-KNJA-LIST-SEC
      *    公費画像確認へ
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 406-P037-CHECK-SEC
      *    選択中を継続
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-P033-SYORI-SEC
      *    資格確認
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 412-ONSHI-KAKUNIN-SEC
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
           MOVE    ZERO                TO  SPA-P035-CUR
      *
      *     EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        リスト選択
      *         WHEN    ANY             ALSO    "DUALLIST"
      *             PERFORM 4001-LIST-SEL-SEC
      *         WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
      *     END-EVALUATE
      *
           .
       200-ENTRY-EXT.
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
           MOVE    P035-HKNJANUM     TO  SPA-P035-HKNJANUM
           MOVE    P035-KIGO         TO  SPA-P035-KIGO
           MOVE    P035-NUM          TO  SPA-P035-NUM
           MOVE    P035-EDABAN       TO  SPA-P035-EDABAN
           MOVE    P035-BIRTHDAY     TO  SPA-P035-BIRTHDAYW
           MOVE    P035-PTNUM        TO  SPA-P035-PTNUM
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
      *    保険者番号
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-HKNJANUM-CHK-SEC
           END-IF
      *
      *    記号
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-KIGO-CHK-SEC
           END-IF
      *    番号
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-NUM-CHK-SEC
           END-IF
      *    枝番
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4101-EDANUM-CHK-SEC
           END-IF
      *    患者番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-PTNUM-CHK-SEC
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
           IF      SPA-P035-BIRTHDAYW      =   SPACE
               MOVE    SPACE               TO  SPA-P035-BIRTHDAY
               GO  TO  4101-BIRTHDAY-CHK-EXT
           END-IF
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P035-BIRTHDAYW  TO  SGDAY-INDAY
           MOVE    SPA-SYSYMD          TO  SGDAY-SYSYMD
           MOVE    "2"                 TO  SGDAY-SYSKBN
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
      *
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-P035-BIRTHDAYW
               MOVE    SGDAY-SDAY          TO  SPA-P035-BIRTHDAY
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P035-CUR
               MOVE    SPACE               TO  SPA-P035-BIRTHDAY
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
      *    システム日付とのチェック
           IF      SPA-P035-BIRTHDAY   >   SPA-SYSYMD
               MOVE    SPACE           TO  SPA-P035-BIRTHDAY
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P035-CUR
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
      *    年齢計算
           PERFORM 300111-NENREI-SET-SEC
           .
       4101-BIRTHDAY-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険者番号チェック処理
      ****************************************************************
       4101-HKNJANUM-CHK-SEC     SECTION.
      *
      *    未入力判定
           IF      SPA-P035-HKNJANUM    =   SPACE
               MOVE    SPACE               TO  SPA-P035-HKNJANAME
               GO      TO      4101-HKNJANUM-CHK-EXT
           END-IF
      *
      *    桁数チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-P035-HKNJANUM   TO  KANACHK-MAE-INPUT
           MOVE    8                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-P035-HKNJANUM
      *    半角で間に空白がないこと
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-RC2     NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   2     )
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-P035-CUR
               GO      TO      4101-HKNJANUM-CHK-EXT
           END-IF
           IF      KANACHK-MAX         =   4  OR  6  OR  8
               MOVE    KANACHK-MAX         TO  WRK-MOJ-MAX
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-P035-CUR
               GO      TO      4101-HKNJANUM-CHK-EXT
           END-IF
      *
      *    保険者番号　検索チェック
           MOVE    SPA-P035-HKNJANUM   TO  WRK-HKNJANUM
           PERFORM 300111-HKNJAINF-HEN-SEC
           MOVE    WRK-HKNJANAME       TO  SPA-P035-HKNJANAME
           .
       4101-HKNJANUM-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    記号　チェック処理
      ****************************************************************
       4101-KIGO-CHK-SEC     SECTION.
      *
           IF      SPA-P035-KIGO    NOT =   SPACE
               MOVE    SPA-P035-KIGO        TO  WRK-MAE-INPUT
               MOVE    40                   TO  WRK-MAX
      *        半角を全角へ変換
               PERFORM 890-HALF-ZENC-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-P035-KIGO
           END-IF
           .
       4101-KIGO-CHK-EXT.
           EXIT.
      ****************************************************************
      *    番号　チェック処理
      ****************************************************************
       4101-NUM-CHK-SEC     SECTION.
      *
           IF      SPA-P035-NUM     NOT =   SPACE
               MOVE    SPA-P035-NUM         TO  WRK-MAE-INPUT
               MOVE    40                   TO  WRK-MAX
      *        半角を全角へ変換
               PERFORM 890-HALF-ZENC-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-P035-NUM
           END-IF
           .
       4101-NUM-CHK-EXT.
           EXIT.
      ****************************************************************
      *    枝番　チェック処理
      ****************************************************************
       4101-EDANUM-CHK-SEC     SECTION.
      *
           IF      SPA-P035-EDABAN     =   SPACE
               GO  TO  4101-EDABAN-CHK-EXT
           END-IF
      *    数値２桁
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-P035-EDABAN     TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )
              OR  (SNUM-MINKBN     NOT =   SPACE )
              OR  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-P035-CUR
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-P035-EDABAN
           END-IF
           .
       4101-EDABAN-CHK-EXT.
           EXIT.
      ****************************************************************
      *    患者番号チェック処理
      ****************************************************************
       4101-PTNUM-CHK-SEC     SECTION.
      *
           IF      SPA-P035-PTNUM      =   SPACE
               MOVE    SPACE           TO  SPA-P035-NAME
               MOVE    SPACE           TO  SPA-P035-KANANAME
               MOVE    SPACE           TO  SPA-P035-SEX
               MOVE    ZERO            TO  SPA-P035-PTID
               MOVE    SPACE           TO  SPA-P035-MAE-PTNUM
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
           IF      SPA-P035-PTNUM      =   SPA-P035-MAE-PTNUM
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-P035-NAME
           MOVE    SPACE           TO  SPA-P035-KANANAME
           MOVE    SPACE           TO  SPA-P035-SEX
           MOVE    ZERO            TO  SPA-P035-PTID
           MOVE    SPACE           TO  SPA-P035-MAE-PTNUM
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-P035-PTNUM      TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-P035-PTNUM
           EVALUATE    SPTNUM-RC
               WHEN    00
      *            患者あり
                   MOVE    SPTNUM-PTID         TO  SPA-P035-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPA-P035-PTNUM
               WHEN    OTHER
                   MOVE    "0008"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-P035-CUR
                   GO      TO      4101-PTNUM-CHK-EXT
           END-EVALUATE
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-P035-PTID       TO  PTINF-PTID
           PERFORM 9001-PTINF-KEYREAD-SEC
           IF     (FLG-PTINF           =   1     )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-P035-CUR
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P035-HEN-BIRTHDAYW
           MOVE    PTINF-BIRTHDAY      TO  SPA-P035-HEN-BIRTHDAY
      *    生年月日の判定
           IF      SPA-P035-BIRTHDAY   NOT =   PTINF-BIRTHDAY
      *         画面が未入力
                IF      SPA-P035-BIRTHDAY  =   SPACE
                                           OR  ZERO
                                           OR  ALL "9"
      *             画面に生年月日複写
                    MOVE    "0101"             TO  SPA-PIDCD
                ELSE
                    MOVE    "0009"             TO  SPA-ERRCD
                    MOVE    6                  TO  SPA-P035-CUR
                END-IF
               GO      TO      4101-PTNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-P035-PTNUM      TO  SPA-P035-MAE-PTNUM
           MOVE    PTINF-NAME          TO  SPA-P035-NAME
           MOVE    PTINF-KANANAME      TO  SPA-P035-KANANAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"        TO  SPA-P035-SEX
               WHEN    "2"
                   MOVE    "女"        TO  SPA-P035-SEX
           END-EVALUATE
           .
       4101-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
      *    オンライン資格確認編集
           PERFORM 3001-ONSHIKAKU-SYORI-SEC
      *
           MOVE    1                   TO  SPA-P035-CUR
           .
       402-CLEAR-EXT.
           EXIT.
      *
      * 
      *****************************************************************
      *    患者一覧へ
      *****************************************************************
       404-KNJA-LIST-SEC             SECTION.
      *
           IF      SPA-P035-BIRTHDAY   =   SPACE
                                       OR  ZERO
                                       OR  ALL "9"
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P035-CUR
               GO      TO      404-KNJA-LIST-EXT
           END-IF
      *    初期画面編集
           INITIALIZE                      SPA-P036-AREA
           MOVE    "P035"              TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP036"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           IF      SPA-P036-MAX        =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P036-CUR
               GO      TO  404-KNJA-LIST-EXT
           END-IF
      *
           MOVE    "P035"              TO  SPA-MOTOPG
           MOVE    "P036"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P036"              TO  MCP-WINDOW
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       404-KNJA-LIST-EXT.
           EXIT.
      *****************************************************************
      *    資格確認再確認処理
      *****************************************************************
       412-ONSHI-KAKUNIN-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
               OR (SPA-PIDCD       NOT =   SPACE )
               GO  TO  412-ONSHI-KAKUNIN-EXT
           END-IF
      *
      *    更新前チェック
           IF      SPA-P035-BIRTHDAYW      =   SPACE
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P035-CUR
               GO  TO  412-ONSHI-KAKUNIN-EXT
           END-IF
           IF    ( SPA-P035-HKNJANUM       =   SPACE )
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-P035-CUR
               GO  TO  412-ONSHI-KAKUNIN-EXT
           END-IF
           IF      (SPA-P035-KIGO           =   SPACE)
             AND   (SPA-P035-NUM            =   SPACE)
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-P035-CUR
               GO  TO  412-ONSHI-KAKUNIN-EXT
           END-IF
      *
      *    処理中選択解除
      **     PERFORM 412012-ONSHIKAKU-MODESET-SEC
      *     IF      SPA-ERRCD       NOT =   SPACE
      *         GO  TO  412-ONSHI-KAKUNIN-EXT
      *     END-IF
      *
      *
           INITIALIZE                  ORCSONSHI001AREA
           MOVE    SPA-P035-PTID       TO  LNK-ONSHI001-PTID
           MOVE    SPA-P035-HKNJANUM   TO  LNK-ONSHI001-OCR-HKNJANUM
           MOVE    SPA-P035-KIGO       TO  LNK-ONSHI001-OCR-KIGO
           MOVE    SPA-P035-NUM        TO  LNK-ONSHI001-OCR-NUM
           MOVE    SPA-P035-EDABAN     TO  LNK-ONSHI001-OCR-EDABAN
           MOVE    SPA-P035-BIRTHDAY   TO  LNK-ONSHI001-OCR-BIRTHDAY
      *
           MOVE    "05"                TO  LNK-ONSHI001-SYOKBN
           MOVE    "12"                TO  LNK-ONSHI001-GYOUMUCD
           MOVE    SPA-P035-SEL-UUID   TO  LNK-ONSHI001-TBL-UUID
           CALL    "ORCSONSHI001"      USING   ORCSONSHI001AREA
                                               SPA-AREA
           IF       LNK-ONSHI001-RC    =   9
      *        異常終了
               MOVE    "1011"              TO  SPA-ERRCD
               GO  TO  412-ONSHI-KAKUNIN-EXT
           END-IF
      *
      *
      *    画像削除
           PERFORM 412013-IMAGE-DEL-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  412-ONSHI-KAKUNIN-EXT
           END-IF
      *
           PERFORM 210-BACK
            .
       412-ONSHI-KAKUNIN-EXT.
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
           MOVE    SPA-P035-HKNOCR-FILE
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
      *    処理中フラグ設定処理
      *****************************************************************
       412012-ONSHIKAKU-MODESET-SEC        SECTION.
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P035-SEL-UUID   TO  ONSKAKU-TBL-UUID
           PERFORM 9002-ONSKAKU-KEYREAD-SEC
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      FLG-ONSKAKU         =   ZERO
               PERFORM 49901-ONSKAKU-MODEUPDATE-SEC
           END-IF
           .
       412012-ONSHIKAKU-MODESET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    オンライン資格確認解除処理
      *****************************************************************
       49901-ONSKAKU-MODEUPDATE-SEC         SECTION.
      *
      *    処理中解除
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
               DISPLAY "P033 ONSKAKU KEY UPD ERR:"  MCP-RC
                   ",KEY:" ONSKAKU-KEY
           END-IF
           .
       49901-ONSKAKU-MODEUPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択中を継続して元画面へ処理
      *****************************************************************
       410-P033-SYORI-SEC                    SECTION.
      *
           IF      SPA-P035-SHOKAI-KBN     =   "3"
      *        保険証OCRがエラーは遷移不可
               MOVE    "0021"              TO  SPA-ERRCD
               GO  TO  410-P033-SYORI-EXT
           END-IF
      *
      *    初期表示と一致しない場合のみ
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    SPA-P035-SEL-UUID   TO  ONSKAKU-TBL-UUID
           PERFORM 9002-ONSKAKU-KEYREAD-SEC
           IF     (ONSKAKU-SHO-HKNJANUM    =   SPA-P035-HKNJANUM)
              AND (ONSKAKU-SHO-KIGO        =   SPA-P035-KIGO    )
              AND (ONSKAKU-SHO-NUM         =   SPA-P035-NUM     )
              AND (ONSKAKU-SHO-EDABAN      =   SPA-P035-EDABAN  )
              AND (ONSKAKU-SHO-BIRTHDAY    =   SPA-P035-BIRTHDAY  )
               CONTINUE
           ELSE
               MOVE    "0022"              TO  SPA-ERRCD
               GO  TO  410-P033-SYORI-EXT
           END-IF
      *
           MOVE    "1"                 TO  SPA-P035-NEXTFLG
           PERFORM 210-BACK
      *
           .
       410-P033-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    オン資格確認　公費画像確認処理
      *****************************************************************
       406-P037-CHECK-SEC                    SECTION.
      *
           IF      SPA-P035-SHOKAI-KBN     =   "3"
      *        保険証OCRがエラーは遷移不可
               MOVE    "0021"              TO  SPA-ERRCD
               GO  TO  406-P037-CHECK-EXT
           END-IF
      **** MOVE     "0102"             TO  SPA-PIDCD
      *R5.3
           PERFORM 406-P037-SYORI-SEC
      *
           .
       406-P037-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    オン資格確認　公費画像確認処理
      *****************************************************************
       406-P037-SYORI-SEC                    SECTION.
      *
           INITIALIZE                  SPA-P037-AREA
      *
           MOVE    "P035"              TO  SPA-MOTOPG
           MOVE    "P035"              TO  SPA-P037-MOTOPG
           MOVE    "P037"              TO  SPA-SAKIPG
           MOVE    SPA-P035-SEL-UUID   TO  SPA-P037-HKN-TBL-UUID
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP037"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           IF      SPA-P037-LSTMAX     =   ZERO
      *        対象なし
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P035-CUR
               GO  TO  406-P037-SYORI-EXT
           END-IF
      *
           MOVE    "B01"               TO  MCP-WIDGET
      * 
           MOVE    "P035"              TO  SPA-MOTOPG
           MOVE    "P037"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P037"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       406-P037-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P035"              TO  SPA-MOTOPG
           MOVE    SPA-P035-MOTOPG     TO  SPA-SAKIPG
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
           MOVE    "P035"              TO  MCP-WINDOW
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
                   STRING  "保険者番号桁数エラーです"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "保険の記号は全角で入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   MOVE    "保険の番号は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   STRING  "枝番は数値で入力して下さい。"
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0008"
                   STRING  "患者番号の登録がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0009"
                   STRING  "患者番号で登録されている生年月日は"
                           SPA-P035-HEN-BIRTHDAYW
                           "です。画面の生年月日と一致しません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0010"
                   STRING  "生年月日を入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "保険者番号、記号、番号に未入力が"
                           "あります。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0013"
                   STRING  "生年月日での患者登録はありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
      *2022/12
                 WHEN    "0020"
                   STRING  "公費医療券画像情報は存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
                 WHEN    "0021"
                   STRING  "保険証ＯＣＲの不正データです。"
                           "資格確認を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
                 WHEN    "0022"
                   STRING  "入力がありました。"
                           "資格確認を行って下さい。"
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
           MOVE    "P035"              TO  SPA-MOTOPG
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
       520-PIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PIDMSG
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   STRING  "患者番号で登録されている生年月日："
                           SPA-P035-HEN-BIRTHDAYW
                           "に変更します。よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "保険証の生年月日で照会済みであることを"
                           "確認して下さい。"
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
           MOVE    "P035"              TO  SPA-MOTOPG
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
