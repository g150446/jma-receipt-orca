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
       PROGRAM-ID.             ORCGMPDEL.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : 患者削除情報一覧（ＭＰＤＥＬ）
      *  管理者            : 
      *  21/02/10    NMED−多々納     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     ORCAMO       22/10/XX  オペレーターＩＤ追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *
      *    スパ領域
       01  SPA-MPDEL.
           03  SPA-GMN-CUR             PIC 9(04).
           03  SPA-GMN-PAGE            PIC 9(03).
           03  SPA-GMN-NEXT            PIC 9(01).
           03  SPA-GMN-SYORI           PIC 9(01).
           03  SPA-GMN-MIDASI          PIC X(50).
      *
           03  SPA-GMN-MEISAI-G.
               05  SPA-GMN-MEISAI-OCC      OCCURS   100.
                   07  SPA-GMN-MODE        PIC X(50).
                   07  SPA-GMN-DELDATE     PIC X(10).
                   07  SPA-GMN-DELTIME     PIC X(10).
                   07  SPA-GMN-PTNUM       PIC X(20).
                   07  SPA-GMN-BIRTHDAY    PIC X(10).
                   07  SPA-GMN-SEX         PIC X(02).
                   07  SPA-GMN-NAME        PIC X(200).
                   07  SPA-GMN-SRYYMD      PIC X(10).
                   07  SPA-GMN-SRYKA       PIC X(02).
                   07  SPA-GMN-DRCD        PIC X(10).
                   07  SPA-GMN-HKNCOMBI    PIC X(04).
                   07  SPA-GMN-DENPNUM     PIC X(10).
                   07  SPA-GMN-OPID        PIC X(64).
               05  SPA-GMN-MAX             PIC 9(04).
      *
           03  SPA-GMN-SELPTNUM       PIC X(20).
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PUSHINFO        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PDEL            PIC 9(01).
      *
      *    添え字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-WSYSYMDH          PIC X(09).
      *
           03  WRK-WSYSYMD.
               05  WRK-WSGO         PIC X(01).
               05  WRK-WSYY         PIC 9(02).
               05  WRK-WSMM         PIC 9(02).
               05  WRK-WSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-PTNUMMSG        PIC X(04).
      *
           03  WRK-PUSHINFO.
               05  WRK-PUSH-HKNCOMBI       PIC X(04).
               05  WRK-FILLER1             PIC X(01).
               05  WRK-PUSH-SRYKA          PIC X(02).
               05  WRK-FILLER2             PIC X(01).
               05  WRK-PUSH-DRCD           PIC X(05).
               05  WRK-FILLER3             PIC X(01).
               05  WRK-PUSH-DENPNUM        PIC X(07).
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG                  PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO                  PIC X(01).
               05  WRK-HYY                  PIC Z9.
               05  FILLER                   PIC X(01)   VALUE  ".".
               05  WRK-HMM                  PIC Z9.
               05  FILLER                   PIC X(01)   VALUE  ".".
               05  WRK-HDD                  PIC Z9.
      *
           03  WRK-HENYMDN.
               05  WRK-HENNYMD              PIC X(22).
               05  FILLER                   PIC X(02)   VALUE  "（".
               05  WRK-HENNYOBI             PIC X(02).
               05  FILLER                   PIC X(02)   VALUE  "）".
      *
           03  WRK-HENTIME.
               05  WRK-THH                  PIC 99.
               05  FILLER                   PIC X(01)   VALUE  ":".
               05  WRK-TMM                  PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    システム管理マスタ領域
      *    職員情報
           COPY  "CPSK1010.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    削除ログ
       01  PUSHINFO-REC.
           COPY    "CPPUSH-INFO.INC".
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
           COPY    "CPORCSPTNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
           COPY    "MPDEL.INC".
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-FREE        TO  SPA-MPDEL
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
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-MPDEL       TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "MPDEL"              TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-MPDEL
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
                   PERFORM 3001-MID1-SYORI-SEC
      *
               WHEN    OTHER
                   MOVE    SPACE           TO  SPA-MIDCD
                   PERFORM 310-SPASET-SEC
                   MOVE    1               TO  SPA-GMN-PAGE
                   MOVE    1               TO  SPA-GMN-CUR
           END-EVALUATE
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3001-MID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-MIDCD
               WHEN    "0001"
                   MOVE    1               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   MOVE    SPACE           TO  SPA-MIDCD
           END-EVALUATE
           .
       3001-MID1-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
           MOVE    SPACE           TO  SPA-MIDCD
      *
           INITIALIZE                  SPA-MPDEL
           MOVE    1                   TO  SPA-GMN-SYORI
      *    画面編集
           PERFORM 3101-HENSYU-SYORI-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       3101-HENSYU-SYORI-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-SYORI
           WHEN    1
               MOVE   "【全削除情報】"         TO  SPA-GMN-MIDASI
           WHEN    2
               MOVE   "【患者削除】"           TO  SPA-GMN-MIDASI
           WHEN    3
               MOVE   "【外来診療削除】"       TO  SPA-GMN-MIDASI
           END-EVALUATE
      *
      *
           INITIALIZE                  SPA-GMN-MEISAI-G
           MOVE    SPACE               TO  PUSHINFO-REC
           INITIALIZE                      PUSHINFO-REC
           MOVE    SPA-HOSPNUM         TO  PUSH-HOSPNUM
           MOVE    "delete"            TO  PUSH-PATIENT-MODE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           EVALUATE    SPA-GMN-SYORI
           WHEN    2
               MOVE    "patient_infomation"    TO  PUSH-EVENT
               MOVE    "key4"              TO  WRK-MCP-PATHNAME
           WHEN    3
               MOVE    "patient_account"       TO  PUSH-EVENT
               MOVE    "key4"              TO  WRK-MCP-PATHNAME
           END-EVALUATE
      *    患者番号指定は全削除
           IF      SPA-GMN-SELPTNUM    NOT =   SPACE
               MOVE    SPA-GMN-SELPTNUM    TO  PUSH-PTNUM
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PUSHINFO-REC        TO  MCPDATA-REC
           MOVE    "tbl_push_info"     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      FLG-PUSHINFO        =   ZERO
               PERFORM 900-PUSHINFO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PUSHINFO
           END-IF
           MOVE    ZERO                TO  IDX-STR
           MOVE    ZERO                TO  SPA-GMN-NEXT
           IF      SPA-GMN-PAGE       =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
           COMPUTE IDX-STR             =  (SPA-GMN-PAGE   -  1)
                                       *   100
                                       +   1
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           PERFORM UNTIL     (FLG-PUSHINFO     =   1   )
                       OR    (SPA-GMN-NEXT     =   1   )
               ADD     1                   TO  IDY
               IF      IDY                 <   IDX-STR
                   CONTINUE
               ELSE
                   PERFORM 3101-PUSHIINFO-HEN-SEC
               END-IF
               MOVE    "tbl_push_info"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PUSHINFO-READ-SEC
           END-PERFORM
           MOVE    "tbl_push_info"     TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       3101-PUSHIINFO-HEN-SEC       SECTION.
      *
           ADD     1                   TO  IDX
           IF      IDX                 >   100
               MOVE    1               TO  SPA-GMN-NEXT
               GO      TO      3101-PUSHIINFO-HEN-EXT
           END-IF
      *
           MOVE    IDX                 TO  SPA-GMN-MAX
           MOVE    ZERO                TO  FLG-PDEL
           EVALUATE    PUSH-EVENT
               WHEN    "patient_infomation"
                   MOVE    "患者削除"          TO  SPA-GMN-MODE
                                                             (IDX)
                   MOVE    1                   TO  FLG-PDEL
               WHEN    "patient_account"
                   MOVE    "外来診療削除"      TO  SPA-GMN-MODE
                                                             (IDX)
           END-EVALUATE
      *    削除日
           MOVE    PUSH-INFORMATION-DATE   TO  SPA-GMN-DELDATE(IDX)
           MOVE    PUSH-INFORMATION-TIME   TO  SPA-GMN-DELTIME(IDX)
      *    OPID
           MOVE    PUSH-OPID           TO  SPA-GMN-OPID (IDX)
      *
           MOVE    PUSH-PTNUM          TO  SPA-GMN-PTNUM (IDX)
           IF    ( FLG-PDEL            =   1  )
             OR  ( PUSH-PTID           =   ZERO)
      *        患者漢字氏名
               MOVE    PUSH-PUSHINFO       TO  SPA-GMN-NAME (IDX)
           ELSE
      *        診療行為削除
               PERFORM 31011-PTINF-HEN-SEC
           END-IF
           .
       3101-PUSHIINFO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為削除編集処理
      *****************************************************************
       31011-PTINF-HEN-SEC       SECTION.
      *
      *    患者IDと患者番号の判定
           MOVE    SPACE               TO  WRK-PTNUMMSG
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    PUSH-PTNUM          TO  PTNUM-PTNUM
           PERFORM 910-PTNUM-READ-SEC
           IF      FLG-PTNUM           =   ZERO
      *        一致しない場合
               IF      PTNUM-PTID      NOT =   PUSH-PTID
                   MOVE    "※"            TO  WRK-PTNUMMSG
               END-IF
           ELSE
               MOVE    "●"                TO  WRK-PTNUMMSG
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PUSH-PTID           TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   ZERO
               STRING  WRK-PTNUMMSG
                       PTINF-NAME          DELIMITED  BY  SPACE
                                       INTO    SPA-GMN-NAME (IDX)
               END-STRING
      *
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 500-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY(IDX)
               EVALUATE    PTINF-SEX
               WHEN    "1"
                    MOVE    "男"                TO  SPA-GMN-SEX(IDX)
                WHEN    "2"
                    MOVE    "女"                TO  SPA-GMN-SEX(IDX)
               END-EVALUATE
           END-IF
      *    診療日付
           MOVE    PUSH-PERFORM-DATE   TO  SPA-GMN-SRYYMD (IDX)
      *
      *    診療行為削除内容変換
           MOVE    PUSH-PUSHINFO       TO  WRK-PUSHINFO
      *    保険組合せ番号
           MOVE    WRK-PUSH-HKNCOMBI   TO  SPA-GMN-HKNCOMBI(IDX)
      *    診療科
           MOVE    WRK-PUSH-SRYKA      TO  SPA-GMN-SRYKA   (IDX)
      *    ドクター
           MOVE    WRK-PUSH-DRCD       TO  SPA-GMN-DRCD    (IDX)
      *    伝票番号
           MOVE    WRK-PUSH-DENPNUM    TO  SPA-GMN-DENPNUM (IDX)
           .
       31011-PTINF-HEN-EXT.
           EXIT.
      *
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
      *    表示切替
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 410-LISTCHNG-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 406-BACKPAGE-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 407-NEXTPAGE-SEC
      *    選択患者番号へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 405-SELNUM-IN-SEC
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        リスト選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 4001-LIST-SEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
          .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4001-LIST-SEL-SEC          SECTION.
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
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4102-SELPTNUM-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    MPDEL-SELPTNUM      TO  SPA-GMN-SELPTNUM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       4102-SELPTNUM-CHK-SEC                 SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *    患者番号変換処理
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-SELPTNUM    TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-SELPTNUM
      *
           MOVE    1                   TO  SPA-GMN-SYORI
           MOVE    ZERO                TO  SPA-GMN-PAGE
           PERFORM 3101-HENSYU-SYORI-SEC
      *
           .
       4102-SELPTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示切替処理
      *****************************************************************
       410-LISTCHNG-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-SYORI
               WHEN    1
                   MOVE    2               TO  SPA-GMN-SYORI
               WHEN    2
                   MOVE    3               TO  SPA-GMN-SYORI
               WHEN    3
                   MOVE    1               TO  SPA-GMN-SYORI
           END-EVALUATE
      *
           IF      SPA-GMN-SELPTNUM    NOT =   SPACE
               MOVE    1               TO  SPA-GMN-SYORI
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    SPACE               TO  SPA-GMN-SELPTNUM
      *    画面編集
           PERFORM 3101-HENSYU-SYORI-SEC
           .
       410-LISTCHNG-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       406-BACKPAGE-SEC            SECTION.
      *
           IF      SPA-GMN-PAGE        >   1
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE
                                       -   1
      *        画面編集
               PERFORM 3101-HENSYU-SYORI-SEC
           ELSE
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
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
           IF      SPA-GMN-NEXT       =   1
               ADD     1               TO  SPA-GMN-PAGE
      *        画面編集
               PERFORM 3101-HENSYU-SYORI-SEC
           ELSE
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       407-NEXTPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択患者番号へ
      *****************************************************************
       405-SELNUM-IN-SEC               SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
           .
       405-SELNUM-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC               SECTION.
      *
           PERFORM 310-SPASET-SEC
           .
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "MPDEL"             TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-MIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "MPDEL"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  MPDEL
           INITIALIZE                      MPDEL
      *
           MOVE    SPA-GMN-MIDASI      TO  MPDEL-MIDASI
           MOVE    SPA-GMN-SELPTNUM    TO  MPDEL-SELPTNUM
      *
           PERFORM   VARYING    IDX    FROM    1   BY  1
                     UNTIL      IDX    >    SPA-GMN-MAX
               MOVE    SPA-GMN-MODE   (IDX)    TO  MPDEL-MODE (IDX)
               MOVE    SPA-GMN-DELDATE(IDX)    TO  MPDEL-DELYMD (IDX)
               MOVE    SPA-GMN-DELTIME(IDX)    TO  MPDEL-DELTIME(IDX)
               MOVE    SPA-GMN-PTNUM (IDX)     TO  MPDEL-PTNUM (IDX)
               MOVE    SPA-GMN-BIRTHDAY (IDX)  TO  MPDEL-BIRTHDAY (IDX)
               MOVE    SPA-GMN-SEX      (IDX)  TO  MPDEL-SEX      (IDX)
               MOVE    SPA-GMN-NAME     (IDX)  TO  MPDEL-NAME     (IDX)
               MOVE    SPA-GMN-SRYYMD   (IDX)  TO  MPDEL-SRYYMD   (IDX)
               MOVE    SPA-GMN-SRYKA    (IDX)  TO  MPDEL-SRYKA   (IDX)
               MOVE    SPA-GMN-DRCD     (IDX)  TO  MPDEL-DRCD    (IDX)
               MOVE    SPA-GMN-HKNCOMBI (IDX)  TO  MPDEL-HKNCOMBI (IDX)
               MOVE    SPA-GMN-DENPNUM  (IDX)  TO  MPDEL-DENPNUM  (IDX)
               MOVE    SPA-GMN-OPID     (IDX)  TO  MPDEL-OPID     (IDX)
           END-PERFORM
           MOVE    SPA-GMN-MAX        TO  MPDEL-LIST-COUNT
      *
           IF      SPA-GMN-NEXT       =   1
               IF      SPA-GMN-PAGE      >   1
                   MOVE    "前次頁あり"  TO  MPDEL-NEXTMSG-TEXTDATA
                   MOVE    "red"         TO  MPDEL-NEXTMSG-STYLE
               ELSE
                   MOVE    "次頁あり"    TO  MPDEL-NEXTMSG-TEXTDATA
                   MOVE    "red"         TO  MPDEL-NEXTMSG-STYLE
               END-IF
           ELSE
               IF      SPA-GMN-PAGE      >   1
                   MOVE    "前頁あり"    TO  MPDEL-NEXTMSG-TEXTDATA
                   MOVE    "red"         TO  MPDEL-NEXTMSG-STYLE
               END-IF
           END-IF
      *
           PERFORM 510-GSRAUTH-SEC
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
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "B04"           TO  MCP-WIDGET
               WHEN    2
                   MOVE  "SELPTNUM"      TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
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
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "前頁はありません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "次頁はありません"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  MERR
           MOVE    SPA-ERRCD            TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "MPDEL"              TO  SPA-MOTOPG
           MOVE    "MERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
      *    EVALUATE    SPA-MIDCD
      *        WHEN    "0001"
      *            MOVE    SPACE
      *                                    TO  WRK-MIDMSG
      *        WHEN    OTHER
      *            MOVE    SPA-MIDCD       TO  WRK-MIDMSG
      *    END-EVALUATE
      *
           MOVE    SPACE               TO  MID1
           INITIALIZE                      MID1
           MOVE    SPA-MIDCD           TO  MID1-ID1CODE
      **   MOVE    WRK-MIDMSG          TO  MID1-ID1MSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "MPDEL"             TO  SPA-MOTOPG
           MOVE    "MID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "MID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       500-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       500-SEIWA-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    削除ログ読み込み
      *****************************************************************
       900-PUSHINFO-READ-SEC           SECTION.
      *
           MOVE    "tbl_push_info"     TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PUSHINFO
               MOVE    MCPDATA-REC         TO  PUSHINFO-REC
           ELSE
               MOVE    1                   TO  FLG-PUSHINFO
               INITIALIZE                      PUSHINFO-REC
           END-IF
           .
       900-PUSHINFO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
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
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
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
      *    患者番号マスター読込処理
      *****************************************************************
       910-PTNUM-READ-SEC         SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
                   MOVE    ZERO                TO  FLG-PTNUM
               ELSE
                   MOVE    1                   TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
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
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
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
           MOVE    "PUTWINDOW"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
