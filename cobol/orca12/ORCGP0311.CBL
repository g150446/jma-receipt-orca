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
       PROGRAM-ID.             ORCGP0311.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : オン資格　同意一括照会画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  24/12/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
           03  FLG-KOUSIN          PIC 9(01).
           03  FLG-JOBKANRI        PIC 9(01).
           03  FLG-CHGARI          PIC 9(01).
           03  FLG-JOBKANRI-OK     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PIDMSG          PIC X(100).
      *
           03  WRK-STRYMD          PIC X(10).
           03  WRK-ENDYMD          PIC X(10).
      *
           03  WRK-WAYMD.
               05  WRK-WAGO         PIC 9(01).
               05  WRK-WAYY         PIC 9(02).
               05  WRK-WAMM         PIC 9(02).
               05  WRK-WADD         PIC 9(02).
      *
           03  WRK-SEYMD.
               05  WRK-SEYY         PIC 9(04).
               05  WRK-SEMM         PIC 9(02).
               05  WRK-SEDD         PIC 9(02).
      *
          03  WRK-MCP-WIDGET        PIC X(20).
      *
          03  WRK-JOBERRMSG         PIC X(50).
      *
          03  WRK-IN-TIME.
              05  WRK-I-HH          PIC 9(02).
              05  WRK-I-MM          PIC 9(02).
              05  WRK-I-SS          PIC 9(04).
          03  WRK-JOB-TIME.
              05  WRK-J-HH          PIC 9(02).
              05  WRK-J-MM          PIC 9(02).
              05  WRK-J-SS          PIC 9(04).
          03  WRK-CHK-TIME.
              05  WRK-C-HH          PIC 9(02).
              05  WRK-C-MM          PIC 9(02).
              05  WRK-C-SS          PIC 9(04).
      *PUSH通信用
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THHH             PIC X(02).
               05  WRK-THMM             PIC X(02).
               05  WRK-THSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
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
      * 訪問診療
       01  CONS-SHELLID-H          PIC X(08)   VALUE   "P02V001".
      * オンライン診療
       01  CONS-SHELLID-O          PIC X(08)   VALUE   "P02O001".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    オン資格照会
           COPY    "CPORCSONSHI001.INC".
      *    PUSH
      *     COPY    "CPPUSHONLINEQUA.INC".
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
           INITIALIZE                  STS-AREA
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
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE           TO  SPA-MOTOPG
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
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "P0311"              TO  MCP-WINDOW
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
           EVALUATE    SPA-MOTOPG
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
                   MOVE    SPACE           TO  SPA-PIDCD
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
      *        一括照会処理
               WHEN    "0101"
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 40091-SYOKAI-CALLSYORI-SEC
                   ELSE
                       MOVE    SPACE          TO  SPA-PIDCD
                   END-IF
           END-EVALUATE
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    診療日付をマシン日付
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  SPA-P0311-SYSYMD
      *
           INITIALIZE                  SPA-P0311-SYUTOKUKBN-COMB
           MOVE    ZERO                TO  IDX
           IF     (SPA-ONSHI-HOUMON    =   1  )
              ADD     1                    TO  IDX
              MOVE    "訪問診療"           TO  SPA-P0311-SYUTOKUKBN-L-M
                                                     (IDX)
              MOVE    "1"                  TO  SPA-P0311-SYUTOKUKBN-L-K
                                                     (IDX)
           END-IF
      ****
           IF     (SPA-ONSHI-ONLINE    =   1  )
              ADD     1                    TO  IDX
              MOVE    "オンライン診療"     TO  SPA-P0311-SYUTOKUKBN-L-M
                                                     (IDX)
              MOVE    "2"                  TO  SPA-P0311-SYUTOKUKBN-L-K
                                                     (IDX)
           END-IF
      ****
           MOVE    IDX                 TO  SPA-P0311-SYUTOKUKBN-MAX
      *    １件目を初期表示
           MOVE    1                   TO  IDX
           MOVE    SPA-P0311-SYUTOKUKBN-L-M (IDX)
                                       TO  SPA-P0311-SYUTOKUKBN
           MOVE    SPA-P0311-SYUTOKUKBN-L-K (IDX)
                                       TO  SPA-P0311-SYUTOKUKBN-K
      *    初期表示
           PERFORM 3101-JOBOKANRI-INT-SEC
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理初期表示処理
      *****************************************************************
       3101-JOBOKANRI-INT-SEC              SECTION.
      *
           INITIALIZE                  SPA-P0311-SYORILST-G
           MOVE    ZERO                TO  SPA-P0311-LIST-MAX
           INITIALIZE                  SPA-P0311-GMN-AREA
           MOVE    ZERO                TO  SPA-P0311-JOBID
           MOVE    SPACE               TO  SPA-P0311-SHELLID
      *
           INITIALIZE                  JOBKANRI-REC
           EVALUATE    SPA-P0311-SYUTOKUKBN-K
      *        訪問診療
               WHEN    "1"
               MOVE    CONS-SHELLID-H      TO  JOB-SHELLID
      *        オンライン診療
               WHEN    "2"
               MOVE    CONS-SHELLID-O      TO  JOB-SHELLID
           END-EVALUATE
           MOVE    JOB-SHELLID         TO  SPA-P0311-SHELLID
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
      *
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
           END-IF
           MOVE    ZERO                TO  FLG-JOBKANRI-OK
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      ( FLG-JOBKANRI   =   1  )
                          OR  ( IDX            >=  10 )
      *        診療日判定
               IF      JOB-SRYYMD      =    SPA-P0311-SYSYMD
      *            最初の履歴
                   IF      FLG-JOBKANRI-OK     =   ZERO
                       PERFORM 31011-JOBKANRI-CHK-SEC
                       MOVE    1               TO  FLG-JOBKANRI-OK
                   END-IF
               END-IF
      *        履歴一覧
               ADD     1               TO  IDX
               MOVE    IDX                 TO  SPA-P0311-RENNUM (IDX)
      *
               MOVE    JOB-SRYYMD          TO  SPA-P0311-TSRYYMD (IDX)
               MOVE    JOB-SRYYMD          TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P0311-TSRYYMDG (IDX)
      *        ジョブ開始年月日
               MOVE    JOB-STARTYMD        TO  SPA-P0311-TSTARTYMD (IDX)
               MOVE    JOB-STARTYMD        TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  WRK-STRYMD
      *        ジョブ終了年月日
               MOVE    JOB-ENDYMD          TO  SPA-P0311-TENDYMD (IDX)
               MOVE    JOB-ENDYMD          TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  WRK-ENDYMD
               STRING  WRK-STRYMD
                       "〜"
                       WRK-ENDYMD      DELIMITED   BY  SIZE
                                       INTO  SPA-P0311-SYORIKIKAN(IDX)
               END-STRING
      *        エラーコード
      **       MOVE    JOB-ERRCD       TO  SPA-P0311-ERRCD (IDX)
               PERFORM 31012-JOBERRCD-MSG-SEC
               MOVE    WRK-JOBERRMSG   TO  SPA-P0311-ERRCDMSG (IDX)
      *        ジョブ開始時間
               MOVE    SPACE               TO  WRK-SYMD
               MOVE    JOB-STARTTIME(1:6)  TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-TIME        TO  SPA-P0311-STARTTIMEG
                                                               (IDX)
      *
      *
               MOVE    IDX             TO  SPA-P0311-LIST-MAX
      *
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-P0311-JOBCHK    =   SPACE
      *        初期表示
      *      訪問診療
             IF      SPA-P0311-SYUTOKUKBN-K  =   "1"
      *        システム日付１月前を開始日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-P0311-SYSYMD    TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KOYOMI     TO  SPA-P0311-STRYMD
               MOVE    SPA-P0311-STRYMD    TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P0311-STRYMDG
      *        終了日
               MOVE    SPA-P0311-SYSYMD    TO  SPA-P0311-ENDYMD
               MOVE    SPA-P0311-ENDYMD    TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P0311-ENDYMDG
             ELSE
      *      オンライン診療
      *        システム日付１週間を開始日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-P0311-SYSYMD    TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -6                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      *test
      *     DISPLAY "P0311 SPA-P0311-SYSYMD:"  SPA-P0311-SYSYMD
      *     ",LNK-DAY6-KOYOMI:"  LNK-DAY6-KOYOMI
      *     ",LNK-DAY6-KEISAN:"  LNK-DAY6-KEISAN
      *TEST
               MOVE    LNK-DAY6-KEISAN     TO  SPA-P0311-STRYMD
               MOVE    SPA-P0311-STRYMD    TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P0311-STRYMDG
      *        終了日
               MOVE    SPA-P0311-SYSYMD    TO  SPA-P0311-ENDYMD
               MOVE    SPA-P0311-ENDYMD    TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P0311-ENDYMDG
             END-IF
           END-IF
           IF      SPA-P0311-JOBCHK    =   SPACE
                                       OR  "4"
               MOVE    2               TO  SPA-P0311-CUR
           ELSE
               MOVE    10              TO  SPA-P0311-CUR 
           END-IF
           .
       3101-JOBOKANRI-INT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理状態チェック処理
      *****************************************************************
       31012-JOBERRCD-MSG-SEC              SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    SPACE
                   MOVE    "受付未"        TO  WRK-JOBERRMSG
               WHEN    "0001"
                   MOVE    "受付前"        TO  WRK-JOBERRMSG
               WHEN    "0002"
                   MOVE    "受付完了"      TO  WRK-JOBERRMSG
               WHEN    "0003"
                   MOVE    "確認完了"      TO  WRK-JOBERRMSG
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-JOBERRMSG
           END-EVALUATE
           .
       31012-JOBERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理状態チェック処理
      *****************************************************************
       31011-JOBKANRI-CHK-SEC              SECTION.
      *
           PERFORM 31012-JOBERRCD-MSG-SEC
           MOVE    SPACE               TO  SPA-P0311-JOBCHK
           MOVE    SPACE               TO  SPA-P0311-JOBERRMSG
           MOVE    ZERO                TO  SPA-P0311-JOBID
      *    エラーコード
           EVALUATE    JOB-ERRCD
               WHEN    SPACE
      *            API実施なし
                   MOVE    "1"             TO  SPA-P0311-JOBCHK
               WHEN    "0001"
      *            API実施なし
                   MOVE    "2"             TO  SPA-P0311-JOBCHK
               WHEN    "0002"
      *            API実施なし
                   MOVE    "3"             TO  SPA-P0311-JOBCHK
               WHEN    "0003"
      *            資格結果受け取り
                   MOVE    "4"             TO  SPA-P0311-JOBCHK
           END-EVALUATE
           IF      SPA-P0311-JOBCHK    =   SPACE
               GO  TO  31011-JOBKANRI-CHK-EXT
           END-IF
      *
           STRING    "状態："
                     WRK-JOBERRMSG    DELIMITED  BY  SPACE
                     " コード："
                     JOB-ERRCD      DELIMITED   BY  SIZE
                                           INTO  SPA-P0311-JOBERRMSG
           END-STRING
      *
           MOVE    JOB-JOBID           TO  SPA-P0311-JOBID
      *    ジョブ開始年月日
           MOVE    JOB-STARTYMD        TO  SPA-P0311-STRYMD
           MOVE    JOB-ENDYMD          TO  SPA-P0311-ENDYMD
           IF      SPA-P0311-JOBCHK    =   "4"
      *        終了日を開始日に
               MOVE    JOB-ENDYMD          TO  SPA-P0311-STRYMD
               MOVE    SPA-P0311-SYSYMD    TO  SPA-P0311-ENDYMD
      *        状態はクリア
               MOVE    SPACE               TO  SPA-P0311-JOBERRMSG
           END-IF
      *
           MOVE    SPA-P0311-STRYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P0311-STRYMDG
      *
      *    ジョブ終了年月日
           MOVE    SPA-P0311-ENDYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-P0311-ENDYMDG
           .
       31011-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P0311
           INITIALIZE                      P0311
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-P0311-SYUTOKUKBN-MAX)
               MOVE    SPA-P0311-SYUTOKUKBN-L-M (IDX)
                                       TO  P0311-SYUTOKUKBN-ITEM
                                                               (IDX)
           END-PERFORM
           MOVE    SPA-P0311-SYUTOKUKBN-MAX
                                       TO   P0311-SYUTOKUKBN-COUNT
           MOVE    SPA-P0311-SYUTOKUKBN
                                       TO  P0311-SYUTOKUKBN
      *
           MOVE    SPA-P0311-STRYMDG   TO  P0311-STRYMD
           MOVE    SPA-P0311-ENDYMDG   TO  P0311-ENDYMD
           MOVE    SPA-P0311-JOBERRMSG TO  P0311-SYORIMSG
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-P0311-LIST-MAX)
               MOVE    SPA-P0311-RENNUM (IDX)
                                       TO  P0311-RENNUM  (IDX)
               MOVE    SPA-P0311-TSRYYMDG  (IDX)
                                       TO  P0311-SRYYMD  (IDX)
               MOVE    SPA-P0311-SYORIKIKAN (IDX)
                                       TO  P0311-SYOKAIKIKAN (IDX)
               MOVE    SPA-P0311-ERRCDMSG (IDX)
                                       TO  P0311-ERRCDMSG  (IDX)
               MOVE    SPA-P0311-STARTTIMEG (IDX)
                                       TO  P0311-STARTTIME (IDX)
      *
               MOVE    IDX             TO  P0311-DOUILST-COUNT
           END-PERFORM
      *
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
           IF      SPA-P0311-CUR         =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "SYUTOKUKBN"
                       MOVE    02           TO  SPA-P0311-CUR
                   WHEN    "STRYMD"
                       MOVE    03           TO  SPA-P0311-CUR
                   WHEN    "ENDYMD"
                       MOVE    12           TO  SPA-P0311-CUR
                   WHEN    OTHER
                       MOVE    01           TO  SPA-P0311-CUR
               END-EVALUATE 
           END-IF
           EVALUATE    SPA-P0311-CUR
              WHEN    01
                  MOVE    "SYUTOKUKBN"   TO  MCP-WIDGET
              WHEN    02
                  MOVE    "STRYMD"       TO  MCP-WIDGET
              WHEN    03
                  MOVE    "ENDYMD"       TO  MCP-WIDGET
              WHEN    10
                  MOVE    "B10"          TO  MCP-WIDGET
              WHEN    12
                  MOVE    "B12"          TO  MCP-WIDGET
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
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
      *         WHEN    "CLICKED"       ALSO    "B02"
      *             PERFORM 402-CLEAR-SEC
      *        履歴更新
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-B10-SYORI-SEC
      *        一括照会
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-P0311-CUR
      *
      *     EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *         WHEN    "ACTIVATE"      ALSO    "PTNUM2"
      *     END-EVALUATE
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
           IF      SPA-P0311-JOBCHK    =   "1"
                                       OR  "2"
                                       OR  "3"
      *        処理途中　変更しない
               IF      (P0311-STRYMD   =   SPA-P0311-STRYMDG )
                  AND  (P0311-ENDYMD   =   SPA-P0311-ENDYMDG )
                   CONTINUE
               ELSE
                   MOVE    "0009"          TO  SPA-ERRCD 
                   GO  TO  410-INPUT-CHK-EXT 
               END-IF
           END-IF
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    取得対象区分
           PERFORM 4002-SYUTOKUKBN-CHK-SEC
      *
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4002-STRYMD-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4002-ENDYMD-CHK-SEC
           END-IF
           .
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P0311-SYUTOKUKBN    TO  SPA-P0311-SYUTOKUKBN
      *
           MOVE    P0311-STRYMD    TO  SPA-P0311-STRYMDG
           MOVE    P0311-ENDYMD    TO  SPA-P0311-ENDYMDG
      *
      *test
           DISPLAY "P0311 4101-GMN-SPA-SET-SEC"
           ",SPA-P0311-STRYMDG:"  SPA-P0311-STRYMDG
           ",SPA-P0311-ENDYMDG:"  SPA-P0311-ENDYMDG
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    取得対象区分処理
      *****************************************************************
       4002-SYUTOKUKBN-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHGARI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-P0311-SYUTOKUKBN-MAX)
               IF      SPA-P0311-SYUTOKUKBN-L-M (IDX)
                                       =   SPA-P0311-SYUTOKUKBN
                   IF      SPA-P0311-SYUTOKUKBN-L-K (IDX)
                                       =   SPA-P0311-SYUTOKUKBN-K
      *                変更なし
                       MOVE    2               TO  FLG-CHGARI
                   ELSE
      *                変更あり
                       MOVE    1               TO  FLG-CHGARI
                   END-IF
                   MOVE    SPA-P0311-SYUTOKUKBN-LST (IDX)
                                           TO  SPA-P0311-SYUTOKUKBN-G
                   MOVE    SPA-P0311-SYUTOKUKBN-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-CHGARI          =   ZERO
               MOVE    SPACE           TO  SPA-P0311-SYUTOKUKBN-K
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-P0311-CUR
               GO  TO  4002-SYUTOKUKBN-CHK-EXT
           END-IF
      *
           IF      FLG-CHGARI          =   2
               GO  TO  4002-SYUTOKUKBN-CHK-EXT
           END-IF
      *
      *    初期表示
           PERFORM 3101-JOBOKANRI-INT-SEC
           .
      *
       4002-SYUTOKUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始日付処理
      *****************************************************************
       4002-STRYMD-CHK-SEC             SECTION.
      *
           IF      SPA-P0311-STRYMDG       =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-P0311-CUR
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P0311-STRYMDG       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"              USING
                                           ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-P0311-STRYMDG
               MOVE    SGDAY-SDAY          TO  SPA-P0311-STRYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-P0311-CUR
           END-IF
           .
       4002-STRYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了日付処理
      *****************************************************************
       4002-ENDYMD-CHK-SEC             SECTION.
      *
           IF      SPA-P0311-ENDYMDG       =   SPACE
               IF     (WRK-MCP-WIDGET      =   "ENDYMD" )
                  OR  (FLG-KOUSIN          =   1    )
      *            マシン日付
                   MOVE    SPA-P0311-SYSYMD    TO  SPA-P0311-ENDYMDG
               ELSE
                   GO  TO   4002-ENDYMD-CHK-EXT
               END-IF
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-P0311-ENDYMDG       TO  SGDAY-INDAY
           CALL    "ORCSGDAY"              USING
                                           ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-P0311-ENDYMDG
               MOVE    SGDAY-SDAY          TO  SPA-P0311-ENDYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-P0311-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-P0311-STRYMD    >   SPA-P0311-ENDYMD
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-P0311-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        マシン日付以降はエラー
               IF      SPA-P0311-ENDYMD    >   SPA-P0311-SYSYMD
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-P0311-CUR
               END-IF
           END-IF
      *    終了日から開始日の期間
           IF      SPA-ERRCD           =   SPACE
      *      訪問診療
             IF      SPA-P0311-SYUTOKUKBN-K  =   "1"
      *        終了日の１ヶ月前
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-P0311-ENDYMD    TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               IF      SPA-P0311-STRYMD    <   LNK-DAY6-KOYOMI
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-P0311-CUR
               END-IF
             ELSE
      *      オンライン診療
      *        開始日の１週間前
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-P0311-ENDYMD    TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -6                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               IF      SPA-P0311-STRYMD    <   LNK-DAY6-KEISAN
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-P0311-CUR
               END-IF
            END-IF
           END-IF
           .
       4002-ENDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *    更新前チェック
           MOVE    1                   TO  FLG-KOUSIN
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF     (SPA-ERRCD           =   SPACE)
               PERFORM 40090-JOBKANRI-SYORI-SEC
           END-IF
      *
      *
           IF      (SPA-ERRCD           =   SPACE )
              AND  (SPA-PIDCD           =   SPACE )
               PERFORM 40091-SYOKAI-CALLSYORI-SEC
           END-IF
      *
          .
       490-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    確定処理
      *****************************************************************
       40090-JOBKANRI-SYORI-SEC             SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-P0311-SHELLID   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
      *
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
           END-IF
           MOVE    ZERO                TO  FLG-JOBKANRI-OK
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      ( FLG-JOBKANRI   =   1  )
                         OR   ( SPA-ERRCD      NOT =   SPACE )
                         OR   ( FLG-JOBKANRI-OK    =   1     )
      *        診療日判定
               IF      JOB-SRYYMD      =    SPA-P0311-SYSYMD
      *            初期表示と同じ
                   IF      (JOB-JOBID       =   SPA-P0311-JOBID)
                       PERFORM 400901-JOBKANRI-CHECK-SEC
                       MOVE    1               TO  FLG-JOBKANRI-OK
      *2025/04     ELSE
      *                別処理
      *                 MOVE    "0006"       TO  SPA-ERRCD
      ***               MOVE    10           TO  SPA-P0311-CUR
                   END-IF
               END-IF
      *
               IF      FLG-JOBKANRI    =   ZERO
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-JOBKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       40090-JOBKANRI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理と時間チェック処理
      *****************************************************************
       400901-JOBKANRI-CHECK-SEC              SECTION.
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           IF      SMCNDATE-YMD    NOT =   SPA-P0311-SYSYMD
              GO  TO  400901-JOBKANRI-CHECK-EXT
           END-IF
           MOVE    SMCNDATE-HMS        TO  WRK-IN-TIME
           MOVE    SPACE               TO  SPA-P0311-JOB-UID
      *    エラーコード
           EVALUATE    JOB-ERRCD
               WHEN    SPACE
      *            API実施なし  ２分後
                   MOVE    JOB-STARTTIME       TO  WRK-CHK-TIME
                   COMPUTE WRK-C-MM        =   WRK-C-MM 
                                           +   2
                   IF      WRK-C-MM        >=  60
                       COMPUTE WRK-C-HH    =   WRK-C-HH
                                           +   1
                       COMPUTE WRK-C-MM    =   WRK-C-MM
                                           -   60
                   END-IF
      *
                   IF      WRK-IN-TIME     <=  WRK-CHK-TIME
      *                処理中
                       MOVE    "0007"          TO  SPA-ERRCD 
                   ELSE
      *                確認
                       MOVE    "0101"          TO  SPA-PIDCD
      *                カルテＵＩＤ
                       MOVE   JOB-KARTE-UID    TO  SPA-P0311-JOB-UID
                   END-IF
               WHEN    "0001"
      *            受付前　 ５分後
                   MOVE    JOB-STARTTIME       TO  WRK-CHK-TIME
                   COMPUTE WRK-C-MM        =   WRK-C-MM 
                                           +   5
                   IF      WRK-C-MM        >=  60
                       COMPUTE WRK-C-HH    =   WRK-C-HH
                                           +   1
                       COMPUTE WRK-C-MM    =   WRK-C-MM
                                           -   60
                   END-IF
      *
                   IF      WRK-IN-TIME     <=  WRK-CHK-TIME
      *                処理中
                       MOVE    "0007"          TO  SPA-ERRCD 
                   ELSE
      *                確認
                       MOVE    "0101"          TO  SPA-PIDCD
      *                カルテＵＩＤ
                       MOVE   JOB-KARTE-UID    TO  SPA-P0311-JOB-UID
                   END-IF
               WHEN    "0002"
      *            受付完了
                   MOVE    "0008"          TO  SPA-ERRCD 
               WHEN    "0003"
      *            資格結果受け取り
                   CONTINUE
           END-EVALUATE
           .
       400901-JOBKANRI-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括照会処理
      *****************************************************************
       40091-SYOKAI-CALLSYORI-SEC              SECTION.
      *
      *オンライン資格確認
           INITIALIZE                  ORCSONSHI001AREA
           EVALUATE     SPA-P0311-SYUTOKUKBN-K
      *        訪問診療　同意一括照会
               WHEN     "1"
                   MOVE    "06"            TO  LNK-ONSHI001-SYOKBN
      *            診療区分
                   MOVE    "2"             TO
                                           LNK-ONSHI001-ONSHINRYO-KBN
               WHEN     "2"
      *        オンライン診療　同意一括照会
                   MOVE    "07"            TO  LNK-ONSHI001-SYOKBN
      *            診療区分
                   MOVE    "3"             TO
                                           LNK-ONSHI001-ONSHINRYO-KBN
           END-EVALUATE
      *
           MOVE    "12"                TO  LNK-ONSHI001-GYOUMUCD
           MOVE    SPA-P0311-STRYMD    TO  LNK-ONSHI001-STARTYMD
           MOVE    SPA-P0311-ENDYMD    TO  LNK-ONSHI001-ENDYMD
           MOVE    SPA-P0311-JOB-UID   TO  LNK-ONSHI001-TBL-UUID
      *
           CALL    "ORCSONSHI001"      USING   ORCSONSHI001AREA
                                               SPA-AREA
      *    P031へ戻る
           PERFORM 210-BACK
           .
       40091-SYOKAI-CALLSYORI-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
      * 402-CLEAR-SEC              SECTION.
      *
      *     .
      * 402-CLEAR-EXT.
      *     EXIT.
      *****************************************************************
      *    履歴更新処理
      *****************************************************************
       410-B10-SYORI-SEC              SECTION.
      *
      *    初期表示
           PERFORM 3101-JOBOKANRI-INT-SEC
           .
       410-B10-SYORI-EXT.
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
           MOVE    "P0311"             TO  MCP-WINDOW.
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
                   MOVE    "開始日付が暦日ではありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "終了日付が暦日ではありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "開始日≦終了日で入力して下さい。"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "終了日が未来日です。"
                                           TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "取得対象区分がありません。"
                                           TO  SPA-ERRMSG
               WHEN    "0006"
                   STRING   "照会履歴が変更となっています。"
                            "再度、履歴更新を押下下さい。"
                                           DELIMITED  BY  SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0007"
                   MOVE    "処理依頼中です。"
                                           TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "処理依頼中です。"
                                           TO  SPA-ERRMSG
               WHEN    "0009"
                   STRING   "依頼中です。"
                            "同意年月日範囲の変更は出来ません。"
                                           DELIMITED  BY  SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING   "同意年月日範囲が１ヶ月超です。"
                            "１ヶ月以内にして下さい。"
                                           DELIMITED  BY  SIZE
                                           INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING   "同意年月日範囲が１週間超です。"
                            "１週間以内にして下さい。"
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
           MOVE    "P0311"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PERR"              TO  MCP-WINDOW
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
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   STRING  "再度依頼します。"
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
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P0311"              TO  SPA-MOTOPG
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
      *
           MOVE    "P0311"             TO  SPA-MOTOPG
           MOVE    SPA-P0311-MOTOPG    TO  SPA-SAKIPG
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
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
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
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THHH            TO  WRK-HEN-THH
           MOVE    WRK-THMM            TO  WRK-HEN-TMM
           MOVE    WRK-THSS            TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      **    ジョブ管理マスター読み込み
      *****************************************************************
       900-JOBKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JOBKANRI-REC
               MOVE    ZERO            TO  FLG-JOBKANRI
           ELSE
               INITIALIZE                  JOBKANRI-REC
               MOVE    1               TO  FLG-JOBKANRI
           END-IF
      *
           .
       900-JOBKANRI-READ-EXT.
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
      *
