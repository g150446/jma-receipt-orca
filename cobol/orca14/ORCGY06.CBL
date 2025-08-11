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
       PROGRAM-ID.             ORCGY06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  :  予約患者一括資格確認照会（Ｙ０６）
      *  管理者            : 
      *  21/11/22    ORCAMO        新規作成
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
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    予約用ワークスパ領域
           COPY    "Y01COMMON-SPA".
      *
      *    予約パ領域
           COPY    "Y01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-JOBKANRI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-CNT             PIC ZZZZZZZZZ9.
           03  WRK-NUM             PIC 99.
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH         PIC X(02).
               05  WRK-TMM         PIC X(02).
               05  WRK-TSS         PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH     PIC X(02).
               05  WRK-HEN-T1      PIC X(01).
               05  WRK-HEN-TMM     PIC X(02).
               05  WRK-HEN-T2      PIC X(01).
               05  WRK-HEN-TSS     PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "yoyaku01".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    予約
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *     COPY  "CPORCSGDAY.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    PUSH
           COPY    "CPPUSHONLINEQUA.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA14SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-Y01
           MOVE    SPA-WORK        TO  SPA-Y01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 210-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-Y01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Y01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "YERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "Y06"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "YID1"
      *            確認画面
                   PERFORM 30010-YID1-SET-SEC
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
           INITIALIZE              SPA-Y06-AREA
           MOVE    "black"         TO  SPA-Y06-GMN-YYKYMD-STYLE
      *
      *    マシン日付取得
           INITIALIZE              ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"   USING
                                   ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  SPA-Y06-NAI-SMCNDATE-YMD
      *
      *    予約日設定
           IF      SPA-Y06-NAI-SMCNDATE-YMD
                                   =   SPA-SYSYMD
      *        システム日付以降の直近の予約を検索     
               INITIALIZE                  YYK-REC
               MOVE    SPA-SYSYMD      TO  YYK-YYKYMD
               MOVE    SPA-HOSPNUM     TO  YYK-HOSPNUM
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "key11"         TO  WRK-MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
               IF      FLG-YYK         =   1
                   MOVE    "対象の予約はありません"
                                           TO  SPA-Y06-GMN-MSG
               ELSE
                   MOVE    YYK-YYKYMD      TO  SPA-Y06-NAI-YYKYMD
               END-IF
           ELSE
               MOVE    SPA-SYSYMD      TO  SPA-Y06-NAI-YYKYMD
      *        該当の運用日付の予約を検索
               INITIALIZE                  YYK-REC
               MOVE    SPA-SYSYMD      TO  YYK-YYKYMD
               MOVE    SPA-HOSPNUM     TO  YYK-HOSPNUM
               MOVE    YYK-REC         TO  MCPDATA-REC
               MOVE    "key12"         TO  WRK-MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
               IF      FLG-YYK         =   1
                   MOVE    1               TO  SPA-Y06-NAI-YYKYMD-CHK
               END-IF
           END-IF
      *
           IF      SPA-Y06-NAI-YYKYMD
                                   =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    SPA-Y06-NAI-YYKYMD
                                       TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD3
                                       TO  SPA-Y06-GMN-YYKYMD
          END-IF
      *
      *    一覧編集処理
           PERFORM 3101-JOBKANRI-HENSYU-SEC
      *
           MOVE    10              TO  SPA-Y06-GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧編集処理
      *****************************************************************
       3101-JOBKANRI-HENSYU-SEC        SECTION.
      *
           INITIALIZE                  SPA-Y06-GMN-YYKLIST-G
                                       SPA-Y06-NAI-YYKLIST-G
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"  TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX
           PERFORM         UNTIL ( FLG-JOBKANRI    =   1  )
                           OR    ( IDX             >=  10 )
               ADD     1               TO  IDX
      *
               MOVE    JOB-SRYYMD      TO  SPA-Y06-NAI-LYYKYMD (IDX)
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    JOB-SRYYMD      TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1
                                       TO  SPA-Y06-GMN-LYYKYMD   (IDX)
               IF      JOB-YOBI    NOT =   SPACE
                   MOVE    "済"            TO  SPA-Y06-GMN-LMARK (IDX)
               END-IF    
               EVALUATE    JOB-SYOKBN
                   WHEN    "1"
                       MOVE    "終了"      TO  SPA-Y06-GMN-LMARK (IDX)
                   WHEN    "9"
                       MOVE    "異常"      TO  SPA-Y06-GMN-LMARK (IDX)
               END-EVALUATE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    JOB-STARTYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1
                                       TO  SPA-Y06-GMN-LSYORIYMD (IDX)
               MOVE    JOB-UPDCNT      TO  SPA-Y06-GMN-LALLCNT   (IDX)
               MOVE    JOB-UPDCNT1     TO  SPA-Y06-GMN-LCNT      (IDX)
               MOVE    JOB-UPDCNT2     TO  SPA-Y06-GMN-LERRCNT   (IDX)
      *
               MOVE    IDX             TO  SPA-Y06-GMN-LMAX
      *
               IF      SPA-Y06-NAI-YYKYMD
                                       =   JOB-SRYYMD
                   MOVE    1               TO  SPA-Y06-NAI-YYKYMD-OK
                   IF      JOB-YOBI        =   SPACE
                       MOVE    1               TO  SPA-Y06-NAI-JYOTAI
                       MOVE    "red"           TO
                                               SPA-Y06-GMN-YYKYMD-STYLE
                   ELSE
                       IF      JOB-UPDCNT      =   ZERO
                           MOVE    2           TO  SPA-Y06-NAI-JYOTAI
                       ELSE
                           MOVE    3           TO  SPA-Y06-NAI-JYOTAI
                       END-IF
                   END-IF    
               END-IF    
      *
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-N-SEC
           END-PERFORM
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3101-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＹＩＤ１）ＯＫ処理
      *****************************************************************
       30010-YID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-YID1CD
               WHEN    "1001"
                   MOVE    SPACE               TO  SPA-YID1CD
                   IF      SPA-YID1-FLG        =   "OK"
                       PERFORM 410-SYORI-SEC
                   END-IF
                   MOVE    1                   TO  SPA-Y06-GMN-CUR
           END-EVALUATE
           MOVE    SPACE               TO  SPA-YID1CD
           MOVE    SPACE               TO  SPA-YID1-FLG
           .
        30010-YID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       210-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    ＯＫ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 410-SYORI-GID-SEC
           END-EVALUATE
      *
           .
       210-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "Y06"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約患者一括資格確認照会処理
      *****************************************************************
       410-SYORI-GID-SEC               SECTION.
      *
           IF      SPA-Y06-NAI-YYKYMD  =   SPACE
               MOVE    "0006"              TO  SPA-ERRCD
               GO  TO  410-SYORI-GID-EXT
           END-IF
      *
           IF      SPA-Y06-NAI-SMCNDATE-YMD
                                   NOT =   SPA-SYSYMD
               IF      SPA-Y06-NAI-YYKYMD-CHK
                                           =   1
                   MOVE    "0004"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-Y06-NAI-SMCNDATE-YMD
                                               >   SPA-SYSYMD
                       MOVE    "0005"              TO  SPA-ERRCD
                   END-IF        
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-Y06-NAI-JYOTAI
                   WHEN    1
                       MOVE    "0001"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "0002"              TO  SPA-ERRCD
                   WHEN    3
                       MOVE    "0003"              TO  SPA-ERRCD
                   WHEN    OTHER
                       MOVE    "1001"              TO  SPA-YID1CD
               END-EVALUATE
           END-IF
           .
       410-SYORI-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約患者一括資格確認照会処理
      *****************************************************************
       410-SYORI-SEC               SECTION.
      *
      *    マシン日付取得
           INITIALIZE              ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"   USING
                                   ORCSMCNDATEAREA
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
      *    JOBID最大値取得
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           PERFORM 900-JOBKANRI-READ-SEC
           MOVE    JOB-JOBID           TO  WRK-CONS-JOB-JOBID
           IF      WRK-CONS-JOB-JOBID  =   10
               MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
           END-IF    
           ADD     1                   TO  WRK-CONS-JOB-JOBID
      *    JOBID最大値+1のデータ削除
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
               IF      SJOBKANRI-RETURN    =   ZERO
                   CONTINUE
               ELSE
                   DISPLAY
                       "DELETE ERR KEY = [" SJOBKANRI-RETURN JOB-KEY "]"
                   MOVE    "1002"              TO  SPA-ERRCD
                   GO  TO  410-SYORI-EXT
               END-IF
           END-IF    
      *    JOBID最大値+1のデータ作成
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           MOVE    SPA-Y06-NAI-YYKYMD  TO  JOB-SRYYMD
           MOVE    SPA-Y06-NAI-SMCNDATE-YMD
                                       TO  JOB-STARTYMD
           MOVE    C-UID               TO  JOB-KARTE-UID
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              >   ZERO
               DISPLAY "INSERT ERR KEY = [" MCP-RC JOB-KEY "]"
               MOVE    "1001"              TO  SPA-ERRCD
               GO  TO  410-SYORI-EXT
           END-IF
      *
           MOVE    SPA-Y06-NAI-YYKYMD  TO  WRK-SYMD
      *****MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
      *
           INITIALIZE                      PUSHQUA-REC
           MOVE    "Bulk_Qualification"
                                       TO  PUSHQUA-EVENT
           MOVE    "Rreq"              TO  PUSHQUA-CLASS
           MOVE    SPA-HOSPNUM         TO  PUSHQUA-HOSPNUM
           MOVE    WRK-HEN-DATE        TO  PUSHQUA-KAKUNINYMD
           MOVE    C-UID               TO  PUSHQUA-UUID
      *****MOVE                        TO  PUSHQUA-INFORMATION-DATE
           DISPLAY "PUSH=" PUSHQUA-REC
      *
           MOVE    PUSHQUA-REC         TO  MCPDATA-REC
           MOVE    "PUSHEVENT"         TO  MCP-FUNC
           MOVE    "push_onlinequa"    TO  MCP-TABLE
           MOVE    "push_onlinequa"    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    一覧編集処理
           PERFORM 3101-JOBKANRI-HENSYU-SEC
      *
           MOVE    1                   TO  SPA-Y06-GMN-CUR
           .
       410-SYORI-EXT.
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
           IF      SPA-YID1CD      NOT =   SPACE
               PERFORM 510-YID1CDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "Y06"               TO  MCP-WINDOW
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
           MOVE    SPACE               TO  Y06
           INITIALIZE                      Y06
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >       SPA-Y06-GMN-LMAX
               MOVE    IDX                 TO  WRK-NUM
               MOVE    WRK-NUM             TO  Y06-TRENNUM   (IDX)
               MOVE    SPA-Y06-GMN-LYYKYMD (IDX)
                                           TO  Y06-TYYKYMD   (IDX)
               MOVE    SPA-Y06-GMN-LMARK   (IDX)
                                           TO  Y06-TMARK     (IDX)
               MOVE    SPA-Y06-GMN-LSYORIYMD (IDX)
                                           TO  Y06-TSYORIYMD (IDX)
               MOVE    SPA-Y06-GMN-LALLCNT (IDX)
                                           TO  WRK-CNT
               MOVE    WRK-CNT             TO  Y06-TALLCNT   (IDX)
               MOVE    SPA-Y06-GMN-LCNT (IDX)
                                           TO  WRK-CNT
               MOVE    WRK-CNT             TO  Y06-TCNT      (IDX)
               MOVE    SPA-Y06-GMN-LERRCNT (IDX)
                                           TO  WRK-CNT
               MOVE    WRK-CNT             TO  Y06-TERRCNT   (IDX)
           END-PERFORM
           MOVE    SPA-Y06-GMN-LMAX    TO  Y06-COUNT
      *
           MOVE    SPA-Y06-GMN-YYKYMD  TO  Y06-YYKYMD
           MOVE    SPA-Y06-GMN-YYKYMD-STYLE
                                       TO  Y06-YYKYMD-STYLE
           MOVE    SPA-Y06-GMN-MSG     TO  Y06-MSG
           MOVE    "red"               TO  Y06-MSG-STYLE
      *
           MOVE    WIDGET-NORMAL       TO  Y06-B12-STATE
      **     IF      SPA-Y06-GMN-LMAX    =   ZERO
      **         MOVE    WIDGET-INSENSITIVE  TO  Y06-B12-STATE
      **     END-IF
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
      **    IF      SPA-Y06-GMN-CUR          =   ZERO
      **         PERFORM 50011-GMN-CURSET-SEC
      **    END-IF
      *
          EVALUATE    SPA-Y06-GMN-CUR
                WHEN    1
                    MOVE  "B01"         TO   MCP-WIDGET
                WHEN    10
                    MOVE  "B12"         TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力後カーソルセット処理
      *****************************************************************
       50011-GMN-CURSET-SEC              SECTION.
      *
      **     EVALUATE    WRK-MCP-WIDGET
      **         WHEN    "YYKYMD"
      **             MOVE    6                   TO  SPA-Y06-GMN-CUR
      **     END-EVALUATE
      *
           .
       50011-GMN-CURSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      * 
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "実施依頼中です"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "資格確認依頼済みです"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "情報取得済みです"  TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "資格確認対象情報がありません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "過去情報の依頼はできません"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "対象の予約はありません"
                                               TO  SPA-ERRMSG
      *
               WHEN    "1001"
                   MOVE
                       "ＪＯＢ管理ＤＢの更新処理でエラーとなりました" 
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE
                       "ＪＯＢ管理ＤＢの削除処理でエラーとなりました" 
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  YERR
           MOVE    SPA-ERRCD            TO  YERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  YERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Y06"                TO  SPA-MOTOPG
           MOVE    "YERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面表示処理
      *****************************************************************
       510-YID1CDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-YID1-FLG
      *
           MOVE    SPACE                TO  YID1
           INITIALIZE                       YID1
           MOVE    SPA-YID1CD           TO  YID1-ID1CODE
           MOVE    "戻る"               TO  YID1-B01
      *
           EVALUATE    SPA-YID1CD
               WHEN    "1001"
                   MOVE    "予約患者の一括資格確認を行います。"
                                        TO  YID1-ID1MSG
           END-EVALUATE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Y06"                TO  SPA-MOTOPG
           MOVE    "YID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       510-YID1CDSET-EXT.
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
      *
      *****************************************************************
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-SEC          SECTION.
      * 
           MOVE    JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-JOBKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-N-SEC          SECTION.
      * 
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JOBKANRI-REC
               MOVE    ZERO            TO  FLG-JOBKANRI
           ELSE
               INITIALIZE                  JOBKANRI-REC
               MOVE    1               TO  FLG-JOBKANRI
               END-IF
           .
       900-JOBKANRI-READ-N-EXT.
           EXIT.
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YYK-READ-SEC          SECTION.
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  YYK-REC
                   MOVE    ZERO            TO  FLG-YYK
               ELSE
                   INITIALIZE                  YYK-REC
                   MOVE    1               TO  FLG-YYK
               END-IF
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1               TO  FLG-YYK
           END-IF
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-YYK-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
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
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
