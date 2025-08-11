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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGR95.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト作成-交付番号設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  24/02/28    ORCAMO        新規作成
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
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "R98SCR-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-MONTHLYNUM      PIC 9(01).
      *****03  FLG-ONSHI-AIDLST    PIC 9(01).
      *     
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).     
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SELNO           PIC 9(02).
           03  WRK-Z9              PIC Z9.
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-RIDMSG          PIC X(70).
           03  WRK-YM              PIC X(07). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG1        PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
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
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-LINE-MAX   PIC 9(02)   VALUE   20.
      *
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    資格確認医療扶助交付番号格納
      ** 01  ONSHI-AIDLST-REC.
      *******COPY    "CPONSHI-AIDLST.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA42SCRAREA.INC".
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
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R98FREE
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
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
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R98FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "RERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "R95"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
               WHEN    "RID1"
                   PERFORM 330-RID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
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
           MOVE    1               TO  SPA-GMN-R95-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＲＩＤ１）ＯＫ処理
      *****************************************************************
       330-RID1-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    =   "OK"
               EVALUATE    SPA-RIDCD
                   WHEN    "1001"
                       PERFORM 450-TOUROKU-SEC
                       IF      SPA-ERRCD       =   SPACE 
                           PERFORM 210-BACK
                       END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
           MOVE    SPACE           TO  SPA-RIDCD
           .
       330-RID1-SET-EXT.
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
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-TOUROKU-MAE-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4101-SELNUM-CHK-SEC
              WHEN     ANY             ALSO    "BODY_LIST"
                   PERFORM 4102-BODYLST-SELECT-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択入力処理
      *****************************************************************
       4101-SELNUM-CHK-SEC         SECTION.
      *
           MOVE    1                   TO  SPA-GMN-R95-CUR
      *
           MOVE    R95-SELNUM          TO  SPA-GMN-R95-SELNUM
      *
           IF      SPA-GMN-R95-SELNUM  =   ZERO
               CONTINUE
           ELSE
               IF      SPA-GMN-R95-SELNUM  >   SPA-GMN-R95-MAX
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-GMN-R95-SELNUM
               ELSE
                   MOVE    SPA-GMN-R95-SELNUM  TO  R95-BODY-LIST-ROW
                   PERFORM 41011-SELNUM-HENSYU-SEC
               END-IF
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択項目編集処理
      *****************************************************************
       41011-SELNUM-HENSYU-SEC            SECTION.
      *
           MOVE    99                  TO  SPA-GMN-R95-CUR 
      *
           .
       41011-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4102-BODYLST-SELECT-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       WRK-CONS-LINE-MAX
               IF      R95-BODY-LIST-SELECT (IDX)  =   "T"
                   IF      IDX             =   SPA-GMN-R95-SELNUM
                       CONTINUE
                    ELSE        
                        MOVE    IDX             TO  WRK-SELNO
                        MOVE    WRK-CONS-LINE-MAX
                                                TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-GMN-R95-SELNUM
               PERFORM 41011-SELNUM-HENSYU-SEC
           END-IF
           .
       4102-BODYLST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-KOFU-IDX
      *
           MOVE    "R98"               TO  SPA-SAKIPG
           MOVE    "R95"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
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
      *    更新前処理
      *****************************************************************
       450-TOUROKU-MAE-SEC                SECTION.
      *
           IF      SPA-GMN-R95-SELNUM   =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-R95-CUR
           ELSE    
               MOVE    "1001"              TO  SPA-RIDCD
                   
           END-IF
          .
       450-TOUROKU-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       450-TOUROKU-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  MONTHLYNUM-REC
           MOVE    SPA-HOSPNUM     TO  MONTHLYNUM-HOSPNUM
           MOVE    SPA-NAI-TPTID  (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                   TO  MONTHLYNUM-PTID
           MOVE    SPA-NAI-TKOHID (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                   TO  MONTHLYNUM-KOHID
           MOVE    SPA-NAI-SRYYM   TO  MONTHLYNUM-SRYYM
           MOVE    SPA-R01-NYUGAIKBN
                                   TO  MONTHLYNUM-NYUGAIKBN
           MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
           PERFORM 910-MONTHLYNUM-READ-SEC
      *    受給者番号未入力のとき月代わり受給者番号マスタに
      *    存在しないときは更新しない                 
           IF    ( SPA-GMN-TJKYSNUM (SPA-GMN-PAGE SPA-NAI-KOFU-IDX) 
                                   =   SPACE )    
           AND   ( FLG-MONTHLYNUM  =   1     )
               CONTINUE
           ELSE                             
               IF      FLG-MONTHLYNUM  =   ZERO
      *            受給者番号、交付番号に変更がない場合は更新しない     
                   IF    ( SPA-GMN-R95-TJKYSNUM
                                       (SPA-GMN-R95-SELNUM)
                                           =   MONTHLYNUM-JKYSNUM )
                   AND   ( SPA-GMN-R95-TKOFUNUM
                                       (SPA-GMN-R95-SELNUM)
                                           =   MONTHLYNUM-KOFUNUM )
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-TJKYSNUM
                                       (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                               TO MONTHLYNUM-JKYSNUM
                       MOVE    SPA-GMN-R95-TKOFUNUM
                                       (SPA-GMN-R95-SELNUM)
                                           TO MONTHLYNUM-KOFUNUM
                       MOVE    SMCNDATE-YMD    TO  MONTHLYNUM-UPYMD
                       MOVE    SMCNDATE-HMS    TO  MONTHLYNUM-UPHMS
                       MOVE    SPA-OPID        TO  MONTHLYNUM-OPID
                       MOVE    "DBUPDATE"      TO  MCP-FUNC
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       MOVE    "tbl_monthlynum"
                                               TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE    SPA-GMN-R95-TJKYSNUM
                                                   (SPA-GMN-R95-SELNUM)
                                               TO
                                               SPA-GMN-TJKYSNUM
                                       (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                           MOVE    SPA-GMN-R95-TKOFUNUM
                                                   (SPA-GMN-R95-SELNUM)
                                               TO
                                               SPA-GMN-TKOFUNUM
                                       (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                       ELSE    
                           MOVE    "1001"          TO  SPA-ERRCD
                           GO  TO  450-TOUROKU-EXT
                       END-IF
                   END-IF
               ELSE
                   INITIALIZE                  MONTHLYNUM-REC 
grpsys             MOVE    SPA-HOSPNUM
                                           TO  MONTHLYNUM-HOSPNUM
                   MOVE    SPA-NAI-TPTID
                                       (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                           TO  MONTHLYNUM-PTID
                   MOVE    SPA-NAI-TKOHNUM
                                       (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                           TO  MONTHLYNUM-KOHNUM
                   MOVE    SPA-NAI-SRYYM   TO  MONTHLYNUM-SRYYM
                   MOVE    SPA-R01-NYUGAIKBN
                                           TO  MONTHLYNUM-NYUGAIKBN
                   MOVE    SPA-GMN-TJKYSNUM
                                       (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                           TO  MONTHLYNUM-JKYSNUM
                   MOVE    SPA-GMN-R95-TKOFUNUM
                                       (SPA-GMN-R95-SELNUM)
                                           TO MONTHLYNUM-KOFUNUM
                   MOVE    SPA-NAI-TKOHID
                                       (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                           TO  MONTHLYNUM-KOHID
                   MOVE    SMCNDATE-YMD    TO  MONTHLYNUM-CREYMD
                   MOVE    SMCNDATE-YMD    TO  MONTHLYNUM-UPYMD
                   MOVE    SMCNDATE-HMS    TO  MONTHLYNUM-UPHMS
                   MOVE    SPA-OPID        TO  MONTHLYNUM-OPID
                   MOVE    "DBINSERT"      TO  MCP-FUNC   
                   MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                   MOVE    "tbl_monthlynum"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    SPA-GMN-R95-TKOFUNUM (SPA-GMN-R95-SELNUM)
                                               TO
                                               SPA-GMN-TKOFUNUM
                                       (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                   ELSE    
                       MOVE    "1001"          TO  SPA-ERRCD
                       GO  TO  450-TOUROKU-EXT
                   END-IF
               END-IF
           END-IF
           .
       450-TOUROKU-EXT.
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
           IF      SPA-RIDCD       NOT =   SPACE
               PERFORM 520-RIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R95"               TO  MCP-WINDOW
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
           MOVE    SPACE               TO  R95
      *
           INITIALIZE                      R95
           MOVE    SPA-GMN-SRYYMH      TO  R95-SRYYMH
           MOVE    R98-RENNUM (SPA-NAI-KOFU-IDX)
                                       TO  R95-RENNUM
           MOVE    SPA-GMN-TPTNUM     (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                       TO  R95-PTNUM
           MOVE    SPA-GMN-TNAME      (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                       TO  R95-NAME
           MOVE    SPA-GMN-TFTNJANUM  (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                       TO  R95-HFTNJANUM
           MOVE    SPA-GMN-TJKYSNUM   (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                       TO  R95-HJKYSNUM
           MOVE    SPA-GMN-TKOFUNUM   (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                       TO  R95-HKOFUNUM
           MOVE    SPA-GMN-TTEKSTYMD  (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                       TO  R95-TEKSTYMD
           MOVE    "black"             TO  R95-JKYSNUM-STYLE
                                           R95-KOFUNUM-STYLE
           IF      SPA-GMN-R95-SELNUM  >   ZERO
               MOVE    SPA-GMN-R95-TJKYSNUM   (SPA-GMN-R95-SELNUM)
                                           TO  R95-JKYSNUM
               MOVE    SPA-GMN-R95-TKOFUNUM   (SPA-GMN-R95-SELNUM)
                                           TO  R95-KOFUNUM
               IF    ( SPA-GMN-TJKYSNUM (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                           =
                           SPA-GMN-R95-TJKYSNUM (SPA-GMN-R95-SELNUM)   )
               AND   ( SPA-GMN-TKOFUNUM (SPA-GMN-PAGE SPA-NAI-KOFU-IDX)
                                           =
                           SPA-GMN-R95-TKOFUNUM (SPA-GMN-R95-SELNUM)   )
                   MOVE    "blue"              TO  R95-JKYSNUM-STYLE
                                                   R95-KOFUNUM-STYLE
               ELSE     
                   MOVE    "red"               TO  R95-JKYSNUM-STYLE
                                                   R95-KOFUNUM-STYLE
               END-IF
           END-IF                                
           MOVE    SPA-GMN-R95-SELNUM  TO  R95-SELNUM
      *          
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-R95-MAX
               MOVE    IDX                 TO  WRK-Z9
               MOVE    WRK-Z9              TO  R95-BODY-TSEQNUM   (IDX)
               MOVE    SPA-GMN-R95-TFTNJANUM  (IDX)
                                           TO  R95-BODY-TFTNJANUM (IDX)
               MOVE    SPA-GMN-R95-TJKYSNUM   (IDX)
                                           TO  R95-BODY-TJKYSNUM  (IDX)
               MOVE    SPA-GMN-R95-TKOFUNUM   (IDX)
                                           TO  R95-BODY-TKOFUNUM  (IDX)
      *
               IF      IDX             =   SPA-GMN-R95-SELNUM
                   MOVE    "T"         TO  R95-BODY-LIST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  R95-BODY-LIST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-R95-MAX     TO  R95-BODY-LIST-COUNT
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
      *    カーソル指定のない時、入力した項目の次へ
           IF      SPA-GMN-R95-CUR         =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-R95-CUR
               WHEN    1
                    MOVE  "SELNUM"     TO  MCP-WIDGET 
               WHEN    98
                    MOVE  "B01"        TO  MCP-WIDGET   
               WHEN    99
                    MOVE  "B12"        TO  MCP-WIDGET   
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
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "RENNUM"
                   MOVE    99              TO  SPA-GMN-R95-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  RERR
           MOVE    SPA-ERRCD            TO  RERR-ERRCODE
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"        TO  RERR-ERRMSG
               WHEN    "0002"
                   MOVE    "対象が選択されていません。"
                                                       TO  RERR-ERRMSG
           END-EVALUATE
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R95"                TO  SPA-MOTOPG
           MOVE    "RERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  RID1
           INITIALIZE                       RID1
           MOVE    SPA-RIDCD            TO  RID1-ID1CODE
           EVALUATE    SPA-RIDCD
               WHEN    "1001"
                   MOVE    "更新します"     TO  RID1-ID1MSG
           END-EVALUATE
           EVALUATE    SPA-RIDCD
               WHEN    "1001"
                   MOVE    "戻る"           TO  RID1-B01-LABEL
           END-EVALUATE        
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R95"                TO  SPA-MOTOPG
           MOVE    "RID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-RIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       910-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_monthlynum"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC         TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
                   INITIALIZE                  MONTHLYNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
               INITIALIZE                  MONTHLYNUM-REC
           END-IF
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
