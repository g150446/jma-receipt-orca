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
       PROGRAM-ID.             ORCSRECECOMMENT.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : レセプト自動記載コメント編集サブ
      *  管理者            : 
      *  18/04/19    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-RECECOMMENT-YMD.
               05  FILLER          PIC X(06).
               05  WRK-RECECOMMENT-DD
                                   PIC X(02).
           03  WRK-YMD-1.
               05  WRK-YM-1        PIC X(16).
               05  FILLER          PIC X(06).
           03  WRK-YMD-2.
               05  FILLER          PIC X(10).
               05  WRK-YM-2        PIC X(04).
           03  WRK-NUM-9           PIC -----9.999999.
           03  WRK-NUM-X           REDEFINES   WRK-NUM-9.
               05  FILLER          PIC X(07).
               05  WRK-NUM-X-1     PIC X(06).
           03  WRK-NUM             PIC X(26).
           03  WRK-ST-IDX          PIC 9(02).
           03  WRK-ED-IDX          PIC 9(02).
           03  WRK-LEN             PIC 9(02).
      *    点数マスタ用基準日
           03  WRK-TNS-YUKOSTYMD   PIC X(08).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "CPORCSRECECOMMENT.INC".
      *
      *****************************************************************
      *
       PROCEDURE                   DIVISION    USING
                                   ORCSRECECOMMENTAREA
                                   SPA-AREA.
       *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *  DISPLAY "LNK-RECECOMMENT-KBN     =" LNK-RECECOMMENT-KBN
      *  DISPLAY "LNK-RECECOMMENT-SRYCD   =" LNK-RECECOMMENT-SRYCD
      *  DISPLAY "LNK-RECECOMMENT-YMD     =" LNK-RECECOMMENT-YMD
      *  DISPLAY "LNK-RECECOMMENT-COMMENT =" LNK-RECECOMMENT-COMMENT
      *  DISPLAY "LNK-RECECOMMENT-NUM     =" LNK-RECECOMMENT-NUM
      *  DISPLAY "LNK-RECECOMMENT-SRYYMD  =" LNK-RECECOMMENT-SRYYMD
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO            TO  LNK-RECECOMMENT-RC
           INITIALIZE                  ORCSRECECOMMENT-OUT-AREA
      *
      *    処理区分チェック
           EVALUATE    LNK-RECECOMMENT-KBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
               WHEN    "4"
               WHEN    "5"
                   CONTINUE
               WHEN    OTHER
                   MOVE    1               TO  LNK-RECECOMMENT-RC
                   MOVE    1                   TO  FLG-END
                   GO  TO  100-INIT-EXT
           END-EVALUATE
      *
      *    基準日チェック
           IF      LNK-RECECOMMENT-SRYYMD
                                       =   SPACE
               MOVE    6                   TO  LNK-RECECOMMENT-RC
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    LNK-RECECOMMENT-SRYYMD
                                           TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               IF    ( STS-DAY-RC1         =   ZERO )
               AND   ( STS-DAY-RC2         =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    6                   TO  LNK-RECECOMMENT-RC
               END-IF
           END-IF
      *
           IF      LNK-RECECOMMENT-RC  =   ZERO
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-END
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    診療行為コードチェック
           IF      LNK-RECECOMMENT-SRYCD
                                       =   SPACE
               MOVE    2                   TO  LNK-RECECOMMENT-RC
           ELSE
               EVALUATE    LNK-RECECOMMENT-KBN
                   WHEN    "1"
                   WHEN    "2"
                       IF      LNK-RECECOMMENT-SRYCD (1:4)
                                           =   "8501"
                           CONTINUE
                       ELSE
                           MOVE    2           TO  LNK-RECECOMMENT-RC
                       END-IF
                   WHEN    "3"
                       IF      LNK-RECECOMMENT-SRYCD (1:4)
                                           =   "8301"
                           CONTINUE
                       ELSE
                           MOVE    2           TO  LNK-RECECOMMENT-RC
                       END-IF
                   WHEN    "4"
                       IF      LNK-RECECOMMENT-SRYCD (1:4)
                                           =   "8421"
                           CONTINUE
                       ELSE
                           MOVE    2           TO  LNK-RECECOMMENT-RC
                       END-IF
                   WHEN    "5"
                       IF      LNK-RECECOMMENT-SRYCD (1:4)
                                           =   "8201"
                           CONTINUE
                       ELSE
                           MOVE    2           TO  LNK-RECECOMMENT-RC
                       END-IF
               END-EVALUATE
               IF      LNK-RECECOMMENT-RC
                                       =   ZERO
                   MOVE    LNK-RECECOMMENT-SRYYMD
                                           TO  WRK-TNS-YUKOSTYMD
                   PERFORM 900-TENSU-READ-SEC
                   IF      FLG-TENSU           =   1
      *                基準日が２０２０年４月から６月のとき７月で検索
                       IF    ( LNK-RECECOMMENT-SRYYMD
                                               >=  "20200401" )
                       AND   ( LNK-RECECOMMENT-SRYYMD
                                               <=  "20200630" )
                           MOVE    "20200701"      TO  WRK-TNS-YUKOSTYMD
                           PERFORM 900-TENSU-READ-SEC
                       END-IF
                   END-IF
                   IF      FLG-TENSU           =   1
                       MOVE    7               TO  LNK-RECECOMMENT-RC
                   END-IF
               END-IF
           END-IF
      *
           IF      LNK-RECECOMMENT-RC  =   ZERO
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-END
               GO  TO  100-INIT-EXT
           END-IF
      *
           EVALUATE    LNK-RECECOMMENT-KBN
               WHEN    "1"
               WHEN    "2"
      *            日付チェック
                   IF      LNK-RECECOMMENT-YMD
                                               =   SPACE
                       MOVE    3               TO  LNK-RECECOMMENT-RC
                   ELSE
                       MOVE    LNK-RECECOMMENT-YMD
                                               TO  WRK-RECECOMMENT-YMD
                       IF      LNK-RECECOMMENT-KBN
                                               =   "2"
                           MOVE    "01"        TO  WRK-RECECOMMENT-DD
                       END-IF
      *
                       INITIALIZE                      STS-AREA-DAY
                       INITIALIZE                      LNK-DAY2-AREA
                       MOVE    "21"                TO  LNK-DAY2-IRAI
                       MOVE    WRK-RECECOMMENT-YMD TO  LNK-DAY2-YMD
                       CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                           LNK-DAY2-AREA
                       IF    ( STS-DAY-RC1         =   ZERO )
                       AND   ( STS-DAY-RC2         =   ZERO )
                           MOVE     LNK-DAY2-EDTYMD3   TO  WRK-YMD-1
                       END-IF
      *
                       INITIALIZE                      STS-AREA-DAY
                       INITIALIZE                      LNK-DAY1-AREA
                       MOVE    "11"                TO  LNK-DAY1-IRAI
                       MOVE    WRK-RECECOMMENT-YMD TO  LNK-DAY1-YMD
                       CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                           LNK-DAY1-AREA
                       IF    ( STS-DAY-RC1         =   ZERO )
                       AND   ( STS-DAY-RC2         =   ZERO )
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "2"                 TO  KANACHK-SYORI
                           MOVE    LNK-DAY1-YMD (2:)   TO
                                                       KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"       USING
                                                       ORCSKANACHKAREA
                           IF      KANACHK-RC          =   ZERO
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                       TO  WRK-YMD-2
                               IF      LNK-RECECOMMENT-KBN
                                                       =   "2"
                                   MOVE    "００"          TO  WRK-YM-2
                               END-IF
                           END-IF
                       ELSE
                           MOVE    3               TO
                                                   LNK-RECECOMMENT-RC
                       END-IF
                   END-IF    
               WHEN    "3"
      *            コメントチェック
                   IF      LNK-RECECOMMENT-COMMENT
                                               =   SPACE
                       MOVE    4               TO  LNK-RECECOMMENT-RC
                   ELSE
                       INITIALIZE                      ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI
                       MOVE    LNK-RECECOMMENT-COMMENT
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                       ORCSKANACHKAREA
                       IF    ( KANACHK-RC          =   ZERO )
                       AND   ( KANACHK-SYUBETU     =   "2"  )
                           CONTINUE
                       ELSE
                           MOVE    4               TO
                                                   LNK-RECECOMMENT-RC
                       END-IF
                   END-IF
               WHEN    "4"
      *            数値チェック
                   MOVE    LNK-RECECOMMENT-NUM TO  WRK-NUM-9
                   IF      WRK-NUM-X-1     =   "000000"
                       MOVE    6               TO  IDY
                   ELSE
                       MOVE    13              TO  IDY
                   END-IF
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       IDY
                       IF      WRK-NUM-9 (IDX:1)
                                           NOT =   SPACE
                           MOVE    IDX             TO  WRK-ST-IDX
                           MOVE    IDY             TO  IDX
                       END-IF
                   END-PERFORM
                   IF      WRK-NUM-X-1     =   "000000"
                       MOVE    6               TO  WRK-ED-IDX
                   ELSE
                       PERFORM VARYING IDX FROM    13  BY  -1
                               UNTIL   IDX <       8
                           IF      WRK-NUM-9 (IDX:1)
                                               NOT =   "0"
                               MOVE    IDX             TO  WRK-ED-IDX
                               MOVE    7               TO  IDX
                           END-IF
                       END-PERFORM
                   END-IF
                   COMPUTE WRK-LEN =   WRK-ED-IDX  -   WRK-ST-IDX  +   1
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"         TO  KANACHK-SYORI
                   MOVE    WRK-NUM-9 (WRK-ST-IDX:WRK-LEN)
                                       TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING
                                           ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-NUM
                   ELSE
                       MOVE    5               TO  LNK-RECECOMMENT-RC
                   END-IF
           END-EVALUATE
      *
           IF      LNK-RECECOMMENT-RC    =   ZERO
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    基準日が２０２０年４月から６月で正式名称があるとき
           IF    ( LNK-RECECOMMENT-SRYYMD
                                   >=  "20200401" )
           AND   ( LNK-RECECOMMENT-SRYYMD
                                   <=  "20200630" )
           AND   ( TNS-FORMALNAME
                               NOT =   SPACE      )
               MOVE    "810000001"     TO  LNK-RECECOMMENT-SRYCD
           END-IF
      *
           EVALUATE    LNK-RECECOMMENT-KBN
               WHEN    "1"
                   IF      TNS-FORMALNAME      =   SPACE
                       STRING    TNS-NAME  DELIMITED   BY  SPACE
                                 "；"      DELIMITED   BY  SIZE
                                 WRK-YMD-1 DELIMITED   BY  SPACE
                                           INTO    LNK-RECECOMMENT-NAME
                       END-STRING
                   ELSE
                       STRING    TNS-FORMALNAME
                                           DELIMITED   BY  SPACE
                                 "；"      DELIMITED   BY  SIZE
                                 WRK-YMD-1 DELIMITED   BY  SPACE
                                           INTO    LNK-RECECOMMENT-NAME
                       END-STRING
                   END-IF
                   MOVE    WRK-YMD-2       TO
                                           LNK-RECECOMMENT-INPUTCOMMENT
               WHEN    "2"
                   IF      TNS-FORMALNAME      =   SPACE
                       STRING    TNS-NAME  DELIMITED   BY  SPACE
                                 "；"      DELIMITED   BY  SIZE
                                 WRK-YM-1  DELIMITED   BY  SPACE
                                           INTO    LNK-RECECOMMENT-NAME
                       END-STRING
                   ELSE
                       STRING    TNS-FORMALNAME
                                           DELIMITED   BY  SPACE
                                 "；"      DELIMITED   BY  SIZE
                                 WRK-YM-1  DELIMITED   BY  SPACE
                                           INTO    LNK-RECECOMMENT-NAME
                       END-STRING
                   END-IF
                   MOVE    WRK-YMD-2       TO
                                           LNK-RECECOMMENT-INPUTCOMMENT
               WHEN    "3"
                   IF      TNS-FORMALNAME      =   SPACE
                       STRING    TNS-NAME  DELIMITED   BY  SPACE
                                 LNK-RECECOMMENT-COMMENT
                                           DELIMITED   BY  SPACE
                                           INTO    LNK-RECECOMMENT-NAME
                       END-STRING
                   ELSE
                       STRING    TNS-FORMALNAME
                                           DELIMITED   BY  SPACE
                                 LNK-RECECOMMENT-COMMENT
                                           DELIMITED   BY  SPACE
                                           INTO    LNK-RECECOMMENT-NAME
                       END-STRING
                   END-IF
                   MOVE    LNK-RECECOMMENT-COMMENT
                                           TO
                                           LNK-RECECOMMENT-INPUTCOMMENT
               WHEN    "4"
                   IF      TNS-FORMALNAME      =   SPACE
                       STRING    TNS-NAME  DELIMITED   BY  SPACE
                                 "；"      DELIMITED   BY  SIZE
                                 WRK-NUM   DELIMITED   BY  SPACE
                                           INTO    LNK-RECECOMMENT-NAME
                       END-STRING
                   ELSE
                       STRING    TNS-FORMALNAME
                                           DELIMITED   BY  SPACE
                                 "；"      DELIMITED   BY  SIZE
                                 WRK-NUM   DELIMITED   BY  SPACE
                                           INTO    LNK-RECECOMMENT-NAME
                       END-STRING
                   END-IF
                   MOVE    WRK-NUM         TO
                                           LNK-RECECOMMENT-INPUTCOMMENT
               WHEN    "5"
                   IF      TNS-FORMALNAME      =   SPACE
                       MOVE    TNS-NAME        TO  LNK-RECECOMMENT-NAME
                   ELSE
                       MOVE    TNS-FORMALNAME  TO  LNK-RECECOMMENT-NAME
                   END-IF
                   MOVE    LNK-RECECOMMENT-NAME
                                           TO
                                           LNK-RECECOMMENT-INPUTCOMMENT
           END-EVALUATE
      *
      *    名称文字数
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    LNK-RECECOMMENT-NAME
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-RC2         =   ZERO )
           AND   ( KANACHK-SYUBETU     =   "2"  )
               COMPUTE LNK-RECECOMMENT-YUKOKETA
                                           =   KANACHK-MAX /   2
           ELSE
               MOVE    8                   TO  LNK-RECECOMMENT-RC
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *     DISPLAY "LNK-RECECOMMENT RC          =" LNK-RECECOMMENT-RC
      *     DISPLAY "LNK-RECECOMMENT-NAME        =" LNK-RECECOMMENT-NAME
      *     DISPLAY "LNK-RECECOMMENT-INPUTCOMMENT=" 
      *                                     LNK-RECECOMMENT-INPUTCOMMENT
      *     DISPLAY "LNK-RECECOMMENT-YUKOKETA    ="
      *                                     LNK-RECECOMMENT-YUKOKETA
      *
           .
       300-END-EXT.
           EXIT. 
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       900-TENSU-READ-SEC           SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    LNK-RECECOMMENT-SRYCD
                                       TO  TNS-SRYCD
           MOVE    WRK-TNS-YUKOSTYMD   TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
