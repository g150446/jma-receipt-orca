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
       PROGRAM-ID.             ORAPI007BR1V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次
      *  コンポーネント名  : 統計ＸＭＬデータ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  21/03/01    NMED−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  XMLERR-FILE  ASSIGN  XMLERR
                                   FILE    STATUS  IS  STS-XMLERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  XMLERR-FILE.
       01  XMLERR-REC              PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //XMLERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-XMLERR          PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TOUKEICSV-B     PIC 9(01).
      *
           03  FLG-DATAEND         PIC 9(01).
           03  FLG-MATCH           PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-AREA1.
               05  FLG-COMMA       PIC 9(01).
               05  FLG-DELI1       PIC 9(01).
               05  FLG-DELI2       PIC 9(01).
               05  FLG-NODATA      PIC 9(01).
           03  FLG-LENGTH-OVERFLOW PIC 9(01).
           03  FLG-OVERFLOW        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(10).
           03  CNT-OUT1            PIC 9(10).
           03  CNT-XMLCOUNT        PIC 9(06).
      *    ＣＳＶ項目数
           03  SPLIT-CNT           PIC 9(03).
           03  SPLIT-CNT1          PIC 9(03).
      *
       01  INDEX-AREA.
           03  IDX                     PIC 9(05).
           03  IDY                     PIC 9(05).
           03  IDZ                     PIC 9(03).
           03  IDW                     PIC 9(03).
           03  IDE                     PIC 9(03).
           03  IDXX                    PIC 9(03).
           03  IDYY                    PIC 9(05).
           03  IDZZ                    PIC 9(05).
      *
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM        PIC 9(02).
               05  WRK-PARA-TBL-KEY        PIC X(8).
               05  WRK-PARA-TBL-GROUP      PIC X(14).
               05  WRK-PARA-SHORI-RENNUM   PIC 9(04).
               05  WRK-PARA-RENNUM         PIC 9(04).
               05  WRK-PARA-TO-DATA        PIC X(200).
               05  WRK-PARA-JOBID          PIC 9(07).
               05  WRK-PARA-SHELLID        PIC X(08).
               05  WRK-PARA-TITLE          PIC X(100).
               05  WRK-PARA-TO-DATA1       PIC X(200).
      *
           03  WRK-JOBNAME         PIC X(40).
       *
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-WK              PIC 9(04).
      *
           03  WRK-PARA-REC.
               05  WRK-PARA-X          PIC X(01)  OCCURS   20000.
      *
           03 WRK-CHK-PARA             PIC X(110).
      *
       01  WRK-ERR-AREA.
           03  WRK-XMLERR              PIC X(200). 
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *
      *    統計データ
       01  TOUKEILST-REC.
           03  TOUKEILST-OCC           OCCURS   299.
               05  TOUKEILST-DATA      PIC X(100)    OCCURS   100.
       01  TOUKEILST-OVERFLOW          PIC X(04).
      *   
           COPY    "CMJOBNAME.INC".
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-SPLIT-MAX      PIC 9(03)   VALUE   101.
           03  WRK-CONS-LENGTH-MAX     PIC 9(03)   VALUE   101.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  TOUKEICSV-B-REC.
           COPY    "CPTOUKEICSV-B.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "MONBLOB".
           COPY    "XMLDATA.INC".
           COPY    "ORCA-BLOB".
      *
           COPY    "CPORCSAPIFILE.INC".
      *
      *****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
      *****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
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
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPNUM
                                               WRK-PARA-TBL-KEY
                                               WRK-PARA-TBL-GROUP
                                               WRK-PARA-SHORI-RENNUM
                                               WRK-PARA-RENNUM
                                               WRK-PARA-TO-DATA 
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-TITLE
                                               WRK-PARA-TO-DATA1
                                               XMLERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    XMLERR          TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  XMLERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  XMLERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORAPI007BR1V3" TO  JOB-PGID
      *
           IF      WRK-PARA-TITLE  =   SPACE
               MOVE    SPACE           TO  WRK-JOBNAME
               SET     TBL-IDX         TO  1
               SEARCH      MNAME-TBL-OCC   VARYING   TBL-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-JOBNAME
                   WHEN    WRK-PARA-TBL-KEY    =   MNAME-SHELLID
                                                               (TBL-IDX)
                       MOVE    MNAME-JOBNAME (TBL-IDX)
                                           TO  WRK-JOBNAME
               END-SEARCH
               STRING  WRK-JOBNAME     DELIMITED  BY  SPACE 
                       "ＸＭＬ作成"    DELIMITED  BY  SIZE
                                       INTO       JOB-SHELLMSG
               END-STRING
           ELSE
               MOVE    WRK-PARA-TITLE  TO  JOB-SHELLMSG
           END-IF
           PERFORM 900-CALL-ORCSJOB-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-TOUKEICSV-B-START-SEC
      *
           PERFORM     UNTIL   FLG-TOUKEICSV-B =   1
               MOVE    ZERO                TO  CNT-OUT1
      *
               MOVE    SPACE               TO  TOUKEILST-REC
               INITIALIZE                      TOUKEILST-REC
      *
               PERFORM         UNTIL   FLG-TOUKEICSV-B =   1
                               OR      CNT-OUT1        >=  299
      *           ＣＳＶデータ取り込み処理
                  PERFORM 2001-PARA-HENSYU-SEC
               END-PERFORM
      *
               ADD     1                   TO  CNT-XMLCOUNT
               PERFORM 910-XMLEXPORT-SEC
           END-PERFORM
      *
           MOVE    "tbl_toukeicsv_b"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ＣＳＶデータ取り込み処理
      *****************************************************************
       2001-PARA-HENSYU-SEC            SECTION.
      *
           ADD     1                   TO  CNT-OUT1
                                           CNT-OUT
      *
           INSPECT TOUKEICSV-B-CSVDATA REPLACING   ALL X"0A"   BY  " "
                                                   ALL X"0D"   BY  " "
           MOVE    TOUKEICSV-B-CSVDATA
                                   TO  WRK-PARA-REC
      *
           MOVE    WRK-CONS-SPLIT-MAX
                                   TO  SPLIT-CNT
                                       SPLIT-CNT1
           ADD     1               TO  SPLIT-CNT1
      *
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  IDE
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   SPLIT-CNT )
                   OR      ( WRK-PO-ED     >=  20000     )
                   OR      ( FLG-DATAEND   =   1         )
      *
               MOVE    ZERO            TO  FLG-AREA1
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
      *
               IF    ( WRK-PARA-REC (WRK-PO-ST:)
                                       =   SPACE )
               OR    ( FLG-DATAEND     =   1     )
                   MOVE    1               TO  FLG-DATAEND
                   MOVE    WRK-PO-ST       TO  WRK-PO-ED
               ELSE
                   ADD     1               TO  WRK-KOUMOKU-NO
      *            項目終了文字後のカンマ位置を取得(WRK-PO-ED)
                   PERFORM 20011-CHAR-END-GET-SEC
               END-IF
      *        項目共通処理
               PERFORM 20012-KOUMOKU-KYOTU-SEC
      *        ＸＭＬ編集処理
               IF      FLG-DATAEND     =   ZERO
                   PERFORM 20013-KOUMOKU-BETU-SEC
               END-IF
           END-PERFORM
      *
           PERFORM 900-TOUKEICSV-B-READ-SEC
           .
       2001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象項目の文字終了位置を取得
      *****************************************************************
       20011-CHAR-END-GET-SEC          SECTION.
      *
           MOVE    WRK-PO-ST           TO  WRK-PO-ED
      *
           PERFORM VARYING   IDYY   FROM   WRK-PO-ST   BY  1
                   UNTIL   ( IDYY          >=  20000 )
                   OR      ( FLG-COMMA     =   1     )
      *        カンマの位置
               IF  (   WRK-PARA-X (IDYY)   =   ","              )  OR
                   ( ( IDX                 =   SPLIT-CNT  OR
                                               SPLIT-CNT1 ) AND
                     ( WRK-PARA-X (IDYY)   =   SPACE      )     )
                   MOVE    IDYY                TO  WRK-PO-ED
                   MOVE    1                   TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
      *            IF      WRK-PARA-X (IDYY)
      *                                NOT =   SPACE
      *                MOVE    IDYY            TO  WRK-PO-ED
      *            空白のみの場合の終了位置
                   IF      WRK-PARA-REC (IDYY:)
                                       =   SPACE
                       MOVE    IDYY            TO  WRK-PO-ED
                       MOVE    1               TO  FLG-COMMA
                   END-IF
               END-IF
      *        デリミタ(")存在するかどうか
               IF      WRK-PARA-X (IDYY)   =   """"
                   IF      FLG-DELI1           =   ZERO
                       MOVE    1                   TO  FLG-DELI1
                   ELSE
                       MOVE    1                   TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20011-CHAR-END-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目共通処理
      *****************************************************************
       20012-KOUMOKU-KYOTU-SEC         SECTION.
      *
           MOVE    ZERO            TO  WRK-MOVE-AREA
      *
      *    データが省略されている場合、FLG-NODATAに1をセット
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   1
               END-IF
               IF      WRK-WK          =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  20012-KOUMOKU-KYOTU-EXT
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF      WRK-PO-ST       =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  20012-KOUMOKU-KYOTU-EXT
               END-IF
           END-IF
      *
      *    転送開始位置・転送桁数を算出
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               COMPUTE WRK-MO-ST       =   WRK-PO-ST   +   1
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-MO-CNT  =   WRK-PO-ED
                                       -  (WRK-PO-ST   +   2)
               ELSE
                   COMPUTE WRK-MO-CNT  =  (WRK-PO-ED   -   1)
                                       -   WRK-PO-ST
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               MOVE    WRK-PO-ST       TO  WRK-MO-ST
               COMPUTE WRK-MO-CNT      =   WRK-PO-ED  -  WRK-PO-ST
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT) =   X"0D" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT) =   X"0A" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           .
       20012-KOUMOKU-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＸＭＬ編集処理
      *****************************************************************
       20013-KOUMOKU-BETU-SEC          SECTION.
      *
      *    項目数チェック
           IF      WRK-KOUMOKU-NO  >=  SPLIT-CNT
               MOVE    1               TO  FLG-OVERFLOW
               GO  TO  20013-KOUMOKU-BETU-EXT
           END-IF
      *
           IF      WRK-MO-CNT      >   ZERO
               MOVE   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO
                              TOUKEILST-DATA (CNT-OUT1 WRK-KOUMOKU-NO)
      *        桁数チェック
               MOVE   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                       TO  WRK-CHK-PARA
               IF      WRK-CHK-PARA (WRK-CONS-LENGTH-MAX:)
                                   NOT =   SPACE
                   MOVE    1               TO  FLG-LENGTH-OVERFLOW
               END-IF
           END-IF
      *
           .
       20013-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT 処理
      *****************************************************************
       910-XMLEXPORT-SEC               SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE   TOUKEILST-REC    TO  XMLDATA-DATA
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_tdatares"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "tdatares"      TO  XMLDATA-RECORDNAME
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           INITIALIZE                   ORCA-BLOB
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    XMLDATA-OBJECT  TO  ORCA-BLOB-OBJECT
           INITIALIZE                  SGETTEMP-AREA
           STRING  WRK-PARA-TO-DATA1
                                   DELIMITED   BY  SPACE
                   CNT-XMLCOUNT    DELIMITED   BY  SIZE
                   ".xml"          DELIMITED   BY  SIZE
                                   INTO        SGETTEMP-BASENAMES  (1)
           END-STRING
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ORCA-BLOB-FILE
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       ORCA-BLOB
           .
       910-XMLEXPORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   XMLERR-FILE
           IF      STS-XMLERR          =   ZERO
               CLOSE   XMLERR-FILE
           ELSE
               OPEN    OUTPUT              XMLERR-FILE
      *
               MOVE    WRK-XMLERR          TO  XMLERR-REC
               WRITE   XMLERR-REC
               CLOSE   XMLERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-XMLERR          TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORAPI007BR1V3 "    DELIMITED  BY  SIZE
                   WRK-XMLERR          DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                    SECTION.
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-XMLERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               INITIALIZE                 ORCSAPIFILEAREA
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-IN-DIRNAME
               MOVE    WRK-PARA-TO-DATA1
                                       TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               move    "2"             TO  SAPIFILE-IN-KBN
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    WRK-PARA-TO-DATA
                                       TO  SAPIFILE-OT-FILENAME
               CALL    "ORCSAPIFILE"   USING   ORCSAPIFILEAREA
      *
               IF      SAPIFILE-RC     =   0
                   CONTINUE
               ELSE
                   STRING   "ｘｍｌファイル作成エラー RC="
                                           DELIMITED   BY  SIZE
                            SAPIFILE-RC    DELIMITED   BY  SIZE
                                           INTO    WRK-XMLERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           MOVE    FLG-OVERFLOW    TO  JOB-UPDCNT1
           MOVE    FLG-LENGTH-OVERFLOW
                                   TO  JOB-UPDCNT2
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "xml     CNT-OUT:" CNT-OUT
           DISPLAY "       OVERFLOW:" FLG-OVERFLOW
           DISPLAY "LENGTH-OVERFLOW:" FLG-LENGTH-OVERFLOW
           DISPLAY "*** ORAPI007BR1V3 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ更新処理
      *****************************************************************
       3001-TOUKEICSV-UPDATE-SEC       SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "UPD"           TO  STOUKEICSV-MODE
           MOVE    WRK-PARA-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    WRK-PARA-RENNUM TO  STOUKEICSV-RENNUM
           MOVE    WRK-PARA-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    WRK-PARA-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                       TO  WRK-XMLERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       3001-TOUKEICSV-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ明細ＤＢ読込
      *****************************************************************
       900-TOUKEICSV-B-START-SEC       SECTION.
      *
           MOVE    SPACE               TO  TOUKEICSV-B-REC
           INITIALIZE                      TOUKEICSV-B-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-B-HOSPNUM
           MOVE    WRK-PARA-TBL-KEY    TO  TOUKEICSV-B-TBL-KEY
           MOVE    WRK-PARA-TBL-GROUP  TO  TOUKEICSV-B-TBL-GROUP
           MOVE    WRK-PARA-SHORI-RENNUM
                                       TO  TOUKEICSV-B-SHORI-RENNUM
           MOVE    WRK-PARA-RENNUM     TO  TOUKEICSV-B-RENNUM
      *
           MOVE    TOUKEICSV-B-REC     TO  MCPDATA-REC
           MOVE    "tbl_toukeicsv_b"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-TOUKEICSV-B-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TOUKEICSV-B
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-TOUKEICSV-B-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ明細ＤＢ読込
      *****************************************************************
       900-TOUKEICSV-B-READ-SEC        SECTION.
      *
           MOVE    "tbl_toukeicsv_b"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TOUKEICSV-B-REC
               MOVE    ZERO                TO  FLG-TOUKEICSV-B
           ELSE
               MOVE    1                   TO  FLG-TOUKEICSV-B
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-TOUKEICSV-B-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-XMLERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORAPI007BR1V3 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"          USING   WRK-ERRMSG
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                 SECTION.
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
       900-CLOSE-SEC                   SECTION.
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
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
