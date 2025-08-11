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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBFIXDENPNUM.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 伝票番号修復処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  24/02/19    ORCAMO        新規作成
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
      *
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                         PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル
           COPY    "CPRECEERR.INC".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-SYUNOU                          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                            PIC 9(08).
           03  CNT-JOB-UPDCNT                      PIC 9(08).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                            PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                          PIC 9(02).
               05  SYS-MM                          PIC 9(02).
               05  SYS-DD                          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                            PIC X(08).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-GAI-DENPNUM                     PIC 9(07).
           03  WRK-NYU-DENPNUM                     PIC 9(07).
      *
       01  ERR-EDIT-AREA.
           03  ERR-PTID                            PIC 9(10).
           03  ERR-PTNUM                           PIC X(20).
           03  ERR-ERRMSG                          PIC X(80).
           03  ERR-HEN-ERRMSG                      PIC X(80).
      *
       01  CONST-AREA.
           03  CONST-SHELLID           PIC X(08) VALUE "MONTHLY".
           03  CONST-JOBID             PIC 9(07) VALUE ZERO.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
           COPY    "CPQTOANY.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK0041.INC".
           COPY    "CPSK0042.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    数値変換
           COPY    "CPORCSNUM.INC".
      *
@@@@@@*    サブプログラムのコピーを追加
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
           END-UNSTRING
      *
           MOVE     WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBFIXDENPNUM"    TO  JOB-PGID
           MOVE    "伝票番号修復処理"
                                       TO  JOB-SHELLMSG
      *
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    3                   TO  JOB-STOPFLG
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    入院分の最大伝票番号を取得しシステム管理0042を更新
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                      SYUNOU-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYU-HOSPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou_main"   TO  MCP-TABLE
           MOVE    "key100"            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou_main"   TO  MCP-TABLE
               MOVE    "key100"            TO  MCP-PATHNAME
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU         =   ZERO
                   DISPLAY   "NYUIN SYU-DENPNUM = " SYU-DENPNUM
                   INITIALIZE                      SYS-0042-REC
                   MOVE    "0042"              TO  SYS-0042-KANRICD
                   MOVE    "*"                 TO  SYS-0042-KBNCD
                   MOVE    "00000000"          TO  SYS-0042-STYUKYMD
                   MOVE    SYS-0042-STYUKYMD   TO  SYS-0042-EDYUKYMD
                   MOVE    WRK-PARA-HOSPNUM    TO  SYS-0042-HOSPNUM
                   MOVE    SYS-0042-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM   910-DBSELECT-SEC
                   IF      MCP-RC     =    ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key10"             TO  MCP-PATHNAME
                       PERFORM   910-DBFETCH-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-0042-REC
                           DISPLAY   "SYS-0042-DENPNUMMAX-NYUIN = "
                                      SYS-0042-DENPNUMMAX-NYU
                           IF    SYU-DENPNUM  >  SYS-0042-DENPNUMMAX-NYU
                               DISPLAY "NYUIN DENPNUM SET"
                               MOVE   SYU-DENPNUM      TO
                                                 SYS-0042-DENPNUMMAX-NYU
                               MOVE    "fixdenpnum"
                                               TO  SYS-0042-OPID
                               PERFORM 800-MCNDATE-SEC
                               MOVE    SMCNDATE-YMD
                                               TO  SYS-0042-UPYMD
                               MOVE    SMCNDATE-HMS
                                               TO  SYS-0042-UPHMS
                               MOVE    SYS-0042-HOSPNUM
                                               TO  SYS-0042-UPD-HOSPNUM
                               MOVE    SYS-0042-KANRICD
                                               TO  SYS-0042-UPD-KANRICD
                               MOVE    SYS-0042-KBNCD
                                               TO  SYS-0042-UPD-KBNCD
                               MOVE    SYS-0042-STYUKYMD
                                               TO  SYS-0042-UPD-STYUKYMD
                               MOVE    SYS-0042-EDYUKYMD
                                               TO  SYS-0042-UPD-EDYUKYMD
                               MOVE     SYS-0042-REC
                                               TO  MCPDATA-REC
                               MOVE    "DBUPDATE"  TO  MCP-FUNC
                               MOVE    "tbl_syskanri" 
                                                   TO  MCP-TABLE
                               MOVE    "upd1"      TO  MCP-PATHNAME
                               CALL    "ORCDBMAIN"    USING  MCPAREA
                                                             MCPDATA-REC
                                                             SPA-AREA
                           END-IF
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF    
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou_main"   TO  MCP-TABLE
           MOVE    "key100"            TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    外来分の最大伝票番号を取得しシステム管理0041を更新
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                      SYUNOU-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYU-HOSPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou_main"   TO  MCP-TABLE
           MOVE    "key101"            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou_main"   TO  MCP-TABLE
               MOVE    "key101"            TO  MCP-PATHNAME
               PERFORM 910-SYUNOU-READ-SEC
               IF   FLG-SYUNOU         =   ZERO
                   DISPLAY   "GAIRAI SYU-DENPNUM = " SYU-DENPNUM
                   INITIALIZE                      SYS-0041-REC
                   MOVE    "0041"              TO  SYS-0041-KANRICD
                   MOVE    "*"                 TO  SYS-0041-KBNCD
                   MOVE    "00000000"          TO  SYS-0041-STYUKYMD
                   MOVE    SYS-0041-STYUKYMD   TO  SYS-0041-EDYUKYMD
                   MOVE    WRK-PARA-HOSPNUM    TO  SYS-0041-HOSPNUM
                   MOVE    SYS-0041-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM   910-DBSELECT-SEC
                   IF      MCP-RC     =    ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key10"             TO  MCP-PATHNAME
                       PERFORM   910-DBFETCH-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-0041-REC
                           DISPLAY   "SYS-0041-DENPNUMMAX-GAIRAI = "
                                      SYS-0041-DENPNUMMAX
                           IF    SYU-DENPNUM  >  SYS-0041-DENPNUMMAX
                               DISPLAY "GAIRAI DENPNUM SET"
                               MOVE    SYU-DENPNUM  TO
                                                 SYS-0041-DENPNUMMAX
                               MOVE    "fixdenpnum"
                                               TO  SYS-0041-OPID
                               PERFORM 800-MCNDATE-SEC
                               MOVE    SMCNDATE-YMD
                                               TO  SYS-0041-UPYMD
                               MOVE    SMCNDATE-HMS
                                               TO  SYS-0041-UPHMS
                               MOVE    SYS-0041-HOSPNUM
                                               TO  SYS-0041-UPD-HOSPNUM
                               MOVE    SYS-0041-KANRICD
                                               TO  SYS-0041-UPD-KANRICD
                               MOVE    SYS-0041-KBNCD
                                               TO  SYS-0041-UPD-KBNCD
                               MOVE    SYS-0041-STYUKYMD
                                               TO  SYS-0041-UPD-STYUKYMD
                               MOVE    SYS-0041-EDYUKYMD
                                               TO  SYS-0041-UPD-EDYUKYMD
                               MOVE    SYS-0041-REC
                                               TO  MCPDATA-REC
                               MOVE    "DBUPDATE"  TO  MCP-FUNC
                               MOVE    "tbl_syskanri" 
                                                   TO  MCP-TABLE
                               MOVE    "upd1"      TO  MCP-PATHNAME
                               CALL    "ORCDBMAIN"    USING  MCPAREA
                                                             MCPDATA-REC
                                                             SPA-AREA
                           END-IF
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF    
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou_main"   TO  MCP-TABLE
           MOVE    "key101"            TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    OUTPUT      RECEERR-FILE
      *
           MOVE    WRK-RECEERR     TO  RECEERR-REC
           WRITE   RECEERR-REC
           CLOSE   RECEERR-FILE
      *
      *    ジョブ管理終了処理
           MOVE    "JBE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECEERR     TO  JOB-YOBI
           MOVE    "9999"          TO  JOB-ERRCD
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           STOP RUN
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID    TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                     TO  JOB-SHELLID
           MOVE    WRK-PARA-HOSPNUM  TO  JOB-HOSPNUM
           CALL    "ORCSJOB"         USING
                                     ORCSJOBKANRIAREA 
                                     JOBKANRI-REC
                                     SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ読込
      *****************************************************************
       910-SYUNOU-READ-SEC        SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBSTART-EXT.
           EXIT.
