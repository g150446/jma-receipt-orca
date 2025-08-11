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
       PROGRAM-ID.             ORCBSETUPYJ.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 薬剤情報マスタ設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/09/25    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-太田    07.09.20  グループ診療対応
      *  04.08.00    NACL-伊藤    14.11.17  ver4.8.0対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    薬情一時ファイル(SEQUENTIAL)
           SELECT  SETUPYJ01-FILE  ASSIGN   SETYJ01
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE     STATUS IS  STS-SETUPYJ01.
      *
      *    薬情一時ファイル(INDEX)
           SELECT  SETUPYJ02-FILE  ASSIGN   SETYJ02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS   MODE   IS  DYNAMIC
                                   RECORD   KEY    IS  SETUPYJ02-KEY
                                   FILE     STATUS IS  STS-SETUPYJ02.
      *
      *    診療行為コードワーク
           SELECT  SRYCD-FILE      ASSIGN   SETYJ03
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS   MODE   IS  DYNAMIC
                                   RECORD   KEY    IS  SRYCD-KEY
                                   FILE     STATUS IS  STS-SRYCD.
      *
      *    確認リスト
           SELECT  HCMSL80-FILE    ASSIGN   SETYJ04
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-HCMSL80.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    薬情一時ファイル(SEQUENTIAL)
       FD  SETUPYJ01-FILE.
           COPY    "CPYAKUJYOTEMP.INC"
                                           REPLACING   //TEMP-//
                                           BY          //SETUPYJ01-//.
      *
      *    薬情一時ファイル(INDEX)
       FD  SETUPYJ02-FILE.
           COPY    "CPYAKUJYOTEMP.INC"
                                           REPLACING   //TEMP-//
                                           BY          //SETUPYJ02-//.
      *
      *    診療行為コードワーク
       FD  SRYCD-FILE.
       01  SRYCD-REC.
           03  SRYCD-KEY           PIC X(09).
           03  SRYCD-FLG           PIC X(01).
      *
      *    確認リスト
       FD  HCMSL80-FILE.
           COPY    "HCMSL80.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
       01  FILE-AREA.
           03  SETYJ01                     PIC X(256).
           03  SETYJ02                     PIC X(256).
           03  SETYJ03                     PIC X(256).
           03  SETYJ04                     PIC X(256).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
           03  STS-SETUPYJ01               PIC X(02).
           03  STS-SETUPYJ02               PIC X(02).
           03  STS-SRYCD                   PIC X(02).
           03  STS-HCMSL80                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-INPUTCD                         PIC 9(01).
           03  FLG-SRYACT                          PIC 9(01).
           03  FLG-YAKUJYO                         PIC 9(01).
           03  FLG-SRYCD                           PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-HCMSL80                         PIC 9(01).
           03  FLG-EXIST                           PIC 9(01).
           03  FLG-SETUPYJ01                       PIC 9(01).
           03  FLG-SETUPYJ02                       PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-END                             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SRYCD                           PIC 9(08).
           03  CNT-LINE                            PIC 9(08).
           03  CNT-PAGE                            PIC 9(08).
           03  CNT-OUTPUT                          PIC 9(08).
           03  CNT-SYORI                           PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDXE                                PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  RECEERR                             PIC X(100).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYCD                           PIC X(09).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ZZZZ9-G.
               05  WRK-ZZZZ9                       PIC ZZZZ9.
           03  WRK-TNS-SRYCD                       PIC X(09).
           03  WRK-TNS-YMD                         PIC X(08).
           03  WRK-BYOMEICD                        PIC X(07).
           03  WRK-MEMO                            PIC X(500).
           03  WRK-TITLE                           PIC X(100).
           03  WRK-STR                             PIC X(200).
           03  WRK-LEN                             PIC 9(05).
           03  WRK-SHO                             PIC 9(05).
           03  WRK-AMARI                           PIC 9(05).
           03  WRK-MAE-INPUT                       PIC X(300).
           03  WRK-OUT-INPUT                       PIC X(300).
           03  WRK-OUT-MAX                         PIC 9(05).
           03  WRK-STR-LEFT                        PIC X(500).
           03  WRK-STR-LEFT-Z      REDEFINES       WRK-STR-LEFT.
               05  WRK-STR-LEFT-ZX99               PIC Z(9)9.
               05                                  PIC X(490).
           03  WRK-STR-LEFT-TMP                    PIC X(500).
      *        全角編集用項目
           03  WRK-N.
               05  WRK-KNS-N                       PIC X(10).
               05  WRK-TIME-N                      PIC X(16).
           03  WRK-MESSAGE                         PIC X(100).
           03  WRK-PATH                            PIC X(256).
           03  WRK-OUTPATH                         PIC X(100).
           03  WRK-FILENAME                        PIC X(255).
           03  WRK-ITEMIN                          PIC X(10000).
           03  WRK-ITEMOT                          PIC X(10000).
           03  WRK-IMAGEKBN                        PIC 9(01).
      *
       01  CONST-AREA.
           03  CONST-MKCD-MAX                  PIC 9(05)   VALUE 21.
           03  CONST-LINE-MAX                  PIC 9(05)   VALUE 80.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    入力コード
       01  ICD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    薬剤情報マスタ
           COPY    "CPYAKUJYO.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ファイルコピーサブ
      *     COPY    "CPORCSFCP.INC"     REPLACING  //IF-//
      *                                 BY         //IFCP-//.
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *    画像イメージ作成パラメタ
           COPY    "CPORCSMIAREA.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA.
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  WRK-PARA
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
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "薬剤情報マスタ更新"
                                       TO  WRK-TITLE
      *
      *    ステップ管理開始処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBSETUPYJ"       TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
                                           JOB-YOBI
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "meddata.txt"       TO  SGETTEMP-BASENAMES (1)
           MOVE    "medphotodata"      TO  SGETTEMP-BASENAMES (2)
           MOVE    "SETYJ02"           TO  SGETTEMP-BASENAMES (3)
           MOVE    "SETYJ03"           TO  SGETTEMP-BASENAMES (4)
           MOVE    "SETYJ04"           TO  SGETTEMP-BASENAMES (5)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  SETYJ01
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  WRK-PATH
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  SETYJ02
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  SETYJ03
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  SETYJ04
      *
      *    INITIALIZE                      SGETTEMP-AREA
      *    MOVE    RECEERR             TO  SGETTEMP-BASENAMES (1)
      *    CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *    MOVE    SGETTEMP-FULLNAMES (1)
      *                                TO  RECEERR
      *
           PERFORM 2004-HEAD-HEN-SEC
      *
           OPEN    OUTPUT              HCMSL80-FILE
      *
           OPEN    OUTPUT              SRYCD-FILE
           CLOSE   SRYCD-FILE
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 1001-SETUPYJ02-EDIT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       1001-SETUPYJ02-EDIT-SEC         SECTION.
      *
           OPEN    INPUT               SETUPYJ01-FILE
           OPEN    OUTPUT              SETUPYJ02-FILE
      *
           PERFORM 900-SETUPYJ01-READ-SEC
      *
           PERFORM UNTIL ( FLG-SETUPYJ01   NOT =   ZERO )
      *
               MOVE    SETUPYJ01-REC   TO  SETUPYJ02-REC
               MOVE    ZERO            TO  SETUPYJ02-HOSPNUM
                                           SETUPYJ02-TERMID
                                           SETUPYJ02-YJCD
                                           SETUPYJ02-RENNUM
               WRITE   SETUPYJ02-REC
      *
               PERFORM 900-SETUPYJ01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE       SETUPYJ01-FILE
                       SETUPYJ02-FILE
      *
           .
       1001-SETUPYJ02-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    入力コード有を対象に薬剤情報情報登録
           PERFORM 2001-ICD-TO-YAKUJYO-SEC
      *
      *    診療行為マスタより対象診療コード抽出
           PERFORM 2002-SRYACT-OUT-SEC
      *    対象診療コードより薬剤情報情報登録
           PERFORM 2002-SRYACT-TO-YAKUJYO-SEC
      *
      *    確認ファイル出力処理
           PERFORM 2004-HCMSL80-OUT-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入力コード対象処理
      *****************************************************************
       2001-ICD-TO-YAKUJYO-SEC         SECTION.
      *
           OPEN    I-O                 SRYCD-FILE
      *
           PERFORM 900-ICD-ALL-SEL-SEC
      *
           PERFORM UNTIL ( FLG-INPUTCD NOT =   ZERO )
      *
             IF  ICD-SRYCD(1:1)        =   "6"
                 MOVE    ICD-SRYCD     TO  WRK-SRYCD
                 PERFORM 900-SRYCD-KREAD-SEC
                 IF  FLG-SRYCD         =   1
                     MOVE  WRK-SRYCD   TO  SRYCD-KEY
                     MOVE  "1"         TO  SRYCD-FLG
                     WRITE SRYCD-REC
                 END-IF
             END-IF
      *
             PERFORM 900-ICD-ALL-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO   MCP-TABLE
           MOVE    "all"               TO   MCP-PATHNAME
      *
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   SRYCD-FILE
           .
       2001-ICD-TO-YAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *    対象診療コードファイル作成
      *****************************************************************
       2002-SRYACT-OUT-SEC             SECTION.
      *
           OPEN    I-O                 SRYCD-FILE
      *
           PERFORM 900-SRYACT-KEY13-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =  ZERO )
      *
               IF  SRY-SRYKBN (1:1)        =  "2"
                   PERFORM VARYING IDX0  FROM  1  BY  1
                           UNTIL ( IDX0  >  5 )
                            OR   ( SRY-SRYCD (IDX0)  =  SPACE )
                       IF  SRY-SRYCD(IDX0)(1:1)  =  "6"
                           MOVE  SRY-SRYCD(IDX0)  TO WRK-SRYCD
                           PERFORM 900-SRYCD-KREAD-SEC
                           IF    FLG-SRYCD   =   1
                                 MOVE  WRK-SRYCD
                                             TO  SRYCD-KEY
                                 MOVE  SPACE TO  SRYCD-FLG
                                 WRITE SRYCD-REC
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
      *
               PERFORM 900-SRYACT-KEY13-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   SRYCD-FILE
           .
       2002-SRYACT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報情報移動処理
      *****************************************************************
       2002-SRYACT-TO-YAKUJYO-SEC       SECTION.
      *
           OPEN    INPUT               SRYCD-FILE
                                       SETUPYJ02-FILE
      *
           PERFORM 900-SRYCD-READ-SEC
      *
           PERFORM UNTIL ( FLG-SRYCD   NOT =   ZERO )
      *
               PERFORM 2004-SYORI-KEIKA-HEN-SEC
      *
               MOVE    SRYCD-KEY       TO   WRK-SRYCD
               PERFORM 900-SETUPYJ02-READ-SEC
               IF  FLG-SETUPYJ02       =    ZERO
                   PERFORM 900-YAKUJYO-KEY-SEL-SEC
                   IF  FLG-YAKUJYO      =    1
                       PERFORM 2003-YAK-TO-YAKUJYO-SEC
                   ELSE
                       PERFORM 2004-YAKUJYO-EXIST-HEN-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-SRYCD-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   SRYCD-FILE
                   SETUPYJ02-FILE
           .
       2002-SRYACT-TO-YAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *    薬情一時ファイルから薬剤情報作成
      *****************************************************************
       2003-YAK-TO-YAKUJYO-SEC          SECTION.
      *
           COMPUTE CNT-SYORI           =   CNT-SYORI  +  1
      *
           INITIALIZE                      YAKUJYO-REC
      *
           MOVE    SPA-HOSPNUM         TO  YAKUJYO-HOSPNUM
           MOVE    SETUPYJ02-SRYCD     TO  YAKUJYO-SRYCD
           MOVE    1                   TO  YAKUJYO-TEKNUM
      *    MOVE    WRK-SRYCD           TO  WRK-TNS-SRYCD
      *    MOVE    SMCNDATE-YMD        TO  WRK-TNS-YMD
      *    PERFORM 900-TENSU-KEY-SEL-SEC
      *    MOVE    TNS-NAME            TO  YAKUJYO-NAME
           MOVE    SETUPYJ02-EFFECT    TO  WRK-ITEMIN
           PERFORM 800-ITEM-LF-EDIT-SEC
           MOVE    WRK-ITEMOT          TO  YAKUJYO-EFFECT
      *
           MOVE    SETUPYJ02-CAUTION   TO  WRK-ITEMIN
           PERFORM 800-ITEM-LF-EDIT-SEC
           MOVE    WRK-ITEMOT          TO  YAKUJYO-CAUTION
      *
           MOVE    SPACE               TO  WRK-FILENAME
           MOVE    1                   TO  WRK-IMAGEKBN
           EVALUATE    TRUE
               WHEN  ( SETUPYJ02-PHOTO1-FILENAME   NOT =   SPACE )
                   MOVE    SETUPYJ02-PHOTO1-FILENAME
                                           TO  WRK-FILENAME
                   MOVE    1               TO  WRK-IMAGEKBN
               WHEN  ( SETUPYJ02-PHOTO2-FILENAME   NOT =   SPACE )
                   MOVE    SETUPYJ02-PHOTO2-FILENAME
                                           TO  WRK-FILENAME
                   MOVE    2               TO  WRK-IMAGEKBN
               WHEN  ( SETUPYJ02-PHOTO3-FILENAME   NOT =   SPACE )
                   MOVE    SETUPYJ02-PHOTO3-FILENAME
                                           TO  WRK-FILENAME
                   MOVE    3               TO  WRK-IMAGEKBN
           END-EVALUATE
      *
           MOVE    WRK-FILENAME        TO  YAKUJYO-PHOTO
           INITIALIZE  ORCSMIAREA
           MOVE    YAKUJYO-SRYCD       TO  ORCSMI-SRYCD
           MOVE    WRK-IMAGEKBN        TO  ORCSMI-IMAGEKBN
           MOVE    WRK-PATH            TO  ORCSMI-TEMPDIR
           MOVE    YAKUJYO-PHOTO       TO  ORCSMI-FILENAME
           CALL    "ORCSMISAVE"    USING   ORCSMIAREA
                                           SPA-AREA
           IF      ORCSMI-RETCD    NOT =   ZERO
               MOVE    SPACE               TO  YAKUJYO-PHOTO
           END-IF
      *
           MOVE    SETUPYJ02-COLOR     TO  WRK-ITEMIN
           PERFORM 800-ITEM-LF-EDIT-SEC
           MOVE    WRK-ITEMOT          TO  YAKUJYO-COLOR
      *
           MOVE    SETUPYJ02-SHAPE     TO  WRK-ITEMIN
           PERFORM 800-ITEM-LF-EDIT-SEC
           MOVE    WRK-ITEMOT          TO  YAKUJYO-SHAPE
      *
           MOVE    SETUPYJ02-MARK      TO  WRK-ITEMIN
           PERFORM 800-ITEM-LF-EDIT-SEC
           MOVE    WRK-ITEMOT          TO  YAKUJYO-MARK
      *
           MOVE    SMCNDATE-YMD        TO  YAKUJYO-CREYMD
                                           YAKUJYO-UPYMD
           MOVE    SMCNDATE-HMS        TO  YAKUJYO-UPHMS
      *
           MOVE    YAKUJYO-REC         TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
      *
       2003-YAK-TO-YAKUJYO-EXT.
           EXIT.
      *****************************************************************
      *    処理経過編集処理
      *****************************************************************
       2004-SYORI-KEIKA-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           DIVIDE  50      INTO        CNT-SRYCD
                           GIVING      WRK-SHO
                           REMAINDER   WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI           NOT =   ZERO )
               GO  TO  2004-SYORI-KEIKA-HEN-EXT
           END-IF
      *
           MOVE    CNT-SRYCD           TO  WRK-STR-LEFT-ZX99
           PERFORM 800-STR-HIDARIZUME-HEN-SEC
           MOVE    WRK-STR-LEFT        TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-KNS-N
           MOVE    SPACE               TO  WRK-MAE-INPUT
           MOVE    SMCNDATE-HMS(1:2)   TO  WRK-MAE-INPUT(1:2)
           MOVE    "."                 TO  WRK-MAE-INPUT(3:1)
           MOVE    SMCNDATE-HMS(3:2)   TO  WRK-MAE-INPUT(4:2)
           MOVE    "."                 TO  WRK-MAE-INPUT(6:1)
           MOVE    SMCNDATE-HMS(5:2)   TO  WRK-MAE-INPUT(7:2)
           MOVE    WRK-MAE-INPUT       TO  WRK-TIME-N
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "処理件数："        DELIMITED BY SIZE
                   WRK-KNS-N           DELIMITED BY SPACE
                   "件　"              DELIMITED BY SIZE
                   WRK-TIME-N          DELIMITED BY SPACE
                   INTO    JOB-YOBI
           END-STRING
           MOVE    ZERO                TO  JOB-UPDCNT
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBCHKYAK1"       TO  JOB-PGID
           MOVE    WRK-TITLE           TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           .
       2004-SYORI-KEIKA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダ情報編集処理
      *****************************************************************
       2004-HEAD-HEN-SEC               SECTION.
      *
           COMPUTE CNT-PAGE            =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE            TO  WRK-ZZZZ9
      *
           INITIALIZE                  HCMSL80
      *
           STRING
               "＊＊　薬剤情報更新確認リスト（医薬品登録済）　＊＊＊"
                                                   DELIMITED   BY  SIZE
               "（作成日："                        DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(1:4)            DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(5:2)            DELIMITED   BY  SIZE
               "."                                 DELIMITED   BY  SIZE
               LNK-PRTKANRI-SKYYMD(7:2)            DELIMITED   BY  SIZE
               "）"                                DELIMITED   BY  SIZE
               "PAGE "                             DELIMITED   BY  SIZE
               WRK-ZZZZ9-G                         DELIMITED   BY  SIZE
               INTO    HCMSL80-MOJI (1)
           END-STRING
      *
           MOVE    2                   TO  CNT-LINE
           .
       2004-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報既登録編集処理
      *****************************************************************
       2004-YAKUJYO-EXIST-HEN-SEC       SECTION.
      *
           MOVE    WRK-SRYCD           TO  WRK-TNS-SRYCD
           MOVE    SMCNDATE-YMD        TO  WRK-TNS-YMD
           PERFORM 2004-SRYNAME-HEN-SEC
           MOVE    WRK-MEMO            TO  HCMSL80-MOJI (CNT-LINE)
           PERFORM 2004-LINE-CNT-SEC
           .
       2004-YAKUJYO-EXIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為名称編集処理
      *****************************************************************
       2004-SRYNAME-HEN-SEC            SECTION.
      *
           MOVE    SPACE
                                       TO  WRK-MESSAGE
      *
           INITIALIZE                  WRK-MEMO
      *
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           IF      FLG-TENSU       NOT =   ZERO
                   MOVE    "00000000"  TO  WRK-TNS-YMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
           END-IF
      *
           STRING  WRK-MESSAGE     DELIMITED   BY  SPACE
                   WRK-TNS-SRYCD   DELIMITED   BY  SIZE
                   " "             DELIMITED   BY  SIZE
                   TNS-NAME        DELIMITED   BY  SPACE
                   " "             DELIMITED   BY  SIZE
                   TNS-YUKOSTYMD   DELIMITED   BY  SIZE
                   "-"             DELIMITED   BY  SIZE
                   TNS-YUKOEDYMD   DELIMITED   BY  SIZE
                   INTO    WRK-MEMO
           END-STRING
           .
       2004-SRYNAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認情報ファイル出力処理
      *****************************************************************
       2004-HCMSL80-OUT-SEC            SECTION.
      *
      *
           IF    ( CNT-LINE            >   2 )
               WRITE   HCMSL80
           END-IF
      *
           CLOSE   HCMSL80-FILE
      *
           OPEN    INPUT               HCMSL80-FILE
      *
           MOVE    ZERO                TO  FLG-HCMSL80
      *
           PERFORM 900-HCMSL80-READ-SEC
      *
           PERFORM UNTIL ( FLG-HCMSL80 NOT =   ZERO )
      *
               PERFORM 2004-PRINT-SEC
      *
               PERFORM 900-HCMSL80-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   HCMSL80-FILE
           .
       2004-HCMSL80-OUT-EXT.
           EXIT.
      *****************************************************************
      *    行カウント処理
      *****************************************************************
       2004-LINE-CNT-SEC                SECTION.
      *
           IF    ( CNT-LINE            >=   CONST-LINE-MAX )
               WRITE   HCMSL80
               PERFORM 2004-HEAD-HEN-SEC
           ELSE
               COMPUTE CNT-LINE        =   CNT-LINE    +   1
           END-IF
           .
       2004-LINE-CNT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2004-PRINT-SEC                  SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCMSL80.red"        TO  SPRT-PRTID
           MOVE    "薬剤情報更新確認リスト"
                                       TO  SPRT-TITLE
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               COMPUTE CNT-OUTPUT      =   CNT-OUTPUT  +   1
           ELSE
               DISPLAY "ORCSPRT ERR!!"
           END-IF
           .
       2004-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    ファイル削除
           PERFORM 800-FILE-DEL-SEC
      *
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-OUTPUT      TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           IF    ( CNT-SYORI           =   ZERO )
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               IF    ( CNT-OUTPUT      >   ZERO )
                   MOVE    "確認リストを確認してください"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF        ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM
                                       TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE  WRK-MAE-INPUT         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-OUT-INPUT
               MOVE    KANACHK-MAX     TO  WRK-OUT-MAX
           END-IF
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    文字列左詰処理
      *****************************************************************
       800-STR-HIDARIZUME-HEN-SEC      SECTION.
      *
           MOVE    WRK-STR-LEFT        TO  IDXE
           PERFORM VARYING IDXE        FROM    1   BY  1
                   UNTIL ( IDXE        >       500 )
      *
               IF    ( WRK-STR-LEFT (IDXE:1)
                                       NOT =   SPACE )
                   MOVE    WRK-STR-LEFT (IDXE:)
                                       TO  WRK-STR-LEFT-TMP
                   MOVE    WRK-STR-LEFT-TMP
                                       TO  WRK-STR-LEFT
                   MOVE    500         TO  IDXE
               END-IF
      *
           END-PERFORM
           .
       800-STR-HIDARIZUME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-ITEM-LF-EDIT-SEC            SECTION.
      *
           INITIALIZE                  WRK-ITEMOT
      *
           MOVE    1               TO  IDX1
                                       IDX2
           PERFORM UNTIL ( IDX1    >   10000 - 3 )
                    OR   ( WRK-ITEMIN (IDX1 :) =   SPACE )
      *
               IF    ( WRK-ITEMIN (IDX1 : 4)    =   "_LF_" )
                   MOVE    X"0A"   TO  WRK-ITEMOT (IDX2 : 1)
                   COMPUTE IDX1    =   IDX1     +   3
                   COMPUTE IDX2    =   IDX2     +   1
               ELSE
                   MOVE    WRK-ITEMIN (IDX1 : 1)
                                   TO  WRK-ITEMOT (IDX2 : 1)
                   COMPUTE IDX2    =   IDX2     +   1
               END-IF
      *
               COMPUTE IDX1    =   IDX1     +   1
      *
           END-PERFORM
      *
           .
       800-ITEM-LF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ファイル削除処理
      *****************************************************************
       800-FILE-DEL-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    SETYJ02         TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    SETYJ03         TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    SETYJ04         TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       800-FILE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードワークファイル読込み処理
      *****************************************************************
       900-SRYCD-READ-SEC              SECTION.
      *
           READ    SRYCD-FILE          NEXT
               AT  END
                   MOVE    1           TO  FLG-SRYCD
               NOT AT  END
                   MOVE    ZERO        TO  FLG-SRYCD
      *            IF      SRYCD-FLG   NOT =   SPACE
      *                    GO          TO  900-SRYCD-READ-SEC
      *            END-IF
                   COMPUTE CNT-SRYCD   =   CNT-SRYCD  +  1
           END-READ
           .
       900-SRYCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードワークファイル読込み処理
      *****************************************************************
       900-SRYCD-KREAD-SEC             SECTION.
      *
           MOVE    WRK-SRYCD           TO  SRYCD-KEY
      *
           READ    SRYCD-FILE
               INVALID
                   MOVE    1           TO  FLG-SRYCD
               NOT INVALID
                   MOVE    ZERO        TO  FLG-SRYCD
           END-READ
           .
       900-SRYCD-KREAD-EXT.
           EXIT.
      *****************************************************************
      *    確認ファイルリスト読込み処理
      *****************************************************************
       900-HCMSL80-READ-SEC            SECTION.
      *
           READ    HCMSL80-FILE
               AT  END
                   MOVE    1           TO  FLG-HCMSL80
           END-READ
           .
       900-HCMSL80-READ-EXT.
           EXIT.
      *****************************************************************
      *    入力コード　抽出
      *****************************************************************
       900-ICD-ALL-SEL-SEC             SECTION.
      *
           INITIALIZE                  ICD-REC
           MOVE    ZERO                TO  FLG-INPUTCD
      *
           MOVE    SPA-HOSPNUM         TO   ICD-HOSPNUM
           MOVE    ICD-REC             TO   MCPDATA-REC
           MOVE    "tbl_inputcd"       TO   MCP-TABLE
           MOVE    "all"               TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ICD-REC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
           .
       900-ICD-ALL-SEL-EXT.
           EXIT.
      *****************************************************************
      *     入力コード　読み込み
      *****************************************************************
       900-ICD-ALL-FET-SEC             SECTION.
      *
           MOVE    "tbl_inputcd"       TO   MCP-TABLE
           MOVE    "all"               TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ICD-REC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
           .
       900-ICD-ALL-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ　抽出
      *****************************************************************
       900-SRYACT-KEY13-SEL-SEC          SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ZERO                TO  FLG-SRYACT
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    "600000000"         TO  SRY-SRYCD (1)
           MOVE    "699999999"         TO  SRY-SRYCD (2)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-KEY13-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタ　読み込み
      *****************************************************************
       900-SRYACT-KEY13-FET-SEC          SECTION.
      *
           MOVE    "tbl_sryact"        TO   MCP-TABLE
           MOVE    "key13"             TO   MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-KEY13-FET-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報検索
      *****************************************************************
       900-YAKUJYO-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKUJYO
      *
           INITIALIZE                      YAKUJYO-REC
           MOVE    SPA-HOSPNUM         TO  YAKUJYO-HOSPNUM
           MOVE    WRK-SRYCD           TO  YAKUJYO-SRYCD
           MOVE    1                   TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC         TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  YAKUJYO-REC
           ELSE
               INITIALIZE                  YAKUJYO-REC
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-YAKUJYO-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    薬情一時ファイル(INDEX)読込処理
      *****************************************************************
       900-SETUPYJ02-READ-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SETUPYJ02
      *
           INITIALIZE                      SETUPYJ02-REC
           MOVE    WRK-SRYCD           TO  SETUPYJ02-SRYCD
           MOVE    ZERO                TO  SETUPYJ02-HOSPNUM
                                           SETUPYJ02-TERMID
                                           SETUPYJ02-YJCD
                                           SETUPYJ02-RENNUM
           READ    SETUPYJ02-FILE
           INVALID 
               MOVE    1               TO  FLG-SETUPYJ02
           END-READ
      *
           .
       900-SETUPYJ02-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-TNS-YMD         TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE              TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO   MCP-TABLE
           MOVE    "key"               TO   MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    薬情一時ファイル(SEQUENTIAL)読込処理
      *****************************************************************
       900-SETUPYJ01-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SETUPYJ01
      *
           READ    SETUPYJ01-FILE
           AT  END
               MOVE    1           TO  FLG-SETUPYJ01
           END-READ
      *
           .
       900-SETUPYJ01-READ-EXT.
           EXIT.
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
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.

