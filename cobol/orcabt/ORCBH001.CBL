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
       PROGRAM-ID.             ORCBH001.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 返戻データ一覧の印刷

      *  管理者            : 
      *  作成日付    作業者        記述
      *  24/04/XX    ORCAMO        新規作成
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
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCMSL55.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HENREI-B        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-OUT             PIC 9(05).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03).
           03  IDY                 PIC 9(03).
           03  IDZ                 PIC 9(03).
      *
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-SYORIYM    PIC 9(06).
           03  WRK-PARA-TEISYUTUSAKI
                                   PIC X(01).
           03  WRK-PARA-TEISYUTUSAKI2
                                   PIC X(01).
           03  WRK-PARA-HOSPCD     PIC X(07).
           03  WRK-PARA-CREYMD     PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-ERRMSG          PIC X(300).
      *
           03  WRK-RECEIPT-NO      PIC X(6).
      *
           03  WRK-ZZZZZ9   PIC ZZZZZ9.
      *
           03  WRK-TEISYUTUSAKI    PIC X(04).
           03  WRK-TEISYUTUSAKI2   PIC X(06).
      *
      *    ＣＳＶ項目数
           03  SPLIT-CNT               PIC 9(03).
           03  SPLIT-CNT1              PIC 9(03).
      *
           03  FLG-DATAEND             PIC 9(01).
           03  FLG-AREA1.
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DELI1           PIC 9(01).
               05  FLG-DELI2           PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
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
               05  WRK-PARA-X          PIC X(01)  OCCURS   3000.
           03  WRK-PARA-REC1           PIC X(3000).
           03  WRK-PARA-REC2           PIC X(3000).
      *
       01  WRK-DATA.
           03  WRK-DATA-OCC            PIC X(140)  OCCURS  8.
      *
       01  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //WRK-HENREI-B-//.
      *
       01  HEAD-01.
           03  FILLER                  PIC X(50)   VALUE
               "　　　　　　　　　　　　　　　　　　　　　　　　　".
           03  H01-SYORIYM             PIC X(18)   VALUE   SPACE.
           03  HO1-TITLE               PIC X(34)   VALUE   SPACE.
           03  FILLER                  PIC X(06)   VALUE
               "　　　".
      **   03  H01-CREYMD              PIC X(22)   VALUE   SPACE.
      **   03  H01-CREYMD-TITLE        PIC X(20)   VALUE   SPACE.
           03  FILLER                  PIC X(20)   VALUE
               "　　　　　　　　　　".
           03  FILLER                  PIC X(08)   VALUE   "作成日：".
           03  H01-SYSYMD              PIC X(09).
           03  FILLER                  PIC X(04)   VALUE   "  P.".
           03  H01-PAGE                PIC ZZ9.
      *
       01  HEAD-02.
           03  FILLER                  PIC X(08)   VALUE
               "　　　　".
           03  FILLER                  PIC X(06)   VALUE
               "入外　".
           03  FILLER                  PIC X(20)   VALUE
               "患者番号　　　　　　".
           03  FILLER                  PIC X(36)   VALUE
               "氏名　　　　　　　　　　　　　　　　".
           03  FILLER                  PIC X(18)   VALUE
               "診療年月　　　　　".     
           03  FILLER                  PIC X(06)   VALUE
               "種別　".
           03  FILLER                  PIC X(12)   VALUE
               "保険者番号　".
           03  FILLER                  PIC X(40)   VALUE
               "記号・番号　　　　　　　　　　　　　　　".
           03  FILLER                  PIC X(06)   VALUE   "施設　".
           03  FILLER                  PIC X(06)   VALUE   "災／資".
      *
       01  BODY-01.
           03  B01-RENNUM              PIC X(06).
           03  B01-FIL1                PIC X(02).
           03  B01-NYUGAIKBN           PIC X(04).
           03  B01-FIL2                PIC X(02).
           03  B01-PTNUM               PIC X(20).
           03  B01-NAME                PIC X(36).
           03  B01-SRYYM               PIC X(16).
           03  B01-FIL3                PIC X(02).
           03  B01-RECESYUBETU         PIC X(04).
           03  B01-FIL4                PIC X(02).
           03  B01-HKNJANUM            PIC X(8).
           03  B01-FIL5                PIC X(04).
           03  B01-KIGO                PIC X(40).
           03  B01-RECEIPT-KBN         PIC X(4).
           03  B01-FIL6                PIC X(02).
           03  B01-DISASTER            PIC X(2).
      *
       01  BODY-02.
           03  B02-FIL1                PIC X(06).
           03  B02-TITLE               PIC X(18).
           03  B02-DATA                PIC X(134).
      *
       01  BODY-03.
           03  B03-FIL1                PIC X(06).
           03  B03-TITLE               PIC X(12).
           03  B03-DATA                PIC X(140).
      *
       01  WRK-CONST-AREA.
           03  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   55.
      *    カンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
       *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
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
      *    主処理
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
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
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
                                       WRK-PARA-SYORIYM
                                       WRK-PARA-TEISYUTUSAKI
                                       WRK-PARA-TEISYUTUSAKI2
                                       WRK-PARA-HOSPCD
                                       WRK-PARA-CREYMD
                                       RECEERR
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-HOSPNUM      =" WRK-PARA-HOSPNUM     
           DISPLAY "WRK-PARA-SYORIYM      =" WRK-PARA-SYORIYM     
           DISPLAY "WRK-PARA-TEISYUTUSAKI =" WRK-PARA-TEISYUTUSAKI
           DISPLAY "WRK-PARA-TEISYUTUSAKI2=" WRK-PARA-TEISYUTUSAKI2  
           DISPLAY "WRK-PARA-HOSPCD       =" WRK-PARA-HOSPCD     
      *****DISPLAY "WRK-PARA-CREYMD       =" WRK-PARA-CREYMD    
      *
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
           MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBH001"      TO  JOB-PGID
           MOVE    "返戻データ一覧作成"
                                   TO  JOB-SHELLMSG
      *
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
           MOVE    WRK-PARA-SYORIYM
                                   TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (1:16)
                                   TO  H01-SYORIYM
      *     
           EVALUATE    WRK-PARA-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "社保"          TO  WRK-TEISYUTUSAKI
               WHEN    "2"
                   MOVE    "国保"          TO  WRK-TEISYUTUSAKI
           END-EVALUATE
           EVALUATE    WRK-PARA-TEISYUTUSAKI2
               WHEN    "1"
                   MOVE    "（保）"        TO  WRK-TEISYUTUSAKI2
           END-EVALUATE
      *
           STRING  WRK-TEISYUTUSAKI            DELIMITED  BY  SPACE
                   WRK-TEISYUTUSAKI2           DELIMITED  BY  SPACE
                   "分　取込済返戻データ一覧"  DELIMITED  BY  SIZE
                                               INTO    HO1-TITLE
           END-STRING  
      **     MOVE    WRK-PARA-CREYMD TO  WRK-SYMD
      **     PERFORM 31012-SEIWA-HEN-SEC
      **     MOVE    LNK-DAY2-EDTYMD3
      **                             TO  H01-CREYMD
      **     MOVE    "取込"          TO  H01-CREYMD-TITLE
      *
      *    作成日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  H01-SYSYMD
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    患者病名の一覧作成処理
           COMPUTE LNK-PRTKANRI-SHORI-RENNUM
                                   =   LNK-PRTKANRI-SHORI-RENNUM
                                   +   1
      *
      *    初期値設定
           MOVE    WRK-CONS-LINE-MAX
                                       TO  CNT-LINE
           MOVE    ZERO                TO  CNT-PAGE
                                           CNT-OUT
      *
           INITIALIZE                      HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    WRK-PARA-SYORIYM    TO  HENREI-B-SYORIYM
           MOVE    WRK-PARA-TEISYUTUSAKI
                                       TO  HENREI-B-TEISYUTUSAKI
           MOVE    WRK-PARA-TEISYUTUSAKI2
                                       TO  HENREI-B-TEISYUTUSAKI2
           MOVE    WRK-PARA-HOSPCD     TO  HENREI-B-HOSPCD
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-HENREI-BODY-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-B
           END-IF
      *
           PERFORM         UNTIL   FLG-HENREI-B    =   1
               IF      CNT-LINE             >=  54
                   PERFORM 2001-HEAD-PRINT-SEC
               END-IF
      *
               MOVE    SPACE               TO  BODY-01
               MOVE    ALL "　"            TO  B01-FIL1
                                               B01-FIL2
                                               B01-FIL3
                                               B01-FIL4
                                               B01-FIL5
                                               B01-FIL6
      *
               ADD     1                   TO  IDZ
               MOVE    IDZ                 TO  WRK-ZZZZZ9
               MOVE    WRK-ZZZZZ9          TO  B01-RENNUM
               EVALUATE    HENREI-B-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"        TO  B01-NYUGAIKBN
                   WHEN    "2"
                       MOVE    "外"        TO  B01-NYUGAIKBN
               END-EVALUATE
               IF   ( HENREI-B-SCREEN-UPD-FLG
                                           =   1 )
               AND  ( HENREI-B-PTID        >   0 )
      *            患者番号設定で設定したとき
                   STRING  "*"             DELIMITED  BY  SIZE
                           HENREI-B-PTNUM  DELIMITED  BY  SIZE
                                           INTO  B01-PTNUM  
                   END-STRING
               ELSE
                   MOVE    HENREI-B-PTNUM  TO  B01-PTNUM
               END-IF    
               MOVE    HENREI-B-NAME   TO  B01-NAME     
               MOVE    HENREI-B-SRYYM  TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (1:16)
                                       TO  B01-SRYYM    
               MOVE    HENREI-B-RECESYUBETU
                                       TO  B01-RECESYUBETU
                                                                   
               IF     HENREI-B-HKNJANUM
                                   NOT =   "00000000"
                   MOVE    HENREI-B-HKNJANUM
                                           TO  B01-HKNJANUM
               END-IF
               STRING  HENREI-B-KIGO   DELIMITED   BY  SPACE
                       "　"            DELIMITED   BY  SIZE
                       HENREI-B-NUM    DELIMITED   BY  SPACE
                                       INTO    B01-KIGO 
               END-STRING
               EVALUATE    HENREI-B-RECEIPT-KBN
                   WHEN    "1"
                       MOVE    "老併"      TO  B01-RECEIPT-KBN
                   WHEN    "2"
                       MOVE    "老健"      TO  B01-RECEIPT-KBN
                   WHEN    "3"
                       MOVE    "医併"      TO  B01-RECEIPT-KBN
                   WHEN    "4"
                       MOVE    "医療"      TO  B01-RECEIPT-KBN
               END-EVALUATE
               EVALUATE    HENREI-B-DISASTER
                   WHEN    "00"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "災"        TO  B01-DISASTER
               END-EVALUATE
               EVALUATE    HENREI-B-SHIKAKU-INFO-FLG
                   WHEN    "0"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "資"         TO  B01-DISASTER
               END-EVALUATE
      *
               ADD     1                   TO  CNT-LINE
               MOVE    BODY-01             TO  HCMSL55-MOJI (CNT-LINE)
      *
      *??         IF      CNT-LINE        >=  WRK-CONS-LINE-MAX
      *??             PERFORM 2001-HEAD-PRINT-SEC
      *??         END-IF
      *
               MOVE    HENREI-B-RECEIPT-NO
                                        TO  WRK-RECEIPT-NO
               PERFORM     UNTIL   FLG-HENREI-B
                                               =   1
                           OR      WRK-RECEIPT-NO
                                           NOT =   HENREI-B-RECEIPT-NO
                   MOVE    HENREI-B-HENREI-INFO2
                                           TO  WRK-PARA-REC
                   INITIALIZE              WRK-HENREI-B-REC    
                   MOVE    7               TO  SPLIT-CNT
                   MOVE    8               TO  SPLIT-CNT1
                   PERFORM 2002-PARA-SPLIT-SEC
      *
                   MOVE    SPACE           TO  BODY-02
                   MOVE    ALL "　"        TO  B02-FIL1
                   MOVE    "【返戻事由コード】"
                                           TO  B02-TITLE
                   MOVE    WRK-HENREI-B-HR-CODE
                                           TO  B02-DATA  
                   IF      CNT-LINE        >=  WRK-CONS-LINE-MAX
                       PERFORM 2001-HEAD-PRINT-SEC
                   END-IF
                   ADD     1               TO  CNT-LINE
                   MOVE    BODY-02         TO  HCMSL55-MOJI (CNT-LINE)
      *             
                   MOVE    SPACE           TO  BODY-03
                   MOVE    ALL "　"        TO  B03-FIL1
                   MOVE    "【返戻事由】"  TO  B03-TITLE
                   MOVE    WRK-HENREI-B-HR-MSG
                                           TO  WRK-DATA
                   PERFORM 2003-HR-DATA-HENSYU-SEC
      *    
                   MOVE    SPACE           TO  BODY-03
                   MOVE    ALL "　"        TO  B03-FIL1
                   MOVE    "【補足事項】"  TO  B03-TITLE
                   MOVE    WRK-HENREI-B-HENREI-INFO2
                                           TO  WRK-DATA
                   PERFORM 2003-HR-DATA-HENSYU-SEC
      *
                   PERFORM 900-HENREI-BODY-READ-SEC
               END-PERFORM 
      *
               ADD     1               TO  CNT-LINE
      *
               ADD     1               TO  CNT-OUT
           END-PERFORM
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key14"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    帳票印刷処理
           IF      CNT-PAGE        >   ZERO
               PERFORM 2004-PRINT-OUT-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       2001-HEAD-PRINT-SEC            SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE            >   ZERO
               PERFORM 2004-PRINT-OUT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  H01-PAGE
      *
           MOVE    SPACE               TO  HCMSL55
           MOVE    HEAD-01             TO  HCMSL55-MOJI (1)
           MOVE    HEAD-02             TO  HCMSL55-MOJI (3)
           MOVE    4                   TO  CNT-LINE
           .
       2001-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    データカンマ分離処理
      *****************************************************************
       2002-PARA-SPLIT-SEC          SECTION.
      *
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   SPLIT-CNT)
                   OR      ( WRK-PO-ED     >=  3000 )
                   OR      ( FLG-END       =   1    )
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
                   PERFORM 20021-CHAR-END-GET-SEC
               END-IF
      *        項目共通処理
               PERFORM 20022-KOUMOKU-KYOTU-SEC
      *        項目別処理
               IF      FLG-DATAEND     =   ZERO
                   PERFORM 20023-RECEIP-KOUMOKU-BETU-SEC
               END-IF
           END-PERFORM
      *
           .
       2002-PARA-SPLIT-EXT.
           EXIT.
      *****************************************************************
      *    対象項目の文字終了位置を取得
      *****************************************************************
       20021-CHAR-END-GET-SEC        SECTION.
      *
           MOVE    WRK-PO-ST           TO  WRK-PO-ED
      *
           PERFORM VARYING   IDYY   FROM    WRK-PO-ST   BY  1
                   UNTIL   ( IDYY           >=  3000 )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF    ( WRK-PARA-X (IDYY)     =   "," )  OR
                     ((IDX             =   SPLIT-CNT  OR  
                                           SPLIT-CNT1) AND
                     ( WRK-PARA-X (IDYY)     =   SPACE))
                   MOVE    IDYY         TO  WRK-PO-ED
                   MOVE    1           TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      WRK-PARA-X (IDYY) NOT =   SPACE
                       MOVE    IDYY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *        デリミタ(")存在するかどうか
               IF      WRK-PARA-X (IDYY)         =   """"
                   IF      FLG-DELI1       =   ZERO
                       MOVE    1       TO  FLG-DELI1
                   ELSE
                       MOVE    1       TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20021-CHAR-END-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目共通処理
      *****************************************************************
       20022-KOUMOKU-KYOTU-SEC       SECTION.
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
                   GO  TO  20022-KOUMOKU-KYOTU-EXT
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF      WRK-PO-ST       =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  20022-KOUMOKU-KYOTU-EXT
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
       20022-KOUMOKU-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻ファイル項目別処理
      *****************************************************************
       20023-RECEIP-KOUMOKU-BETU-SEC       SECTION.
      *
           EVALUATE    WRK-PARA-REC(1:2)
               WHEN    "HR"
                   EVALUATE    WRK-KOUMOKU-NO
      *                返戻事由コード
                       WHEN    5
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   1    )
                           AND   ( KANACHK-MAX     <=  5    )
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                                   WRK-HENREI-B-HR-CODE
      *                     ELSE
      *                         MOVE    "W03"       TO  WRK-ERRCD
      *                         PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF    
      *                返戻理由
                       WHEN    6
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   2    )
                           AND   ( KANACHK-MAX     <=  1000 )
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                                   WRK-HENREI-B-HR-MSG
      *                     ELSE
      *                         MOVE    "W04"       TO  WRK-ERRCD
      *                         PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF    
      *                    補足事項
                       WHEN    7
                           IF      WRK-MO-CNT      >   ZERO
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "1"         TO  KANACHK-SYORI
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF    ( KANACHK-RC      =   ZERO )
                               AND   ( KANACHK-RC2     =   ZERO )
                               AND   ( KANACHK-SYUBETU =   2    )
                               AND   ( KANACHK-MAX     <=  1000 )
                                   MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                               WRK-HENREI-B-HENREI-INFO2
      *                         ELSE
      *                             MOVE    "W04"       TO  WRK-ERRCD
      *                             PERFORM 500-ERRCD-HENSYU-SEC
                               END-IF
                           END-IF    
                  END-EVALUATE
           END-EVALUATE
      *
           .
       20023-RECEIP-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *   返戻取り込みデータ明細詳細情報編集処理
      *****************************************************************
       2003-HR-DATA-HENSYU-SEC         SECTION.
      *
           IF      WRK-DATA        =   SPACE
               IF      CNT-LINE        >=  WRK-CONS-LINE-MAX
                   PERFORM 2001-HEAD-PRINT-SEC
               END-IF
               ADD     1        TO  CNT-LINE
               MOVE    BODY-03  TO  HCMSL55-MOJI (CNT-LINE)
           ELSE                                                
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       8
                       OR      WRK-DATA-OCC (IDX)
                                   =        SPACE             
                   MOVE    WRK-DATA-OCC (IDX)
                                           TO  B03-DATA
                   IF      CNT-LINE        >=  WRK-CONS-LINE-MAX
                        PERFORM 2001-HEAD-PRINT-SEC
                   END-IF
                   ADD     1               TO  CNT-LINE
                   MOVE    BODY-03         TO  HCMSL55-MOJI (CNT-LINE)
      *
                   MOVE    SPACE           TO  BODY-03
                   MOVE    ALL "　"        TO  B03-FIL1
               END-PERFORM
           END-IF    
      *
           .
       2003-HR-DATA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2004-PRINT-OUT-SEC            SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
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
           MOVE   "HCMSL55.red"        TO  SPRT-PRTID
           MOVE    HCMSL55             TO  SPRT-PRTDATA
           MOVE    SPACE               TO  SPRT-TITLE
           STRING  "返戻データ一覧　"  DELIMITED  BY  SIZE
                   WRK-TEISYUTUSAKI    DELIMITED  BY  SPACE
                   WRK-TEISYUTUSAKI2   DELIMITED  BY  SPACE
                                       INTO    SPRT-TITLE
           END-STRING
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       2004-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF    ( STS-RECEERR     =   ZERO )
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
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           MOVE    SPACE           TO  WRK-ERRMSG
           STRING  "ORCBH001 "     DELIMITED  BY  SIZE
                   WRK-RECEERR     DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"      USING   WRK-ERRMSG
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** ORCBH001 IN  "   CNT-OUT
           DISPLAY "*** ORCBH001 PAGE "  CNT-PAGE
           DISPLAY "*** ORCBH001 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-SEC   SECTION.
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key14"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
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
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
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
      *
