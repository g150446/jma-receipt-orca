      ********************************************************************
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
       PROGRAM-ID.             ORCGH03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 返戻データ取込一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/09/14    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.02    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *  04.08.03    NACL-藤原    18/03/28  平成３０年４月改正対応
      *
      *  05.01.01    NNED-藤原    20/10/28  アフターケアレセ電作成対応
      *  05.00.01    ORCAMO       21/09/17  社保の保険者からの返戻分対応
      *  05.01.02    ORCAMO       21/12/01  番号の表示を追加
      *  05.01.03    ORCAMO       22/08/01  返戻事由確認対応
      *  05.02.01    ORCAMO       23/05/26  改ページ対応
      *  05.02.02    ORCAMO       23/07/11  返戻データを取り込み後に患者
      *                                     番号の再発行をした場合の対応
      *  05.02.03    ORCAMO       23/07/25  オン資を行うことができない
      *                                     場合のレセプト対応
      *  05.02.02    ORCAMO       23/08/26  返戻データ患者番号置換処理対応
      *  05.02.03    ORCAMO       23/08/26  取込み済返戻データ一覧の印刷対応
      *  05.02.04    ORCAMO       24/11/18  クライアント印刷対応
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
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
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING   //TEMPFLPARA//
                                   BY          //RECEERR//.
      *
           COPY    "CPERRFL.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求管理スパ領域
           COPY    "H01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "H01SCR-SPA".
      *
      *    設定済返戻データ
           02  SPA-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //SPA-HENREI-B-//.
      *
      *    対象一覧用返戻データ
           02  TBL-HENREI-B-REC    OCCURS  30.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //TBL-HENREI-B-//.
           02  TBL-HENREI-B-ERR    PIC 9(01)   OCCURS  30.
           02  TBL-HENREI-B-MAX    PIC 9(02).
      *
      *    更新対象返戻データ
           02  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //WRK-HENREI-B-//.
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HENREI-K        PIC 9(01).
           03  FLG-HENREI-B        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-HENREI-B-CHK    PIC 9(01).
      *
           03  FLG-KANRI-ROW       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
           03  IDWW                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNO           PIC 9(05).
      *
           03 WRK-ZZ9              PIC ZZ9.
           03  WRK-STR             PIC 9(05).
           03  WRK-BANGO           PIC 9(05).
      *
           03  WRK-H03-BODY-ROW    PIC S9(09).
      *
           03  WRK-PRTKANRI-RENNUM PIC 9(04).
           03  WRK-PRT-PAGE        PIC 9(10).
            03  WRK-PAGE           PIC ZZZZZZZZZ9.
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-H03-KANRI-ROW   PIC S9(09).
      *
           03  WRK-RECEIPT-NO      PIC X(06).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
      *
       01  CHK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //CHK-HENREI-B-//.
      *
           COPY    "CPSK1001.INC"  REPLACING   //SYS-1001-//
                                   BY          //CHK-1001-//.
      *
       01  WRK-HEN-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG.
               05  WRK-HENYM       PIC X(06).
               05  WRK-HENYM-FIL   PIC X(03).
           03  WRK-HENYMDG3        PIC X(22).
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
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *     
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-KANRI-LINE-MAX
                                   PIC 9(04)   VALUE   100.
           03  WRK-CONS-BODY-LINE-MAX
                                   PIC 9(04)   VALUE   100.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "043".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "HENREI".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "HENREI".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  HENREI-K-REC.
           COPY    "CPHENREI-KANRI.INC".
      *
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK3003.INC".
           COPY    "CPSK1031.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *****COPY    "CPORCSPTNUM.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    プリンタ名取得サブ
           COPY  "CPORCSPRTNMGET.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA43SCRAREA.INC".
      *
       PROCEDURE                       DIVISION    USING
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
                                       STS-AREA
                                       IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-H01FREE
           MOVE    SPA-WORK        TO  SPA-H01KYOUTU
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
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-H01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-H01FREE     TO  SPA-FREE
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
           INITIALIZE                  IDX-AREA
      *
           IF      SPA-MOTOPG          =   "HERR"
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
           MOVE    "H03"               TO  MCP-WINDOW
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
           MOVE    1               TO  SPA-GMN-H03-CUR
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XA01"  OR  "XB01"  OR
                                           "XC01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    SPA-WORK        TO  SPA-H01KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-H01FREE
                   INITIALIZE                  SPA-H01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
      *
               INITIALIZE              SPA-H01KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *        返戻一覧
               WHEN    "H04"
                   MOVE    1               TO  SPA-GMN-H03-CUR
                   GO  TO  300-SCREEN-EXT
               WHEN    "HID1"
                   PERFORM 330-HID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "HID2"
                   PERFORM 330-HID2-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "H98"
                   PERFORM 340-H98-SET-SEC
                   GO  TO  300-SCREEN-EXT
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
           MOVE    SPACE           TO  SPA-H03-AREA
           INITIALIZE                  SPA-H03-AREA
      *
           INITIALIZE                  HENREI-K-REC
           MOVE   SPA-HOSPNUM      TO  HENREI-K-HOSPNUM
           MOVE    HENREI-K-REC    TO  MCPDATA-REC
           MOVE    "tbl_henrei_kanri"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_henrei_kanri"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-HENREI-KANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-K
               INITIALIZE                  HENREI-K-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM         UNTIL   FLG-HENREI-K
                                           =   1
                           OR      IDX     >=  WRK-CONS-KANRI-LINE-MAX
               ADD     1               TO  IDX
               MOVE    IDX             TO  SPA-GMN-H03-K-TSEQNUM   (IDX)
               MOVE    HENREI-K-SYORIYM
                                       TO  SPA-NAI-H03-K-TSYORIYM  (IDX)
                                           WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG (1:6)
                                       TO  SPA-GMN-H03-K-TSYORIYM  (IDX)
               MOVE    HENREI-K-TEISYUTUSAKI
                                       TO  SPA-NAI-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
               MOVE    "　"            TO  SPA-GMN-H03-K-TTEISYUTUSAKI2
                                                                   (IDX)
               EVALUATE    HENREI-K-TEISYUTUSAKI
                   WHEN    "1"
                       MOVE    "社保"  TO  SPA-GMN-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
                       EVALUATE    HENREI-K-TEISYUTUSAKI2
                           WHEN    "1"
                               MOVE    "保"
                                       TO  SPA-GMN-H03-K-TTEISYUTUSAKI2
                                                                   (IDX)
                       END-EVALUATE
                   WHEN    "2"
                       MOVE    "国保"  TO  SPA-GMN-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
                       EVALUATE    HENREI-K-TEISYUTUSAKI2
                           WHEN    "1"
                               MOVE    "保"
                                       TO  SPA-GMN-H03-K-TTEISYUTUSAKI2
                                                                   (IDX)
                       END-EVALUATE
                   WHEN    "3"
                       EVALUATE    HENREI-K-TEISYUTUSAKI2
                           WHEN    "0"
                               MOVE    "労災"
                                       TO  SPA-GMN-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
                           WHEN    "1"
                              MOVE    "アフターケア"
                                       TO  SPA-GMN-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
                       END-EVALUATE
               END-EVALUATE
               MOVE    HENREI-K-TEISYUTUSAKI2
                                       TO  SPA-NAI-H03-K-TTEISYUTUSAKI2
                                                                   (IDX)
               MOVE    HENREI-K-RECV-PATIENT-CNT
                                       TO  SPA-GMN-H03-K-TCNT      (IDX)
               MOVE    HENREI-K-CREYMD TO  SPA-NAI-H03-K-TCREYMD   (IDX)
                                           WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-H03-K-TCREYMD   (IDX)
               MOVE    HENREI-K-HOSPCD TO  SPA-NAI-H03-K-THOSPCD   (IDX)
               INITIALIZE                  SYS-1001-REC
               MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
               MOVE    "1001"          TO  SYS-1001-KANRICD
               MOVE    "*"             TO  SYS-1001-KBNCD
               MOVE    HENREI-K-SYORIYM
                                       TO  SYS-1001-STYUKYMD (1:6)
               MOVE    "01"            TO  SYS-1001-STYUKYMD (7:2)
               MOVE    SYS-1001-STYUKYMD
                                       TO  SYS-1001-EDYUKYMD
               MOVE    SYS-1001-REC    TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
                   IF      HENREI-K-HOSPCD NOT =   SPACE
                       IF      SYS-1001-HOSPCD
                                           NOT =   HENREI-K-HOSPCD
                           MOVE    "旧"            TO
                                             SPA-GMN-H03-K-THOSPCD (IDX)
                       END-IF
                   ELSE
                       MOVE    SYS-1001-REC    TO  CHK-1001-REC
      *
                       INITIALIZE                  CHK-HENREI-B-REC
                       MOVE    SPA-HOSPNUM     TO  CHK-HENREI-B-HOSPNUM
                       MOVE    HENREI-K-SYORIYM
                                               TO  CHK-HENREI-B-SYORIYM
                       MOVE    HENREI-K-TEISYUTUSAKI
                                               TO
                                           CHK-HENREI-B-TEISYUTUSAKI
                       MOVE    HENREI-K-TEISYUTUSAKI2
                                               TO
                                           CHK-HENREI-B-TEISYUTUSAKI2
                       MOVE    HENREI-K-HOSPCD TO CHK-HENREI-B-HOSPCD
                       MOVE    CHK-HENREI-B-REC
                                               TO  MCPDATA-REC
                       MOVE    "key4"          TO  WRK-MCP-PATHNAME
                       PERFORM 900-HENREI-BODY-READ-SEC
                       IF      FLG-HENREI-B-CHK    =   ZERO
                           INITIALIZE                  SYS-1001-REC
                           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
                           MOVE    "1001"          TO  SYS-1001-KANRICD
                           MOVE    "*"             TO  SYS-1001-KBNCD
                           MOVE    CHK-HENREI-B-SRYYM  TO
                                               SYS-1001-STYUKYMD (1:6)
                           MOVE    "01"            TO
                                               SYS-1001-STYUKYMD (7:2)
                           MOVE    SYS-1001-STYUKYMD
                                                   TO  SYS-1001-EDYUKYMD
                           MOVE    SYS-1001-REC    TO  MCPDATA-REC
                           PERFORM 900-SYSKANRI-READ-SEC
                           IF      FLG-SYSKANRI    =   ZERO
                               MOVE    MCPDATA-REC     TO  SYS-1001-REC
                               IF      SYS-1001-HOSPCD
                                               NOT =   CHK-1001-HOSPCD
                                   MOVE    "旧"            TO
                                           SPA-GMN-H03-K-THOSPCD (IDX)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               INITIALIZE                      CHK-HENREI-B-REC
               MOVE    SPA-HOSPNUM         TO  CHK-HENREI-B-HOSPNUM
               MOVE    HENREI-K-SYORIYM    TO  CHK-HENREI-B-SYORIYM
               MOVE    HENREI-K-TEISYUTUSAKI
                                           TO  CHK-HENREI-B-TEISYUTUSAKI
               MOVE    HENREI-K-TEISYUTUSAKI2
                                           TO CHK-HENREI-B-TEISYUTUSAKI2
               MOVE    HENREI-K-HOSPCD     TO CHK-HENREI-B-HOSPCD
               MOVE    CHK-HENREI-B-REC    TO  MCPDATA-REC
               MOVE    "key6"              TO  WRK-MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
               IF      FLG-HENREI-B-CHK    =   ZERO
                   MOVE  CHK-HENREI-B-ERRCD    TO
                                       SPA-GMN-H03-K-TERRCD    (IDX)
               END-IF
      *
               MOVE    "tbl_henrei_kanri"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-HENREI-KANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_henrei_kanri"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-H03-KANRI-MAX
      *
           MOVE    "　記号・番号　"
                                   TO  SPA-GMN-H03-B-HYODAI
      *
           MOVE   1                TO  FLG-KANRI-ROW
      *
           MOVE    1               TO  SPA-GMN-H03-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＨＩＤ１）ＯＫ処理
      *****************************************************************
       330-HID1-SET-SEC            SECTION.
      *
           IF      SPA-HID1-FLG    =   "OK"
               EVALUATE    SPA-HIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"      TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
               END-EVALUATE
           ELSE        
               MOVE    SPACE           TO  SPA-HID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-HIDCD
           .
       330-HID1-SET-EXT.
           EXIT.
      *     
      *
      *****************************************************************
      *    確認画面（ＨＩＤ２）ＯＫ処理
      *****************************************************************
       330-HID2-SET-SEC            SECTION.
      *
           IF    ( SPA-HID1-FLG    NOT =   SPACE )
               EVALUATE    SPA-HIDCD
                   WHEN    "1001"
                       PERFORM 490-PRINT-SEC
               END-EVALUATE
           ELSE        
               MOVE    SPACE           TO  SPA-HID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-H98-FLG
           MOVE    SPACE           TO  SPA-HIDCD
           .
       330-HID2-SET-EXT.
           EXIT.
      *     
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-H98-SET-SEC             SECTION.
      *
           IF    ( SPA-H98-FLG     =   "OK" )
               PERFORM 490-NEXT-GMN-SEC
               GO  TO  340-H98-SET-EXT
           END-IF
      *
           MOVE    1               TO  SPA-GMN-H03-CUR
      *
           INITIALIZE                  SPA-HIDCD
                                       SPA-HID1-FLG
                                       SPA-H98-FLG
      *
           .
       340-H98-SET-EXT.
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
                   MOVE    "CHANGE"        TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        再印刷
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 490-REPRINT-SEC
      *        返戻一覧
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-HENREI-LIST-SEC
      *        前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *        次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *        情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *        印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-PRINT-GID-SEC
      *        処理確認
               WHEN    "CLICKED"       ALSO    "B24"
                   PERFORM 490-KAKUNIN-SEC
            END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
                WHEN   "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 410-SELNUM-CHK-SEC
                WHEN   ANY             ALSO    "KANRI_LIST"
                   PERFORM 450-KANRILST-SELECT-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       410-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-GMN-H03-CUR
      *
           INITIALIZE                      SPA-GMN-H03-BODY-TBL
                                           SPA-NAI-H03-BODY-TBL
                                           SPA-GMN-H03-KOUMOKU
                                           SPA-NAI-H03-KOUMOKU
                                           SPA-GMN-H03-NEXT
      *
           MOVE    H03-SELNUM          TO  SPA-GMN-H03-SELNUM
      *
           IF      SPA-GMN-H03-SELNUM  =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-H03-SELNUM  >   SPA-GMN-H03-KANRI-MAX
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-H03-SELNUM  TO  SPA-NAI-H03-SELNUM
                                               H03-KANRI-LIST-ROW
               MOVE    1                   TO  SPA-GMN-H03-PAGE
      *        返戻取り込みデータ明細情報編集処理
               PERFORM 4101-HENREI-BODY-HENSYU-SEC
           END-IF
      *
           .
       410-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   返戻取り込みデータ明細情報編集処理
      *****************************************************************
       4101-HENREI-BODY-HENSYU-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-H03-BODY-TBL
                                           SPA-NAI-H03-BODY-TBL
      *
           MOVE    "　記号・番号　"    TO  SPA-GMN-H03-B-HYODAI
           MOVE    "種別"              TO  SPA-GMN-H03-B-HYODAI2
           IF      SPA-NAI-H03-K-TTEISYUTUSAKI (SPA-NAI-H03-SELNUM)
                                       =   "3"
               IF      SPA-NAI-H03-K-TTEISYUTUSAKI2 (SPA-NAI-H03-SELNUM)
                                       =   "0"
                   MOVE    "年金証書番号等"    TO  SPA-GMN-H03-B-HYODAI
               ELSE    
                   MOVE    "健康管理手帳番号"  TO  SPA-GMN-H03-B-HYODAI
                   MOVE    "診察日"            TO  SPA-GMN-H03-B-HYODAI2
               END-IF
           END-IF
      *
           INITIALIZE                      HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    SPA-NAI-H03-K-TSYORIYM       (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-SYORIYM
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI  (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI2 (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI2
           MOVE    SPA-NAI-H03-K-THOSPCD        (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-HOSPCD
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-HENREI-BODY-READ-KEY4-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-B
           END-IF
      *
           COMPUTE WRK-STR         = ( SPA-GMN-H03-PAGE  -  1 )
                                   *   WRK-CONS-BODY-LINE-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-H03-MAX
           PERFORM         UNTIL   FLG-HENREI-B    =   1
                           OR      IDX             =
                                               WRK-CONS-BODY-LINE-MAX
      *
            ADD     1               TO  IDX2
            IF      IDX2            >=  WRK-STR
             ADD     1               TO  IDX
      *
             IF      IDX             <=  WRK-CONS-BODY-LINE-MAX
               MOVE    IDX             TO  SPA-GMN-H03-BODY-MAX
               MOVE    IDX2            TO  SPA-GMN-H03-B-TSEQNUM   (IDX)
      *
               MOVE    HENREI-B-NYUGAIKBN
                                       TO  SPA-NAI-H03-B-TNYUGAIKBN(IDX)
               MOVE    HENREI-B-PTID   TO  SPA-NAI-H03-B-TPTID     (IDX)
               MOVE    HENREI-B-PTNUM  TO  SPA-GMN-H03-B-TPTNUM    (IDX)
               MOVE    HENREI-B-NAME   TO  SPA-GMN-H03-B-TNAME     (IDX)
               MOVE    HENREI-B-SRYYM  TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG  (1:6)
                                       TO  SPA-GMN-H03-B-TSRYYM    (IDX)
               MOVE    HENREI-B-RECESYUBETU
                                       TO  SPA-NAI-H03-B-TRECESYUBETU
                                                                   (IDX)
               MOVE    HENREI-B-HKNJANUM
                                       TO  SPA-GMN-H03-B-THKNJANUM (IDX)
               STRING  HENREI-B-KIGO   DELIMITED   BY  SPACE
                       "　"            DELIMITED   BY  SIZE
                       HENREI-B-NUM    DELIMITED   BY  SPACE
                                       INTO    SPA-GMN-H03-B-TKIGO (IDX)
               END-STRING
               MOVE    HENREI-B-RECEIPT-KBN
                                       TO  SPA-NAI-H03-B-TRECEIPT-KBN
                                                                   (IDX)
               MOVE    HENREI-B-DISASTER
                                       TO  SPA-NAI-H03-B-TDISASTER (IDX)
               MOVE    HENREI-B-SHIKAKU-INFO-FLG
                                       TO
                                   SPA-NAI-H03-B-TSHIKAKU-INFO-FLG (IDX)
               MOVE    HENREI-B-HR-MSG TO  SPA-GMN-H03-B-TERR      (IDX)
               IF    ( SPA-NAI-H03-K-TTEISYUTUSAKI (SPA-NAI-H03-SELNUM)
                                       =   "3" )
               AND   ( SPA-NAI-H03-K-TTEISYUTUSAKI2 (SPA-NAI-H03-SELNUM)
                                       =   "1" )
                   MOVE    HENREI-B-NUM
                                       TO  SPA-NAI-H03-B-TSRYDD    (IDX)
               END-IF
               MOVE    HENREI-B-RECEIPT-NO
                                       TO  SPA-NAI-H03-B-TRECEIPT-NO
                                                                   (IDX)
               MOVE     HENREI-B-SCREEN-UPD-FLG
                                       TO  SPA-NAI-H03-B-SCREEN-UPD-FLG
                                                                   (IDX)
      *
               MOVE    HENREI-B-REC        TO  MCPDATA-REC
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
               IF      FLG-HENREI-B-CHK    =   ZERO
                   MOVE  CHK-HENREI-B-ERRCD    TO
                                           SPA-GMN-H03-B-TERRCD    (IDX)
               END-IF
      *
      *        患者ＩＤチェック
               IF      HENREI-B-PTID   >   ZERO
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    HENREI-B-PTNUM  TO  SPTID-PTNUM
                   CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                   SPA-AREA
                   IF      SPTID-RC        NOT =   ZERO
                       INITIALIZE                      ORCSPTIDAREA
                       MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                       MOVE    HENREI-B-PTID       TO  SPTID-PTID
                       CALL    "ORCSPTID"           USING   ORCSPTIDAREA
                                                           SPA-AREA
                       IF      SPTID-RC            =   ZERO
                           CONTINUE
                       ELSE
                           MOVE    1               TO
                                                   SPA-NAI-H03-B-TERRFLG
                                                                   (IDX)
                       END-IF
                   ELSE
                       IF      SPTID-PTID      NOT =   HENREI-B-PTID
                           MOVE    2           TO  SPA-NAI-H03-B-TERRFLG
                                                                   (IDX)
                       END-IF
                   END-IF
               END-IF
               MOVE    IDX             TO  SPA-GMN-H03-MAX
             END-IF
            END-IF
      *
            PERFORM 900-HENREI-BODY-READ-KEY4-SEC
           END-PERFORM
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-CONS-BODY-LINE-MAX )  AND
                 ( FLG-HENREI-B          =   ZERO                )
               MOVE    1                   TO  SPA-GMN-H03-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-H03-NEXT
           END-IF
      *
           .
       4101-HENREI-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理情報一覧選択処理
      *****************************************************************
       450-KANRILST-SELECT-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-H03-KOUMOKU
                                           SPA-NAI-H03-KOUMOKU
      *
           INITIALIZE                      SPA-GMN-H03-BODY-TBL
                                           SPA-NAI-H03-BODY-TBL
                                           SPA-GMN-H03-NEXT
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H03-KANRI-MAX
               IF      H03-KANRI-LIST-SELECT (IDX)
                                       =   "T"
                   MOVE    IDX             TO  WRK-SELNO
                   MOVE    SPA-GMN-H03-KANRI-MAX
                                           TO  IDX
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-GMN-H03-SELNUM
                                               SPA-NAI-H03-SELNUM
                                               H03-KANRI-LIST-ROW
               MOVE    1                   TO  SPA-GMN-H03-PAGE
      *        明細情報編集処理
               PERFORM 4101-HENREI-BODY-HENSYU-SEC
           END-IF
           .
       450-KANRILST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻一覧処理
      *****************************************************************
       450-HENREI-LIST-SEC             SECTION.
      *
      *    対象の取込みデータが選択されてないとき
      *    選択したデータが労災のときは表示しない
           IF      SPA-NAI-H03-SELNUM  =   ZERO
               GO  TO  450-HENREI-LIST-EXT
           END-IF
           IF      SPA-NAI-H03-K-TTEISYUTUSAKI (SPA-NAI-H03-SELNUM)
                                       =   "3" 
               GO  TO  450-HENREI-LIST-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-H01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGH04"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-H01FREE
           MOVE    SPA-WORK            TO  SPA-H01KYOUTU
      *
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
      *
           MOVE    "H03"                TO  SPA-MOTOPG
           MOVE    "H04"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "H04"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
          .
       450-HENREI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF    ( SPA-GMN-H03-PAGE  =   1    )
           OR    ( SPA-GMN-H03-MAX  =   ZERO )
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-H03-CUR
               GO      TO      460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-H03-PAGE    =   SPA-GMN-H03-PAGE    -   1
      *
           INITIALIZE                      SPA-GMN-H03-BODY-TBL
                                           SPA-NAI-H03-BODY-TBL
           MOVE    ZERO                TO  SPA-GMN-H03-MAX
      ***                                     SPA-NAI-H03-SELNUM
      ***                                     SPA-GMN-H03-SELNUM
      *    検索処理
           PERFORM 4101-HENREI-BODY-HENSYU-SEC
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF    ( SPA-GMN-H03-NEXT =   ZERO )
           OR    ( SPA-GMN-H03-MAX  =   ZERO )
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-H03-CUR
               GO      TO      470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-H03-PAGE
      *
           INITIALIZE                      SPA-GMN-H03-BODY-TBL
                                           SPA-NAI-H03-BODY-TBL
           MOVE    ZERO                TO  SPA-GMN-H03-MAX
      ***                                     SPA-NAI-H03-SELNUM
      ***                                     SPA-GMN-H03-SELNUM
      *    検索処理
           PERFORM 4101-HENREI-BODY-HENSYU-SEC
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力前処理
      *****************************************************************
       490-PRINT-GID-SEC                SECTION.
      *
      *    入力チェック
      **     MOVE    1               TO  FLG-TOUROKU
      **     PERFORM 410-INPUT-CHK-SEC
      **     IF      SPA-ERRCD   NOT =   SPACE
      **         GO  TO  490-PRINT-GID-EXT
      **     END-IF
      *
      *    ジョブ管理チェック
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN
                                           =   ZERO )
                       IF    ( JOB-ENDYMD  =   SPACE )
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-RECEERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-RECEERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
      ***                  GO  TO  490-PRINT-GID-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    ガイドメッセージセット
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1001"              TO  SPA-HIDCD
           END-IF
      *
           .
       490-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       490-PRINT-SEC                   SECTION.
      *
           INITIALIZE                      SPA-H01-TBL-GROUP
                                           SPA-H01-JOB-JOBID
      *
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "1005"              TO  SPA-ERRCD
                   GO  TO  490-PRINT-EXT
               END-IF
           END-IF
       *
      *    ジョブ管理削除処理
           MOVE    "ALL"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  490-PRINT-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           MOVE    SMCNDATE-HMS        TO  SYS-TIME 
      *
      *    出力先プリンタ名割り当て情報取得
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
      *
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "OR043ERR"          TO  ERRFLPARA-FILE-ID
           MOVE    1                   TO  ERRFLPARA-SYOKBN1
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
      *
           MOVE    WRK-SYSYMD          TO
                                       WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                       WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
      *    返戻一覧印刷処理
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-NAI-H03-K-TSYORIYM (SPA-NAI-H03-SELNUM)
                                   TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "henrei_data_print.sh"
                                   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "henrei_data_print.sh"
                                   TO  SHELLTBL-NAME
      *
           MOVE     SPA-HOSPNUM    TO   SHELLTBL-HOSPNUM
           MOVE     WRK-CONS-JOB-JOBID
                                   TO   SHELLTBL-JOBID
           MOVE     WRK-CONS-JOB-SHELLID
                                   TO   SHELLTBL-SHELLID
      *
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11
      *
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG14
           MOVE    SPA-NAI-H03-K-TSYORIYM       (SPA-NAI-H03-SELNUM)
                                   TO  SHELLTBL-ARG15
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI  (SPA-NAI-H03-SELNUM)
                                   TO  SHELLTBL-ARG16
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI2 (SPA-NAI-H03-SELNUM)
                                   TO  SHELLTBL-ARG17
           MOVE    SPA-NAI-H03-K-THOSPCD        (SPA-NAI-H03-SELNUM)
                                   TO  SHELLTBL-ARG18
           MOVE    SPA-NAI-H03-K-TCREYMD        (SPA-NAI-H03-SELNUM)
                                   TO  SHELLTBL-ARG19
            MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG20
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
      *    印刷処理
           IF     ( SPA-HID1-FLG        =   "OK"  OR "PV")
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SPA-H01-TBL-GROUP
               MOVE    WRK-CONS-JOB-JOBID
                                       TO  SPA-H01-JOB-JOBID
      *
               MOVE    0               TO  WRK-STPAGE
               MOVE    99999           TO  WRK-EDPAGE
      *
               MOVE    SPACE           TO  SHELLTBL
               MOVE    "print_prv.sh"  TO  SHELLTBL-NAME
      *
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-HOSPNUM
               MOVE    WRK-CONS-JOB-JOBID
                                       TO  SHELLTBL-JOBID
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-SHELLID
      *
               MOVE    SPACE           TO  LNK-PRTKANRI-AREA
               INITIALIZE                  LNK-PRTKANRI-AREA
               MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  LNK-PRTKANRI-TBL-KEY
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO            TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM
               MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"  TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
               MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM
                                       TO  SHELLTBL-ARG1
               MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
               MOVE    LNK-PRTKANRI-SRYYM
                                       TO  SHELLTBL-ARG5
               MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SHELLTBL-ARG6
               MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
               MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
               MOVE    LNK-PRTKANRI-TERMID
                                       TO  SHELLTBL-ARG9
               MOVE    LNK-PRTKANRI-OPID
                                       TO  SHELLTBL-ARG10
               MOVE    LNK-PRTKANRI-PRTNM
                                       TO  SHELLTBL-ARG11
      *
               MOVE    ERRFLPARA-BASENAME
                                       TO  SHELLTBL-ARG12
               MOVE    WRK-CONS-JOB-JOBID
                                       TO  SHELLTBL-ARG13
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE      TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE      TO  SHELLTBL-ARG16
               EVALUATE    SPA-HID1-FLG
               WHEN    "PV"
                    MOVE    "3"            TO  SHELLTBL-ARG17
               WHEN    "OK"
                   IF      SPA-CLIENT-PRT-FLG  =  2
                       MOVE    "5"         TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    "2"         TO  SHELLTBL-ARG17
                   END-IF
               END-EVALUATE
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG18
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "allways"       TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 45012-JOBID-UPD-SEC
      *
           PERFORM 490-KAKUNIN-SEC
           .
       490-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           PERFORM 800-ORCSJOB-SEC
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
     *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SYS-3003-JOBID
      *
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
               MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      H98
           MOVE    ZERO                TO  H98-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  H98-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           MOVE    5                   TO  H98-DURATION
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       MOVE    JOB-JOBID       TO  H98-RENNUM   (IDX)
                       MOVE    JOB-SHELLMSG    TO  H98-SHORINM  (IDX)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  H98-STARTTIME(IDX)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  H98-ENDTIME  (IDX)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  H98-PAGE     (IDX)
                       MOVE    JOB-YOBI        TO  H98-ERRMSG   (IDX)
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "処理中です"    TO  H98-MSG
                       ELSE
                           IF    ( JOB-ERRCD   =   SPACE )
                               CONTINUE
                           ELSE
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE
                                                   INTO  H98-MSG
                               END-STRING
                           END-IF
                       END-IF
                       MOVE    IDX             TO  H98-COUNT
      *
                       IF    ( JOB-JOBID   =   2 )
                           MOVE    JOB-UPDCNT  TO  WRK-PRT-PAGE
                       END-IF
      *
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( H98-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( H98-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                       TO  H98-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                       TO  H98-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-HID1-FLG    =   "PV" )
               MOVE    "F12:プレビュー"    TO  H98-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                   MOVE    WIDGET-NORMAL   TO  H98-B12-STATE
                   MOVE    "B12"           TO  MCP-WIDGET
               END-IF
           END-IF
      *
           MOVE    "H03"                TO  SPA-MOTOPG
           MOVE    "H98"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "H98"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       490-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-H01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "H03"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-H01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "H03"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       490-NEXT-GMN-SEC                SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-H01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "H03"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    0                   TO  WRK-STPAGE
           MOVE    0                   TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      **                                   LNK-XC01-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-H01-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE          TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE          TO  LNK-XC01-ED-PAGE
           MOVE    "H03"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       490-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CH2"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN    =   ZERO )
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "6666"          TO  SPA-HIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           INITIALIZE                  SPA-HIDCD
                                       SPA-HID1-FLG
                                       SPA-H98-FLG
      *
           MOVE    "H01"           TO  SPA-SAKIPG
           MOVE    "H03"           TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO   WRK-HENYMDG
           ELSE
               IF      WRK-SYMD            =   ALL "9"
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
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               END-IF
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
           IF      SPA-HIDCD       NOT =   SPACE
               EVALUATE    SPA-HIDCD
                   WHEN    "1001"
                       PERFORM 530-HID2SET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    OTHER
                       PERFORM 520-HIDSET-SEC
                       GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "H03"               TO  MCP-WINDOW
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
           IF      FLG-KANRI-ROW       =   1
               MOVE    ZERO                TO  WRK-H03-KANRI-ROW
           ELSE
               MOVE    H03-KANRI-LIST-ROW  TO  WRK-H03-KANRI-ROW
           END-IF
      *
           INITIALIZE                  H03
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H03-KANRI-MAX
               MOVE    SPA-GMN-H03-K-TSEQNUM     (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9            TO  H03-KANRI-TSEQNUM    (IDX)
               MOVE    SPA-GMN-H03-K-TSYORIYM    (IDX)
                                           TO  H03-KANRI-TSYORIYM  (IDX)
               MOVE    SPA-GMN-H03-K-TTEISYUTUSAKI    (IDX)
                                           TO  H03-KANRI-TTEISYUTUSAKI
                                                                   (IDX)
               MOVE    SPA-GMN-H03-K-TTEISYUTUSAKI2   (IDX)
                                           TO  H03-KANRI-TTEISYUTUSAKI2
                                                                   (IDX)
               MOVE    SPA-GMN-H03-K-TCNT      (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  H03-KANRI-TCNT      (IDX)
               MOVE    SPA-GMN-H03-K-TERRCD    (IDX)
                                           TO  H03-KANRI-TERRCD    (IDX)
               MOVE    SPA-GMN-H03-K-TCREYMD (IDX)
                                           TO  H03-KANRI-TCREYMD   (IDX)
               MOVE    SPA-GMN-H03-K-THOSPCD   (IDX)
                                           TO  H03-KANRI-THOSPCD   (IDX)
      *         
               IF      IDX                 =   SPA-GMN-H03-SELNUM
                   MOVE    "T"                 TO  H03-KANRI-LIST-SELECT
                                                                   (IDX)
               ELSE
                   MOVE    "F"                 TO  H03-KANRI-LIST-SELECT
                                                                   (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-H03-KANRI-MAX
                                       TO  H03-KANRI-LIST-COUNT
           MOVE    ZERO                TO  H03-KANRI-LIST-ROW
           IF      WRK-H03-KANRI-ROW   >   ZERO
               MOVE    WRK-H03-KANRI-ROW   TO  H03-KANRI-LIST-ROW
           END-IF
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H03-BODY-MAX
               MOVE    SPA-GMN-H03-B-TSEQNUM    (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  H03-BODY-TSEQNUM    (IDX)
               EVALUATE    SPA-NAI-H03-B-TNYUGAIKBN (IDX)
                   WHEN    "1"
                       MOVE    "入"        TO  H03-BODY-TNYUGAIKBN (IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  H03-BODY-TNYUGAIKBN (IDX)
               END-EVALUATE
               IF   ( SPA-NAI-H03-B-SCREEN-UPD-FLG (IDX)
                                           =   1 )
               AND  ( SPA-NAI-H03-B-TPTID (IDX)
                                           >   0 )
      *            患者番号設定で設定したとき
                   STRING  "*"             DELIMITED  BY  SIZE
                           SPA-GMN-H03-B-TPTNUM     (IDX)
                                           DELIMITED  BY  SIZE
                                           INTO  H03-BODY-TPTNUM   (IDX)
                   END-STRING
               ELSE
                   MOVE    SPA-GMN-H03-B-TPTNUM     (IDX)
                                           TO  H03-BODY-TPTNUM     (IDX)
               END-IF
               MOVE    SPA-GMN-H03-B-TNAME      (IDX)
                                           TO  H03-BODY-TNAME      (IDX)
               MOVE    SPA-GMN-H03-B-TSRYYM     (IDX)
                                           TO  H03-BODY-TSRYYM     (IDX)
               EVALUATE    SPA-NAI-H03-B-TRECESYUBETU (IDX)
                   WHEN    "7101"
                   WHEN    "7091"
                       MOVE    "傷病・業務"    TO
                                           H03-BODY-TRECESYUBETU (IDX)
                   WHEN    "7102"
                   WHEN    "7092"
                       MOVE    "傷病・通勤"    TO
                                           H03-BODY-TRECESYUBETU (IDX)
                   WHEN    "7071"
                   WHEN    "7081"
                       MOVE    "短期・業務"    TO
                                           H03-BODY-TRECESYUBETU (IDX)
                   WHEN    "7072"
                   WHEN    "7082"
                       MOVE    "短期・通勤"    TO
                                           H03-BODY-TRECESYUBETU (IDX)
                   WHEN    "7021"
                       MOVE    SPA-NAI-H03-B-TSRYDD (IDX)
                                               TO
                                           H03-BODY-TRECESYUBETU (IDX)
                      CONTINUE
                   WHEN    OTHER
                       MOVE    SPA-NAI-H03-B-TRECESYUBETU (IDX)
                                                       TO
                                           H03-BODY-TRECESYUBETU (IDX)
               END-EVALUATE
               IF     SPA-GMN-H03-B-THKNJANUM  (IDX)
                                   NOT =   "00000000"
                   MOVE    SPA-GMN-H03-B-THKNJANUM  (IDX)
                                           TO  H03-BODY-THKNJANUM (IDX)
               END-IF
               MOVE    SPA-GMN-H03-B-TKIGO      (IDX)
                                           TO  H03-BODY-TKIGO     (IDX)
               EVALUATE    SPA-NAI-H03-B-TRECEIPT-KBN (IDX)(1:1)
                   WHEN    "1"
                       MOVE    "老併"          TO  H03-BODY-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "2"
                       MOVE    "老健"          TO  H03-BODY-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "3"
                       MOVE    "医併"          TO  H03-BODY-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "4"
                       MOVE    "医療"          TO  H03-BODY-TRECEIPT-KBN
                                                                  (IDX)
               END-EVALUATE
               EVALUATE    SPA-NAI-H03-B-TDISASTER (IDX)
                   WHEN    "00"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "災"            TO  H03-BODY-TDISASTER
                                                                  (IDX)
               END-EVALUATE
               EVALUATE    SPA-NAI-H03-B-TSHIKAKU-INFO-FLG (IDX)
                   WHEN    "0"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "資"            TO  H03-BODY-TDISASTER
                                                                  (IDX)
                END-EVALUATE
               MOVE    SPA-GMN-H03-B-TERR      (IDX)
                                           TO  H03-BODY-TERR      (IDX)
               MOVE    SPA-GMN-H03-B-TERRCD    (IDX)
                                           TO  H03-BODY-TERRCD    (IDX)
      *
               IF      SPA-H01NAI-PTID     >   ZERO
                   IF      SPA-H01NAI-PTID     =
                                               SPA-NAI-H03-B-TPTID (IDX)
                       MOVE    "#FFCC99"       TO
                                           H03-BODY-LIST-BGCOLOR (IDX)
                   END-IF
               END-IF
      *
               IF      SPA-GMN-H03-B-TERRCD (IDX)
                                        NOT =   SPACE
      *            エラーがあったとき（水色）
                   MOVE    "#4169E1"       TO
                                           H03-BODY-LIST-FGCOLOR (IDX)
               ELSE
                   IF      SPA-NAI-H03-B-TERRFLG (IDX)
                                               >   0
      *                患者番号変換でエラーになったとき（赤）
                       MOVE    "#FF0000"       TO
                                           H03-BODY-LIST-FGCOLOR (IDX)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-H03-BODY-MAX
                                           TO  H03-BODY-LIST-COUNT
      *
           MOVE    SPA-GMN-H03-B-HYODAI    TO  H03-BODY-LIST-HYODAI
           MOVE    SPA-GMN-H03-B-HYODAI2   TO  H03-BODY-LIST-HYODAI2
      *
           MOVE    SPA-GMN-H03-SELNUM      TO  H03-SELNUM
      *
           MOVE    WIDGET-NORMAL           TO  H03-B05-STATE
                                               H03-B12-STATE
           MOVE    "返戻詳細"              TO  H03-B05-LABEL
           IF      SPA-NAI-H03-SELNUM  =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  H03-B05-STATE
                                               H03-B12-STATE
           ELSE
               IF      SPA-NAI-H03-K-TTEISYUTUSAKI (SPA-NAI-H03-SELNUM)
                                           =   "3" 
                  MOVE    WIDGET-INSENSITIVE  TO  H03-B05-STATE
                                                  H03-B12-STATE
               END-IF
           END-IF
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
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-GMN-H03-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-H03-CUR
               WHEN    1
                   MOVE  "SELNUM"          TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"             TO  MCP-WIDGET   
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    1               TO  SPA-GMN-H03-CUR
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
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           MOVE    SPACE                TO  HERR
           MOVE    SPA-ERRCD            TO  HERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                        TO  HERR-ERRMSG
               WHEN    "0005"
                   MOVE    "前ページはありません"
                                        TO  HERR-ERRMSG
               WHEN    "0006"
                   MOVE    "次ページはありません"
                                        TO  HERR-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                        TO  HERR-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                        TO  HERR-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                        TO  HERR-ERRMSG
               WHEN    "1010"
                   MOVE
                   "システム管理ＤＢ（３００３）が更新できませんでした"
                                        TO  HERR-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR  TO  HERR-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR  TO  HERR-ERRMSG
               WHEN    "4001"
                   MOVE    "ＤＢの更新に失敗しました"
                                        TO  HERR-ERRMSG
      *
           END-EVALUATE
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "H03"                TO  SPA-MOTOPG
           MOVE    "HERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HERR"               TO  MCP-WINDOW
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
       520-HIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  HID1
           INITIALIZE                       HID1
           MOVE    SPA-HIDCD            TO  HID1-ID1CODE
           EVALUATE    SPA-HIDCD
               WHEN    "6666"
                   MOVE    "処理中です"     TO  HID1-ID1MSG
           END-EVALUATE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "H03"                TO  SPA-MOTOPG
           MOVE    "HID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-HIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       530-HID2SET-SEC              SECTION.
      *
           MOVE    SPACE                TO  HID2
           INITIALIZE                       HID2
           MOVE    SPA-HIDCD            TO  HID2-ID1CODE
           EVALUATE    SPA-HIDCD
               WHEN    "1001"
                   MOVE    "帳票の印刷を開始します"
                                            TO  HID2-ID1MSG
           END-EVALUATE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "H03"                TO  SPA-MOTOPG
           MOVE    "HID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-HIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻管理読み込み処理
      *****************************************************************
       900-HENREI-KANRI-READ-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE   ZERO                 TO  FLG-HENREI-K
               MOVE   MCPDATA-REC          TO  HENREI-K-REC
           ELSE
               INITIALIZE                      HENREI-K-REC
               MOVE   1                    TO  FLG-HENREI-K
           END-IF
           .
       900-HENREI-KANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-SEC        SECTION.
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE  MCPDATA-REC           TO  CHK-HENREI-B-REC
                   MOVE  ZERO                  TO  FLG-HENREI-B-CHK
               ELSE
                   INITIALIZE                      CHK-HENREI-B-REC
                   MOVE  1                     TO  FLG-HENREI-B-CHK
               END-IF
           ELSE
               INITIALIZE                      CHK-HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B-CHK
           END-IF
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HENREI-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-KEY4-SEC   SECTION.
      *
           MOVE    "tbl_henrei_body"       TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-KEY4-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
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
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
