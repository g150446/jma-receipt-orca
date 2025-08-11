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
       PROGRAM-ID.             ORCGH05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 返戻データ患者番号置換処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  23/08/06    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.02.01    ORCAMO       24/05/27  複数の保険情報と一致するとき
      *                                     の患者情報の表示を修正
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
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
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HENREI-K        PIC 9(01).
           03  FLG-HENREI-B        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-MONTHLYNUM      PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-LABOR-SIO       PIC 9(01).
      *
           03  FLG-HENREI-B-CHK    PIC 9(01).
           03  FLG-HKN-CHK         PIC 9(01).
      *
           03  FLG-BODY-ROW        PIC 9(01).
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
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-H05-KANRI-ROW   PIC S9(09).
           03  WRK-H05-BODY-ROW    PIC S9(09).
      *
           03  WRK-RECEIPT-NO      PIC X(06).
      *
           03  WRK-CHK-DATA        PIC X(40).
      *
           03  WRK-DATA-R.
               05  WRK-DATA        PIC X(02)   OCCURS  9.       
      *
       01  CHK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //CHK-HENREI-B-//.
      * 
      *    退避患者保険情報
       01  WRK-PTHKNINF-AREA.
           02  WRK-PTHKNINF-MAX    PIC 9(02).
           02  WRK-PTHKNINF-REC    OCCURS  10.  
           COPY    "CPPTHKNINF.INC"
                                   REPLACING   //PTHKN//
                                   BY          //WRK-PTHKN//.
           02  WRK-PTKOHINF-REC    OCCURS  10.  
           COPY    "CPPTKOHINF.INC"
                                   REPLACING   //PTKOH//
                                   BY          //WRK-PTKOH//.
           02  WRK-PTRSIINF-REC    OCCURS  10.  
           COPY    "CPPTRSIINF.INC"
                                   REPLACING   //PTRSI//
                                   BY          //WRK-PTRSI//.
      *
      *    スパ領域（ワーク）
           COPY    "COMMON-SPA"    REPLACING   //SPA-//
                                   BY          //WRK-SPA-//.
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
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-KANRI-LINE-MAX
                                   PIC 9(04)   VALUE   100.
           03  WRK-CONS-BODY-LINE-MAX
                                   PIC 9(04)   VALUE   100.
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
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
           COPY    "CPORCSPTNUM.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           END-EVALUATE.
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
           INITIALIZE                  WRK-AREA
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
           MOVE    "H05"               TO  MCP-WINDOW
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-H01FREE
                   MOVE    SPA-WORK        TO  SPA-H01KYOUTU
      *
                   PERFORM 3101-U01SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *        戻る
               WHEN    "HID1"
                   IF      SPA-HID1-FLG    =   "OK"
                       EVALUATE    SPA-HIDCD
                           WHEN    "0101"
                               MOVE    SPACE           TO  SPA-HID1-FLG
                               MOVE    SPACE           TO  SPA-HIDCD
                               MOVE    "JOIN"      TO  MCP-PUTTYPE
                               PERFORM 210-BACK-SYORI-SEC
                               GO  TO  300-SCREEN-EXT
                        END-EVALUATE
                   ELSE
                       MOVE    2               TO  SPA-GMN-H05-CUR
                   END-IF
      *
                   MOVE    SPACE           TO  SPA-HID1-FLG
                   MOVE    SPACE           TO  SPA-HIDCD      
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
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    3                   TO  SPA-GMN-H05-CUR
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-GMN-H05-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
                   INITIALIZE                      SPA-GMN-H05-KOMOKU1
                                                   SPA-NAI-H05-KOMOKU1
      *
                   INITIALIZE                      ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                   MOVE    WRK-SPA-PTNUM       TO  SPTID-PTNUM
                   CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                       SPA-AREA
                   IF      SPTID-RC        NOT =   ZERO
                       MOVE    "0004"              TO  SPA-ERRCD
                   ELSE
      *                患者情報取得
                       MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                       MOVE    SPTID-PTID          TO  PTINF-PTID
                       PERFORM 900-PTINF-READ-SEC
                       IF      FLG-PTINF       NOT =   ZERO
                           MOVE    "0004"              TO  SPA-ERRCD
                       ELSE
      *                    患者情報
                           IF      PTINF-TSTPTNUMKBN   =   "1"
                               STRING  "*"         DELIMITED  BY  SIZE
                                       SPTID-PTNUM DELIMITED  BY  SPACE
                                               INTO    SPA-GMN-H05-PTNUM
                               END-STRING
                           ELSE                           
                               MOVE   PTINF-PTID   TO  SPA-NAI-H05-PTID
                               MOVE   SPTID-PTNUM  TO  SPA-GMN-H05-PTNUM
                           END-IF                            
                           MOVE   PTINF-KANANAME   TO
                                                   SPA-GMN-H05-KANANAME
                           MOVE   PTINF-NAME   TO  SPA-GMN-H05-NAME
                           MOVE   PTINF-SEX        TO  SPA-NAI-H05-SEX
                           EVALUATE    PTINF-SEX
                               WHEN    "1"
                                   MOVE    "男"    TO  SPA-GMN-H05-SEX
                               WHEN    "2"
                                   MOVE    "女"    TO  SPA-GMN-H05-SEX
                           END-EVALUATE
                           MOVE   PTINF-BIRTHDAY   TO
                                                   SPA-NAI-H05-BIRTHDAY
                                                   WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    WRK-HENYMDG     TO
                                                   SPA-GMN-H05-BIRTHDAY
      *
                           IF      PTINF-TSTPTNUMKBN
                                               NOT =   "1"
                               MOVE    99          TO  SPA-GMN-H05-CUR
                           END-IF    
                       END-IF    
                   END-IF     
               END-IF    
           END-IF
           
           .
       3101-U01SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-H05-AREA
           INITIALIZE                  SPA-H05-AREA
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
      *      紐付けできなかったレセプトが存在するデータだけ表示
             INITIALIZE                      CHK-HENREI-B-REC
             MOVE    SPA-HOSPNUM         TO  CHK-HENREI-B-HOSPNUM
             MOVE    HENREI-K-SYORIYM    TO  CHK-HENREI-B-SYORIYM
             MOVE    HENREI-K-TEISYUTUSAKI
                                         TO  CHK-HENREI-B-TEISYUTUSAKI
             MOVE    HENREI-K-TEISYUTUSAKI2
                                         TO  CHK-HENREI-B-TEISYUTUSAKI2
             MOVE    HENREI-K-HOSPCD     TO  CHK-HENREI-B-HOSPCD
             MOVE    1                   TO  CHK-HENREI-B-SELLIMIT
             MOVE    ZERO                TO  CHK-HENREI-B-SELOFFSET
             MOVE    CHK-HENREI-B-REC    TO  MCPDATA-REC
             MOVE    "key12"             TO  WRK-MCP-PATHNAME
             PERFORM 900-HENREI-BODY-READ-SEC
             IF      FLG-HENREI-B-CHK    =   ZERO
               ADD     1               TO  IDX
               MOVE    IDX             TO  SPA-GMN-H05-K-TSEQNUM   (IDX)
               MOVE    HENREI-K-SYORIYM
                                       TO  SPA-NAI-H05-K-TSYORIYM  (IDX)
                                           WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG (1:6)
                                       TO  SPA-GMN-H05-K-TSYORIYM  (IDX)
               MOVE    HENREI-K-TEISYUTUSAKI
                                       TO  SPA-NAI-H05-K-TTEISYUTUSAKI
                                                                   (IDX)
               MOVE    "　"            TO  SPA-GMN-H05-K-TTEISYUTUSAKI2
                                                                   (IDX)
               EVALUATE    HENREI-K-TEISYUTUSAKI
                   WHEN    "1"
                       MOVE    "社保"  TO  SPA-GMN-H05-K-TTEISYUTUSAKI
                                                                   (IDX)
                       EVALUATE    HENREI-K-TEISYUTUSAKI2
                           WHEN    "1"
                               MOVE    "保"
                                       TO  SPA-GMN-H05-K-TTEISYUTUSAKI2
                                                                   (IDX)
                       END-EVALUATE
                   WHEN    "2"
                       MOVE    "国保"  TO  SPA-GMN-H05-K-TTEISYUTUSAKI
                                                                   (IDX)
                       EVALUATE    HENREI-K-TEISYUTUSAKI2
                           WHEN    "1"
                               MOVE    "保"
                                       TO  SPA-GMN-H05-K-TTEISYUTUSAKI2
                                                                   (IDX)
                       END-EVALUATE
                   WHEN    "3"
                       EVALUATE    HENREI-K-TEISYUTUSAKI2
                           WHEN    "0"
                               MOVE    "労災"
                                       TO  SPA-GMN-H05-K-TTEISYUTUSAKI
                                                                   (IDX)
                           WHEN    "1"
                              MOVE    "アフターケア"
                                       TO  SPA-GMN-H05-K-TTEISYUTUSAKI
                                                                   (IDX)
                       END-EVALUATE
               END-EVALUATE
               MOVE    HENREI-K-TEISYUTUSAKI2
                                       TO  SPA-NAI-H05-K-TTEISYUTUSAKI2
                                                                   (IDX)
               MOVE    HENREI-K-RECV-PATIENT-CNT
                                       TO  SPA-GMN-H05-K-TCNT      (IDX)
               MOVE    HENREI-K-CREYMD TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-H05-K-TCREYMD   (IDX)
               MOVE    HENREI-K-HOSPCD TO  SPA-NAI-H05-K-THOSPCD   (IDX)
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
                                             SPA-GMN-H05-K-THOSPCD (IDX)
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
                                           SPA-GMN-H05-K-THOSPCD (IDX)
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
                                       SPA-GMN-H05-K-TERRCD    (IDX)
               END-IF
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
           MOVE    IDX             TO  SPA-GMN-H05-KANRI-MAX
      *
           MOVE    "　記号・番号　"
                                   TO  SPA-GMN-H05-B-HYODAI
      *
           MOVE    1               TO  FLG-KANRI-ROW
                                       FLG-BODY-ROW
      *
           MOVE    1               TO  SPA-GMN-H05-CUR
      *
            .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        解除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 410-KAIJYO-SEC
      *    前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *    次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *        氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-KENSAKU-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-UPDATE-SEC
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
                WHEN   "ACTIVATE"      ALSO    "B_SELNUM"
                   PERFORM 420-B-SELNUM-CHK-SEC
                WHEN   ANY             ALSO    "KANRI_LIST"
                   PERFORM 450-KANRILST-SELECT-SEC
               WHEN   ANY             ALSO    "BODY_LIST"
                   PERFORM 450-BODYLST-SELECT-SEC
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
           IF      SPA-NAI-H05-B-SELNUM    >   0
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      410-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-H05-CUR
      *
           INITIALIZE                      SPA-GMN-H05-BODY-TBL
                                           SPA-NAI-H05-BODY-TBL
                                           SPA-GMN-H05-KOMOKU
                                           SPA-NAI-H05-KOMOKU
                                           SPA-GMN-H05-NEXT
      *
           MOVE    H05-SELNUM          TO  SPA-GMN-H05-SELNUM
      *
           IF      SPA-GMN-H05-SELNUM  =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-H05-SELNUM  >   SPA-GMN-H05-KANRI-MAX
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-H05-SELNUM  TO  SPA-NAI-H05-SELNUM
                                               H05-KANRI-LIST-ROW
               MOVE    ZERO                TO  FLG-KANRI-ROW
               MOVE    1                   TO  FLG-BODY-ROW
               MOVE    1                   TO  SPA-GMN-H05-PAGE
      *        返戻取り込みデータ明細情報編集処理
               PERFORM 4101-HENREI-KANRI-HENSYU-SEC
           ELSE    
               INITIALIZE                      SPA-GMN-H05-SELNUM
                                               SPA-NAI-H05-SELNUM
           END-IF
      *
           .
       410-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   返戻取り込みデータ管理報編集処理
      *****************************************************************
       4101-HENREI-KANRI-HENSYU-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-H05-KOMOKU1
                                           SPA-NAI-H05-KOMOKU1
                                           SPA-GMN-H05-B-SELNUM
                                           SPA-NAI-H05-B-SELNUM
           INITIALIZE                      SPA-GMN-H05-BODY-TBL
                                           SPA-NAI-H05-BODY-TBL
      *
           MOVE    "　記号・番号　"    TO  SPA-GMN-H05-B-HYODAI
           MOVE    "種別"              TO  SPA-GMN-H05-B-HYODAI2
           IF      SPA-NAI-H05-K-TTEISYUTUSAKI (SPA-NAI-H05-SELNUM)
                                       =   "3"
               IF      SPA-NAI-H05-K-TTEISYUTUSAKI2 (SPA-NAI-H05-SELNUM)
                                       =   "0"
                   MOVE    "年金証書番号等"    TO  SPA-GMN-H05-B-HYODAI
               ELSE    
                   MOVE    "健康管理手帳番号"  TO  SPA-GMN-H05-B-HYODAI
                   MOVE    "診察日"            TO  SPA-GMN-H05-B-HYODAI2
               END-IF
           END-IF
      *
           INITIALIZE                      HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    SPA-NAI-H05-K-TSYORIYM       (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-SYORIYM
           MOVE    SPA-NAI-H05-K-TTEISYUTUSAKI  (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI
           MOVE    SPA-NAI-H05-K-TTEISYUTUSAKI2 (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI2
           MOVE    SPA-NAI-H05-K-THOSPCD        (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-HOSPCD
           COMPUTE HENREI-B-SELLIMIT
                       =   WRK-CONS-BODY-LINE-MAX
                                       +   1
           COMPUTE HENREI-B-SELOFFSET
                       = ( SPA-GMN-H05-PAGE    -   1 )
                                       *   WRK-CONS-BODY-LINE-MAX
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    "key12"             TO  MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-B
           END-IF
      *
      *
           MOVE    ZERO                TO  IDX
                                           SPA-GMN-H03-BODY-MAX
           PERFORM         UNTIL   FLG-HENREI-B    =   1
                           OR      IDX             =
                                               WRK-CONS-BODY-LINE-MAX
                           OR      SPA-ERRCD   NOT =   SPACE
               ADD     1               TO  IDX
               COMPUTE SPA-GMN-H05-B-TSEQNUM   (IDX)
                       = ( SPA-GMN-H05-PAGE    -   1 )
                                       *   WRK-CONS-BODY-LINE-MAX
                                       +   IDX
      *
               EVALUATE    HENREI-B-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "入"    TO  SPA-GMN-H05-B-TNYUGAIKBN(IDX)
                   WHEN    "2"
                       MOVE    "外"    TO  SPA-GMN-H05-B-TNYUGAIKBN(IDX)
               END-EVALUATE
               MOVE    HENREI-B-PTID   TO  SPA-NAI-H05-B-TPTID     (IDX)
               MOVE    HENREI-B-PTNUM  TO  SPA-GMN-H05-B-TPTNUM    (IDX)
               MOVE    HENREI-B-NAME   TO  SPA-GMN-H05-B-TNAME     (IDX)
               MOVE    HENREI-B-BIRTHDAY
                                       TO  SPA-NAI-H05-B-TBIRTHDAY (IDX)
                                           WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-H05-B-TBIRTHDAY (IDX)
                                           
               EVALUATE    HENREI-B-SEX
                   WHEN    "1"
                       MOVE    "男"    TO  SPA-GMN-H05-B-TSEX      (IDX)
                   WHEN    "2"
                       MOVE    "女"    TO  SPA-GMN-H05-B-TSEX      (IDX)
               END-EVALUATE
               MOVE    HENREI-B-SRYYM  TO  WRK-SYMD (1:6)
                                           SPA-NAI-H05-B-TSRYYM    (IDX)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG  (1:6)
                                       TO  SPA-GMN-H05-B-TSRYYM    (IDX)
               MOVE    HENREI-B-RECESYUBETU
                                       TO  SPA-NAI-H05-B-TRECESYUBETU
                                                                   (IDX) 
               EVALUATE    SPA-NAI-H05-B-TRECESYUBETU (IDX)
                   WHEN    "7101"
                   WHEN    "7091"
                       MOVE    "傷病・業務"    TO
                                       SPA-GMN-H05-B-TRECESYUBETU (IDX)
                   WHEN    "7102"
                   WHEN    "7092"
                       MOVE    "傷病・通勤"    TO
                                       SPA-GMN-H05-B-TRECESYUBETU (IDX)
                   WHEN    "7071"
                   WHEN    "7081"
                       MOVE    "短期・業務"    TO
                                       SPA-GMN-H05-B-TRECESYUBETU (IDX)
                   WHEN    "7072"
                   WHEN    "7082"
                       MOVE    "短期・通勤"    TO
                                       SPA-GMN-H05-B-TRECESYUBETU (IDX)
                   WHEN    "7021"
                       MOVE    SPA-NAI-H05-B-TSRYDD (IDX)
                                               TO
                                       SPA-GMN-H05-B-TRECESYUBETU (IDX)
                      CONTINUE
                   WHEN    OTHER
                       MOVE    HENREI-B-RECESYUBETU
                                               TO
                                       SPA-GMN-H05-B-TRECESYUBETU (IDX)
               END-EVALUATE
               IF      SPA-NAI-H05-K-TTEISYUTUSAKI (SPA-NAI-H05-SELNUM)
                                           =   "3"
                   CONTINUE
               ELSE    
                   EVALUATE    SPA-NAI-H05-B-TRECESYUBETU (IDX)(2:1)
                       WHEN    "2"
      *                    全国公費又は地方公費を設定
                           PERFORM VARYING IDY     FROM    1   BY  1
                                   UNTIL   IDY     >       4
                                   OR    ( HENREI-B-FTNNUM (IDY)
                                                   =   SPACE 
                                               AND
                                           HENREI-B-JKYNUM  (IDY)
                                                   =   SPACE )
                               IF    ( HENREI-B-FTNNUM (IDY) (1:2)
                                              =   "28"      )
                               AND   ( HENREI-B-JKYNUM (IDY)
                                              =   "9999996" )
                                   MOVE    "1"         TO
                                               SPA-NAI-H05-B-TPCR (IDX)
                               ELSE
      *                            負担者番号、受給者番号は先頭の空白を除く
                                   MOVE    HENREI-B-FTNNUM (IDY)
                                                       TO  WRK-CHK-DATA
                                   PERFORM VARYING IDW FROM    1   BY  1
                                           UNTIL   IDW >       8
                                       IF      WRK-CHK-DATA (IDW:1)
                                                       NOT =   SPACE
                                           MOVE    WRK-CHK-DATA (IDW:)
                                                               TO
                                               SPA-GMN-H05-B-TKIGO (IDX)
                                           MOVE    8           TO  IDW
                                       END-IF
                                   END-PERFORM
                                   MOVE    HENREI-B-JKYNUM (IDY) 
                                                       TO  WRK-CHK-DATA
                                   PERFORM VARYING IDW FROM    1   BY  1
                                           UNTIL   IDW >       7
                                       IF      WRK-CHK-DATA (IDW:1)
                                                       NOT =   SPACE
                                           MOVE    WRK-CHK-DATA (IDW:)
                                                               TO
                                               SPA-GMN-H05-B-TNUM  (IDX)
                                           MOVE    7           TO  IDW
                                       END-IF
                                   END-PERFORM
                                   MOVE    4               TO  IDY
                               END-IF
                           END-PERFORM
                       WHEN    OTHER    
                           MOVE    HENREI-B-HKNJANUM
                                       TO  SPA-GMN-H05-B-THKNJANUM (IDX)
      *                    記号番号は全角変換を行い先頭の空白を除く
                           IF      HENREI-B-KIGO   NOT =   SPACE
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "2"         TO  KANACHK-SYORI
                               MOVE    HENREI-B-KIGO
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF      KANACHK-RC      =   ZERO
                                   MOVE
                                       KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                                        TO  WRK-CHK-DATA
                                   PERFORM VARYING IDW FROM    1   BY  2
                                           UNTIL   IDW >       29
                                       IF      WRK-CHK-DATA (IDW:2)
                                                    NOT =   "　"
                                           MOVE    WRK-CHK-DATA (IDW:)
                                                               TO
                                               SPA-GMN-H05-B-TKIGO (IDX)
                                           MOVE    29          TO  IDW
                                       END-IF
                                   END-PERFORM
                               END-IF
                           END-IF
                           IF      HENREI-B-NUM    NOT =   SPACE
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "2"         TO  KANACHK-SYORI
                               MOVE    HENREI-B-NUM
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               IF      KANACHK-RC      =   ZERO
                                   MOVE
                                       KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                        TO  WRK-CHK-DATA
                                   PERFORM VARYING IDW FROM    1   BY  2
                                           UNTIL   IDW >       29
                                       IF      WRK-CHK-DATA (IDW:2)
                                                        NOT =   "　"
                                           MOVE    WRK-CHK-DATA (IDW:)
                                                               TO
                                               SPA-GMN-H05-B-TNUM  (IDX)
                                           MOVE    29          TO  IDW
                                       END-IF
                                   END-PERFORM
                               END-IF
                           END-IF
                   END-EVALUATE
               END-IF
               EVALUATE    HENREI-B-RECEIPT-KBN (1:1)
                   WHEN    "1"
                       MOVE    "老併"  TO  SPA-GMN-H05-B-TRECEIPT-KBN
                                                                   (IDX)
                   WHEN    "2"
                       MOVE    "老健"  TO  SPA-GMN-H05-B-TRECEIPT-KBN
                                                                   (IDX)
                   WHEN    "3"
                       MOVE    "医併"  TO  SPA-GMN-H05-B-TRECEIPT-KBN
                                                                   (IDX)
                   WHEN    "4"
                       MOVE    "医療"  TO  SPA-GMN-H05-B-TRECEIPT-KBN
                                                                   (IDX)
               END-EVALUATE
               EVALUATE    HENREI-B-DISASTER
                   WHEN    "00"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "災"    TO  SPA-GMN-H05-B-TDISASTER (IDX)
               END-EVALUATE
               EVALUATE    HENREI-B-SHIKAKU-INFO-FLG
                   WHEN    "0"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "資"    TO  SPA-GMN-H05-B-TDISASTER (IDX)
               END-EVALUATE
               IF    ( SPA-NAI-H05-K-TTEISYUTUSAKI (SPA-NAI-H05-SELNUM)
                                       =   "3" )
               AND   ( SPA-NAI-H05-K-TTEISYUTUSAKI2 (SPA-NAI-H05-SELNUM)
                                       =   "1" )
                   MOVE    HENREI-B-NUM
                                       TO  SPA-NAI-H05-B-TSRYDD    (IDX)
               END-IF
               MOVE    HENREI-B-RECEIPT-NO
                                       TO  SPA-NAI-H05-B-TRECEIPT-NO
                                                                   (IDX)
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       10
                   IF      HENREI-B-S-TEISYUTUSAKI (IDZZ)
                                   NOT =   SPACE
                       MOVE    "1"         TO  SPA-NAI-H05-B-TSEIKYU-FLG
                                                                   (IDX)
                       MOVE    10          TO  IDZZ
                   END-IF
               END-PERFORM
      *        枝番取得処理
      *        （医保で公費単独、後期高齢者以外のとき）
               IF    ( SPA-NAI-H05-K-TTEISYUTUSAKI (SPA-NAI-H05-SELNUM)
                                       =   "3"         )
               OR    ( HENREI-B-SRYYM  <   "202109"    )
               OR    ( HENREI-B-RECESYUBETU (2:1)
                                       =   "2" OR  "3" )
               OR    ( HENREI-B-HKNJANUM
                                       =   "77777777"  )
                   CONTINUE
               ELSE
                   PERFORM 41011-HENREI-BODY-EDABAN-SEC
               END-IF
      *
               IF    ( HENREI-B-SCREEN-UPD-FLG
                                       =   ZERO        )
      *            保険情報から患者情報検索処理
                   PERFORM 41012-PTNUM-CHK-SEC
      *            患者ＩＤ更新処理
                   IF      WRK-PTHKNINF-MAX    =   1
                       PERFORM 41013-HENREI-BODY-UPDATE-SEC
                   END-IF
               ELSE
      *            紐付済みの患者情報・保険情報取得処理
                   IF      HENREI-B-PTID   >   ZERO
                       PERFORM 41014-PTINF-HENSYU-SEC
                   ELSE    
                       MOVE    "※※　解　　除　※※"
                                               TO
                                       SPA-GMN-H05-B-CHK-TPTNUM    (IDX)
                   END-IF
               END-IF
      *
               MOVE    IDX             TO  SPA-GMN-H05-BODY-MAX
               COMPUTE SPA-GMN-H05-BODY-MAX2
                                       =   SPA-GMN-H05-BODY-MAX    *   2
      *
            PERFORM 900-HENREI-BODY-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-CONS-BODY-LINE-MAX )  AND
                 ( FLG-HENREI-B        =   ZERO                   )
               MOVE    1                   TO  SPA-GMN-H05-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-H05-NEXT
           END-IF
      *
      *
           MOVE    2               TO  SPA-GMN-H05-CUR
      *
           .
       4101-HENREI-KANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    枝番取得処理
      *****************************************************************
       41011-HENREI-BODY-EDABAN-SEC    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATA-R
      *
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "key11"             TO  WRK-MCP-PATHNAME
           PERFORM 900-HENREI-BODY-READ-SEC
           IF      FLG-HENREI-B-CHK    =   ZERO
                UNSTRING    CHK-HENREI-B-HENREI-INFO2
                                       DELIMITED  BY  ","
                                       INTO    WRK-DATA (1)
                                               WRK-DATA (2)
                                               WRK-DATA (3)
                                               WRK-DATA (4)
                                               WRK-DATA (5)
                                               WRK-DATA (6)
                                               WRK-DATA (7)
                                               WRK-DATA (8)
                                               WRK-DATA (9)
               END-UNSTRING
               MOVE    WRK-DATA (7)     TO  SPA-GMN-H05-B-TEDABAN (IDX)
           END-IF
      *
           .
       41011-HENREI-BODY-EDABAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報から患者情報検索処理
      *****************************************************************
       41012-PTNUM-CHK-SEC             SECTION.
      *
            INITIALIZE                  WRK-PTHKNINF-AREA
      *
           EVALUATE    SPA-NAI-H05-K-TTEISYUTUSAKI (SPA-NAI-H05-SELNUM)
                           ALSO    SPA-NAI-H05-B-TRECESYUBETU (IDX)(2:1)
               WHEN    "1" ALSO    "2"
                   INITIALIZE                  PTKOHINF-REC 
                   MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                   MOVE    SPA-GMN-H05-B-TKIGO  (IDX)
                                           TO  PTKOH-FTNJANUM
                   MOVE    SPA-GMN-H05-B-TNUM   (IDX)
                                           TO  PTKOH-JKYSNUM
                   MOVE    "99999999"      TO  PTKOH-TEKSTYMD
                   MOVE    "00000000"      TO  PTKOH-TEKEDYMD
                   MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                   MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                   MOVE    "key15"         TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                       MOVE    "key15"         TO  MCP-PATHNAME
                       PERFORM 900-PTKOHINF-READ-N-SEC
                   ELSE
                       INITIALIZE                  PTKOHINF-REC
                       MOVE    1               TO  FLG-PTKOHINF
                   END-IF
      *
                   PERFORM         UNTIL   FLG-PTKOHINF        =   1
                                   OR      WRK-PTHKNINF-MAX    >   1
                       IF      WRK-PTKOH-PTID (1)  =   PTKOH-PTID
                           CONTINUE
                       ELSE                                      
      *                    患者情報
                           INITIALIZE                  PTINF-REC
                           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                           MOVE    PTKOH-PTID      TO  PTINF-PTID
                           PERFORM 900-PTINF-READ-SEC
      *                
                           ADD     1               TO  WRK-PTHKNINF-MAX
                           MOVE    PTKOHINF-REC    TO  WRK-PTKOHINF-REC
                                                      (WRK-PTHKNINF-MAX)
                       END-IF
      *
                       MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                       MOVE    "key15"         TO  MCP-PATHNAME
                       PERFORM 900-PTKOHINF-READ-N-SEC
                   END-PERFORM 
                   MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                   MOVE    "key15"         TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               WHEN    OTHER
      *            資格なしのときは検索しない 
                   IF      HENREI-B-HKNJANUM   =   "77777777"
                       CONTINUE
                   ELSE
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
                       MOVE    SPA-GMN-H05-B-THKNJANUM  (IDX)
                                               TO  PTHKN-HKNJANUM
                       MOVE    SPA-GMN-H05-B-TKIGO      (IDX)
                                               TO  PTHKN-KIGO
                       MOVE    SPA-GMN-H05-B-TNUM       (IDX)
                                               TO  PTHKN-NUM
                       MOVE    "99999999"      TO  PTHKN-TEKSTYMD
                       MOVE    "00000000"      TO  PTHKN-TEKEDYMD
                       MOVE    PTHKNINF-REC    TO  MCPDATA-REC
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key15"         TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                           MOVE    "key15"         TO  MCP-PATHNAME
                           PERFORM 900-PTHKNINF-READ-N-SEC
                       ELSE
                           INITIALIZE                  PTHKNINF-REC
                           MOVE    1               TO  FLG-PTHKNINF
                       END-IF
      *
                       PERFORM         UNTIL   FLG-PTHKNINF        =   1
                                       OR      WRK-PTHKNINF-MAX    >   1
                         IF      WRK-PTHKN-PTID (1)  =   PTHKN-PTID
                            CONTINUE
                         ELSE                                      
      *                    患者情報
                           INITIALIZE                  PTINF-REC
                           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                           MOVE    PTHKN-PTID      TO  PTINF-PTID
                           PERFORM 900-PTINF-READ-SEC
      *                
                           IF      PTHKN-HKNNUM    =   "039"
                               ADD     1           TO  WRK-PTHKNINF-MAX
                               MOVE    PTHKNINF-REC
                                                   TO  WRK-PTHKNINF-REC
                                                      (WRK-PTHKNINF-MAX)
                           ELSE    
                               IF      SPA-GMN-H05-B-TEDABAN (IDX)
                                                   NOT =   SPACE
                                   IF    ( SPA-GMN-H05-B-TEDABAN (IDX)
                                                   =   PTHKN-EDABAN   )
                                       ADD     1   TO  WRK-PTHKNINF-MAX
                                       MOVE    PTHKNINF-REC
                                                   TO  WRK-PTHKNINF-REC
                                                      (WRK-PTHKNINF-MAX)
                                   ELSE                   
                                       IF    ( PTHKN-EDABAN
                                                   =   SPACE          ) 
                                       AND   ( SPA-NAI-H05-B-TBIRTHDAY
                                                               (IDX)
                                                   =   PTINF-BIRTHDAY )
                                           ADD     1
                                                   TO  WRK-PTHKNINF-MAX
                                           MOVE    PTHKNINF-REC
                                                   TO  WRK-PTHKNINF-REC
                                                      (WRK-PTHKNINF-MAX)
                                       END-IF
                                   END-IF
                               ELSE 
                                   IF      SPA-NAI-H05-B-TBIRTHDAY
                                                               (IDX)
                                                   =   PTINF-BIRTHDAY
                                       ADD     1   TO  WRK-PTHKNINF-MAX
                                       MOVE    PTHKNINF-REC
                                                   TO  WRK-PTHKNINF-REC
                                                      (WRK-PTHKNINF-MAX)
                                   END-IF
                               END-IF
                           END-IF
                         END-IF  
      *
                         MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                         MOVE    "key15"          TO  MCP-PATHNAME
                         PERFORM 900-PTHKNINF-READ-N-SEC
                       END-PERFORM 
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key15"         TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   END-IF        
           END-EVALUATE
      *
           MOVE    WRK-PTHKNINF-MAX    TO  SPA-NAI-H05-B-CHK-TMAX (IDX)
           IF      WRK-PTHKNINF-MAX    =   0
               MOVE    "※※該当患者なし※※"
                                       TO  SPA-GMN-H05-B-CHK-TPTNUM(IDX)
               GO  TO  41012-PTNUM-CHK-EXT
           ELSE
               IF      WRK-PTHKNINF-MAX    >   1
                   MOVE    "※※　複数該当　※※"
                                       TO  SPA-GMN-H05-B-CHK-TPTNUM(IDX)
                   GO  TO  41012-PTNUM-CHK-EXT
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-HKN-CHK
      *    保険情報 
           EVALUATE    SPA-NAI-H05-K-TTEISYUTUSAKI (SPA-NAI-H05-SELNUM)
                           ALSO    SPA-NAI-H05-B-TRECESYUBETU (IDX)(2:1)
               WHEN    "1" ALSO    "2"
                   MOVE    WRK-PTKOH-PTID (1)  TO
                                       SPA-NAI-H05-B-CHK-TPTID     (IDX)
                   STRING  WRK-PTKOH-FTNJANUM (1)  DELIMITED  BY  SPACE
                           "　"                    DELIMITED  BY  SIZE
                           WRK-PTKOH-JKYSNUM  (1)  DELIMITED  BY  SPACE
                                   INTO    SPA-GMN-H05-B-CHK-TKIGO (IDX)
                   END-STRING
               WHEN    OTHER         
                   MOVE    WRK-PTHKN-PTID     (1)
                                               TO
                                       SPA-NAI-H05-B-CHK-TPTID     (IDX)
                   MOVE    WRK-PTHKN-HKNJANUM (1)
                                               TO
                                       SPA-GMN-H05-B-CHK-THKNJANUM (IDX)
                   STRING  WRK-PTHKN-KIGO (1)     DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-PTHKN-NUM  (1)     DELIMITED  BY  SPACE
                                   INTO    SPA-GMN-H05-B-CHK-TKIGO (IDX)
                   END-STRING 
                   MOVE    WRK-PTHKN-EDABAN   (1)
                                               TO
                                       SPA-GMN-H05-B-CHK-TEDABAN   (IDX)
           END-EVALUATE
      *                 
      *    保険情報チェック処理
           PERFORM 41019-HKN-CHK-SEC 
      *
      *    患者情報
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    SPA-NAI-H05-B-CHK-TPTID  (IDX)
                                   TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      FLG-HKN-CHK     =   ZERO
               MOVE    SPTID-PTNUM     TO
                                       SPA-GMN-H05-B-CHK-TPTNUM    (IDX)
           ELSE
               STRING  "*"             DELIMITED  BY  SIZE
                       SPTID-PTNUM     DELIMITED  BY  SPACE
                                       INTO    SPA-GMN-H05-B-CHK-TPTNUM
                                                                   (IDX)
               END-STRING
           END-IF
      *    患者情報
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-H05-B-CHK-TPTID  (IDX)
                                   TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *                
           MOVE   PTINF-KANANAME   TO  SPA-NAI-H05-B-CHK-TKANANAME (IDX)
           MOVE   PTINF-NAME       TO  SPA-GMN-H05-B-CHK-TNAME     (IDX)
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"    TO  SPA-GMN-H05-B-CHK-TSEX      (IDX)
               WHEN    "2"
                   MOVE    "女"    TO SPA-GMN-H05-B-CHK-TSEX       (IDX)
           END-EVALUATE
           MOVE   PTINF-BIRTHDAY   TO  SPA-NAI-H05-B-CHK-TBIRTHDAY (IDX)
                                       WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-H05-B-CHK-TBIRTHDAY (IDX)
      *
            .
       41012-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者ＩＤ更新処理
      *****************************************************************
       41013-HENREI-BODY-UPDATE-SEC   SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SPA-NAI-H05-B-CHK-TPTID (IDX)
                                       TO  HENREI-B-PTID
           MOVE    1                   TO  HENREI-B-SCREEN-UPD-FLG
           MOVE    SMCNDATE-YMD        TO  HENREI-B-UPYMD
           MOVE    SMCNDATE-HMS        TO  HENREI-B-UPHMS
      *
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
            PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC          NOT =   ZERO
               CALL "coblog"   USING   "abort tbl_henrei_body update"
               MOVE    "1001"              TO  SPA-ERRCD
           ELSE
      *        更新した患者ＩＤを再度設定  
               MOVE    HENREI-B-PTID   TO  SPA-NAI-H05-B-TPTID (IDX)
           END-IF
      *
            .
       41013-HENREI-BODY-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    紐付けた患者情報・保険情報取得処理
      *****************************************************************
       41014-PTINF-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-HKN-CHK
      *     
           MOVE    HENREI-B-PTID   TO  SPA-NAI-H05-B-CHK-TPTID     (IDX)
      *    患者情報
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    HENREI-B-PTID   TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE   PTINF-KANANAME   TO  SPA-NAI-H05-B-CHK-TKANANAME (IDX)
           MOVE   PTINF-NAME       TO  SPA-GMN-H05-B-CHK-TNAME     (IDX)
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"    TO  SPA-GMN-H05-B-CHK-TSEX      (IDX)
               WHEN    "2"
                   MOVE    "女"    TO  SPA-GMN-H05-B-CHK-TSEX      (IDX)
           END-EVALUATE
           MOVE   PTINF-BIRTHDAY   TO  SPA-NAI-H05-B-CHK-TBIRTHDAY (IDX)
                                       WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-H05-B-CHK-TBIRTHDAY (IDX)
      *     
      *    保険情報
           EVALUATE    SPA-NAI-H05-K-TTEISYUTUSAKI (SPA-NAI-H05-SELNUM)
                           ALSO    SPA-NAI-H05-B-TRECESYUBETU (IDX)(2:1)
               WHEN    "1" ALSO    "2"
                   INITIALIZE                  PTKOHINF-REC 
                   MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                   MOVE    HENREI-B-PTID   TO  PTKOH-PTID
                   MOVE    SPA-GMN-H05-B-TKIGO  (IDX)
                                           TO  PTKOH-FTNJANUM
                   MOVE    SPA-GMN-H05-B-TNUM   (IDX)
                                           TO  PTKOH-JKYSNUM
                   MOVE    "99999999"      TO  PTKOH-TEKSTYMD
                   MOVE    "00000000"      TO  PTKOH-TEKEDYMD
                   MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                   MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                   MOVE    "key16"         TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                       MOVE    "key16"         TO  MCP-PATHNAME
                       PERFORM 900-PTKOHINF-READ-N-SEC
                   ELSE
                       INITIALIZE                  PTKOHINF-REC
                       MOVE    1               TO  FLG-PTKOHINF
                   END-IF
                   MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                   MOVE    "key16"         TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      FLG-PTKOHINF    =   ZERO
                       STRING  PTKOH-FTNJANUM  DELIMITED  BY  SPACE
                               "　"            DELIMITED  BY  SIZE
                               PTKOH-JKYSNUM   DELIMITED  BY  SPACE
                                   INTO    SPA-GMN-H05-B-CHK-TKIGO (IDX)
                       END-STRING
      *                 
      *                保険情報チェック処理
                       PERFORM 41019-HKN-CHK-SEC 
                   END-IF
                WHEN    OTHER
      *            資格なしのときは検索しない 
                   IF      HENREI-B-HKNJANUM   =   "77777777"
                       CONTINUE
                   ELSE
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
                       MOVE    HENREI-B-PTID   TO  PTHKN-PTID
                       MOVE    SPA-GMN-H05-B-THKNJANUM  (IDX)
                                               TO  PTHKN-HKNJANUM
                       MOVE    SPA-GMN-H05-B-TKIGO      (IDX)
                                               TO  PTHKN-KIGO
                       MOVE    SPA-GMN-H05-B-TNUM       (IDX)
                                               TO  PTHKN-NUM
                       MOVE    "99999999"      TO  PTHKN-TEKSTYMD
                       MOVE    "00000000"      TO  PTHKN-TEKEDYMD
                       MOVE    PTHKNINF-REC    TO  MCPDATA-REC
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key14"         TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC          =   ZERO
                           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                           MOVE    "key14"         TO  MCP-PATHNAME
                           PERFORM 900-PTHKNINF-READ-N-SEC
                       ELSE
                           INITIALIZE                  PTHKNINF-REC
                           MOVE    1               TO  FLG-PTHKNINF
                       END-IF
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key14"         TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
                       IF      FLG-PTHKNINF    =   ZERO
                           MOVE    PTHKN-HKNJANUM  TO
                                       SPA-GMN-H05-B-CHK-THKNJANUM (IDX)
                           STRING  PTHKN-KIGO      DELIMITED  BY  SPACE
                                   "　"            DELIMITED  BY  SIZE
                                   PTHKN-NUM       DELIMITED  BY  SPACE
                                   INTO    SPA-GMN-H05-B-CHK-TKIGO (IDX)
                           END-STRING 
                           MOVE    PTHKN-EDABAN    TO
                                       SPA-GMN-H05-B-CHK-TEDABAN   (IDX)
      *                 
      *                    保険情報チェック処理
                           PERFORM 41019-HKN-CHK-SEC 
                       END-IF
                   END-IF        
           END-EVALUATE
      *
      *    患者番号
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    HENREI-B-PTID   TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      FLG-HKN-CHK     =   ZERO
               MOVE    SPTID-PTNUM     TO
                                       SPA-GMN-H05-B-CHK-TPTNUM    (IDX)
           ELSE
               STRING  "*"             DELIMITED  BY  SIZE
                       SPTID-PTNUM     DELIMITED  BY  SPACE
                                       INTO    SPA-GMN-H05-B-CHK-TPTNUM
                                                                   (IDX)
               END-STRING
           END-IF    
             .
       41014-PTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報チェック処理
      *****************************************************************
       41019-HKN-CHK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKN-CHK
      *
           EVALUATE    SPA-NAI-H05-K-TTEISYUTUSAKI (SPA-NAI-H05-SELNUM)
                           ALSO    SPA-NAI-H05-B-TRECESYUBETU (IDX)(2:1)
               WHEN    "1" ALSO    "2"
                   INITIALIZE                  PTKOHINF-REC 
                   MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                   MOVE    SPA-NAI-H05-B-CHK-TPTID (IDX)
                                           TO  PTKOH-PTID
                   MOVE    SPA-GMN-H05-B-TKIGO  (IDX)
                                           TO  PTKOH-FTNJANUM
                   MOVE    SPA-GMN-H05-B-TNUM   (IDX)
                                           TO  PTKOH-JKYSNUM
                   MOVE    SPA-NAI-H05-B-TSRYYM (IDX)
                                           TO  PTKOH-TEKSTYMD (1:6)
                                               PTKOH-TEKEDYMD (1:6)
                   MOVE    "31"            TO  PTKOH-TEKSTYMD (7:2)
                   MOVE    "01"            TO  PTKOH-TEKEDYMD (7:2)
                   MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                   MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                   MOVE    "key16"         TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                       MOVE    "key16"         TO  MCP-PATHNAME
                       PERFORM 900-PTKOHINF-READ-N-SEC
                   ELSE
                       INITIALIZE                  PTKOHINF-REC
                       MOVE    1               TO  FLG-PTKOHINF
                   END-IF
                   MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                   MOVE    "key16"         TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   IF      FLG-PTKOHINF    =   1
                       MOVE    1               TO  FLG-HKN-CHK
                   END-IF
                WHEN    OTHER
      *            資格なしのときは検索しない 
                   IF      HENREI-B-HKNJANUM   =   "77777777"
                       CONTINUE
                   ELSE
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
                       MOVE    SPA-NAI-H05-B-CHK-TPTID (IDX)
                                               TO  PTHKN-PTID
                       MOVE    SPA-GMN-H05-B-THKNJANUM  (IDX)
                                               TO  PTHKN-HKNJANUM
                       MOVE    SPA-GMN-H05-B-TKIGO      (IDX)
                                               TO  PTHKN-KIGO
                       MOVE    SPA-GMN-H05-B-TNUM       (IDX)
                                               TO  PTHKN-NUM
                       MOVE    SPA-NAI-H05-B-TSRYYM (IDX)
                                               TO  PTHKN-TEKSTYMD (1:6)
                                                   PTHKN-TEKEDYMD (1:6)
                       MOVE    "31"            TO  PTHKN-TEKSTYMD (7:2)
                       MOVE    "01"            TO  PTHKN-TEKEDYMD (7:2)
                       MOVE    PTHKNINF-REC    TO  MCPDATA-REC
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key14"         TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                      IF      MCP-RC          =   ZERO
                           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                           MOVE    "key14"         TO  MCP-PATHNAME
                           PERFORM 900-DBFETCH-SEC
                           IF      MCP-RC          =   ZERO
                               MOVE  MCPDATA-REC       TO  PTHKNINF-REC
                               MOVE  ZERO              TO  FLG-PTHKNINF
                            ELSE
                               INITIALIZE                  PTHKNINF-REC
                               MOVE    1               TO  FLG-PTHKNINF
                           END-IF
                       ELSE
                               INITIALIZE                  PTHKNINF-REC
                               MOVE    1               TO  FLG-PTHKNINF
                       END-IF
      *
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key14"         TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
      *
      *                診療年月に該当する保険情報が存在しないとき
                       IF      FLG-PTHKNINF    =   1 
                           MOVE    1               TO  FLG-HKN-CHK
                       END-IF    
                   END-IF        
           END-EVALUATE
           .
       41019-HKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    管理情報一覧選択処理
      *****************************************************************
       450-KANRILST-SELECT-SEC         SECTION.
      *
           IF      SPA-NAI-H05-B-SELNUM    >   0
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      450-KANRILST-SELECT-EXT
           END-IF
      *
      *
           INITIALIZE                      SPA-GMN-H05-KOMOKU
                                           SPA-NAI-H05-KOMOKU
      *
           INITIALIZE                      SPA-GMN-H05-BODY-TBL
                                           SPA-NAI-H05-BODY-TBL
                                           SPA-GMN-H05-NEXT
      *
           MOVE    ZERO                TO  WRK-SELNO
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H05-KANRI-MAX
               IF      H05-KANRI-LIST-SELECT (IDX)
                                       =   "T"
                   MOVE    IDX             TO  WRK-SELNO
                   MOVE    SPA-GMN-H05-KANRI-MAX
                                           TO  IDX
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-GMN-H05-SELNUM
                                               SPA-NAI-H05-SELNUM
                                               H05-KANRI-LIST-ROW
               MOVE    1                   TO  SPA-GMN-H05-PAGE
               MOVE    1                   TO  FLG-BODY-ROW
      *        返戻取り込みデータ明細情報編集処理
               PERFORM 4101-HENREI-KANRI-HENSYU-SEC
           END-IF
           .
       450-KANRILST-SELECT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    選択番号（明細）入力処理
      *****************************************************************
       420-B-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    2                   TO  SPA-GMN-H05-CUR
      *
           INITIALIZE                      SPA-GMN-H05-KOMOKU1
                                           SPA-NAI-H05-KOMOKU1
      *
           MOVE    H05-B-SELNUM        TO  SPA-GMN-H05-B-SELNUM
           MOVE    ZERO                TO  SPA-NAI-H05-B-SELNUM
      *
           IF      SPA-GMN-H05-B-SELNUM    =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               COMPUTE WRK-BANGO       =   SPA-GMN-H05-B-SELNUM
                                       - ( SPA-GMN-H05-PAGE    -   1 )
                                       *   WRK-CONS-BODY-LINE-MAX
               IF    ( WRK-BANGO       >   SPA-GMN-H05-BODY-MAX )
               OR    ( WRK-BANGO       <   1                    )
                   MOVE    "0001"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-BANGO       TO  SPA-NAI-H05-B-SELNUM
                   COMPUTE H05-BODY-LIST-ROW
                                       =   SPA-NAI-H05-B-SELNUM *   2
                                           -   1
      *            請求管理と紐付け済のときは解除できない
                   IF      SPA-NAI-H05-B-TSEIKYU-FLG
                                               (SPA-NAI-H05-B-SELNUM)
                                               >   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                  END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        返戻取り込みデータ明細情報編集処理
              PERFORM 4201-HENREI-BODY-HENSYU-SEC
           ELSE   
               INITIALIZE                      SPA-GMN-H05-KOMOKU1
                                               SPA-NAI-H05-KOMOKU1
                                               SPA-GMN-H05-B-SELNUM
                                               SPA-NAI-H05-B-SELNUM
           END-IF
      *
           .
       420-B-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   返戻取り込みデータ明細情報編集処理
      *****************************************************************
       4201-HENREI-BODY-HENSYU-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-H05-KOMOKU1
                                           SPA-NAI-H05-KOMOKU1
      *
           IF    ( SPA-NAI-H05-B-CHK-TPTID (SPA-NAI-H05-B-SELNUM)
                                       >   0 )
           OR    ( SPA-NAI-H05-B-TPTID     (SPA-NAI-H05-B-SELNUM)
                                       >   0 )
      *        請求管理と紐付けされているかチェック
               INITIALIZE                      HENREI-B-REC
               MOVE    SPA-HOSPNUM     TO  HENREI-B-HOSPNUM
               MOVE    SPA-NAI-H05-K-TSYORIYM       (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-SYORIYM
               MOVE    SPA-NAI-H05-K-TTEISYUTUSAKI  (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI
               MOVE    SPA-NAI-H05-K-TTEISYUTUSAKI2 (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI2
               MOVE    SPA-NAI-H05-K-THOSPCD        (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-HOSPCD
               MOVE    SPA-NAI-H05-B-TRECEIPT-NO  (SPA-NAI-H05-B-SELNUM)
                                       TO  HENREI-B-RECEIPT-NO
               MOVE    HENREI-B-REC    TO  MCPDATA-REC
               MOVE    "key13"         TO  WRK-MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
               IF      CHK-HENREI-B-S-TEISYUTUSAKI (1)
                                       NOT =   SPACE
                       MOVE    "0009"          TO  SPA-ERRCD
                       MOVE    2               TO  SPA-GMN-H05-CUR
                       GO  TO  410-KAIJYO-EXT
               END-IF
      *
               MOVE    SPA-GMN-H05-B-CHK-TPTNUM   (SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-GMN-H05-PTNUM
               MOVE    SPA-GMN-H05-B-CHK-TNAME   (SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-GMN-H05-NAME
               MOVE    SPA-NAI-H05-B-CHK-TKANANAME(SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-GMN-H05-KANANAME
               MOVE    SPA-GMN-H05-B-CHK-TBIRTHDAY(SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-GMN-H05-BIRTHDAY
               MOVE    SPA-GMN-H05-B-CHK-THKNJANUM(SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-GMN-H05-HKNJANUM
               MOVE    SPA-GMN-H05-B-CHK-TKIGO    (SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-GMN-H05-KIGO
               MOVE    SPA-GMN-H05-B-CHK-TEDABAN  (SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-GMN-H05-EDABAN
               MOVE    SPA-NAI-H05-B-CHK-TPTID    (SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-NAI-H05-PTID
               MOVE    SPA-NAI-H05-B-CHK-TBIRTHDAY(SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-NAI-H05-BIRTHDAY
               MOVE    SPA-GMN-H05-B-CHK-TSEX     (SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-GMN-H05-SEX 
               MOVE    SPA-NAI-H05-B-CHK-TSEX     (SPA-NAI-H05-B-SELNUM)
                                       TO  SPA-NAI-H05-SEX 
           END-IF
      *
           MOVE    2                   TO  SPA-GMN-H05-CUR
           .
       4201-HENREI-BODY-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    一覧選択処理
      *****************************************************************
       450-BODYLST-SELECT-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-H05-KOMOKU1
                                           SPA-NAI-H05-KOMOKU1
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H05-BODY-MAX2
               IF      H05-BODY-LIST-SELECT (IDX)
                                       =   "T"
                   MOVE    IDX             TO  WRK-SELNO
                   COMPUTE IDZZ        = ( WRK-SELNO   +   1 )
                                           /   2
      *            前回と違う行なら終わり                                     
                   IF      IDZZ        =   SPA-GMN-H05-B-SELNUM
                       CONTINUE
                   ELSE     
                       MOVE    SPA-GMN-H05-BODY-MAX2
                                               TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               COMPUTE SPA-NAI-H05-B-SELNUM  = ( WRK-SELNO   +   1 )
                                                 /   2
               MOVE    SPA-GMN-H05-B-TSEQNUM (SPA-NAI-H05-B-SELNUM)
                                          TO  SPA-GMN-H05-B-SELNUM
               COMPUTE H05-BODY-LIST-ROW  =   SPA-NAI-H05-B-SELNUM *   2
                                              -   1
      *        請求管理と紐付け済のときは解除できない
               IF      SPA-NAI-H05-B-TSEIKYU-FLG (SPA-NAI-H05-B-SELNUM)
                                           >   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
                   INITIALIZE                      SPA-GMN-H05-KOMOKU1
                                                   SPA-NAI-H05-KOMOKU1
                                                   SPA-GMN-H05-B-SELNUM
                                                   SPA-NAI-H05-B-SELNUM
                   MOVE    2                   TO  SPA-GMN-H05-CUR
               ELSE
      *            明細情報編集処理
                   PERFORM 4201-HENREI-BODY-HENSYU-SEC
               END-IF    
           END-IF
           .
       450-BODYLST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           IF      SPA-NAI-H05-B-SELNUM    >   0
               INITIALIZE                      SPA-GMN-H05-KOMOKU1
                                               SPA-NAI-H05-KOMOKU1
                                               SPA-GMN-H05-B-SELNUM
                                               SPA-NAI-H05-B-SELNUM
               MOVE    2                   TO  SPA-GMN-H05-CUR
           ELSE
               INITIALIZE                      SPA-GMN-H05-BODY-TBL
                                               SPA-NAI-H05-BODY-TBL
                                               SPA-GMN-H05-KOMOKU
                                               SPA-NAI-H05-KOMOKU
                                               SPA-GMN-H05-NEXT
               MOVE    1                   TO  SPA-GMN-H05-CUR
           END-IF
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *        解除処理
      *****************************************************************
       410-KAIJYO-SEC                  SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    SPA-NAI-H05-K-TSYORIYM       (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-SYORIYM
           MOVE    SPA-NAI-H05-K-TTEISYUTUSAKI  (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI
           MOVE    SPA-NAI-H05-K-TTEISYUTUSAKI2 (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI2
           MOVE    SPA-NAI-H05-K-THOSPCD        (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-HOSPCD
           MOVE    SPA-NAI-H05-B-TRECEIPT-NO    (SPA-NAI-H05-B-SELNUM)
                                       TO  HENREI-B-RECEIPT-NO
           MOVE    ZERO                TO  HENREI-B-PTID
           MOVE    1                   TO  HENREI-B-SCREEN-UPD-FLG
           MOVE    SMCNDATE-YMD        TO  HENREI-B-UPYMD
           MOVE    SMCNDATE-HMS        TO  HENREI-B-UPHMS
      *
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
            PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC          NOT =   ZERO
               CALL "coblog"   USING   "abort tbl_henrei_body update"
               MOVE    "1002"              TO  SPA-ERRCD
           ELSE
      *        返戻取り込みデータ明細情報編集処理
               PERFORM 4101-HENREI-KANRI-HENSYU-SEC
           END-IF
           .
       410-KAIJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *        設定処理
      *****************************************************************
       490-UPDATE-SEC                  SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                      HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    SPA-NAI-H05-K-TSYORIYM       (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-SYORIYM
           MOVE    SPA-NAI-H05-K-TTEISYUTUSAKI  (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI
           MOVE    SPA-NAI-H05-K-TTEISYUTUSAKI2 (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI2
           MOVE    SPA-NAI-H05-K-THOSPCD        (SPA-NAI-H05-SELNUM)
                                       TO  HENREI-B-HOSPCD
           MOVE    SPA-NAI-H05-B-TRECEIPT-NO    (SPA-NAI-H05-B-SELNUM)
                                       TO  HENREI-B-RECEIPT-NO
           MOVE    SPA-NAI-H05-PTID    TO  HENREI-B-PTID
           MOVE    1                   TO  HENREI-B-SCREEN-UPD-FLG
           MOVE    SMCNDATE-YMD        TO  HENREI-B-UPYMD
           MOVE    SMCNDATE-HMS        TO  HENREI-B-UPHMS
      *
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
            IF      MCP-RC          NOT =   ZERO
               CALL "coblog"   USING   "abort tbl_henrei_body update"
               MOVE    "1003"              TO  SPA-ERRCD
           ELSE
      *        返戻取り込みデータ明細情報編集処理
               PERFORM 4101-HENREI-KANRI-HENSYU-SEC
           END-IF
           .
       490-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       480-KENSAKU-SEC             SECTION.
      *
           IF      SPA-NAI-H05-B-SELNUM    =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-H05-CUR
               GO  TO  480-KENSAKU-EXT
           END-IF
      *         
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-H01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "H05"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名を設定
           IF      SPA-NAI-H05-B-SELNUM  >   0
               MOVE    SPA-GMN-H05-B-TNAME  (SPA-NAI-H05-B-SELNUM)
                                           TO  SPA-NAME
           END-IF
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       480-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      * 
           MOVE    2               TO  SPA-GMN-H05-CUR
      *
           IF      SPA-NAI-H05-B-SELNUM    >   0
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      460-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-H05-PAGE    =   1
               MOVE    "0005"              TO  SPA-ERRCD
               GO      TO      460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-H05-PAGE    =   SPA-GMN-H05-PAGE    -   1
      *
      *    返戻取り込みデータ明細情報編集処理
           PERFORM 4101-HENREI-KANRI-HENSYU-SEC
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      * 
           MOVE    2                   TO  SPA-GMN-H05-CUR
      *
           IF      SPA-NAI-H05-B-SELNUM    >   0
               MOVE    "0007"          TO  SPA-ERRCD
               GO      TO      470-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-H05-NEXT    =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               GO      TO      470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-H05-PAGE
      * 
      *    返戻取り込みデータ明細情報編集処理
           PERFORM 4101-HENREI-KANRI-HENSYU-SEC
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-NAI-H05-B-SELNUM    >   0
               MOVE    "0101"          TO  SPA-HIDCD
               GO      TO      210-BACK-EXT
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
           . 
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC           SECTION.
      *
           MOVE    "H01"               TO  SPA-SAKIPG
           MOVE    "H05"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  SPA-H01KYOUTU
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-SYORI-EXT.
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
               PERFORM 520-HIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      * 
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "H05"               TO  MCP-WINDOW
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
               MOVE    ZERO                TO  WRK-H05-KANRI-ROW
           ELSE
               MOVE    H05-KANRI-LIST-ROW  TO  WRK-H05-KANRI-ROW
           END-IF
      *
           IF      FLG-BODY-ROW        =   1
               MOVE    ZERO                TO  WRK-H05-BODY-ROW
           ELSE
               MOVE    H05-BODY-LIST-ROW   TO  WRK-H05-BODY-ROW
           END-IF
      * 
           INITIALIZE                  H05
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H05-KANRI-MAX
               MOVE    SPA-GMN-H05-K-TSEQNUM     (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9            TO  H05-KANRI-TSEQNUM    (IDX)
               MOVE    SPA-GMN-H05-K-TSYORIYM    (IDX)
                                           TO  H05-KANRI-TSYORIYM  (IDX)
               MOVE    SPA-GMN-H05-K-TTEISYUTUSAKI    (IDX)
                                           TO  H05-KANRI-TTEISYUTUSAKI
                                                                   (IDX)
               MOVE    SPA-GMN-H05-K-TTEISYUTUSAKI2   (IDX)
                                           TO  H05-KANRI-TTEISYUTUSAKI2
                                                                   (IDX)
               MOVE    SPA-GMN-H05-K-TCNT      (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  H05-KANRI-TCNT      (IDX)
               MOVE    SPA-GMN-H05-K-TERRCD    (IDX)
                                           TO  H05-KANRI-TERRCD    (IDX)
               MOVE    SPA-GMN-H05-K-TCREYMD (IDX)
                                           TO  H05-KANRI-TCREYMD   (IDX)
               MOVE    SPA-GMN-H05-K-THOSPCD   (IDX)
                                           TO  H05-KANRI-THOSPCD   (IDX)
      *
               IF      IDX                 =   SPA-NAI-H05-SELNUM
                   MOVE    "T"                 TO  H05-KANRI-LIST-SELECT
                                                                   (IDX)
               ELSE
                   MOVE    "F"                 TO  H05-KANRI-LIST-SELECT
                                                                   (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-H05-KANRI-MAX
                                       TO  H05-KANRI-LIST-COUNT
           MOVE    ZERO                TO  H05-KANRI-LIST-ROW
           IF      WRK-H05-KANRI-ROW   >   ZERO
               MOVE    WRK-H05-KANRI-ROW   TO  H05-KANRI-LIST-ROW
           END-IF
      *
           MOVE    SPA-GMN-H05-SELNUM  TO  H05-SELNUM
      *
           MOVE    ZERO                TO  IDX2
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H05-BODY-MAX
               ADD     1                   TO  IDX2
               MOVE    SPA-GMN-H05-B-TSEQNUM    (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  H05-BODY-TSEQNUM   (IDX2)
               MOVE    SPA-GMN-H05-B-TNYUGAIKBN (IDX)
                                           TO  H05-BODY-TNYUGAIKBN(IDX2)
               MOVE    SPA-GMN-H05-B-TPTNUM     (IDX)
                                           TO  H05-BODY-TPTNUM    (IDX2)
               MOVE    SPA-GMN-H05-B-TNAME      (IDX)
                                           TO  H05-BODY-TNAME     (IDX2)
               MOVE    SPA-GMN-H05-B-TBIRTHDAY  (IDX)
                                           TO  H05-BODY-TBIRTHDAY (IDX2)
               MOVE    SPA-GMN-H05-B-TSEX (IDX)
                                           TO  H05-BODY-TSEX      (IDX2)
               MOVE    SPA-GMN-H05-B-TSRYYM     (IDX)
                                           TO  H05-BODY-TSRYYM    (IDX2)
               IF      SPA-NAI-H05-B-TPCR (IDX)
                                      =   "1"
                   STRING    "*"           DELIMITED  BY  SIZE
                             SPA-GMN-H05-B-TRECESYUBETU (IDX)
                                           DELIMITED  BY  SIZE 
                                           INTO
                                           H05-BODY-TRECESYUBETU  (IDX2)
                   END-STRING
               ELSE
                   MOVE    SPA-GMN-H05-B-TRECESYUBETU (IDX)
                                           TO
                                           H05-BODY-TRECESYUBETU  (IDX2)
               END-IF
               IF     SPA-GMN-H05-B-THKNJANUM  (IDX)
                                   NOT =   "00000000"
                   MOVE    SPA-GMN-H05-B-THKNJANUM  (IDX)
                                           TO  H05-BODY-THKNJANUM (IDX2)
               END-IF
               STRING  SPA-GMN-H05-B-TKIGO (IDX)   DELIMITED   BY  SPACE
                       "　"                        DELIMITED   BY  SIZE
                       SPA-GMN-H05-B-TNUM  (IDX)   DELIMITED   BY  SPACE
                                           INTO    H05-BODY-TKIGO (IDX2)
               END-STRING
               MOVE    SPA-GMN-H05-B-TRECEIPT-KBN (IDX)
                                           TO  H05-BODY-TRECEIPT-KBN
                                                                  (IDX2)
               MOVE    SPA-GMN-H05-B-TEDABAN   (IDX)
                                           TO  H05-BODY-TEDABAN   (IDX2)
               MOVE    SPA-GMN-H05-B-TDISASTER (IDX)
                                           TO  H05-BODY-TDISASTER (IDX2)
      *
               MOVE    "#FFCC99"           TO
                                           H05-BODY-LIST-BGCOLOR  (IDX2)
              IF      SPA-NAI-H05-B-TPTID    (IDX)
                                       >   ZERO
      *           （ベビーブルー）
                   MOVE    "#bbe2f1"           TO
                                           H05-BODY-LIST-BGCOLOR  (IDX2)
               END-IF
               IF      IDX                 =   SPA-GMN-H05-B-SELNUM
                   MOVE    "T"                 TO  H05-BODY-LIST-SELECT
                                                                  (IDX2)
               ELSE
                   MOVE    "F"                 TO  H05-BODY-LIST-SELECT
                                                                  (IDX2)
               END-IF
      *
               ADD     1                   TO  IDX2
               MOVE    SPA-GMN-H05-B-CHK-TPTNUM     (IDX)
                                           TO  H05-BODY-TPTNUM    (IDX2)
               MOVE    SPA-GMN-H05-B-CHK-TNAME      (IDX)
                                           TO  H05-BODY-TNAME     (IDX2)
               MOVE    SPA-GMN-H05-B-CHK-TBIRTHDAY  (IDX)
                                           TO  H05-BODY-TBIRTHDAY (IDX2)
               MOVE    SPA-GMN-H05-B-CHK-TSEX       (IDX)
                                           TO  H05-BODY-TSEX      (IDX2)
               MOVE    SPA-GMN-H05-B-CHK-THKNJANUM (IDX)
                                           TO  H05-BODY-THKNJANUM (IDX2)
               MOVE    SPA-GMN-H05-B-CHK-TKIGO     (IDX)
                                           TO  H05-BODY-TKIGO     (IDX2)
               MOVE    SPA-GMN-H05-B-CHK-TEDABAN   (IDX)
                                           TO  H05-BODY-TEDABAN   (IDX2)
               IF      IDX                 =   SPA-GMN-H05-B-SELNUM
                   MOVE    "T"                 TO  H05-BODY-LIST-SELECT
                                                                  (IDX2)
               ELSE
                   MOVE    "F"                 TO  H05-BODY-LIST-SELECT
                                                                  (IDX2)
               END-IF
           END-PERFORM
           MOVE    IDX2                    TO  H05-BODY-LIST-COUNT
           MOVE    ZERO                    TO  H05-BODY-LIST-ROW
           IF      WRK-H05-BODY-ROW        >   ZERO
               MOVE    WRK-H05-BODY-ROW        TO  H05-BODY-LIST-ROW
               MOVE    2                       TO  H05-BODY-LIST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-H05-B-HYODAI    TO  H05-BODY-LIST-HYODAI
           MOVE    SPA-GMN-H05-B-HYODAI2   TO  H05-BODY-LIST-HYODAI2
      *
           MOVE    SPA-GMN-H05-B-SELNUM    TO  H05-B-SELNUM
      *
           IF      SPA-NAI-H05-B-SELNUM    >   0
               MOVE    SPA-GMN-H05-B-TPTNUM   (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TPTNUM      (1)
               MOVE    SPA-GMN-H05-B-TNAME    (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TNAME       (1)
      **       MOVE    SPA-NAI-H05-B-TKANANAME (SPA-NAI-H05-B-SELNUM)
      **                                   TO  SPA-GMN-H05-KANANAME(1)
               MOVE    SPA-GMN-H05-B-TBIRTHDAY (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TBIRTHDAY   (1)
               MOVE    SPA-GMN-H05-B-TSEX      (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TSEX        (1)
               MOVE    SPA-GMN-H05-B-THKNJANUM (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-THKNJANUM   (1)
               STRING  SPA-GMN-H05-B-TKIGO (SPA-NAI-H05-B-SELNUM)
                                           DELIMITED   BY  SPACE
                       "　"                DELIMITED   BY  SIZE
                       SPA-GMN-H05-B-TNUM  (SPA-NAI-H05-B-SELNUM)
                                           DELIMITED   BY  SPACE
                                           INTO    H05-CHK-TKIGO   (1)
               END-STRING
               MOVE    SPA-GMN-H05-B-TEDABAN      (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TEDABAN     (1)
               MOVE    SPA-GMN-H05-B-TNYUGAIKBN   (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TNYUGAIKBN   (1)
               IF      SPA-NAI-H05-B-TPCR  (SPA-NAI-H05-B-SELNUM)
                                           =   "1"
                   STRING    "*"           DELIMITED  BY  SIZE
                             SPA-GMN-H05-B-TRECESYUBETU
                                               (SPA-NAI-H05-B-SELNUM)
                                           DELIMITED  BY  SIZE 
                                           INTO H05-CHK-TRECESYUBETU (1)
                   END-STRING
               ELSE
                   MOVE    SPA-GMN-H05-B-TRECESYUBETU
                                               (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TRECESYUBETU (1)
               END-IF                            
               MOVE    SPA-GMN-H05-B-TSRYYM       (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TSRYYM       (1) 
               MOVE    SPA-GMN-H05-B-TRECEIPT-KBN (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TRECEIPT-KBN (1)
               MOVE    SPA-GMN-H05-B-TDISASTER    (SPA-NAI-H05-B-SELNUM)
                                           TO  H05-CHK-TDISASTER (1)

               MOVE    SPA-GMN-H05-PTNUM   TO  H05-CHK-TPTNUM      (2)
               MOVE    SPA-GMN-H05-NAME    TO  H05-CHK-TNAME       (2)
      **       MOVE    SPA-GMN-H05-KANANAME
      **                                   TO  SPA-GMN-H05-KANANAME(2)
               MOVE    SPA-GMN-H05-BIRTHDAY
                                           TO  H05-CHK-TBIRTHDAY   (2)
               MOVE    SPA-GMN-H05-SEX     TO  H05-CHK-TSEX        (2)
               MOVE    SPA-GMN-H05-HKNJANUM
                                           TO  H05-CHK-THKNJANUM   (2)
               MOVE    SPA-GMN-H05-KIGO    TO  H05-CHK-TKIGO       (2)
               MOVE    SPA-GMN-H05-EDABAN  TO  H05-CHK-TEDABAN     (2)
      *
               MOVE    2                   TO  H05-CHK-LIST-COUNT
            END-IF
      *
           MOVE    SPA-GMN-H05-B-HYODAI    TO  H05-CHK-LIST-HYODAI
           MOVE    SPA-GMN-H05-B-HYODAI2   TO  H05-CHK-LIST-HYODAI2
      * 
           MOVE    WIDGET-INSENSITIVE  TO  H05-B03-STATE
                                           H05-B09-STATE
                                           H05-B12-STATE
           IF      SPA-NAI-H05-B-SELNUM    >   0
               IF      SPA-NAI-H05-B-TPTID (SPA-NAI-H05-B-SELNUM)
                                               >   0
      *            紐付け済は解除のみ可能
                   MOVE    WIDGET-NORMAL       TO  H05-B03-STATE
               ELSE
                   MOVE    WIDGET-NORMAL       TO  H05-B09-STATE
      *            氏名検索から設定した場合のみ確定が可能
                   IF      SPA-NAI-H05-PTID        >   0
                       MOVE    WIDGET-NORMAL       TO  H05-B12-STATE
                   END-IF                                    
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
           AND   ( SPA-GMN-H05-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-H05-CUR
               WHEN    1
                   MOVE  "SELNUM"          TO  MCP-WIDGET
               WHEN    2
                   MOVE  "B_SELNUM"        TO  MCP-WIDGET
               WHEN    3
                   MOVE  "B09"             TO  MCP-WIDGET   
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
                   MOVE    1               TO  SPA-GMN-H05-CUR
               WHEN    "B_SELNUM"
                   MOVE    2               TO  SPA-GMN-H05-CUR
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
           INITIALIZE                      HERR
           MOVE    SPA-ERRCD           TO  HERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                               TO  HERR-ERRMSG
               WHEN    "0002"
                   MOVE    "対象のレセプトが選択してありません。"
                                               TO  HERR-ERRMSG
               WHEN    "0003"
                   MOVE
                   "請求管理と紐付けが行われているので解除できません。"
                                               TO  HERR-ERRMSG
               WHEN    "0004"
                   MOVE    "該当の患者が存在しません"
                                               TO  HERR-ERRMSG
               WHEN    "0005"
                   MOVE    "前ページはありません"
                                               TO  HERR-ERRMSG
               WHEN    "0006"
                   MOVE    "次ページはありません"
                                               TO  HERR-ERRMSG
               WHEN    "0007"
                   MOVE    "対象のレセプトを選択中です"
                                               TO  HERR-ERRMSG
               WHEN    "0008"
                   MOVE    "テスト患者です"    TO  HERR-ERRMSG
               WHEN    "0009"
                   MOVE
                       "請求管理と紐付けされているので、解除できません"
                                               TO  HERR-ERRMSG
               WHEN    "1001"
               WHEN    "1002"
               WHEN    "1003"
                   MOVE
                       "オンライン返戻明細ＤＢが更新できませんでした。"
                                                       TO  HERR-ERRMSG
           END-EVALUATE
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "H05"               TO  SPA-MOTOPG
           MOVE    "HERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "HERR"              TO  MCP-WINDOW
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
               WHEN    "1001"
                   MOVE    "更新処理をします"
                                        TO  HID1-ID1MSG
               WHEN    "0101"
                   MOVE    "処理の途中です。終了してよろしいですか。"
                                        TO  HID1-ID1MSG
           END-EVALUATE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "H05"                TO  SPA-MOTOPG
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
      *    オンライン返戻管理読み込み処理
      *****************************************************************
       900-HENREI-KANRI-READ-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE   ZERO                 TO  FLG-HENREI-K
               MOVE   MCPDATA-REC          TO  HENREI-K-REC
               IF      HENREI-K-TEISYUTUSAKI   =   "3"
                    GO  TO  900-HENREI-KANRI-READ-SEC
               END-IF
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
       900-HENREI-BODY-READ-N-SEC   SECTION.
      *
           MOVE    "tbl_henrei_body"       TO  MCP-TABLE
           MOVE    "key12"                 TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-N-EXT.
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
      ******************************************************************
      *    患者保険情報読込
      ******************************************************************
       900-PTHKNINF-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTHKNINF-REC
               MOVE    ZERO                TO  FLG-PTHKNINF
      *        削除分は対象外とする         
               IF  (   PTHKN-SAKJOKBN      =   "1" )
                   GO  TO  900-PTHKNINF-READ-N-SEC
               END-IF    
           ELSE
               INITIALIZE                      PTHKNINF-REC
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           .
       900-PTHKNINF-READ-N-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者公費情報読込
      ******************************************************************
       900-PTKOHINF-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
      *        削除分は対象外とする         
               IF  (   PTKOH-SAKJOKBN      =   "1" )
                   GO  TO  900-PTKOHINF-READ-N-SEC
               ELSE    
                   MOVE    ZERO                TO  FLG-PTKOHINF
               END-IF    
           ELSE
               INITIALIZE                      PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者読込
      ******************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *  
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                      PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
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
