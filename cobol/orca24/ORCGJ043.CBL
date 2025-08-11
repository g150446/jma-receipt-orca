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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGJ043.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : コメント内容入力画面（Ｋ０２５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  20/06/XX    NMED−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
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
      *    診療行為用共通パラメタ
           COPY    "J01COMMON-SPA".
      *
      *    画面用共通スパ領域
           COPY    "J04SCR-SPA".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-TENSU           PIC 9(01). 
           03  FLG-RECEKISAI       PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-OVER            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDS                 PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
           03  IDX-CHK             PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WFIL1       PIC X(01).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WFIL2       PIC X(01).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-JIDMSG          PIC X(80).
      *
           03  WRK-IC-INPUTCD      PIC X(22).
      *
           03  WRK-IDX             PIC 9(04).
           03  WRK-LEN             PIC 9(04).
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-MEISYO-IN       PIC X(100).
      *    名称項目桁
           03  WRK-MEISYO-GMN      PIC X(138).
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
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    レセプト記載事項
       01  RECEKISAI-REC.
           COPY    "CPRECEKISAI.INC".
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
      *01  SPATMP-REC.
      *    COPY    "CPSPA-TMP.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA24SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-J04
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
      *
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-J04SPA-SET-SEC
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J04         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
      **** INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "J043"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＳＰＡパラメタデータ読込処理
      *****************************************************************
       1002-J04SPA-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           IF     SPA-HOSPNUM  NOT NUMERIC
               GO   TO  1002-J04SPA-SET-EXT
           END-IF
      *
           INITIALIZE                      PARA-REC
           MOVE    "J04"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                 OR   (IDX                 >=  SPA-MAX-LINE)
               ADD    1                    TO  IDX
               MOVE   PARA-DATA-REC        TO  SPA-GMN-TBL (IDX)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    行位置とカーソル位置の決定
      ***  MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
      **       MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
      **       MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
      *        IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
      *            ADD     1                 TO  IDX
      *            IF      IDX               >   MAX-GMN
      *                ADD     1             TO  WRK-PAGE
      *                MOVE    1             TO  IDX
      *            END-IF
      *
      *            MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
      *            MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      **       END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           .
       1002-J04SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡパラメタデータＷＲＩＴＥ処理
      *****************************************************************
       1003-J04SPA-OUT-SEC             SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "J04"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      PARA-REC
           MOVE    "J04"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )
               IF      (SPA-COM-INPUTCD(IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(IDX)   =   SPACE )
               AND     (IDX            >   SPA-GMN-MAX  )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-TBL (IDX)   TO  PARA-DATA-REC
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "J043 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       1003-J04SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "JID1"
      *            確認画面より
                   PERFORM 30010-JID1-SET-SEC
                   MOVE    SPACE               TO  SPA-JIDCD
      *
               WHEN    OTHER
                   PERFORM                     310-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＪＩＤ１）ＯＫ処理
      *****************************************************************
       30010-JID1-SET-SEC          SECTION.
      *
           .
       30010-JID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           INITIALIZE                  SPA-J043-GMNAREA
      *
           IF      SPA-J043-GMNIDX     =   ZERO
      *        最初の対象
               MOVE    ZERO                TO  SPA-J043-IDX
               MOVE    ZERO                TO  SPA-J043-SYORIKBN
               PERFORM 470-NEXT-SYORI-SEC
           ELSE
               MOVE    SPA-J043-GMNIDX     TO  SPA-J043-IDX
               PERFORM 3101-SRYCD-HENSYU-SEC
               MOVE    1                   TO  SPA-J043-CUR
           END-IF
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    管理画面編集処理
      *****************************************************************
       3101-SRYCD-HENSYU-SEC           SECTION.
      *
           MOVE    SPA-COM-INPUTCD (SPA-J043-IDX)
                                       TO   SPA-J043-SRYCD
           MOVE    SPA-COM-INPUTCD (SPA-J043-IDX)
                                       TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    正式名称あり
           IF      TNS-FORMALYUKOKETA  >   ZERO
               MOVE    TNS-FORMALNAME      TO  SPA-J043-FORMALNAME
           ELSE
               MOVE    TNS-NAME            TO  SPA-J043-FORMALNAME
           END-IF
           MOVE    TNS-NAME            TO  SPA-J043-NAME
           MOVE    TNS-YUKOKETA        TO  SPA-J043-YUKOKETA
           MOVE    TNS-FORMALYUKOKETA  TO  SPA-J043-FORMALYUKOKETA
      *
           MOVE    SPACE               TO  SPA-J043-INPUTSTRING
           MOVE    SPACE               TO  SPA-J043-KUBUN
      *    入力コメント内容
           MOVE    ZERO                TO  SPA-J043-INPUTNASI
           IF     (TNS-SRYCD(1:3)          =   "830")
               MOVE    SPA-GMN-INPUTSTRING (SPA-J043-IDX)
                                       TO  SPA-J043-INPUTSTRING
      *        正式名なし
               IF     (SPA-GMN-INPUTSTRING (SPA-J043-IDX)  =   SPACE )
      *            名称から入力値を抜き出す
                   COMPUTE IDS             =   (TNS-YUKOKETA  *  2)
                                           +   1
                   MOVE    SPA-GMN-MEISYO  (SPA-J043-IDX)
                                                   (IDS:)
                                           TO  SPA-J043-INPUTSTRING
               END-IF
               IF      SPA-J043-INPUTSTRING    =   SPACE
                   MOVE    1                   TO  SPA-J043-INPUTNASI
               END-IF
           END-IF
      *
      *    選択式コメント
           IF      TNS-MEIUSESKB       =   1
               PERFORM 31011-RECEKISAI-HEN-SEC
           END-IF
      *    表示行
           MOVE    SPA-COM-GYOU(SPA-J043-IDX)  TO  WRK-ZZ9
      *    表示　入力ＣＤ
           MOVE    SPACE                   TO  WRK-IC-INPUTCD
           IF      SPA-COM-ICD-INPUTCD(SPA-J043-IDX)   NOT =   SPACE
               STRING  "（"
                       SPA-COM-ICD-INPUTCD (SPA-J043-IDX)
                       "）"
                                              DELIMITED BY  SPACE
                                              INTO  WRK-IC-INPUTCD
               END-STRING
           END-IF
           MOVE    SPACE               TO  SPA-J043-GMNLINE
      *J043 行なし
      *    STRING  WRK-ZZ9
      *            "行目"              DELIMITED BY  SIZE
      *            WRK-IC-INPUTCD      DELIMITED BY  SPACE
      *                            INTO   SPA-J043-GMNLINE
      **** END-STRING
           MOVE    WRK-IC-INPUTCD      TO  SPA-J043-GMNLINE
      *
      *    名称表示
           MOVE    SPACE               TO  SPA-J043-FORMALNAME
           IF      SPA-J043-INPUTSTRING    =   SPACE
      *        正式名称なし
               IF      TNS-FORMALYUKOKETA  =   ZERO
                   MOVE    SPA-GMN-MEISYO  (SPA-J043-IDX)
                                           TO  SPA-J043-FORMALNAME
               ELSE
                   COMPUTE IDS         =   (TNS-YUKOKETA  *  2)
                                           +   1
                   MOVE    SPA-GMN-MEISYO  (SPA-J043-IDX)
                                               (IDS:)
                                           TO  WRK-MEISYO-IN
                   STRING  TNS-FORMALNAME
                           WRK-MEISYO-IN
                                              DELIMITED BY  SPACE
                                          INTO  SPA-J043-FORMALNAME
                   END-STRING
               END-IF
           ELSE
      *        コメント入力あり
               IF      TNS-FORMALYUKOKETA  =   ZERO
                    STRING  TNS-NAME
                           SPA-J043-INPUTSTRING 
                                              DELIMITED BY  SPACE
                                          INTO  SPA-J043-FORMALNAME
                   END-STRING
               ELSE
                   STRING  TNS-FORMALNAME
                               SPA-J043-INPUTSTRING 
                                              DELIMITED BY  SPACE
                                          INTO  SPA-J043-FORMALNAME
                   END-STRING
               END-IF
           END-IF
           .
       3101-SRYCD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    レセプト記載事項編集処理
      *****************************************************************
       31011-RECEKISAI-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  RECEKISAI-REC
           INITIALIZE                      RECEKISAI-REC
           MOVE    SPA-J043-SRYCD      TO  RKISAI-C-CD
           MOVE    SPA-SRYYMD(1:6)     TO  RKISAI-STYM
           MOVE    SPA-SRYYMD(1:6)     TO  RKISAI-EDYM
           MOVE    RECEKISAI-REC       TO  MCPDATA-REC
           MOVE    "tbl_recekisai"     TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekisai"     TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 930-RECEKISAI-READ-SEC
           ELSE
               INITIALIZE                  RECEKISAI-REC
               MOVE    1                   TO  FLG-RECEKISAI
           END-IF
           MOVE    "tbl_recekisai"     TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-RECEKISAI       =   ZERO
               MOVE    RKISAI-KUBUN        TO  SPA-J043-KUBUN
           END-IF
           .
       31011-RECEKISAI-HEN-EXT.
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
      *        更新
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 412-KOUSIN-SYORI-SEC
      *        前
               WHEN    "CLICKED"       ALSO    "B06"
                    PERFORM 460-BEFORE-SYORI-SEC
      *        次
               WHEN    "CLICKED"       ALSO    "B07"
                    PERFORM 470-NEXT-SYORI-SEC
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
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE   J043-INPUTSTRING     TO  SPA-J043-INPUTSTRING
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    入力コメント内容チェック処理
           PERFORM 41021-INPUTSTRING-CHK-SEC
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コメント内容チェック処理
      *****************************************************************
       41021-INPUTSTRING-CHK-SEC       SECTION.
      *
      *    入力不可
           IF     (SPA-J043-SRYCD(1:3) NOT =   "830" )
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-J043-INPUTSTRING
               MOVE    4                   TO  SPA-J043-CUR
               GO      TO      41021-INPUTSTRING-CHK-EXT
           END-IF
      *
      *    全角変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    100                 TO  KANACHK-MAX-CNT
           MOVE    SPA-J043-INPUTSTRING
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    KANACHK-OUT-INPUT   TO  SPA-J043-INPUTSTRING
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-J043-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-J043-INPUTSTRING
               MOVE    2                   TO  SPA-J043-CUR
           END-IF
      *
      *    名称＋入力値が６９文字以下であること(処方箋印刷対応）
           IF      (SPA-COM-SRYKBN (SPA-J043-IDX)  =   "21"
                                                   OR  "22"
                                                   OR  "23"
                                                   OR  "98")
      *        画面名称欄編集判定
               PERFORM 41021-GMNNAME-CHK-SEC
               IF      FLG-OVER            =   1
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-J043-CUR
               END-IF
           END-IF
           .
       41021-INPUTSTRING-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面名称欄編集判定処理
      *****************************************************************
       41021-GMNNAME-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-OVER
           MOVE    SPACE           TO  WRK-MEISYO-GMN
           STRING  SPA-J043-NAME
                   SPA-J043-INPUTSTRING
                                       DELIMITED  BY  SPACE
                                       INTO    WRK-MEISYO-GMN
                           ON  OVERFLOW
                   MOVE    1           TO  FLG-OVER
           END-STRING
           .
       41021-GMNNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    更新　処理
      *****************************************************************
       412-KOUSIN-SYORI-SEC           SECTION.
      *
           IF      SPA-J043-SYORIKBN   =   1
                                       OR  ZERO
               CONTINUE
           ELSE
               MOVE    SPACE           TO  SPA-J043-INPUTSTRING
               GO  TO  412-KOUSIN-SYORI-EXT
           END-IF
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  412-KOUSIN-SYORI-EXT
           END-IF
      *
           IF      SPA-J043-IDX        >   ZERO
               IF      SPA-COM-INPUTCD (SPA-J043-IDX)(1:3)
                                               =   "830"
      *            画面名称欄編集判定
                   PERFORM 41021-GMNNAME-CHK-SEC
                   MOVE    WRK-MEISYO-GMN      TO  SPA-GMN-MEISYO
                                                   (SPA-J043-IDX)
                   MOVE    SPACE               TO  SPA-GMN-INPUTSTRING
                                                   (SPA-J043-IDX)
      *            入力文字が画面表示（138バイト以上のみ）
                   IF    (FLG-OVER        =   1  )
                       MOVE    SPA-J043-INPUTSTRING
                                       TO  SPA-GMN-INPUTSTRING
                                               (SPA-J043-IDX)
                   END-IF
      *            画面ＳＰＡ書込み
                   PERFORM 1003-J04SPA-OUT-SEC
               END-IF
           END-IF
      *
           IF      SPA-J043-SYORIKBN   =   1
      *        戻る
               PERFORM 210-BACK
               GO  TO  412-KOUSIN-SYORI-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-J043-GMNAREA-2
           INITIALIZE                  SPA-J043-GMNAREA-2
           PERFORM 3101-SRYCD-HENSYU-SEC
           MOVE    4                   TO  SPA-J043-CUR
           .
       412-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次　処理
      *****************************************************************
       470-NEXT-SYORI-SEC             SECTION.
      *
      *    次行の対象を表示
           IF      SPA-J043-IDX        =   ZERO
               MOVE    1               TO  IDX-STR
           ELSE
               COMPUTE IDX-STR         =   SPA-J043-IDX
                                       +   1
           END-IF
           MOVE    ZERO               TO  SPA-J043-SYORIKBN
      *
           PERFORM 4701-NEXT-HEN-SEC
           IF     (IDX-CHK             =   ZERO )
              AND (IDX-STR             >   1    )
      *        先頭以外からの検索で対象がない時、先頭から
               MOVE    1               TO  IDX-STR
               PERFORM 4701-NEXT-HEN-SEC
           END-IF
      *
           IF      IDX-CHK             =   ZERO
               INITIALIZE                  SPA-J043-GMNAREA-2
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-J043-CUR
           ELSE
               MOVE    IDX-CHK         TO  SPA-J043-IDX
               PERFORM 3101-SRYCD-HENSYU-SEC
               MOVE    1                   TO  SPA-J043-CUR
           END-IF
           .
       470-NEXT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    表示対象コメントコード検索処理
      *****************************************************************
       4701-NEXT-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-CHK
           PERFORM VARYING     IDX     FROM   IDX-STR   BY  1
                   UNTIL      (IDX         >  SPA-GMN-MAX )
                          OR  (IDX-CHK     >  ZERO    )
               IF     (SPA-COM-SET (IDX)           =   SPACE )
      *            対象判定
                   PERFORM 47011-TBLCHEK-SEC
               END-IF
           END-PERFORM
           .
       4701-NEXT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    対象判定処理
      *****************************************************************
       47011-TBLCHEK-SEC             SECTION.
      *
      *    コメント入力対象
           IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "830")
               MOVE    IDX                 TO  IDX-CHK
               GO  TO        47011-TBLCHEK-EXT
           END-IF
      *
      *
           EVALUATE    SPA-J043-SYORIKBN
               WHEN    ZERO
               WHEN    1
      *            コメント入力のまだのもの
                   IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "830")
      *            名称から入力値を抜き出す
                       COMPUTE IDS         =   (SPA-COM-YUKOKETA(IDX)
                                           *  2 )
                                           +   1
                       IF      SPA-GMN-MEISYO (IDX)(IDS:)
                                               =   SPACE 
                           MOVE    IDX             TO  IDX-CHK
                       END-IF
                   END-IF
               WHEN    2
      *            コメント入力対象
                   IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "830")
                       MOVE    IDX             TO  IDX-CHK
                   END-IF
               WHEN    3
      *            正式名称のあるすべてのコメントすべて
                   IF    ((SPA-COM-FORMAL(IDX)    =   "1"  )
                      AND (SPA-COM-INPUTCD(IDX)(1:1)  =   "8"))
      *
                   OR    ( SPA-COM-INPUTCD(IDX)(1:2)  =   "83"
                                                      OR  "84"
                                                      OR  "85")
                       MOVE    IDX             TO  IDX-CHK
                   END-IF
           END-EVALUATE
           .
       47011-TBLCHEK-EXT.
           EXIT.
      *****************************************************************
      *    前　処理
      *****************************************************************
       460-BEFORE-SYORI-SEC             SECTION.
      *
      *    前行の対象を表示
           IF      SPA-J043-IDX        <=  1
               MOVE    SPA-GMN-MAX     TO  IDX-STR
           ELSE
               COMPUTE IDX-STR         =   SPA-J043-IDX
                                       -   1
           END-IF
      *
           MOVE    ZERO               TO  SPA-J043-SYORIKBN
      *
           PERFORM 4601-BEFORE-HEN-SEC
           IF     (IDX-CHK             =   ZERO )
              AND (IDX-STR             <   SPA-GMN-MAX )
      *        先頭以外からの検索で対象がない時、最後から
               MOVE    SPA-GMN-MAX     TO  IDX-STR
               PERFORM 4601-BEFORE-HEN-SEC
           END-IF
      *
           IF      IDX-CHK             =   ZERO
               INITIALIZE                  SPA-J043-GMNAREA-2
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-J043-CUR
           ELSE
               MOVE    IDX-CHK         TO  SPA-J043-IDX
               PERFORM 3101-SRYCD-HENSYU-SEC
               MOVE    1                   TO  SPA-J043-CUR
           END-IF
      *
           .
       460-BEFORE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    前表示対象コメントコード検索処理
      *****************************************************************
       4601-BEFORE-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-CHK
           PERFORM VARYING     IDX     FROM   IDX-STR   BY  -1
                   UNTIL      (IDX         <  1       )
                          OR  (IDX-CHK     >  ZERO    )
               IF     (SPA-COM-SET (IDX)           =   SPACE )
      *            対象判定
                   PERFORM 47011-TBLCHEK-SEC
               END-IF
           END-PERFORM
           .
       4601-BEFORE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "J04"               TO  SPA-SAKIPG
           MOVE    "J043"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-J01KYOUTU       TO  SPA-WORK
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
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
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-JIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "J043"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *
           MOVE    SPACE               TO  J043
           INITIALIZE                      J043
      *
           MOVE    SPA-J043-SRYCD      TO  J043-SRYCD
           MOVE    SPA-J043-KUBUN      TO  J043-MIDASHI1
           MOVE    SPA-J043-GMNLINE    TO  J043-MIDASHI2
      *
           MOVE    SPA-J043-FORMALNAME     TO  J043-FORMALNAME
           MOVE    SPA-J043-INPUTSTRING    TO  J043-INPUTSTRING
      *
           IF      SPA-J043-INPUTNASI  =   1
               MOVE    "（コメント未入力）"    TO  J043-MIDASHI4
               MOVE    "red"               TO  J043-MIDASHI4-STYLE
           END-IF
      *
           MOVE    "名称"              TO  J043-NAMELABEL
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-J043-CUR
               WHEN    01
                   MOVE    "INPUTSTRING"     TO  MCP-WIDGET
               WHEN    02
                   MOVE    "B12"             TO  MCP-WIDGET
               WHEN    04
                   MOVE    "B07"             TO  MCP-WIDGET
               WHEN    09
                   MOVE    "B01"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "コメント入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "コメント入力できるコードではありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "コメント内容にエラーがあります。"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   STRING  "処方箋記載できませんので名称＋入力値を"
                           "６９文字までとして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "コメント入力できるコメントがありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRCD           TO  JERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J043"               TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-JIDMSG
      *    EVALUATE    SPA-JIDCD
      *        WHEN    "0101"
      *            MOVE
      *            "対象のデータを解除します。よろしいですか？"
      *                                TO  WRK-JIDMSG
      *        WHEN    OTHER
      *            MOVE    SPA-JIDCD
      *                                TO  WRK-JIDMSG
      *    END-EVALUATE
      *
           MOVE    SPACE               TO  JID1
           INITIALIZE                      JID1
           MOVE    SPA-JIDCD           TO  JID1-ID1CODE
           MOVE    WRK-JIDMSG          TO  JID1-ID1MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "J043"              TO  SPA-MOTOPG
           MOVE    "JID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JID1"              TO  MCP-WINDOW.
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
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-SEC-EXT.
           EXIT.
      *****************************************************************
      *    レセプト記載事項マスタ読み込み
      *****************************************************************
       930-RECEKISAI-READ-SEC         SECTION.
      *
           MOVE    "tbl_recekisai"     TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  RECEKISAI-REC
               MOVE    ZERO                TO  FLG-RECEKISAI
           ELSE
               INITIALIZE                      RECEKISAI-REC
               MOVE    1                   TO  FLG-RECEKISAI
           END-IF
      *
           .
       930-RECEKISAI-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
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
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
