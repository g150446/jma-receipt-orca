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
       PROGRAM-ID.             ORCSPOPUPSET.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 患者情報通知編集処理
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  12/12/14    NACL- 多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NMED-多々納  20/12/XX  資格確認照会対応変更
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTMEMOINF       PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-ONSKAKU         PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX-SK              PIC 9(04).
           03  IDX-SK2             PIC 9(04).
           03  IDXK                PIC 9(04).
           03  IDXK2               PIC 9(04).
           03  IDXKM               PIC 9(04).
           03  IDX-GEN              PIC 9(04).
           03  IDX-MAX              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMMDD.
                   07  WRK-SMM             PIC 9(02).
                   07  WRK-SDD             PIC 9(02).
           03  WRK-KAKUNINYMD          PIC X(08).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HKN-MSG             PIC X(30).
           03  WRK-HEDMESG             PIC X(08).
      *
           03  WRK-MEMO-HEN            PIC X(1060).
      *
           03  WRK-MEMOKBN             PIC 9(01).
           03  WRK-MEMO-G.
               05  WRK-MEMO-YMD        PIC X(08).
               05  WRK-MEMO            PIC X(1000).
               05  WRK-MEMO-1          PIC X(1000).
               05  WRK-MEMO-2          PIC X(1000).
      *R02.12
           03  WRK-SIKAKU-YUKO         PIC X(1000).
           03  WRK-SIKAKU-YUKO2        PIC X(1000).
           03  WRK-MEMO-BODY           PIC X(1060).
      *
      *    限度額適用認定証提供
           03  WRK-GENDO               PIC X(100).
           03  WRK-GENDO-HEN           PIC X(100).
           03  WRK-SIKKAN-HEN          PIC X(100).
      *
           03  WRK-GENDO-SHO           PIC X(50).
           03  WRK-GENDO-TEK           PIC X(50).
           03  WRK-GENDO-STYMD         PIC X(09).
           03  WRK-GENDO-EDYMD         PIC X(09).
           03  WRK-GENDO-SKJGNSTYMD    PIC X(09).
      *
           03  WRK-SIKKAN              PIC X(50).
           03  WRK-SIKKAN-STYMD        PIC X(09).
           03  WRK-SIKKAN-EDYMD        PIC X(09).
           03  WRK-SIKKAN-FTNMONEY     PIC X(06).
           03  WRK-SIKKAN-FTNMONEY-9   PIC 9(06).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
           03  WRK-SIKKAN-MSG-G.
               05  WRK-SIKKAN-MSG-T       OCCURS  3.
                   07  WRK-SIKKAN-MSG         PIC X(100).
                   07  WRK-SIK-MAX            PIC 9(03).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *R02.12
      *    資格確認照会パラメタ
           COPY    "CPORCSONSHI001.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    患者メモ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *TEST
      *    オンライン資格確認照会結果テーブル
       01  ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC".
      *
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *    患者情報通知パラメタ
           COPY    "CPORCSPOPUPSET.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SPA-AREA
           ORCSPOPUPSETAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ORCSPOPUPSET-BODY
           INITIALIZE                  ORCSPOPUPSET-SUMMARY
      *
      *    保険確認日
           IF     ORCSPOPUPSET-HKNCHK  =   "1"
               PERFORM 100-PTHKNINF-HEN-SEC
           END-IF
      *    メモ１
           IF     (ORCSPOPUPSET-MEMO1  =   "1"  )  OR
                  (ORCSPOPUPSET-MEMO2  =   "1"  )
               PERFORM 200-PTMEMOINF-SYORI-SEC
           END-IF
      *R02.12
      *    オンライン資格確認
           IF     (ORCSPOPUPSET-ONSHI  =   "1"  )
               PERFORM 210-ONSHIKAKU-SYORI-SEC
           END-IF
      *    見出し編集
           PERFORM 220-SUMMARY-HENSYU-SEC
      *
      *    出力内容編集
      **** IF      WRK-MEMO-HEN    NOT =   SPACE
           IF      WRK-MEMO-BODY   NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
      *        出力桁数
               MOVE    1000                TO  KANACHK-MAX-CNT
      ******   MOVE    WRK-MEMO-HEN        TO  KANACHK-MAE-INPUT
               MOVE    WRK-MEMO-BODY       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  ORCSPOPUPSET-BODY
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    保険確認日処理
      *****************************************************************
       100-PTHKNINF-HEN-SEC                SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
     ****  MOVE    SPA-PTID            TO  PTHKN-PTID
           MOVE    ORCSPOPUPSET-PTID   TO  PTHKN-PTID
           MOVE    SPA-SYSYMD          TO  PTHKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  PTHKN-TEKEDYMD
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           PERFORM UNTIL      (FLG-PTHKNINF    =   1   )
               IF     (PTHKN-SAKJOKBN      =   "1" )
                  OR  (PTHKN-HKNNUM (1:1)  =   "9" )
                   CONTINUE
               ELSE
                   IF     (WRK-KAKUNINYMD      =   SPACE         )
                      OR  (PTHKN-KAKUNINYMD    >=  WRK-KAKUNINYMD)
                       MOVE    PTHKN-KAKUNINYMD    TO  WRK-KAKUNINYMD
                       MOVE    PTHKN-HKNNUM        TO  WRK-HKNNUM
                   END-IF
               END-IF
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-KAKUNINYMD  NOT =   SPACE
               PERFORM 1001-HKNNUM-HEN-SEC
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-KAKUNINYMD      TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "保険最終確認日："
                       HKN-TANSEIDONAME
                                           DELIMITED  BY  SPACE
                       "　"
                       LNK-DAY2-EDTYMD1
                       X"0A"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-MEMO-HEN
               END-STRING
           END-IF
           .
       100-PTHKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報処理
      *****************************************************************
       1001-HKNNUM-HEN-SEC                SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-HKNNUM-READ-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1001-HKNNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ表示処理
      *****************************************************************
       200-PTMEMOINF-SYORI-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-MEMO-G
           IF     (ORCSPOPUPSET-MEMO1  =   "1"  )
               MOVE    1                   TO  WRK-MEMOKBN
               MOVE    "メモ１："          TO  WRK-HEDMESG
               PERFORM 2001-PTMEMOINF-HEN-SEC
           END-IF
      *
           IF     (ORCSPOPUPSET-MEMO2  =   "1"  )
               MOVE    2                   TO  WRK-MEMOKBN
               MOVE    "メモ２："          TO  WRK-HEDMESG
               PERFORM 2001-PTMEMOINF-HEN-SEC
           END-IF
      *
           .
       200-PTMEMOINF-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ表示処理
      *****************************************************************
       2001-PTMEMOINF-HEN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-MEMO
           MOVE    SPACE               TO  WRK-MEMO-YMD
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
    ****   MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    ORCSPOPUPSET-PTID   TO  PTMEMOINF-PTID
           MOVE    WRK-MEMOKBN         TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-SYSYMD          TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           PERFORM UNTIL    FLG-PTMEMOINF  =   1
      *
               IF      PTMEMOINF-SYSYMD    =   SPA-SYSYMD
                   STRING  WRK-HEDMESG
                           PTMEMOINF-MEMO
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-MEMO
                   END-STRING
                   MOVE    1                   TO  FLG-PTMEMOINF
               ELSE
                   MOVE    PTMEMOINF-SYSYMD    TO  WRK-MEMO-YMD
                   IF      PTMEMOINF-SYSYMD    <   SPA-SYSYMD
      *                システム日付より前で終了
                       MOVE    1               TO  FLG-PTMEMOINF
                   END-IF
               END-IF
               IF      FLG-PTMEMOINF       =   ZERO
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-PTMEMOINF-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (WRK-MEMO-YMD    NOT =   SPACE )  AND
                  (WRK-MEMO            =   SPACE )
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-MEMO-YMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               STRING  "【"
                       WRK-HEDMESG
                       LNK-DAY2-EDTYMD1
                       "に登録があります】"
                                           DELIMITED  BY  SIZE
                                           INTO    WRK-MEMO
               END-STRING
           END-IF
      *
           IF      WRK-MEMO        NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
      *        出力桁数
               MOVE    400                 TO  KANACHK-MAX-CNT
               MOVE    WRK-MEMO            TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-MEMO
               IF      KANACHK-MAX         >   ZERO
      *            最終文字が改行以外は、改行を設定する
                   IF      WRK-MEMO(KANACHK-MAX:1)
                                           NOT =   X"0A"
                       COMPUTE IDX         =   KANACHK-MAX  +  1
                       MOVE    X"0A"       TO  WRK-MEMO(IDX:1)
                   END-IF
               END-IF
      *
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
      *        出力桁数
               MOVE    1000                TO  KANACHK-MAX-CNT
               MOVE    WRK-MEMO-HEN        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-MAX         <   1000
                   COMPUTE IDX                 =   KANACHK-MAX   +  1
                   MOVE    WRK-MEMO            TO  WRK-MEMO-HEN
                                                    (IDX:)
               END-IF
           END-IF
      *
           .
       2001-PTMEMOINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認編集処理
      *****************************************************************
       210-ONSHIKAKU-SYORI-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                  ORCSONSHI001AREA
           MOVE    "01"                TO  LNK-ONSHI001-SYOKBN
           MOVE    ORCSPOPUPSET-PTID   TO  LNK-ONSHI001-PTID
      *    業務
           MOVE    ORCSPOPUPSET-GYOUMUCD
                                       TO  LNK-ONSHI001-GYOUMUCD
      *R04.1
      *    診療行為資格確認有無
           MOVE    ORCSPOPUPSET-ONSHISINRYO
                                       TO  LNK-ONSHI001-ONSHISINRYO
           CALL    "ORCSONSHI001"      USING   ORCSONSHI001AREA
                                               SPA-AREA
           MOVE    SPACE               TO  WRK-SIKAKU-YUKO
           MOVE    LNK-ONSHI001-SIKAKU-YUKO
                                       TO  ORCSPOPUPSET-SIKAKU-YUKO
           IF     (ORCSPOPUPSET-ONS-TIMEOUT    =   999)
      *        通知なし
               GO      TO      210-ONSHIKAKU-SYORI-EXT
           END-IF
      *
           IF      LNK-ONSHI001-SIKAKU-YUKO    =   "A"
      *                         A:未確認(資格レコード無し)  ----> 表示不要
      *                         D:テスト患者a           ---> 表示不要
                                               OR  "D"
      *                         空白：データ不整合
                                               OR  SPACE
      *                         9:資格確認中    ----> 表示不要
      ***                                      OR  "9"
               CONTINUE
           ELSE
      *R03.1
      *        限度額適用認定証などの編集
               MOVE    SPACE               TO  WRK-GENDO-HEN
               MOVE    SPACE               TO  WRK-SIKKAN-MSG-G
               IF      LNK-ONSHI001-OUT-TBL-UUID   NOT =   SPACE
                   PERFORM 2101-ONSHIKAKU-HEN-SEC
               END-IF
      *
      *        患者番号を記載する
               STRING "患者番号："
                      ORCSPOPUPSET-PTNUM   DELIMITED BY  SPACE
                       X"0A"
                       X"0A"
                                           DELIMITED  BY  SIZE
                      "【資格確認結果："
                      LNK-ONSHI001-SIKAKU-YUKO-NM 
                      "】"
                                           DELIMITED BY  SPACE
                       X"0A"
                       X"0A"
                                           DELIMITED  BY  SIZE
                                           INTO     WRK-SIKAKU-YUKO
               END-STRING
      *TEST
               IF     (WRK-GENDO-HEN   NOT =  SPACE )
                   MOVE    WRK-SIKAKU-YUKO     TO  WRK-SIKAKU-YUKO2
                   MOVE    SPACE               TO  WRK-SIKAKU-YUKO
                   STRING  WRK-SIKAKU-YUKO2    DELIMITED BY  SPACE
                           WRK-GENDO-HEN(1:IDX-GEN)
                                               DELIMITED BY  SIZE
                            X"0A"
                                           DELIMITED  BY  SIZE
                                           INTO     WRK-SIKAKU-YUKO
                   END-STRING
               END-IF
      *
               PERFORM VARYING    IDX-SK      FROM    1   BY  1
                       UNTIL     (IDX-SK      >   3  )
                            OR   (WRK-SIKKAN-MSG(IDX-SK)  =   SPACE)
      *            桁数取得
                   MOVE    ZERO               TO  IDXKM
                   PERFORM VARYING    IDXK        FROM    1   BY  1
                           UNTIL     (IDXK        >   1000)
                            OR   (WRK-SIKAKU-YUKO(IDXK:)   =   SPACE)
                       MOVE   IDXK                TO  IDXKM
                   END-PERFORM
                   MOVE    WRK-SIKAKU-YUKO     TO  WRK-SIKAKU-YUKO2
                   MOVE    SPACE               TO  WRK-SIKAKU-YUKO
                   MOVE    WRK-SIK-MAX (IDX-SK)    TO  IDX-MAX
                   STRING  WRK-SIKAKU-YUKO2(1:IDXKM)
                                               DELIMITED BY  SIZE
                           WRK-SIKKAN-MSG(IDX-SK)(1:IDX-MAX)
                                               DELIMITED BY   SIZE
                                           INTO     WRK-SIKAKU-YUKO
                   END-STRING
               END-PERFORM
      *
      *        改行設定
      *        桁数取得
               MOVE    ZERO               TO  IDXKM
               PERFORM VARYING    IDXK        FROM    1   BY  1
                       UNTIL     (IDXK        >   1000 )
                        OR   (WRK-SIKAKU-YUKO(IDXK:)   =   SPACE)
                   MOVE   IDXK                TO  IDXKM
               END-PERFORM
               MOVE    WRK-SIKAKU-YUKO     TO  WRK-SIKAKU-YUKO2
               MOVE    SPACE               TO  WRK-SIKAKU-YUKO
               IF     (WRK-SIKKAN-MSG-G    =  SPACE )
                   STRING  WRK-SIKAKU-YUKO2(1:IDXKM)
                                           DELIMITED BY  SIZE
                       X"0A"
                                           DELIMITED  BY  SIZE
                                           INTO     WRK-SIKAKU-YUKO
                   END-STRING
               ELSE
                   STRING  WRK-SIKAKU-YUKO2(1:IDXKM)
                                           DELIMITED BY  SIZE
                       X"0A"
                       X"0A"
                                           DELIMITED  BY  SIZE
                                           INTO     WRK-SIKAKU-YUKO
                   END-STRING
               END-IF
      *
               IF      LNK-ONSHI001-SIKAKU-YUKO
                                           =   "Z"
      *R05.01   エラー内容が存在する時のみ
                IF      (ONSKAKU-ERR-CODE  =   SPACE )
                  AND   (ONSKAKU-ERR-MSG   =   SPACE )
                   CONTINUE
                ELSE
                   MOVE    ZERO               TO  IDXKM
                   PERFORM VARYING    IDXK        FROM    1   BY  1
                           UNTIL     (IDXK        >   1000 )
                           OR   (WRK-SIKAKU-YUKO(IDXK:)   =   SPACE)
                       MOVE   IDXK                TO  IDXKM
                   END-PERFORM
                   COMPUTE IDXK               =   IDXKM  -   2
                   IF      WRK-SIKAKU-YUKO(IDXK:2)
                                              =   X"0A0A"
                       COMPUTE IDXKM              =   IDXKM  -   1
                   END-IF
                   MOVE    WRK-SIKAKU-YUKO TO  WRK-SIKAKU-YUKO2
                   MOVE    SPACE           TO  WRK-SIKAKU-YUKO
      *
                   STRING  WRK-SIKAKU-YUKO2(1:IDXKM)
                                           DELIMITED BY  SIZE
                       "【"                DELIMITED BY  SIZE
                       ONSKAKU-ERR-CODE    DELIMITED BY  SPACE
                       "】"                DELIMITED BY  SIZE
                       X"0A"               DELIMITED BY  SIZE
                       ONSKAKU-ERR-MSG     DELIMITED BY  SPACE
                                           INTO     WRK-SIKAKU-YUKO
                   END-STRING
      *
                 END-IF
               END-IF
      *
           END-IF
           .
       210-ONSHIKAKU-SYORI-EXT.
           EXIT.
      *TEST
      *TEST
      *****************************************************************
      *    オンライン資格確認編集処理
      *****************************************************************
       2101-ONSHIKAKU-HEN-SEC        SECTION.
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-OUT-TBL-UUID
                                       TO  ONSKAKU-TBL-UUID
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-GENDO
           MOVE    SPACE               TO  WRK-GENDO-HEN
           MOVE    SPACE               TO  WRK-SIKKAN
           MOVE    SPACE               TO  WRK-SIKKAN-HEN
      *R05.1  保険証OCR以外
           IF      (ONSKAKU-SHOKAI-KBN         =   "2" )
             AND   (ONSKAKU-ORCA-SHOKAI-KBN    =   "2" )
               CONTINUE
           ELSE
      *R04.1
      *    保険証確認の時は、患者登録で同意があること
           IF      (ONSKAKU-SHOKAI-KBN     =   "2")
              AND  (ORCSPOPUPSET-ONS-DOUIFLG   NOT =   "1")
               GO  TO  2101-ONSHIKAKU-HEN-EXT
           END-IF
      *
           END-IF
      *
      *    限度額適用認定証
           IF     (ONSKAKU-GENDO-DOUIFLG   =   "1" )
               IF     (ONSKAKU-GENDO-SHOKBN    =   SPACE
                                               OR  ZERO  )
                   CONTINUE
               ELSE
                   PERFORM 21011-GENDO-HEN-SEC
               END-IF
           END-IF
      *
      *    特定疾病療養受療証提供
           IF     (ONSKAKU-SIKKAN-DOUIFLG  =   "1" )
               PERFORM 21012-SIKKAN-HEN-SEC
           END-IF
      *
           .
       2101-ONSHIKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    限度額適用認定証編集処理
      *****************************************************************
       21011-GENDO-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-GENDO-SHO
           MOVE    SPACE               TO  WRK-GENDO-TEK
           EVALUATE    ONSKAKU-GENDO-SHOKBN
               WHEN    "01"
                   MOVE    "限度額適用区分認定証"
                                           TO  WRK-GENDO-SHO
               WHEN    "02"
                   MOVE    "限度額適用・減額認定証"
                                           TO  WRK-GENDO-SHO
               WHEN    "03"
                   MOVE    "標準負担額減額認定証"
                                           TO  WRK-GENDO-SHO
           END-EVALUATE
      *    限度額適用認定証適用区分
           EVALUATE    ONSKAKU-GENDO-TEKKBN
               WHEN    "A01"
                   MOVE    "ア"            TO  WRK-GENDO-TEK
               WHEN    "A02"
                   MOVE    "イ"            TO  WRK-GENDO-TEK
               WHEN    "A03"
                   MOVE    "ウ"            TO  WRK-GENDO-TEK
               WHEN    "A04"
                   MOVE    "エ"            TO  WRK-GENDO-TEK
               WHEN    "A05"
                   MOVE    "オ"            TO  WRK-GENDO-TEK
               WHEN    "A06"
                   MOVE    "オ（境）"      TO  WRK-GENDO-TEK
               WHEN    "B01"
                   MOVE    "現役並み３"    TO  WRK-GENDO-TEK
               WHEN    "B02"
                   MOVE    "現役並み２"    TO  WRK-GENDO-TEK
               WHEN    "B03"
                   MOVE    "現役並み１"    TO  WRK-GENDO-TEK
               WHEN    "B04"
                   MOVE    "一般"          TO  WRK-GENDO-TEK
               WHEN    "B05"
                   MOVE    "低所得２"      TO  WRK-GENDO-TEK
               WHEN    "B06"
                   MOVE    "低所得１"      TO  WRK-GENDO-TEK
               WHEN    "B07"
                   MOVE    "低所得１（老福）"  TO  WRK-GENDO-TEK
               WHEN    "B08"
                   MOVE    "低所得１（境）"    TO  WRK-GENDO-TEK
      *2022/9
               WHEN    "B09"
                   MOVE    "一般２"        TO  WRK-GENDO-TEK
               WHEN    "B10"
                   MOVE    "一般１"        TO  WRK-GENDO-TEK
           END-EVALUATE
           MOVE    ONSKAKU-GENDO-TEKSTYMD  TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-STYMD
           MOVE    ONSKAKU-GENDO-TEKEDYMD  TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-EDYMD
      *    限度額適用認定証長期入院該当年月日
           MOVE    ONSKAKU-GENDO-SKJGNSTYMD    TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-GENDO-SKJGNSTYMD
      *
           MOVE    SPACE               TO  WRK-GENDO-HEN
           STRING  ONSKAKU-GENDO-SHOKBN    DELIMITED  BY  SPACE
                   "："                    DELIMITED  BY  SPACE
                   WRK-GENDO-SHO           DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SPACE
                   ONSKAKU-GENDO-TEKKBN    DELIMITED  BY  SPACE
                   "："                    DELIMITED  BY  SPACE
                   WRK-GENDO-TEK           DELIMITED  BY  SPACE
                   "　"
                   WRK-GENDO-STYMD         DELIMITED  BY  SIZE
                   "〜"
                   WRK-GENDO-EDYMD         DELIMITED  BY  SIZE
                   "　長期入院該当年月日："
                   WRK-GENDO-SKJGNSTYMD    DELIMITED  BY  SIZE
                       X"0A"
                                           DELIMITED  BY  SIZE
                                           INTO  WRK-GENDO-HEN
           END-STRING
      *
      *    桁数取得
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    100                 TO  KANACHK-MAX-CNT
           MOVE    WRK-GENDO-HEN       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  IDX-GEN
      *
           .
       21011-GENDO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾病療養受療証提供編集処理
      *****************************************************************
       21012-SIKKAN-HEN-SEC              SECTION.
      *
           MOVE    SPACE                  TO  WRK-SIKKAN-MSG-G
           MOVE    ZERO                   TO  IDX-SK2
           PERFORM VARYING    IDX-SK      FROM    1   BY  1
                   UNTIL      IDX-SK      >   3
              IF      ONSKAKU-SIKKAN-SIPKBN(IDX-SK)   NOT =   SPACE
                   PERFORM 210121-SIKKAN-HENSYU-SEC
              END-IF
           END-PERFORM
           .
       21012-SIKKAN-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    特定疾病療養受療証提供編集処理
      *****************************************************************
       210121-SIKKAN-HENSYU-SEC          SECTION.
      *
           ADD     1               TO  IDX-SK2
           MOVE    SPACE           TO  WRK-SIKKAN
           MOVE    SPACE           TO  WRK-SIKKAN-FTNMONEY
           EVALUATE    ONSKAKU-SIKKAN-SIPKBN(IDX-SK)
               WHEN    "1"
                   MOVE    "人工透析　　　　"     TO  WRK-SIKKAN
               WHEN    "2"
                   MOVE    "血液凝固因子障害"     TO  WRK-SIKKAN
               WHEN    "3"
                   MOVE    "ＨＩＶ感染症　　"     TO  WRK-SIKKAN
           END-EVALUATE
      *
           MOVE    ONSKAKU-SIKKAN-TEKSTYMD (IDX-SK)
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-STYMD
           MOVE    ONSKAKU-SIKKAN-TEKEDYMD (IDX-SK)
                                       TO  WRK-SYMD
           PERFORM 8001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  WRK-SIKKAN-EDYMD
      *
           IF      ONSKAKU-SIKKAN-FTNMONEY(IDX-SK) NOT =   SPACE
               INITIALIZE                      ORCSNUMAREA
               MOVE    ONSKAKU-SIKKAN-FTNMONEY(IDX-SK) TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    ONSKAKU-SIKKAN-FTNMONEY (IDX-SK)
                                           TO   WRK-SIKKAN-FTNMONEY
               ELSE
                   MOVE    SNUM-OUTNUM     TO  WRK-ZZ9
                   MOVE    WRK-ZZ9         TO  WRK-SIKKAN-FTNMONEY
               END-IF
           END-IF
      *
           STRING  ONSKAKU-SIKKAN-SIPKBN(IDX-SK)
                                           DELIMITED  BY  SPACE
                   "："                    DELIMITED  BY  SPACE
                   WRK-SIKKAN              DELIMITED  BY  SPACE
                   "　"                    DELIMITED  BY  SPACE
                   "　"
                   WRK-SIKKAN-STYMD         DELIMITED  BY  SIZE
                   "〜"
                   WRK-SIKKAN-EDYMD         DELIMITED  BY  SIZE
                   "　自己負担限度額："
                   WRK-SIKKAN-FTNMONEY      DELIMITED  BY  SIZE
                       X"0A"
                                       DELIMITED  BY  SIZE
                                            INTO  WRK-SIKKAN-MSG
                                                        (IDX-SK2)
           END-STRING
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    100                 TO  KANACHK-MAX-CNT
           MOVE    WRK-SIKKAN-MSG(IDX-SK2)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-SIK-MAX (IDX-SK2)
           .
       210121-SIKKAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容結合・見出し編集処理
      *****************************************************************
       220-SUMMARY-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEMO-BODY
           IF     (WRK-SIKAKU-YUKO     =   SPACE  )
             AND  (WRK-MEMO-HEN        =   SPACE  )
               GO  TO  220-SUMMARY-HENSYU-EXT
           END-IF
      *
           IF      WRK-SIKAKU-YUKO     =   SPACE
      *        保険情報のみ
               MOVE    WRK-MEMO-HEN        TO  WRK-MEMO-BODY
               MOVE    "患者情報"          TO  ORCSPOPUPSET-SUMMARY
           ELSE
               IF      WRK-MEMO-HEN        =   SPACE
      *            オンライン資格確認のみ
                   MOVE    WRK-SIKAKU-YUKO     TO  WRK-MEMO-BODY
                   MOVE    "資格確認情報"      TO  ORCSPOPUPSET-SUMMARY
               ELSE
      *            オンライン資格確認の桁数
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    1000                TO  KANACHK-MAX-CNT
                   MOVE    WRK-SIKAKU-YUKO     TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF      KANACHK-MAX         <   1000
                       COMPUTE IDX             =   KANACHK-MAX   +  1
                   ELSE
                       MOVE    ZERO            TO  IDX
                   END-IF
      *            オンライン資格確認の下に保険情報などを記載
                   MOVE    WRK-SIKAKU-YUKO     TO  WRK-MEMO-BODY
                   IF      IDX                 >   ZERO
                       MOVE    WRK-MEMO-HEN        TO  WRK-MEMO-BODY
                                                        (IDX:)
                   END-IF
                   MOVE    "資格確認情報・患者情報"
                                               TO  ORCSPOPUPSET-SUMMARY
               END-IF
      *        オンライン資格確認がある時のみ、表示時間の設定
               MOVE    ORCSPOPUPSET-ONS-TIMEOUT
                                       TO  ORCSPOPUPSET-TIMEOUT
           END-IF
           .
       220-SUMMARY-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       8001-SEIWA-YMDEDIT-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
                                       OR  ZERO
                                       OR  ALL "9"
               MOVE    WRK-SYMD            TO  WRK-HENYMD
           ELSE
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               IF      STS-DAY-RC1             =   ZERO
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
               ELSE
                   MOVE    SPACE               TO  WRK-HENYMD
               END-IF
           END-IF
           .
       8001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ読込処理
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
           EXIT.
      ****************************************************************
      *    保険番号マスタ次検索処理
      ****************************************************************
       920-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       920-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    オンライン資格確認読込
      *****************************************************************
       900-ONSKAKU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ONSKAKU-REC
               MOVE    ZERO            TO  FLG-ONSKAKU
           ELSE
               INITIALIZE                  ONSKAKU-REC
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           .
       900-ONSKAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込み処理
      *****************************************************************
       920-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
