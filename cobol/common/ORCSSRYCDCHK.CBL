      *****************************************************************
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
      *****************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSSRYCDCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 診療行為コードチェックサブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  23/07/27    ORCAMO        新規作成
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
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTKOHINF          PIC 9(01).
           03  FLG-PTINF             PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-SRYACCT           PIC 9(01).
           03  FLG-SRYACT            PIC 9(01).
           03  FLG-PTHKNINF          PIC 9(01).
           03  FLG-GAITO-HKNJYA      PIC 9(01).
           03  FLG-KOH959-ARI        PIC 9(01).
           03  FLG-YOYAKUCD-ARI      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                   PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-CHECK-TABLE.
             05  WRK-CHECK-OCC       OCCURS   10.
               07  WRK-CHECK-FLG     PIC 9(01).
               07  WRK-CHECK-YMD.
                 09  WRK-CHECK-YM    PIC 9(06).
                 09  WRK-CHECK-DD    PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSRYCDCHK.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           LNK-ORCSSRYCDCHK-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-ORCSSRYCDCHK-RC
           INITIALIZE                      LNK-ORCSSRYCDCHK-OUT-AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメタチェック
           PERFORM   100-PRM-CHK-SEC
           IF    LNK-ORCSSRYCDCHK-RC   NOT =   ZERO
               GO   TO   000-PROC-EXT
           END-IF
      *
           IF    LNK-ORCSSRYCDCHK-RC       =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    パラメタチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    入外区分
           IF    LNK-ORCSSRYCDCHK-NYUGAIKBN  =  "1" OR "2"
             CONTINUE
           ELSE
             MOVE      1    TO   LNK-ORCSSRYCDCHK-RC
           END-IF
      *
      *    患者ＩＤチェック
           IF    LNK-ORCSSRYCDCHK-RC  =  ZERO
             IF    LNK-ORCSSRYCDCHK-PTID  =  ZERO
               MOVE    2    TO   LNK-ORCSSRYCDCHK-RC
             ELSE
               INITIALIZE                            PTINF-REC
               MOVE  LNK-ORCSSRYCDCHK-HOSPNUM    TO  PTINF-HOSPNUM
               MOVE  LNK-ORCSSRYCDCHK-PTID       TO  PTINF-PTID
               MOVE  PTINF-REC                   TO  MCPDATA-REC
               MOVE  "DBSELECT"                  TO  MCP-FUNC
               MOVE  "tbl_ptinf"                 TO  MCP-TABLE
               MOVE  "key"                       TO  MCP-PATHNAME
               CALL  "ORCDBMAIN"                 USING  MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
               IF    MCP-RC  =  ZERO
                  MOVE    "tbl_ptinf"     TO  MCP-TABLE
                  MOVE    "key"           TO  MCP-PATHNAME
                  PERFORM  910-PTINF-READ-SEC
                  IF   FLG-PTINF  =  ZERO
                    CONTINUE
                  ELSE
                    MOVE    2             TO  LNK-ORCSSRYCDCHK-RC
                  END-IF
               ELSE
                    MOVE    2             TO  LNK-ORCSSRYCDCHK-RC
               END-IF
               MOVE    "tbl_ptinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM  910-DB-CLOSE-SEC
             END-IF
           END-IF
      *
      *    算定年月チェック
           IF    LNK-ORCSSRYCDCHK-RC  =  ZERO
             IF   (LNK-ORCSSRYCDCHK-SANTEIYM  =   SPACE  )
             OR   (LNK-ORCSSRYCDCHK-SANTEIYM  <  "202307")
               MOVE    3    TO   LNK-ORCSSRYCDCHK-RC
             END-IF
           END-IF
      *
      *    保険組み合わせ番号チェック
           IF    LNK-ORCSSRYCDCHK-RC  =  ZERO
             IF    LNK-ORCSSRYCDCHK-HKNCOMBI(1)  =  ZERO
               MOVE    4    TO   LNK-ORCSSRYCDCHK-RC
             END-IF
           END-IF
      *
      *    診療行為コードチェック
           IF    LNK-ORCSSRYCDCHK-RC  =  ZERO
             IF    LNK-ORCSSRYCDCHK-SRYCD  =   SPACE
               MOVE    5    TO   LNK-ORCSSRYCDCHK-RC
             END-IF
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           MOVE     ZERO     TO   FLG-GAITO-HKNJYA
           MOVE     ZERO     TO   FLG-KOH959-ARI
           MOVE     ZERO     TO   FLG-YOYAKUCD-ARI
           INITIALIZE             WRK-CHECK-TABLE
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                     UNTIL  (IDX                             >   10 )
                      OR    (LNK-ORCSSRYCDCHK-HKNCOMBI(IDX)  =  ZERO)
      *
             INITIALIZE                              HKNCOMBI-REC
             MOVE    LNK-ORCSSRYCDCHK-HOSPNUM    TO  COMB-HOSPNUM
             MOVE    LNK-ORCSSRYCDCHK-PTID       TO  COMB-PTID
             MOVE    LNK-ORCSSRYCDCHK-HKNCOMBI(IDX)
                                                 TO  COMB-HKNCOMBINUM
             MOVE    HKNCOMBI-REC                TO  MCPDATA-REC
             MOVE    "DBSELECT"                  TO  MCP-FUNC
             MOVE    "tbl_hkncombi"              TO  MCP-TABLE
             MOVE    "key"                       TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"                 USING  MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
             IF      MCP-RC  =  ZERO
               MOVE    "tbl_hkncombi"     TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               PERFORM  910-HKNCOMBI-READ-SEC
               IF    FLG-HKNCOMBI  =  ZERO
                 IF  COMB-HKNNUM  NOT = SPACE
                   IF  COMB-HKNNUM(1:2) = "98" OR "90" OR "91"
                     CONTINUE
                   ELSE
      *              患者保険情報検索
                     PERFORM  2001-PTHKNINF-KENSAKU-SEC
                   END-IF
                 END-IF
               END-IF
             END-IF
             MOVE    "tbl_hkncombi"       TO  MCP-TABLE
             MOVE    "key"                TO  MCP-PATHNAME
             PERFORM  910-DB-CLOSE-SEC
      *
           END-PERFORM
      *
           IF   FLG-GAITO-HKNJYA  =  1
      *      患者公費情報検索（保険番号959）
             PERFORM  2002-PTKOHINF-KENSAKU-SEC
             IF   FLG-KOH959-ARI  =  ZERO
      *        診療会計検索
               PERFORM  2003-SRYACCT-KENSAKU-SEC
               IF  FLG-YOYAKUCD-ARI =  1
                 MOVE   1    TO    LNK-ORCSSRYCDCHK-GAITOFLG
               END-IF
             END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      ***************************************************************
      *    患者保険情報検索
      ***************************************************************
       2001-PTHKNINF-KENSAKU-SEC           SECTION.
      *
           INITIALIZE                              PTHKNINF-REC
           MOVE    LNK-ORCSSRYCDCHK-HOSPNUM    TO  PTHKN-HOSPNUM
           MOVE    LNK-ORCSSRYCDCHK-PTID       TO  PTHKN-PTID
           MOVE    COMB-HKNID                  TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC                TO  MCPDATA-REC
           MOVE    "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_pthkninf"              TO  MCP-TABLE
           MOVE    "key"                       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING  MCPAREA
                                                      MCPDATA-REC
                                                      SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE    "tbl_pthkninf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             PERFORM  910-PTHKNINF-READ-SEC
             IF    FLG-PTHKNINF  =  ZERO
               IF  PTHKN-HKNJANUM  =  SPACE
                 MOVE    1             TO  FLG-GAITO-HKNJYA
                 MOVE    1             TO  WRK-CHECK-FLG(IDX)
                 MOVE  LNK-ORCSSRYCDCHK-SANTEIYM
                                       TO  WRK-CHECK-YM(IDX)
                 IF    PTHKN-TEKSTYMD(1:6) >=
                                   LNK-ORCSSRYCDCHK-SANTEIYM
                   MOVE   PTHKN-TEKSTYMD(7:2)
                                       TO  WRK-CHECK-DD(IDX)
                 ELSE
                   MOVE   01           TO  WRK-CHECK-DD(IDX)
                 END-IF
               END-IF
             END-IF
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       2001-PTHKNINF-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報検索（保険番号959）
      *****************************************************************
       2002-PTKOHINF-KENSAKU-SEC            SECTION.
      *
           INITIALIZE                              PTKOHINF-REC
           MOVE    LNK-ORCSSRYCDCHK-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    LNK-ORCSSRYCDCHK-PTID       TO  PTKOH-PTID
           MOVE    "959"                       TO  PTKOH-KOHNUM
           MOVE    PTKOHINF-REC                TO  MCPDATA-REC
           MOVE    "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_ptkohinf"              TO  MCP-TABLE
           MOVE    "key4"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING  MCPAREA
                                                      MCPDATA-REC
                                                      SPA-AREA
           IF      MCP-RC  =  ZERO
              MOVE      "tbl_ptkohinf"    TO  MCP-TABLE
              MOVE      "key4"            TO  MCP-PATHNAME
              PERFORM    910-PTKOHINF-READ-SEC
              PERFORM  UNTIL  FLG-PTKOHINF  =  1
                PERFORM  VARYING  IDX  FROM  1  BY  1
                          UNTIL  (IDX                      >   10 )
                           OR    (LNK-ORCSSRYCDCHK-HKNCOMBI(IDX)
                                                           =  ZERO)
                           OR    (FLG-KOH959-ARI           =   1  )
                  IF  WRK-CHECK-FLG(IDX) = 1
                    IF  (PTKOH-TEKSTYMD     <=  WRK-CHECK-YMD(IDX))
                    AND (WRK-CHECK-YMD(IDX) <=  PTKOH-TEKEDYMD    )
                    AND (PTKOH-SAKJOKBN      =  SPACE             )
                      MOVE    1    TO    FLG-KOH959-ARI
                      MOVE    1    TO    FLG-PTKOHINF
                    END-IF
                  END-IF
                END-PERFORM
                IF  FLG-PTKOHINF = ZERO
                  MOVE    "tbl_ptkohinf"    TO  MCP-TABLE
                  MOVE    "key4"            TO  MCP-PATHNAME
                  PERFORM  910-PTKOHINF-READ-SEC
                END-IF
              END-PERFORM
           END-IF
           MOVE    "tbl_ptkohinf"           TO  MCP-TABLE
           MOVE    "key4"                   TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
       2002-PTKOHINF-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計検索
      *****************************************************************
       2003-SRYACCT-KENSAKU-SEC            SECTION.
      *
           INITIALIZE                                SRYACT-REC
           MOVE    LNK-ORCSSRYCDCHK-HOSPNUM      TO  SRY-HOSPNUM
           MOVE    LNK-ORCSSRYCDCHK-NYUGAIKBN    TO  SRY-NYUGAIKBN
           MOVE    LNK-ORCSSRYCDCHK-PTID         TO  SRY-PTID
           MOVE    LNK-ORCSSRYCDCHK-SANTEIYM     TO  SRY-SRYYM
           MOVE    LNK-ORCSSRYCDCHK-SRYCD        TO  SRY-SRYCD(1)
           MOVE    LNK-ORCSSRYCDCHK-SRYCD        TO  SRY-SRYCD(2)
           MOVE    LNK-ORCSSRYCDCHK-SRYCD        TO  SRY-SRYCD(3)
           MOVE    LNK-ORCSSRYCDCHK-SRYCD        TO  SRY-SRYCD(4)
           MOVE    LNK-ORCSSRYCDCHK-SRYCD        TO  SRY-SRYCD(5)
           MOVE    SRYACT-REC                    TO  MCPDATA-REC
           MOVE    "DBSELECT"                    TO  MCP-FUNC
           MOVE    "tbl_sryact"                  TO  MCP-TABLE
           MOVE    "key7"                        TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                   USING  MCPAREA
                                                        MCPDATA-REC
                                                        SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE    "tbl_sryact"       TO  MCP-TABLE
             MOVE    "key7"             TO  MCP-PATHNAME
             PERFORM  910-SRYACT-READ-SEC
             PERFORM  UNTIL  FLG-SRYACT  =  1
               INITIALIZE                   SRYACCT-REC
               MOVE    SRY-HOSPNUM      TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN    TO  ACCT-NYUGAIKBN
               MOVE    SRY-SRYKA        TO  ACCT-SRYKA
               MOVE    SRY-PTID         TO  ACCT-PTID
               MOVE    SRY-SRYYM        TO  ACCT-SRYYM
               MOVE    SRY-ZAINUM       TO  ACCT-ZAINUM
               MOVE    SRYACCT-REC      TO  MCPDATA-REC
               MOVE    "DBSELECT"       TO  MCP-FUNC
               MOVE    "tbl_sryacct"    TO  MCP-TABLE
               MOVE    "key31"          TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC  =  ZERO
                 MOVE    "tbl_sryacct"  TO  MCP-TABLE
                 MOVE    "key31"        TO  MCP-PATHNAME
                 PERFORM  910-SRYACCT-READ-SEC
                 IF    FLG-SRYACCT  =  ZERO
                   IF  ACCT-ZAIKAISU  NOT =  ZERO
                     PERFORM  VARYING  IDX  FROM  1  BY  1
                               UNTIL  (IDX                      >   10 )
                                OR    (LNK-ORCSSRYCDCHK-HKNCOMBI(IDX)
                                                                =  ZERO)
                                OR    (FLG-YOYAKUCD-ARI         =   1  )
                       IF  ACCT-HKNCOMBI =
                                      LNK-ORCSSRYCDCHK-HKNCOMBI(IDX)
                         MOVE    1    TO    FLG-YOYAKUCD-ARI
                         MOVE    1    TO    FLG-SRYACT
                       END-IF
                     END-PERFORM
                   END-IF
                 END-IF
               END-IF
               MOVE    "tbl_sryacct"    TO  MCP-TABLE
               MOVE    "key31"          TO  MCP-PATHNAME
               PERFORM  910-DB-CLOSE-SEC
      *
               IF  FLG-SRYACT = ZERO
                 MOVE    "tbl_sryact"     TO  MCP-TABLE
                 MOVE    "key7"           TO  MCP-PATHNAME
                 PERFORM  910-SRYACT-READ-SEC
               END-IF
             END-PERFORM
           END-IF
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key7"               TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
       2003-SRYACCT-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       910-PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       910-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       910-PTINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報読込
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
      *
      *****************************************************************
      *    診療会計読込
      *****************************************************************
       910-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       910-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       910-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       910-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
