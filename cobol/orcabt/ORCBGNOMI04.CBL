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
       PROGRAM-ID.             ORCBGNOMI04.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   患者病名疾患区分一括更新
      *  管理者            : 
      *  作成日付    作業者        記述
      *  24/08/01    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    一覧（縦）
           COPY    "HCMSL80.INC".
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTBYOMEI            PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-GAITO               PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                     PIC 9(02).
           03  IDW                     PIC 9(05).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *****03  RECEERR                 PIC X(100).
           03  WRK-PARA-SYORIKBN       PIC X(01).
           03  WRK-PARA-SRYYMD         PIC X(08).
           03  WRK-PARA-CSVKBN         PIC X(01).
           03  WRK-PARA-UPYMD          PIC X(08).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(05).
           03  CNT-LINE                PIC 9(02).
           03  CNT-CSV                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-PTID                PIC 9(10).
           03  WRK-MSNSEIKBN           PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
           03  WRK-SYSYMDG             PIC X(22).
      *    処理年月
           03  WRK-SRY-YYMM.
               05  WRK-SRY-YY          PIC 9(04).
               05  WRK-SRY-MM          PIC 9(02).
      *
      * 確認リスト
       01  LST-PRT-AREA.
           03  LST-PRT-HEAD1.
               05  PRT-TITLE           PIC X(81).
               05  PRT-SYSYMD          PIC X(17)   VALUE   SPACE.
               05  PRT-PAGE            PIC ZZZ9.
               05  FILLER              PIC X(02)   VALUE   "頁".
           03  LST-PRT-HEAD2.
               05  FILLER              PIC X(21)   VALUE   "患者番号".
               05  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  LST-PRT-HEAD3.
               05  FILLER              PIC X(10)   VALUE   SPACE.
               05  FILLER              PIC X(07)   VALUE
                   "診療科".
               05  FILLER              PIC X(12)   VALUE
                   "診療開始日".
               05  FILLER              PIC X(11)   VALUE
                   "病名コード".
               05  FILLER              PIC X(51)   VALUE
                   "病名".
               05  FILLER              PIC X(20)   VALUE
                   "疾患区分変更内容".
      *
           03  LST-PRT-BODY-01.
               05  PRT-PTNUM           PIC X(20).
               05  PRT-FIL10           PIC X(01).
               05  PRT-NAME            PIC X(100).
      *
           03  LST-PRT-BODY-02.
               05  PRT-FIL20           PIC X(12).
               05  PRT-SRYKA           PIC X(02).
               05  PRT-FIL21           PIC X(03).
               05  PRT-SRYYMD          PIC X(09).
               05  PRT-FIL22           PIC X(02).
               05  PRT-BYOMEICD        PIC X(07).
               05  PRT-FIL23           PIC X(04).
               05  PRT-BYOMEI          PIC X(50).
               05  PRT-FIL24           PIC X(01).
               05  PRT-MANSEIKBN-BE    PIC 9(02).
               05  PRT-FIL25           PIC X(04).
               05  PRT-MANSEIKBN-AF    PIC 9(02).
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
      *    ＣＳＶ用
           03  WRK-CSVHENYMD.
               05  WRK-CYY         PIC 9(04).
               05  FILLER          PIC X(01)   VALUE   "/".
               05  WRK-CMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   "/".
               05  WRK-CDD         PIC 9(02).
      *
       01  MAX-LINE           PIC 9(03)   VALUE   79.
      *
      *    ＣＳＶデータ編集用
       01  WRK-CSV-AREA.
           03  WRK-IN-DATA             PIC X(2000).
           03  WRK-OUT-REC             PIC X(2000).
           03  WRK-CSV-REC             PIC X(2000).
           03  WRK-REC-LENGTH          PIC 9(05).
           03  WRK-IN-MAX              PIC 9(04).
           03  WRK-END                 PIC 9(01).
      *
           03  WRK-TOUKEIERR           PIC X(200).
      *
      *    ＣＳＶ出力先
       01  WRK-TO-DATA-AREA.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(19)   VALUE
                                       "sknkbn_chg_list.csv".
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
      *    未コード化傷病名コード
           03  WRK-CONS-BYOMEICD   PIC X(07)   VALUE   "0000999".
      *
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "診療開始日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "病名コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "病名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "疾患区分・前".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "疾患区分・後".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
           PERFORM UNTIL   FLG-END NOT =   ZERO
               PERFORM 200-MAIN-SEC
           END-PERFORM
           PERFORM 300-END-SEC
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  WRK-PARA
           INITIALIZE  SPA-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
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
                                               WRK-PARA-SYORIKBN
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-CSVKBN
                                               WRK-PARA-UPYMD
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE  SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBGNOMI04"       TO  JOB-PGID
           MOVE    "患者病名疾患区分一括更新"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           PERFORM 110-PARA-HENSYU-SEC
      *
           INITIALIZE  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-SRYYMD          TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    1                   TO  FLG-END
               END-IF
           ELSE
               INITIALIZE  PTBYOMEI-REC
               MOVE    1               TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  WRK-SYSYMD
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
           END-IF
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *
      *    処理日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SYSYMD
      *
      *    処理日
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-SRYYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
      *    診療開始日
           IF      WRK-PARA-SRYYMD NOT =   SPACE
               MOVE    WRK-PARA-SRYYMD     TO  SPA-SRYYMD
           END-IF
      *
      *    ＣＳＶ
           MOVE    SPA-HOSPNUM         TO  WRK-TO-DATA-HOSPNUM
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    MCPDATA-REC         TO  PTBYOMEI-REC
      *
      * 患者病名（未コード化はスキップ）テーブルの病名コードから
      * 病名マスタを参照し、疾患区分をセットする）
      * 開始日は切れないので無条件に疾患区分をリセットする
      *
      * 対象とする患者病名の条件
      * 1.転帰日がない
      * 2.基本病名コードが未コード化でない
      * 3.疑い病名でない
      *
      *    IF      PTBYO-DLTFLG        =   "1"
      *        削除フラグがあるものはスキップ
      *        GO  TO  200-MAIN-90
      *    END-IF
      *
           IF     (PTBYO-TENKIYMD  NOT =   SPACE)  OR
      *        転帰日があるものはスキップ
                  (PTBYO-KHNBYOMEICD   =   WRK-CONS-BYOMEICD)  OR
      *        基本病名コードが未コード化はスキップ
                  (PTBYO-UTAGAIFLG     =   "1"  OR  "3")
      *        疑い病名はスキップ
               GO  TO  200-MAIN-90
           END-IF
      *
           IF      WRK-PARA-UPYMD  NOT =   SPACE
               IF      PTBYO-UPYMD        >=   WRK-PARA-UPYMD
      *            更新スキップ指定日がある場合
      *            更新日が指定日以上はスキップ
                   GO  TO  200-MAIN-90
               END-IF
           END-IF
      *
      *    慢性疾患区分 09難病はスキップ
           IF      PTBYO-MANSEIKBN     =   09
               GO  TO  200-MAIN-90
           END-IF
      *
      * 病名コードをすべてチェックするのはやめた
      **   病名コード
      **   PERFORM VARYING IDX     FROM    1   BY  1
      **           UNTIL   IDX     >       PTBYO-BYOMEICDSU
      **       修飾語はスキップ
      **       IF      PTBYO-BYOMEICD (IDXX) (1:3) =   "ZZZ"
      **       未コード化はスキップ
      **       IF      PTBYO-BYOMEICD (IDXX)   =   WRK-CONS-BYOMEICD
      **   END-PERFORM
      *
      * 基本病名コードを元にチェックを行う
      *
           MOVE    ZERO                TO  FLG-GAITO
           MOVE    PTBYO-MANSEIKBN     TO  WRK-MSNSEIKBN
      *
      *    慢性疾患区分 05,08 ZEROを対象とする
           IF      (PTBYO-MANSEIKBN    =   05  OR  08  OR  ZERO)
      *        臨時対応でコード指定対応（病名マスタアクセス軽減）
               PERFORM 2001-TOKBYOMEI-CHK-SEC
           END-IF
      *
           IF      FLG-GAITO           =   ZERO
      *        既定のコードに合致しない場合は病名マスタを検索する
               INITIALIZE  BYOMEI-REC
               MOVE    PTBYO-KHNBYOMEICD   TO  BYO-BYOMEICD
               PERFORM 900-BYOMEI-READ-SEC
               IF      FLG-BYOMEI          =   ZERO
                   IF     (PTBYO-MANSEIKBN NOT =   BYO-TOKSKNCD)   AND
                         ((PTBYO-MANSEIKBN     =   05  OR  08)     OR
                         ((BYO-TOKSKNCD        =   05  OR  08)     AND
                          (BYO-NANBYOCD    NOT =   09)))
                       MOVE    BYO-TOKSKNCD        TO
                                                   PTBYO-MANSEIKBN
                       MOVE    1                   TO  FLG-GAITO
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-GAITO           =   1
               IF      WRK-PARA-SYORIKBN   NOT =   "1"
                   PERFORM 2002-PTBYO-UPDATE-SEC
               END-IF
               PERFORM 2001-PRINT-SYORI-SEC
           END-IF
           .
       200-MAIN-90.
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  FLG-END
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    特定疾患療養管理料対象外チェック処理
      *****************************************************************
       2001-TOKBYOMEI-CHK-SEC          SECTION.
      *
           IF      PTBYO-MANSEIKBN     =   05
                                       OR  08
      *    特定疾患療養管理料 としない
      *        基本病名コード
               EVALUATE    PTBYO-KHNBYOMEICD
      *            インスリン抵抗性糖尿病
                   WHEN    "2500001"
      *            糖尿病
                   WHEN    "2500013"
      *            １型糖尿病
                   WHEN    "2500014"
      *            ２型糖尿病
                   WHEN    "2500015"
      *            高血圧症
                   WHEN    "8833421"
      *            収縮期高血圧症
                   WHEN    "8842500"
      *            若年高血圧症
                   WHEN    "4019016"
      *            本態性高血圧症
                   WHEN    "8840107"
      *            脂質異常症
                   WHEN    "8844446"
      *            高脂血症
                   WHEN    "2724007"
      *            高コレステロール血症
                   WHEN    "2720004"
                       MOVE     ZERO           TO  PTBYO-MANSEIKBN
      *                変更あり
                       MOVE     1              TO  FLG-GAITO
               END-EVALUATE
           ELSE
      *        特定疾患療養管理料 とする
      *        基本病名コード
               EVALUATE    PTBYO-KHNBYOMEICD
      *            アナフィラキシー
                   WHEN    "8830279"
      *            ギラン・バレー症候群
                   WHEN    "3570001"
                       MOVE     05             TO  PTBYO-MANSEIKBN
      *                変更あり
                       MOVE     1              TO  FLG-GAITO
               END-EVALUATE
           END-IF
           .
       2001-TOKBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者病名更新処理
      *****************************************************************
       2002-PTBYO-UPDATE-SEC       SECTION.
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "患者病名の更新に失敗しました"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       2002-PTBYO-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       2001-PRINT-SYORI-SEC        SECTION.
      *
           IF      PTBYO-PTID      NOT =   WRK-PTID
               MOVE    PTBYO-PTID          TO  WRK-PTID
      *        明細編集（患者情報：１行目）
               MOVE    SPACE               TO  LST-PRT-BODY-01
      *        患者情報取得
               PERFORM 20011-PTNUM-HEN-SEC
               IF      CNT-LINE            >=  MAX-LINE
                   PERFORM 400-PRINT-SEC
                   MOVE    ZERO                TO  CNT-LINE
                   PERFORM 280-HEAD-HENSYU-SEC
               END-IF
               IF      CNT-LINE            =   ZERO
                   PERFORM 280-HEAD-HENSYU-SEC
               END-IF
               IF      CNT-LINE            =   4
                   ADD     1                   TO  CNT-LINE
               ELSE
                   ADD     2                   TO  CNT-LINE
               END-IF
               MOVE    LST-PRT-BODY-01     TO  HCMSL80-MOJI (CNT-LINE)
           END-IF
      *
      *    明細編集（疾患区分変更情報：２行目）
           MOVE    SPACE               TO  LST-PRT-BODY-02
           MOVE    PTBYO-SRYKA         TO  PRT-SRYKA
           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  PRT-SRYYMD
           MOVE    PTBYO-KHNBYOMEICD   TO  PRT-BYOMEICD
           MOVE    PTBYO-BYOMEI        TO  PRT-BYOMEI
           MOVE    WRK-MSNSEIKBN       TO  PRT-MANSEIKBN-BE
           MOVE    " => "              TO  PRT-FIL25
           MOVE    PTBYO-MANSEIKBN     TO  PRT-MANSEIKBN-AF
           IF      CNT-LINE            >=  MAX-LINE
               PERFORM 400-PRINT-SEC
               MOVE    ZERO                TO  CNT-LINE
               PERFORM 280-HEAD-HENSYU-SEC
           END-IF
           ADD     1                   TO  CNT-LINE
           MOVE    LST-PRT-BODY-02     TO  HCMSL80-MOJI (CNT-LINE)
      *
           IF      WRK-PARA-CSVKBN     =   "1"
      *         ＣＳＶデータ出力
                PERFORM 500-CSV-SYORI-SEC
           END-IF
           .
       2001-PRINT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20011-PTNUM-HEN-SEC         SECTION.
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTBYO-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM         TO  PRT-PTNUM
           END-IF
      *    患者マスター読み込み
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTBYO-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "＊＊＊＊"          TO  PRT-NAME
           ELSE
               MOVE    PTINF-NAME          TO  PRT-NAME
           END-IF
           .
       20011-PTNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       280-HEAD-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  HCMSL80
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  PRT-PAGE
           IF      WRK-PARA-SYORIKBN   =   "1"
               MOVE    "患者病名疾患区分一括更新（更新なし）"
                                           TO  PRT-TITLE
           ELSE
               MOVE    "患者病名疾患区分一括更新"
                                           TO  PRT-TITLE
           END-IF
           MOVE    LST-PRT-HEAD1       TO  HCMSL80-MOJI (1)
           MOVE    LST-PRT-HEAD2       TO  HCMSL80-MOJI (2)
           MOVE    LST-PRT-HEAD3       TO  HCMSL80-MOJI (3)
           MOVE    ALL "-"             TO  HCMSL80-MOJI (4)
           MOVE    4                   TO  CNT-LINE
           .
       280-HEAD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       400-PRINT-SEC               SECTION.
      *
           INITIALIZE  ORCSPRTAREA
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
           MOVE    "HCMSL80.red"       TO  SPRT-PRTID
           MOVE    "患者病名疾患区分一括更新"
                                       TO  SPRT-TITLE
           MOVE    HCMSL80             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"       USING   ORCSPRTAREA
                                           SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-EXIT.
           EXIT.
      *****************************************************************
      *    ＣＳＶデータ処理
      *****************************************************************
       500-CSV-SYORI-SEC           SECTION.
      *
           IF      CNT-CSV             =   ZERO
               PERFORM 5001-CSV-HEAD-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-CSV-AREA
           INITIALIZE  WRK-CSV-AREA
      *    患者番号
           MOVE    SPTID-PTNUM         TO  WRK-IN-DATA
           MOVE    20                  TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    患者氏名
           MOVE    PTINF-NAME          TO  WRK-IN-DATA
           MOVE    100                 TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    診療科コード
           MOVE    PTBYO-SRYKA         TO  WRK-IN-DATA
           MOVE    2                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    診療開始日
           MOVE    PTBYO-SRYYMD        TO  WRK-IN-DATA
           MOVE    8                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    病名コード
           MOVE    PTBYO-KHNBYOMEICD   TO  WRK-IN-DATA
           MOVE    7                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    病名
           MOVE    PTBYO-BYOMEI        TO  WRK-IN-DATA
           MOVE    100                 TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    疾患区分・前
           MOVE    WRK-MSNSEIKBN       TO  WRK-IN-DATA
           MOVE    2                   TO  WRK-IN-MAX
           PERFORM 5002-MOJI-HENSYU-SEC
      *    疾患区分・後
           MOVE    PTBYO-MANSEIKBN     TO  WRK-IN-DATA
           MOVE    2                   TO  WRK-IN-MAX
           MOVE    1                   TO  WRK-END
           PERFORM 5002-MOJI-HENSYU-SEC
           MOVE    SPACE               TO  WRK-OUT-REC
           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                   WRK-KAIGYO              DELIMITED  BY  SIZE
                   INTO        WRK-OUT-REC
           END-STRING
           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
           PERFORM 600-TOUKEICSV-SEC
           .
       500-CSV-SYORI-EXIT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       5001-CSV-HEAD-SEC           SECTION.
      *
           MOVE    CSV-HEAD-01         TO  WRK-OUT-REC
           MOVE    SPACE               TO  WRK-CSV-REC
           STRING  WRK-OUT-REC         DELIMITED  BY  SPACE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                                       INTO  WRK-CSV-REC
           END-STRING
           PERFORM 600-TOUKEICSV-SEC
           .
       5001-CSV-HEAD-EXIT.
           EXIT.
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5002-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-IN-DATA         =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW     FROM    WRK-IN-MAX  BY  -1
                       UNTIL   IDW     <       1
                   IF      WRK-IN-DATA (IDW:1) NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-IN-DATA (1:IDW)
                                                   TO  WRK-CSV-REC
                       ELSE
                           MOVE    SPACE           TO  WRK-OUT-REC
                           STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-IN-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-OUT-REC
                           END-STRING
                           MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END             =   ZERO
               MOVE    SPACE               TO  WRK-OUT-REC
               STRING  WRK-CSV-REC(1:WRK-REC-LENGTH)
                       WRK-KUGIRI              DELIMITED  BY  SIZE
                       INTO        WRK-OUT-REC
               END-STRING
               MOVE    WRK-OUT-REC         TO  WRK-CSV-REC
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
           .
       5002-MOJI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       600-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE  ORCSTOUKEICSVAREA
           MOVE    "INS"               TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  STOUKEICSV-SHELLID
           MOVE    WRK-SRY-YYMM        TO  STOUKEICSV-SRYYM
           MOVE    ZERO                TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"         TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA         TO  STOUKEICSV-TO-DATA
           MOVE    "2"                 TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSV-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "患者病名疾患区分一括更新"
                                       TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV" USING   ORCSTOUKEICSVAREA
                                           SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               ADD     1                   TO  CNT-CSV
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       600-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
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
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-RECEERR
           STRING  "ORCBGNOMI04"       DELIMITED  BY  SIZE
                   WRK-TOUKEIERR       DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-LINE            >   ZERO
               PERFORM 400-PRINT-SEC
           END-IF
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      CNT-PAGE            =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBGNOMI04 END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  920-DBFETCH-SEC 
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
