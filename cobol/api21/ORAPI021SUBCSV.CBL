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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI021SUBCSV.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 電子処方箋ＣＳＶ作成（xml）
      *  管理者            :
      *  作成日付    作業者        記述
      *  22/12/2X    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
       DATA                DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
      **    COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *     03  FLG-WKSRYACT            PIC 9(01).
      *     03  FLG-PTNYUINRRK          PIC 9(01).
      *     03  FLG-HKNNUM              PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-SPATMP              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-CHKWK               PIC 9(01).
           03  FLG-DOUJITU             PIC 9(01).
      *
           03  FLG-END-SYORI2          PIC 9(01).
           03  FLG-YAKUZAI-ARI         PIC 9(01).
           03  FLG-SYOHOU-ARI          PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-END-SYORI           PIC 9(01).
           03  FLG-KENSADEL            PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-SURYO               PIC 9(01).
           03  FLG-KEIMSG              PIC 9(01).
           03  FLG-REF-ERR             PIC 9(01).
      *
      *     03  FLG-WKSRYACT-INS        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
      *
           03  IDK                 PIC 9(04).
           03  IDX-I               PIC 9(04).
           03  IDX-I1              PIC 9(04).
           03  IDX-I2              PIC 9(04).
           03  IDX6                PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-ERR             PIC 9(04).
      *2024/01
           03  IDXR                PIC 9(04).
           03  IDX-P               PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-CSV                 PIC 9(04).
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    保険組合せ
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(80).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(20).
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID                PIC X(10).
      *
      *    エラーコード
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-WERRCD              PIC X(03).
           03  WRK-KEICD-G.
               05  WRK-KEICD           PIC X(03)   OCCURS   10.
           03  WRK-ERR-SRYCD           PIC X(09).
      *
           03  WRK-ERRMSG-02           PIC X(200).
           03  WRK-MEISYO              PIC X(200).
           03  WRK-ERRMSG1             PIC X(200).
           03  WRK-ERRMSG2             PIC X(200).
      *    内容エラー
           03  WRK-MD-ERRCD            PIC X(04).
           03  WRK-MD-ERRMSG.
               05  WRK-MD-ERRMSG1          PIC X(100).
               05  WRK-MD-ERRMSG2          PIC X(100).
           03  WRK-MD-ERR-LINE         PIC X(03).
           03  WRK-MD-ERR-IDX          PIC X(02).
           03  WRK-MD-SRYCD            PIC X(09).
      *    警告メッセージ
           03  WRK-KEIMSG.
               05  WRK-KEIMSG1             PIC X(100).
               05  WRK-KEIMSG2             PIC X(100).
           03  WRK-ERRCD-1             PIC X(02).
           03  WRK-ERRMSG-1            PIC X(100).
           03  WRK-ERRCD-2             PIC X(02).
           03  WRK-ERRMSG-2            PIC X(100).
           03  WRK-BYO-IDX             PIC 9(02).
      *
      *    ユーザプログラム
           03  WRK-RENNUM              PIC 9(02).
           03  WRK-GMN-SELNUM          PIC 9(02).
           03  WRK-SYORI-FLG           PIC 9(01).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-MCP-WINDOW          PIC X(64).
      *
           03  WRK-OPID                PIC X(16).
      *
      *    保険組合せ内容
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-RSI-HKNKBN          PIC X(01).
      *
           03  WRK-SURYO               PIC 9(05)V9(5).
      *
      *    診療内
      *    コメント入力値編集
           03  WRK-INPUTCHI-G.
               05  WRK-INPUTCHI        PIC X(40)   OCCURS  5.
      *
           03  WRK-REF-KAISU           PIC X(03).
      *
      *    警告メッセージ
       01  TBL-KEIMSG-AREA.
           03  TBL-KEIMSG-G            OCCURS  400.
               05  TBL-KEIERRCD            PIC X(04).
               05  TBL-KEIMSG.
                   07  TBL-KEIMSG1             PIC X(100).
                   07  TBL-KEIMSG2             PIC X(100).
               05  TBL-KEI-SRYCD           PIC X(09).
               05  TBL-KEI-LINE            PIC X(03).
               05  TBL-KEI-IDX             PIC X(02).
           03  IDX-KEI                     PIC 9(03).
      *
      *
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT           PIC X(500).
           03  WRK-ATO-INPUT           PIC X(500).
           03  WRK-MAX                 PIC 9(04).
           03  WRK-LEN                 PIC 9(04).
      *
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *
      *受取剤数
       01  COB-ZAI-INMAX           PIC 9(04)   VALUE   40.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      ***  COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    患者番号発生サブ
           COPY    "CPORCSP01.INC".
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *    ユーザプログラム起動チェックサブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      ****
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC92.INC".
      *    パラメタ
           COPY    "CPORCSC10S.INC".
      *    診療行為内容編集・計算
           COPY    "CPORCSC100.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    ワーク診療行為データ編集領域
           COPY    "CPORAPI021SUB1V3.INC".
      *
      *    診療内容編集サブ領域
           COPY    "CPORAPI021SUB2V3.INC".
      *
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCWKSRY.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    電子処方箋ＣＳＶファイル名
           COPY    "CPORCHC02EPRE.INC".
      *
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *    ＳＰＡ画面パラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *H29.6
      *    一時ディレクトリ取得・削除サブ
           COPY    "CPORCSTEMPDIR.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本
      *01  SYSBASE-REC.
      *    COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
      *01  SYSUSER-REC.
      *    COPY    "CPSYSUSER.INC".
      *
           COPY    "CPSYSKANRI.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY  "CPSK1010.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    患者マスタ−
      * 01  PTNUM-REC.
      *     COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ワーク診療行為マスタ−
      * 01  WKSRYACT-REC.
      *     COPY    "CPWKSRYACT.INC".
      *
      *    入院履歴
      * 01  PTNYUINRRK-REC.
      *     COPY    "CPPTNYUINRRK.INC".
      *
      *    保険番号
      * 01  HKNNUM-REC.
      *     COPY    "CPHKNNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *　　（ＣＬＡＩＭ接続）
      *    COPY    "CPSK9000.INC".
      *    COPY    "CPSK9001.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
      *    スパ領域
            COPY    "COMMON-SPA".
      *
      *    API XML読み込み用定義体
           COPY    "CPMDCXMLV2REQ.INC".
      *
           COPY    "CPORAPI021SUBCSV.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           XML-MEDICALREQ
           ORAPI021SUBCSVAREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      ORAPICSV-OUT-AREA
           INITIALIZE                      ORAPICSV-ERRCD
           INITIALIZE                      ORAPICSV-ERRMSG
           INITIALIZE                      ORAPICSV-MD-ERR-G
      *
           INITIALIZE                      ORCHC02EPREAREA
           INITIALIZE                      TBL-KEIMSG-AREA
      *
           INITIALIZE                      SPA-K02
      *
      *    診療日付
           MOVE    ORAPICSV-SRYYMD         TO  SPA-SRYYMD
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    SPA患者情報
           MOVE    ORAPICSV-PTID       TO  SPA-PTID
           MOVE    ORAPICSV-PTNUM      TO  SPA-PTNUM
           MOVE    ORAPICSV-SRYKA      TO  SPA-SRYKA
           MOVE    ORAPICSV-HKNCOMBI   TO  SPA-HKNCOMBINUM
      *
      *     MOVE    WRK-SRYKA-NAME      TO  SPA-SRYKAMEI
      *     MOVE    WRK-DRCD-NAME       TO  SPA-DRCD-NAME
           MOVE    ORAPICSV-DRCD       TO  SPA-DRCD
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "@"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    "K10"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    保険組合せ判定
           IF     (SPA-HKN-ERR     NOT =   ZERO )  OR
                  (SPA-RJN-ERR     NOT =   ZERO )
               PERFORM 20021-HKN-CHEK-SEC
           END-IF
      *    ワーク診療行為データ作成
           IF      WRK-ERRCD           =   SPACE
               PERFORM 20021-WKSRYACT-SYORI-SEC
      *
      *        エラー(ORAPICSV-MD-ERRCD　編集ずみ)
               IF      WRK-ERRCD       NOT =   SPACE
                   GO      TO      20021-MAIN-SYORI-EXT
               END-IF
           END-IF
      *    処方情報のチェック
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
               PERFORM 20013-SHOHOU-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        中途データ展開・点数計算チェックなど
               PERFORM 20011-INPUTSYORI-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
               MOVE    ORCHCEPRE-OT-CSVFILE
                                       TO  ORAPICSV-FILENAME
           END-IF
      *
           IF     (WRK-ERRCD       NOT =   SPACE )
              OR  (WRK-MD-ERRCD    NOT =   SPACE )
               IF      ORAPICSV-ERRCD      =   SPACE
                   PERFORM 2900-ORAPICSV-ERRMSG-SEC
               END-IF
           END-IF
           .
       20021-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ引数に編集処理
      *****************************************************************
       2900-ORAPICSV-ERRMSG-SEC        SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    "61"            TO  WRK-ERRCD
           END-IF
      *
           MOVE    WRK-ERRCD        TO  ORAPICSV-ERRCD
           MOVE    WRK-ERRMSG       TO  ORAPICSV-ERRMSG
      *    診療内容エラー
           MOVE    WRK-MD-ERRCD     TO  ORAPICSV-MD-ERRCD  (1)
           MOVE    WRK-MD-ERRMSG1   TO  ORAPICSV-MD-ERRMSG1 (1)
           MOVE    WRK-MD-ERRMSG2   TO  ORAPICSV-MD-ERRMSG2 (1)
           MOVE    WRK-MD-SRYCD     TO  ORAPICSV-MD-SRYCD (1)
           IF      WRK-MD-SRYCD     NOT =   SPACE
               MOVE    ORAPICSV-MD-SRYCD (1)
                                          TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME        TO  ORAPICSV-MD-SRYNAME (1)
               END-IF
           END-IF
           .
       2900-ORAPICSV-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ判定処理
      *****************************************************************
       20021-HKN-CHEK-SEC        SECTION.
      *
           EVALUATE    SPA-RJN-ERR
      *        老人保険編集エラーがある時、エラーとする
               WHEN    1
                   MOVE    "1027"              TO  SPA-ERRCD
      *        老人保険編集エラーがある時、エラーとする
               WHEN    2
                   MOVE    "1070"              TO  SPA-ERRCD
      *        後期高齢者の保険がない時
               WHEN    3
                   MOVE    "1076"              TO  SPA-ERRCD
      *        退職国保で６５歳以上
               WHEN    9
                   MOVE    "1077"              TO  SPA-ERRCD
           END-EVALUATE
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-HKN-ERR
      *            保険エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1028"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "1032"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200232-ERR-HEN-SEC
               MOVE    "61"            TO  WRK-ERRCD
           END-IF
           .
       20021-HKN-CHEK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為登録処理
      *****************************************************************
       20021-WKSRYACT-SYORI-SEC        SECTION.
      *
           INITIALIZE                  ORAPI021SUB1V3AREA
           MOVE    SPA-HKNCOMBINUM     TO  ORAPISUB01-HKNCOMBI
      *
           PERFORM VARYING     IDX     FROM    1   BY    1
                   UNTIL      (IDX     >   COB-ZAI-INMAX )
                        OR    (MDCREQ-MDC-CLASS (IDX)   =   SPACE)
               MOVE    MDCREQ-MDC-CLASS (IDX)
                                       TO  ORAPISUB01-MDC-CLASS
                                                                (IDX)
               MOVE    MDCREQ-MDC-CLASS-NUMBER (IDX)
                                       TO  ORAPISUB01-MDC-CLASS-NUMBER
                                                                (IDX)
      *        回数ゼロは１とする
               INITIALIZE                      ORCSNUMAREA
               MOVE    MDCREQ-MDC-CLASS-NUMBER (IDX)  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC             =   ZERO  )
                 AND  (SNUM-OUTNUM         =   ZERO  )
                   MOVE    "1"         TO  ORAPISUB01-MDC-CLASS-NUMBER
                                                                (IDX)
               END-IF
      *
      *        診療内容
               MOVE    ZERO                TO  IDY2
               PERFORM VARYING     IDY     FROM    1   BY    1
                       UNTIL      (IDY     >   40   )
                 IF      MDCREQ-PRSCRPT-CODE  (IDX IDY)
                                           NOT =   SPACE
                   ADD     1               TO  IDY2
                   MOVE    MDCREQ-PRSCRPT-CODE (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-CODE
                                                           (IDX IDY2)
                   MOVE    MDCREQ-PRSCRPT-NAME (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-NAME
                                                           (IDX IDY2)
      *           コメントなど埋め込み値
                   MOVE    ZERO                 TO  FLG-SURYO
                   IF     (MDCREQ-PRSCRPT-CODE (IDX IDY)(1:2)
                                            =   "84"    )
                      OR  (MDCREQ-PRSCRPT-CODE (IDX IDY)(1:4)
                                            =   "0084"  )
                      OR  (MDCREQ-PRSCRPT-CODE (IDX IDY)(1:3)
                                            =   "001"  )
                      OR  (MDCREQ-PRSCRPT-CODE (IDX IDY)(1:2)
                                            =   "85"  )
                      OR  (MDCREQ-PRSCRPT-CODE (IDX IDY)(1:3)
                                            =   "831"  )
                      OR  (MDCREQ-PRSCRPT-CODE (IDX IDY)(1:3)
                                            =   "880"  )
                      OR  (MDCREQ-PRSCRPT-CODE (IDX IDY)(1:7)
                                            =   "0992081"  )
                       IF     (MDCREQ-PRSCRPT-NUMBER (IDX IDY)
                                               NOT =   SPACE )
                           PERFORM 200321-INPUTCHI-HEN-SEC
                           MOVE    1               TO  FLG-SURYO
                       END-IF
                   END-IF
                   IF      FLG-SURYO       =   ZERO
                       MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)
                                       TO  ORAPISUB01-PRSCRPT-NUMBER
                                                           (IDX IDY2)
      *
      *                数量ゼロは１とする
                       INITIALIZE                      ORCSNUMAREA
                       MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)
                                                   TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
      *                数量ゼロは１とする
                       IF     (SNUM-RC          =   ZERO  )
                         AND  (SNUM-OUTNUM      =   ZERO  )
                           MOVE    "1"
                                       TO  ORAPISUB01-PRSCRPT-NUMBER
                                                           (IDX IDY2)
                       END-IF
                   END-IF
      *            自費コードの金額(NUMBERからの反映なし)
      *             MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)
      *                                 TO  ORAPISUB01-PRSCRPT-MONEY
      *                                                     (IDX IDY)
                   MOVE    MDCREQ-CONTKBN (IDX IDY)
                                       TO  ORAPISUB01-CONTKBN
                                                           (IDX IDY2)
                   MOVE    MDCREQ-NAIFKBN (IDX IDY)
                                       TO  ORAPISUB01-NAIFKBN
                                                           (IDX IDY2)
      *            フィルム分画数(NUMBERからの反映なし)
      *             MOVE    MDCREQ-PRSCRPT-FILMNUM (IDX IDY)
      *                                 TO  ORAPISUB01-PRSCRPT-FILMNUM
      *                                                     (IDX IDY)
      *            般名記載区分編集
                   IF      (MDCREQ-PRSCRPT-CODE  (IDX IDY)(1:1)
                                           =   "6" )
                      AND  (MDCREQ-GENERIC-FLG (IDX IDY)
                                           =   "yes"
                                           OR  "no"   )
                      EVALUATE    MDCREQ-GENERIC-FLG (IDX IDY) 
                      WHEN     "yes"
      *                   一般名記載
                          ADD      1           TO  IDY2
                          MOVE    "099209908"
                                           TO  ORAPISUB01-PRSCRPT-CODE
                                                            (IDX IDY2)
                      WHEN     "no"
      *                   銘柄名記載
                          ADD      1           TO  IDY2
                          MOVE    "099209907"
                                           TO  ORAPISUB01-PRSCRPT-CODE
                                                            (IDX IDY2)
                       END-EVALUATE
      *                次行が銘柄名記載などは削除
                       IF     (IDY             <   40  )
                         AND  (MDCREQ-PRSCRPT-CODE  (IDX IDY + 1 )
                                               =    "099209907"
                                               OR   "099209908"  )
                           ADD     1           TO  IDY
                       END-IF
                   END-IF
                 END-IF
               END-PERFORM
           END-PERFORM
      *
           MOVE    SPACE               TO  ORAPISUB01-SYORIKBN
           MOVE    SPA-HKNCOMBINUM     TO  ORAPISUB01-HKNCOMBI
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORAPI021SUB1V3"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORAPI021SUB1V3AREA
                                       SPA-K02
      *
      *    エラー処理
           IF      ORAPISUB01-ERRCD    NOT =   SPACE
               MOVE    ORAPISUB01-ERRCD    TO  WRK-ERRCD
               MOVE    ORAPISUB01-ERRMSG   TO  WRK-ERRMSG
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX    FROM    1   BY  1
                   UNTIL       (IDX        >   100 )
                           OR  (ORAPISUB01-IN-ERR (IDX)
                                           =   SPACE )
      *        エラーのみ返却 
               IF      ORAPISUB01-IN-ERR (IDX)(1:1)
                                           =   "W"
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    ORAPISUB01-IN-SRYCD (IDX)
                                       TO  ORAPICSV-MD-SRYCD (IDY)
                   MOVE    ORAPISUB01-IN-ERR (IDX)
                                       TO  ORAPICSV-MD-ERRCD (IDY)
                   MOVE    ORAPISUB01-IN-ERRMSG (IDX)
                                       TO  ORAPICSV-MD-ERRMSG1 (IDY)
                   MOVE    ORAPISUB01-IN-POS (IDX)
                                       TO  ORAPICSV-MD-POS  (IDY)
                   MOVE    ORAPISUB01-IN-LINE (IDX)
                                       TO  ORAPICSV-MD-LINE  (IDY)
      *
                   IF      ORAPISUB01-IN-SRYCD (IDX) NOT =   SPACE
                       MOVE    ORAPISUB01-IN-SRYCD (IDX)
                                          TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
                           MOVE    TNS-NAME 
                                       TO  ORAPICSV-MD-SRYNAME  (IDY)
                       END-IF
                   END-IF
      *
                  IF      WRK-ERRCD       =   SPACE
                       MOVE    "62"            TO  WRK-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-ERRCD       TO  ORAPICSV-ERRCD
           MOVE    WRK-ERRMSG      TO  ORAPICSV-ERRMSG
           .
       20021-WKSRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    コメント値編集処理
      *****************************************************************
       200321-INPUTCHI-HEN-SEC         SECTION.
      *
      *    コメント842対応
           IF     (MDCREQ-PRSCRPT-CODE  (IDY IDY)(1:3)
                                       =   "842"   )
      *        入力値はそのまま（１）へ
               MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)
                                       TO  ORAPISUB01-ATAI
                                                       (IDX IDY2 1)
               GO      TO  200321-INPUTCHI-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-INPUTCHI-G
           MOVE    1                   TO  IDX-I1
           MOVE    ZERO                TO  IDX-I2
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL      (IDX-I   >   40  )   OR
                              (IDX-I1  >   5   )
               IF      MDCREQ-PRSCRPT-NUMBER (IDX IDY)(IDX-I:1)
                                           =   "-"
                   ADD     1               TO  IDX-I1
                   MOVE    ZERO            TO  IDX-I2
               ELSE
                   ADD     1               TO  IDX-I2
                   MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)(IDX-I:1)
                                           TO  WRK-INPUTCHI
                                                 (IDX-I1)(IDX-I2:1)
               END-IF
      *R04.4
      *       880 の時、４番目（検査値）で最後
              IF    (MDCREQ-PRSCRPT-CODE (IDY IDY)(1:3)
                                              =   "880"  )
                AND (IDX-I1               =   4      )
                   COMPUTE IDX-I2         =    IDX-I   +  1
                   MOVE    MDCREQ-PRSCRPT-NUMBER (IDX IDY)(IDX-I2:)
                                           TO  WRK-INPUTCHI
                                                 (IDX-I1)
                   MOVE    5          TO  IDX-I1
              END-IF
      ******
      *
           END-PERFORM
      *
           PERFORM VARYING     IDX-I   FROM    1   BY  1
                   UNTIL       IDX-I   >   5
               MOVE    WRK-INPUTCHI(IDX-I)
                                      TO   ORAPISUB01-ATAI
                                               (IDX IDY2 IDX-I)
           END-PERFORM
      *
           .
       200321-INPUTCHI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    処方箋情報チェック処理
      *****************************************************************
       20013-SHOHOU-CHK-SEC           SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   10 )
                        OR   ( WRK-ERRCD   NOT =   SPACE )
               IF      MDCREQ-PRE-NAME (IDX)   NOT =   SPACE
      *
      *            全角チェック        
                   MOVE    MDCREQ-PRE-NAME (IDX)   TO  WRK-MAE-INPUT
                   MOVE    180                     TO  WRK-LEN
                   PERFORM 801-ZENKAKUCHK-SEC
                   IF      FLG-ERR             =   1
                       MOVE    "65"            TO  WRK-ERRCD
                       STRING "処方箋情報の医薬品名称に誤りがあります"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
                       END-STRING
                       MOVE    MDCREQ-PRE-NAME (IDX)
                                               TO  WRK-ERRMSG2
      *             ELSE
      *                 MOVE    WRK-ATO-INPUT   TO  MDCREQ-PRE-NAME
      *                                                  (IDX)
                   END-IF
      *            コメントがあること
                   IF     (MDCREQ-PRE-COMMENT (IDX)    =   SPACE)
                     AND  (WRK-ERRCD           =   SPACE )
                       MOVE    "66"            TO  WRK-ERRCD
                       STRING "処方箋情報のコメントが未設定です。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
                       END-STRING
                   END-IF
               END-IF
               IF      MDCREQ-PRE-COMMENT (IDX)    NOT =   SPACE
      *            全角変換                   
                   MOVE    MDCREQ-PRE-COMMENT (IDX)
                                                  TO  WRK-MAE-INPUT
                   MOVE    300                    TO  WRK-LEN
                   PERFORM 801-ZENKAKUCHK-SEC
                   IF     (FLG-ERR             =   1 )
                     AND  (WRK-ERRCD           =   SPACE )
                       MOVE    "67"            TO  WRK-ERRCD
                       STRING "処方箋情報のコメント内容に"
                              "誤りがあります。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
                       END-STRING
                       MOVE    MDCREQ-PRE-COMMENT (IDX)
                                               TO  WRK-ERRMSG2
                   END-IF
               END-IF
           END-PERFORM
      *2024/01
      *    リフィル回数毎
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL     ( IDY     >   4    )
                        OR   ( WRK-ERRCD   NOT =   SPACE )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL     ( IDX     >   10 )
                            OR   ( WRK-ERRCD   NOT =   SPACE )
               IF      MDCREQ-REF-PRE-NAME (IDY IDX)   NOT =   SPACE
      *
      *            全角チェック        
                   MOVE    MDCREQ-REF-PRE-NAME (IDY IDX)
                                                   TO  WRK-MAE-INPUT
                   MOVE    180                     TO  WRK-LEN
                   PERFORM 801-ZENKAKUCHK-SEC
                   IF      FLG-ERR             =   1
                       MOVE    "65"            TO  WRK-ERRCD
                       STRING "処方箋情報の医薬品名称に誤りがあります"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
                       END-STRING
                       MOVE    MDCREQ-REF-PRE-NAME (IDY IDX)
                                               TO  WRK-ERRMSG2
      *             ELSE
      *                 MOVE    WRK-ATO-INPUT   TO  MDCREQ-PRE-NAME
      *                                                  (IDX)
                   END-IF
      *            コメントがあること
                   IF     (MDCREQ-REF-PRE-COMMENT (IDY IDX) =   SPACE)
                     AND  (WRK-ERRCD           =   SPACE )
                       MOVE    "66"            TO  WRK-ERRCD
                       STRING "処方箋情報のコメントが未設定です。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
                       END-STRING
                   END-IF
               END-IF
               IF      MDCREQ-REF-PRE-COMMENT (IDY IDX) NOT =   SPACE
      *            全角変換                   
                   MOVE    MDCREQ-REF-PRE-COMMENT (IDY IDX)
                                                  TO  WRK-MAE-INPUT
                   MOVE    300                    TO  WRK-LEN
                   PERFORM 801-ZENKAKUCHK-SEC
                   IF     (FLG-ERR             =   1 )
                     AND  (WRK-ERRCD           =   SPACE )
                       MOVE    "67"            TO  WRK-ERRCD
                       STRING "処方箋情報のコメント内容に"
                              "誤りがあります。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
                       END-STRING
                       MOVE    MDCREQ-REF-PRE-COMMENT (IDY IDX)
                                               TO  WRK-ERRMSG2
                   END-IF
               END-IF
               END-PERFORM
           END-PERFORM
           .
       20013-SHOHOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面用スパ編集処理
      *****************************************************************
       20011-INPUTSYORI-SEC           SECTION.
      *
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  SPA-GMN-IDX
           MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  SPA-ERRCD
      *    診療行為処理
           INITIALIZE                  SPA-SCR-REC
      *
      *     MOVE    WRK-HKNCOMBI        TO  SPA-HKNCOMBINUM
      *     MOVE    WRK-SRYKA           TO  SPA-SRYKA
      *     MOVE    WRK-PTID            TO  SPA-PTID
      *     MOVE    WRK-PTNUM           TO  SPA-PTNUM
      *
      *    中途データ展開処理
           PERFORM 200221-SAIHEN-HEN-SEC
      *
      *     MOVE    SPACE               TO  WRK-GENECHG-KBN
      *    一般名記載とする
      **   MOVE    "1"                 TO  SPA-GENERICFLG
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                    OR     (SPA-ERRCD          NOT =   SPACE )
               EVALUATE    SPA-GMN-INPUTCD (IDX)(1:1)
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
                   WHEN    "#"
                   WHEN    "$"
                       CONTINUE
                   WHEN    OTHER
      *                点数マスタチェック（再表示編集時にエラー）
                       IF      SPA-COM-CUR (IDX)   NOT =   SPACE
                           PERFORM 200222-KIHONCHK-SEC
                       END-IF
      *                電話再診の加算処理なし(初診))
                       IF     (SPA-COM-SRYKBN(IDX) =   "12"
                                                   OR  "11"  )
                         AND  (SPA-COM-INPUTCD(IDX)(1:1) =  "1")
                           MOVE    "1"         TO  SPA-COM-KSNAUTO(IDX)
                       END-IF
      *!!
      *                後発品変更不可
      *                 IF      SPA-COM-INPUTCD (IDX)   =   "099209910"
      *                     MOVE    "1"             TO  WRK-GENECHG-KBN
      *                 END-IF
      *                後発品変更可
      *                 IF      SPA-COM-INPUTCD (IDX)   =   "099209911"
      *                     MOVE    "2"             TO  WRK-GENECHG-KBN
      *                 END-IF
      *                選択式コメントの自動表示なし
                       IF      (SPA-COM-INPUTCD(IDX)(1:1)  =   "1"  )
                          AND  (SPA-COM-RECEKISAI (IDX)    =   "1"  )
                           MOVE    "1"         TO   SPA-COM-RECEAUTO
                                                              (IDX)
                       END-IF
      *
               END-EVALUATE
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 200232-ERR-HEN-SEC
               MOVE    "62"            TO  WRK-ERRCD
           END-IF
      *
      *    診療行為、計計算処理、編集
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
               PERFORM 510-TBLHEN-SYORI-SEC
      *
               IF     (SPA-ERRCD       NOT =   SPACE )
                  AND (WRK-ERRCD           =   SPACE )
                   PERFORM 200232-ERR-HEN-SEC
                   MOVE    "62"            TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    包括の判定　（なし）
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )  AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
      *            治験以外
             AND  (SPA-CHIKENKBN   NOT =   "1"   )
               IF     (SPA-HKTSANTEI-FLG   =   1     )  OR
                      (SPA-HKTSANTEI-08    =   1     )  OR
                      (SPA-NYUHKTCHK       =   1     )
      ****????     PERFORM 5109-HKTSANTEI-CHK-SEC
                   CONTINUE
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (WRK-ERRCD           =   SPACE )
      *        更新前自動算定・更新追加
               PERFORM 530-TBLCHI-SYORI-SEC
      *        電子処方箋作成処理
               IF     (SPA-ERRCD           =   SPACE )
                 AND  (WRK-ERRCD           =   SPACE )
                   PERFORM 600-EPRECSV-SYORI-SEC
              END-IF
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )
              AND (WRK-ERRCD           =   SPACE )
               PERFORM 200232-ERR-HEN-SEC
           END-IF
      *
      *    データ削除処理
           PERFORM 700-FILE-DELETE-SEC
           .
       20011-INPUTSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面ＳＰＡ編集処理
      *****************************************************************
       200221-SAIHEN-HEN-SEC           SECTION.
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
     ******MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *R02.4
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
      *
      *    診療算定回数編集
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "3"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    診察料自動発生区分
      *    MOVE    SPA-INITAUTO-FLG    TO  WRK-SPA-INITAUTO-FLG
      *    診察料自動発生区分なし
           MOVE    ZERO                TO  SPA-INITAUTO-FLG
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02"               TO  ORCSCWKSRY-PG
      *    処理区分（１：中途終了展開）
           MOVE    1                   TO  ORCSCWKSRY-SYORIFLG
      *    追加開始位置
           MOVE    ZERO                TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    診察料自動発生区分もとへ
      **   MOVE    WRK-SPA-INITAUTO-FLG    TO  SPA-INITAUTO-FLG
      *
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  SPA-ERRCD
           END-IF
      *
      *
           .
       200221-SAIHEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *   点数マスタチェック処理
      *****************************************************************
       200222-KIHONCHK-SEC            SECTION.
      *
      *    点数マスタ編集
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    IDX                 TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD(IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)
                                                   TO  SPA-ERRCD
               END-IF
           END-PERFORM
           IF     (SPA-ERRMSG2         =   SPACE )
              AND (LNK-SC92-ERRMSG NOT =   SPACE )
               MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
           END-IF
      *
      *    警告は登録対象
           IF      SPA-ERRCD(1:1)      =   "K"
               MOVE    "1"                 TO  SPA-COM-KZMFLG (IDX)
               MOVE    "2"                 TO  SPA-COM-CUR  (IDX)
               MOVE    ZERO                TO  FLG-ERR
               PERFORM 3001-KEIMSGSYORI-SEC
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           NOT =   SPACE )
               MOVE    "1"             TO  SPA-COM-CUR (IDX)
           END-IF
           .
       200222-KIHONCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SYORI-SEC            SECTION.
      *
      *すべて院外とする
           MOVE     "1"                TO  SPA-INGAIKBN
      *2024/01
      *    院内
           IF      MDCREQ-REQUEST-NUMBER   =   "06"
               MOVE    "0"                 TO  SPA-INGAIKBN
           END-IF
      *
      *    腫瘍マーカー一覧展開しないとする
           MOVE    ZERO                TO  SPA-AKUSEIHYOFLG
      *H30.9
      *    選択式コメント一覧展開しないとする
           MOVE    ZERO                TO  SPA-RECEKISAIFLG
      *    外来管理加算チェックなし
           MOVE    "0"                 TO  SPA-GAIKANRIKBN-CHK
      *    診療行為、計計算処理、編集
           MOVE    ZERO                TO  FLG-END-SYORI
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-KENSADEL
           PERFORM
               UNTIL   (FLG-END-SYORI  =   1  )
               OR      (SPA-ERRCD      NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-KIDCD
               MOVE    SPACE               TO  SPA-ERRMSG2
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "1"                 TO  ORCSC100-KBN
               MOVE    SPACE               TO  ORCSC100-TOROKU
               MOVE    ZERO                TO  ORCSC100-INIT
               MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
               MOVE    ZERO                TO  ORCSC100-RIHTEI
               CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    ZERO                TO  FLG-KENSADEL
      *
      *        初診の同日受診はワーングとする
               IF     (SPA-KIDCD       =   "0101" )
                   PERFORM 5103-KIDCD0101-CHK-SEC
               END-IF
      *
      *        確認メッセージはすべてエラーとする
               IF      SPA-KIDCD        NOT =   SPACE
                   PERFORM 5101-KIDCD-CHK-SEC
                   MOVE    1                   TO  FLG-END-SYORI
               ELSE
      *            警告は登録対象
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    2                   TO  FLG-ERR
                       PERFORM 3001-KEIMSGSYORI-SEC
                       MOVE    SPACE               TO  SPA-ERRCD
                       MOVE    ZERO                TO  FLG-END-SYORI
                   ELSE
      *                 IF      WRK-TOROKU      =   SPACE
      *                    登録前チェックへ
      *                     MOVE    "1"             TO  WRK-TOROKU
      *                 ELSE
                           MOVE    1               TO  FLG-END-SYORI
      *                 END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       510-TBLHEN-SYORI-EXT.
           EXIT.
     
      *****************************************************************
      *    初診が同日受診はワーング処理
      *****************************************************************
       5103-KIDCD0101-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                    OR     (FLG-CHK                =   1    )
               IF      (SPA-COM-INPUTCD (IDX)(1:1) =   "1" )
                  AND  (SPA-COM-DATAKBN (IDX)      =   "1" )
                  AND  (SPA-COM-SRYKBN  (IDX)      =   "11")
                  AND  (SPA-COM-KZMFLG  (IDX)      =   "1" )
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               GO  TO  5103-KIDCD0101-CHK-EXT
           END-IF
      *    初診時は警告とする
           MOVE    "KAP1"              TO  SPA-KIDCD
           MOVE    SPACE               TO  WRK-KEIMSG
           STRING  "すでに当日診察料を算定しています。"
                   "同日再診へ変更して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KEIMSG
           END-STRING
           ADD     1                   TO  IDX-KEI
           IF      IDX-KEI             <=  400
               MOVE    SPA-KIDCD       TO  TBL-KEIERRCD (IDX-KEI)
               MOVE    WRK-KEIMSG      TO  TBL-KEIMSG   (IDX-KEI)
           END-IF
           MOVE    ZERO                TO  FLG-END-SYORI
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  WRK-KEIMSG
           MOVE    1                   TO  FLG-KEIMSG
           .
       5103-KIDCD0101-CHK-EXT.
           EXIT.
      *****************************************************************
      *    確認メッセージ（入力）処理
      *****************************************************************
       5101-KIDCD-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    SPA-KIDCD
      *        同日再診へ
               WHEN    "0101"
               WHEN    "0109"
                   MOVE    "AP01"              TO  SPA-ERRCD
      *        外来管理加算削除
               WHEN    "0102"
                   MOVE    "AP02"              TO  SPA-ERRCD
      *        同一検査削除へ
               WHEN    "0160"
                   MOVE    "0064"              TO  SPA-ERRCD
      *
               WHEN    OTHER
                   MOVE    SPA-KIDCD       TO  WRK-MD-ERRCD
                   IF      WRK-ERRCD           =   SPACE
                       MOVE    "62"                TO  WRK-ERRCD
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       5101-KIDCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    警告メッセージ処理
      *****************************************************************
       3001-KEIMSGSYORI-SEC          SECTION.
      *
           MOVE    "K02"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
      *    全角変換
           MOVE    SPA-ERRMSG          TO  WRK-MEISYO
           PERFORM 800-ZENKAKUHEN-SEC
           MOVE    WRK-MEISYO          TO  WRK-ERRMSG1
      *
           MOVE    SPACE               TO  WRK-KEIMSG
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    SPACE               TO  WRK-ERRMSG2
           ELSE
               MOVE    SPA-ERRMSG2         TO  WRK-MEISYO
               PERFORM 800-ZENKAKUHEN-SEC
               MOVE    WRK-MEISYO          TO  WRK-ERRMSG2
           END-IF
           IF      SPA-ERRCD(1:1)      =   "K"
      *        警告メッセージ（保存のみ）
               MOVE    WRK-ERRMSG2         TO  WRK-KEIMSG1
               MOVE    WRK-ERRMSG1         TO  WRK-KEIMSG2
           END-IF
      *
      *
      *
      **   STRING  WRK-ERRMSG1         DELIMITED  BY  SPACE
      *            WRK-ERRMSG2         DELIMITED  BY  SPACE
      *                INTO                    WRK-ERRMSG
      **   END-STRING
      *
      *    警告行判定
           MOVE    SPACE               TO  WRK-ERR-SRYCD
           PERFORM VARYING IDX6    FROM    1   BY  1
                   UNTIL  (IDX6       >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX6)  =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX6)  =   SPACE ))
                   OR     ( WRK-ERR-SRYCD      NOT =   SPACE  )
               IF     (SPA-COM-CUR    (IDX6)   =   "2"
                                               OR  "1" )
                   PERFORM 30011-KEIMSGHEN-SEC
                   MOVE    SPA-COM-INPUTCD (IDX6)  TO  WRK-ERR-SRYCD
                   MOVE    SPACE                   TO  SPA-COM-CUR(IDX6)
               END-IF
           END-PERFORM
      *
           IF      WRK-ERR-SRYCD       =   SPACE
               ADD     1                   TO  IDX-KEI
               IF      IDX-KEI             <=  400
                   MOVE    SPA-ERRCD       TO  TBL-KEIERRCD (IDX-KEI)
                   MOVE    WRK-KEIMSG      TO  TBL-KEIMSG   (IDX-KEI)
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  WRK-ERR-SRYCD
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           MOVE    1                   TO  FLG-KEIMSG
           .
       3001-KEIMSGSYORI-EXIT.
           EXIT.
      *
      *****************************************************************
      *    警告メッセージ編集処理
      *****************************************************************
       30011-KEIMSGHEN-SEC     SECTION.
      *
           ADD     1                   TO  IDX-KEI
           IF      IDX-KEI             <=  400
               MOVE    SPA-ERRCD           TO  TBL-KEIERRCD (IDX-KEI)
               MOVE    WRK-KEIMSG          TO  TBL-KEIMSG   (IDX-KEI)
               MOVE    SPA-COM-INPUTCD (IDX)
                                           TO  TBL-KEI-SRYCD(IDX-KEI)
               IF      SPA-COM-INPUTCD (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD (IDX)(1:9)
                                           TO  TBL-KEI-SRYCD(IDX-KEI)
               END-IF
      *        警告位置
               MOVE    IDX-KEI             TO  SPA-COM-GYOU (IDX)
           END-IF
           .
       30011-KEIMSGHEN-EXIT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       200232-ERR-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "AP01"
      *            同日再診へ
                   STRING  "すでに当日診察料を算定しています。"
                           "同日再診へ変更して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
                   MOVE    SPACE               TO  SPA-ERRMSG2
               WHEN    "AP02"
      *            外来管理加算削除
                   STRING  "外来管理加算は算定できません。"
                           "削除して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
                   MOVE    SPACE               TO  SPA-ERRMSG2
               WHEN    OTHER
                   MOVE    "K02"               TO  LNK-SC96-PG
                   CALL    "ORCSC96"           USING    SPA-AREA
                                                        ORCSC96AREA
           END-EVALUATE
      *
      *    全角変換
           MOVE    SPA-ERRMSG          TO  WRK-MEISYO
           PERFORM 800-ZENKAKUHEN-SEC
           MOVE    WRK-MEISYO          TO  WRK-ERRMSG1
      *
           MOVE    SPACE               TO  WRK-MD-ERRMSG
           IF      SPA-ERRMSG2         =   SPACE
               MOVE    WRK-ERRMSG1         TO  WRK-MD-ERRMSG1
           ELSE
      *        全角変換
               MOVE    SPA-ERRMSG2         TO  WRK-MEISYO
               PERFORM 800-ZENKAKUHEN-SEC
               MOVE    WRK-MEISYO          TO  WRK-ERRMSG2
      *
               MOVE    WRK-ERRMSG2         TO  WRK-MD-ERRMSG1
               MOVE    WRK-ERRMSG1         TO  WRK-MD-ERRMSG2
           END-IF
           MOVE    SPA-ERRCD           TO  WRK-MD-ERRCD
      *
      *    行判定
           MOVE    SPACE           TO  WRK-ERR-SRYCD
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                   OR     ( WRK-ERR-SRYCD      NOT =   SPACE  )
               IF     (SPA-COM-CUR    (IDX)    =   "1" )
                  AND (SPA-COM-KZMFLG (IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG2(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG3(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG4(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG5(IDX)    =   SPACE )
                  AND (SPA-COM-KZMFLG6(IDX)    =   SPACE )
                  AND (SPA-COM-KINKIFLG(IDX)   =   SPACE )
                  AND (SPA-COM-TUKIJGNFLG(IDX) =   SPACE )
      *
                   MOVE    SPA-COM-INPUTCD (IDX)   TO  WRK-ERR-SRYCD
                   IF      SPA-COM-INPUTCD (IDX)   =   SPACE
                       MOVE    SPA-GMN-INPUTCD (IDX)   TO  WRK-ERR-SRYCD
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-ERR-SRYCD       TO  WRK-MD-SRYCD
      *
      *    複数科保険行
           IF      WRK-ERR-SRYCD(1:1)  =   "#"
                                       OR  "$"
               MOVE    SPACE               TO  WRK-MD-SRYCD
           END-IF
      *
           IF      WRK-ERRCD           =   SPACE
               MOVE    "62"                TO  WRK-ERRCD
               MOVE    SPACE               TO  WRK-ERRMSG
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    SPACE               TO  SPA-ERRMSG
           .
       200232-ERR-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為自動算定・算定チェック処理
      *****************************************************************
       530-TBLCHI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-END-SYORI2
           PERFORM
               UNTIL    FLG-END-SYORI2 =   1
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                  ORCSC100AREA
               MOVE    "4"                 TO  ORCSC100-KBN
               MOVE    "2"                 TO  ORCSC100-TOROKU
               MOVE    ZERO                TO  ORCSC100-INIT
               MOVE    ZERO                TO  ORCSC100-KENSADEL
               MOVE    ZERO                TO  ORCSC100-RIHTEI
               CALL    "ORCSC100"          USING
                                           ORCSC100AREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *        確認メッセージはすべてエラー
               IF      SPA-KIDCD        NOT =   SPACE
                   MOVE    "OK"             TO  SPA-KID1-FLG
                   PERFORM 5101-KIDCD-CHK-SEC
                   MOVE    1                   TO  FLG-END-SYORI2
               ELSE
      *        警告は登録対象
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    ZERO                TO  FLG-ERR
                       PERFORM 3001-KEIMSGSYORI-SEC
                       MOVE    ZERO                TO  FLG-END-SYORI2
                   ELSE
                       MOVE    1                   TO  FLG-END-SYORI2
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        ワーク診療行為データ出力処理
               PERFORM 45101-SNRYO-SYORI-SEC
           END-IF
           .
       530-TBLCHI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45101-SNRYO-SYORI-SEC              SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
      *    パラメタに登録
           MOVE    "3"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02"               TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    DBエラー
           IF      SPA-ERRCD           =   "1093"
                                       OR  "9999"
               MOVE    "81"            TO  WRK-ERRCD
               MOVE    "一時ファイル出力エラー"
                                       TO  WRK-ERRMSG
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
           .
       45101-SNRYO-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    電子処方箋ＣＳＶ作成処理
      *****************************************************************
       600-EPRECSV-SYORI-SEC   SECTION.
      *
      *    処方せん発行の有無
      *     MOVE    ZERO                TO  FLG-SYOHOU-NAI
      *
           MOVE    ZERO                TO  FLG-SYOHOU-ARI
           MOVE    ZERO                TO  FLG-YAKUZAI-ARI
           INITIALIZE                  ORCHC19AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-GMN-MAX  )
      *      コロナトクレイ対応判定
             IF     (SPA-COM-SRYSYUKBN(IDX)    =   "212"
                                               OR  "210"
                                               OR  "148" )
               AND  (SPA-COM-YAKUHYO (IDX) NOT =   SPACE )
               AND  (SPA-COM-INPUTCD (IDX)     =  "820000094"
                                               OR "630010011")
               AND  (SPA-COM-INPUTCD (IDX + 1)(1:3)
                                               =   "059" )
               AND  (SPA-COM-HKNTEKKBN (IDX + 1)   =   9 )
      *        院外投薬ありとする
               MOVE    1                  TO  FLG-SYOHOU-ARI
             END-IF
      ****
      *    薬評以外
             IF     (SPA-COM-YAKUHYO (IDX)     =   SPACE )
      *        薬剤チェック
               IF     (SPA-COM-SRYKBN(IDX)     =   "21"  OR  "22"  OR
                                                   "23"   )  AND
                     ((SPA-COM-INPUTCD(IDX)(1:1)   =   "6" )  OR
                      (SPA-COM-INPUTCD(IDX)(1:3)   =   "059" ))
                   MOVE    1                   TO  FLG-YAKUZAI-ARI
               END-IF
      *
      *        院外処方
               IF    ((SPA-COM-SRYKBN(IDX)     =   "21"  OR  "22"  OR
                                                   "23"   )  AND
                      (SPA-COM-ZAIEND(IDX)     =   "1"    )  AND
                      (SPA-COM-TENSU (IDX)     =   ZERO   )  AND
                      (FLG-YAKUZAI-ARI         =   1      ))
      *            薬剤のみの時、院内分とする
                   IF      SPA-COM-SRYSYUKBN(IDX)  =  "213"  OR
                                                      "223"  OR
                                                      "233"
      *H28.4           院内投薬
                                                   OR "211"
                                                   OR "221"
                                                   OR "231"
                                                   OR "291"
                                                   OR "294"
                                                   OR "297"
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-SYOHOU-ARI
                   END-IF
               END-IF
      *        院外の診療種別の時、院外とする
               IF      SPA-COM-SRYSYUKBN(IDX)  =  "212"  OR
                                                  "222"  OR
                                                  "232"  OR
                                                  "292"
      *            外用臨時
                                               OR "295"
      *            頓服臨時
                                               OR "298"
                   MOVE    1               TO  FLG-SYOHOU-ARI
               END-IF
      *        在宅注射院外処方
               IF     (SPA-COM-SRYKBN(IDX)     =   "14"  )
                   IF      SPA-COM-SRYSYUKBN(IDX)  =   "148"  OR
                                                       "149"
      *                院外分
                       MOVE    1                   TO  FLG-SYOHOU-ARI
                   END-IF
               END-IF
             END-IF
             IF      SPA-COM-ZAIEND(IDX)     =   "1" 
                 MOVE    ZERO            TO  FLG-YAKUZAI-ARI
             END-IF
      *
           END-PERFORM
      *
           INITIALIZE                  ORCHC02EPREAREA
      *    電子処方箋情報編集、チェック
           PERFORM 6001-PRE-HENSYU-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO  TO  600-EPRECSV-SYORI-EXT
           END-IF
      *
      *    電子処方箋ＣＶＳデータ作成
      **   IF     (FLG-SYOHOU-ARI          =   1   )
      *        一時ディレクトリ作成
      *         INITIALIZE                  STEMPDIR-AREA
      *         CALL    "ORCSTEMPDIR"       USING
      *                                     STEMPDIR-AREA
      *
      *        電子処方箋ＣＶＳデータ作成
               INITIALIZE                  ORCHC19AREA
      *        確定時（院外）
               IF      MDCREQ-REQUEST-NUMBER   =   "05"
      *            院外
                   MOVE    "E"                  TO  ORCHC19-KBN
               ELSE
      *2024/01     院内
                   MOVE    "F"                 TO  ORCHC19-KBN
               END-IF
      *2025/04
      *        チェック用（院外）
               IF      MDCREQ-REQUEST-NUMBER   =   "07"
      *            院外
                   MOVE    "E"                 TO  ORCHC19-KBN
                   MOVE    "CHK"               TO  ORCHCEPRE-CHKKBN
               END-IF
      *
               MOVE    "API"               TO  ORCHCEPRE-SYORIKBN
               CALL    "ORCHC02QEPRE02"    USING   SPA-AREA
                                                   ORCHC19AREA
                                                   ORCHC02EPREAREA
      *     END-IF
      *
           IF      ORCHCEPRE-ERRCD  NOT =   SPACE
               MOVE    ORCHCEPRE-ERRCD      TO  WRK-ERRCD
               MOVE    ORCHCEPRE-ERRMSG     TO  WRK-ERRMSG
           ELSE
               IF      ORCHCEPRE-DATAMAX    =   ZERO
                   MOVE    "80"             TO  WRK-ERRCD
                   MOVE    "ＣＳＶが作成できませんでした"
                                            TO  WRK-ERRMSG
               END-IF
           END-IF
           MOVE    ORCHCEPRE-CSVDATA-AREA
                                       TO  ORAPICSV-CSVDATA-AREA
      *
           .
       600-EPRECSV-SYORI-EXT.
           EXIT.
      *
      *2024/01
      *****************************************************************
      *    処方箋管理テーブル情報保存処理
      *****************************************************************
       6001-PRE-HENSYU-SEC        SECTION.
      *
      *    リフィル回数対応
           MOVE    ZERO                TO  IDX-P
      *2024/03
      *    引換番号または提供診療コメントあり
           IF      (MDCREQ-PRE-ACCESS-CODE NOT =   SPACE )
              OR   (MDCREQ-PRE-COMMENT (1) NOT =   SPACE )
               ADD     1                   TO  IDX-P
      *        処方箋発行形態
               MOVE    MDCREQ-PRE-ISSUESELECT
                                       TO  ORCHCEPRE-PRE-ISSUESELECT
                                                              (IDX-P)
      *        処方箋ID
               MOVE    MDCREQ-PRE-ID       TO  ORCHCEPRE-PRE-ID (IDX-P)
      *        引換番号
               MOVE    MDCREQ-PRE-ACCESS-CODE
                                           TO  ORCHCEPRE-ACCESS-CODE
                                                              (IDX-P) 
      *        リフィル回数
               IF      MDCREQ-REFILL-NUM       =   "2"
                                               OR  "3"
                   MOVE    MDCREQ-REFILL-NUM   TO  ORCHCEPRE-REFILL-NUM
                                                              (IDX-P)
               ELSE
                   MOVE    ZERO                TO  ORCHCEPRE-REFILL-NUM
                                                              (IDX-P)
               END-IF
      *        リフィル投与回数
               IF      MDCREQ-REFILL-NUM       =   "2"
                                               OR  "3"
                   MOVE    MDCREQ-REFILL-KAISU
                                           TO  WRK-REF-KAISU
                   PERFORM 2002181-REF-KAKSUHEN-SEC
                   IF      FLG-REF-ERR     =   1
                       MOVE    "63"            TO  WRK-ERRCD
                       MOVE    "リフィル処方日数が数値ではありません。"
                                               TO  WRK-ERRMSG
                   ELSE
                       MOVE    WRK-REF-KAISU
                                           TO  ORCHCEPRE-REFILL-KAISU
                                                              (IDX-P)
                   END-IF
               ELSE
                   MOVE    ZERO            TO  ORCHCEPRE-REFILL-KAISU
                                                              (IDX-P)
               END-IF
      *        処方箋コメント情報
               PERFORM VARYING     IDX     FROM   1  BY  1
                       UNTIL      (IDX     >     10  )
                   MOVE    MDCREQ-PRE-NAME (IDX)
                                      TO  ORCHCEPRE-PRE-NAME 
                                                    (IDX-P IDX)
                   MOVE    MDCREQ-PRE-COMMENT (IDX)
                                      TO  ORCHCEPRE-PRE-COMMENT
                                                    (IDX-P IDX)
               END-PERFORM
           END-IF
      *
           PERFORM VARYING     IDXR    FROM    1   BY  1
                   UNTIL      (IDXR            >   4   )
                         OR   (WRK-ERRCD   NOT =   SPACE )
      *2024/03
      *        引換番号または提供診療コメントあり
               IF      (MDCREQ-REF-PRE-ACCESS-CODE(IDXR)
                                               NOT =   SPACE )
                 OR    (MDCREQ-REF-PRE-COMMENT (IDXR 1)
                                               NOT =   SPACE )
                   ADD     1                   TO  IDX-P
      *            処方箋発行形態
                   MOVE    MDCREQ-REF-PRE-ISSUESELECT(IDXR)
                                       TO  ORCHCEPRE-PRE-ISSUESELECT
                                                              (IDX-P)
      *            処方箋ID
                   MOVE    MDCREQ-REF-PRE-ID(IDXR)
                                           TO  ORCHCEPRE-PRE-ID (IDX-P)
      *            引換番号
                   MOVE    MDCREQ-REF-PRE-ACCESS-CODE (IDXR)
                                           TO   ORCHCEPRE-ACCESS-CODE
                                                              (IDX-P) 
      *            リフィル回数
                   IF      MDCREQ-REF-REFILL-NUM(IDXR)     =   "2"
                                                           OR  "3"
                       MOVE    MDCREQ-REF-REFILL-NUM(IDXR)
                                           TO  ORCHCEPRE-REFILL-NUM
                                                              (IDX-P)
                   ELSE
                       MOVE    ZERO        TO  ORCHCEPRE-REFILL-NUM
                                                              (IDX-P)
                   END-IF
      *            リフィル投与回数
                   IF      MDCREQ-REF-REFILL-NUM(IDXR)     =   "2"
                                                           OR  "3"
                       MOVE    MDCREQ-REF-REFILL-KAISU (IDXR)
                                           TO  WRK-REF-KAISU
                       PERFORM 2002181-REF-KAKSUHEN-SEC
                       IF      FLG-REF-ERR     =   1
                           MOVE    "63"            TO  WRK-ERRCD
                           MOVE
                           "リフィル処方日数が数値ではありません。"
                                               TO  WRK-ERRMSG
                       ELSE
                           MOVE    WRK-REF-KAISU
                                           TO  ORCHCEPRE-REFILL-KAISU
                                                              (IDX-P)
                       END-IF
                   ELSE
                       MOVE    ZERO        TO  ORCHCEPRE-REFILL-KAISU
                                                              (IDX-P)
                   END-IF
      *            処方箋コメント情報
                   PERFORM VARYING     IDX     FROM   1  BY  1
                           UNTIL      (IDX     >     10  )
                       MOVE    MDCREQ-REF-PRE-NAME (IDXR IDX)
                                      TO  ORCHCEPRE-PRE-NAME 
                                                    (IDX-P IDX)
                       MOVE    MDCREQ-REF-PRE-COMMENT (IDXR IDX)
                                      TO  ORCHCEPRE-PRE-COMMENT
                                                    (IDX-P IDX)
                   END-PERFORM
      *            リフィル回数・投薬回数は重複しないこと
                   IF      ORCHCEPRE-REFILL-NUM (IDX-P) NOT =   SPACE
                       PERFORM VARYING     IDX     FROM  1  BY  1
                           UNTIL      (IDX     >=    IDX-P  )
                          IF    (ORCHCEPRE-REFILL-NUM (IDX)
                                           =  ORCHCEPRE-REFILL-NUM 
                                                              (IDX-P))
                          AND   (ORCHCEPRE-REFILL-KAISU (IDX)
                                           =   ORCHCEPRE-REFILL-KAISU
                                                              (IDX-P))
                           MOVE    "64"            TO  WRK-ERRCD
                           STRING  "リフィル回数、リフィル処方日数が"
                                   "重複しています。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ERRMSG
                           END-STRING
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
           MOVE    IDX-P               TO  ORCHCEPRE-REF-MAX
      *
           .
       200218-PRE-HENSYU-EXT.
           EXIT.
      *2024/01
      *****************************************************************
      *    リフィル処方回数数値変換処理
      *****************************************************************
       2002181-REF-KAKSUHEN-SEC        SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-REF-KAISU      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
      *        数値エラーはゼロ（送信なし）
               MOVE    ZERO                TO  WRK-REF-KAISU
               MOVE    1                   TO  FLG-REF-ERR
           ELSE
               MOVE    SNUM-OUTEDT(13:3)   TO  WRK-REF-KAISU
               MOVE    ZERO                TO  FLG-REF-ERR
           END-IF
           .
       2002181-REF-KAKSUHEN-EXT.
           EXIT.
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       700-FILE-DELETE-SEC          SECTION.
      *
      *    一時保存ファイル削除
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       700-FILE-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKUHEN-SEC          SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MEISYO          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-MEISYO
           END-IF
           .
       800-ZENKAKUHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       801-ZENKAKUCHK-SEC           SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
      *
      *    改行エラー
           IF      (KANACHK-RC4     NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
      *     ELSE
      *         MOVE    ZERO                TO  FLG-ERR
      *         MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           END-IF
           .
       801-ZENKAKUCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
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
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *
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
      *    テーブル検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
