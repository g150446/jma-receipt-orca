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
       PROGRAM-ID.             ORCSEPRECSV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 電子処方箋　ＣＳＶデータ出力
      *                      電子処方箋対策用
      *  管理者            : 
      *  作成日付    作業者        記述
      *  22/10/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.03.00     ORCAMO       23/06/XX  用法マスタ反映対応
      * 05.03.00     ORCAMO       24/01/XX  リフィル回数対応
      * 05.03.00     ORCAMO       24/03/XX  交付番号備考記載対応
      * 05.02.00     ORCAMO       24/08/XX  先発医薬品患者希望対応
      * 05.02.00     ORCAMO       24/12/XX  漢字氏名を40バイトとする
      * 05.02.00     ORCAMO       25/01/XX  ダミーコードなど警告対応
      * 05.02.00     ORCAMO       25/04/XX  使用期限年月日対応
      *                                     一般名コード編集変更対応
      *                                     チェック用　追加
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    CSVデータ
           SELECT  EPCSV-FILE      ASSIGN  FILE-NAME1
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-FILE1.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    CSVデータ
       FD  EPCSV-FILE.
       01  EPCSV-REC           PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    ファイル指定領域
       01  FILE-NAME1              PIC X(1024).
       01  FILE-NAME2              PIC X(1024).
      *    ファイル指定領域
      *
       01  FILE-NAME1-PARA.
      **** 03  FILLER              PIC X(05)   VALUE   "/tmp/".
           03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
           03  FILE-FORM-ID1       PIC X(04)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILE-TERMID1        PIC X(32)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  FILE-SYOHMS1        PIC 9(08)   VALUE   ZERO.
           03  FILLER              PIC X(07)   VALUE   ".dat".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-FILE1           PIC X(02).
           03  STS-FILE2           PIC X(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTPUBLIC        PIC 9(01).
           03  FLG-GENERICPRICE    PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-YOUHOU          PIC 9(01).
           03  FLG-YOUHOUCHG       PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-CHK1            PIC 9(01).
           03  FLG-YOUHOU-END      PIC 9(01).
      *2024/03
           03  FLG-NO              PIC 9(01).
           03  FLG-HEN             PIC 9(01).
      *2025/04
           03  FLG-CHKKBN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX-H               PIC 9(02).
           03  IDX-B               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDXM                PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXA                PIC 9(04).
      *
           03  TBL-MAX             PIC 9(04).
           03  IDX-B2              PIC 9(03).
      *
           03  IDXF                PIC 9(04).
           03  IDXF2               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDS                 PIC 9(04).
           03  IDH                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(05).
               05  WRK-PO-ED           PIC 9(05).
               05  WRK-CNT             PIC 9(05).
           03  WRK-DATA                PIC X(400).
      *
           03  WRK-RP1                 PIC 9(03).
           03  WRK-RP2-1               PIC 9(02).
           03  WRK-RP2                 PIC 9(02).
           03  WRK-RP-11               PIC 9(02).
      *
           03  WRK-H-RP1               PIC X(03).
           03  WRK-H-RP2               PIC X(03).
           03  WRK-H-RP2-1             PIC X(03).
           03  WRK-H-RP-11             PIC X(03).
           03  WRK-H-DATA              PIC X(03).
           03  WRK-ZZZ                 PIC ZZZ.
      *
           03  WRK-PR-4                PIC X(01).
      *
           03  WRK-SURYO-K          PIC 9(05)V9(05).
           03  WRK-KANSAN           PIC 9(05)V9(05).
           03  WRK-SURYO            PIC 9(05)V9(05).
           03  WRK-SURYO-R          REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1     PIC 9(05).
               05  WRK-SURYO-S2     PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1     PIC ZZZZ9.
               05  WRK-SURYO-Z2     PIC X(01).
               05  WRK-SURYO-Z3     PIC ZZZZZ.
           03  WRK-SURYO-H          PIC X(12).
      *
           03  WRK-QR-KOHDATA.
               05  WRK-QR-KOU-G        OCCURS   4.
                   07  WRK-QR-FTNJA        PIC X(08).
                   07  WRK-QR-JKYSNUM      PIC X(07).
               05  WRK-QR-FTNJA-N      PIC X(60).
               05  WRK-QR-JKYSNUM-N    PIC X(60).
      *2024/03
           03  WRK-KOH-ID-G.
               05  WRK-KOH-ID          PIC 9(10)
                                       OCCURS   4.
      *    薬品名称
           03  WRK-NAME.
               05  WRK-NAME-1          PIC X(180).
               05  WRK-NAME-2          PIC X(20).
      *H24.8
           03  WRK-YAKKAKJNCD          PIC X(12).
           03  WRK-GENERIC-STYMD       PIC X(08).
           03  WRK-GENERIC-EDYMD       PIC X(08).
      *
           03  WRK-KEI-SURYO           PIC 9(05)V9(05).
      *
      *    CSVデータ名称
           03  WRK-CSV-FILENAME        PIC X(1024).
      *
           03  WRK-DAYCNT              PIC 9(02).
      *
           03  WRK-PTNUM               PIC X(15).
           03  WRK-BIKO-FILE           PIC X(100).
      *
           03  WRK-KANANAME            PIC X(60).
      *
           03  WRK-YOUHOU-NAME         PIC X(200).
           03  WRK-YOUHOU-CD           PIC X(16).
      *
      *    ＣＳＶテーブル
       01  TBL-EPCSV-AREA.
           03  TBL-EPCSV-REC.
               05  TBL-EPCSV-DATA        PIC X(400)  OCCURS  13.
      *
      *    APIの時ＣＳＶデータを返却
       01  TBL-CSVDATA-AREA.
           03  TBL-CSVDATA-OCC       OCCURS   500.
               07  TBL-CSVDATA               PIC X(500).
           03  TBL-CSVMAX                    PIC 9(04).
      *
      *    一般名例外コードテーブル
      *     COPY    "CMGENERICTBL.INC".
      *
      *    一時ディレクトリ取得・削除サブ
           COPY    "CPORCSTEMPDIR.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    地域連携情報
           COPY     "CPSK9101.INC".
      *
      *    オンライン資格確認等情報
           COPY    "CPSK1051.INC".
      *
      *    患者地域連携テーブル
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    最低薬価マスタ
       01  GENERICPRICE-REC.
           COPY    "CPGENERICPRICE.INC".
      *
      *    用法コード
       01  YOUHOU-REC.
           COPY    "CPYOUHOU.INC".
      *    用法コード対応テーブル
       01  YOUHOUCHG-REC.
           COPY    "CPYOUHOUCHG.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＣＳＶファイル作成パラメタ
           COPY    "CPORCSFILEENCODE.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "ENUM-VALUE".
      *
      ***     COPY    "CPORCXKANACONV2.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    処方箋共通パラメタ
           COPY    "CPORCSEPRECSV.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSEPRECSVAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO                TO  ORCSEPRECSV-ERRCD
           MOVE    SPACE               TO  ORCSEPRECSV-CSVFILE
           INITIALIZE                  ORCSEPRECSV-CSVDATA-AREA
           MOVE    SPACE               TO  FILE-NAME1
           MOVE    SPACE               TO  FILE-NAME2
      *
      *    ginbee対応
      *     IF      SPA-MW              =   SPA-GINBEE
      *         MOVE    9               TO  ORCSEPRECSV-ERRCD
      *         GO      TO      000-PROC-EXT
      *     END-IF
      *
      *APIはパラメタテーブルで返却
           IF      ORCSEPRECSV-SYORIKBN    =   "API"
              CONTINUE
           ELSE
      *
      *        CSVデータファイル名取得
               PERFORM 100-CVSFILE-HEN-SEC
               IF      ORCSEPRECSV-ERRCD NOT =   ZERO
                   GO      TO      000-PROC-EXT
               END-IF
      *        ＣＶＳデータ保存場所
               MOVE    FILE-NAME2      TO  FILE-NAME1
      *
               OPEN    OUTPUT              EPCSV-FILE
               IF      STS-FILE1       NOT =   ZERO
                   MOVE    9               TO  ORCSEPRECSV-ERRCD
                   GO      TO      000-PROC-EXT
               END-IF
           END-IF
      *
      *    ＤＢ検索処理
           PERFORM 1001-DBKENSAKU-SEC
      *    共通情報出力
           IF      ORCSEPRECSV-ERRCD     =   ZERO
               PERFORM 100-HEAD-SYORI-SEC
           END-IF
      *    薬剤情報出力
           IF      ORCSEPRECSV-ERRCD     =   ZERO
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
      *
      *APIはパラメタテーブルで返却
           IF      ORCSEPRECSV-SYORIKBN    =   "API"
              CONTINUE
           ELSE
      *
           CLOSE                       EPCSV-FILE
      *
           IF      ORCSEPRECSV-ERRCD   =   ZERO
      *        CSVファイル名編集
               MOVE    FILE-NAME1      TO  ORCSEPRECSV-CSVFILE
           END-IF
      *
           END-IF
      *
      *    データ削除
      *     MOVE    ZERO                TO  IF-RESULT
      *     MOVE    FILE-NAME1          TO  IF-FILENAME
      *     CALL    "orcfiledel"        USING
      *                                 ORCSFDELAREA
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    CSVデータファイル名取得処理
      *****************************************************************
       100-CVSFILE-HEN-SEC             SECTION.
      *
      *2023/09
           MOVE    SPACE           TO  WRK-CSV-FILENAME
      *    電子処方箋の保存場所
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1051"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1051-REC
           ELSE
               MOVE    SPACE               TO  SYS-1051-REC
               INITIALIZE                      SYS-1051-REC
           END-IF
           IF      SYS-1051-CSVFILEDIR NOT =   SPACE
               MOVE    SYS-1051-CSVFILEDIR     TO  WRK-CSV-FILENAME
           END-IF
      *
           IF      WRK-CSV-FILENAME    =   SPACE
      *
      *    ＣＶＳデータ保存場所
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "9103"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9101-REC
           ELSE
               MOVE    SPACE               TO  SYS-9101-REC
               INITIALIZE                      SYS-9101-REC
           END-IF
           IF    (SYS-9101-COOPBASE   =   "1"
                                      OR  "2"
                                      OR  "0"  )
           AND   (SYS-9101-FILE-DIR   NOT =   SPACE )
               MOVE    SYS-9101-FILE-DIR   TO  WRK-CSV-FILENAME
           END-IF
      *
           END-IF
      *
           IF      WRK-CSV-FILENAME    =   SPACE
      *        ＣＶＳデータ保存場所がなければ、出力なし
               MOVE    9                   TO  ORCSEPRECSV-ERRCD
               GO      TO      100-CVSFILE-HEN-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           ACCEPT  SYS-TIME            FROM    TIME
      *
      *    患者番号変換（６桁以下は６桁とする）
           PERFORM VARYING    IDX      FROM    15  BY  -1
                   UNTIL     (IDX      <   1    )
                          OR (IDX-MAX  >   ZERO )
               IF      ORCSEPRECSV-PTNUM(IDX:1)  NOT =   SPACE
                   MOVE    IDX             TO  IDX-MAX
               END-IF
           END-PERFORM
           IF      IDX-MAX             <   6
               MOVE    ZERO                TO  WRK-PTNUM
               COMPUTE IDS                 =   6    -  IDX-MAX  +  1
               MOVE    ORCSEPRECSV-PTNUM (1:IDX-MAX)
                                           TO  WRK-PTNUM(IDS:)
           ELSE
               MOVE    ORCSEPRECSV-PTNUM     TO  WRK-PTNUM
           END-IF
      *
           MOVE    SPACE               TO  FILE-NAME2
           STRING  WRK-CSV-FILENAME
                   "/"
                  "ORCAepre-tp-"
                   ORCSEPRECSV-HOSPCD
                   "-"
                   WRK-PTNUM
                   "-"
                   SMCNDATE-YMD
                   SYS-TIME
                   ".csv"
                                       DELIMITED  BY  SPACE
                                       INTO   FILE-NAME2
           END-STRING
           .
       100-CVSFILE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       1020-FILENAME-SET-SEC             SECTION.
      *
      *    一時ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
           MOVE    "ECSV"              TO  FILE-FORM-ID1
           MOVE    SPA-TERMID          TO  FILE-TERMID1
           MOVE    ORCSEPRECSV-SYSTIME TO  FILE-SYOHMS1
      *
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    FILE-NAME1-PARA     TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                       TO  FILE-NAME1
      **
           .
       1020-FILENAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVデータ（JIS）作成処理
      *****************************************************************
      * 290-CSVOUT-SYORI-SEC             SECTION.
      *
      *    CSVデータ（JIS）の作成
      *     INITIALIZE                      ORCSFILEENCODEAREA
      *     MOVE    FILE-NAME1          TO  FLEN-INFILENAME
      *     MOVE    FILE-NAME2          TO  FLEN-OUTFILENAME
      *     MOVE    "1"                 TO  FLEN-OPTION
      *     CALL    "orcfileencode"     USING
      *                                     ORCSFILEENCODEAREA
      *     IF      FLEN-RET        NOT =   ZERO
      *         MOVE    9               TO  ORCSEPRECSV-ERRCD
      *     END-IF
      *
      *     .
      * 290-CSVOUT-SYORI-EXT.
      **     EXIT.
      *
      *****************************************************************
      *    共通情報出力処理
      *****************************************************************
       100-HEAD-SYORI-SEC             SECTION.
      *
      *    バージョン情報
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "SJ1"               TO  TBL-EPCSV-DATA (1)
           MOVE    1                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    医療機関コード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "1"                 TO  TBL-EPCSV-DATA (1)
           MOVE    "1"                 TO  TBL-EPCSV-DATA (2)
           MOVE    ORCSEPRECSV-HOSPCD      TO  TBL-EPCSV-DATA (3)
           MOVE    ORCSEPRECSV-PREFNUM     TO  TBL-EPCSV-DATA (4)
           MOVE    ORCSEPRECSV-HOSPNAME    TO  TBL-EPCSV-DATA (5)
           MOVE    5                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    医療機関所在地レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "2"                 TO  TBL-EPCSV-DATA (1)
           MOVE    ORCSEPRECSV-HOSPPOST    TO  TBL-EPCSV-DATA (2)
           MOVE    ORCSEPRECSV-HOSPADRS    TO  TBL-EPCSV-DATA (3)
           MOVE    3                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    医療機関電話レコード
           IF      ORCSEPRECSV-HOSPTEL   NOT =   SPACE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "3"                 TO  TBL-EPCSV-DATA (1)
               MOVE    ORCSEPRECSV-HOSPTEL TO  TBL-EPCSV-DATA (2)
               MOVE    ORCSEPRECSV-HOSPFAX TO  TBL-EPCSV-DATA (3)
               MOVE    4                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    診療科
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "4"                 TO  TBL-EPCSV-DATA (1)
           IF      ORCSEPRECSV-RECESRYKA =   SPACE  OR  ZERO
               MOVE    "1"                 TO  TBL-EPCSV-DATA (2)
           ELSE
               MOVE    "2"                 TO  TBL-EPCSV-DATA (2)
               MOVE    ORCSEPRECSV-RECESRYKA   TO  TBL-EPCSV-DATA (3)
           END-IF
           MOVE    ORCSEPRECSV-SRYKANAME   TO  TBL-EPCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    医師レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "5"                 TO  TBL-EPCSV-DATA (1)
      *    医師コード
           MOVE    ORCSEPRECSV-DR-CD   TO  TBL-EPCSV-DATA (2)
      *    拡張漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSEPRECSV-DR-NAME TO  KANACHK-MAE-INPUT
      *2024/12
      *    40バイトとする
           MOVE    40                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                       TO  TBL-EPCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    患者氏名レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "11"                TO  TBL-EPCSV-DATA (1)
      *    患者番号
      *****MOVE    WRK-PTNUM           TO  TBL-EPCSV-DATA (2)
      *2025/06
           MOVE    ORCSEPRECSV-PTNUM   TO  TBL-EPCSV-DATA (2)
      *    拡張漢字変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSEPRECSV-NAME    TO  KANACHK-MAE-INPUT
      *2024/12
      *    40バイトとする
           MOVE    40                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT(1:40)
                                       TO  TBL-EPCSV-DATA (3)
      *    カナ氏名を半角カナへ
           PERFORM 20033-KANANAME-HEN-SEC
           MOVE    WRK-KANANAME        TO  TBL-EPCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    患者性別レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "12"                TO  TBL-EPCSV-DATA (1)
           MOVE    ORCSEPRECSV-SEX     TO  TBL-EPCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    患者生年月日レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "13"                TO  TBL-EPCSV-DATA (1)
           MOVE    ORCSEPRECSV-BIRTHDAY    TO  TBL-EPCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    一部負担区分レコード
           IF      ORCSEPRECSV-BIKO-FTN  NOT =   SPACE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    ORCSEPRECSV-BIKO-FTN    TO  TBL-EPCSV-DATA (2)
               IF      TBL-EPCSV-DATA (2)  NOT =   SPACE
                   MOVE    "14"                TO  TBL-EPCSV-DATA (1)
                   MOVE    2                   TO  TBL-MAX
                   PERFORM 400-DATAOUT-SEC
               END-IF
           END-IF
      *    保険種別レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "21"                TO  TBL-EPCSV-DATA (1)
           EVALUATE    COMB-HKNNUM
      *       国保
              WHEN     "060"
              WHEN     "067"
              WHEN     "068"
              WHEN     "069"
                  MOVE    "2"          TO  TBL-EPCSV-DATA (2)
      *       労災
              WHEN     "971"
      *       自賠責
              WHEN     "973"
      *       公害
              WHEN     "975"
      *       自費
              WHEN     "980"  THRU  "989"
                   MOVE    1                   TO  ORCSEPRECSV-ERRCD
      *       保険なし
              WHEN     SPACE
                  MOVE    "1"          TO  TBL-EPCSV-DATA (2)
      *       後期高齢者
              WHEN     "039"
              WHEN     "040"
                  MOVE    "7"          TO  TBL-EPCSV-DATA (2)
      *       医保、公費
              WHEN     OTHER
                  MOVE    "1"          TO  TBL-EPCSV-DATA (2)
           END-EVALUATE
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    労災・自賠責・自費は保険者・記号番号クリア
      *     IF     (COMB-HKNNUM         =   "971"
      *                                 OR  "973" )
      *         OR (COMB-HKNNUM (1:2)   =   "98"  )
      *         MOVE    SPACE               TO  PTHKN-HKNJANUM
      *         MOVE    SPACE               TO  PTHKN-KIGO
      *         MOVE    SPACE               TO  PTHKN-NUM
      *         MOVE    "1"                 TO  PTHKN-HONKZKKBN
      *     END-IF
      **    公費単独は本人とする
      *     IF     (COMB-HKNNUM         =   SPACE )
      *       AND  (COMB-KOHHKNNUM (1)  NOT =   SPACE )
      *         MOVE    "1"                 TO  PTHKN-HONKZKKBN
      *     END-IF
      *    後期高齢者は記号クリア
           IF      COMB-HKNNUM         =   "039"
               MOVE    SPACE               TO  PTHKN-KIGO
           END-IF
      *    保険者番号レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "22"                TO  TBL-EPCSV-DATA (1)
           MOVE    PTHKN-HKNJANUM      TO  TBL-EPCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    記号・番号レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "23"                TO  TBL-EPCSV-DATA (1)
           MOVE    PTHKN-KIGO          TO  TBL-EPCSV-DATA (2)
           MOVE    PTHKN-NUM           TO  TBL-EPCSV-DATA (3)
           MOVE    PTHKN-HONKZKKBN     TO  TBL-EPCSV-DATA (4)
           MOVE    PTHKN-EDABAN        TO  TBL-EPCSV-DATA (5)
           MOVE    5                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    職務上の事由レコード
      *        船員,共済
           IF      PTHKN-HKNNUM        =   "002"
                                       OR  "031"
                                       OR  "032"
                                       OR  "033"
                                       OR  "034"
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               EVALUATE    PTHKN-HOJOKBN
                   WHEN    "1"
                   WHEN    "A"
                   WHEN    "G"
                   WHEN    "D"
      *                職務
                       MOVE    "1"         TO  TBL-EPCSV-DATA (2)
                   WHEN    "2"
                   WHEN    "B"
                   WHEN    "H"
                   WHEN    "E"
      *                下船
                       MOVE    "2"         TO  TBL-EPCSV-DATA (2)
                   WHEN    "3"
                   WHEN    "C"
                   WHEN    "I"
                   WHEN    "F"
      *                通勤
                       MOVE    "3"         TO  TBL-EPCSV-DATA (2)
               END-EVALUATE
               IF      TBL-EPCSV-DATA (2)  NOT =   SPACE
                   MOVE    "25"                TO  TBL-EPCSV-DATA (1)
                   MOVE    2                   TO  TBL-MAX
                   PERFORM 400-DATAOUT-SEC
               END-IF
           END-IF
      *
      *    公費１レコード
           IF      WRK-QR-FTNJA (1)    NOT =   SPACE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "27"                TO  TBL-EPCSV-DATA (1)
               MOVE    WRK-QR-FTNJA (1)    TO  TBL-EPCSV-DATA (2)
               MOVE    WRK-QR-JKYSNUM(1)   TO  TBL-EPCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    公費２レコード
           IF      WRK-QR-FTNJA (2)    NOT =   SPACE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "28"                TO  TBL-EPCSV-DATA (1)
               MOVE    WRK-QR-FTNJA (2)    TO  TBL-EPCSV-DATA (2)
               MOVE    WRK-QR-JKYSNUM(2)   TO  TBL-EPCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    公費３レコード
           IF      WRK-QR-FTNJA (3)    NOT =   SPACE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "29"                TO  TBL-EPCSV-DATA (1)
               MOVE    WRK-QR-FTNJA (3)    TO  TBL-EPCSV-DATA (2)
               MOVE    WRK-QR-JKYSNUM(3)   TO  TBL-EPCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    特殊公費レコード
           IF      WRK-QR-FTNJA-N      NOT =   SPACE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "30"                TO  TBL-EPCSV-DATA (1)
               MOVE    WRK-QR-FTNJA-N      TO  TBL-EPCSV-DATA (2)
               MOVE    WRK-QR-JKYSNUM-N    TO  TBL-EPCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    交付日レコード
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "51"                TO  TBL-EPCSV-DATA (1)
           MOVE    SPA-SRYYMD          TO  TBL-EPCSV-DATA (2)
           MOVE    2                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *2025/04
      *    使用期限年月日レコード
           IF      ORCSEPRECSV-KIKAN-YMD   NOT =   SPACE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "52"                TO  TBL-EPCSV-DATA (1)
               MOVE    ORCSEPRECSV-KIKAN-YMD
                                           TO  TBL-EPCSV-DATA (2)
               MOVE    2                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    麻薬施用レコード
           IF      ORCSEPRECSV-MAYAKU    =   1
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "60"                TO  TBL-EPCSV-DATA (1)
      *        麻薬施用コード
               MOVE    ORCSEPRECSV-MAYAKULICENSE
                                           TO  TBL-EPCSV-DATA (2)
               MOVE    ORCSEPRECSV-ADRS    TO  TBL-EPCSV-DATA (3)
               MOVE    ORCSEPRECSV-TEL     TO  TBL-EPCSV-DATA (4)
               MOVE    4                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    残量確認指示
           IF      ORCSEPRECSV-ZANSIJI   =   "1"
                                         OR  "2"
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "62"                TO  TBL-EPCSV-DATA (1)
      *        残量確認指示
               MOVE    ORCSEPRECSV-ZANSIJI TO  TBL-EPCSV-DATA (2)
               MOVE    2                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *2023/12
      *    リフィル処方箋情報
           IF      ORCSEPRECSV-REFILL-CNT  =   2
                                           OR  3
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "64"                TO  TBL-EPCSV-DATA (1)
      *        リフィル回数
               MOVE    ORCSEPRECSV-REFILL-CNT
                                           TO  TBL-EPCSV-DATA (2)
               MOVE    2                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    備考レコード
           MOVE    ZERO                TO  IDX-B2
           PERFORM VARYING     IDX-B   FROM    1   BY  1
                   UNTIL      (IDX-B   >   20  )
                           OR (ORCSEPRECSV-BIKOU (IDX-B) =   SPACE)
      *2024/03
               MOVE    ZERO                TO  FLG-NO
               IF      ORCSEPRECSV-BIKOU-KOHID (IDX-B)   NOT =   ZERO
                   IF      ORCSEPRECSV-BIKOU-KOHID (IDX-B)
                                               =   WRK-KOH-ID(1)
                                               OR  WRK-KOH-ID(2)
                                               OR  WRK-KOH-ID(3)
                                               OR  WRK-KOH-ID(4)
                       MOVE    1               TO  FLG-NO 
                   END-IF
               END-IF
      *        公費レコード記載分は対象外
      *2024/03
               IF      FLG-NO              =   ZERO
      *
               ADD     1                   TO  IDX-B2
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "81"                TO  TBL-EPCSV-DATA (1)
               MOVE    IDX-B2              TO  WRK-ZZZ
               PERFORM 20011-SUJIHEN-SEC
               MOVE    WRK-H-DATA          TO  TBL-EPCSV-DATA (2)
      *        種別
               IF      ORCSEPRECSV-BIKOU-KBN (IDX-B)  NOT =   SPACE
                   MOVE    ORCSEPRECSV-BIKOU-KBN (IDX-B)
                                           TO  TBL-EPCSV-DATA (3)
               END-IF
               MOVE    ORCSEPRECSV-BIKOU (IDX-B)
                                           TO  TBL-EPCSV-DATA (4)
               MOVE    4                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
      *
               END-IF
           END-PERFORM
      *
      *    処方箋番号レコード
           IF      ORCSEPRECSV-ACCESS-CODE NOT =   SPACE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "82"                TO  TBL-EPCSV-DATA (1)
               MOVE    "1"                 TO  TBL-EPCSV-DATA (2)
               MOVE    ORCSEPRECSV-ACCESS-CODE
                                           TO  TBL-EPCSV-DATA (3)
               MOVE    3                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
           .
       100-HEAD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       1001-DBKENSAKU-SEC             SECTION.
      *
      *    保険組合せマスター検索
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    ORCSEPRECSV-HKNCOMBINUM
                                       TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
                INITIALIZE                      HKNCOMBI-REC
           END-IF
      *    患者保険情報検索
           IF      COMB-HKNID          =   ZERO
               INITIALIZE                  PTHKNINF-REC
           ELSE
               INITIALIZE                      PTHKNINF-REC
               MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
               MOVE    SPA-PTID            TO  PTHKN-PTID
      *        主保険−ＩＤ
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               PERFORM 920-PTHKNINF-READ-SEC
               IF      FLG-PTHKNINF    NOT =   ZERO
                   INITIALIZE              PTHKNINF-REC
               END-IF
           END-IF
      *    公費保健情報編集処理
           PERFORM 1001-KOHHKN-HEN-SEC
      *
           .
       1001-DBKENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保健情報編集処理
      *****************************************************************
       1001-KOHHKN-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-QR-KOHDATA
           MOVE    ZERO                TO  IDX-H
      *
           PERFORM VARYING    IDX-K    FROM    1   BY  1
                   UNTIL     (IDX-K    >   4   )
                         OR  (ORCSEPRECSV-KOHID (IDX-K)  =   ZERO )
                         OR  (ORCSEPRECSV-ERRCD  NOT =   ZERO     )
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-PTID            TO  PTKOH-PTID
               MOVE    ORCSEPRECSV-KOHID (IDX-K)
                                           TO  PTKOH-KOHID
               PERFORM 940-PTKOHINF-READ-SEC
      *        負担者番号がない時、エラーとする
               IF      PTKOH-FTNJANUM      =   SPACE
                   IF      PTKOH-JKYSNUM       =   SPACE
      *                受給者番号もないとき、対象外
                       CONTINUE
                   ELSE
                       MOVE    2                   TO  ORCSEPRECSV-ERRCD
                   END-IF
               ELSE
      *            公費
                   PERFORM 1001-KOHCHEK-SEC
               END-IF
           END-PERFORM
           .
       1001-KOHHKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費区分チェック処理
      *****************************************************************
       1001-KOHCHEK-SEC             SECTION.
      *
      *2024/03
           MOVE    ZERO                TO  FLG-HEN
      *    負担者番号
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    PTKOH-FTNJANUM      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-MAX         >   8    )  OR
                  (KANACHK-SYUBETU     =   2    )
      *        ８桁以上か、全角
               MOVE    PTKOH-FTNJANUM      TO  WRK-QR-FTNJA-N
               MOVE    PTKOH-JKYSNUM       TO  WRK-QR-JKYSNUM-N
      *2024/03
               MOVE    1                   TO  FLG-HEN
           ELSE
      *    受給者番号
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    PTKOH-JKYSNUM       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-MAX         >   7    )  OR
                      (KANACHK-SYUBETU     =   2    )
      *            ７桁以上か、全角
                   MOVE    PTKOH-FTNJANUM      TO  WRK-QR-FTNJA-N
                   MOVE    PTKOH-JKYSNUM       TO  WRK-QR-JKYSNUM-N
      *2024/03
                   MOVE    1                   TO  FLG-HEN
               ELSE
                   ADD     1                   TO  IDX-H
                   IF      IDX-H               <=  3
                       MOVE    PTKOH-FTNJANUM      TO  WRK-QR-FTNJA
                                                               (IDX-H)
                       MOVE    PTKOH-JKYSNUM       TO  WRK-QR-JKYSNUM
                                                                (IDX-H)
      *2024/03
                       MOVE    1                   TO  FLG-HEN
                   END-IF
               END-IF
           END-IF
      *2024/03
           IF      FLG-HEN             =   1
               ADD     1               TO  IDH 
               MOVE    PTKOH-KOHID     TO  WRK-KOH-ID (IDH)
           END-IF
           .
       1001-KOHCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       400-DATAOUT-SEC             SECTION.
      *
           INITIALIZE                  WRK-POINT-AREA
           MOVE    SPACE           TO  EPCSV-REC
           INITIALIZE                  EPCSV-REC
      *
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >   TBL-MAX )
                       OR     (WRK-PO-ED  >=  1000 )
      *
               COMPUTE WRK-PO-ST      =   WRK-PO-ED   +   1
      *
               MOVE    TBL-EPCSV-DATA (IDK)    TO  WRK-DATA
      *        項目別編集処理
               PERFORM 4001-KOUMOKU-HEN-SEC
               MOVE    WRK-PO-ST       TO  WRK-PO-ED
           END-PERFORM
      *
      *
           IF      ORCSEPRECSV-SYORIKBN    =   "API"
               IF      ORCSEPRECSV-CSVMAX       >=  500
                   MOVE    8               TO  ORCSEPRECSV-ERRCD
               ELSE
                   ADD     1               TO  ORCSEPRECSV-CSVMAX
                   MOVE    EPCSV-REC       TO  ORCSEPRECSV-CSVDATA
                                                (ORCSEPRECSV-CSVMAX)
               END-IF
           ELSE
      *
      *    ＣＳＶデータ出力
           WRITE   EPCSV-REC
      *
           END-IF
           .
       400-DATAOUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目別編集処理
      *****************************************************************
       4001-KOUMOKU-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT
           PERFORM VARYING IDX-D       FROM    1   BY  1
                   UNTIL  (IDX-D       >   400     )
      **       IF      WRK-DATA (IDX-D:1)  =   SPACE
               IF      WRK-DATA (IDX-D:)   =   SPACE
                   MOVE    400             TO  IDX-D
               ELSE
                   ADD     1               TO  WRK-CNT
               END-IF
           END-PERFORM
      *
           IF          WRK-CNT         >   0
               MOVE    WRK-DATA (1:WRK-CNT)
                                           TO  EPCSV-REC
                                               (WRK-PO-ST:WRK-CNT)
               COMPUTE WRK-PO-ST   =   WRK-PO-ST   +   WRK-CNT
           END-IF
      *
      *    最終項目以外に , 追加
           IF          IDK             <   TBL-MAX
               MOVE    ","             TO  EPCSV-REC(WRK-PO-ST:1)
           END-IF
           .
       4001-KOUMOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報出力処理
      *****************************************************************
       200-MAIN-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-RP1
           MOVE    ZERO                TO  WRK-RP-11
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   50  )
                         OR   (ORCSEPRECSV-ZAIKATA(IDX) =   SPACE )
      *        薬剤レコード判定
               PERFORM 2000-SRYCD-CHK-SEC
               IF      FLG-SYORI           =   1
      *            剤型レコード
                   PERFORM 2001-RP101-SEC
      *H30.4
      *            分割指示数量レコード
      *             IF     (SPA-SRYYMD          >=  "20180401" )
      *               AND  (ORCSEPRECSV-GOKKAISU(IDX) >   ZERO   )
      *                 PERFORM 2001-RP102-SEC
      **             END-IF
      *            用法レコード
                   PERFORM 2002-RP111-SYORI-SEC
      *
                   MOVE    ZERO                TO  WRK-RP2
                   PERFORM VARYING     IDX2    FROM    1   BY  1
      *2023/04             UNTIL      (IDX2    >   50  )
                           UNTIL      (IDX2    >   100  )
      *                薬剤レコード
                       IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:1)
                                                           =  "6"
                                                           OR "7" )
                           OR (ORCSEPRECSV-SRYCD(IDX IDX2)(1:3)
                                                           =  "059")
                           PERFORM 2003-RP201-SEC
                       END-IF
      *                薬剤補足レコード
                       IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:1)
                                                           =  "8" )
                           OR (ORCSEPRECSV-SRYCD(IDX IDX2)(1:3)
                                                           =  "008"
                                                           OR "099")
                       OR    ((ORCSEPRECSV-SRYCD(IDX IDX2)(1:3)
                                                           =  "001")
                          AND (ORCSEPRECSV-YCOMMNTKBN(IDX IDX2)
                                                           =   "2" ))
                           PERFORM 20030-RP281-SYORI-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    処方箋情報
           IF      ORCSEPRECSV-ERRCD     =   ZERO
               PERFORM 20040-RP301-SYORI-SEC
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤型レコード処理
      *****************************************************************
       2000-SRYCD-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
           PERFORM VARYING     IDX2    FROM    1   BY  1
      *2023/04     UNTIL      (IDX2    >   50  )
                   UNTIL      (IDX2    >   100  )
                          OR  (FLG-SYORI   =   1   )
      *        薬剤レコード
               IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:1) =  "6"
                                                      OR "7" )
                   OR (ORCSEPRECSV-SRYCD(IDX IDX2)(1:3) =  "059")
                   MOVE    1               TO  FLG-SYORI
               END-IF
           END-PERFORM
           .
       2000-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤型レコード処理
      *****************************************************************
       2001-RP101-SEC            SECTION.
      *
           ADD     1                   TO  WRK-RP1
           MOVE    WRK-RP1             TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP1
      *
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "101"               TO  TBL-EPCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
           MOVE    ORCSEPRECSV-ZAIKATA (IDX)
                                       TO  TBL-EPCSV-DATA (3)
           MOVE    ORCSEPRECSV-KAISU (IDX)   TO  WRK-ZZZ
      *H30.4
      *    分割指示の時は、総量
           IF     (SPA-SRYYMD          >=  "20180401" )
             AND  (ORCSEPRECSV-GOKKAISU(IDX) >   ZERO   )
               MOVE    ORCSEPRECSV-GOKKAISU (IDX)    TO  WRK-ZZZ
           END-IF
      *
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-EPCSV-DATA (5)
           MOVE    5                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
           .
       2001-RP101-EXT.
           EXIT.
      *
      *未使用
      *****************************************************************
      *    分割指示数量レコード処理
      *****************************************************************
       2001-RP102-SEC            SECTION.
      *
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "102"               TO  TBL-EPCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
           MOVE    ORCSEPRECSV-KAISU (IDX)   TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-EPCSV-DATA (3)
           MOVE    ORCSEPRECSV-GOKKAISU(IDX) TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-EPCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
           .
       2001-RP102-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＲ番号数値編集処理
      *****************************************************************
       20011-SUJIHEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-H-DATA
           PERFORM VARYING    IDZ      FROM    1  BY  1
                   UNTIL      IDZ      >   3
               IF      WRK-ZZZ(IDZ:1)  NOT =   SPACE
                   MOVE    WRK-ZZZ(IDZ:)   TO  WRK-H-DATA
                   MOVE    3               TO  IDZ
               END-IF
           END-PERFORM
           .
       20011-SUJIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法レコード処理
      *****************************************************************
       2002-RP111-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-RP-11
      *    用法レコード
           MOVE    ZERO                TO  FLG-YOUHOU-END
           MOVE    SPACE               TO  WRK-YOUHOU-CD
      *    用法コード設定のコード
           PERFORM VARYING     IDX2    FROM    100  BY  -1
                   UNTIL      (IDX2    <   1    )
                         OR   (FLG-YOUHOU-END  NOT =   ZERO  )
      *        用法レコード
               IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:3) =   "001")
               AND    (ORCSEPRECSV-YCOMMNTKBN(IDX IDX2) =   "1"
                                                        OR  "0" )
      *            剤最後の用法コード
                   PERFORM 20021-RP111-SEC
                   MOVE    SPACE       TO  ORCSEPRECSV-SRYCD(IDX IDX2)
                   MOVE    1           TO  FLG-YOUHOU-END
               END-IF
           END-PERFORM
      *
      *    用法コードの下は用法補助とする
           IF     (FLG-YOUHOU-END      =   1    )
               PERFORM VARYING     IDX2    FROM    1  BY  1
                       UNTIL      (IDX2    >   100   )
      *        用法補助レコード
                   IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:3) =  "001")
                   AND    (ORCSEPRECSV-YCOMMNTKBN(IDX IDX2) =   "1"
                                                            OR  "0" )
                       MOVE    SPACE           TO  WRK-PR-4
                       PERFORM 20022-RP181-SEC
                       MOVE    SPACE       TO  ORCSEPRECSV-SRYCD
                                                          (IDX IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *    用法がない時、最後のコメントを用法とする
           IF      WRK-RP-11           =   ZERO
               PERFORM VARYING     IDX2    FROM    100  BY  -1
                       UNTIL      (IDX2    <   1   )
                             OR   (WRK-RP-11   >   ZERO  )
      *            用法レコード
                   IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:1) =  "8")
                     OR   (ORCSEPRECSV-SRYCD(IDX IDX2)(1:3) =  "008")
                     OR  ((ORCSEPRECSV-SRYCD(IDX IDX2)(1:3) =  "001")
                      AND (ORCSEPRECSV-YCOMMNTKBN(IDX IDX2) =   "2" ))
                       PERFORM 20021-RP111-SEC
                       MOVE    SPACE       TO  ORCSEPRECSV-SRYCD
                                                           (IDX IDX2)
                   END-IF
               END-PERFORM
           END-IF
      *?
      **** 用法がなければ空白
           IF      WRK-RP-11           =   ZERO
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "111"               TO  TBL-EPCSV-DATA (1)
               MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
               MOVE    "3"                 TO  TBL-EPCSV-DATA (3)
      *        用法コードは不明とする
               MOVE    "0X0XXXXXXXXX0000"  TO  TBL-EPCSV-DATA (4)
               MOVE    6                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           .
       2002-RP111-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法レコード処理
      *****************************************************************
       20021-RP111-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-YOUHOU-NAME
           MOVE    SPACE               TO  WRK-YOUHOU-CD
      *
           IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:3) =   "001")
           AND    (ORCSEPRECSV-YCOMMNTKBN(IDX IDX2) =   "1"
                                                    OR  "0" )
      *        用法レコードから名称を編集する
               PERFORM 200211-YOUHOU-HEN-SEC
           END-IF
      *
      *    内服の時、用法コードから１日回数を取得する
           MOVE    ZERO                TO  WRK-DAYCNT
           IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:3) =   "001")
           AND    (ORCSEPRECSV-YCOMMNTKBN(IDX IDX2) =   "1"
                                                    OR  "0" )
           AND    (ORCSEPRECSV-ZAIKATA (IDX)        =   "1" )
               MOVE    ORCSEPRECSV-SRYCD(IDX IDX2)
                                          TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   IF     (TNS-KENTAICOMMENT   =   0
                                               OR  1 )
                       PERFORM VARYING    IDXA FROM    1   BY  1
                               UNTIL      IDXA >   5
                           IF      TNS-SSTKIJUNCD (IDXA)   =   1
                               ADD     1           TO  WRK-DAYCNT
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
      *2023/10廃止
      **** １日回数不明はゼロ？
      *     IF      WRK-DAYCNT          =   ZERO
      *         MOVE    1                   TO  WRK-DAYCNT
      *     END-IF
      *
      *
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "111"               TO  TBL-EPCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
           MOVE    "3"                 TO  TBL-EPCSV-DATA (3)
      *    用法コード、標準用法マスタなし
           IF    (WRK-YOUHOU-CD        =   SPACE )
             OR  (WRK-YOUHOU-NAME      =   SPACE )
      *        用法コードは不明とする
               MOVE    "0X0XXXXXXXXX0000"  TO  TBL-EPCSV-DATA (4)
               MOVE    ORCSEPRECSV-TNS-NAME (IDX IDX2)
                                           TO  TBL-EPCSV-DATA (5)
           ELSE
      *        汎用用法コード出力区分
               IF    ( YHCHG-YOUHOUFLG1    =   1   )
                   MOVE    "0X0XXXXXXXXX0000"  TO  TBL-EPCSV-DATA (4)
               ELSE
                   MOVE    WRK-YOUHOU-CD       TO  TBL-EPCSV-DATA (4)
               END-IF
               MOVE    WRK-YOUHOU-NAME     TO  TBL-EPCSV-DATA (5)
           END-IF
      *
      *    １日回数
           MOVE    WRK-DAYCNT          TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  TBL-EPCSV-DATA (6)
           MOVE    6                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
           ADD     1                   TO  WRK-RP-11
      *
           MOVE    WRK-RP-11           TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP-11
      *
           .
       20021-RP111-EXT.
           EXIT.
      *****************************************************************
      *    標準用法名称編集処理
      *****************************************************************
       200211-YOUHOU-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-YOUHOU-NAME
           MOVE    SPACE               TO  WRK-YOUHOU-CD
           INITIALIZE                  YOUHOUCHG-REC
           MOVE    SPA-HOSPNUM         TO  YHCHG-HOSPNUM
           MOVE    ORCSEPRECSV-SRYCD(IDX IDX2)
                                           TO  YHCHG-SRYCD
           PERFORM 940-YOUHOUCHG-READ-SEC
           IF      FLG-YOUHOUCHG       =   ZERO
               MOVE    YHCHG-CODE          TO  WRK-YOUHOU-CD
               INITIALIZE                  YOUHOU-REC
               MOVE    YHCHG-CODE          TO  YOUHOU-CODE
               MOVE    SPA-SRYYMD          TO  YOUHOU-YUKOSTYMD
               MOVE    SPA-SRYYMD          TO  YOUHOU-YUKOEDYMD
               PERFORM 940-YOUHOU-READ-SEC
               IF      FLG-YOUHOU          =   ZERO
                   MOVE    YOUHOU-NAME         TO  WRK-YOUHOU-NAME
               END-IF
           END-IF
           .
       200211-YOUHOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    用法補助レコード処理
      *****************************************************************
       20022-RP181-SEC            SECTION.
      *
           PERFORM 200211-YOUHOU-HEN-SEC
      *
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "181"               TO  TBL-EPCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
           MOVE    WRK-H-RP-11         TO  TBL-EPCSV-DATA (3)
           MOVE    WRK-PR-4            TO  TBL-EPCSV-DATA (4)
           IF      WRK-YOUHOU-NAME     =   SPACE
               MOVE    ORCSEPRECSV-TNS-NAME (IDX IDX2)
                                       TO  TBL-EPCSV-DATA (5)
           ELSE
               MOVE    WRK-YOUHOU-NAME
                                       TO  TBL-EPCSV-DATA (5)
           END-IF
           MOVE    7                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
           ADD     1                   TO  WRK-RP-11
      *
           MOVE    WRK-RP-11           TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP-11
           .
       20022-RP181-EXT.
           EXIT.
      *****************************************************************
      *    薬剤レコード処理
      *****************************************************************
       2003-RP201-SEC            SECTION.
      *
           ADD     1                   TO  WRK-RP2
           MOVE    WRK-RP2             TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP2
      *
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "201"               TO  TBL-EPCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
           MOVE    WRK-H-RP2           TO  TBL-EPCSV-DATA (3)
      *    情報区分
           EVALUATE    ORCSEPRECSV-SRYCD(IDX IDX2)(1:1)
               WHEN    "6"
                   MOVE    "1"             TO  TBL-EPCSV-DATA (4)
               WHEN    "7"
                   MOVE    "2"             TO  TBL-EPCSV-DATA (4)
               WHEN    OTHER
      *            不明は薬剤とする
                   MOVE    "1"             TO  TBL-EPCSV-DATA (4)
           END-EVALUATE
      *2025/04
           MOVE    ZERO                TO  FLG-CHKKBN
      *
           IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:1)  =   "6"
                                                     OR  "7" )
      *???     AND  (ORCSEPRECSV-KISAIKBN (IDX IDX2)   =   ZERO )
      *2025/04
      *        処方名記載
               AND  (ORCSEPRECSV-KISAIKBN (IDX IDX2)   =   ZERO
                                                       OR  "2" )
      *        レセプト電算コード
               MOVE    "2"                 TO  TBL-EPCSV-DATA (5)
               MOVE    ORCSEPRECSV-SRYCD(IDX IDX2)
                                           TO  TBL-EPCSV-DATA (6)
      *2025/01
      *        経過措置年月日有り
               IF      ORCSEPRECSV-KEIKASOCHIYMD (IDX IDX2)
                                           =   SPACE
                                           OR  ZERO
                   CONTINUE
               ELSE
                   MOVE    1               TO  ORCSEPRECSV-WARNING2
               END-IF
           END-IF
           IF     (ORCSEPRECSV-SRYCD(IDX IDX2)(1:1)  =   "6"
                                                     OR  "7" )
               CONTINUE
           ELSE
      *        不明の薬剤とする
               MOVE    "2"                 TO  TBL-EPCSV-DATA (5)
               MOVE    "666660000"         TO  TBL-EPCSV-DATA (6)
      *2025/01
      *        ダミーコードあり
               MOVE    1                   TO  ORCSEPRECSV-WARNING1
           END-IF
      *
      *    一般名記載
           IF      (ORCSEPRECSV-SRYCD(IDX IDX2)(1:1)  =   "6" )
             AND   (ORCSEPRECSV-KISAIKBN (IDX IDX2)   =   1   )
      ***      MOVE    "7"                 TO  TBL-EPCSV-DATA (5)
      *2025/04
               IF      SPA-SRYYMD          <   "20250401"
      *
      *        最低薬価テーブルより
               PERFORM 20039-GENERICPRICE-HEN-SEC
      *
               ELSE
      *        R07.4.1からは点数マスタの一般名コード
                   MOVE    ORCSEPRECSV-GECODE (IDX IDX2)
                                           TO  WRK-YAKKAKJNCD
               END-IF
      *2023/10
      *        一般名コードがない時は、不明の薬剤とする
               IF      WRK-YAKKAKJNCD      =   SPACE
                   MOVE    "2"                 TO  TBL-EPCSV-DATA (5)
                   MOVE    "666660000"         TO  TBL-EPCSV-DATA (6)
      *2025/04
      *            チェック用は薬剤コード
                   IF      ORCSEPRECSV-CHKKBN  =   "CHK"
                       MOVE    ORCSEPRECSV-SRYCD(IDX IDX2)
                                               TO  TBL-EPCSV-DATA (6)
                       MOVE    1               TO  FLG-CHKKBN
                   ELSE
      *2025/01
      *            ダミーコードあり
                   MOVE    1                   TO  ORCSEPRECSV-WARNING1
      *
                   END-IF
               ELSE
                   MOVE    "7"                 TO  TBL-EPCSV-DATA (5)
                   MOVE    WRK-YAKKAKJNCD      TO  TBL-EPCSV-DATA (6)
               END-IF
           END-IF
      *
      *    薬品の名称は180バイトとする
           MOVE    ORCSEPRECSV-TNS-NAME (IDX IDX2)
                                       TO  WRK-NAME
           IF      WRK-NAME            =   SPACE
               MOVE    ORCSEPRECSV-CSV-NAME (IDX IDX2)
                                           TO  WRK-NAME
           END-IF
      *2025/04
      *    チェックで一般名を銘柄名に変更
           IF    (FLG-CHKKBN           =   1   )
             AND (ORCSEPRECSV-CSV-NAME (IDX IDX2) NOT =   SPACE)
               MOVE    ORCSEPRECSV-CSV-NAME (IDX IDX2)
                                           TO  WRK-NAME
           END-IF
      *
           MOVE    WRK-NAME-1          TO  TBL-EPCSV-DATA (7)
      *
           MOVE    ORCSEPRECSV-SURYOU(IDX IDX2)  TO  WRK-SURYO
           PERFORM 20031-SURYO-HEN-SEC
           MOVE    WRK-SURYO-H         TO  TBL-EPCSV-DATA (8)
           MOVE    "1"                 TO  TBL-EPCSV-DATA (9)
           MOVE    ORCSEPRECSV-TANINAME(IDX IDX2)
                                       TO  TBL-EPCSV-DATA (10)
           MOVE    10                  TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
      *    単位変換レコード
           IF      ORCSEPRECSV-KANZANCHI (IDX IDX2)  =   ZERO
                                                   OR  1
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "211"               TO  TBL-EPCSV-DATA (1)
               MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
               MOVE    WRK-H-RP2           TO  TBL-EPCSV-DATA (3)
               COMPUTE WRK-SURYO-K         =   ORCSEPRECSV-SURYOU
                                                          (IDX IDX2)
                                           /   ORCSEPRECSV-KANZANCHI
                                                          (IDX IDX2)
               COMPUTE WRK-KANSAN          =   WRK-SURYO-K
                                           /   ORCSEPRECSV-SURYOU
                                                          (IDX IDX2)
               MOVE    WRK-KANSAN         TO  WRK-SURYO
               PERFORM 20031-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-EPCSV-DATA (4)
               MOVE    4                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *H26.5
      *    １回服用量レコード
           IF     (ORCSEPRECSV-SURYOU-SG (IDX IDX2)  NOT =   ZERO)
              AND (ORCSEPRECSV-KAISU-SG  (IDX IDX2)  NOT =   ZERO)
               PERFORM 20032-RP241-SYORI-SEC
           END-IF
      *
           MOVE    ZERO                TO   WRK-RP2-1
      *    器材商品名
           IF      (ORCSEPRECSV-SRYCD(IDX IDX2)(1:1)  =   "7" )
              AND  (ORCSEPRECSV-SHO-NAME (IDX IDX2)   NOT =   SPACE)
               PERFORM 20033-RP281-SHO-SEC
           END-IF
      *
      *    前に破砕がある時
           COMPUTE IDX3                =   IDX2  -  1
           IF     (IDX2                >   1  )  AND
                  (ORCSEPRECSV-SRYCD(IDX IDX3) =    "099209901" )
               PERFORM 20032-RP281-SEC
           END-IF
           .
       2003-RP201-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般名例外コード変換処理
      *****************************************************************
       20039-GENERICPRICE-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-YAKKAKJNCD
      *
           MOVE    SPACE               TO  GENERICPRICE-REC
           INITIALIZE                      GENERICPRICE-REC
           MOVE    ORCSEPRECSV-YAKKAKJNCD(IDX IDX2)
                                       TO  GENERICPRICE-YAKKAKJNCD
           MOVE    SPA-SRYYMD          TO  GENERICPRICE-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  GENERICPRICE-YUKOEDYMD
           MOVE    GENERICPRICE-REC    TO  MCPDATA-REC
           MOVE    "tbl_generic_price" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_generic_price" TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICPRICE-REC
      *            一般名コード
                   MOVE    GENERICPRICE-GECODE TO  WRK-YAKKAKJNCD
               ELSE
                   INITIALIZE                  GENERICPRICE-REC
                   MOVE    1                   TO  FLG-GENERICPRICE
               END-IF
           ELSE
               INITIALIZE                  GENERICPRICE-REC
               MOVE    1                   TO  FLG-GENERICPRICE
           END-IF
           MOVE    "tbl_generic_price" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *2023/10
      *     IF      WRK-YAKKAKJNCD      =   SPACE
      *         STRING  ORCSEPRECSV-YAKKAKJNCD(IDX IDX2)(1:9)
      *                 "ZZZ"               DELIMITED  BY  SIZE
      *                                     INTO    WRK-YAKKAKJNCD
      *         END-STRING
      ****  END-IF
           .
       20039-GENERICPRICE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １回服用量レコード作成処理
      *****************************************************************
       20032-RP241-SYORI-SEC         SECTION.
      *
           COMPUTE WRK-KEI-SURYO  =   ORCSEPRECSV-SURYOU-SG (IDX IDX2)
                                  *   ORCSEPRECSV-KAISU-SG  (IDX IDX2)
           IF      WRK-KEI-SURYO      =    ORCSEPRECSV-SURYOU(IDX IDX2)
      *    服用量×服用回数＝数量
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "241"               TO  TBL-EPCSV-DATA (1)
               MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
               MOVE    WRK-H-RP2           TO  TBL-EPCSV-DATA (3)
      *        １回服用量
               MOVE    ORCSEPRECSV-SURYOU-SG (IDX IDX2)
                                           TO  WRK-SURYO
               PERFORM 20031-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-EPCSV-DATA (4)
      *        １日服用回数
               MOVE    ORCSEPRECSV-KAISU-SG (IDX IDX2)
                                           TO  WRK-SURYO
               PERFORM 20031-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-EPCSV-DATA (5)
               MOVE    5                   TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           ELSE
      *        不均等レコード
               MOVE    SPACE               TO  TBL-EPCSV-AREA
               MOVE    "221"               TO  TBL-EPCSV-DATA (1)
               MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
               MOVE    WRK-H-RP2           TO  TBL-EPCSV-DATA (3)
      *        １回服用量
               MOVE    ORCSEPRECSV-SURYOU-SG (IDX IDX2)
                                           TO  WRK-SURYO
               PERFORM 20031-SURYO-HEN-SEC
               MOVE    3                   TO  IDXF
               PERFORM VARYING    IDXF2    FROM    1   BY  1
                       UNTIL     (IDXF2    >   ORCSEPRECSV-KAISU-SG
                                                         (IDX IDX2))
                              OR (IDXF2    >   5    )
                   ADD     1               TO  IDXF
                   PERFORM 20031-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-EPCSV-DATA(IDXF)
               END-PERFORM
               MOVE    13                  TO  TBL-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           .
       20032-RP241-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬品補助レコード作成処理
      *****************************************************************
       20030-RP281-SYORI-SEC         SECTION.
      *
           IF     (ORCSEPRECSV-SRYCD(IDX IDX2)   =   "099209901" )
               OR (WRK-RP2             =   ZERO  )
               CONTINUE
           ELSE
               MOVE    IDX2            TO  IDX3
               PERFORM 20032-RP281-SEC
           END-IF
           .
       20030-RP281-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬品補助レコード処理
      *****************************************************************
       20032-RP281-SEC         SECTION.
      *
           ADD     1                   TO  WRK-RP2-1
           MOVE    WRK-RP2-1           TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP2-1
      *
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "281"               TO  TBL-EPCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
           MOVE    WRK-H-RP2           TO  TBL-EPCSV-DATA (3)
           MOVE    WRK-H-RP2-1         TO  TBL-EPCSV-DATA (4)
      *
           EVALUATE    ORCSEPRECSV-SRYCD(IDX IDX3)
               WHEN    "099209901"
      *            粉砕
                   MOVE    "2"         TO  TBL-EPCSV-DATA (5)
               WHEN    "099209903"
      *            後発医薬品へ変更不可
                   MOVE    "3"         TO  TBL-EPCSV-DATA (5)
               WHEN    "099209906"
      *            剤型変更不可
                   MOVE    "4"         TO  TBL-EPCSV-DATA (5)
               WHEN    "099209905"
      *            含量規格変更不可
                   MOVE    "5"         TO  TBL-EPCSV-DATA (5)
      *2024/09
               WHEN    "099209909"
      *            患者希望（先発医薬品患者希望）
                   MOVE    "8"         TO  TBL-EPCSV-DATA (5)
           END-EVALUATE
      *
           MOVE    ORCSEPRECSV-TNS-NAME (IDX IDX3)
                                       TO  TBL-EPCSV-DATA (6)
           MOVE    7                   TO  TBL-MAX
           PERFORM 400-DATAOUT-SEC
           .
       20032-RP281-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬品補助レコード（商品名）処理
      *****************************************************************
       20033-RP281-SHO-SEC         SECTION.
      *
           ADD     1                   TO  WRK-RP2-1
           MOVE    WRK-RP2-1           TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP2-1
      *
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "281"               TO  TBL-EPCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
           MOVE    WRK-H-RP2           TO  TBL-EPCSV-DATA (3)
           MOVE    WRK-H-RP2-1         TO  TBL-EPCSV-DATA (4)
      *
           MOVE    ORCSEPRECSV-SHO-NAME (IDX IDX2)
                                       TO  TBL-EPCSV-DATA (6)
           MOVE    7                   TO  TBL-MAX
      *
           PERFORM 400-DATAOUT-SEC
           .
       20033-RP281-SHO-EXT.
           EXIT.
      *****************************************************************
      *    提供診療情報レコード編集処理
      *****************************************************************
       20040-RP301-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-RP1
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   10
               IF      ORCSEPRECSV-PRE-COMMENT (IDX)
                                       NOT =   SPACE
                   PERFORM 200401-RP301-HEN-SEC
               END-IF
           END-PERFORM
           .
       20040-RP301-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    提供診療情報レコード処理
      *****************************************************************
       200401-RP301-HEN-SEC         SECTION.
      *
           ADD     1                   TO  WRK-RP1
           MOVE    WRK-RP1             TO  WRK-ZZZ
           PERFORM 20011-SUJIHEN-SEC
           MOVE    WRK-H-DATA          TO  WRK-H-RP1
      *
           MOVE    SPACE               TO  TBL-EPCSV-AREA
           MOVE    "301"               TO  TBL-EPCSV-DATA (1)
           MOVE    WRK-H-RP1           TO  TBL-EPCSV-DATA (2)
      *
           MOVE    ORCSEPRECSV-PRE-NAME (IDX)
                                       TO  TBL-EPCSV-DATA (3)
           MOVE    ORCSEPRECSV-PRE-COMMENT (IDX)
                                       TO  TBL-EPCSV-DATA (4)
           MOVE    4                   TO  TBL-MAX
      *
           PERFORM 400-DATAOUT-SEC
           .
       200401-RP301-HEN-EXT.
           EXIT.
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       20031-SURYO-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)  TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
           MOVE    SPACE               TO  WRK-SURYO-H
           PERFORM VARYING     IDXM    FROM    1  BY  1
                   UNTIL       IDXM    >   12
               IF      WRK-SURYO-X(IDXM:1) NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:) TO  WRK-SURYO-H
                   MOVE    12                  TO  IDXM
               END-IF
           END-PERFORM
           .
       20031-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角カナを半角カナへ編集処理
      *****************************************************************
       20033-KANANAME-HEN-SEC         SECTION.
      *
      *    カナ氏名を半角カナ変換
           MOVE    1                   TO  IDY
           MOVE    SPACE               TO  WRK-KANANAME
           MOVE    ZERO                TO  FLG-CHK1
      *
           PERFORM VARYING IDX FROM    1   BY  2
                   UNTIL  (IDX >       60  )
                   OR     (ORCSEPRECSV-KANANAME (IDX:)   =   SPACE)
                   OR     (IDY  >   60 )
               IF     (ORCSEPRECSV-KANANAME (IDX:2)   =   "　")
                  AND (FLG-CHK1         =   1   )
                   CONTINUE
               ELSE
                   EVALUATE    ORCSEPRECSV-KANANAME (IDX:2)
                       WHEN    "　"
                       WHEN    SPACE
      **                     MOVE    " "      TO  WRK-KANANAME (IDY:1)
                           ADD     1        TO  IDY
                           MOVE    1        TO  FLG-CHK1
      *                 WHEN    SPACE
                            CONTINUE
      *
                       WHEN    "ァ"
                           MOVE    "ｧ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ア"
                           MOVE    "ｱ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ィ"
                           MOVE    "ｨ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "イ"
                           MOVE    "ｲ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ゥ"
                           MOVE    "ｩ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ウ"
                           MOVE    "ｳ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ェ"
                           MOVE    "ｪ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "エ"
                           MOVE    "ｴ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ォ"
                           MOVE    "ｫ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "オ"
                           MOVE    "ｵ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
      *
                       WHEN    "カ"
                           MOVE    "ｶ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ガ"
                           MOVE    "ｶﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "キ"
                           MOVE    "ｷ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ギ"
                           MOVE    "ｷﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ク"
                           MOVE    "ｸ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "グ"
                           MOVE    "ｸﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ケ"
                           MOVE    "ｹ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ゲ"
                           MOVE    "ｹﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "コ"
                           MOVE    "ｺ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ゴ"
                           MOVE    "ｺﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
      *
                       WHEN    "サ"
                           MOVE    "ｻ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ザ"
                           MOVE    "ｻﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "シ"
                           MOVE    "ｼ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ジ"
                           MOVE    "ｼﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ス"
                           MOVE    "ｽ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ズ"
                           MOVE    "ｽﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "セ"
                           MOVE    "ｾ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ゼ"
                           MOVE    "ｾﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ソ"
                           MOVE    "ｿ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ゾ"
                           MOVE    "ｿﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
      *
                       WHEN    "タ"
                           MOVE    "ﾀ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ダ"
                           MOVE    "ﾀﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "チ"
                           MOVE    "ﾁ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヂ"
                           MOVE    "ﾁﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ッ"
                           MOVE    "ｯ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ツ"
                           MOVE    "ﾂ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヅ"
                            MOVE  "ﾂﾞ"      TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "テ"
                           MOVE    "ﾃ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "デ"
                           MOVE  "ﾃﾞ"       TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ト"
                           MOVE    "ﾄ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ド"
                           MOVE    "ﾄﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
      * 
                       WHEN    "ナ"
                           MOVE    "ﾅ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ニ"
                           MOVE    "ﾆ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヌ"
                           MOVE    "ﾇ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ネ"
                           MOVE    "ﾈ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ノ"
                           MOVE    "ﾉ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
      *
                       WHEN    "ハ"
                           MOVE    "ﾊ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "バ"
                           MOVE    "ﾊﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "パ"
                           MOVE    "ﾊﾟ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ヒ"
                           MOVE    "ﾋ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ビ"
                           MOVE    "ﾋﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ピ"
                           MOVE    "ﾋﾟ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "フ"
                           MOVE    "ﾌ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ブ"
                           MOVE    "ﾌﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "プ"
                           MOVE    "ﾌﾟ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ヘ"
                           MOVE    "ﾍ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ベ"
                           MOVE    "ﾍﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ペ"
                           MOVE    "ﾍﾟ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ホ"
                           MOVE    "ﾎ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ボ"
                           MOVE    "ﾎﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ポ"
                           MOVE    "ﾎﾟ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
      *
                       WHEN    "マ"
                           MOVE    "ﾏ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ミ"
                           MOVE    "ﾐ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ム"
                           MOVE    "ﾑ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "メ"
                           MOVE    "ﾒ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "モ"
                           MOVE    "ﾓ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
      *
                       WHEN    "ャ"
                           MOVE    "ｬ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヤ"
                           MOVE    "ﾔ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ュ"
                           MOVE    "ｭ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                      WHEN    "ユ"
                           MOVE    "ﾕ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ョ"
                           MOVE    "ｮ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                        WHEN    "ヨ"
                           MOVE    "ﾖ"      TO  WRK-KANANAME (IDY:2)
      *
                           ADD     2        TO  IDY
                       WHEN    "ラ"
                           MOVE    "ﾗ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "リ"
                           MOVE    "ﾘ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ル"
                           MOVE    "ﾙ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "レ"
                           MOVE    "ﾚ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ロ"
                           MOVE    "ﾛ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
      *
                       WHEN    "ヮ"
                           MOVE    "ﾜ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ワ"
                           MOVE    "ﾜ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヰ"
                           MOVE    "ｲ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヱ"
                           MOVE    "ｴ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヲ"
                           MOVE    "ｦ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ン"
                           MOVE    "ﾝ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヴ"
                           MOVE    "ｳﾞ"     TO  WRK-KANANAME (IDY:4)
                           ADD     4        TO  IDY
                       WHEN    "ヵ"
                           MOVE    "ｶ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
                       WHEN    "ヶ"
                           MOVE    "ｹ"      TO  WRK-KANANAME (IDY:2)
                           ADD     2        TO  IDY
      *                A8 A1
                       WHEN    "─"
      *                A1 DD
                       WHEN    "−"
      *                A1 BE
                       WHEN    "‐"
      *                A1 BD
                       WHEN    "―"
      *                A1 BC
                       WHEN    "ー"
                           MOVE    "-"      TO  WRK-KANANAME (IDY:4)
                           ADD     1        TO  IDY
                   END-EVALUATE
      *
      *            ２０２１年４月分より英字も対象とする
                   IF      SPA-SRYYMD(1:6)      >=   "202104"
                       EVALUATE    ORCSEPRECSV-KANANAME (IDX:2)
                           WHEN    "Ａ"
                               MOVE    "A"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｂ"
                               MOVE    "B"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｃ"
                               MOVE    "C"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｄ"
                               MOVE    "D"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｅ"
                               MOVE    "E"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｆ"
                               MOVE    "F"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｇ"
                               MOVE    "G"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｈ"
                               MOVE    "H"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｉ"
                               MOVE    "I"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｊ"
                               MOVE    "J"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｋ"
                               MOVE    "K"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｌ"
                               MOVE    "L"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｍ"
                               MOVE    "M"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｎ"
                               MOVE    "N"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｏ"
                               MOVE    "O"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｐ"
                               MOVE    "P"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｑ"
                               MOVE    "Q"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｒ"
                               MOVE    "R"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｓ"
                               MOVE    "S"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｔ"
                               MOVE    "T"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｕ"
                               MOVE    "U"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｖ"
                               MOVE    "V"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｗ"
                               MOVE    "W"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｘ"
                               MOVE    "X"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｙ"
                               MOVE    "Y"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "Ｚ"
                               MOVE    "Z"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ａ"
                               MOVE    "a"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｂ"
                               MOVE    "b"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｃ"
                               MOVE    "c"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｄ"
                               MOVE    "d"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｅ"
                               MOVE    "e"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｆ"
                               MOVE    "f"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｇ"
                               MOVE    "g"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｈ"
                               MOVE    "h"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｉ"
                               MOVE    "i"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｊ"
                               MOVE    "j"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｋ"
                               MOVE    "k"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｌ"
                               MOVE    "l"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｍ"
                               MOVE    "m"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｎ"
                               MOVE    "n"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｏ"
                               MOVE    "o"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｐ"
                               MOVE    "p"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｑ"
                               MOVE    "q"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｒ"
                               MOVE    "r"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｓ"
                               MOVE    "s"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｔ"
                               MOVE    "t"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｕ"
                               MOVE    "u"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｖ"
                               MOVE    "v"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｗ"
                               MOVE    "w"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｘ"
                               MOVE    "x"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｙ"
                               MOVE    "y"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                           WHEN    "ｚ"
                               MOVE    "z"      TO  WRK-KANANAME (IDY:1)
                               ADD     1        TO  IDY
                     END-EVALUATE
                   END-IF      
               END-IF
           END-PERFORM
           .
       20033-KANANAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
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
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    用法コード対応テーブル読み込み
      *****************************************************************
       940-YOUHOUCHG-READ-SEC         SECTION.
      *
           MOVE    YOUHOUCHG-REC       TO  MCPDATA-REC
           MOVE    "tbl_youhouchg"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_youhouchg"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  YOUHOUCHG-REC
                   MOVE    ZERO            TO  FLG-YOUHOUCHG
               ELSE
                   INITIALIZE                  YOUHOUCHG-REC
                   MOVE    1               TO  FLG-YOUHOUCHG
               END-IF
           ELSE
               INITIALIZE                  YOUHOUCHG-REC
               MOVE    1               TO  FLG-YOUHOUCHG
           END-IF
      *
           MOVE    "tbl_youhouchg"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-YOUHOUCHG-READ-EXT.
           EXIT.
      *****************************************************************
      *    用法コードーブル読み込み
      *****************************************************************
       940-YOUHOU-READ-SEC         SECTION.
      *
           MOVE    YOUHOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_youhou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_youhou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  YOUHOU-REC
                   MOVE    ZERO            TO  FLG-YOUHOU
               ELSE
                   INITIALIZE                  YOUHOU-REC
                   MOVE    1               TO  FLG-YOUHOU
               END-IF
           ELSE
               INITIALIZE                  YOUHOU-REC
               MOVE    1               TO  FLG-YOUHOU
           END-IF
      *
           MOVE    "tbl_youhou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-YOUHOU-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者地域連携データ読込処理
      *****************************************************************
       920-PTPUBLIC-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTPUBLIC
               MOVE    MCPDATA-REC         TO  PTPUBLIC-REC
           ELSE
               MOVE    1                   TO  FLG-PTPUBLIC
               INITIALIZE                  PTPUBLIC-REC
           END-IF
      *
           .
       920-PTPUBLIC-READ-EXT.
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
      *    テーブル読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
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
      *
