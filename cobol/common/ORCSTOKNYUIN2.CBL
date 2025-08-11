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
       PROGRAM-ID.                 ORCSTOKNYUIN2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 特定入院料算定期間取得取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/08/06    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-小豆沢  17/11/20  (A314)認知症治療病棟入院料の
      *                                     起算日変更
      *  05.00.01    NACL-小豆沢  18/08/28  改正で算定コードが変更となった
      *                                     特定入院料の通算日数取得変更
      *  05.00.01    NACL-小豆沢  18/11/20  施設基準変更で緩和ケア病棟入院料
      *                                     の１と２が混在する場合の特定入院料
      *                                     の通算日数取得変更
      *  05.00.01    NACL-小豆沢  18/12/06  施設基準変更で地域包括ケア病棟入院料、
      *                                     地域包括ケア入院医療管理料の１と２が
      *                                     混在する場合の特定入院料の通算日数
      *                                     取得変更（特定地域は未対応）
      *  05.00.01    NACL-小豆沢  18/12/06  地域包括ケア病棟入院料、
      *                                     地域包括ケア入院医療管理料の
      *                                     通算日数取得変更
      *  05.00.01    NACL-小豆沢  19/04/17  地域包括ケア病棟入院料、
      *                                     地域包括ケア入院医療管理料の
      *                                     介護からの異動（急性増悪）対応
      *新型コロナウイルス感染症（診療報酬上臨時的取扱）対応
      *  05.00.00    NACL-小豆沢  20/05/11  特定入院料算定コード置き換え
      *  05.00.00    NACL-小豆沢  20/05/12  対象となる特定入院料の算定上限日数は
      *                                     患者状態に応じて２１日、３５日となる
      *                                     ことから３５日上限で算定を行う
      *  05.00.00    NACL-小豆沢  20/05/11  特定入院料算定コード置き換え
      *                                     （点数２倍コード新設）
      *  05.00.00    NACL-小豆沢  20/05/29  特定入院料算定コード置き換え
      *                                     （点数３倍コード新設）
      *  05.00.00    NACL-小豆沢  20/06/24  診療報酬上臨時的取扱の算定
      *                                     コード追加（２２件）
      *                                     ・救命救急入院料３（ロ）
      *                                     ・救命救急入院料４（ロ）
      *                                     ・特定手中治療室管理料２（ロ）
      *                                     ・特定手中治療室管理料４（ロ）
      *  05.00.00    NACL-小豆沢  20/09/17  地域包括ケア病棟入院料の経過措置
      *                                     終了に伴う地域包括ケア病棟特別入院料
      *                                     の追加
      *  05.00.00    NACL-小豆沢  21/01/26  診療報酬上臨時的取扱（上限延長）の
      *                                     算定対応
      *  05.02.01    ORCAMO       23/04/20  コロナ５類化特例対応
      *  05.02.02    ORCAMO       24/04/30  令和６年改正対応
      *                                     地域包括ケア病棟入院料
      *                                     地域包括ケア入院医療管理料
      *                                     特定一般病棟入院料（地域包括ケア）
      *                                     特定入院料の通算日数カウント見直し
      *  05.02.02    ORCAMO       24/08/20  精神科救急急性期医療入院料の急性増悪対応
      *  05.02.02    ORCAMO       24/09/04  A308-3 地域包括ケア病棟入院料
      *                                     日数カウント方法見直し
      *****************************************************************
      *
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
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-PTCONF                  PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
      *  （０：治療室単位以外の特定入院料、１：治療室単位の特定入院料）
           03  FLG-ROOM                    PIC 9(01).
           03  FLG-KAIGO                   PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
           03  IDY                         PIC 9(04).
           03  IDZ                         PIC 9(04).
           03  TN-IDX                      PIC 9(04).
           03  IDX1                        PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-TOKNYUIN2-SRYCD         PIC X(09)  OCCURS 5.
           03  WRK-JGN-NISSU               PIC 9(05)  OCCURS 5.
           03  WRK-SRYCD-COUNT             PIC 9(02).
           03  WRK-TOKUTEI-DAY             PIC 9(03).
           03  WRK-TOKUTEI-DAY1            PIC 9(03).
           03  WRK-TOKUTEI-DAY2            PIC 9(03).
           03  WRK-TOKUTEI-DAY3            PIC 9(03).
           03  WRK-TOKUTEI-DAY4            PIC 9(03).
           03  WRK-TOKUTEI-DAY5            PIC 9(03).
           03  WRK-TOKU-TAISYONISSU        PIC 9(05).
           03  WRK-KYUSEI-ZOUAKU-NISSU     PIC 9(05).
      *    特定入院料の会計作成後に一般の入院料で会計を作成するための
      *    領域
           03  IPN-NYUKAIKEI-SAKUSEI-FLG   PIC X(01).
           03  IPN-SANTEISTYMD             PIC X(08).
           03  WRK-IPN-HANTEI-YMD1.
               05  WRK-IPN-HANTEI-YM1.
                   07  WRK-IPN-HANTEI-Y1   PIC 9(04).
                   07  WRK-IPN-HANTEI-M1   PIC 9(02).
               05  WRK-IPN-HANTEI-D1       PIC 9(02).
           03  WRK-IPN-HANTEI-YMD2         PIC X(08).
      *
           03  WRK-WEIGHT                  PIC 9(04).
           03  WRK-TOKUTEI-ROOM-NISSU      PIC 9(04).
           03  WRK-CHIIKI-HOUKATSU-NISSU   PIC 9(04).
      *
           COPY    "CPTOKSRYCD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSK5001.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    患者個別設定
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    特定入院料算定期間取得パラメタ
           COPY    "CPORCSTOKNYUIN2.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTOKNYUIN2-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      LNK-TOKNYUIN2-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN2-RC
      *
           IF    LNK-TOKNYUIN2-SANTEIYMD(1:6)  >=   "202406"
               PERFORM    201-EDIT-SEC
           ELSE
               PERFORM    200-EDIT-SEC
           END-IF
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    特定入院料コードセット（令和６年５月まで）
      *****************************************************************
       200-EDIT-SEC                    SECTION.
      *
      **     DISPLAY  "====  ORCSTOKNYUIN2 SET  ===="
      **     DISPLAY  "LNK-TOKNYUIN2-TOKUTEI-NYUIN  = " 
      **               LNK-TOKNYUIN2-TOKUTEI-NYUIN
      **     DISPLAY  "LNK-TOKNYUIN2-IDOKBN    = " LNK-TOKNYUIN2-IDOKBN
      **     DISPLAY  "LNK-TOKNYUIN2-NYUINYMD  = " LNK-TOKNYUIN2-NYUINYMD
      **     DISPLAY  "LNK-TOKNYUIN2-SANTEIYMD = " LNK-TOKNYUIN2-SANTEIYMD
      **     DISPLAY  "LNK-TOKNYUIN2-NISSU08   = " LNK-TOKNYUIN2-NISSU08
      *
      *    特別処理
      *    精神科急性期治療病棟入院料１の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121110")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "24")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *    精神科急性期治療病棟入院料２の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121210")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "25")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *    精神科救急急性期医療入院料の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190264010")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "92")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *
      *    出生児体重の取得
           MOVE   SPACE                TO  PTCONF-REC
           INITIALIZE                      PTCONF-REC
           MOVE   LNK-TOKNYUIN2-HOSPNUM
                                       TO  PTCONF-HOSPNUM
           MOVE   LNK-TOKNYUIN2-PTID   TO  PTCONF-PTID
           MOVE   "BIRTHWEIGHT"        TO  PTCONF-CKEY
           PERFORM 900-PTCONF-READ-SEC
           IF     FLG-PTCONF           =   ZERO
               MOVE   PTCONF-CDATA(1:4)  TO  WRK-WEIGHT
           ELSE
               MOVE   9999               TO  WRK-WEIGHT
           END-IF
           DISPLAY  "ORCSTOKNYUIN2  WRK-WEIGHT = " WRK-WEIGHT
      *
      *    特定入院料コードと算定上限日数のセット（平成１８年３月３１日以前）
           IF     LNK-TOKNYUIN2-SANTEIYMD   <=   "20060331"
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料１（高度）
              WHEN      "190110210"
                  MOVE  "190110210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料１（Ａ）
              WHEN      "190110310"
                  MOVE  "190110310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２（高度）
              WHEN      "190110410"
                  MOVE  "190110410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２（Ａ）
              WHEN      "190110510"
                  MOVE  "190110510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料（重症者等比率基準未満）
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)ハイケアユニット入院医療管理料
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)新生児特定集中治療室管理料
      *      (A303)総合周産期特定集中治療室管理料
      *          ※上記入院料は出生時体重により上限日数が変動する為、
      *            このサブプログラムでは対応しない
      *      (A304)広範囲熱傷特定集中治療室管理料
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308)回復期リハビリテーション病棟入院料
              WHEN      "190075910"
                  MOVE  "190075910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  180            TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A316)診療所老人医療管理料
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    特定入院料コードと算定上限日数のセット
      *    （平成１８年４月１日から平成２０年３月３１日）
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20060401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20080331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料１（高度）
              WHEN      "190110210"
                  MOVE  "190110210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料１（Ａ）
              WHEN      "190110310"
                  MOVE  "190110310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２（高度）
              WHEN      "190110410"
                  MOVE  "190110410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２（Ａ）
              WHEN      "190110510"
                  MOVE  "190110510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190110910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料（重症者等比率基準適合）
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)ハイケアユニット入院医療管理料
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)新生児特定集中治療室管理料
      *      (A303)総合周産期特定集中治療室管理料
      *          ※上記入院料は出生時体重により上限日数が変動する為、
      *            このサブプログラムでは対応しない
      *      (A304)広範囲熱傷特定集中治療室管理料
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)精神科救急入院料
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)老人性認知症疾患治療病棟入院料１
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)老人性認知症疾患治療病棟入院料２
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)診療所老人医療管理料
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)診療所老人医療管理料（生活療養）
              WHEN      "190125010"
                  MOVE  "190125010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190125110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    特定入院料コードと算定上限日数のセット
      *    （平成２０年４月１日から平成２２年３月３１日）
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20080401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20100331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料（重症者等比率基準適合）
              WHEN      "190024610"
                  MOVE  "190024610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190074610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)ハイケアユニット入院医療管理料
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)新生児特定集中治療室管理料
      *      (A303)総合周産期特定集中治療室管理料
      *          ※上記入院料は出生時体重により上限日数が変動する為、
      *            このサブプログラムでは対応しない
      *      (A304)広範囲熱傷特定集中治療室管理料
              WHEN      "190024810"
                  MOVE  "190024810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料１
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料２
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)精神科救急入院料１
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)精神科救急入院料２
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)精神科救急・合併症入院料
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)認知症病棟入院料１
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)認知症病棟入院料２
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)診療所後期高齢者医療管理料
              WHEN      "190774110"
                  MOVE  "190774110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190774210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A316)診療所後期高齢者医療管理料（生活療養）
              WHEN      "190125010"
                  MOVE  "190125010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190125110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    特定入院料コードと算定上限日数のセット
      *    （平成２２年４月１日から平成２４年３月３１日）
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20100401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20120331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料３（救命救急入院料）
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料４（救命救急入院料）
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料１
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)ハイケアユニット入院医療管理料
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)新生児特定集中治療室管理料１
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)新生児特定集中治療室管理料２
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)総合周産期特定集中治療室管理料（新生児）
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303-2)新生児治療回復室入院医療管理料
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料１
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  90             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料２
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A311)精神科救急入院料１
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)精神科救急入院料２
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)精神科救急・合併症入院料
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)認知症治療病棟入院料１
              WHEN      "190739910"
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)認知症治療病棟入院料２
              WHEN      "190813410"
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    特定入院料コードと算定上限日数のセット
      *    （平成２４年４月１日から平成２６年３月３１日）
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20120401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20140331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料３（救命救急入院料）
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料４（救命救急入院料）
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料１
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A301-2)ハイケアユニット入院医療管理料
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A301-4)小児特定集中治療室管理料
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A302)新生児特定集中治療室管理料１
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A302)新生児特定集中治療室管理料２
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303)総合周産期特定集中治療室管理料（新生児）
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A303-2)新生児治療回復室入院医療管理料
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料１
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料１（指定地域）
              WHEN      "190150310"
                  MOVE  "190150310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料２
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A308-2)亜急性期入院医療管理料２（指定地域）
              WHEN      "190150410"
                  MOVE  "190150410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A310)緩和ケア病棟入院料
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A311)精神科救急入院料１
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311)精神科救急入院料２
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A311-3)精神科救急・合併症入院料
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
      *      (A314)認知症治療病棟入院料１
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A314)認知症治療病棟入院料２
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
      *      (A317)特定一般病棟入院料（亜急性期入院医療管理）
              WHEN      "190152610"
                  MOVE  "190152610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
      *      (A317)特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
              WHEN      "190152710"
                  MOVE  "190152710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
             END-EVALUATE
           END-IF
      *
      *    特定入院料コードと算定上限日数のセット
      *    （平成２６年４月１日から平成２８年３月３１日）
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20140401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20160331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、未対応
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、未対応
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、未対応
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は５０日限度だが、現状では判定できない
      **          為、未対応
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190075510"
                  MOVE  "190075510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190075610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A308-2)亜急性期入院医療管理料１
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)亜急性期入院医療管理料１（指定地域）
              WHEN      "190150310"
                  MOVE  "190150310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)亜急性期入院医療管理料２
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)亜急性期入院医療管理料２（指定地域）
              WHEN      "190150410"
                  MOVE  "190150410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１
              WHEN      "190175310"
                  MOVE  "190175310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（生活療養）
              WHEN      "190175410"
                  MOVE  "190175410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１
              WHEN      "190175510"
                  MOVE  "190175510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）
              WHEN      "190175610"
                  MOVE  "190175610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２
              WHEN      "190175710"
                  MOVE  "190175710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（生活療養）
              WHEN      "190175810"
                  MOVE  "190175810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２
              WHEN      "190175910"
                  MOVE  "190175910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）
              WHEN      "190176010"
                  MOVE  "190176010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（特定地域）
              WHEN      "190176110"
                  MOVE  "190176110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（生活療養）（特定地域）
              WHEN      "190176210"
                  MOVE  "190176210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（特定地域）
              WHEN      "190176310"
                  MOVE  "190176310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）（特定地域）
              WHEN      "190176410"
                  MOVE  "190176410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（特定地域）
              WHEN      "190176510"
                  MOVE  "190176510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（生活療養）（特定地域）
              WHEN      "190176610"
                  MOVE  "190176610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（特定地域）
              WHEN      "190176710"
                  MOVE  "190176710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）（特定地域）
              WHEN      "190176810"
                  MOVE  "190176810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)緩和ケア病棟入院料
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)精神科救急入院料１
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)精神科救急入院料２
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)精神科救急・合併症入院料
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料１
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料２
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（亜急性期入院医療管理）
              WHEN      "190152610"
                  MOVE  "190152610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
              WHEN      "190152710"
                  MOVE  "190152710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア入院医療管理１）
              WHEN      "190182310"
                  MOVE  "190182310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア入院医療管理２）
              WHEN      "190182410"
                  MOVE  "190182410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *    特定入院料コードと算定上限日数のセット
      *    （平成２８年４月１日から平成３０年３月３１日）
           IF    (LNK-TOKNYUIN2-SANTEIYMD   >=   "20160401")  AND
                 (LNK-TOKNYUIN2-SANTEIYMD   <=   "20180331")
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料（経過措置）
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、未対応
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、未対応
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、未対応
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は５０日限度だが、現状では判定できない
      **          為、未対応
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190194310"
                  MOVE  "190194310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190194410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A308-2)亜急性期入院医療管理料１
              WHEN      "190117410"
                  MOVE  "190117410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)亜急性期入院医療管理料１（指定地域）
              WHEN      "190150310"
                  MOVE  "190150310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)亜急性期入院医療管理料２
              WHEN      "190129910"
                  MOVE  "190129910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  IF    LNK-TOKNYUIN2-TOKUBETSU-FLG(1)  =  "1"
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  ELSE
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-2)亜急性期入院医療管理料２（指定地域）
              WHEN      "190150410"
                  MOVE  "190150410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１
              WHEN      "190175310"
                  MOVE  "190175310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（生活療養）
              WHEN      "190175410"
                  MOVE  "190175410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１
              WHEN      "190175510"
                  MOVE  "190175510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）
              WHEN      "190175610"
                  MOVE  "190175610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２
              WHEN      "190175710"
                  MOVE  "190175710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（生活療養）
              WHEN      "190175810"
                  MOVE  "190175810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２
              WHEN      "190175910"
                  MOVE  "190175910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）
              WHEN      "190176010"
                  MOVE  "190176010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（特定地域）
              WHEN      "190176110"
                  MOVE  "190176110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（生活療養）（特定地域）
              WHEN      "190176210"
                  MOVE  "190176210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（特定地域）
              WHEN      "190176310"
                  MOVE  "190176310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）（特定地域）
              WHEN      "190176410"
                  MOVE  "190176410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（特定地域）
              WHEN      "190176510"
                  MOVE  "190176510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（生活療養）（特定地域）
              WHEN      "190176610"
                  MOVE  "190176610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（特定地域）
              WHEN      "190176710"
                  MOVE  "190176710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）（特定地域）
              WHEN      "190176810"
                  MOVE  "190176810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)緩和ケア病棟入院料
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)精神科救急入院料１
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)精神科救急入院料２
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)精神科救急・合併症入院料
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料１
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料２
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（亜急性期入院医療管理）
              WHEN      "190152610"
                  MOVE  "190152610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
              WHEN      "190152710"
                  MOVE  "190152710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア入院医療管理１）
              WHEN      "190182310"
                  MOVE  "190182310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア入院医療管理２）
              WHEN      "190182410"
                  MOVE  "190182410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *
      *    特定入院料コードと算定上限日数のセット（平成３０年４月１日以降）
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20180401"
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料１（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190221450"
                  MOVE  "190221450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190221550"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190221650"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料１（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225950"
                  MOVE  "190225950"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190226050"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190226150"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190221750"
                  MOVE  "190221750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190221850"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190221950"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190226250"
                  MOVE  "190226250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190226350"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190226450"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190222050"
                  MOVE  "190222050"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190222150"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190222250"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190226550"
                  MOVE  "190226550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190226650"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190226750"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228910"
                  MOVE  "190228910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190229010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190229110"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190229210"
                  MOVE  "190229210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190229310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190229410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190222350"
                  MOVE  "190222350"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190222450"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190222550"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190226850"
                  MOVE  "190226850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190226950"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190227050"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190229510"
                  MOVE  "190229510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190229610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190229710"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190229810"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  35             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190229910"
                  MOVE  "190229910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190230010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190230110"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190230210"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  35             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190223050"
                  MOVE  "190223050"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190223150"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227150"
                  MOVE  "190227150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190227250"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190223250"
                  MOVE  "190223250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190223350"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227350"
                  MOVE  "190227350"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190227450"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190230310"
                  MOVE  "190230310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190230410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190230510"
                  MOVE  "190230510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190230610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190223450"
                  MOVE  "190223450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190223550"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227550"
                  MOVE  "190227550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190227650"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190223650"
                  MOVE  "190223650"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190223750"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227750"
                  MOVE  "190227750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190227850"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190230710"
                  MOVE  "190230710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190230810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190230910"
                  MOVE  "190230910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190231010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190224250"
                  MOVE  "190224250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227950"
                  MOVE  "190227950"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190224350"
                  MOVE  "190224350"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228050"
                  MOVE  "190228050"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190224650"
                  MOVE  "190224650"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228150"
                  MOVE  "190228150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190224850"
                  MOVE  "190224850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190224950"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228250"
                  MOVE  "190228250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190228350"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225150"
                  MOVE  "190225150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228450"
                  MOVE  "190228450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225250"
                  MOVE  "190225250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228550"
                  MOVE  "190228550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225450"
                  MOVE  "190225450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228650"
                  MOVE  "190228650"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225550"
                  MOVE  "190225550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228750"
                  MOVE  "190228750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は５０日限度だが、現状では判定できない
      **          為、未対応（通常は３０日限度）
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  50             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225750"
                  MOVE  "190225750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228850"
                  MOVE  "190228850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190194310"
                  MOVE  "190194310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190194410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１
              WHEN      "190209110"
                  MOVE  "190209110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（生活療養）
              WHEN      "190209210"
                  MOVE  "190209210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１
              WHEN      "190209310"
                  MOVE  "190209310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）
              WHEN      "190209410"
                  MOVE  "190209410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２
              WHEN      "190209510"
                  MOVE  "190209510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（生活療養）
              WHEN      "190209610"
                  MOVE  "190209610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２
              WHEN      "190209710"
                  MOVE  "190209710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）
              WHEN      "190209810"
                  MOVE  "190209810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院３
              WHEN      "190209910"
                  MOVE  "190209910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院３（生活療養）
              WHEN      "190210010"
                  MOVE  "190210010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料３
              WHEN      "190210110"
                  MOVE  "190210110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料３（生活療養）
              WHEN      "190210210"
                  MOVE  "190210210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院４
              WHEN      "190210310"
                  MOVE  "190210310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院４（生活療養）
              WHEN      "190210410"
                  MOVE  "190210410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料４
              WHEN      "190210510"
                  MOVE  "190210510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料４（生活療養）
              WHEN      "190210610"
                  MOVE  "190210610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（特定地域）
              WHEN      "190210710"
                  MOVE  "190210710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（生活療養）（特定地域）
              WHEN      "190210810"
                  MOVE  "190210810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（特定地域）
              WHEN      "190210910"
                  MOVE  "190210910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）（特定地域）
              WHEN      "190211010"
                  MOVE  "190211010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（特定地域）
              WHEN      "190211110"
                  MOVE  "190211110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（生活療養）（特定地域）
              WHEN      "190211210"
                  MOVE  "190211210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（特定地域）
              WHEN      "190211310"
                  MOVE  "190211310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）（特定地域）
              WHEN      "190211410"
                  MOVE  "190211410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院３（特定地域）
              WHEN      "190211510"
                  MOVE  "190211510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院３（生活療養）（特定地域）
              WHEN      "190211610"
                  MOVE  "190211610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料３（特定地域）
              WHEN      "190211710"
                  MOVE  "190211710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料３（生活療養）（特定地域）
              WHEN      "190211810"
                  MOVE  "190211810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院４（特定地域）
              WHEN      "190211910"
                  MOVE  "190211910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院４（生活療養）（特定地域）
              WHEN      "190212010"
                  MOVE  "190212010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料４（特定地域）
              WHEN      "190212110"
                  MOVE  "190212110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料４（生活療養）（特定地域）
              WHEN      "190212210"
                  MOVE  "190212210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)緩和ケア病棟入院料１
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)緩和ケア病棟入院料２
              WHEN      "190212710"
                  MOVE  "190212710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190212810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190212910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)精神科救急入院料１
              WHEN      "190121010"
                  MOVE  "190121010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190111410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)精神科救急入院料２
              WHEN      "190130010"
                  MOVE  "190130010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)精神科救急・合併症入院料
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料１
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料２
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア１）
              WHEN      "190213310"
                  MOVE  "190213310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア２）
              WHEN      "190213410"
                  MOVE  "190213410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア３）
              WHEN      "190213510"
                  MOVE  "190213510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア４）
              WHEN      "190213610"
                  MOVE  "190213610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料１
              WHEN      "190231110"
                  MOVE  "190231110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料１（生活療養）
              WHEN      "190231210"
                  MOVE  "190231210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料２
              WHEN      "190231310"
                  MOVE  "190231310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料２（生活療養）
              WHEN      "190231410"
                  MOVE  "190231410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料３
              WHEN      "190231510"
                  MOVE  "190231510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料３（生活療養）
              WHEN      "190231610"
                  MOVE  "190231610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料４
              WHEN      "190231710"
                  MOVE  "190231710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料４（生活療養）
              WHEN      "190231810"
                  MOVE  "190231810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料１（特定地域）
              WHEN      "190231910"
                  MOVE  "190231910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料１（生活療養）（特定地域）
              WHEN      "190232010"
                  MOVE  "190232010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料２（特定地域）
              WHEN      "190232110"
                  MOVE  "190232110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料２（生活療養）（特定地域）
              WHEN      "190232210"
                  MOVE  "190232210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料３（特定地域）
              WHEN      "190232310"
                  MOVE  "190232310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料３（生活療養）（特定地域）
              WHEN      "190232410"
                  MOVE  "190232410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料４（特定地域）
              WHEN      "190232510"
                  MOVE  "190232510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟特別入院料４（生活療養）（特定地域）
              WHEN      "190232610"
                  MOVE  "190232610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *-----------------------------------------------------------------
      *    特定入院料コードと算定上限日数のセット（令和４年４月１日以降）
      *-----------------------------------------------------------------
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20220401"
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料１（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190221450"
                  MOVE  "190221450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190221550"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190221650"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料１（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225950"
                  MOVE  "190225950"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190226050"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190226150"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料１（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190244510"
                  MOVE  "190244510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190244610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190244710"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  30             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料１（特例の患者）
      *            上限日数は患者状態に応じて３日、７日、１４日、上限超え
              WHEN      "190293550"
                  MOVE  "190293550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190293650"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190293750"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190294750"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  99999          TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190221750"
                  MOVE  "190221750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190221850"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190221950"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190226250"
                  MOVE  "190226250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190226350"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190226450"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190244810"
                  MOVE  "190244810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190244910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190245010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  30             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２（特例の患者）
      *            上限日数は患者状態に応じて３日、７日、１４日、上限超え
              WHEN      "190293850"
                  MOVE  "190293850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190293950"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190294050"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190294850"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  99999          TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190222050"
                  MOVE  "190222050"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190222150"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190222250"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190226550"
                  MOVE  "190226550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190226650"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190226750"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190245110"
                  MOVE  "190245110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190245210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190245310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  30             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）（特例の患者）
      *            上限日数は患者状態に応じて３日、７日、１４日、上限超え
              WHEN      "190294150"
                  MOVE  "190294150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190294250"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190294350"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190294950"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  99999          TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228910"
                  MOVE  "190228910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190229010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190229110"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190229210"
                  MOVE  "190229210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190229310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190229410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）（特例の患者）
      *            上限日数は患者状態に応じて３日、７日、６０日、上限超え
              WHEN      "190292750"
                  MOVE  "190292750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190292850"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190292950"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  "190292650"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  99999          TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190222350"
                  MOVE  "190222350"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190222450"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190222550"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190226850"
                  MOVE  "190226850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190226950"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190227050"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190245410"
                  MOVE  "190245410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190245510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190245610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  30             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）（特例の患者）
      *            上限日数は患者状態に応じて３日、７日、１４日、上限超え
              WHEN      "190294450"
                  MOVE  "190294450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190294550"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190294650"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190295050"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  99999          TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190229510"
                  MOVE  "190229510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190229610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190229710"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190229810"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  35             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190229910"
                  MOVE  "190229910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190230010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190230110"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190230210"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  35             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）（特例の患者）
      *            上限日数は患者状態に応じて３日、７日、１４日、上限超え
              WHEN      "190293150"
                  MOVE  "190293150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190293250"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190293350"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190293450"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  "190293050"    TO  WRK-TOKNYUIN2-SRYCD(5)
                  MOVE  99999          TO  WRK-JGN-NISSU(5)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190223050"
                  MOVE  "190223050"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190223150"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227150"
                  MOVE  "190227150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190227250"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190246510"
                  MOVE  "190246510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190246610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１（特例の患者）
              WHEN      "190296250"
                  MOVE  "190296250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190296350"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  "190297050"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190223250"
                  MOVE  "190223250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190223350"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227350"
                  MOVE  "190227350"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190227450"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190246710"
                  MOVE  "190246710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190246810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）（特例の患者）
              WHEN      "190296450"
                  MOVE  "190296450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190296550"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  "190297150"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190230310"
                  MOVE  "190230310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190230410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190230510"
                  MOVE  "190230510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190230610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）（特例の患者）
              WHEN      "190295750"
                  MOVE  "190295750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190295850"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190295650"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190223450"
                  MOVE  "190223450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190223550"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227550"
                  MOVE  "190227550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190227650"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190246910"
                  MOVE  "190246910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190247010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３（特例の患者）
              WHEN      "190296650"
                  MOVE  "190296650"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190296750"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  "190297250"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190223650"
                  MOVE  "190223650"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190223750"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227750"
                  MOVE  "190227750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190227850"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190247110"
                  MOVE  "190247110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190247210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）（特例の患者）
              WHEN      "190296850"
                  MOVE  "190296850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190296950"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  "190297350"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190230710"
                  MOVE  "190230710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190230810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190230910"
                  MOVE  "190230910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190231010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）（特例の患者）
              WHEN      "190296050"
                  MOVE  "190296050"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190296150"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190295950"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190224250"
                  MOVE  "190224250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190227950"
                  MOVE  "190227950"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１（特例の患者）
              WHEN      "190297850"
                  MOVE  "190297850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  "190297950"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190224350"
                  MOVE  "190224350"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228050"
                  MOVE  "190228050"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２（特例の患者）
              WHEN      "190298050"
                  MOVE  "190298050"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  "190298150"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料（経過措置）
              WHEN      "190117310"
                  MOVE  "190117310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190224650"
                  MOVE  "190224650"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228150"
                  MOVE  "190228150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料（特例の患者）
              WHEN      "190298450"
                  MOVE  "190298450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190298550"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190224850"
                  MOVE  "190224850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190224950"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228250"
                  MOVE  "190228250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190228350"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料（特例の患者）
              WHEN      "190298750"
                  MOVE  "190298750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190298850"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  35             TO  WRK-JGN-NISSU(2)
                  MOVE  "190298950"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  35             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225150"
                  MOVE  "190225150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228450"
                  MOVE  "190228450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１（特例の患者）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190299150"
                  MOVE  "190299150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  "190299350"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225250"
                  MOVE  "190225250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228550"
                  MOVE  "190228550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２（特例の患者）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190299250"
                  MOVE  "190299250"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  "190299450"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225450"
                  MOVE  "190225450"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228650"
                  MOVE  "190228650"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）（特例の患者）
              WHEN      "190299650"
                  MOVE  "190299650"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190299750"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225550"
                  MOVE  "190225550"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228750"
                  MOVE  "190228750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）（特例の患者）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190299850"
                  MOVE  "190299850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  "190299950"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は５０日限度だが、現状では判定できない
      **          為、未対応（通常は３０日限度）
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  50             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  135        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  140        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料（診療報酬上臨時的取扱２倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190225750"
                  MOVE  "190225750"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料（診療報酬上臨時的取扱３倍）
      *            上限日数は患者状態に応じて２１日、３５日となることから
      *            ３５日上限で算定を行う
              WHEN      "190228850"
                  MOVE  "190228850"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料（特例の患者）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は５０日限度だが、現状では判定できない
      **          為、未対応（通常は３０日限度）
              WHEN      "190300150"
                  MOVE  "190300150"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  50             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  135        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  140        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  "190300250"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190194310"
                  MOVE  "190194310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190194410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１
              WHEN      "190209110"
                  MOVE  "190209110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（生活療養）
              WHEN      "190209210"
                  MOVE  "190209210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１
              WHEN      "190209310"
                  MOVE  "190209310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）
              WHEN      "190209410"
                  MOVE  "190209410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２
              WHEN      "190209510"
                  MOVE  "190209510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（生活療養）
              WHEN      "190209610"
                  MOVE  "190209610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２
              WHEN      "190209710"
                  MOVE  "190209710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）
              WHEN      "190209810"
                  MOVE  "190209810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院３
              WHEN      "190209910"
                  MOVE  "190209910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院３（生活療養）
              WHEN      "190210010"
                  MOVE  "190210010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料３
              WHEN      "190210110"
                  MOVE  "190210110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料３（生活療養）
              WHEN      "190210210"
                  MOVE  "190210210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院４
              WHEN      "190210310"
                  MOVE  "190210310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院４（生活療養）
              WHEN      "190210410"
                  MOVE  "190210410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料４
              WHEN      "190210510"
                  MOVE  "190210510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料４（生活療養）
              WHEN      "190210610"
                  MOVE  "190210610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（特定地域）
              WHEN      "190210710"
                  MOVE  "190210710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院１（生活療養）（特定地域）
              WHEN      "190210810"
                  MOVE  "190210810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（特定地域）
              WHEN      "190210910"
                  MOVE  "190210910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）（特定地域）
              WHEN      "190211010"
                  MOVE  "190211010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（特定地域）
              WHEN      "190211110"
                  MOVE  "190211110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院２（生活療養）（特定地域）
              WHEN      "190211210"
                  MOVE  "190211210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（特定地域）
              WHEN      "190211310"
                  MOVE  "190211310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）（特定地域）
              WHEN      "190211410"
                  MOVE  "190211410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院３（特定地域）
              WHEN      "190211510"
                  MOVE  "190211510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院３（生活療養）（特定地域）
              WHEN      "190211610"
                  MOVE  "190211610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料３（特定地域）
              WHEN      "190211710"
                  MOVE  "190211710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料３（生活療養）（特定地域）
              WHEN      "190211810"
                  MOVE  "190211810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院４（特定地域）
              WHEN      "190211910"
                  MOVE  "190211910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院４（生活療養）（特定地域）
              WHEN      "190212010"
                  MOVE  "190212010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料４（特定地域）
              WHEN      "190212110"
                  MOVE  "190212110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア入院医療管理料４（生活療養）（特定地域）
              WHEN      "190212210"
                  MOVE  "190212210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)緩和ケア病棟入院料１
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)緩和ケア病棟入院料２
              WHEN      "190212710"
                  MOVE  "190212710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190212810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190212910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190264910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  90             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190265010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  90             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)精神科救急・合併症入院料
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190265110"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  90             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料１
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料２
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア１）
              WHEN      "190213310"
                  MOVE  "190213310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア２）
              WHEN      "190213410"
                  MOVE  "190213410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア３）
              WHEN      "190213510"
                  MOVE  "190213510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア４）
              WHEN      "190213610"
                  MOVE  "190213610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)精神科救急急性期医療入院料
              WHEN      "190264010"
                  MOVE  "190264010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190264110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190264210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  90             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A318)特定機能病院リハビリテーション病棟入院料
              WHEN      "190265310"
                  MOVE  "190265310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  99999          TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A308-3)地域包括ケア病棟入院
      *              地域包括ケア入院医療管理料
      *           注１
              WHEN      "190249710"
              WHEN      "190249810"
              WHEN      "190250110"
              WHEN      "190250210"
              WHEN      "190250510"
              WHEN      "190250610"
              WHEN      "190252110"
              WHEN      "190252210"
              WHEN      "190252910"
              WHEN      "190253010"
              WHEN      "190253310"
              WHEN      "190253410"
              WHEN      "190253710"
              WHEN      "190253810"
              WHEN      "190255310"
              WHEN      "190255410"
      *           注９
              WHEN      "190256110"
              WHEN      "190256210"
              WHEN      "190256910"
              WHEN      "190257010"
              WHEN      "190250710"
              WHEN      "190250810"
              WHEN      "190253910"
              WHEN      "190254010"
      *           注１０、注１１、注１２
              WHEN      "190257710"
              WHEN      "190257810"
              WHEN      "190257910"
              WHEN      "190258010"
              WHEN      "190258110"
              WHEN      "190258210"
              WHEN      "190258510"
              WHEN      "190258610"
              WHEN      "190253110"
              WHEN      "190253210"
              WHEN      "190253510"
              WHEN      "190253610"
              WHEN      "190254110"
              WHEN      "190254210"
              WHEN      "190255510"
              WHEN      "190255610"
      *           注１１
              WHEN      "190258910"
              WHEN      "190259010"
              WHEN      "190259310"
              WHEN      "190259410"
              WHEN      "190259710"
              WHEN      "190259810"
              WHEN      "190259910"
              WHEN      "190260010"
              WHEN      "190250910"
              WHEN      "190251010"
              WHEN      "190252310"
              WHEN      "190252410"
              WHEN      "190254310"
              WHEN      "190254410"
              WHEN      "190255710"
              WHEN      "190255810"
      *           注１２
              WHEN      "190260110"
              WHEN      "190260210"
              WHEN      "190260310"
              WHEN      "190260410"
              WHEN      "190260510"
              WHEN      "190260610"
              WHEN      "190260710"
              WHEN      "190260810"
              WHEN      "190249910"
              WHEN      "190250010"
              WHEN      "190250310"
              WHEN      "190250410"
              WHEN      "190251110"
              WHEN      "190251210"
              WHEN      "190252510"
              WHEN      "190252610"
      *           合成コード
              WHEN      "190251310"
              WHEN      "190251410"
              WHEN      "190251510"
              WHEN      "190251610"
              WHEN      "190251710"
              WHEN      "190251810"
              WHEN      "190251910"
              WHEN      "190252010"
              WHEN      "190252710"
              WHEN      "190252810"
              WHEN      "190254510"
              WHEN      "190254610"
              WHEN      "190254710"
              WHEN      "190254810"
              WHEN      "190254910"
              WHEN      "190255010"
              WHEN      "190255110"
              WHEN      "190255210"
              WHEN      "190255910"
              WHEN      "190256010"
              WHEN      "190256310"
              WHEN      "190256410"
              WHEN      "190256510"
              WHEN      "190256610"
              WHEN      "190256710"
              WHEN      "190256810"
              WHEN      "190257110"
              WHEN      "190257210"
              WHEN      "190257310"
              WHEN      "190257410"
              WHEN      "190257510"
              WHEN      "190257610"
              WHEN      "190258310"
              WHEN      "190258410"
              WHEN      "190258710"
              WHEN      "190258810"
              WHEN      "190259110"
              WHEN      "190259210"
              WHEN      "190259510"
              WHEN      "190259610"
                  MOVE  LNK-TOKNYUIN2-SET-SRYCD
                                       TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  60             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *    特定入院料にヒットした場合のフラグセット
           IF        WRK-TOKNYUIN2-SRYCD(1)   NOT =   SPACE
               MOVE  1                 TO  FLG-HIT
           END-IF
      *
      *    算定上限日数をプラス計算で使用する為にマイナス１しておく
           IF    (WRK-JGN-NISSU(1)     >=     1)   AND
                 (WRK-JGN-NISSU(1)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(1)  =   WRK-JGN-NISSU(1)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(2)     >=     1)   AND
                 (WRK-JGN-NISSU(2)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(2)  =   WRK-JGN-NISSU(2)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(3)     >=     1)   AND
                 (WRK-JGN-NISSU(3)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(3)  =   WRK-JGN-NISSU(3)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(4)     >=     1)   AND
                 (WRK-JGN-NISSU(4)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(4)  =   WRK-JGN-NISSU(4)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(5)     >=     1)   AND
                 (WRK-JGN-NISSU(5)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(5)  =   WRK-JGN-NISSU(5)   -  1
           END-IF
      *
      *    算定特定入院料が治療室単位以外である場合、既に算定済みの同一
      *    特定入院料について通算日数を算出する
           IF   FLG-ROOM      =    0
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             IF      WRK-TOKNYUIN2-SRYCD(1)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(1)  TO  STUSAN-TOKUTEI-SRYCD(1)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(2)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(2)  TO  STUSAN-TOKUTEI-SRYCD(2)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(3)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(3)  TO  STUSAN-TOKUTEI-SRYCD(3)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(4)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(4)  TO  STUSAN-TOKUTEI-SRYCD(4)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(5)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(5)  TO  STUSAN-TOKUTEI-SRYCD(5)
             END-IF
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *
      *    算定特定入院料が治療室単位である場合、既に算定済みの治療室
      *    単位の特定入院料について通算日数を算出する
           IF   FLG-ROOM      =    1
             PERFORM    201-TOKUTEI-ROOM-NISSU-SEC
           END-IF
      *
      *    歴作成で登録した入院履歴について「特定入院−通算対象日数」の
      *    入力分日数を加算する
           PERFORM    202-RRK-TOKUTEI-NISSU-SEC
      *
      *    以下２つの特定入院料は初回入院日（他院分は含めない）からの
      *    通算日数により算定を行う
      *    (A314)認知症治療病棟入院料１
      *    (A314)認知症治療病棟入院料２
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20160401"
              EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
                WHEN      "190151610"
                WHEN      "190151710"
                INITIALIZE                      ORCSTUSANAREA
                MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
                MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
                MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
                CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                                SPA-AREA
                COMPUTE   LNK-TOKNYUIN2-NISSU08  =  STUSAN-NISSU03 +
                                                    STUSAN-TAINREL-NISSU
                IF      LNK-TOKNYUIN2-NISSU08   NOT =  ZERO
                     COMPUTE    LNK-TOKNYUIN2-NISSU08 =
                                LNK-TOKNYUIN2-NISSU08  -  1
                END-IF
             END-EVALUATE
           END-IF
      *
      *    改正により特定入院料の算定コードが変更になった入院料について
      *    従前の算定コードの日数も通算する必要がある為、通算日数の取得
      *    を変更（平成３０年改正）
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院１
              WHEN      "190209110"
      *      (A308-3)地域包括ケア病棟入院１（生活療養）
              WHEN      "190209210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア入院医療管理料１
              WHEN      "190209310"
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）
              WHEN      "190209410"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院２
              WHEN      "190209510"
      *      (A308-3)地域包括ケア病棟入院２（生活療養）
              WHEN      "190209610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア入院医療管理料２
              WHEN      "190209710"
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）
              WHEN      "190209810"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院１（特定地域）
              WHEN      "190210710"
      *      (A308-3)地域包括ケア病棟入院１（生活療養）（特定地域）
              WHEN      "190210810"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア入院医療管理料１（特定地域）
              WHEN      "190210910"
      *      (A308-3)地域包括ケア入院医療管理料１（生活療養）（特定地域）
              WHEN      "190211010"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院２（特定地域）
              WHEN      "190211110"
      *      (A308-3)地域包括ケア病棟入院２（生活療養）（特定地域）
              WHEN      "190211210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア入院医療管理料２（特定地域）
              WHEN      "190211310"
      *      (A308-3)地域包括ケア入院医療管理料２（生活療養）（特定地域）
              WHEN      "190211410"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院３
              WHEN      "190209910"
      *      (A308-3)地域包括ケア病棟入院３（生活療養）
              WHEN      "190210010"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア入院医療管理料３
              WHEN      "190210110"
      *      (A308-3)地域包括ケア入院医療管理料３（生活療養）
              WHEN      "190210210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院４
              WHEN      "190210310"
      *      (A308-3)地域包括ケア病棟入院４（生活療養）
              WHEN      "190210410"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア入院医療管理料４
              WHEN      "190210510"
      *      (A308-3)地域包括ケア入院医療管理料４（生活療養）
              WHEN      "190210610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院３（特定地域）
              WHEN      "190211510"
      *      (A308-3)地域包括ケア病棟入院３（生活療養）（特定地域）
              WHEN      "190211610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア入院医療管理料３（特定地域）
              WHEN      "190211710"
      *      (A308-3)地域包括ケア入院医療管理料３（生活療養）（特定地域）
              WHEN      "190211810"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院４（特定地域）
              WHEN      "190211910"
      *      (A308-3)地域包括ケア病棟入院４（生活療養）（特定地域）
              WHEN      "190212010"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア入院医療管理料４（特定地域）
              WHEN      "190212110"
      *      (A308-3)地域包括ケア入院医療管理料４（生活療養）（特定地域）
              WHEN      "190212210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟特別入院料１
              WHEN      "190231110"
      *      (A308-3)地域包括ケア病棟特別入院料１（生活療養）
              WHEN      "190231210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟特別入院料２
              WHEN      "190231310"
      *      (A308-3)地域包括ケア病棟特別入院料２（生活療養）
              WHEN      "190231410"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟特別入院料３
              WHEN      "190231510"
      *      (A308-3)地域包括ケア病棟特別入院料３（生活療養）
              WHEN      "190231610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟特別入院料４
              WHEN      "190231710"
      *      (A308-3)地域包括ケア病棟特別入院料４（生活療養）
              WHEN      "190231810"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟特別入院料１（特定地域）
              WHEN      "190231910"
      *      (A308-3)地域包括ケア病棟特別入院料１（生活療養）（特定地域）
              WHEN      "190232010"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟特別入院料２（特定地域）
              WHEN      "190232110"
      *      (A308-3)地域包括ケア病棟特別入院料２（生活療養）（特定地域）
              WHEN      "190232210"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟特別入院料３（特定地域）
              WHEN      "190232310"
      *      (A308-3)地域包括ケア病棟特別入院料３（生活療養）（特定地域）
              WHEN      "190232410"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟特別入院料４（特定地域）
              WHEN      "190232510"
      *      (A308-3)地域包括ケア病棟特別入院料４（生活療養）（特定地域）
              WHEN      "190232610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院１
      *              地域包括ケア入院医療管理料
      *              注１、注９、注１０、注１１、注１２、合成コード
      *           注１
              WHEN      "190249710"
              WHEN      "190249810"
              WHEN      "190250110"
              WHEN      "190250210"
              WHEN      "190250510"
              WHEN      "190250610"
              WHEN      "190252110"
              WHEN      "190252210"
              WHEN      "190252910"
              WHEN      "190253010"
              WHEN      "190253310"
              WHEN      "190253410"
              WHEN      "190253710"
              WHEN      "190253810"
              WHEN      "190255310"
              WHEN      "190255410"
      *           注９
              WHEN      "190256110"
              WHEN      "190256210"
              WHEN      "190256910"
              WHEN      "190257010"
              WHEN      "190250710"
              WHEN      "190250810"
              WHEN      "190253910"
              WHEN      "190254010"
      *           注１０、注１１、注１２
              WHEN      "190257710"
              WHEN      "190257810"
              WHEN      "190257910"
              WHEN      "190258010"
              WHEN      "190258110"
              WHEN      "190258210"
              WHEN      "190258510"
              WHEN      "190258610"
              WHEN      "190253110"
              WHEN      "190253210"
              WHEN      "190253510"
              WHEN      "190253610"
              WHEN      "190254110"
              WHEN      "190254210"
              WHEN      "190255510"
              WHEN      "190255610"
      *           注１１
              WHEN      "190258910"
              WHEN      "190259010"
              WHEN      "190259310"
              WHEN      "190259410"
              WHEN      "190259710"
              WHEN      "190259810"
              WHEN      "190259910"
              WHEN      "190260010"
              WHEN      "190250910"
              WHEN      "190251010"
              WHEN      "190252310"
              WHEN      "190252410"
              WHEN      "190254310"
              WHEN      "190254410"
              WHEN      "190255710"
              WHEN      "190255810"
      *           注１２
              WHEN      "190260110"
              WHEN      "190260210"
              WHEN      "190260310"
              WHEN      "190260410"
              WHEN      "190260510"
              WHEN      "190260610"
              WHEN      "190260710"
              WHEN      "190260810"
              WHEN      "190249910"
              WHEN      "190250010"
              WHEN      "190250310"
              WHEN      "190250410"
              WHEN      "190251110"
              WHEN      "190251210"
              WHEN      "190252510"
              WHEN      "190252610"
      *           合成コード
              WHEN      "190251310"
              WHEN      "190251410"
              WHEN      "190251510"
              WHEN      "190251610"
              WHEN      "190251710"
              WHEN      "190251810"
              WHEN      "190251910"
              WHEN      "190252010"
              WHEN      "190252710"
              WHEN      "190252810"
              WHEN      "190254510"
              WHEN      "190254610"
              WHEN      "190254710"
              WHEN      "190254810"
              WHEN      "190254910"
              WHEN      "190255010"
              WHEN      "190255110"
              WHEN      "190255210"
              WHEN      "190255910"
              WHEN      "190256010"
              WHEN      "190256310"
              WHEN      "190256410"
              WHEN      "190256510"
              WHEN      "190256610"
              WHEN      "190256710"
              WHEN      "190256810"
              WHEN      "190257110"
              WHEN      "190257210"
              WHEN      "190257310"
              WHEN      "190257410"
              WHEN      "190257510"
              WHEN      "190257610"
              WHEN      "190258310"
              WHEN      "190258410"
              WHEN      "190258710"
              WHEN      "190258810"
              WHEN      "190259110"
              WHEN      "190259210"
              WHEN      "190259510"
              WHEN      "190259610"
                  PERFORM  201-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A317)特定一般病棟入院料（地域包括ケア１）
              WHEN      "190213310"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190213310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190182310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190182410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A317)特定一般病棟入院料（地域包括ケア２）
              WHEN      "190213410"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190213410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190182310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190182410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A317)特定一般病棟入院料（地域包括ケア３）
              WHEN      "190213510"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190213510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190182310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190182410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A317)特定一般病棟入院料（地域包括ケア４）
              WHEN      "190213610"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190213610"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190182310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190182410"             TO  STUSAN-TOKUTEI-SRYCD(3)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
      *    施設基準変更により緩和ケア病棟入院料の１と２が混在する場合に
      *    特定入院料の算定コードが変更になった入院料について
      *    従前の算定コードの日数も通算する必要がある為、通算日数の取得
      *    を変更
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A310)緩和ケア病棟入院料１
              WHEN      "190028910"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190028910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190150510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190150610"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    "190212710"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    "190212810"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    "190212910"             TO  STUSAN-TOKUTEI-SRYCD(6)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A310)緩和ケア病棟入院料２
              WHEN      "190212710"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190028910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190150510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190150610"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    "190212710"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    "190212810"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    "190212910"             TO  STUSAN-TOKUTEI-SRYCD(6)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
      *    入院料の名称及び入院料算定コード変更となった精神科救急急性期
      *    医療入院料について従前の算定コードの日数も通算する必要がある為、
      *    通算日数の取得を変更
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A311)精神科救急急性期医療入院料
              WHEN      "190264010"
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             MOVE    "190264010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    "190264110"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    "190264210"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    "190121010"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    "190111410"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    "190130010"             TO  STUSAN-TOKUTEI-SRYCD(6)
             MOVE    "190130110"             TO  STUSAN-TOKUTEI-SRYCD(7)
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
             ADD     WRK-TOKU-TAISYONISSU    TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
      *    特別処理
      *    精神科急性期治療病棟入院料１の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121110")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "24")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *    精神科急性期治療病棟入院料２の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121210")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "25")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *    精神科救急急性期医療入院料の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190264010")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "92")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *
      *    特定入院料コードが１種類の場合
           IF    WRK-SRYCD-COUNT       =   1
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              特定入院料コード（１）
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              特定入院開始日（１）
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              特定入院終了日（１）
                   IF    WRK-JGN-NISSU(1) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              一般入院会計作成フラグと作成開始日のセット
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
      *
           END-IF
      *
      *    特定入院料コードが２種類の場合
           IF    WRK-SRYCD-COUNT       =   2
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              特定入院料コード（１）
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              特定入院開始日（１）
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              特定入院終了日（１）
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（２）
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（２）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            特定入院終了日（２）
                   IF    WRK-JGN-NISSU(2) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              一般入院会計作成フラグと作成開始日のセット
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      *    特定入院料コードが３種類の場合
           IF    WRK-SRYCD-COUNT       =   3
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              特定入院料コード（１）
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              特定入院開始日（１）
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              特定入院終了日（１）
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（２）
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（２）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            特定入院終了日（２）
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1
      **             DISPLAY "=== KAKUNIN DISPLAY ==="
      **             DISPLAY  "WRK-JGN-NISSU(2) = " WRK-JGN-NISSU(2)
      **             DISPLAY  "LNK-TOKNYUIN2-NISSU08 = " 
      **                       LNK-TOKNYUIN2-NISSU08
      **             DISPLAY  "WRK-TOKUTEI-DAY1 = " WRK-TOKUTEI-DAY1
      **             DISPLAY  "WRK-TOKUTEI-DAY = " WRK-TOKUTEI-DAY
      **             DISPLAY "=== KAKUNIN DISPLAY END ==="
                   COMPUTE  WRK-TOKUTEI-DAY2 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(3)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（３）
                   MOVE   WRK-TOKNYUIN2-SRYCD(3)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（３）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            特定入院終了日（３）
                   IF    WRK-JGN-NISSU(3) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(3)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1  -
                                                 WRK-TOKUTEI-DAY2
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              一般入院会計作成フラグと作成開始日のセット
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      *    特定入院料コードが４種類の場合
           IF    WRK-SRYCD-COUNT       =   4
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              特定入院料コード（１）
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              特定入院開始日（１）
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              特定入院終了日（１）
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（２）
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（２）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            特定入院終了日（２）
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1
                   COMPUTE  WRK-TOKUTEI-DAY2 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(3)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（３）
                   MOVE   WRK-TOKNYUIN2-SRYCD(3)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（３）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            特定入院終了日（３）
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(3)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1  -
                                               WRK-TOKUTEI-DAY2
                   COMPUTE  WRK-TOKUTEI-DAY3 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                      TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(4)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（４）
                   MOVE   WRK-TOKNYUIN2-SRYCD(4)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（４）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  4
                       MOVE    LNK-TOKNYUIN2-EDYMD (3)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            特定入院終了日（４）
                   IF    WRK-JGN-NISSU(4) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(4)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1 -
                                                 WRK-TOKUTEI-DAY2 -
                                                 WRK-TOKUTEI-DAY3
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              一般入院会計作成フラグと作成開始日のセット
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      **     DISPLAY   "TN-IDX = " TN-IDX
      *    特定入院期間を経過した際の一般入院会計作成用のフラグ設定
           EVALUATE   LNK-TOKNYUIN2-IDOKBN
               WHEN   "1"
                   MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                              TO WRK-IPN-HANTEI-YMD1
                   IF      WRK-IPN-HANTEI-M1   =   12
                         ADD    1     TO      WRK-IPN-HANTEI-Y1
                         MOVE   1     TO      WRK-IPN-HANTEI-M1
                   ELSE
                         ADD    1     TO      WRK-IPN-HANTEI-M1
                   END-IF
               WHEN   "2"
                   MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                              TO WRK-IPN-HANTEI-YMD1
           END-EVALUATE
      *    月末日を算定
           INITIALIZE                STS-AREA-DAY
           INITIALIZE                LNK-DAY7-AREA
           MOVE    "71"              TO  LNK-DAY7-IRAI
           MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
           CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
      *    特定入院料の終了日付をセット
           IF     TN-IDX      NOT =     ZERO
               MOVE    LNK-TOKNYUIN2-EDYMD(TN-IDX)
                                     TO      WRK-IPN-HANTEI-YMD2
           ELSE
               MOVE    "00000000"    TO      WRK-IPN-HANTEI-YMD2
           END-IF
      *    一般入院会計作成用のフラグ設定
      **     DISPLAY "WRK-IPN-HANTEI-YMD1 = " WRK-IPN-HANTEI-YMD1
      **     DISPLAY "WRK-IPN-HANTEI-YMD2 = " WRK-IPN-HANTEI-YMD2
           IF    WRK-IPN-HANTEI-YMD1    >   WRK-IPN-HANTEI-YMD2
               MOVE    "1"        TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
               IF    WRK-IPN-HANTEI-YMD2   =   "00000000"
                   MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                  TO     LNK-TOKNYUIN2-IPN-KAIKEI-YMD
               END-IF
           ELSE
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
           IF        FLG-HIT    =    ZERO
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
      *    療養病棟（病床）の入院時は特定入院料算定後の一般入院会計は
      *    作成しない
           IF     LNK-TOKNYUIN2-TOKUBETSU-FLG(2)    =    "1"
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
      **     DISPLAY  "====  ORCSTOKNYUIN2 HENKYAKU  ===="
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(1) = " LNK-TOKNYUIN2-SRYCD(1)
      **           " " LNK-TOKNYUIN2-STYMD(1) " " LNK-TOKNYUIN2-EDYMD(1)
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(2) = " LNK-TOKNYUIN2-SRYCD(2)
      **           " " LNK-TOKNYUIN2-STYMD(2) " " LNK-TOKNYUIN2-EDYMD(2)
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(3) = " LNK-TOKNYUIN2-SRYCD(3)
      **           " " LNK-TOKNYUIN2-STYMD(3) " " LNK-TOKNYUIN2-EDYMD(3)
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(4) = " LNK-TOKNYUIN2-SRYCD(4)
      **           " " LNK-TOKNYUIN2-STYMD(4) " " LNK-TOKNYUIN2-EDYMD(4)
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(5) = " LNK-TOKNYUIN2-SRYCD(5)
      **           " " LNK-TOKNYUIN2-STYMD(5) " " LNK-TOKNYUIN2-EDYMD(5)
      **     DISPLAY  "LNK-TOKNYUIN2-IPN-KAIKEI-FLG = "
      **               LNK-TOKNYUIN2-IPN-KAIKEI-FLG
      **     DISPLAY  "LNK-TOKNYUIN2-IPN-KAIKEI-YMD = "
      **               LNK-TOKNYUIN2-IPN-KAIKEI-YMD
      *
           IF   LNK-TOKNYUIN2-SRYCD(1)  =      SPACE
               MOVE    1                TO     LNK-TOKNYUIN2-RC
           END-IF
           IF     FLG-HIT     =    ZERO
                  MOVE    2             TO     LNK-TOKNYUIN2-RC
           END-IF
      *
      *
           .
       200-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（令和６年６月から）
      *****************************************************************
       201-EDIT-SEC                    SECTION.
      *
      **     DISPLAY  "====  ORCSTOKNYUIN2 SET  ===="
      **     DISPLAY  "LNK-TOKNYUIN2-TOKUTEI-NYUIN  = " 
      **               LNK-TOKNYUIN2-TOKUTEI-NYUIN
      **     DISPLAY  "LNK-TOKNYUIN2-IDOKBN    = " LNK-TOKNYUIN2-IDOKBN
      **     DISPLAY  "LNK-TOKNYUIN2-NYUINYMD  = " LNK-TOKNYUIN2-NYUINYMD
      **     DISPLAY  "LNK-TOKNYUIN2-SANTEIYMD = " LNK-TOKNYUIN2-SANTEIYMD
      **     DISPLAY  "LNK-TOKNYUIN2-NISSU08   = " LNK-TOKNYUIN2-NISSU08
      *
      *    特別処理
      *    精神科急性期治療病棟入院料１の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121110")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "24")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *    精神科急性期治療病棟入院料２の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121210")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "25")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *    精神科救急急性期医療入院料の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190264010")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "92")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *
      *    出生児体重の取得
           MOVE   SPACE                TO  PTCONF-REC
           INITIALIZE                      PTCONF-REC
           MOVE   LNK-TOKNYUIN2-HOSPNUM
                                       TO  PTCONF-HOSPNUM
           MOVE   LNK-TOKNYUIN2-PTID   TO  PTCONF-PTID
           MOVE   "BIRTHWEIGHT"        TO  PTCONF-CKEY
           PERFORM 900-PTCONF-READ-SEC
           IF     FLG-PTCONF           =   ZERO
               MOVE   PTCONF-CDATA(1:4)  TO  WRK-WEIGHT
           ELSE
               MOVE   9999               TO  WRK-WEIGHT
           END-IF
           DISPLAY  "ORCSTOKNYUIN2  WRK-WEIGHT = " WRK-WEIGHT
      *-----------------------------------------------------------------
      *    特定入院料コードと算定上限日数のセット
      *-----------------------------------------------------------------
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20240601"
             EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
      *      (A300)救命救急入院料１
              WHEN      "190024510"
                  MOVE  "190024510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190074510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料１（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190244510"
                  MOVE  "190244510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190244610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190244710"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  30             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２
              WHEN      "190024310"
                  MOVE  "190024310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190128710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190024410"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料２（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190244810"
                  MOVE  "190244810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190244910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190245010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  30             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（救命救急入院料）
              WHEN      "190138110"
                  MOVE  "190138110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190245110"
                  MOVE  "190245110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190245210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190245310"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  30             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料３（広範囲熱傷特定集中治療）
              WHEN      "190138410"
                  MOVE  "190138410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  60             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（救命救急入院料）
              WHEN      "190138710"
                  MOVE  "190138710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190138810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190138910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190245410"
                  MOVE  "190245410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190245510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190245610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  30             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A300)救命救急入院料４（広範囲熱傷特定集中治療）
              WHEN      "190139010"
                  MOVE  "190139010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  3              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  7              TO  WRK-JGN-NISSU(2)
                  MOVE  "190139210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  14             TO  WRK-JGN-NISSU(3)
                  MOVE  "190139310"    TO  WRK-TOKNYUIN2-SRYCD(4)
                  MOVE  60             TO  WRK-JGN-NISSU(4)
                  MOVE  4              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１
              WHEN      "190174410"
                  MOVE  "190174410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料１（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190246510"
                  MOVE  "190246510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190246610"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（特定集中治療室管理料）
              WHEN      "190174610"
                  MOVE  "190174610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190246710"
                  MOVE  "190246710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190246810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）
              WHEN      "190174810"
                  MOVE  "190174810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190174910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３
              WHEN      "190116310"
                  MOVE  "190116310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190116410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料３（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190246910"
                  MOVE  "190246910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190247010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（特定集中治療室管理料）
              WHEN      "190139810"
                  MOVE  "190139810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190139910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190247110"
                  MOVE  "190247110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190247210"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）
              WHEN      "190140010"
                  MOVE  "190140010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190140110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料５
              WHEN      "190834810"
                  MOVE  "190834810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190834910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料５（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190835010"
                  MOVE  "190835010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190835110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料６（特定集中治療室管理料）
              WHEN      "190835210"
                  MOVE  "190835210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190835310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  14             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料６（急性血液浄化等の患者）
      *            上限日数は患者状態に応じて２５日、３０日となることから
      *            ３０日上限で算定を行う
              WHEN      "190835410"
                  MOVE  "190835410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190835510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  30             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301)特定集中治療室管理料６（広範囲熱傷特定集中治療）
              WHEN      "190835610"
                  MOVE  "190835610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190835710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料１
              WHEN      "190175010"
                  MOVE  "190175010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-2)ハイケアユニット入院医療管理料２
              WHEN      "190175110"
                  MOVE  "190175110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  21             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-3)脳卒中ケアユニット入院医療管理料
              WHEN      "190120810"
                  MOVE  "190120810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A301-4)小児特定集中治療室管理料
              WHEN      "190149910"
                  MOVE  "190149910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  "190150010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料１
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190024710"
                  MOVE  "190024710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A302)新生児特定集中治療室管理料２
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190140410"
                  MOVE  "190140410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（母体・胎児）
              WHEN      "190066710"
                  MOVE  "190066710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303)総合周産期特定集中治療室管理料（新生児）
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は３５日限度だが、現状では判定できない
      **          為、３５日限度としておく（通常は２１日限度）
              WHEN      "190066810"
                  MOVE  "190066810"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  35             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  105        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  110        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  60         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A303-2)新生児治療回復室入院医療管理料
      **          体重１，５００グラム以上で，厚生労働大臣が定める疾患
      **          を主病とする患者は５０日限度だが、現状では判定できない
      **          為、未対応（通常は３０日限度）
              WHEN      "190140510"
                  MOVE  "190140510"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  50             TO  WRK-JGN-NISSU(1)
                  IF    WRK-WEIGHT     <   1000
                      MOVE  120        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=   500)   AND
                       (WRK-WEIGHT     <    750)
                      MOVE  135        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     <    500)
                      MOVE  140        TO  WRK-JGN-NISSU(1)
                  END-IF
                  IF   (WRK-WEIGHT     >=  1000)   AND
                       (WRK-WEIGHT     <   1500)
                      MOVE  90         TO  WRK-JGN-NISSU(1)
                  END-IF
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A305)一類感染症患者入院医療管理料
              WHEN      "190194310"
                  MOVE  "190194310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  14             TO  WRK-JGN-NISSU(1)
                  MOVE  "190194410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  99999          TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  1              TO  FLG-ROOM
      *      (A310)緩和ケア病棟入院料１
              WHEN      "190028910"
                  MOVE  "190028910"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190150510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190150610"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A310)緩和ケア病棟入院料２
              WHEN      "190212710"
                  MOVE  "190212710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190212810"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190212910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料１
              WHEN      "190121110"
                  MOVE  "190121110"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190066910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190264910"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  90             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-2)精神科急性期治療病棟入院料２
              WHEN      "190121210"
                  MOVE  "190121210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190067010"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190265010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  90             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311-3)精神科救急・合併症入院料
              WHEN      "190130210"
                  MOVE  "190130210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190130310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190265110"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  90             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料１
              WHEN      "190151610"
                  MOVE  "190151610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190739910"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190740010"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A314)認知症治療病棟入院料２
              WHEN      "190151710"
                  MOVE  "190151710"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190813410"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190813510"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  99999          TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア１）
              WHEN      "190869010"
                  MOVE  "190869010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  40             TO  WRK-JGN-NISSU(1)
                  MOVE  "190869110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア２）
              WHEN      "190869210"
                  MOVE  "190869210"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  40             TO  WRK-JGN-NISSU(1)
                  MOVE  "190869310"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア３）
              WHEN      "190869410"
                  MOVE  "190869410"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  40             TO  WRK-JGN-NISSU(1)
                  MOVE  "190869510"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A317)特定一般病棟入院料（地域包括ケア４）
              WHEN      "190869610"
                  MOVE  "190869610"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  40             TO  WRK-JGN-NISSU(1)
                  MOVE  "190869710"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  2              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A311)精神科救急急性期医療入院料
              WHEN      "190264010"
                  MOVE  "190264010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  30             TO  WRK-JGN-NISSU(1)
                  MOVE  "190264110"    TO  WRK-TOKNYUIN2-SRYCD(2)
                  MOVE  60             TO  WRK-JGN-NISSU(2)
                  MOVE  "190264210"    TO  WRK-TOKNYUIN2-SRYCD(3)
                  MOVE  90             TO  WRK-JGN-NISSU(3)
                  MOVE  3              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A318)特定機能病院リハビリテーション病棟入院料
              WHEN      "190265310"
                  MOVE  "190265310"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  99999          TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
      *      (A302-2)新生児特定集中治療室重症児対応体制強化管理料
              WHEN      "190836010"
                  MOVE  "190836010"    TO  WRK-TOKNYUIN2-SRYCD(1)
                  MOVE  7              TO  WRK-JGN-NISSU(1)
                  MOVE  1              TO  WRK-SRYCD-COUNT
                  MOVE  0              TO  FLG-ROOM
             END-EVALUATE
           END-IF
      *
      *    (A308-3)地域包括ケア病棟入院
      *    (A308-3)地域包括ケア入院医療管理料
      *    コピー句に定義した特定入院料コードをセット
           MOVE    1               TO  IDZ
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       CONST-TOKSRYCD-MAX
               IF      LNK-TOKNYUIN2-SET-SRYCD
                                   =   CONST-TOKSRYCD-NEW (IDZ IDY 1)
                   MOVE    CONST-TOKSRYCD-NEW (IDZ IDY 1)
                                       TO  WRK-TOKNYUIN2-SRYCD (1)
                   MOVE    40          TO  WRK-JGN-NISSU       (1)
                   MOVE    CONST-TOKSRYCD-NEW (IDZ IDY 2)
                                       TO  WRK-TOKNYUIN2-SRYCD (2)
                   MOVE    60          TO  WRK-JGN-NISSU       (2)
                   MOVE    2           TO  WRK-SRYCD-COUNT
                   MOVE    0           TO  FLG-ROOM
      *
                   MOVE    CONST-TOKSRYCD-MAX
                                       TO  IDY
               END-IF    
           END-PERFORM
      *     
      *    特定入院料にヒットした場合のフラグセット
           IF        WRK-TOKNYUIN2-SRYCD(1)   NOT =   SPACE
               MOVE  1                 TO  FLG-HIT
           END-IF
      *
      *    算定上限日数をプラス計算で使用する為にマイナス１しておく
           IF    (WRK-JGN-NISSU(1)     >=     1)   AND
                 (WRK-JGN-NISSU(1)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(1)  =   WRK-JGN-NISSU(1)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(2)     >=     1)   AND
                 (WRK-JGN-NISSU(2)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(2)  =   WRK-JGN-NISSU(2)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(3)     >=     1)   AND
                 (WRK-JGN-NISSU(3)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(3)  =   WRK-JGN-NISSU(3)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(4)     >=     1)   AND
                 (WRK-JGN-NISSU(4)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(4)  =   WRK-JGN-NISSU(4)   -  1
           END-IF
           IF    (WRK-JGN-NISSU(5)     >=     1)   AND
                 (WRK-JGN-NISSU(5)     <=   99998)
               COMPUTE  WRK-JGN-NISSU(5)  =   WRK-JGN-NISSU(5)   -  1
           END-IF
      *
      *
      *    算定特定入院料が治療室単位以外である場合、既に算定済みの同一
      *    特定入院料について通算日数を算出する
           IF   FLG-ROOM      =    0
             INITIALIZE                      ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
             IF      WRK-TOKNYUIN2-SRYCD(1)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(1)  TO  STUSAN-TOKUTEI-SRYCD(1)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(2)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(2)  TO  STUSAN-TOKUTEI-SRYCD(2)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(3)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(3)  TO  STUSAN-TOKUTEI-SRYCD(3)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(4)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(4)  TO  STUSAN-TOKUTEI-SRYCD(4)
             END-IF
             IF      WRK-TOKNYUIN2-SRYCD(5)  NOT =   SPACE
                MOVE WRK-TOKNYUIN2-SRYCD(5)  TO  STUSAN-TOKUTEI-SRYCD(5)
             END-IF
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
             CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                             SPA-AREA
             MOVE    STUSAN-NISSU09          TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *
      *    算定特定入院料が治療室単位である場合、既に算定済みの治療室
      *    単位の特定入院料について通算日数を算出する
           IF   FLG-ROOM      =    1
             PERFORM    201-TOKUTEI-ROOM-NISSU-SEC
           END-IF
      *
      *    歴作成で登録した入院履歴について「特定入院−通算対象日数」の
      *    入力分日数を加算する
           PERFORM    202-RRK-TOKUTEI-NISSU-SEC
      *
      *    特別処理
      *    精神科急性期治療病棟入院料１の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121110")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "24")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *    精神科急性期治療病棟入院料２の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190121210")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "25")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *    精神科救急急性期医療入院料の急性増悪による転棟
           IF   (LNK-TOKNYUIN2-SET-SRYCD   =  "190264010")  AND
      *         (LNK-TOKNYUIN2-IDOKBN      =  "1")          AND
                (LNK-TOKNYUIN2-TOKUTEI-NYUIN   =   "92")
               MOVE    ZERO            TO  LNK-TOKNYUIN2-NISSU08
               PERFORM  202-KYUSEI-ZOUAKU-NISSU-SEC
           END-IF
      *
      *    以下２つの特定入院料は初回入院日（他院分は含めない）からの
      *    通算日数により算定を行う
      *    (A314)認知症治療病棟入院料１
      *    (A314)認知症治療病棟入院料２
           IF     LNK-TOKNYUIN2-SANTEIYMD   >=   "20160401"
              EVALUATE    LNK-TOKNYUIN2-SET-SRYCD
                WHEN      "190151610"
                WHEN      "190151710"
                INITIALIZE                      ORCSTUSANAREA
                MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
                MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
                MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
                CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                                SPA-AREA
                COMPUTE   LNK-TOKNYUIN2-NISSU08  =  STUSAN-NISSU03 +
                                                    STUSAN-TAINREL-NISSU
                IF      LNK-TOKNYUIN2-NISSU08   NOT =  ZERO
                     COMPUTE    LNK-TOKNYUIN2-NISSU08 =
                                LNK-TOKNYUIN2-NISSU08  -  1
                END-IF
             END-EVALUATE
           END-IF
      *
      *
           EVALUATE  WRK-TOKNYUIN2-SRYCD(1)
      *      (A308-3)地域包括ケア病棟入院
      *              地域包括ケア入院医療管理料
      *              注１、注９、注１０、注１１、注１２、合成コード
      *           注１
              WHEN      "190858810"
              WHEN      "190858910"
              WHEN      "190859010"
              WHEN      "190859110"
              WHEN      "190859210"
              WHEN      "190859310"
              WHEN      "190859410"
              WHEN      "190859510"
              WHEN      "190859610"
              WHEN      "190859710"
              WHEN      "190859810"
              WHEN      "190859910"
              WHEN      "190860010"
              WHEN      "190860110"
              WHEN      "190860210"
              WHEN      "190860310"
              WHEN      "190860410"
              WHEN      "190860510"
              WHEN      "190860610"
              WHEN      "190860710"
              WHEN      "190860810"
              WHEN      "190860910"
              WHEN      "190861010"
              WHEN      "190861110"
              WHEN      "190861210"
              WHEN      "190861310"
              WHEN      "190861410"
              WHEN      "190861510"
              WHEN      "190861610"
              WHEN      "190861710"
              WHEN      "190861810"
              WHEN      "190861910"
              WHEN      "190862010"
              WHEN      "190862110"
              WHEN      "190862210"
              WHEN      "190862310"
              WHEN      "190862410"
              WHEN      "190862510"
              WHEN      "190862610"
              WHEN      "190862710"
              WHEN      "190862810"
              WHEN      "190862910"
              WHEN      "190863010"
              WHEN      "190863110"
              WHEN      "190863210"
              WHEN      "190863310"
              WHEN      "190863410"
              WHEN      "190863510"
              WHEN      "190863610"
              WHEN      "190863710"
              WHEN      "190863810"
              WHEN      "190863910"
              WHEN      "190864010"
              WHEN      "190864110"
              WHEN      "190864210"
              WHEN      "190864310"
              WHEN      "190864410"
              WHEN      "190864510"
              WHEN      "190864610"
              WHEN      "190864710"
              WHEN      "190864810"
              WHEN      "190864910"
              WHEN      "190865010"
              WHEN      "190865110"
              WHEN      "190865210"
              WHEN      "190865310"
              WHEN      "190865410"
              WHEN      "190865510"
              WHEN      "190865610"
              WHEN      "190865710"
              WHEN      "190865810"
              WHEN      "190865910"
              WHEN      "190866010"
              WHEN      "190866110"
              WHEN      "190866210"
              WHEN      "190866310"
              WHEN      "190866410"
              WHEN      "190866510"
              WHEN      "190866610"
              WHEN      "190866710"
              WHEN      "190866810"
              WHEN      "190866910"
              WHEN      "190867010"
              WHEN      "190867110"
              WHEN      "190867210"
              WHEN      "190867310"
              WHEN      "190867410"
              WHEN      "190867510"
              WHEN      "190867610"
              WHEN      "190867710"
              WHEN      "190867810"
              WHEN      "190867910"
              WHEN      "190868010"
              WHEN      "190868110"
              WHEN      "190868210"
              WHEN      "190868310"
              WHEN      "190838910"
              WHEN      "190839010"
              WHEN      "190839110"
              WHEN      "190839210"
              WHEN      "190839310"
              WHEN      "190839410"
              WHEN      "190839510"
              WHEN      "190839610"
              WHEN      "190839710"
              WHEN      "190839810"
              WHEN      "190839910"
              WHEN      "190840010"
              WHEN      "190840110"
              WHEN      "190840210"
              WHEN      "190840310"
              WHEN      "190840410"
              WHEN      "190840510"
              WHEN      "190840610"
              WHEN      "190840710"
              WHEN      "190840810"
              WHEN      "190840910"
              WHEN      "190841010"
              WHEN      "190841110"
              WHEN      "190841210"
              WHEN      "190841310"
              WHEN      "190841410"
              WHEN      "190841510"
              WHEN      "190841610"
              WHEN      "190841710"
              WHEN      "190841810"
              WHEN      "190841910"
              WHEN      "190842010"
              WHEN      "190842110"
              WHEN      "190842210"
              WHEN      "190842310"
              WHEN      "190842410"
              WHEN      "190842510"
              WHEN      "190842610"
              WHEN      "190842710"
              WHEN      "190842810"
              WHEN      "190842910"
              WHEN      "190843010"
              WHEN      "190843110"
              WHEN      "190843210"
              WHEN      "190843310"
              WHEN      "190843410"
              WHEN      "190843510"
              WHEN      "190843610"
              WHEN      "190843710"
              WHEN      "190843810"
              WHEN      "190843910"
              WHEN      "190844010"
              WHEN      "190844110"
              WHEN      "190844210"
              WHEN      "190844310"
              WHEN      "190844410"
              WHEN      "190844510"
              WHEN      "190844610"
              WHEN      "190844710"
              WHEN      "190844810"
              WHEN      "190844910"
              WHEN      "190845010"
              WHEN      "190845110"
              WHEN      "190845210"
              WHEN      "190845310"
              WHEN      "190845410"
              WHEN      "190845510"
              WHEN      "190845610"
              WHEN      "190845710"
              WHEN      "190845810"
              WHEN      "190845910"
              WHEN      "190846010"
              WHEN      "190846110"
              WHEN      "190846210"
              WHEN      "190846310"
              WHEN      "190846410"
              WHEN      "190846510"
              WHEN      "190846610"
              WHEN      "190846710"
              WHEN      "190846810"
              WHEN      "190846910"
              WHEN      "190847010"
              WHEN      "190847110"
              WHEN      "190847210"
              WHEN      "190847310"
              WHEN      "190847410"
              WHEN      "190847510"
              WHEN      "190847610"
              WHEN      "190847710"
              WHEN      "190847810"
              WHEN      "190847910"
              WHEN      "190848010"
              WHEN      "190848110"
              WHEN      "190848210"
              WHEN      "190848310"
              WHEN      "190848410"
              WHEN      "190848510"
              WHEN      "190848610"
              WHEN      "190848710"
              WHEN      "190848810"
              WHEN      "190848910"
              WHEN      "190849010"
              WHEN      "190849110"
              WHEN      "190849210"
              WHEN      "190849310"
              WHEN      "190849410"
              WHEN      "190849510"
              WHEN      "190849610"
              WHEN      "190849710"
              WHEN      "190849810"
              WHEN      "190849910"
              WHEN      "190850010"
              WHEN      "190850110"
              WHEN      "190850210"
              WHEN      "190850310"
              WHEN      "190850410"
              WHEN      "190850510"
              WHEN      "190850610"
              WHEN      "190850710"
              WHEN      "190850810"
              WHEN      "190850910"
              WHEN      "190851010"
              WHEN      "190851110"
              WHEN      "190851210"
              WHEN      "190851310"
              WHEN      "190851410"
              WHEN      "190851510"
              WHEN      "190851610"
              WHEN      "190851710"
              WHEN      "190851810"
              WHEN      "190851910"
              WHEN      "190852010"
              WHEN      "190852110"
              WHEN      "190852210"
              WHEN      "190852310"
              WHEN      "190852410"
              WHEN      "190852510"
              WHEN      "190852610"
              WHEN      "190852710"
              WHEN      "190852810"
              WHEN      "190852910"
              WHEN      "190853010"
              WHEN      "190853110"
              WHEN      "190853210"
              WHEN      "190853310"
              WHEN      "190853410"
              WHEN      "190853510"
              WHEN      "190853610"
              WHEN      "190853710"
              WHEN      "190853810"
              WHEN      "190853910"
              WHEN      "190854010"
              WHEN      "190854110"
              WHEN      "190854210"
              WHEN      "190854310"
              WHEN      "190854410"
              WHEN      "190854510"
              WHEN      "190854610"
              WHEN      "190854710"
              WHEN      "190854810"
              WHEN      "190854910"
              WHEN      "190855010"
              WHEN      "190855110"
              WHEN      "190855210"
              WHEN      "190855310"
              WHEN      "190855410"
              WHEN      "190855510"
              WHEN      "190855610"
              WHEN      "190855710"
              WHEN      "190855810"
              WHEN      "190855910"
              WHEN      "190856010"
              WHEN      "190856110"
              WHEN      "190856210"
              WHEN      "190856310"
              WHEN      "190856410"
              WHEN      "190856510"
              WHEN      "190856610"
              WHEN      "190856710"
              WHEN      "190856810"
              WHEN      "190856910"
              WHEN      "190857010"
              WHEN      "190857110"
              WHEN      "190857210"
              WHEN      "190857310"
              WHEN      "190857410"
              WHEN      "190857510"
              WHEN      "190857610"
              WHEN      "190857710"
              WHEN      "190857810"
              WHEN      "190857910"
              WHEN      "190858010"
                  PERFORM  202-CHIIKI-HOUKATSU-NISSU-SEC
                  MOVE  WRK-CHIIKI-HOUKATSU-NISSU
                                             TO  LNK-TOKNYUIN2-NISSU08
           END-EVALUATE
      *
      *
      *    特定入院料コードが１種類の場合
           IF    WRK-SRYCD-COUNT       =   1
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              特定入院料コード（１）
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              特定入院開始日（１）
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              特定入院終了日（１）
                   IF    WRK-JGN-NISSU(1) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              一般入院会計作成フラグと作成開始日のセット
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
      *
           END-IF
      *
      *    特定入院料コードが２種類の場合
           IF    WRK-SRYCD-COUNT       =   2
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              特定入院料コード（１）
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              特定入院開始日（１）
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              特定入院終了日（１）
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（２）
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（２）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            特定入院終了日（２）
                   IF    WRK-JGN-NISSU(2) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              一般入院会計作成フラグと作成開始日のセット
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      *    特定入院料コードが３種類の場合
           IF    WRK-SRYCD-COUNT       =   3
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              特定入院料コード（１）
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              特定入院開始日（１）
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              特定入院終了日（１）
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（２）
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（２）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            特定入院終了日（２）
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1
      **             DISPLAY "=== KAKUNIN DISPLAY ==="
      **             DISPLAY  "WRK-JGN-NISSU(2) = " WRK-JGN-NISSU(2)
      **             DISPLAY  "LNK-TOKNYUIN2-NISSU08 = " 
      **                       LNK-TOKNYUIN2-NISSU08
      **             DISPLAY  "WRK-TOKUTEI-DAY1 = " WRK-TOKUTEI-DAY1
      **             DISPLAY  "WRK-TOKUTEI-DAY = " WRK-TOKUTEI-DAY
      **             DISPLAY "=== KAKUNIN DISPLAY END ==="
                   COMPUTE  WRK-TOKUTEI-DAY2 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(3)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（３）
                   MOVE   WRK-TOKNYUIN2-SRYCD(3)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（３）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            特定入院終了日（３）
                   IF    WRK-JGN-NISSU(3) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(3)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1  -
                                                 WRK-TOKUTEI-DAY2
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              一般入院会計作成フラグと作成開始日のセット
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      *    特定入院料コードが４種類の場合
           IF    WRK-SRYCD-COUNT       =   4
                 MOVE   ZERO           TO  TN-IDX
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(1)
                     ADD   1           TO  TN-IDX
      *              特定入院料コード（１）
                     MOVE  WRK-TOKNYUIN2-SRYCD(1)
                                       TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *              特定入院開始日（１）
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                       TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
      *              特定入院終了日（１）
                     MOVE     ZERO     TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(1)  -
                                                 LNK-TOKNYUIN2-NISSU08
                     COMPUTE  WRK-TOKUTEI-DAY1 = WRK-TOKUTEI-DAY + 1
                     INITIALIZE                STS-AREA-DAY
                     INITIALIZE                LNK-DAY6-AREA
                     MOVE    "61"              TO  LNK-DAY6-IRAI
                     MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                               TO  LNK-DAY6-KIJUN
                     MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY   TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(2)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（２）
                   MOVE   WRK-TOKNYUIN2-SRYCD(2)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（２）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   IF      TN-IDX         =   2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   ELSE
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-IF
      *            特定入院終了日（２）
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(2)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1
                   COMPUTE  WRK-TOKUTEI-DAY2 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(3)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（３）
                   MOVE   WRK-TOKNYUIN2-SRYCD(3)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（３）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            特定入院終了日（３）
                   MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                   COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(3)  -
                                               LNK-TOKNYUIN2-NISSU08 -
                                               WRK-TOKUTEI-DAY1  -
                                               WRK-TOKUTEI-DAY2
                   COMPUTE  WRK-TOKUTEI-DAY3 = WRK-TOKUTEI-DAY + 1
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN
                                      TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
                 END-IF
      *
                 IF     LNK-TOKNYUIN2-NISSU08    <=  WRK-JGN-NISSU(4)
      *
                   ADD    1   TO    TN-IDX
      *
      *            特定入院料コード（４）
                   MOVE   WRK-TOKNYUIN2-SRYCD(4)
                                        TO  LNK-TOKNYUIN2-SRYCD (TN-IDX)
      *            特定入院開始日（４）
                   INITIALIZE                 STS-AREA-DAY
                   INITIALIZE                 LNK-DAY6-AREA
                   MOVE    "61"           TO  LNK-DAY6-IRAI
                   EVALUATE  TN-IDX
                     WHEN  1
                       MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  2
                       MOVE    LNK-TOKNYUIN2-EDYMD (1)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  3
                       MOVE    LNK-TOKNYUIN2-EDYMD (2)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                     WHEN  4
                       MOVE    LNK-TOKNYUIN2-EDYMD (3)
                                          TO  LNK-DAY6-KIJUN
                       MOVE    "1"        TO  LNK-DAY6-ZOGENPTN
                       MOVE     1         TO  LNK-DAY6-ZOGEN
                       CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN
                                        TO  LNK-TOKNYUIN2-STYMD (TN-IDX)
                   END-EVALUATE
      *            特定入院終了日（４）
                   IF    WRK-JGN-NISSU(4) =   99999
                     MOVE  "99999999"   TO  LNK-TOKNYUIN2-EDYMD (TN-IDX)
                   ELSE
                     MOVE     ZERO      TO  WRK-TOKUTEI-DAY
                     COMPUTE  WRK-TOKUTEI-DAY =  WRK-JGN-NISSU(4)  -
                                                 LNK-TOKNYUIN2-NISSU08 -
                                                 WRK-TOKUTEI-DAY1 -
                                                 WRK-TOKUTEI-DAY2 -
                                                 WRK-TOKUTEI-DAY3
                     INITIALIZE              STS-AREA-DAY
                     INITIALIZE              LNK-DAY6-AREA
                     MOVE    "61"            TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-STYMD (TN-IDX)
                                             TO  LNK-DAY6-KIJUN
                     MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                     MOVE    WRK-TOKUTEI-DAY TO  LNK-DAY6-ZOGEN
                     CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                     MOVE    LNK-DAY6-KEISAN
                                        TO LNK-TOKNYUIN2-EDYMD (TN-IDX)
      *              一般入院会計作成フラグと作成開始日のセット
                     MOVE    "1"     TO   LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                     INITIALIZE                 STS-AREA-DAY
                     INITIALIZE                 LNK-DAY6-AREA
                     MOVE    "61"           TO  LNK-DAY6-IRAI
                     MOVE    LNK-TOKNYUIN2-EDYMD (TN-IDX)
                                            TO  LNK-DAY6-KIJUN
                     MOVE    "1"            TO  LNK-DAY6-ZOGENPTN
                     MOVE     1             TO  LNK-DAY6-ZOGEN
                     CALL  "ORCSDAY"        USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                     MOVE   LNK-DAY6-KEISAN TO
                                          LNK-TOKNYUIN2-IPN-KAIKEI-YMD
                   END-IF
                 END-IF
           END-IF
      *
      **     DISPLAY   "TN-IDX = " TN-IDX
      *    特定入院期間を経過した際の一般入院会計作成用のフラグ設定
           EVALUATE   LNK-TOKNYUIN2-IDOKBN
               WHEN   "1"
                   MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                              TO WRK-IPN-HANTEI-YMD1
                   IF      WRK-IPN-HANTEI-M1   =   12
                         ADD    1     TO      WRK-IPN-HANTEI-Y1
                         MOVE   1     TO      WRK-IPN-HANTEI-M1
                   ELSE
                         ADD    1     TO      WRK-IPN-HANTEI-M1
                   END-IF
               WHEN   "2"
                   MOVE    LNK-TOKNYUIN2-SANTEIYMD
                                              TO WRK-IPN-HANTEI-YMD1
           END-EVALUATE
      *    月末日を算定
           INITIALIZE                STS-AREA-DAY
           INITIALIZE                LNK-DAY7-AREA
           MOVE    "71"              TO  LNK-DAY7-IRAI
           MOVE   WRK-IPN-HANTEI-YM1 TO  LNK-DAY7-YM
           CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                             LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI   TO      WRK-IPN-HANTEI-YMD1
      *    特定入院料の終了日付をセット
           IF     TN-IDX      NOT =     ZERO
               MOVE    LNK-TOKNYUIN2-EDYMD(TN-IDX)
                                     TO      WRK-IPN-HANTEI-YMD2
           ELSE
               MOVE    "00000000"    TO      WRK-IPN-HANTEI-YMD2
           END-IF
      *    一般入院会計作成用のフラグ設定
      **     DISPLAY "WRK-IPN-HANTEI-YMD1 = " WRK-IPN-HANTEI-YMD1
      **     DISPLAY "WRK-IPN-HANTEI-YMD2 = " WRK-IPN-HANTEI-YMD2
           IF    WRK-IPN-HANTEI-YMD1    >   WRK-IPN-HANTEI-YMD2
               MOVE    "1"        TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
               IF    WRK-IPN-HANTEI-YMD2   =   "00000000"
                   MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                  TO     LNK-TOKNYUIN2-IPN-KAIKEI-YMD
               END-IF
           ELSE
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
           IF        FLG-HIT    =    ZERO
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
      *    療養病棟（病床）の入院時は特定入院料算定後の一般入院会計は
      *    作成しない
           IF     LNK-TOKNYUIN2-TOKUBETSU-FLG(2)    =    "1"
               MOVE    SPACE      TO     LNK-TOKNYUIN2-IPN-KAIKEI-FLG
                                         LNK-TOKNYUIN2-IPN-KAIKEI-YMD
           END-IF
      *
      **     DISPLAY  "====  ORCSTOKNYUIN2 HENKYAKU  ===="
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(1) = " LNK-TOKNYUIN2-SRYCD(1)
      **           " " LNK-TOKNYUIN2-STYMD(1) " " LNK-TOKNYUIN2-EDYMD(1)
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(2) = " LNK-TOKNYUIN2-SRYCD(2)
      **           " " LNK-TOKNYUIN2-STYMD(2) " " LNK-TOKNYUIN2-EDYMD(2)
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(3) = " LNK-TOKNYUIN2-SRYCD(3)
      **           " " LNK-TOKNYUIN2-STYMD(3) " " LNK-TOKNYUIN2-EDYMD(3)
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(4) = " LNK-TOKNYUIN2-SRYCD(4)
      **           " " LNK-TOKNYUIN2-STYMD(4) " " LNK-TOKNYUIN2-EDYMD(4)
      **     DISPLAY  "LNK-TOKNYUIN2-SRYCD(5) = " LNK-TOKNYUIN2-SRYCD(5)
      **           " " LNK-TOKNYUIN2-STYMD(5) " " LNK-TOKNYUIN2-EDYMD(5)
      **     DISPLAY  "LNK-TOKNYUIN2-IPN-KAIKEI-FLG = "
      **               LNK-TOKNYUIN2-IPN-KAIKEI-FLG
      **     DISPLAY  "LNK-TOKNYUIN2-IPN-KAIKEI-YMD = "
      **               LNK-TOKNYUIN2-IPN-KAIKEI-YMD
      *
           IF   LNK-TOKNYUIN2-SRYCD(1)  =      SPACE
               MOVE    1                TO     LNK-TOKNYUIN2-RC
           END-IF
           IF     FLG-HIT     =    ZERO
                  MOVE    2             TO     LNK-TOKNYUIN2-RC
           END-IF
      *
      *
           .
       201-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    治療室単位の特定入院料通算日数（平成２６年４月１日以降）
      *    新型コロナウイルス感染症患者の診療報酬上臨時的取扱コードも追加
      *    新型コロナウイルス感染症患者の上限延長コードも追加
      *****************************************************************
       201-TOKUTEI-ROOM-NISSU-SEC                    SECTION.
      *
           MOVE   ZERO                  TO  WRK-TOKUTEI-ROOM-NISSU
      *
           IF    LNK-TOKNYUIN2-NISSU08   NOT =    ZERO
      *     (A300)救命救急入院料１
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190128610"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190074510"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190221450"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190221550"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190221650"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190225950"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190226050"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE   "190226150"             TO  STUSAN-TOKUTEI-SRYCD(9)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料１（急性血液浄化等の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190244510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190244610"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190244710"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料１（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190293550"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190293650"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190293750"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190294750"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料２
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190128710"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190024410"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190221750"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190221850"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190221950"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190226250"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190226350"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE   "190226450"             TO  STUSAN-TOKUTEI-SRYCD(9)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料２（急性血液浄化等の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190244810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190244910"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190245010"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料２（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190293850"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190293950"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190294050"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190294850"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料３（救命救急入院料）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138110"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138210"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138310"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190222050"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190222150"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190222250"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190226550"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190226650"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE   "190226750"             TO  STUSAN-TOKUTEI-SRYCD(9)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料３（急性血液浄化等の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190245110"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190245210"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190245310"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料３（救命救急入院料）（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190294150"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190294250"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190294350"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190294950"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料３（広範囲熱傷特定集中治療）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138610"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190228910"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190229010"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190229110"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190229210"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190229310"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE   "190229410"             TO  STUSAN-TOKUTEI-SRYCD(9)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料３（広範囲熱傷特定集中治療）（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190292750"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190292850"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190292950"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190292650"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料４（救命救急入院料）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190138710"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190138810"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190138910"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190222350"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190222450"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190222550"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190226850"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190226950"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE   "190227050"             TO  STUSAN-TOKUTEI-SRYCD(9)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料４（急性血液浄化等の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190245410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190245510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190245610"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料４（救命救急入院料）（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190294450"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190294550"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190294650"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190295050"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料４（広範囲熱傷特定集中治療）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190139010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190139110"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190139210"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190139310"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料４（広範囲熱傷特定集中治療）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190229510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190229610"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190229710"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190229810"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190229910"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190230010"             TO  STUSAN-TOKUTEI-SRYCD(6)
             MOVE   "190230110"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190230210"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料４（広範囲熱傷特定集中治療）（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190293150"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190293250"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190293350"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190293450"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190293050"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料１
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174510"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190223050"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190223150"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190227150"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190227250"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （急性血液浄化等の患者）
             MOVE   "190246510"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190246610"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料１（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190296250"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190296350"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190297050"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料２（特定集中治療室管理料）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174610"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174710"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190223250"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190223350"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190227350"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190227450"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （急性血液浄化等の患者）
             MOVE   "190246710"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190246810"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料２（特定集中治療室管理料）（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190296450"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190296550"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190297150"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190174810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190174910"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190230310"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190230410"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190230510"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190230610"             TO  STUSAN-TOKUTEI-SRYCD(6)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料２（広範囲熱傷特定集中治療）（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190295750"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190295850"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190295650"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料３
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190116310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190116410"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190223450"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190223550"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190227550"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190227650"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （急性血液浄化等の患者）
             MOVE   "190246910"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190247010"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料３（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190296650"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190296750"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190297250"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料４（特定集中治療室管理料）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190139810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190139910"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190223650"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190223750"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190227750"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190227850"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （急性血液浄化等の患者）
             MOVE   "190247110"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190247210"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料４（特定集中治療室管理料）（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190296850"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190296950"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190297350"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190140110"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190230710"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190230810"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190230910"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190231010"             TO  STUSAN-TOKUTEI-SRYCD(6)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料４（広範囲熱傷特定集中治療）（特例の患者）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190296050"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190296150"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190295950"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-2)ハイケアユニット入院医療管理料１
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190175010"             TO  STUSAN-TOKUTEI-SRYCD(1)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190224250"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190227950"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （特例の患者）
             MOVE   "190297850"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190297950"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-2)ハイケアユニット入院医療管理料２
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190175110"             TO  STUSAN-TOKUTEI-SRYCD(1)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190224350"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190228050"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （特例の患者）
             MOVE   "190298050"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190298150"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-2)ハイケアユニット入院医療管理料
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190117310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-3)脳卒中ケアユニット入院医療管理料
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190120810"             TO  STUSAN-TOKUTEI-SRYCD(1)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190224650"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190228150"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （特例の患者）
             MOVE   "190298450"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190298550"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-4)小児特定集中治療室管理料
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190149910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190150010"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190224850"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190224950"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190228250"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE   "190228350"             TO  STUSAN-TOKUTEI-SRYCD(6)
      *      （特例の患者）
             MOVE   "190298750"             TO  STUSAN-TOKUTEI-SRYCD(7)
             MOVE   "190298850"             TO  STUSAN-TOKUTEI-SRYCD(8)
             MOVE   "190298950"             TO  STUSAN-TOKUTEI-SRYCD(9)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A302)新生児特定集中治療室管理料１
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190024710"             TO  STUSAN-TOKUTEI-SRYCD(1)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190225150"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190228450"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （特例の患者）
             MOVE   "190299150"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190299350"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A302)新生児特定集中治療室管理料２
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140410"             TO  STUSAN-TOKUTEI-SRYCD(1)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190225250"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190228550"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （特例の患者）
             MOVE   "190299250"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190299450"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A303)総合周産期特定集中治療室管理料（母体・胎児）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190066710"             TO  STUSAN-TOKUTEI-SRYCD(1)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190225450"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190228650"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （特例の患者）
             MOVE   "190299650"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190299750"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A303)総合周産期特定集中治療室管理料（新生児）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190066810"             TO  STUSAN-TOKUTEI-SRYCD(1)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190225550"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190228750"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （特例の患者）
             MOVE   "190299850"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190299950"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A303-2)新生児治療回復室入院医療管理料
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190140510"             TO  STUSAN-TOKUTEI-SRYCD(1)
      *      （診療報酬上臨時的取扱２倍）
             MOVE   "190225750"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      （診療報酬上臨時的取扱３倍）
             MOVE   "190228850"             TO  STUSAN-TOKUTEI-SRYCD(3)
      *      （特例の患者）
             MOVE   "190300150"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE   "190300250"             TO  STUSAN-TOKUTEI-SRYCD(5)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A305)一類感染症患者入院医療管理料
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190075510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190075610"             TO  STUSAN-TOKUTEI-SRYCD(2)
      *      平成２８年度改正で診療行為コード変更
             MOVE   "190194310"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190194410"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料１（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190232910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190234610"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料２（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190234710"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料３（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233110"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190234810"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190236310"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190236410"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A300)救命救急入院料４（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233210"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190234910"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190236510"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190236610"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料１（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235010"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料２（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235110"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190236710"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190236810"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料３（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235210"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301)特定集中治療室管理料４（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233610"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235310"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE   "190236910"             TO  STUSAN-TOKUTEI-SRYCD(3)
             MOVE   "190237010"             TO  STUSAN-TOKUTEI-SRYCD(4)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-2)ハイケアユニット入院医療管理料１（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233710"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235410"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-2)ハイケアユニット入院医療管理料２（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233810"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235510"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-3)脳卒中ケアユニット入院医療管理料（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190233910"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235610"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A301-4)小児特定集中治療室管理料（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190234010"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235710"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A302)新生児特定集中治療室管理料１（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190234110"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235810"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A302)新生児特定集中治療室管理料２（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190234210"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190235910"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A303-2)新生児治療回復室入院医療管理料（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190234510"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190236210"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A303)総合周産期特定集中治療室管理料（母体・胎児）（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190234310"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190236010"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *     (A303)総合周産期特定集中治療室管理料（新生児）（上限延長）
             INITIALIZE                         ORCSTUSANAREA
             MOVE    LNK-TOKNYUIN2-HOSPNUM  TO  STUSAN-HOSPNUM
             MOVE    LNK-TOKNYUIN2-PTID     TO  STUSAN-PTID
             MOVE   "190234410"             TO  STUSAN-TOKUTEI-SRYCD(1)
             MOVE   "190236110"             TO  STUSAN-TOKUTEI-SRYCD(2)
             MOVE    LNK-TOKNYUIN2-SANTEIYMD TO STUSAN-KJNYMD
             CALL   "ORCSTUSAN"          USING  ORCSTUSANAREA
                                                SPA-AREA
             ADD     STUSAN-NISSU07         TO  WRK-TOKUTEI-ROOM-NISSU
      *
      *
             MOVE    WRK-TOKUTEI-ROOM-NISSU TO  LNK-TOKNYUIN2-NISSU08
           END-IF
           .
       201-TOKUTEI-ROOM-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *  （平成３０年４月１日以降）
      *    地域包括ケア病棟入院料、地域包括ケア入院医療管理料の
      *    通算日数取得
      *    ※平成３０年３月３１日までの算定コードもカウントすること
      *****************************************************************
       201-CHIIKI-HOUKATSU-NISSU-SEC                    SECTION.
      *
           MOVE   ZERO                  TO  WRK-CHIIKI-HOUKATSU-NISSU
           MOVE   ZERO                  TO  FLG-KAIGO
      *
      *    最終の入院履歴が介護からの異動（急性増悪）によるものか
      *    判定用に入院履歴の読み込み
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM
                                        TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID   TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
               MOVE    "key9"           TO  MCP-PATHNAME
               PERFORM 920-PTNYUINRRK-FETCH-SEC
               IF     FLG-PTNYUINRRK    =   ZERO
                   IF   PTNYUINRRK-KAIGOKBN   =   "2"
                       MOVE   1         TO  FLG-KAIGO
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"       TO   MCP-TABLE
           MOVE    "key9"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      **     DISPLAY "*******CHIIKI-HOUKATSU-NISSU****************"
      **     DISPLAY "PTNYUINRRK-RRKNUM = " PTNYUINRRK-RRKNUM
      **     DISPLAY "PTNYUINRRK-RRKEDANUM = " PTNYUINRRK-RRKEDANUM
      **     DISPLAY "PTNYUINRRK-KAIGOKBN = " PTNYUINRRK-KAIGOKBN
      *
      *    (A308-3)地域包括ケア病棟入院１
      *    (A308-3)地域包括ケア病棟入院１（生活療養）
      *    (A308-3)地域包括ケア病棟入院２
      *    (A308-3)地域包括ケア病棟入院２（生活療養）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
      *    地域包括ケア病棟入院１
           MOVE    "190209110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190209210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190175310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190175410"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *    地域包括ケア病棟入院２
           MOVE    "190209510"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190209610"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190175710"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190175810"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア入院医療管理料１
      *    (A308-3)地域包括ケア入院医療管理料１（生活療養）
      *    (A308-3)地域包括ケア入院医療管理料２
      *    (A308-3)地域包括ケア入院医療管理料２（生活療養）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
      *    地域包括ケア入院医療管理料１
           MOVE    "190209310"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190209410"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190175510"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190175610"             TO  STUSAN-TOKUTEI-SRYCD(4)
      *    地域包括ケア入院医療管理料２
           MOVE    "190209710"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190209810"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190175910"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190176010"             TO  STUSAN-TOKUTEI-SRYCD(8)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟入院１（特定地域）
      *    (A308-3)地域包括ケア病棟入院１（生活療養）（特定地域）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190210710"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190210810"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190176110"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190176210"             TO  STUSAN-TOKUTEI-SRYCD(4)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟入院２（特定地域）
      *    (A308-3)地域包括ケア病棟入院２（生活療養）（特定地域）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190211110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190211210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190176510"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190176610"             TO  STUSAN-TOKUTEI-SRYCD(4)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア入院医療管理料１（特定地域）
      *    (A308-3)地域包括ケア入院医療管理料１（生活療養）（特定地域）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190210910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190211010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190176310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190176410"             TO  STUSAN-TOKUTEI-SRYCD(4)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア入院医療管理料２（特定地域）
      *    (A308-3)地域包括ケア入院医療管理料２（生活療養）（特定地域）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190211310"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190211410"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190176710"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190176810"             TO  STUSAN-TOKUTEI-SRYCD(4)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟入院３
      *    (A308-3)地域包括ケア病棟入院３（生活療養）
      *    (A308-3)地域包括ケア入院医療管理料３
      *    (A308-3)地域包括ケア入院医療管理料３（生活療養）
      *    (A308-3)地域包括ケア病棟入院４
      *    (A308-3)地域包括ケア病棟入院４（生活療養）
      *    (A308-3)地域包括ケア入院医療管理料４
      *    (A308-3)地域包括ケア入院医療管理料４（生活療養）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190209910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190210010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190210110"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190210210"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190210310"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190210410"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190210510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190210610"             TO  STUSAN-TOKUTEI-SRYCD(8)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟入院３（特定地域）
      *    (A308-3)地域包括ケア病棟入院３（生活療養）（特定地域）
      *    (A308-3)地域包括ケア入院医療管理料３（特定地域）
      *    (A308-3)地域包括ケア入院医療管理料３（生活療養）（特定地域）
      *    (A308-3)地域包括ケア病棟入院４（特定地域）
      *    (A308-3)地域包括ケア病棟入院４（生活療養）（特定地域）
      *    (A308-3)地域包括ケア入院医療管理料４（特定地域）
      *    (A308-3)地域包括ケア入院医療管理料４（生活療養）（特定地域）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190211510"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190211610"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190211710"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190211810"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190211910"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190212010"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190212110"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190212210"             TO  STUSAN-TOKUTEI-SRYCD(8)
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟特別入院料１
      *    (A308-3)地域包括ケア病棟特別入院料１（生活療養）
      *    (A308-3)地域包括ケア病棟特別入院料２
      *    (A308-3)地域包括ケア病棟特別入院料２（生活療養）
      *    (A308-3)地域包括ケア病棟特別入院料３
      *    (A308-3)地域包括ケア病棟特別入院料３（生活療養）
      *    (A308-3)地域包括ケア病棟特別入院料４
      *    (A308-3)地域包括ケア病棟特別入院料４（生活療養）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190231110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190231210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190231310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190231410"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190231510"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190231610"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190231710"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190231810"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟特別入院料１（特定地域）
      *    (A308-3)地域包括ケア病棟特別入院料１（生活療養）（特定地域）
      *    (A308-3)地域包括ケア病棟特別入院料２（特定地域）
      *    (A308-3)地域包括ケア病棟特別入院料２（生活療養）（特定地域）
      *    (A308-3)地域包括ケア病棟特別入院料３（特定地域）
      *    (A308-3)地域包括ケア病棟特別入院料３（生活療養）（特定地域）
      *    (A308-3)地域包括ケア病棟特別入院料４（特定地域）
      *    (A308-3)地域包括ケア病棟特別入院料４（生活療養）（特定地域）
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190231910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190232010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190232110"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190232210"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190232310"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190232410"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190232510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190232610"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注１
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190249710"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190249810"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190250110"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190250210"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190250510"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190250610"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190252110"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190252210"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注１
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190252910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190253010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190253310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190253410"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190253710"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190253810"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190255310"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190255410"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注９
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190256110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190256210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190256910"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190257010"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190250710"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190250810"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190253910"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190254010"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注10
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190257710"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190257810"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190257910"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190258010"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190258110"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190258210"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190258510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190258610"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注10
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190253110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190253210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190253510"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190253610"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190254110"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190254210"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190255510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190255610"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注11
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190258910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190259010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190259310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190259410"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190259710"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190259810"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190259910"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190260010"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注11
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190250910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190251010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190252310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190252410"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190254310"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190254410"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190255710"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190255810"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注12
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190260110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190260210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190260310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190260410"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190260510"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190260610"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190260710"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190260810"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    注12
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190249910"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190250010"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190250310"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190250410"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190251110"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190251210"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190252510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190252610"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    gousei-code
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190251310"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190251410"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190251510"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190251610"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190251710"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190251810"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190251910"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190252010"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    gousei-code
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190252710"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190252810"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190254510"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190254610"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190254710"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190254810"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190254910"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190255010"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    gousei-code
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190255110"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190255210"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190255910"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190256010"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190256310"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190256410"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190256510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190256610"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    gousei-code
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190256710"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190256810"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190257110"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190257210"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190257310"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190257410"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190257510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190257610"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    (A308-3)地域包括ケア病棟院料１
      *    (A308-3)地域包括ケア入院医療管理料
      *    gousei-code
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           MOVE    "190258310"             TO  STUSAN-TOKUTEI-SRYCD(1)
           MOVE    "190258410"             TO  STUSAN-TOKUTEI-SRYCD(2)
           MOVE    "190258710"             TO  STUSAN-TOKUTEI-SRYCD(3)
           MOVE    "190258810"             TO  STUSAN-TOKUTEI-SRYCD(4)
           MOVE    "190259110"             TO  STUSAN-TOKUTEI-SRYCD(5)
           MOVE    "190259210"             TO  STUSAN-TOKUTEI-SRYCD(6)
           MOVE    "190259510"             TO  STUSAN-TOKUTEI-SRYCD(7)
           MOVE    "190259610"             TO  STUSAN-TOKUTEI-SRYCD(8)
      *
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
           ADD     WRK-TOKU-TAISYONISSU    TO  WRK-CHIIKI-HOUKATSU-NISSU
      *
           .
       201-CHIIKI-HOUKATSU-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *  （令和６年６月１日以降）
      *    地域包括ケア病棟入院料、地域包括ケア入院医療管理料の
      *    通算日数取得
      *    ※令和６年５月３１日までの算定コードもカウントすること
      *****************************************************************
       202-CHIIKI-HOUKATSU-NISSU-SEC                    SECTION.
      *
           MOVE   ZERO                  TO  WRK-CHIIKI-HOUKATSU-NISSU
           MOVE   ZERO                  TO  FLG-KAIGO
      *
      *    最終の入院履歴が介護からの異動（急性増悪）によるものか
      *    判定用に入院履歴の読み込み
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM
                                        TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID   TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key9"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
               MOVE    "key9"           TO  MCP-PATHNAME
               PERFORM 920-PTNYUINRRK-FETCH-SEC
               IF     FLG-PTNYUINRRK    =   ZERO
                   IF   PTNYUINRRK-KAIGOKBN   =   "2"
                       MOVE   1         TO  FLG-KAIGO
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"       TO   MCP-TABLE
           MOVE    "key9"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      **     DISPLAY "*******CHIIKI-HOUKATSU-NISSU****************"
      **     DISPLAY "PTNYUINRRK-RRKNUM = " PTNYUINRRK-RRKNUM
      **     DISPLAY "PTNYUINRRK-RRKEDANUM = " PTNYUINRRK-RRKEDANUM
      **     DISPLAY "PTNYUINRRK-KAIGOKBN = " PTNYUINRRK-KAIGOKBN
      *
      *    令和６年５月３１日までの地域包括ケア
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           INITIALIZE                          TNS-TENSU-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  TNS-HOSPNUM
           MOVE    "A"                     TO  TNS-CDKBN-KBN
           MOVE    308                     TO  TNS-CDKBN-KBNNUM
           MOVE    3                       TO  TNS-CDKBN-KBNNUM-EDA
           MOVE    "20240531"              TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
           MOVE    "tbl_tensu"             TO  MCP-TABLE
           MOVE    "key91e"                TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC                  =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key91e"            TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               MOVE    ZERO                TO  IDX1
               PERFORM     UNTIL
                           (IDX1        >      500)   OR
                           (MCP-RC      NOT =  0)
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   IF      TNS-DATAKBN     =    1
                         ADD     1     TO   IDX1
      **                   DISPLAY "IDX1 = " IDX1
                         MOVE  TNS-SRYCD  TO  STUSAN-TOKUTEI-SRYCD(IDX1)
      **                   DISPLAY "STUSAN-TOKUTEI-SRYCD(IDX1) = "
      **                            STUSAN-TOKUTEI-SRYCD(IDX1) 
                   END-IF 
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key91e"            TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_tensu"           TO   MCP-TABLE
           MOVE    "key91e"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC

           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    令和６年６月１日からの地域包括ケア
           INITIALIZE                      ORCSTUSANAREA
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID      TO  STUSAN-PTID
           MOVE    LNK-TOKNYUIN2-SANTEIYMD TO  STUSAN-KJNYMD
           INITIALIZE                          TNS-TENSU-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM   TO  TNS-HOSPNUM
           MOVE    "A"                     TO  TNS-CDKBN-KBN
           MOVE    308                     TO  TNS-CDKBN-KBNNUM
           MOVE    3                       TO  TNS-CDKBN-KBNNUM-EDA
           MOVE    "20240601"              TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
           MOVE    "tbl_tensu"             TO  MCP-TABLE
           MOVE    "key91e"                TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC                  =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key91e"            TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               PERFORM     UNTIL
                           (IDX1        >      500)   OR
                           (MCP-RC      NOT =  0)
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   IF      TNS-DATAKBN     =    1
                         ADD     1     TO   IDX1
      **                   DISPLAY "IDX1 = " IDX1
                         MOVE  TNS-SRYCD  TO  STUSAN-TOKUTEI-SRYCD(IDX1)
      **                   DISPLAY "STUSAN-TOKUTEI-SRYCD(IDX1) = "
      **                            STUSAN-TOKUTEI-SRYCD(IDX1) 
                   END-IF 
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key91e"            TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_tensu"           TO   MCP-TABLE
           MOVE    "key91e"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC

           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF      FLG-KAIGO               =   ZERO
               ADD     STUSAN-NISSU09      TO  WRK-CHIIKI-HOUKATSU-NISSU
           ELSE
               ADD     STUSAN-NISSU07      TO  WRK-CHIIKI-HOUKATSU-NISSU
           END-IF
      *
      *    歴作成で登録した「特定入院?通算対象日数」
           ADD     WRK-TOKU-TAISYONISSU    TO  WRK-CHIIKI-HOUKATSU-NISSU
           .
       202-CHIIKI-HOUKATSU-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    歴作成で登録した入院履歴について「特定入院−通算対象日数」の
      *    入力分日数を加算する
      *****************************************************************
       202-RRK-TOKUTEI-NISSU-SEC                    SECTION.
      *
           MOVE    ZERO                 TO  WRK-TOKU-TAISYONISSU
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM
                                        TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID   TO  PTNYUINRRK-PTID
           MOVE    LNK-TOKNYUIN2-SHONUM TO  PTNYUINRRK-SHONUM
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key65"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
               MOVE    "key65"          TO  MCP-PATHNAME
               PERFORM 920-PTNYUINRRK-FETCH-SEC
               PERFORM    UNTIL   FLG-PTNYUINRRK  =   1
      *
                  IF      PTNYUINRRK-TOKU-TAISYONISSU  NOT =  ZERO
                      ADD PTNYUINRRK-TOKU-TAISYONISSU  TO
                          LNK-TOKNYUIN2-NISSU08
                      ADD PTNYUINRRK-TOKU-TAISYONISSU  TO
                          WRK-TOKU-TAISYONISSU
                  END-IF
      *
                  MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
                  MOVE    "key65"          TO  MCP-PATHNAME
                  PERFORM 920-PTNYUINRRK-FETCH-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"       TO   MCP-TABLE
           MOVE    "key65"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       202-RRK-TOKUTEI-NISSU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    急性増悪による異動時の基準日までの通算日数取得
      *****************************************************************
       202-KYUSEI-ZOUAKU-NISSU-SEC                    SECTION.
      *
           MOVE    ZERO                 TO  WRK-KYUSEI-ZOUAKU-NISSU
           INITIALIZE                       PTNYUINRRK-REC
           MOVE    LNK-TOKNYUIN2-HOSPNUM
                                        TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-TOKNYUIN2-PTID   TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key36"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_ptnyuinrrk" TO  MCP-TABLE
               MOVE    "key36"          TO  MCP-PATHNAME
               PERFORM 920-PTNYUINRRK-FETCH-SEC
      **         DISPLAY   "***************************************"
      **         DISPLAY   "PTNYUINRRK-TENNYUYMD = " PTNYUINRRK-TENNYUYMD
      **         DISPLAY   "LNK-TOKNYUIN2-SANTEIYMD = "
      **                    LNK-TOKNYUIN2-SANTEIYMD
      *        前日算出
               INITIALIZE                STS-AREA-DAY
               INITIALIZE                LNK-DAY6-AREA
               MOVE    "61"              TO  LNK-DAY6-IRAI
               MOVE   LNK-TOKNYUIN2-SANTEIYMD
                                         TO  LNK-DAY6-KIJUN
               MOVE   "1"                TO  LNK-DAY6-ZOGENPTN
               MOVE   -1                 TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                 LNK-DAY6-AREA
      **         DISPLAY "LNK-DAY6-KOYOMI = "   LNK-DAY6-KOYOMI
      **         DISPLAY "LNK-DAY6-KEISAN = "   LNK-DAY6-KEISAN
      *        特定入院料算定済み日数計算
               INITIALIZE                STS-AREA-DAY
               INITIALIZE                LNK-DAY5-AREA
               MOVE    "51"              TO  LNK-DAY5-IRAI
               MOVE   PTNYUINRRK-TENNYUYMD
                                         TO  LNK-DAY5-START
               MOVE   LNK-DAY6-KEISAN
                                         TO  LNK-DAY5-END
               CALL    "ORCSDAY"         USING   STS-AREA-DAY
                                                 LNK-DAY5-AREA
      **         DISPLAY "LNK-DAY5-NISSU2 = "   LNK-DAY5-NISSU2
               MOVE     LNK-DAY5-NISSU2   TO  LNK-TOKNYUIN2-NISSU08
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"       TO   MCP-TABLE
           MOVE    "key36"                TO   MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       202-KYUSEI-ZOUAKU-NISSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別設定テーブル読込処理
      *****************************************************************
       900-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTCONF-REC
                   MOVE    ZERO        TO  FLG-PTCONF
               ELSE
                   INITIALIZE              PTCONF-REC
                   MOVE    1           TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTCONF-READ-EXT.
           EXIT.
      *
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
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       920-PTNYUINRRK-FETCH-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF.
      *
       920-PTNYUINRRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
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
