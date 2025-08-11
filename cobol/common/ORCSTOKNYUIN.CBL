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
       PROGRAM-ID.                 ORCSTOKNYUIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 特定入院料コード取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/08    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *令和２年４月改正対応
      *  05.00.00    NACL-小豆沢  20/03/09  短期滞在３追加対応
      *
      *新型コロナウイルス感染症（診療報酬上臨時的取扱）対応
      *  05.00.00    NACL-小豆沢  20/05/01  特定入院料算定コード置き換え
      *                                     （点数２倍コード新設）
      *  05.00.00    NACL-小豆沢  20/05/29  特定入院料算定コード置き換え
      *                                     （点数３倍コード新設）
      *  05.00.00    NACL-小豆沢  20/06/18  特定入院料算定要件区分を判定し
      *                                     点数２倍コード３倍コードの
      *                                     どちらでも算定可とする
      *  05.00.00    NACL-小豆沢  20/06/24  診療報酬上臨時的取扱の算定
      *                                     コード追加（２２件）
      *                                     ・救命救急入院料３（ロ）
      *                                     ・救命救急入院料４（ロ）
      *                                     ・特定手中治療室管理料２（ロ）
      *                                     ・特定手中治療室管理料４（ロ）
      *  05.00.00    NACL-小豆沢  20/09/17  地域包括ケア病棟入院料の経過措置
      *                                     終了に伴う地域包括ケア病棟特別入院料
      *                                     の追加
      *  05.00.00    NACL-小豆沢  21/01/21  一般病棟特別入院基本料（療養病床・診療報酬上臨時的取扱）
      *                                     の追加対応（令和２年２月から）"CJ"
      *  05.00.00    NACL-小豆沢  21/01/26  診療報酬上臨時的取扱（上限延長）の
      *                                     算定対応
      *  05.01.00    ORCAMO       22/11/16  令和４年新設
      *                                     地域包括ケア病棟入院（療養病床）
      *                                     地域包括ケア入院医療管理料（療養病床）
      *                                     回復期リハビリテーション病棟入院料５
      *                                     の算定要件外対応
      *  05.02.01    ORCAMO       23/04/20  コロナ５類化特例対応
      *  05.02.01    ORCAMO       23/04/24  一般病棟特別入院基本料（療養病床）（特例）
      *                                     の追加対応（令和５年５月から）"G2"
      *  05.02.01    ORCAMO       24/04/30  令和６年改正対応
      *                                     特定集中治療室管理料５，６
      *                                     一般病棟・療養病棟入院料１対応
      *                                     特定一般病棟・療養病棟入院料１対応
      *                                     特定入院料の算定要件外算定コード変更
      * 　05.02.02   ORCAMO       24/08/20  精神科救急急性期医療入院料の急性増悪対応
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
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSK5001.INC".
           COPY    "CPSK5002.INC".
           COPY    "CPSK5110.INC".
           COPY    "CPSK5111.INC".
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
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    特定入院料コード取得パラメタ
           COPY    "CPORCSTOKNYUIN.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTOKNYUIN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-TOKNYUIN-SANTEIYMD(1:6)    >=   "202406"
               PERFORM    208-EDIT-SEC
           ELSE
            IF  LNK-TOKNYUIN-SANTEIYMD(1:6)    >=   "202204"
               PERFORM    207-EDIT-SEC
            ELSE
             IF  LNK-TOKNYUIN-SANTEIYMD(1:6)    >=   "202004"
                 PERFORM    206-EDIT-SEC
             ELSE
               IF  LNK-TOKNYUIN-SANTEIYMD(1:6)    >=   "201804"
                   PERFORM    205-EDIT-SEC
               ELSE
                 IF  LNK-TOKNYUIN-SANTEIYMD(1:6)    >=   "201604"
                     PERFORM    204-EDIT-SEC
                 ELSE
                   IF  LNK-TOKNYUIN-SANTEIYMD(1:6)    >=   "201404"
                       PERFORM    203-EDIT-SEC
                   ELSE
                      IF  LNK-TOKNYUIN-SANTEIYMD(1:6)    >=   "201204"
                          PERFORM    202-EDIT-SEC
                      ELSE
                          IF  LNK-TOKNYUIN-SANTEIYMD(1:6)  >=  "201004"
                              PERFORM    201-EDIT-SEC
                          ELSE
                              PERFORM    200-EDIT-SEC
                          END-IF
                      END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
            END-IF
           END-IF
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    特定入院料コードセット（平成２２年３月まで）
      *****************************************************************
       200-EDIT-SEC                    SECTION.
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料２
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料３
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症病棟入院料１
                       MOVE   "190739910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                老人性認知症疾患療養病棟入院料
                       MOVE   "190740110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
                   IF  LNK-TOKNYUIN-SANTEIYMD(1:6) <= "200603"
      *                精神科救急入院料
                       MOVE   "190111410"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "11"
                   IF  LNK-TOKNYUIN-SANTEIYMD(1:6) <= "200603"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190066910"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "12"
                   IF  LNK-TOKNYUIN-SANTEIYMD(1:6) <= "200603"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190067010"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "13"
      *                回復期リハビリテーション病棟入院料
                       MOVE   "190075910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料４
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症病棟入院料２
                       MOVE   "190813410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                回復期リハビリテーション病棟入院料（生活療養）
                       MOVE   "190124910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190129410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190129510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190129610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190129710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                後期高齢者特定入院基本料
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                後期高齢者特定入院基本料（後期高齢者特別入院基本料算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料Ａ
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                療養病棟入院基本料Ｂ
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                療養病棟入院基本料Ｃ
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                療養病棟入院基本料Ｄ
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                療養病棟入院基本料Ｅ
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料Ａ（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料Ｂ（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料Ｃ（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料Ｄ（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料Ｅ（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       200-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２２年４月から平成２４年３月まで）
      *****************************************************************
       201-EDIT-SEC                    SECTION.
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症病棟入院料１
                       MOVE   "190739910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                老人性認知症疾患療養病棟入院料
                       MOVE   "190740110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
                   IF  LNK-TOKNYUIN-SANTEIYMD(1:6) <= "200603"
      *                精神科救急入院料
                       MOVE   "190111410"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "11"
                   IF  LNK-TOKNYUIN-SANTEIYMD(1:6) <= "200603"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190066910"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "12"
                   IF  LNK-TOKNYUIN-SANTEIYMD(1:6) <= "200603"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190067010"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "13"
      *                回復期リハビリテーション病棟入院料
                       MOVE   "190075910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症病棟入院料２
                       MOVE   "190813410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                回復期リハビリテーション病棟入院料（生活療養）
                       MOVE   "190124910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                回復期リハビリテーション病棟入院料１（基準未適合）
                       MOVE   "190129410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                回復期リハビリテーション病棟入院料１（生活療養）（基準未適合）
                       MOVE   "190129510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                回復期リハビリテーション病棟入院料２（基準未適合）
                       MOVE   "190129610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                回復期リハビリテーション病棟入院料２（生活療養）（基準未適合）
                       MOVE   "190129710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                後期高齢者特定入院基本料
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                後期高齢者特定入院基本料（後期高齢者特別入院基本料算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       201-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２４年４月から平成２６年３月まで）
      *****************************************************************
       202-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190150110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190150210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料１
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "92"
      *                亜急性期入院医療管理料１（指定地域）
                       MOVE   "190150310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "93"
      *                亜急性期入院医療管理料２（指定地域）
                       MOVE   "190150410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "95"
      *                特定一般病棟入院料（亜急性期入院医療管理）
                       MOVE   "190152610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "96"
      *                特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
                       MOVE   "190152710"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       202-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２６年４月から平成２８年３月まで））
      *****************************************************************
       203-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190150110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190150210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190175310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190175410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190175710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190175810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190176110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190176210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190176510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190176610"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "92"
      *                亜急性期入院医療管理料１（指定地域）
                       MOVE   "190150310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "93"
      *                亜急性期入院医療管理料２（指定地域）
                       MOVE   "190150410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "95"
      *                特定一般病棟入院料（亜急性期入院医療管理）
                       MOVE   "190152610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "96"
      *                特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
                       MOVE   "190152710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "97"
      *                短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））
                       MOVE   "190177610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "98"
      *                短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））（生活）
                       MOVE   "190177710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "99"
      *                短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））
                       MOVE   "190177810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9A"
      *                短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））（生活）
                       MOVE   "190177910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9H"
      *                短手３（腋臭症手術（皮膚有毛部切除術））
                       MOVE   "190178610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9I"
      *                短手３（腋臭症手術（皮膚有毛部切除術））（生活）
                       MOVE   "190178710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他のもの）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他のもの）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9P"
      *                短手３（水晶体再建術（眼内レンズ挿入しない場合））
                       MOVE   "190179410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Q"
      *                短手３（水晶体再建術（眼内レンズ挿入しない場合））（生活）
                       MOVE   "190179510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A3"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳未満））
                       MOVE   "190180810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A4"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳未満））（生活）
                       MOVE   "190180910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190175510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190175610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190175910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190176010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190176310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190176410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190176710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190176810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア入院医療管理１）
                       MOVE   "190182310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア入院医療管理２）
                       MOVE   "190182410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AW"
      *                短手３（水晶体再建術・水晶体嚢拡張リング・縫着なし）
                       MOVE   "190182750"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AX"
      *                短手３（水晶体再建術・水晶体嚢拡張リング・縫着なし）（生活）
                       MOVE   "190182850"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
      *            緩和ケア病棟入院料
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "41"
               WHEN    "45"
               WHEN    "AL"
               WHEN    "AP"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "42"
               WHEN    "46"
               WHEN    "AM"
               WHEN    "AQ"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "29"
               WHEN    "43"
               WHEN    "47"
               WHEN    "AN"
               WHEN    "AR"
      *            回復期リハビリテーション病棟入院料３
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "30"
               WHEN    "44"
               WHEN    "48"
               WHEN    "AO"
               WHEN    "AS"
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AN"
               WHEN    "AR"
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AO"
               WHEN    "AS"
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
      *            特定一般病棟入院料（地域包括ケア入院医療管理１）
      *            特定一般病棟入院料（地域包括ケア入院医療管理２）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       203-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２８年４月から平成３０年３月まで）
      *****************************************************************
       204-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190150110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190150210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190175310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190175410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190175710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190175810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190176110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190176210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190176510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190176610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                特殊疾患病棟入院料１（医療区分２の患者に相当するもの）
                       MOVE   "190195010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                特殊疾患病棟入院料１（医療区分１の患者に相当するもの）
                       MOVE   "190195110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                特殊疾患病棟入院料２（医療区分２の患者に相当するもの）
                       MOVE   "190195210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                特殊疾患病棟入院料２（医療区分１の患者に相当するもの）
                       MOVE   "190195310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                地域移行機能強化病棟入院料
                       MOVE   "190195510"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190194310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "97"
      *                短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））
                       MOVE   "190177610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "98"
      *                短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））（生活）
                       MOVE   "190177710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "99"
      *                短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））
                       MOVE   "190177810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9A"
      *                短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））（生活）
                       MOVE   "190177910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9H"
      *                短手３（腋臭症手術（皮膚有毛部切除術））
                       MOVE   "190178610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9I"
      *                短手３（腋臭症手術（皮膚有毛部切除術））（生活）
                       MOVE   "190178710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9P"
      *                短手３（水晶体再建術（眼内レンズ挿入しない場合）・片側）
                       MOVE   "190179410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Q"
      *                短手３（水晶体再建術（眼内レンズ挿入しない場合）・片側）（生活）
                       MOVE   "190179510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190175510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190175610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190175910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190176010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190176310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190176410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190176710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190176810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア入院医療管理１）
                       MOVE   "190182310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア入院医療管理２）
                       MOVE   "190182410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AY"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）
                       MOVE   "190195910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AZ"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）（生活）
                       MOVE   "190196010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B0"
      *                短手３（水晶体再建術・眼内レンズ挿入しない・両側）
                       MOVE   "190196110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B1"
      *                短手３（水晶体再建術・眼内レンズ挿入しない・両側）（生活）
                       MOVE   "190196210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B2"
      *                短手３（経皮的シャント拡張術・血栓除去術）
                       MOVE   "190196310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B3"
      *                短手３（経皮的シャント拡張術・血栓除去術）（生活）
                       MOVE   "190196410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B4"
      *                短手３（鼠径ヘルニア手術（３歳未満））
                       MOVE   "190196510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B5"
      *                短手３（鼠径ヘルニア手術（３歳未満））（生活）
                       MOVE   "190196810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B6"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））
                       MOVE   "190196610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B7"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））（生活）
                       MOVE   "190196910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B8"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））
                       MOVE   "190196710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B9"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））（生活）
                       MOVE   "190197010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BA"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））
                       MOVE   "190197110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BB"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））（生活）
                       MOVE   "190197410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BC"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））
                       MOVE   "190197210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BD"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））（生活）
                       MOVE   "190197510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BE"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））
                       MOVE   "190197310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BF"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））（生活）
                       MOVE   "190197610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BG"
      *                短手３（体外衝撃波腎・尿管結石破砕術）
                       MOVE   "190197710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BH"
      *                短手３（体外衝撃波腎・尿管結石破砕術）（生活）
                       MOVE   "190197810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BI"
      *                短手３（ガンマナイフによる定位放射線治療）
                       MOVE   "190197910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BJ"
      *                短手３（ガンマナイフによる定位放射線治療）（生活）
                       MOVE   "190198010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BK"
      *                障害者施設等７対１入院基本料（医療区分２）
                       MOVE   "190189410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BL"
      *                障害者施設等７対１入院基本料（医療区分１）
                       MOVE   "190189510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BM"
      *                障害者施設等１０対１入院基本料（医療区分２）
                       MOVE   "190189610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BN"
      *                障害者施設等１０対１入院基本料（医療区分１）
                       MOVE   "190189710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BO"
      *                障害者施設等１３対１入院基本料（医療区分２）
                       MOVE   "190189810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BP"
      *                障害者施設等１３対１入院基本料（医療区分１）
                       MOVE   "190189910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BQ"
      *                障害者施設等１５対１入院基本料（医療区分２）
                       MOVE   "190190010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BR"
      *                障害者施設等１５対１入院基本料（医療区分１）
                       MOVE   "190190110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BT"
      *                特殊疾患入院医療管理料（医療区分２）
                       MOVE   "190194510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BU"
      *                特殊疾患入院医療管理料（医療区分１）
                       MOVE   "190194610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BV"
      *                療養病棟・一般病棟７対１入院基本料
                       MOVE   "190198110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BW"
      *                療養病棟・一般病棟１０対１入院基本料
                       MOVE   "190198210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BX"
      *                療養病棟・一般病棟１３対１入院基本料
                       MOVE   "190198310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BY"
      *                療養病棟・一般病棟１５対１入院基本料
                       MOVE   "190198410"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
      *            緩和ケア病棟入院料
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
               WHEN    "53"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            地域移行機能強化病棟入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "41"
               WHEN    "45"
               WHEN    "AL"
               WHEN    "AP"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "42"
               WHEN    "46"
               WHEN    "AM"
               WHEN    "AQ"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "29"
               WHEN    "43"
               WHEN    "47"
               WHEN    "AN"
               WHEN    "AR"
      *            回復期リハビリテーション病棟入院料３
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "30"
               WHEN    "44"
               WHEN    "48"
               WHEN    "AO"
               WHEN    "AS"
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AN"
               WHEN    "AR"
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AO"
               WHEN    "AS"
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
      *            特定一般病棟入院料（地域包括ケア入院医療管理１）
      *            特定一般病棟入院料（地域包括ケア入院医療管理２）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       204-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成３０年４月から令和２年３月まで）
      *****************************************************************
       205-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料１
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190208110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190208210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190208310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190208410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190207910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190208010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190209110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190209210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190209510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190209610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190210710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190210810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190211110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190211210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                特殊疾患病棟入院料１（医療区分２の患者に相当するもの）
                       MOVE   "190195010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                特殊疾患病棟入院料１（医療区分１の患者に相当するもの）
                       MOVE   "190195110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                特殊疾患病棟入院料２（医療区分２の患者に相当するもの）
                       MOVE   "190195210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                特殊疾患病棟入院料２（医療区分１の患者に相当するもの）
                       MOVE   "190195310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                地域移行機能強化病棟入院料
                       MOVE   "190195510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                緩和ケア病棟入院料２
                       MOVE   "190212710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                回復期リハビリテーション病棟入院料４
                       MOVE   "190208510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                回復期リハビリテーション病棟入院料４（生活療養）
                       MOVE   "190208610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                回復期リハビリテーション病棟入院料５
                       MOVE   "190208710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                回復期リハビリテーション病棟入院料５（生活療養）
                       MOVE   "190208810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                回復期リハビリテーション病棟入院料６
                       MOVE   "190208910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                回復期リハビリテーション病棟入院料６（生活療養）
                       MOVE   "190209010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                地域包括ケア病棟入院料３
                       MOVE   "190209910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                地域包括ケア病棟入院料３（生活療養）
                       MOVE   "190210010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                地域包括ケア病棟入院料４
                       MOVE   "190210310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "64"
      *                地域包括ケア病棟入院料４（生活療養）
                       MOVE   "190210410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                地域包括ケア病棟入院料３（特定地域）
                       MOVE   "190211510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "66"
      *                地域包括ケア病棟入院料３（生活療養）（特定地域）
                       MOVE   "190211610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                地域包括ケア病棟入院料４（特定地域）
                       MOVE   "190211910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "68"
      *                地域包括ケア病棟入院料４（生活療養）（特定地域）
                       MOVE   "190212010"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190194310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院料１（Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院料１（Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院料１（Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院料１（Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院料１（Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院料１（Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院料１（Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院料１（Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院料１（Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院料１（Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院料１（Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院料１（Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院料１（Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院料１（Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院料１（Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院料１（Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院料１（Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院料１（Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院料２（Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院料２（Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院料２（Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院料２（Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院料２（Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院料２（Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院料２（Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院料２（Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院料２（Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院料２（Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院料２（Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院料２（Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院料２（Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院料２（Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院料２（Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院料２（Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院料２（Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院料２（Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190209310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190209410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190209710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190209810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190210910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190211010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190211310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190211410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア１）
                       MOVE   "190213310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア２）
                       MOVE   "190213410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AY"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）
                       MOVE   "190195910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AZ"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）（生活）
                       MOVE   "190196010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B2"
      *                短手３（経皮的シャント拡張術・血栓除去術）
                       MOVE   "190196310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B3"
      *                短手３（経皮的シャント拡張術・血栓除去術）（生活）
                       MOVE   "190196410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B4"
      *                短手３（鼠径ヘルニア手術（３歳未満））
                       MOVE   "190196510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B5"
      *                短手３（鼠径ヘルニア手術（３歳未満））（生活）
                       MOVE   "190196810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B6"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））
                       MOVE   "190196610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B7"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））（生活）
                       MOVE   "190196910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B8"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））
                       MOVE   "190196710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B9"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））（生活）
                       MOVE   "190197010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BA"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））
                       MOVE   "190197110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BB"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））（生活）
                       MOVE   "190197410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BC"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））
                       MOVE   "190197210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BD"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））（生活）
                       MOVE   "190197510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BE"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））
                       MOVE   "190197310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BF"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））（生活）
                       MOVE   "190197610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BG"
      *                短手３（体外衝撃波腎・尿管結石破砕術）
                       MOVE   "190197710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BH"
      *                短手３（体外衝撃波腎・尿管結石破砕術）（生活）
                       MOVE   "190197810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BI"
      *                短手３（ガンマナイフによる定位放射線治療）
                       MOVE   "190197910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BJ"
      *                短手３（ガンマナイフによる定位放射線治療）（生活）
                       MOVE   "190198010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BK"
      *                障害者施設等７対１入院基本料（医療区分２）
                       MOVE   "190189410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BL"
      *                障害者施設等７対１入院基本料（医療区分１）
                       MOVE   "190189510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BM"
      *                障害者施設等１０対１入院基本料（医療区分２）
                       MOVE   "190189610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BN"
      *                障害者施設等１０対１入院基本料（医療区分１）
                       MOVE   "190189710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BO"
      *                障害者施設等１３対１入院基本料（医療区分２）
                       MOVE   "190189810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BP"
      *                障害者施設等１３対１入院基本料（医療区分１）
                       MOVE   "190189910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BQ"
      *                障害者施設等１５対１入院基本料（医療区分２）
                       MOVE   "190190010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BR"
      *                障害者施設等１５対１入院基本料（医療区分１）
                       MOVE   "190190110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BT"
      *                特殊疾患入院医療管理料（医療区分２）
                       MOVE   "190194510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BU"
      *                特殊疾患入院医療管理料（医療区分１）
                       MOVE   "190194610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BV"
      *                療養病棟・急性期一般入院料１
                       MOVE   "190198110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BW"
      *                療養病棟・急性期一般入院料７
                       MOVE   "190198210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BX"
      *                療養病棟・地域一般入院料２
                       MOVE   "190198310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BY"
      *                療養病棟・地域一般入院料３
                       MOVE   "190198410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BZ"
      *                地域包括ケア入院医療管理料３
                       MOVE   "190210110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C0"
      *                地域包括ケア入院医療管理料３（生活療養）
                       MOVE   "190210210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C1"
      *                地域包括ケア入院医療管理料４
                       MOVE   "190210510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C2"
      *                地域包括ケア入院医療管理料４（生活療養）
                       MOVE   "190210610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C3"
      *                地域包括ケア入院医療管理料３（特定地域）
                       MOVE   "190211710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C4"
      *                地域包括ケア入院医療管理料３（生活療養）（特定地域）
                       MOVE   "190211810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C5"
      *                地域包括ケア入院医療管理料４（特定地域）
                       MOVE   "190212110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C6"
      *                地域包括ケア入院医療管理料４（生活療養）（特定地域）
                       MOVE   "190212210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C7"
      *                療養病棟・急性期一般入院料２
                       MOVE   "190201910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C8"
      *                療養病棟・急性期一般入院料３
                       MOVE   "190202010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C9"
      *                療養病棟・急性期一般入院料４
                       MOVE   "190202110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CA"
      *                療養病棟・急性期一般入院料５
                       MOVE   "190202210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CB"
      *                療養病棟・急性期一般入院料６
                       MOVE   "190202310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CC"
      *                療養病棟・地域一般入院料１
                       MOVE   "190202410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CD"
      *                特定一般病棟入院料（地域包括ケア３）
                       MOVE   "190213510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CE"
      *                特定一般病棟入院料（地域包括ケア４）
                       MOVE   "190213610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CJ"
      *                一般病棟特別入院基本料（療養病床・診療報酬上臨時的取扱）
                       MOVE    SPACE        TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "190232710"   TO  LNK-TOKNYUIN-GAI-SRYCD
      **               MOVE   "1"           TO
      **                                    LNK-TOKNYUIN-GAI-NYUKSNKBN
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
               WHEN    "54"
      *            緩和ケア病棟入院料１
      *            緩和ケア病棟入院料２
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
               WHEN    "53"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            地域移行機能強化病棟入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "29"
               WHEN    "55"
               WHEN    "41"
               WHEN    "45"
               WHEN    "43"
               WHEN    "47"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            回復期リハビリテーション病棟入院料３
      *            回復期リハビリテーション病棟入院料４
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "30"
               WHEN    "56"
               WHEN    "42"
               WHEN    "46"
               WHEN    "44"
               WHEN    "48"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            回復期リハビリテーション病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "57"
               WHEN    "59"
               WHEN    "61"
               WHEN    "65"
               WHEN    "63"
               WHEN    "67"
      *            回復期リハビリテーション病棟入院料５
      *            回復期リハビリテーション病棟入院料６
      *            地域包括ケア病棟入院料３
      *            地域包括ケア病棟入院料３（特定地域）
      *            地域包括ケア病棟入院料４
      *            地域包括ケア病棟入院料４（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "58"
               WHEN    "60"
               WHEN    "62"
               WHEN    "66"
               WHEN    "64"
               WHEN    "68"
      *            回復期リハビリテーション病棟入院料５（生活療養）
      *            回復期リハビリテーション病棟入院料６（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料４（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
               WHEN    "AN"
               WHEN    "AR"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
               WHEN    "AO"
               WHEN    "AS"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "BZ"
               WHEN    "C3"
               WHEN    "C1"
               WHEN    "C5"
      *            地域包括ケア入院医療管理料３
      *            地域包括ケア入院医療管理料３（特定地域）
      *            地域包括ケア入院医療管理料４
      *            地域包括ケア入院医療管理料４（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "C0"
               WHEN    "C4"
               WHEN    "C2"
               WHEN    "C6"
      *            地域包括ケア入院医療管理料３（生活療養）
      *            地域包括ケア入院医療管理料３（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料４（生活療養）
      *            地域包括ケア入院医療管理料４（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
               WHEN    "CD"
               WHEN    "CE"
      *            特定一般病棟入院料（地域包括ケア１）
      *            特定一般病棟入院料（地域包括ケア２）
      *            特定一般病棟入院料（地域包括ケア３）
      *            特定一般病棟入院料（地域包括ケア４）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       205-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（令和２年４月から）
      *****************************************************************
       206-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料１
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190208110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190208210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190208310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190208410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190207910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190208010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190209110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190209210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190209510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190209610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190210710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190210810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190211110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190211210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                特殊疾患病棟入院料１（医療区分２の患者に相当するもの）
                       MOVE   "190195010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                特殊疾患病棟入院料１（医療区分１の患者に相当するもの）
                       MOVE   "190195110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                特殊疾患病棟入院料２（医療区分２の患者に相当するもの）
                       MOVE   "190195210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                特殊疾患病棟入院料２（医療区分１の患者に相当するもの）
                       MOVE   "190195310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                地域移行機能強化病棟入院料
                       MOVE   "190195510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                緩和ケア病棟入院料２
                       MOVE   "190212710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                回復期リハビリテーション病棟入院料４
                       MOVE   "190208510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                回復期リハビリテーション病棟入院料４（生活療養）
                       MOVE   "190208610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                回復期リハビリテーション病棟入院料５
                       MOVE   "190208710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                回復期リハビリテーション病棟入院料５（生活療養）
                       MOVE   "190208810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                回復期リハビリテーション病棟入院料６
                       MOVE   "190208910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                回復期リハビリテーション病棟入院料６（生活療養）
                       MOVE   "190209010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                地域包括ケア病棟入院料３
                       MOVE   "190209910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                地域包括ケア病棟入院料３（生活療養）
                       MOVE   "190210010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                地域包括ケア病棟入院料４
                       MOVE   "190210310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "64"
      *                地域包括ケア病棟入院料４（生活療養）
                       MOVE   "190210410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                地域包括ケア病棟入院料３（特定地域）
                       MOVE   "190211510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "66"
      *                地域包括ケア病棟入院料３（生活療養）（特定地域）
                       MOVE   "190211610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                地域包括ケア病棟入院料４（特定地域）
                       MOVE   "190211910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "68"
      *                地域包括ケア病棟入院料４（生活療養）（特定地域）
                       MOVE   "190212010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                地域包括ケア病棟特別入院料１
                       MOVE   "190231110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6A"
      *                地域包括ケア病棟特別入院料１（生活療養）
                       MOVE   "190231210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6B"
      *                地域包括ケア病棟特別入院料２
                       MOVE   "190231310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6C"
      *                地域包括ケア病棟特別入院料２（生活療養）
                       MOVE   "190231410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6D"
      *                地域包括ケア病棟特別入院料３
                       MOVE   "190231510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6E"
      *                地域包括ケア病棟特別入院料３（生活療養）
                       MOVE   "190231610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6F"
      *                地域包括ケア病棟特別入院料４
                       MOVE   "190231710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6G"
      *                地域包括ケア病棟特別入院料４（生活療養）
                       MOVE   "190231810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6H"
      *                地域包括ケア病棟特別入院料１（特定地域）
                       MOVE   "190231910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6I"
      *                地域包括ケア病棟特別入院料１（生活療養）（特定地域）
                       MOVE   "190232010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6J"
      *                地域包括ケア病棟特別入院料２（特定地域）
                       MOVE   "190232110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6K"
      *                地域包括ケア病棟特別入院料２（生活療養）（特定地域）
                       MOVE   "190232210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6L"
      *                地域包括ケア病棟特別入院料３（特定地域）
                       MOVE   "190232310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6M"
      *                地域包括ケア病棟特別入院料３（生活療養）（特定地域）
                       MOVE   "190232410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6N"
      *                地域包括ケア病棟特別入院料４（特定地域）
                       MOVE   "190232510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6O"
      *                地域包括ケア病棟特別入院料４（生活療養）（特定地域）
                       MOVE   "190232610"   TO  LNK-TOKNYUIN-SRYCD

             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190194310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "1"          TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院料１（Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院料１（Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院料１（Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院料１（Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院料１（Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院料１（Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院料１（Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院料１（Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院料１（Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院料１（Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "63"
      *                療養病棟入院料１（Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院料１（Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院料１（Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院料１（Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院料１（Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院料１（Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院料１（Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院料１（Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院料２（Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院料２（Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院料２（Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院料２（Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院料２（Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院料２（Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院料２（Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院料２（Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院料２（Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院料２（Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院料２（Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院料２（Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院料２（Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院料２（Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院料２（Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院料２（Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院料２（Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院料２（Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190209310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190209410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190209710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190209810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190210910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190211010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190211310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190211410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア１）
                       MOVE   "190213310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア２）
                       MOVE   "190213410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AY"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）
                       MOVE   "190195910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AZ"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）（生活）
                       MOVE   "190196010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B2"
      *                短手３（経皮的シャント拡張術・血栓除去術）
                       MOVE   "190196310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B3"
      *                短手３（経皮的シャント拡張術・血栓除去術）（生活）
                       MOVE   "190196410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B4"
      *                短手３（鼠径ヘルニア手術（３歳未満））
                       MOVE   "190196510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B5"
      *                短手３（鼠径ヘルニア手術（３歳未満））（生活）
                       MOVE   "190196810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B6"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））
                       MOVE   "190196610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B7"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））（生活）
                       MOVE   "190196910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B8"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））
                       MOVE   "190196710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B9"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））（生活）
                       MOVE   "190197010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BA"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））
                       MOVE   "190197110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BB"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））（生活）
                       MOVE   "190197410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BC"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））
                       MOVE   "190197210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BD"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））（生活）
                       MOVE   "190197510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BE"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））
                       MOVE   "190197310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BF"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））（生活）
                       MOVE   "190197610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BG"
      *                短手３（体外衝撃波腎・尿管結石破砕術）
                       MOVE   "190197710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BH"
      *                短手３（体外衝撃波腎・尿管結石破砕術）（生活）
                       MOVE   "190197810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BI"
      *                短手３（ガンマナイフによる定位放射線治療）
                       MOVE   "190197910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BJ"
      *                短手３（ガンマナイフによる定位放射線治療）（生活）
                       MOVE   "190198010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BK"
      *                障害者施設等７対１入院基本料（医療区分２）
                       MOVE   "190189410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BL"
      *                障害者施設等７対１入院基本料（医療区分１）
                       MOVE   "190189510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BM"
      *                障害者施設等１０対１入院基本料（医療区分２）
                       MOVE   "190189610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BN"
      *                障害者施設等１０対１入院基本料（医療区分１）
                       MOVE   "190189710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BO"
      *                障害者施設等１３対１入院基本料（医療区分２）
                       MOVE   "190189810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BP"
      *                障害者施設等１３対１入院基本料（医療区分１）
                       MOVE   "190189910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BQ"
      *                障害者施設等１５対１入院基本料（医療区分２）
                       MOVE   "190190010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BR"
      *                障害者施設等１５対１入院基本料（医療区分１）
                       MOVE   "190190110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BT"
      *                特殊疾患入院医療管理料（医療区分２）
                       MOVE   "190194510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BU"
      *                特殊疾患入院医療管理料（医療区分１）
                       MOVE   "190194610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BV"
      *                療養病棟・急性期一般入院料１
                       MOVE   "190198110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BW"
      *                療養病棟・急性期一般入院料７
                       MOVE   "190198210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BX"
      *                療養病棟・地域一般入院料２
                       MOVE   "190198310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BY"
      *                療養病棟・地域一般入院料３
                       MOVE   "190198410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BZ"
      *                地域包括ケア入院医療管理料３
                       MOVE   "190210110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C0"
      *                地域包括ケア入院医療管理料３（生活療養）
                       MOVE   "190210210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C1"
      *                地域包括ケア入院医療管理料４
                       MOVE   "190210510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C2"
      *                地域包括ケア入院医療管理料４（生活療養）
                       MOVE   "190210610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C3"
      *                地域包括ケア入院医療管理料３（特定地域）
                       MOVE   "190211710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C4"
      *                地域包括ケア入院医療管理料３（生活療養）（特定地域）
                       MOVE   "190211810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C5"
      *                地域包括ケア入院医療管理料４（特定地域）
                       MOVE   "190212110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C6"
      *                地域包括ケア入院医療管理料４（生活療養）（特定地域）
                       MOVE   "190212210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C7"
      *                療養病棟・急性期一般入院料２
                       MOVE   "190201910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C8"
      *                療養病棟・急性期一般入院料３
                       MOVE   "190202010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C9"
      *                療養病棟・急性期一般入院料４
                       MOVE   "190202110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CA"
      *                療養病棟・急性期一般入院料５
                       MOVE   "190202210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CB"
      *                療養病棟・急性期一般入院料６
                       MOVE   "190202310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CC"
      *                療養病棟・地域一般入院料１
                       MOVE   "190202410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CD"
      *                特定一般病棟入院料（地域包括ケア３）
                       MOVE   "190213510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CE"
      *                特定一般病棟入院料（地域包括ケア４）
                       MOVE   "190213610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CF"
      *                短手３（経皮的シャント拡張術・血栓除去術（初回））
                       MOVE   "190220910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CG"
      *                短手３（経皮的シャント拡張術・血栓除去術（初回））（生活）
                       MOVE   "190221010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CH"
      *                短手３（経皮的シャント拡張術・血栓除去術・３月以内）
                       MOVE   "190221110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CI"
      *                短手３（経皮的シャント拡張術・血栓除去術・３月以内）（生活）
                       MOVE   "190221210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CJ"
      *                一般病棟特別入院基本料（療養病床・診療報酬上臨時的取扱）
                       MOVE    SPACE        TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "190232710"   TO  LNK-TOKNYUIN-GAI-SRYCD
      **               MOVE   "1"           TO
      **                                    LNK-TOKNYUIN-GAI-NYUKSNKBN
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
               WHEN    "54"
      *            緩和ケア病棟入院料１
      *            緩和ケア病棟入院料２
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
               WHEN    "53"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            地域移行機能強化病棟入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "29"
               WHEN    "55"
               WHEN    "41"
               WHEN    "45"
               WHEN    "43"
               WHEN    "47"
               WHEN    "69"
               WHEN    "6H"
               WHEN    "6B"
               WHEN    "6J"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            回復期リハビリテーション病棟入院料３
      *            回復期リハビリテーション病棟入院料４
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            地域包括ケア病棟特別入院料１
      *            地域包括ケア病棟特別入院料１（特定地域）
      *            地域包括ケア病棟特別入院料２
      *            地域包括ケア病棟特別入院料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "30"
               WHEN    "56"
               WHEN    "42"
               WHEN    "46"
               WHEN    "44"
               WHEN    "48"
               WHEN    "6A"
               WHEN    "6I"
               WHEN    "6C"
               WHEN    "6K"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            回復期リハビリテーション病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料１（生活療養）
      *            地域包括ケア病棟特別入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料２（生活療養）
      *            地域包括ケア病棟特別入院料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "57"
               WHEN    "59"
               WHEN    "61"
               WHEN    "65"
               WHEN    "63"
               WHEN    "67"
               WHEN    "6D"
               WHEN    "6L"
               WHEN    "6F"
               WHEN    "6N"
      *            回復期リハビリテーション病棟入院料５
      *            回復期リハビリテーション病棟入院料６
      *            地域包括ケア病棟入院料３
      *            地域包括ケア病棟入院料３（特定地域）
      *            地域包括ケア病棟入院料４
      *            地域包括ケア病棟入院料４（特定地域）
      *            地域包括ケア病棟特別入院料３
      *            地域包括ケア病棟特別入院料３（特定地域）
      *            地域包括ケア病棟特別入院料４
      *            地域包括ケア病棟特別入院料４（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "58"
               WHEN    "60"
               WHEN    "62"
               WHEN    "66"
               WHEN    "64"
               WHEN    "68"
               WHEN    "6E"
               WHEN    "6M"
               WHEN    "6G"
               WHEN    "6O"
      *            回復期リハビリテーション病棟入院料５（生活療養）
      *            回復期リハビリテーション病棟入院料６（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料４（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料３（生活療養）
      *            地域包括ケア病棟特別入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料４（生活療養）
      *            地域包括ケア病棟特別入院料４（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
               WHEN    "AN"
               WHEN    "AR"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
               WHEN    "AO"
               WHEN    "AS"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "BZ"
               WHEN    "C3"
               WHEN    "C1"
               WHEN    "C5"
      *            地域包括ケア入院医療管理料３
      *            地域包括ケア入院医療管理料３（特定地域）
      *            地域包括ケア入院医療管理料４
      *            地域包括ケア入院医療管理料４（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "C0"
               WHEN    "C4"
               WHEN    "C2"
               WHEN    "C6"
      *            地域包括ケア入院医療管理料３（生活療養）
      *            地域包括ケア入院医療管理料３（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料４（生活療養）
      *            地域包括ケア入院医療管理料４（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
               WHEN    "CD"
               WHEN    "CE"
      *            特定一般病棟入院料（地域包括ケア１）
      *            特定一般病棟入院料（地域包括ケア２）
      *            特定一般病棟入院料（地域包括ケア３）
      *            特定一般病棟入院料（地域包括ケア４）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の
      *    特定入院料算定コード置き換え
      *    ・点数２倍の算定コード（令和２年４月１日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "2"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20200401"
      *         点数２倍の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190221450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190221750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190222050"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190228910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190222350"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190229510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190223050"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190223250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190230310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190223450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190223650"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190230710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190224250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190224350"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190224650"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190224850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190225150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190225250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190225450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190225550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190225750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の
      *    特定入院料算定コード置き換え
      *    ・点数３倍の算定コード（令和２年５月２６日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "3"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20200526"
      *         点数３倍の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190225950"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190226250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190226550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190229210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190226850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190229910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190227150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190227350"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190230510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190227550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190227750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190230910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190227950"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190228050"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190228150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190228250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190228450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190228550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190228650"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190228750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190228850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の特定入院料算定コード
      *    ・上限延長　点数２倍の算定コード（令和３年１月２２日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "4"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20210122"
      *         点数２倍の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190232910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190233010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190233110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190236310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190233210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190236510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190233310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190233410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190236710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190233510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190233610"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190236910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190233710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190233810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190233910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190234010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190234110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190234210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190234310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190234410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190234510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の特定入院料算定コード
      *    ・上限延長　点数３倍の算定コード（令和３年１月２２日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "5"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20210122"
      *         点数３倍の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190234610"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190234710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190234810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190236410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190234910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190236610"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190235010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190235110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190236810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190235210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190235310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190237010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190235410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190235510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190235610"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190235710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190235810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190235910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190236010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190236110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190236210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       206-EDIT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    特定入院料コードセット（令和４年４月から）
      *****************************************************************
       207-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    システム管理検索（地域包括ケア入院の注の区分取得）
           MOVE    SPACE               TO  SYS-5002-REC
           INITIALIZE                      SYS-5002-REC
           MOVE    "5002"              TO  SYS-5002-KANRICD
           MOVE    LNK-TOKNYUIN-BRMNUM TO  SYS-5002-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5002-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5002-STYUKYMD(7:2)
           MOVE    SYS-5002-STYUKYMD   TO  SYS-5002-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5002-HOSPNUM
           MOVE    SYS-5002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5002-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5002-REC
               INITIALIZE                  SYS-5002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    地域包括ケア病棟入院料を注の算定に置き換え
           IF  (LNK-TOKNYUIN-KBN   =   "2")  AND
               (LNK-TOKNYUIN-SANTEIYOUKENKBN  =  SPACE)
      *      一般
             IF   LNK-TOKNYUIN-NYUIN   =   "41" OR "43" OR "61" OR "63"
                                       OR  "6X" OR "6Z" OR "71" OR "73"
                 IF   SYS-5001-BTU-CHIIKIHOU-KBN  NOT = SPACE
                      MOVE   SYS-5001-BTU-CHIIKIHOU-KBN
                                        TO  LNK-TOKNYUIN-NYUIN
                 END-IF
             END-IF
      *      生活療養
             IF   LNK-TOKNYUIN-NYUIN   =   "42" OR "44" OR "62" OR "64"
                                       OR  "6Y" OR "70" OR "72" OR "74"
                 IF   SYS-5001-BTU-CHIIKIHOU-KBN  NOT = SPACE
                     EVALUATE   SYS-5001-BTU-CHIIKIHOU-KBN
                         WHEN   "75"
                           MOVE   "76"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "77"
                           MOVE   "78"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "79"
                           MOVE   "7A"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7B"
                           MOVE   "7C"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7D"
                           MOVE   "7E"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7F"
                           MOVE   "7G"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7H"
                           MOVE   "7I"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7J"
                           MOVE   "7K"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7L"
                           MOVE   "7M"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7N"
                           MOVE   "7O"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7P"
                           MOVE   "7Q"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7R"
                           MOVE   "7S"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7T"
                           MOVE   "7U"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7V"
                           MOVE   "7W"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7X"
                           MOVE   "7Y"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7Z"
                           MOVE   "80"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "81"
                           MOVE   "82"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "83"
                           MOVE   "84"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "85"
                           MOVE   "86"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "87"
                           MOVE   "88"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "89"
                           MOVE   "8A"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8B"
                           MOVE   "8C"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8D"
                           MOVE   "8E"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8F"
                           MOVE   "8G"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8H"
                           MOVE   "8I"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8J"
                           MOVE   "8K"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8L"
                           MOVE   "8M"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8N"
                           MOVE   "8O"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8P"
                           MOVE   "8Q"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8R"
                           MOVE   "8S"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8T"
                           MOVE   "8U"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8V"
                           MOVE   "8W"   TO  LNK-TOKNYUIN-NYUIN
                     END-EVALUATE
                 END-IF
             END-IF
           END-IF
      *
      *    地域包括ケア入院医療管理料を注の算定に置き換え
           IF  (LNK-TOKNYUIN-KBN   =   "3")   AND
               (LNK-TOKNYUIN-SANTEIYOUKENKBN  =  SPACE)
      *      一般
             IF   LNK-TOKNYUIN-NYUIN   =   "AL" OR "AN" OR "BZ" OR "C1"
                                       OR  "EW" OR "EY" OR "F0" OR "F2"
                 IF   SYS-5002-BRM-CHIIKIHOU-KBN  NOT = SPACE
                      MOVE   SYS-5002-BRM-CHIIKIHOU-KBN
                                        TO  LNK-TOKNYUIN-NYUIN
                 END-IF
             END-IF
      *      生活療養
             IF   LNK-TOKNYUIN-NYUIN   =   "AM" OR "AO" OR "C0" OR "C2"
                                       OR  "EX" OR "EZ" OR "F1" OR "F3"
                 IF   SYS-5002-BRM-CHIIKIHOU-KBN  NOT = SPACE
                     EVALUATE   SYS-5002-BRM-CHIIKIHOU-KBN
                         WHEN   "F4"
                           MOVE   "F5"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "F6"
                           MOVE   "F7"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "F8"
                           MOVE   "F9"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FA"
                           MOVE   "FB"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FC"
                           MOVE   "FD"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FE"
                           MOVE   "FF"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FG"
                           MOVE   "FH"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FI"
                           MOVE   "FJ"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FK"
                           MOVE   "FL"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FM"
                           MOVE   "FN"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FO"
                           MOVE   "FP"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FQ"
                           MOVE   "FR"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FS"
                           MOVE   "FT"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FU"
                           MOVE   "FV"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FW"
                           MOVE   "FX"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FY"
                           MOVE   "FZ"   TO  LNK-TOKNYUIN-NYUIN
                     END-EVALUATE
                 END-IF
             END-IF
           END-IF
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料１
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190208110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190208210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190208310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190208410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190207910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190208010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190209110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190209210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190209510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190209610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190210710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190210810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190211110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190211210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                特殊疾患病棟入院料１（医療区分２の患者に相当するもの）
                       MOVE   "190195010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                特殊疾患病棟入院料１（医療区分１の患者に相当するもの）
                       MOVE   "190195110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                特殊疾患病棟入院料２（医療区分２の患者に相当するもの）
                       MOVE   "190195210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                特殊疾患病棟入院料２（医療区分１の患者に相当するもの）
                       MOVE   "190195310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                地域移行機能強化病棟入院料
                       MOVE   "190195510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                緩和ケア病棟入院料２
                       MOVE   "190212710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                回復期リハビリテーション病棟入院料４
                       MOVE   "190208510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                回復期リハビリテーション病棟入院料４（生活療養）
                       MOVE   "190208610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                回復期リハビリテーション病棟入院料５（経過措置）
                       MOVE   "190208710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                回復期リハビリテーション病棟入院料５（生活療養）（経過措置）
                       MOVE   "190208810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                回復期リハビリテーション病棟入院料６（経過措置）
                       MOVE   "190208910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                回復期リハビリテーション病棟入院料６（生活療養）（経過措置）
                       MOVE   "190209010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                地域包括ケア病棟入院料３
                       MOVE   "190209910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                地域包括ケア病棟入院料３（生活療養）
                       MOVE   "190210010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                地域包括ケア病棟入院料４
                       MOVE   "190210310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "64"
      *                地域包括ケア病棟入院料４（生活療養）
                       MOVE   "190210410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                地域包括ケア病棟入院料３（特定地域）
                       MOVE   "190211510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "66"
      *                地域包括ケア病棟入院料３（生活療養）（特定地域）
                       MOVE   "190211610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                地域包括ケア病棟入院料４（特定地域）
                       MOVE   "190211910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "68"
      *                地域包括ケア病棟入院料４（生活療養）（特定地域）
                       MOVE   "190212010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                地域包括ケア病棟特別入院料１
                       MOVE   "190231110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6A"
      *                地域包括ケア病棟特別入院料１（生活療養）
                       MOVE   "190231210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6B"
      *                地域包括ケア病棟特別入院料２
                       MOVE   "190231310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6C"
      *                地域包括ケア病棟特別入院料２（生活療養）
                       MOVE   "190231410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6D"
      *                地域包括ケア病棟特別入院料３
                       MOVE   "190231510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6E"
      *                地域包括ケア病棟特別入院料３（生活療養）
                       MOVE   "190231610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6F"
      *                地域包括ケア病棟特別入院料４
                       MOVE   "190231710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6G"
      *                地域包括ケア病棟特別入院料４（生活療養）
                       MOVE   "190231810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6H"
      *                地域包括ケア病棟特別入院料１（特定地域）
                       MOVE   "190231910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6I"
      *                地域包括ケア病棟特別入院料１（生活療養）（特定地域）
                       MOVE   "190232010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6J"
      *                地域包括ケア病棟特別入院料２（特定地域）
                       MOVE   "190232110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6K"
      *                地域包括ケア病棟特別入院料２（生活療養）（特定地域）
                       MOVE   "190232210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6L"
      *                地域包括ケア病棟特別入院料３（特定地域）
                       MOVE   "190232310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6M"
      *                地域包括ケア病棟特別入院料３（生活療養）（特定地域）
                       MOVE   "190232410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6N"
      *                地域包括ケア病棟特別入院料４（特定地域）
                       MOVE   "190232510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6O"
      *                地域包括ケア病棟特別入院料４（生活療養）（特定地域）
                       MOVE   "190232610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6P"
      *                特殊疾患病棟入院料１（医療区分２）（重度意識障害等除く）
                       MOVE   "190263510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6Q"
      *                特殊疾患病棟入院料１（医療区分１）（重度意識障害等除く）
                       MOVE   "190263610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6R"
      *                特殊疾患病棟入院料２（医療区分２）（重度意識障害等除く）
                       MOVE   "190263710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6S"
      *                特殊疾患病棟入院料２（医療区分１）（重度意識障害等除く）
                       MOVE   "190263810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6T"
      *                回復期リハビリテーション病棟入院料５
                       MOVE   "190249510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6U"
      *                回復期リハビリテーション病棟入院料５（生活療養）
                       MOVE   "190249610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6V"
      *                精神科救急急性期医療入院料
                       MOVE   "190264010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6W"
      *                特定機能病院リハビリテーション病棟入院料
                       MOVE   "190265310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                特定機能病院リハビリテーション病棟入院料（生活療養）
                       MOVE   "190265210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6X"
      *                地域包括ケア
                       MOVE   "190249710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6Y"
      *                地域包括ケア
                       MOVE   "190249810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "6Z"
      *                地域包括ケア
                       MOVE   "190250510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "70"
      *                地域包括ケア
                       MOVE   "190250610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                地域包括ケア
                       MOVE   "190252910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                地域包括ケア
                       MOVE   "190253010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                地域包括ケア
                       MOVE   "190253710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                地域包括ケア
                       MOVE   "190253810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                地域包括ケア
                       MOVE   "190256110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                地域包括ケア
                       MOVE   "190256210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                地域包括ケア
                       MOVE   "190256910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                地域包括ケア
                       MOVE   "190257010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                地域包括ケア
                       MOVE   "190250710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7A"
      *                地域包括ケア
                       MOVE   "190250810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7B"
      *                地域包括ケア
                       MOVE   "190253910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7C"
      *                地域包括ケア
                       MOVE   "190254010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7D"
      *                地域包括ケア
                       MOVE   "190257710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7E"
      *                地域包括ケア
                       MOVE   "190257810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7F"
      *                地域包括ケア
                       MOVE   "190258110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7G"
      *                地域包括ケア
                       MOVE   "190258210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7H"
      *                地域包括ケア
                       MOVE   "190253110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7I"
      *                地域包括ケア
                       MOVE   "190253210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7J"
      *                地域包括ケア
                       MOVE   "190254110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7K"
      *                地域包括ケア
                       MOVE   "190254210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7L"
      *                地域包括ケア
                       MOVE   "190258910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7M"
      *                地域包括ケア
                       MOVE   "190259010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7N"
      *                地域包括ケア
                       MOVE   "190259710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7O"
      *                地域包括ケア
                       MOVE   "190259810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7P"
      *                地域包括ケア
                       MOVE   "190250910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7Q"
      *                地域包括ケア
                       MOVE   "190251010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7R"
      *                地域包括ケア
                       MOVE   "190254310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7S"
      *                地域包括ケア
                       MOVE   "190254410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7T"
      *                地域包括ケア
                       MOVE   "190260110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7U"
      *                地域包括ケア
                       MOVE   "190260210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7V"
      *                地域包括ケア
                       MOVE   "190260510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7W"
      *                地域包括ケア
                       MOVE   "190260610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7X"
      *                地域包括ケア
                       MOVE   "190249910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7Y"
      *                地域包括ケア
                       MOVE   "190250010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "7Z"
      *                地域包括ケア
                       MOVE   "190251110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                地域包括ケア
                       MOVE   "190251210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                地域包括ケア
                       MOVE   "190251310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                地域包括ケア
                       MOVE   "190251410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                地域包括ケア
                       MOVE   "190251510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                地域包括ケア
                       MOVE   "190251610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                地域包括ケア
                       MOVE   "190251710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                地域包括ケア
                       MOVE   "190251810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                地域包括ケア
                       MOVE   "190251910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                地域包括ケア
                       MOVE   "190252010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "89"
      *                地域包括ケア
                       MOVE   "190254510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8A"
      *                地域包括ケア
                       MOVE   "190254610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8B"
      *                地域包括ケア
                       MOVE   "190254710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8C"
      *                地域包括ケア
                       MOVE   "190254810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8D"
      *                地域包括ケア
                       MOVE   "190254910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8E"
      *                地域包括ケア
                       MOVE   "190255010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8F"
      *                地域包括ケア
                       MOVE   "190255110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8G"
      *                地域包括ケア
                       MOVE   "190255210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8H"
      *                地域包括ケア
                       MOVE   "190256310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8I"
      *                地域包括ケア
                       MOVE   "190256410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8J"
      *                地域包括ケア
                       MOVE   "190256510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8K"
      *                地域包括ケア
                       MOVE   "190256610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8L"
      *                地域包括ケア
                       MOVE   "190256710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8M"
      *                地域包括ケア
                       MOVE   "190256810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8N"
      *                地域包括ケア
                       MOVE   "190257110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8O"
      *                地域包括ケア
                       MOVE   "190257210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8P"
      *                地域包括ケア
                       MOVE   "190257310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8Q"
      *                地域包括ケア
                       MOVE   "190257410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8R"
      *                地域包括ケア
                       MOVE   "190257510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8S"
      *                地域包括ケア
                       MOVE   "190257610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8T"
      *                地域包括ケア
                       MOVE   "190258310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8U"
      *                地域包括ケア
                       MOVE   "190258410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8V"
      *                地域包括ケア
                       MOVE   "190259110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "8W"
      *                地域包括ケア
                       MOVE   "190259210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "92"
      *                精神科救急急性期医療入院料（急性増悪の再入院）
                       MOVE   "190264010"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190194310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院料１（Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院料１（Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院料１（Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院料１（Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院料１（Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院料１（Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院料１（Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院料１（Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院料１（Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院料１（Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "63"
      *                療養病棟入院料１（Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院料１（Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院料１（Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院料１（Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院料１（Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院料１（Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院料１（Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院料１（Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院料２（Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院料２（Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院料２（Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院料２（Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院料２（Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院料２（Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院料２（Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院料２（Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院料２（Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院料２（Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院料２（Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院料２（Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院料２（Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院料２（Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院料２（Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院料２（Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院料２（Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院料２（Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190209310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190209410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190209710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190209810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190210910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190211010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190211310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190211410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア１）
                       MOVE   "190213310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア２）
                       MOVE   "190213410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AY"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）
                       MOVE   "190195910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AZ"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）（生活）
                       MOVE   "190196010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B2"
      *                短手３（経皮的シャント拡張術・血栓除去術）
                       MOVE   "190196310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B3"
      *                短手３（経皮的シャント拡張術・血栓除去術）（生活）
                       MOVE   "190196410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B4"
      *                短手３（鼠径ヘルニア手術（３歳未満））
                       MOVE   "190196510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B5"
      *                短手３（鼠径ヘルニア手術（３歳未満））（生活）
                       MOVE   "190196810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B6"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））
                       MOVE   "190196610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B7"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））（生活）
                       MOVE   "190196910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B8"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））
                       MOVE   "190196710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B9"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））（生活）
                       MOVE   "190197010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BA"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））
                       MOVE   "190197110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BB"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））（生活）
                       MOVE   "190197410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BC"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））
                       MOVE   "190197210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BD"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））（生活）
                       MOVE   "190197510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BE"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））
                       MOVE   "190197310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BF"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））（生活）
                       MOVE   "190197610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BG"
      *                短手３（体外衝撃波腎・尿管結石破砕術）
                       MOVE   "190197710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BH"
      *                短手３（体外衝撃波腎・尿管結石破砕術）（生活）
                       MOVE   "190197810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BI"
      *                短手３（ガンマナイフによる定位放射線治療）
                       MOVE   "190197910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BJ"
      *                短手３（ガンマナイフによる定位放射線治療）（生活）
                       MOVE   "190198010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BK"
      *                障害者施設等７対１入院基本料（医療区分２）
                       MOVE   "190189410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BL"
      *                障害者施設等７対１入院基本料（医療区分１）
                       MOVE   "190189510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BM"
      *                障害者施設等１０対１入院基本料（医療区分２）
                       MOVE   "190189610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BN"
      *                障害者施設等１０対１入院基本料（医療区分１）
                       MOVE   "190189710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BO"
      *                障害者施設等１３対１入院基本料（医療区分２）
                       MOVE   "190189810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BP"
      *                障害者施設等１３対１入院基本料（医療区分１）
                       MOVE   "190189910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BQ"
      *                障害者施設等１５対１入院基本料（医療区分２）
                       MOVE   "190190010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BR"
      *                障害者施設等１５対１入院基本料（医療区分１）
                       MOVE   "190190110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BT"
      *                特殊疾患入院医療管理料（医療区分２）
                       MOVE   "190194510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BU"
      *                特殊疾患入院医療管理料（医療区分１）
                       MOVE   "190194610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BV"
      *                療養病棟・急性期一般入院料１
                       MOVE   "190198110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BW"
      *                療養病棟・急性期一般入院料７
                       MOVE   "190198210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BX"
      *                療養病棟・地域一般入院料２
                       MOVE   "190198310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BY"
      *                療養病棟・地域一般入院料３
                       MOVE   "190198410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BZ"
      *                地域包括ケア入院医療管理料３
                       MOVE   "190210110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C0"
      *                地域包括ケア入院医療管理料３（生活療養）
                       MOVE   "190210210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C1"
      *                地域包括ケア入院医療管理料４
                       MOVE   "190210510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C2"
      *                地域包括ケア入院医療管理料４（生活療養）
                       MOVE   "190210610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C3"
      *                地域包括ケア入院医療管理料３（特定地域）
                       MOVE   "190211710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C4"
      *                地域包括ケア入院医療管理料３（生活療養）（特定地域）
                       MOVE   "190211810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C5"
      *                地域包括ケア入院医療管理料４（特定地域）
                       MOVE   "190212110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C6"
      *                地域包括ケア入院医療管理料４（生活療養）（特定地域）
                       MOVE   "190212210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C7"
      *                療養病棟・急性期一般入院料２
                       MOVE   "190201910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C8"
      *                療養病棟・急性期一般入院料３
                       MOVE   "190202010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C9"
      *                療養病棟・急性期一般入院料４
                       MOVE   "190202110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CA"
      *                療養病棟・急性期一般入院料５
                       MOVE   "190202210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CB"
      *                療養病棟・急性期一般入院料６
                       MOVE   "190202310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CC"
      *                療養病棟・地域一般入院料１
                       MOVE   "190202410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CD"
      *                特定一般病棟入院料（地域包括ケア３）
                       MOVE   "190213510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CE"
      *                特定一般病棟入院料（地域包括ケア４）
                       MOVE   "190213610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CF"
      *                短手３（経皮的シャント拡張術・血栓除去術（初回））
                       MOVE   "190220910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CG"
      *                短手３（経皮的シャント拡張術・血栓除去術（初回））（生活）
                       MOVE   "190221010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CH"
      *                短手３（経皮的シャント拡張術・血栓除去術・３月以内）
                       MOVE   "190221110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CI"
      *                短手３（経皮的シャント拡張術・血栓除去術・３月以内）（生活）
                       MOVE   "190221210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CJ"
      *                一般病棟特別入院基本料（療養病床・診療報酬上臨時的取扱）
                       MOVE    SPACE        TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "190232710"   TO  LNK-TOKNYUIN-GAI-SRYCD
      **               MOVE   "1"           TO
      **                                    LNK-TOKNYUIN-GAI-NYUKSNKBN
               WHEN    "CK"
      *                障害者施設等７対１入院基本料（医療区分２）（重度除く）
                       MOVE   "190240610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CL"
      *                障害者施設等７対１入院基本料（医療区分１）（重度除く）
                       MOVE   "190240710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CM"
      *                障害者施設等１０対１入院基本料（医療区分２）（重度除く）
                       MOVE   "190240810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CN"
      *                障害者施設等１０対１入院基本料（医療区分１）（重度除く）
                       MOVE   "190240910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CO"
      *                障害者施設等１３対１入院基本料（医療区分２）（重度除く）
                       MOVE   "190241010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CP"
      *                障害者施設等１３対１入院基本料（医療区分１）（重度除く）
                       MOVE   "190241110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CQ"
      *                障害者施設等１５対１入院基本料（医療区分２）（重度除く）
                       MOVE   "190241210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CR"
      *                障害者施設等１５対１入院基本料（医療区分１）（重度除く）
                       MOVE   "190241310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CS"
      *                短手３（終夜睡眠ポリグラフィー・１、２以外・安全精度管理下）
                       MOVE   "190265610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CT"
      *                短手３（終夜睡眠ポリグラフィー・１、２以外・安全精度管理下）（生活）
                       MOVE   "190265710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CU"
      *                短手３（終夜睡眠ポリグラフィー・１、２以外・その他）
                       MOVE   "190265810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CV"
      *                短手３（終夜睡眠ポリグラフィー１、２以外・その他・生活）（生活）
                       MOVE   "190265910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CW"
      *                短手３（ＭＳＬＴ）
                       MOVE   "190266010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CX"
      *                短手３（ＭＳＬＴ）（生活）
                       MOVE   "190266110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CY"
      *                短手３（内分泌負荷試験・下垂体前葉負荷試験（ＧＨ））
                       MOVE   "190266210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CZ"
      *                短手３（内分泌負荷試験・下垂体前葉負荷試験（ＧＨ））（生活）
                       MOVE   "190266310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D0"
      *                短手３（経皮的放射線治療用金属マーカー留置術）
                       MOVE   "190266410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D1"
      *                短手３（経皮的放射線治療用金属マーカー留置術）（生活）
                       MOVE   "190266510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D2"
      *                短手３（四肢・躯幹軟部腫瘍摘出術（手））
                       MOVE   "190266610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D3"
      *                短手３（四肢・躯幹軟部腫瘍摘出術（手））（生活）
                       MOVE   "190266710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D4"
      *                短手３（骨折観血的手術（手舟状骨））
                       MOVE   "190266810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D5"
      *                短手３（骨折観血的手術（手舟状骨））（生活）
                       MOVE   "190266910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D6"
      *                短手３（骨内異物（挿入物を含む）除去術（前腕））
                       MOVE   "190267010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D7"
      *                短手３（骨内異物（挿入物を含む）除去術（前腕））（生活）
                       MOVE   "190267110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D8"
      *                短手３（骨内異物（挿入物を含む）除去術（鎖骨））
                       MOVE   "190267210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "D9"
      *                短手３（骨内異物（挿入物を含む）除去術（鎖骨））（生活）
                       MOVE   "190267310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DA"
      *                短手３（骨内異物（挿入物を含む）除去術（手））
                       MOVE   "190267410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DB"
      *                短手３（骨内異物（挿入物を含む）除去術（手））（生活）
                       MOVE   "190267510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DC"
      *                短手３（ガングリオン摘出術（手））
                       MOVE   "190267610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DD"
      *                短手３（ガングリオン摘出術（手））（生活）
                       MOVE   "190267710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DE"
      *                短手３（涙管チューブ挿入術（涙道内視鏡））
                       MOVE   "190267810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DF"
      *                短手３（涙管チューブ挿入術（涙道内視鏡））（生活）
                       MOVE   "190267910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DG"
      *                短手３（眼瞼内反症手術（皮膚切開法））
                       MOVE   "190268010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DH"
      *                短手３（眼瞼内反症手術（皮膚切開法））（生活）
                       MOVE   "190268110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DI"
      *                短手３（眼瞼下垂症手術（眼瞼挙筋前転法））
                       MOVE   "190268210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DJ"
      *                短手３（眼瞼下垂症手術（眼瞼挙筋前転法））（生活）
                       MOVE   "190268310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DK"
      *                短手３（眼瞼下垂症手術（その他のもの））
                       MOVE   "190268410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DL"
      *                短手３（眼瞼下垂症手術（その他のもの））（生活）
                       MOVE   "190268510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DM"
      *                短手３（翼状片手術（弁の移植を要するもの））
                       MOVE   "190268610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DN"
      *                短手３（翼状片手術（弁の移植を要するもの））（生活）
                       MOVE   "190268710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DO"
      *                短手３（斜視手術（後転法））
                       MOVE   "190268810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DP"
      *                短手３（斜視手術（後転法））（生活）
                       MOVE   "190268910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DQ"
      *                短手３（斜視手術（前転法及び後転法の併施））
                       MOVE   "190269010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DR"
      *                短手３（斜視手術（前転法及び後転法の併施））（生活）
                       MOVE   "190269110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DS"
      *                短手３（治療的角膜切除術・エキシマレーザー）
                       MOVE   "190269210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DT"
      *                短手３（治療的角膜切除術・エキシマレーザー）（生活）
                       MOVE   "190269310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DU"
      *                短手３（緑内障手術・水晶体再建術併用眼内ドレーン挿入）
                       MOVE   "190269410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DV"
      *                短手３（緑内障手術・水晶体再建術併用眼内ドレーン挿入）（生活）
                       MOVE   "190269510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DW"
      *                短手３（水晶体再建術・眼内レンズを挿入しない・片側）
                       MOVE   "190269610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DX"
      *                短手３（水晶体再建術・眼内レンズを挿入しない・片側）（生活）
                       MOVE   "190269710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DY"
      *                短手３（水晶体再建術・眼内レンズを挿入しない・両側）
                       MOVE   "190269810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "DZ"
      *                短手３（水晶体再建術・眼内レンズを挿入しない・両側）（生活）
                       MOVE   "190269910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E0"
      *                短手３（鼓膜形成手術）
                       MOVE   "190270010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E1"
      *                短手３（鼓膜形成手術）（生活）
                       MOVE   "190270110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E2"
      *                短手３（鼻骨骨折整復固定術）
                       MOVE   "190270210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E3"
      *                短手３（鼻骨骨折整復固定術）（生活）
                       MOVE   "190270310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E4"
      *                短手３（喉頭・声帯ポリープ切除術（直達喉頭鏡、ファイバー））
                       MOVE   "190270410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E5"
      *                短手３（喉頭・声帯ポリープ切除術（直達喉頭鏡、ファイバー））（生活）
                       MOVE   "190270510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E6"
      *                短手３（乳腺腫瘍摘出術（直径５ｃｍ以上）
                       MOVE   "190270610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E7"
      *                短手３（乳腺腫瘍摘出術（直径５ｃｍ以上）（生活）
                       MOVE   "190270710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E8"
      *                短手３（大伏在静脈抜去術）
                       MOVE   "190270810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "E9"
      *                短手３（大伏在静脈抜去術）（生活）
                       MOVE   "190270910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EA"
      *                短手３（下肢静脈瘤血管内焼灼術）
                       MOVE   "190271010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EB"
      *                短手３（下肢静脈瘤血管内焼灼術）（生活）
                       MOVE   "190271110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EC"
      *                短手３（下肢静脈瘤血管内塞栓術）
                       MOVE   "190271210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "ED"
      *                短手３（下肢静脈瘤血管内塞栓術）（生活）
                       MOVE   "190271310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EE"
      *                短手３（肛門ポリープ切除術）
                       MOVE   "190271410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EF"
      *                短手３（肛門ポリープ切除術）（生活）
                       MOVE   "190271510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EG"
      *                短手３（肛門尖圭コンジローム切除術）
                       MOVE   "190271610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EH"
      *                短手３（肛門尖圭コンジローム切除術）（生活）
                       MOVE   "190271710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EI"
      *                短手３（尿失禁手術（ボツリヌス毒素によるもの））
                       MOVE   "190271810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EJ"
      *                短手３（尿失禁手術（ボツリヌス毒素によるもの））（生活）
                       MOVE   "190271910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EK"
      *                短手３（顕微鏡下精索静脈瘤手術）
                       MOVE   "190272010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EL"
      *                短手３（顕微鏡下精索静脈瘤手術）（生活）
                       MOVE   "190272110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EM"
      *                短手３（子宮鏡下有茎粘膜下筋腫切出、ポリープ・電解液）
                       MOVE   "190272210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EN"
      *                短手３（子宮鏡下有茎粘膜下筋腫切出、ポリープ・電解液）（生活）
                       MOVE   "190272310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EO"
      *                短手３（子宮鏡下有茎粘膜下筋腫切出、ポリープ・その他）
                       MOVE   "190272410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EP"
      *                短手３（子宮鏡下有茎粘膜下筋腫切出、ポリープ・その他）（生活）
                       MOVE   "190272510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EQ"
      *                短手３（子宮鏡下子宮筋腫摘出術（電解質溶液利用））
                       MOVE   "190272610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "ER"
      *                短手３（子宮鏡下子宮筋腫摘出術（電解質溶液利用））（生活）
                       MOVE   "190272710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "ES"
      *                短手３（子宮鏡下子宮筋腫摘出術（その他））
                       MOVE   "190272810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "ET"
      *                短手３（子宮鏡下子宮筋腫摘出術（その他））（生活）
                       MOVE   "190272910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EU"
      *                短手３（腹腔鏡下卵管形成術）
                       MOVE   "190273010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EV"
      *                短手３（腹腔鏡下卵管形成術）（生活）
                       MOVE   "190273110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EW"
      *                地域包括ケア入院医療管理料１（療養病床）
                       MOVE   "190250110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EX"
      *                地域包括ケア入院医療管理料１（生活療養・療養病床）
                       MOVE   "190250210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EY"
      *                地域包括ケア入院医療管理料２（療養病床）
                       MOVE   "190252110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "EZ"
      *                地域包括ケア入院医療管理料２（生活療養・療養病床）
                       MOVE   "190252210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F0"
      *                地域包括ケア入院医療管理料３（療養病床）
                       MOVE   "190253310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F1"
      *                地域包括ケア入院医療管理料３（生活療養・療養病床）
                       MOVE   "190253410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F2"
      *                地域包括ケア入院医療管理料４（療養病床）
                       MOVE   "190255310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F3"
      *                地域包括ケア入院医療管理料４（生活療養・療養病床）
                       MOVE   "190255410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F4"
      *                地域包括ケア入院医療管理料
                       MOVE   "190257910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F5"
      *                地域包括ケア入院医療管理料
                       MOVE   "190258010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F6"
      *                地域包括ケア入院医療管理料
                       MOVE   "190258510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F7"
      *                地域包括ケア入院医療管理料
                       MOVE   "190258610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F8"
      *                地域包括ケア入院医療管理料
                       MOVE   "190253510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "F9"
      *                地域包括ケア入院医療管理料
                       MOVE   "190253610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FA"
      *                地域包括ケア入院医療管理料
                       MOVE   "190255510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FB"
      *                地域包括ケア入院医療管理料
                       MOVE   "190255610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FC"
      *                地域包括ケア入院医療管理料
                       MOVE   "190259310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FD"
      *                地域包括ケア入院医療管理料
                       MOVE   "190259410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FE"
      *                地域包括ケア入院医療管理料
                       MOVE   "190259910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FF"
      *                地域包括ケア入院医療管理料
                       MOVE   "190260010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FG"
      *                地域包括ケア入院医療管理料
                       MOVE   "190252310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FH"
      *                地域包括ケア入院医療管理料
                       MOVE   "190252410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FI"
      *                地域包括ケア入院医療管理料
                       MOVE   "190255710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FJ"
      *                地域包括ケア入院医療管理料
                       MOVE   "190255810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FK"
      *                地域包括ケア入院医療管理料
                       MOVE   "190260310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FL"
      *                地域包括ケア入院医療管理料
                       MOVE   "190260410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FM"
      *                地域包括ケア入院医療管理料
                       MOVE   "190260710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FN"
      *                地域包括ケア入院医療管理料
                       MOVE   "190260810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FO"
      *                地域包括ケア入院医療管理料
                       MOVE   "190250310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FP"
      *                地域包括ケア入院医療管理料
                       MOVE   "190250410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FQ"
      *                地域包括ケア入院医療管理料
                       MOVE   "190252510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FR"
      *                地域包括ケア入院医療管理料
                       MOVE   "190252610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FS"
      *                地域包括ケア入院医療管理料
                       MOVE   "190252710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FT"
      *                地域包括ケア入院医療管理料
                       MOVE   "190252810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FU"
      *                地域包括ケア入院医療管理料
                       MOVE   "190255910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FV"
      *                地域包括ケア入院医療管理料
                       MOVE   "190256010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FW"
      *                地域包括ケア入院医療管理料
                       MOVE   "190258710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FX"
      *                地域包括ケア入院医療管理料
                       MOVE   "190258810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FY"
      *                地域包括ケア入院医療管理料
                       MOVE   "190259510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "FZ"
      *                地域包括ケア入院医療管理料
                       MOVE   "190259610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "G0"
      *                特殊疾患入院医療管理料（医療区分２）（重度意識障害等除く）
                       MOVE   "190248710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "G1"
      *                特殊疾患入院医療管理料（医療区分１）（重度意識障害等除く）
                       MOVE   "190248810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "G2"
      *                一般病棟特別入院基本料（療養病床）（特例）
                       MOVE    SPACE        TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "190291350"   TO  LNK-TOKNYUIN-GAI-SRYCD
      **               MOVE   "1"           TO
      **                                    LNK-TOKNYUIN-GAI-NYUKSNKBN
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
               WHEN    "54"
               WHEN    "6W"
      *            緩和ケア病棟入院料１
      *            緩和ケア病棟入院料２
      *            特定機能病院リハビリテーション病棟入院料
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
               WHEN    "53"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            地域移行機能強化病棟入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "29"
               WHEN    "55"
               WHEN    "41"
               WHEN    "45"
               WHEN    "43"
               WHEN    "47"
               WHEN    "69"
               WHEN    "6H"
               WHEN    "6B"
               WHEN    "6J"
               WHEN    "6X"
               WHEN    "6Z"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            回復期リハビリテーション病棟入院料３
      *            回復期リハビリテーション病棟入院料４
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            地域包括ケア病棟特別入院料１
      *            地域包括ケア病棟特別入院料１（特定地域）
      *            地域包括ケア病棟特別入院料２
      *            地域包括ケア病棟特別入院料２（特定地域）
      *            地域包括ケア病棟入院料１（療養病床）
      *            地域包括ケア病棟入院料２（療養病床）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "30"
               WHEN    "56"
               WHEN    "42"
               WHEN    "46"
               WHEN    "44"
               WHEN    "48"
               WHEN    "6A"
               WHEN    "6I"
               WHEN    "6C"
               WHEN    "6K"
               WHEN    "6Y"
               WHEN    "70"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            回復期リハビリテーション病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料１（生活療養）
      *            地域包括ケア病棟特別入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料２（生活療養）
      *            地域包括ケア病棟特別入院料２（生活療養）（特定地域）
      *            地域包括ケア病棟入院料１（生活療養・療養病床）
      *            地域包括ケア病棟入院料２（生活療養・療養病床）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "57"
               WHEN    "59"
               WHEN    "61"
               WHEN    "65"
               WHEN    "63"
               WHEN    "67"
               WHEN    "6D"
               WHEN    "6L"
               WHEN    "6F"
               WHEN    "6N"
               WHEN    "71"
               WHEN    "73"
      *            回復期リハビリテーション病棟入院料５
      *            回復期リハビリテーション病棟入院料６
      *            地域包括ケア病棟入院料３
      *            地域包括ケア病棟入院料３（特定地域）
      *            地域包括ケア病棟入院料４
      *            地域包括ケア病棟入院料４（特定地域）
      *            地域包括ケア病棟特別入院料３
      *            地域包括ケア病棟特別入院料３（特定地域）
      *            地域包括ケア病棟特別入院料４
      *            地域包括ケア病棟特別入院料４（特定地域）
      *            地域包括ケア病棟入院料３（療養病床）
      *            地域包括ケア病棟入院料４（療養病床）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "58"
               WHEN    "60"
               WHEN    "62"
               WHEN    "66"
               WHEN    "64"
               WHEN    "68"
               WHEN    "6E"
               WHEN    "6M"
               WHEN    "6G"
               WHEN    "6O"
               WHEN    "72"
               WHEN    "74"
      *            回復期リハビリテーション病棟入院料５（生活療養）
      *            回復期リハビリテーション病棟入院料６（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料４（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料３（生活療養）
      *            地域包括ケア病棟特別入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料４（生活療養）
      *            地域包括ケア病棟特別入院料４（生活療養）（特定地域）
      *            地域包括ケア病棟入院料３（生活療養・療養病床）
      *            地域包括ケア病棟入院料４（生活療養・療養病床）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "6T"
      *            回復期リハビリテーション病棟入院料５
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "6U"
      *            回復期リハビリテーション病棟入院料５（生活療養）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
               WHEN    "EW"
               WHEN    "AN"
               WHEN    "AR"
               WHEN    "EY"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            地域包括ケア入院医療管理料１（療養病床）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            地域包括ケア入院医療管理料２（療養病床）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
               WHEN    "EX"
               WHEN    "AO"
               WHEN    "AS"
               WHEN    "EZ"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料１（生活療養・療養病床）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養・療養病床）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "BZ"
               WHEN    "C3"
               WHEN    "F0"
               WHEN    "C1"
               WHEN    "C5"
               WHEN    "F2"
      *            地域包括ケア入院医療管理料３
      *            地域包括ケア入院医療管理料３（特定地域）
      *            地域包括ケア入院医療管理料３（療養病床）
      *            地域包括ケア入院医療管理料４
      *            地域包括ケア入院医療管理料４（特定地域）
      *            地域包括ケア入院医療管理料４（療養病床）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "C0"
               WHEN    "C4"
               WHEN    "F1"
               WHEN    "C2"
               WHEN    "C6"
               WHEN    "F3"
      *            地域包括ケア入院医療管理料３（生活療養）
      *            地域包括ケア入院医療管理料３（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料３（生活療養・療養病床）
      *            地域包括ケア入院医療管理料４（生活療養）
      *            地域包括ケア入院医療管理料４（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料４（生活療養・療養病床）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
               WHEN    "CD"
               WHEN    "CE"
      *            特定一般病棟入院料（地域包括ケア１）
      *            特定一般病棟入院料（地域包括ケア２）
      *            特定一般病棟入院料（地域包括ケア３）
      *            特定一般病棟入院料（地域包括ケア４）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の
      *    特定入院料算定コード置き換え
      *    ・点数２倍の算定コード（令和２年４月１日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "2"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20200401"
      *         点数２倍の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190221450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190221750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190222050"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190228910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190222350"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190229510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190223050"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190223250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190230310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190223450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190223650"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190230710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190224250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190224350"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190224650"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190224850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190225150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190225250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190225450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190225550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190225750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の
      *    特定入院料算定コード置き換え
      *    ・点数３倍の算定コード（令和２年５月２６日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "3"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20200526"
      *         点数３倍の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190225950"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190226250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190226550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190229210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190226850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190229910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190227150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190227350"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190230510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190227550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190227750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190230910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190227950"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190228050"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190228150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190228250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190228450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190228550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190228650"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190228750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190228850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の特定入院料算定コード
      *    ・上限延長　点数２倍の算定コード（令和３年１月２２日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "4"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20210122"
      *         点数２倍の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190232910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190233010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190233110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190236310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190233210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190236510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190233310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190233410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190236710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190233510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190233610"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190236910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190233710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190233810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190233910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190234010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190234110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190234210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190234310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190234410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190234510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の特定入院料算定コード
      *    ・上限延長　点数３倍の算定コード（令和３年１月２２日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "5"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20210122"
      *         点数３倍の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190234610"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190234710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190234810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190236410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190234910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190236610"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190235010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190235110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190236810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190235210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190235310"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190237010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190235410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190235510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190235610"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190235710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190235810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190235910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190236010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190236110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190236210"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *    急性血液浄化等の特定入院料算定コード（令和４年４月１日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "6"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20220401"
      *         急性血液浄化等の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190244510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190244810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３
                        MOVE   "190245110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４
                        MOVE   "190245410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190246510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２
                        MOVE   "190246710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190246910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４
                        MOVE   "190247110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *    新型コロナウイルス感染症（診療報酬上臨時的取扱）の
      *    特定入院料算定コード置き換え
      *    ・特例の算定コード（令和５年５月８日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "8"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20230508"
      *         特例の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190293550"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190293850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３（イ）
                        MOVE   "190294150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "56"
      *            救命救急入院料３（ロ）
                        MOVE   "190292750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４（イ）
                        MOVE   "190294450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "58"
      *            救命救急入院料４（ロ）
                        MOVE   "190293150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190296250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "190296450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AT"
      *            特定集中治療室管理料２（広範囲熱傷特定集中治療管理料）
                        MOVE   "190295750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190296650"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４（特定集中治療室管理料）
                        MOVE   "190296850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "60"
      *            特定集中治療室管理料４（広範囲熱傷特定集中治療管理料）
                        MOVE   "190296050"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AJ"
      *            ハイケアユニット入院医療管理料１
                        MOVE   "190297850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AK"
      *            ハイケアユニット入院医療管理料２
                        MOVE   "190298050"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "19"
      *            脳卒中ケアニット入院医療管理料
                        MOVE   "190298450"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "91"
      *            小児特定集中治療室管理料
                        MOVE   "190298750"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "10"
      *            新生児特定集中治療室管理料１
                        MOVE   "190299150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "61"
      *            新生児特定集中治療室管理料２
                        MOVE   "190299250"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "11"
      *            総合周産期特定集中治療室管理料（母体・胎児）
                        MOVE   "190299650"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "12"
      *            総合周産期特定集中治療室管理料（新生児）
                        MOVE   "190299850"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "62"
      *            新生児治療回復室入院医療管理料
                        MOVE   "190300150"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       207-EDIT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    特定入院料コードセット（令和６年６月から）
      *****************************************************************
       208-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    システム管理検索（地域包括ケア入院の注の区分取得）
           MOVE    SPACE               TO  SYS-5002-REC
           INITIALIZE                      SYS-5002-REC
           MOVE    "5002"              TO  SYS-5002-KANRICD
           MOVE    LNK-TOKNYUIN-BRMNUM TO  SYS-5002-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                       TO  SYS-5002-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5002-STYUKYMD(7:2)
           MOVE    SYS-5002-STYUKYMD   TO  SYS-5002-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5002-HOSPNUM
           MOVE    SYS-5002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5002-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5002-REC
               INITIALIZE                  SYS-5002-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    地域包括ケア病棟入院料を注の算定に置き換え
           IF  (LNK-TOKNYUIN-KBN   =   "2")  AND
               (LNK-TOKNYUIN-SANTEIYOUKENKBN  =  SPACE)
      *      一般
             IF   LNK-TOKNYUIN-NYUIN   =   "41" OR "43" OR "61" OR "63"
                                       OR  "6X" OR "6Z" OR "71" OR "73"
                 IF   SYS-5001-BTU-CHIIKIHOU-KBN  NOT = SPACE
                      MOVE   SYS-5001-BTU-CHIIKIHOU-KBN
                                        TO  LNK-TOKNYUIN-NYUIN
                 END-IF
             END-IF
      *      生活療養
             IF   LNK-TOKNYUIN-NYUIN   =   "42" OR "44" OR "62" OR "64"
                                       OR  "6Y" OR "70" OR "72" OR "74"
                 IF   SYS-5001-BTU-CHIIKIHOU-KBN  NOT = SPACE
                     EVALUATE   SYS-5001-BTU-CHIIKIHOU-KBN
                         WHEN   "75"
                           MOVE   "76"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "77"
                           MOVE   "78"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "79"
                           MOVE   "7A"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7B"
                           MOVE   "7C"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7D"
                           MOVE   "7E"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7F"
                           MOVE   "7G"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7H"
                           MOVE   "7I"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7J"
                           MOVE   "7K"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7L"
                           MOVE   "7M"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7N"
                           MOVE   "7O"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7P"
                           MOVE   "7Q"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7R"
                           MOVE   "7S"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7T"
                           MOVE   "7U"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7V"
                           MOVE   "7W"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7X"
                           MOVE   "7Y"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "7Z"
                           MOVE   "80"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "81"
                           MOVE   "82"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "83"
                           MOVE   "84"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "85"
                           MOVE   "86"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "87"
                           MOVE   "88"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "89"
                           MOVE   "8A"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8B"
                           MOVE   "8C"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8D"
                           MOVE   "8E"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8F"
                           MOVE   "8G"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8H"
                           MOVE   "8I"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8J"
                           MOVE   "8K"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8L"
                           MOVE   "8M"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8N"
                           MOVE   "8O"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8P"
                           MOVE   "8Q"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8R"
                           MOVE   "8S"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8T"
                           MOVE   "8U"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "8V"
                           MOVE   "8W"   TO  LNK-TOKNYUIN-NYUIN
                     END-EVALUATE
                 END-IF
             END-IF
           END-IF
      *
      *    地域包括ケア入院医療管理料を注の算定に置き換え
           IF  (LNK-TOKNYUIN-KBN   =   "3")   AND
               (LNK-TOKNYUIN-SANTEIYOUKENKBN  =  SPACE)
      *      一般
             IF   LNK-TOKNYUIN-NYUIN   =   "AL" OR "AN" OR "BZ" OR "C1"
                                       OR  "EW" OR "EY" OR "F0" OR "F2"
                 IF   SYS-5002-BRM-CHIIKIHOU-KBN  NOT = SPACE
                      MOVE   SYS-5002-BRM-CHIIKIHOU-KBN
                                        TO  LNK-TOKNYUIN-NYUIN
                 END-IF
             END-IF
      *      生活療養
             IF   LNK-TOKNYUIN-NYUIN   =   "AM" OR "AO" OR "C0" OR "C2"
                                       OR  "EX" OR "EZ" OR "F1" OR "F3"
                 IF   SYS-5002-BRM-CHIIKIHOU-KBN  NOT = SPACE
                     EVALUATE   SYS-5002-BRM-CHIIKIHOU-KBN
                         WHEN   "F4"
                           MOVE   "F5"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "F6"
                           MOVE   "F7"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "F8"
                           MOVE   "F9"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FA"
                           MOVE   "FB"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FC"
                           MOVE   "FD"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FE"
                           MOVE   "FF"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FG"
                           MOVE   "FH"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FI"
                           MOVE   "FJ"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FK"
                           MOVE   "FL"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FM"
                           MOVE   "FN"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FO"
                           MOVE   "FP"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FQ"
                           MOVE   "FR"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FS"
                           MOVE   "FT"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FU"
                           MOVE   "FV"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FW"
                           MOVE   "FX"   TO  LNK-TOKNYUIN-NYUIN
                         WHEN   "FY"
                           MOVE   "FZ"   TO  LNK-TOKNYUIN-NYUIN
                     END-EVALUATE
                 END-IF
             END-IF
           END-IF
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             MOVE    SPACE               TO  SYS-5110-REC
             INITIALIZE                      SYS-5110-REC
             MOVE    "5110"              TO  SYS-5110-KANRICD
             MOVE    LNK-TOKNYUIN-NYUIN  TO  SYS-5110-KBNCD
             MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                         TO  SYS-5110-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-5110-STYUKYMD(7:2)
             MOVE    SYS-5110-STYUKYMD   TO  SYS-5110-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-5110-HOSPNUM
             MOVE    SYS-5110-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key10"             TO  MCP-PATHNAME
                 PERFORM 920-DBFETCH-SEC
                 IF      MCP-RC              =   ZERO
                     MOVE    ZERO                TO  FLG-SYSKANRI
                     MOVE    MCPDATA-REC         TO  SYS-5110-REC
                 ELSE
                     MOVE    1                   TO  FLG-SYSKANRI
                 END-IF
             ELSE
                 INITIALIZE                  SYS-5110-REC
                 MOVE    1               TO  FLG-SYSKANRI
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 990-DBCLOSE-SEC
      *
             IF      FLG-SYSKANRI        =   ZERO
                 MOVE    SYS-5110-SRYCD      TO  LNK-TOKNYUIN-SRYCD
             ELSE
                 MOVE    2                   TO  LNK-TOKNYUIN-RC
                 GO  TO  208-EDIT-EXT
             END-IF
           END-IF
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "92"
      *                精神科救急急性期医療入院料（急性増悪の再入院）
                       MOVE   "190264010"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             MOVE    SPACE               TO  SYS-5111-REC
             INITIALIZE                      SYS-5111-REC
             MOVE    "5111"              TO  SYS-5111-KANRICD
             MOVE    LNK-TOKNYUIN-NYUIN  TO  SYS-5111-KBNCD
             MOVE    LNK-TOKNYUIN-SANTEIYMD(1:6)
                                         TO  SYS-5111-STYUKYMD(1:6)
             MOVE    "01"                TO  SYS-5111-STYUKYMD(7:2)
             MOVE    SYS-5111-STYUKYMD   TO  SYS-5111-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-5111-HOSPNUM
             MOVE    SYS-5111-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key10"             TO  MCP-PATHNAME
                 PERFORM 920-DBFETCH-SEC
                 IF      MCP-RC              =   ZERO
                     MOVE    ZERO                TO  FLG-SYSKANRI
                     MOVE    MCPDATA-REC         TO  SYS-5111-REC
                 ELSE
                     MOVE    1                   TO  FLG-SYSKANRI
                 END-IF
             ELSE
                 MOVE    SPACE               TO  SYS-5111-REC
                 INITIALIZE                  SYS-5111-REC
                 MOVE    1               TO  FLG-SYSKANRI
             END-IF
      *
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key10"             TO  MCP-PATHNAME
             PERFORM 990-DBCLOSE-SEC
      *
             IF      FLG-SYSKANRI        =   ZERO
                 MOVE    SYS-5111-SRYCD      TO  LNK-TOKNYUIN-SRYCD
             ELSE
                 MOVE    2                   TO  LNK-TOKNYUIN-RC
                 GO  TO  208-EDIT-EXT
             END-IF
      *       
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "HA"
      *                療養病棟入院料１（１）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190869810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190814610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190814610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HB"
      *                療養病棟入院料１（２）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190869910"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190814710"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190814710"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HC"
      *                療養病棟入院料１（３）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190814810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190814810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HD"
      *                療養病棟入院料１（４）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870110"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190814910"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190814910"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HE"
      *                療養病棟入院料１（５）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HF"
      *                療養病棟入院料１（６）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870310"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815110"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815110"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HG"
      *                療養病棟入院料１（７）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HH"
      *                療養病棟入院料１（８）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870510"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815310"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815310"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HI"
      *                療養病棟入院料１（９）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HJ"
      *                療養病棟入院料１（１０）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870710"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815510"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815510"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HK"
      *                療養病棟入院料１（１１）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HL"
      *                療養病棟入院料１（１２）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190870910"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815710"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815710"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HM"
      *                療養病棟入院料１（１３）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HN"
      *                療養病棟入院料１（１４）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871110"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190815910"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190815910"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HO"
      *                療養病棟入院料１（１５）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HP"
      *                療養病棟入院料１（１６）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871310"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816110"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816110"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HQ"
      *                療養病棟入院料１（１７）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HR"
      *                療養病棟入院料１（１８）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871510"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816310"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816310"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HS"
      *                療養病棟入院料１（１９）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HT"
      *                療養病棟入院料１（２０）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871710"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816510"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816510"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HU"
      *                療養病棟入院料１（２１）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HV"
      *                療養病棟入院料１（２２）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190871910"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816710"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816710"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HW"
      *                療養病棟入院料１（２３）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190872010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HX"
      *                療養病棟入院料１（２４）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190872110"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190816910"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190816910"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HY"
      *                療養病棟入院料１（２５）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190872210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190817010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190817010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "HZ"
      *                療養病棟入院料１（２６）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190872310"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190817110"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190817110"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "I0"
      *                療養病棟入院料１（２７）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190872410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190817210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190817210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "I1"
      *                療養病棟入院料１（２８）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190872510"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190817310"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190817310"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "I2"
      *                療養病棟入院料１（２９）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190872610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190817410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190817010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "I3"
      *                療養病棟入院料１（３０）
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190872710"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190817510"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190817510"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
                WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                        MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                        MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
                       MOVE   "1"           TO  LNK-TOKNYUIN-COVID19-FLG
               WHEN    "H7"
      *                特定集中治療室管理料５
                       MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
               WHEN    "H8"
      *                特定集中治療室管理料６（特定集中治療室管理料）
                        MOVE   "1"           TO  LNK-TOKNYUIN-BLOOD-FLG
            END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
               WHEN    "54"
               WHEN    "6W"
      *            緩和ケア病棟入院料１
      *            緩和ケア病棟入院料２
      *            特定機能病院リハビリテーション病棟入院料
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
               WHEN    "53"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            地域移行機能強化病棟入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "29"
               WHEN    "55"
               WHEN    "41"
               WHEN    "45"
               WHEN    "43"
               WHEN    "47"
               WHEN    "69"
               WHEN    "6H"
               WHEN    "6B"
               WHEN    "6J"
               WHEN    "6X"
               WHEN    "6Z"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            回復期リハビリテーション病棟入院料３
      *            回復期リハビリテーション病棟入院料４
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            地域包括ケア病棟特別入院料１
      *            地域包括ケア病棟特別入院料１（特定地域）
      *            地域包括ケア病棟特別入院料２
      *            地域包括ケア病棟特別入院料２（特定地域）
      *            地域包括ケア病棟入院料１（療養病床）
      *            地域包括ケア病棟入院料２（療養病床）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（２７）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190822810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "30"
               WHEN    "56"
               WHEN    "42"
               WHEN    "46"
               WHEN    "44"
               WHEN    "48"
               WHEN    "6A"
               WHEN    "6I"
               WHEN    "6C"
               WHEN    "6K"
               WHEN    "6Y"
               WHEN    "70"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            回復期リハビリテーション病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料１（生活療養）
      *            地域包括ケア病棟特別入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料２（生活療養）
      *            地域包括ケア病棟特別入院料２（生活療養）（特定地域）
      *            地域包括ケア病棟入院料１（生活療養・療養病床）
      *            地域包括ケア病棟入院料２（生活療養・療養病床）
      *            療養病棟の時は療養病棟入院料１（２７）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190822910"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "61"
               WHEN    "65"
               WHEN    "63"
               WHEN    "67"
               WHEN    "6D"
               WHEN    "6L"
               WHEN    "6F"
               WHEN    "6N"
               WHEN    "71"
               WHEN    "73"
      *            地域包括ケア病棟入院料３
      *            地域包括ケア病棟入院料３（特定地域）
      *            地域包括ケア病棟入院料４
      *            地域包括ケア病棟入院料４（特定地域）
      *            地域包括ケア病棟特別入院料３
      *            地域包括ケア病棟特別入院料３（特定地域）
      *            地域包括ケア病棟特別入院料４
      *            地域包括ケア病棟特別入院料４（特定地域）
      *            地域包括ケア病棟入院料３（療養病床）
      *            地域包括ケア病棟入院料４（療養病床）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190828810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "62"
               WHEN    "66"
               WHEN    "64"
               WHEN    "68"
               WHEN    "6E"
               WHEN    "6M"
               WHEN    "6G"
               WHEN    "6O"
               WHEN    "72"
               WHEN    "74"
      *            地域包括ケア病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料４（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料３（生活療養）
      *            地域包括ケア病棟特別入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟特別入院料４（生活療養）
      *            地域包括ケア病棟特別入院料４（生活療養）（特定地域）
      *            地域包括ケア病棟入院料３（生活療養・療養病床）
      *            地域包括ケア病棟入院料４（生活療養・療養病床）
      *            療養病棟の時は療養病棟入院料２（２７）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190828910"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "6T"
      *            回復期リハビリテーション病棟入院料５
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（２７）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190828810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "6U"
      *            回復期リハビリテーション病棟入院料５（生活療養）
      *            療養病棟の時は療養病棟入院料２（２７）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190828910"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "8Z"
      *            精神科地域包括ケア病棟入院料
      *            精神病棟の時は精神病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
               WHEN    "EW"
               WHEN    "AN"
               WHEN    "AR"
               WHEN    "EY"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            地域包括ケア入院医療管理料１（療養病床）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            地域包括ケア入院医療管理料２（療養病床）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（２７）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190822810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
               WHEN    "EX"
               WHEN    "AO"
               WHEN    "AS"
               WHEN    "EZ"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料１（生活療養・療養病床）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養・療養病床）
      *            療養病棟の時は療養病棟入院料１（２７）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190822910"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "BZ"
               WHEN    "C3"
               WHEN    "F0"
               WHEN    "C1"
               WHEN    "C5"
               WHEN    "F2"
      *            地域包括ケア入院医療管理料３
      *            地域包括ケア入院医療管理料３（特定地域）
      *            地域包括ケア入院医療管理料３（療養病床）
      *            地域包括ケア入院医療管理料４
      *            地域包括ケア入院医療管理料４（特定地域）
      *            地域包括ケア入院医療管理料４（療養病床）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（２７）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190828810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "C0"
               WHEN    "C4"
               WHEN    "F1"
               WHEN    "C2"
               WHEN    "C6"
               WHEN    "F3"
      *            地域包括ケア入院医療管理料３（生活療養）
      *            地域包括ケア入院医療管理料３（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料３（生活療養・療養病床）
      *            地域包括ケア入院医療管理料４（生活療養）
      *            地域包括ケア入院医療管理料４（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料４（生活療養・療養病床）
      *            療養病棟の時は療養病棟入院料２（２７）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190828910"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
               WHEN    "CD"
               WHEN    "CE"
      *            特定一般病棟入院料（地域包括ケア１）
      *            特定一般病棟入院料（地域包括ケア２）
      *            特定一般病棟入院料（地域包括ケア３）
      *            特定一般病棟入院料（地域包括ケア４）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
      *
      *    急性血液浄化等の特定入院料算定コード（令和４年４月１日以降算定可）
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "6"
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               IF  LNK-TOKNYUIN-SANTEIYMD    >=   "20220401"
      *         急性血液浄化等の算定コード
                 EVALUATE    LNK-TOKNYUIN-NYUIN
                 WHEN    "03"
      *            救命救急入院料１
                        MOVE   "190244510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "06"
      *            救命救急入院料２
                        MOVE   "190244810"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "55"
      *            救命救急入院料３
                        MOVE   "190245110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "57"
      *            救命救急入院料４
                        MOVE   "190245410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AH"
      *            特定集中治療室管理料１
                        MOVE   "190246510"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "AI"
      *            特定集中治療室管理料２
                        MOVE   "190246710"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "16"
      *            特定集中治療室管理料３
                        MOVE   "190246910"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "59"
      *            特定集中治療室管理料４
                        MOVE   "190247110"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "H7"
      *            特定集中治療室管理料５
                        MOVE   "190835010"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 WHEN    "H8"
      *            特定集中治療室管理料６
                        MOVE   "190835410"  TO  LNK-TOKNYUIN-SRYCD
                        MOVE   SPACE        TO  LNK-TOKNYUIN-GAI-SRYCD
                 END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       208-EDIT-EXT.
           EXIT.
      ******************************************************************
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
