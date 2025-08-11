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
      ******************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.          ORAPION002R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 顔認証資格確認登録、更新処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  20/11/06    NACL−藤原    新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01     ORCAMO       21/06/10  資格確認：要求ファイル名修正
      *  05.00.02     ORCAMO       21/07/05  資格確認：照会番号に患者番号を設定
      *  05.00.03     ORCAMO       21/07/12  資格確認：住所から全角空白を除く
      *  05.00.04     ORCAMO       21/09/28  資格確認：公費取り込み対応
      *  05.01.01     ORCAMO       21/12/08  資格確認：後期高齢者の枝番対応
      *  05.01.02     ORCAMO       22/04/25  資格確認：間違って登録され
      *                                      ている照会番号の再登録依頼
      *  05.01.03    ORCAMO        22/07/15  資格確認：診療情報対応
      *  05.01.04    ORCAMO        22/08/22  保険証ＯＣＲ(アルメックス）対応
      *  05.01.05    ORCAMO        22/12/09  処方箋発行形態、手術情報対応
      *  05.01.07    ORCAMO        22/01/17  電子処方箋対応
      *  05.02.01    ORCAMO        23/12/21  医療扶助対応
      *  05.02.02    ORCAMO        24/05/13  エラーデータ対応
      *                                      被保険者証区分が空白の場合
      *  05.02.03    ORCAMO        24/07/26  資格取得年月日追加
      *                                      保険情報が主保険の場合、
      *                                      資格取得年月日を設定する
      *  05.02.04    ORCAMO        24/11/20  医療扶助対応
      *                                      資格確認結果の保険情報が空白
      *                                      のとき、公費情報の検索は行わない
      *  05.02.05    ORCAMO        24/11/21  資格確認結果の保険情報から検索
      *                                      した保険情報がテスト患者のとき
      *                                      対象にしない
      *  05.02.06    ORCAMO        24/11/15  訪問診療対応
      *  05.02.07    ORCAMO        25/01/17  電子カルテ共有サービス対応
      *  05.02.08    ORCAMO        25/02/20  照会番号と保険情報から取得した
      *                                      患者番号が異なる場合の対応
      *                                      訪問診療のときの薬剤等の取得
      *                                      依頼用のレスポンスを返却
      *  05.02.09    ORCAMO        25/02/27  公費情報検索時のチェック追加
      *  05.02.10    ORCAMO        25/03/07  オンライン診療対応
      *  05.02.11    ORCAMO        25/06/09  旧医療機関コード対応
      ******************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-ONSHI-KAKU          PIC 9(01).
           03  FLG-ONSHI-YKZ           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-MONTHLYNUM          PIC 9(01).
      *
          03  PTHKNINF-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HOSPCD              PIC X(10).
           03  WRK-SIKIBETU            PIC X(50).
           03  WRK-OPID                PIC X(16).
           03  WRK-PTID-AREA.
               05  WRK-PTID            PIC 9(10).
               05  WRK-PATIENT-ID      PIC X(20).
      *         
           03  WRK-FILE-PATIENT-ID     PIC X(20).
           03  WRK-UUID                PIC X(36).
      *
           03  WRK-MCP-PATHNAME        PIC X(62).
           03  WRK-MCP-TABLE           PIC X(62).
      *
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
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
      *    資格確認用エラーコード     
           03  WRK-ERR-CODE            PIC X(09).
           03  WRK-ERR-PTID            PIC 9(10).
      *    全角変換用 
           03  WRK-KANACHK-MAE-INPUT   PIC X(5000).
           03  WRK-KANACHK-OUT-INPUT   PIC X(5000).
           03  WRK-KANACHK-LEN         PIC 9(04).
      *
           03  WRK-SPA-SYSYMD-1        PIC X(08).     
      *    公費の処理実行日時
           03  WRK-KOH-SHORI-TIME      PIC X(14).
      *
      *    テスト患者チェック
           03  CHK-PTID-AREA.           
               05  CHK-PTID            PIC 9(10).
               05  CHK-PTID-SYU        PIC X(05).
               05  CHK-PTID-FLG        PIC 9(01).
      *
       01  WRK-QUAREQ2-AREA.     
           03  WRK-QUAREQ2-SHO-KIGO    PIC X(80).
           03  WRK-QUAREQ2-SHO-NUM     PIC X(80).
           03  WRK-QUAREQ2-RESULT-HKN  PIC 9(02).
           03  WRK-QUAREQ2-RESULT-MAX  PIC 9(02).
           03  WRK-QUAREQ2-RESULT-AREA OCCURS  50.
               05  WRK-QUAREQ2-RES-KIGO
                                       PIC X(80).
               05  WRK-QUAREQ2-RES-NUM PIC X(80).
               05  WRK-QUAREQ2-RES-HIHKNJANAME
                                       PIC X(200).
               05  WRK-QUAREQ2-RES-NAME
                                       PIC X(200).
               05  WRK-QUAREQ2-RES-NAME-ETC
                                       PIC X(200).
               05  WRK-QUAREQ2-RES-KANANAME
                                       PIC X(200).
               05  WRK-QUAREQ2-RES-KANANAME1
                                       PIC X(200).
               05  WRK-QUAREQ2-RES-KANANAME2
                                       PIC X(200).
               05  WRK-QUAREQ2-RES-KANANAME-ETC
                                       PIC X(200).
               05  WRK-QUAREQ2-RES-ADRS
                                       PIC X(500).
               05  WRK-QUAREQ2-HKNJANUM-NAME
                                       PIC X(200).
      *        PTID不一致で初期化したときのエラーコード
               05  WRK-QUAREQ2-ERR-CODE
                                       PIC X(09).
               05  WRK-QUAREQ2-ERR-PTID
                                       PIC 9(10).
      *    検索したPTID
           03  WRK-QUAREQ2-PTID-AREA.
               05  WRK-QUAREQ2-PTID    PIC 9(10)   OCCURS  50.
      *    設定するPTID
           03  WRK-QUAREQ2-PTID-NEW-AREA.
               05  WRK-QUAREQ2-N-PTID  PIC 9(10)   OCCURS  50.
      *
      *    保険証の資格確認情報     
       01  WRK-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //WRK-ONSKAKU//.
      *
      *    顔認証の資格確認情報（公費の資格確認情報の親ＵＩＤ）
       01  KOH-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //KOH-ONSKAKU//.
      * 
      *    退避資格管理情報（保険証による）
       01  HKN-ONSHI-KAKU-AREA.
           02  HKN-ONSHI-KAKU-OK       PIC 9(01).
           02  HKN-ONSHI-KAKU-MAX      PIC 9(02). 
           02  HKN-ONSHI-KAKU-IDX      PIC 9(02).
           02  HKN-ONSHI-KAKU-REC      OCCURS  50.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //HKN-ONSKAKU//.
      * 
      *    退避患者保険情報
       01  WRK-PTHKNINF-AREA.
           02  WRK-PTHKNINF-OK     PIC 9(02). 
           02  WRK-PTHKNINF-MAX    PIC 9(02).
           02  FLG-PTHKNINF-EDABAN PIC 9(01). 
           02  WRK-PTHKNINF-REC    OCCURS  10.   
           COPY    "CPPTHKNINF.INC"
                                   REPLACING   //PTHKN//
                                   BY          //WRK-PTHKN//.
      * 
      *    退避患者公費情報
       01  WRK-PTKOHINF-AREA.
           02  WRK-PTKOHINF-OK     PIC 9(02). 
           02  WRK-PTKOHINF-MAX    PIC 9(02).
           02  WRK-PTKOHINF-REC    OCCURS  10.   
               03  WRK-PTKOH-PTID  PIC 9(10).
      *****COPY    "CPPTKOHINF.INC"
      *****                        REPLACING   //PTKOH//
      *****                        BY          //WRK-PTKOH//.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUAREQ2.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUARES1.INC".
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
           COPY    "MCPDATA2.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1001.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK1051.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *     
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    資格確認テーブル
       01  ONSKAKU-REC.       
           COPY    "CPONSHI-KAKU.INC".
      *
      *    資格確認薬剤情報
       01  ONSHI-YAKUZAI-MAIN-REC.       
           COPY    "CPONSHI-YAKUZAI-MAIN.INC".
      *
      *    処方箋管理
       01  SHOHO-KANRI-REC.
           COPY    "CPSHOHO-KANRI.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA14SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "**********************"
           DISPLAY   "*  onlinequa2 start  *"
           DISPLAY   "**********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           IF      WRK-ERRCD           =   "999"
      *        ユーザＩＤエラー
               MOVE   404                  TO  APILST02-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "*   onlinequa2 end   *"
           DISPLAY   "**********************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      XML-ONLINEQUARES1
           INITIALIZE                      WRK-QUAREQ2-AREA
      *
           MOVE    SPACE               TO  QUARES1-QUARES1
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *****MOVE    "20281010"          TO  SMCNDATE-YMD
      *
           STRING  "A+"                DELIMITED  BY  SIZE
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    システム日付の前月      
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SPA-SYSYMD-1
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "999"               TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      * 
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "E89"            TO  WRK-ERRCD
               GO  TO  100-INIT-EXT
           END-IF
      *
      ******INITIALIZE                  SYS-1001-REC
      *     MOVE    "1001"              TO  SYS-1001-KANRICD
      *     MOVE    "*"                 TO  SYS-1001-KBNCD
      *     MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
      *                                     SYS-1001-EDYUKYMD 
      *     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *     MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *     PERFORM 900-SYSKANRI-READ-SEC
      *     IF      FLG-SYSKANRI        =   ZERO
      *         MOVE    MCPDATA-REC     TO  SYS-1001-REC
      *     ELSE
      *         DISPLAY "HOSPCD NOT FOUND"
      *         MOVE    "E??"           TO  WRK-ERRCD
      *         GO  TO  100-INIT-EXT
      *     END-IF
      *
      *    医療機関コード設定
      *     STRING  SPA-PREFNUM         DELIMITED  BY  SIZE
      *             "1"                 DELIMITED  BY  SIZE
      *             SYS-1001-HOSPCD     DELIMITED  BY  SIZE
      *                                 INTO    WRK-HOSPCD
      *****END-STRING
      *
      *    オンライン資格確認照会情報取得 
           INITIALIZE                  SYS-1051-REC
           MOVE    "1051"          TO  SYS-1051-KANRICD
           MOVE    "*"             TO  SYS-1051-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1051-STYUKYMD
                                       SYS-1051-EDYUKYMD 
           MOVE    SPA-HOSPNUM     TO  SYS-1051-HOSPNUM
           MOVE    SYS-1051-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1051-REC
           ELSE
               INITIALIZE                  SYS-1051-REC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    SPACE           TO  WRK-ERRCD
      *
      *    XML情報取り出し
           PERFORM 1000-RGTXMLREAD-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    入力項目チェック処理
           PERFORM 2001-INPUT-CHK-SEC
           IF      WRK-ERRCD   NOT =   SPACE
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    資格確認登録更新処理
           PERFORM 2002-SIKAKU-KAKUNIN-UPDATE-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       1000-RGTXMLREAD-SEC   SECTION.
      *
           IF      APILST02-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAREQ2
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinequareq2" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST02-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinequareq2"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAREQ2
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "E98"             TO  WRK-ERRCD
           END-IF
      *
           .
       1000-RGTXMLREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報書き込み処理
      *****************************************************************
       300-RGTXMLMAKE-SEC   SECTION.
      *
           IF      WRK-ERRCD           =   SPACE
      *        正常終了
               CONTINUE
           ELSE
               DISPLAY "300 WRK-ERRCD=" WRK-ERRCD
      *        エラーメッセージ
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  QUARES1-API-RESULT
               MOVE    WRK-ERRMSG      TO  QUARES1-API-RESULT-MSG
            END-IF
      *
           IF      APILST02-BODY       NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "xml_onlinequares1" TO  MCP-TABLE
           MOVE    1                   TO  QUARES1-MODE
           MOVE    "onlinequares1"     TO  QUARES1-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-ONLINEQUARES1
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO    OR  1 )
               MOVE  QUARES1-OBJECT    TO  APILST02-BODY
               MOVE  "application/xml" TO  APILST02-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       300-RGTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           IF      QUAREQ2-PATIENTREQ
                               =   SPACE 
               MOVE    "E15"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           IF      QUAREQ2-SRYKBN
                               =   "2" OR  "3"
               CONTINUE
           ELSE    
               IF      QUAREQ2-SHOKAI-KBN
                                   =   "1"
                   CONTINUE
               ELSE
                   DISPLAY "SHOKAI-KBN=" QUAREQ2-SHOKAI-KBN
                   MOVE    "E01"           TO  WRK-ERRCD
                   GO  TO  2001-INPUT-CHK-EXT
               END-IF
           END-IF 
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認登録、更新処理
      *****************************************************************
       2002-SIKAKU-KAKUNIN-UPDATE-SEC  SECTION.
      *
           INITIALIZE                  KOH-ONSKAKU-REC
                                       WRK-ONSKAKU-REC
      * 
      *    扶助UIDコード設定
           IF      QUAREQ2-FUJYO-UUID  =   SPACE 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       50
                       OR      QUAREQ2-CARD-CLASS (IDX)
                                   =       SPACE 
                   IF      QUAREQ2-CARD-CLASS (IDX)
                                           =   "A1"
                       PERFORM 200211-UID-GET-SEC
                       MOVE    C-UID           TO  QUAREQ2-FUJYO-UUID
                       MOVE    50              TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    一番目の被保険者証区分が空白のとき「０１」を設定
           IF      QUAREQ2-CARD-CLASS (1)  =   SPACE
               MOVE    "01"                TO  QUAREQ2-CARD-CLASS (1)
           END-IF    
      *
      *    全角文字変換処理
           MOVE    QUAREQ2-SHO-KIGO
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ2-SHO-KIGO
           IF    ( QUAREQ2-SHO-HKNJANUM (1:2)
                                   =   "12"  )
           AND   ( QUAREQ2-SHO-HKNJANUM (7:2)
                               NOT =   SPACE )
      *        負担者番号が医療扶助のとき番号は半角のまま
               MOVE    QUAREQ2-SHO-NUM TO  WRK-QUAREQ2-SHO-NUM
           ELSE
               MOVE    QUAREQ2-SHO-NUM TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ2-SHO-NUM
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
                   OR      QUAREQ2-CARD-CLASS (IDX)
                               =       SPACE 
               INITIALIZE              WRK-PTID-AREA
               INITIALIZE              HKN-ONSHI-KAKU-AREA
      *
      *        全角文字変換処理
               MOVE    QUAREQ2-RES-KIGO (IDX)
                                       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ2-RES-KIGO (IDX)
               IF      QUAREQ2-CARD-CLASS (IDX)
                                       =   "A1"
                   MOVE    QUAREQ2-RES-NUM (IDX)
                                           TO  WRK-QUAREQ2-RES-NUM (IDX)
               ELSE                            
                   MOVE    QUAREQ2-RES-NUM (IDX)
                                           TO  WRK-KANACHK-MAE-INPUT
                   PERFORM 800-KANACHK-SEC
                   MOVE    WRK-KANACHK-OUT-INPUT
                                           TO  WRK-QUAREQ2-RES-NUM (IDX)
               END-IF                            
               MOVE    QUAREQ2-RES-HIHKNJANAME (IDX)
                                       TO  WRK-KANACHK-MAE-INPUT
               MOVE    200             TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ2-RES-HIHKNJANAME
                                                                   (IDX)
               MOVE    QUAREQ2-RES-NAME (IDX)
                                       TO  WRK-KANACHK-MAE-INPUT
               MOVE    200             TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ2-RES-NAME (IDX)
                MOVE    QUAREQ2-RES-NAME-ETC (IDX)
                                       TO  WRK-KANACHK-MAE-INPUT
               MOVE    200             TO  WRK-KANACHK-LEN
               PERFORM 801-KANACHK2-SEC                       
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ2-RES-NAME-ETC
                                                                   (IDX)
               MOVE    QUAREQ2-RES-KANANAME (IDX)
                                       TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ2-RES-KANANAME
                                                                   (IDX)
               UNSTRING    WRK-QUAREQ2-RES-KANANAME (IDX)
                                       DELIMITED   BY  "　"
                                       INTO    WRK-QUAREQ2-RES-KANANAME1
                                                                   (IDX)
                                               WRK-QUAREQ2-RES-KANANAME2
                                                                   (IDX)
               END-UNSTRING
      ****     IF    ( WRK-QUAREQ2-RES-KANANAME1   =   SPACE )
      ****     OR    ( WRK-QUAREQ2-RES-KANANAME2   =   SPACE )
      ****         MOVE    SPACE       TO  WRK-QUAREQ2-RES-KANANAME1
      ***                                  WRK-QUAREQ2-RES-KANANAME2
      ****     END-IF
               MOVE    QUAREQ2-RES-KANANAME-ETC (IDX)
                                       TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ2-RES-KANANAME-ETC
                                                                   (IDX)
                MOVE    QUAREQ2-RES-ADRS (IDX)
                                       TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    1               TO  IDYY
               PERFORM VARYING IDZZ    FROM    1  BY  2
                       UNTIL   IDZZ    >       250
                       OR      WRK-KANACHK-OUT-INPUT (IDZZ:)
                                       =       SPACE
                   IF      WRK-KANACHK-OUT-INPUT (IDZZ:2)
                                       NOT =   "　"
                       MOVE    WRK-KANACHK-OUT-INPUT (IDZZ:2)
                                       TO  WRK-QUAREQ2-RES-ADRS
                                                           (IDX)(IDYY:2)
                       ADD     2       TO  IDYY
                   END-IF
               END-PERFORM
               MOVE    QUAREQ2-HKNJANUM-NAME (IDX)
                                       TO    WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ2-HKNJANUM-NAME
                                                                   (IDX)
      *
      *        訪問診療のとき
               IF      QUAREQ2-SRYKBN  =   "2" OR  "3"
                   IF      QUAREQ2-CARD-CLASS (IDX)
                                           =   "A1"
                       CONTINUE
                   ELSE
                       MOVE    IDX         TO  WRK-QUAREQ2-RESULT-HKN
                   END-IF    
               ELSE
      *            顔認証資格確認から患者ＩＤ取得
                   IF      QUAREQ2-CARD-CLASS (IDX)
                                           =   "A1"
                       PERFORM 20023-PTID-03-SEC
                   ELSE
                       MOVE    IDX         TO  WRK-QUAREQ2-RESULT-HKN
                       IF      QUAREQ2-SHOKAI-NUM (IDX)
                                                   =   SPACE
                           PERFORM 20021-PTID-01-SEC
                       ELSE                           
                           PERFORM 20022-PTID-02-SEC
                       END-IF
                   END-IF
               END-IF    
      *
               COMPUTE WRK-QUAREQ2-RESULT-MAX  =
                                       WRK-QUAREQ2-RESULT-MAX  +   1
           END-PERFORM
      *
      *    訪問診療のとき
           IF      QUAREQ2-SRYKBN  =   "2" OR  "3"
      *        訪問診療資格確認登録、更新処理
               PERFORM 2002-SIKAKU-HOUMON-UPDATE-SEC
               GO  TO  2002-SIKAKU-KAKUNIN-UPDATE-EXT
           END-IF
      *
      *    複数返却時のＰＴＩＤの設定
            MOVE    WRK-QUAREQ2-PTID-AREA
                                       TO  WRK-QUAREQ2-PTID-NEW-AREA 
           IF      WRK-QUAREQ2-RESULT-HKN
                                       >   ZERO
      *        保険と医療扶助の併用のとき
               IF      WRK-QUAREQ2-N-PTID (WRK-QUAREQ2-RESULT-HKN)
                                           =   ZERO
      *            保険のＰＴＩＤが設定できなかったとき全てのＰＴＩＤを設定しない
                   INITIALIZE         WRK-QUAREQ2-PTID-NEW-AREA
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-QUAREQ2-RESULT-MAX
                       IF      QUAREQ2-CARD-CLASS (IDX)
                                               =   "A1"
      *                    医療扶助のＰＴＩＤが設定できなかったとき保険のＰＴＩＤを設定
                           IF      WRK-QUAREQ2-N-PTID (IDX)
                                                   =   ZERO
                               MOVE    WRK-QUAREQ2-N-PTID
                                               (WRK-QUAREQ2-RESULT-HKN)
                                           TO  WRK-QUAREQ2-N-PTID (IDX)
                           ELSE                            
      *                        医療扶助のＰＴＩＤが保険のＰＴＩＤと異なったとき全てのＰＴＩＤを設定しない
                               IF      WRK-QUAREQ2-N-PTID (IDX)
                                               NOT =
                                           WRK-QUAREQ2-N-PTID
                                               (WRK-QUAREQ2-RESULT-HKN)
                                   INITIALIZE  WRK-QUAREQ2-PTID-NEW-AREA
                                   MOVE    WRK-QUAREQ2-RESULT-MAX
                                                              TO  IDX
                               END-IF
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
      *        医療扶助単独のとき
               PERFORM VARYING IDX     FROM    2   BY  1
                       UNTIL   IDX     >=      WRK-QUAREQ2-RESULT-MAX
      *            検索したＰＴＩＤが異なってる場合は設定しない
                   IF      WRK-QUAREQ2-N-PTID (1)
                                       NOT =   WRK-QUAREQ2-N-PTID (IDX)
                        INITIALIZE             WRK-QUAREQ2-PTID-NEW-AREA
                        MOVE    WRK-QUAREQ2-RESULT-MAX
                                               TO  IDX
                  END-IF
               END-PERFORM
           END-IF
      *    資格確認から設定したＰＴＩＤと違うＰＴＩＤを設定したとき
      *    エラーコード設定
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-QUAREQ2-RESULT-MAX
               IF      WRK-QUAREQ2-PTID (IDX)
                                       NOT =   WRK-QUAREQ2-N-PTID (IDX)
                   MOVE    "ORCAE18"               TO
                                           WRK-QUAREQ2-ERR-CODE (IDX)
                   MOVE    WRK-QUAREQ2-PTID (IDX)  TO
                                           WRK-QUAREQ2-ERR-PTID (IDX)
               END-IF
           END-PERFORM
      *
           IF    ( QUAREQ2-SIKAKU-YUKO
                                   =   "3" OR  "4" )
           AND   ( QUAREQ2-CARD-CLASS (1)
                                   =   SPACE       )
               MOVE    1               TO  IDX
               IF      QUAREQ2-SHOKAI-NUM (IDX)
                                           =   SPACE
                   PERFORM 20021-SYORI-01-SEC
               ELSE                           
                   PERFORM 20022-SYORI-02-SEC
               END-IF
               GO  TO  2002-SIKAKU-KAKUNIN-UPDATE-EXT
           END-IF
      *
      *    顔認証資格確認登録、更新処理
           IF      WRK-QUAREQ2-RESULT-HKN
                                       >   ZERO
      *        保険                                 
               MOVE    WRK-QUAREQ2-RESULT-HKN
                                       TO  IDX
               INITIALIZE              WRK-PTID-AREA
               MOVE    WRK-QUAREQ2-N-PTID (IDX)
                                       TO  WRK-PTID
               IF      QUAREQ2-SHOKAI-NUM (IDX)
                                           =   SPACE
                   PERFORM 20021-SYORI-01-SEC
               ELSE                           
                   PERFORM 20022-SYORI-02-SEC
               END-IF
           END-IF    
      *    医療扶助                                 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
                   OR      QUAREQ2-CARD-CLASS (IDX)
                               =       SPACE 
               INITIALIZE              WRK-PTID-AREA
               MOVE    WRK-QUAREQ2-N-PTID (IDX)
                                       TO  WRK-PTID
               IF      QUAREQ2-CARD-CLASS (IDX)
                                   =   "A1"
                   PERFORM 20023-SYORI-03-SEC
               END-IF    
           END-PERFORM
           .        
       2002-SIKAKU-KAKUNIN-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    訪問診療資格確認登録、更新処理
      *****************************************************************
       2002-SIKAKU-HOUMON-UPDATE-SEC   SECTION.
      * 
           UNSTRING    QUAREQ2-FILE-SIKIBETU
                                   DELIMITED   BY  ","
                                   INTO    WRK-PTID
                                           WRK-UUID
           END-UNSTRING
           MOVE    SPACE           TO  QUAREQ2-FILE-SIKIBETU
           STRING   WRK-PTID       DELIMITED  BY  SIZE
                    ","            DELIMITED  BY  SIZE
                    WRK-UUID       DELIMITED  BY  SPACE
                                   INTO    QUAREQ2-FILE-SIKIBETU
           END-STRING
      *
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    WRK-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        =   00
               MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
           END-IF
      *
           IF    ( QUAREQ2-SIKAKU-YUKO
                                   =   "3" OR  "4" )
           AND   ( QUAREQ2-CARD-CLASS (1)
                                   =   SPACE       )
               MOVE    1               TO  IDX
               PERFORM 20024-SYORI-HOU-01-SEC
               GO  TO  2002-SIKAKU-HOUMON-UPDATE-EXT
           END-IF
      *
      *    顔認証資格確認登録、更新処理
           IF      WRK-QUAREQ2-RESULT-HKN
                                       >   ZERO
      *        保険                                 
               MOVE    WRK-QUAREQ2-RESULT-HKN
                                       TO  IDX
               PERFORM 20024-SYORI-HOU-01-SEC
           END-IF    
      *    医療扶助                                 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
                   OR      QUAREQ2-CARD-CLASS (IDX)
                               =       SPACE 
               IF      QUAREQ2-CARD-CLASS (IDX)
                                   =   "A1"
                   PERFORM 20023-SYORI-03-SEC
               END-IF    
           END-PERFORM
           .        
       2002-SIKAKU-HOUMON-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認患者ＩＤ取得（照会番号の設定がない場合）
      *     医療扶助以外
      *****************************************************************
       20021-PTID-01-SEC  SECTION.
      *
      *    保険情報から患者保険情報を検索
           PERFORM 200211-PTHKNINF-TAIHI-SEC
      *
           IF      WRK-PTHKNINF-MAX    >   ZERO
               MOVE    ZERO            TO  WRK-PTHKNINF-OK
                                           FLG-PTHKNINF-EDABAN
      *
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       WRK-PTHKNINF-MAX
                       OR      WRK-PTHKNINF-OK >   ZERO
                   IF    ( WRK-PTHKN-EDABAN (IDY)
                                           =   SPACE )
                   AND   ( WRK-PTHKN-HKNNUM (IDY)
                                       NOT =   "039" )
                       MOVE    1               TO  FLG-PTHKNINF-EDABAN
                   ELSE
                       IF      QUAREQ2-RES-EDABAN   (IDX)
                                           =   WRK-PTHKN-EDABAN (IDY)
                           MOVE    IDY             TO  WRK-PTHKNINF-OK
                       END-IF
                   END-IF
               END-PERFORM
      *
               IF      WRK-PTHKNINF-OK >   ZERO
      *            枝番まで一致した場合
                   MOVE    WRK-PTHKN-PTID (WRK-PTHKNINF-OK)
                                           TO  WRK-QUAREQ2-PTID (IDX)
      *        ELSE
      *            全ての患者保険情報に枝番が設定されていて、一致しなかったとき
      *            IF      FLG-PTHKNINF-EDABAN =   ZERO
      *                CONTINUE
      *            ELSE
      *                枝番の設定がない場合患者情報から検索し性別、生年月日が
      *                一致すればカナ氏名と一致するか
      *                PERFORM VARYING IDY FROM    1   BY  1
      *                        UNTIL   IDY >       WRK-PTHKNINF-MAX
      *                        OR      WRK-PTHKNINF-OK  >   ZERO
      *                  IF      WRK-PTHKN-EDABAN (IDY)
      *                                         NOT =   SPACE
      *                    INITIALIZE                  PTINF-REC
      *                    MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
      *                    MOVE    PTHKN-PTID      TO  PTINF-PTID
      *                    PERFORM 900-PTINF-READ-SEC
      *                    IF      FLG-PTINF       =   ZERO 
      *                        IF    (  QUAREQ2-RES-SEX1     (IDX)
      *                                            =   PTINF-SEX      )
      *                        AND   (  QUAREQ2-RES-BIRTHDAY (IDX)
      *                                            =   PTINF-BIRTHDAY )
      *                            IF    ( WRK-QUAREQ2-RES-KANANAME
      *                                             =   PTINF-KANANAME )
      *                                 MOVE    IDY TO  WRK-PTHKNINF-OK
      *                            END-IF
      *                        END-IF
      *                    END-IF 
      *                  END-IF                                     
      *                END-PERFORM
      *                IF      WRK-PTHKNINF-OK     =   ZERO
      *                    CONTINUE
      *                ELSE
      *                    MOVE    WRK-PTHKN-PTID (WRK-PTHKNINF-OK)
      *                                         TO  WRK-PTID    
      *                END-IF
      *            END-IF
               END-IF
           END-IF
      *
          .        
       20021-PTID-01-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認登録、更新処理（照会番号の設定がない場合）
      *     医療扶助以外
      *****************************************************************
       20021-SYORI-01-SEC  SECTION.
      *
           MOVE    SPACE           TO  QUARES1-QUARES1
           INITIALIZE                  ONSKAKU-REC
      * 
      *    UIDコード取得処理
           PERFORM 200211-UID-GET-SEC
      *    任意のファイル識別子(医療機関固有項目)の設定
           STRING  WRK-PTID        DELIMITED  BY  SIZE
                   ","             DELIMITED  BY  SIZE  
                   C-UID           DELIMITED  BY  SIZE
                                   INTO       WRK-SIKIBETU
           END-STRING
      *
           IF      WRK-PTID        =   ZERO
               MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                       TO  QUARES1-PATIENT-ID
      *        資格確認レコードの編集処理（顔認証）
               PERFORM 200221-ONSHI-KAKU-HENSYU-SEC
           ELSE
      *2-1     
      *        照会番号から患者番号取得
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    WRK-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                               SPA-AREA
               IF      SPTID-RC        =   00
                   MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
               END-IF
      *
      *        保険証による資格確認の退避処理（最新の作成日時）
               PERFORM 2002201-ONSHI-KAKU-TAIHI-SEC
      *        保険証による資格確認テーブル検索（最新）
               PERFORM VARYING IDY FROM    1   BY  1
                          UNTIL   IDY >       HKN-ONSHI-KAKU-MAX
                          OR      HKN-ONSHI-KAKU-IDX  >   ZERO 
      *2-1-1
      *2-1-2
                   IF    ( QUAREQ2-RES-HKNJANUM (IDX)
                                       =
                                       HKN-ONSKAKU-SHO-HKNJANUM  (IDY) )
                   AND   ( WRK-QUAREQ2-RES-KIGO (IDX)
                                       =   HKN-ONSKAKU-SHO-KIGO  (IDY) )
                   AND   ( WRK-QUAREQ2-RES-NUM  (IDX) 
                                       =   HKN-ONSKAKU-SHO-NUM   (IDY) )
                   AND   ( QUAREQ2-RES-EDABAN   (IDX)
                                       =   HKN-ONSKAKU-SHO-EDABAN(IDY) )
                       MOVE    C-UID       TO
                                           HKN-ONSKAKU-AITE-UUID (IDY)
                       MOVE    HKN-ONSKAKU-TBL-UUID (IDY)
                                           TO  ONSKAKU-AITE-UUID
                       MOVE    IDY         TO  HKN-ONSHI-KAKU-IDX
                   END-IF                        
               END-PERFORM
      *        照会番号、薬剤、特定検診の返却処理
               IF      WRK-PTID        >   ZERO
                   PERFORM 200222-RES1-HENSYU-SEC
               END-IF
      *        資格確認レコードの編集処理（顔認証）
               PERFORM 200221-ONSHI-KAKU-HENSYU-SEC
      *        資格確認レコードの更新処理（保険証）
               IF      HKN-ONSHI-KAKU-IDX >   ZERO
                   PERFORM 200223-ONSHI-KAKU-UPDATE-SEC
               END-IF
           END-IF
           .        
       20021-SYORI-01-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険情報から患者保険情報を検索
      ******************************************************************
       200211-PTHKNINF-TAIHI-SEC    SECTION.
      *
           INITIALIZE                  WRK-PTHKNINF-AREA
      *         
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                   TO  PTHKN-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-KIGO (IDX) 
                                   TO  PTHKN-KIGO
           MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                   TO  PTHKN-NUM
           MOVE    SMCNDATE-YMD    TO  PTHKN-TEKSTYMD
                                       PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key12"         TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           PERFORM         UNTIL   FLG-PTHKNINF        =   1
      *        テスト患者チェック処理
               INITIALIZE                  CHK-PTID-AREA
               MOVE    "PTHKN"         TO  CHK-PTID-SYU
               MOVE    PTHKN-PTID      TO  CHK-PTID
               PERFORM 20029-PTID-CHECK-SEC
               IF      CHK-PTID-FLG    =   ZERO
                   ADD     1               TO  WRK-PTHKNINF-MAX
                   MOVE    PTHKNINF-REC    TO  WRK-PTHKNINF-REC
                                                   (WRK-PTHKNINF-MAX)
               END-IF                                    
      *    
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key12"         TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200211-PTHKNINF-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認患者ＩＤ取得（照会番号の設定がある場合）
      *     医療扶助以外
      *****************************************************************
       20022-PTID-02-SEC  SECTION.
      * 
      *    保険情報から患者保険情報を検索
           MOVE    ZERO            TO  PTHKNINF-OK
      *     
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                   TO  PTHKN-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-KIGO (IDX)
                                   TO  PTHKN-KIGO
           MOVE    WRK-QUAREQ2-RES-NUM  (IDX) 
                                   TO  PTHKN-NUM
           MOVE    SMCNDATE-YMD    TO  PTHKN-TEKSTYMD
                                       PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key12"         TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           PERFORM         UNTIL   FLG-PTHKNINF    =   1
                           OR      PTHKNINF-OK     =   1
      *        テスト患者チェック処理
               INITIALIZE                  CHK-PTID-AREA
               MOVE    PTHKN-PTID      TO  CHK-PTID
               MOVE    "PTHKN"         TO  CHK-PTID-SYU
               PERFORM 20029-PTID-CHECK-SEC
               IF      CHK-PTID-FLG    =   ZERO
                   IF      QUAREQ2-RES-EDABAN   (IDX)
                                           =   PTHKN-EDABAN
                       MOVE    1               TO  PTHKNINF-OK
                   ELSE    
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key12"         TO  MCP-PATHNAME
                       PERFORM 900-PTHKNINF-READ-N-SEC
                   END-IF                      
               ELSE    
                   MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                   MOVE    "key12"         TO  MCP-PATHNAME
                   PERFORM 900-PTHKNINF-READ-N-SEC
               END-IF                      
           END-PERFORM
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key12"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      PTHKNINF-OK     =   1
               MOVE    PTHKN-PTID      TO  WRK-QUAREQ2-PTID (IDX)
           ELSE
      *        患者が存在しない場合         
      *        照会番号から患者ＩＤ取得
               INITIALIZE                  ORCSPTNUMAREA
               MOVE    QUAREQ2-SHOKAI-NUM (IDX)
                                      TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"    USING   ORCSPTNUMAREA
                                              SPA-AREA
               IF      SPTNUM-RC       =   00
                   MOVE    SPTNUM-PTID     TO  WRK-QUAREQ2-PTID (IDX)
               ELSE
                   DISPLAY "SHOKAI-NUM ERR " QUAREQ2-SHOKAI-NUM (IDX)
               END-IF
           END-IF
      *
           .
       20022-PTID-02-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認登録、更新処理（照会番号の設定がある場合）
      *     医療扶助以外
      *****************************************************************
       20022-SYORI-02-SEC  SECTION.
      *
           MOVE    SPACE           TO  QUARES1-QUARES1
           INITIALIZE                  ONSKAKU-REC
      *
      *    患者番号取得
           IF      WRK-PTID        >   ZERO
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    WRK-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                SPA-AREA
               IF      SPTID-RC        =   00
                   MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
               ELSE
                   DISPLAY "PTNUM NOT FOUND " WRK-PTID
               END-IF
           END-IF
      *    UIDコード取得処理
           PERFORM 200211-UID-GET-SEC
      *    保険証の資格確認検索処理 
           IF      WRK-PTID        >   ZERO
               PERFORM 200220-HKN-CHECK-SEC
           END-IF
      *??
           display "HKN-ONSHI-KAKU-IDX=" HKN-ONSHI-KAKU-IDX     
      *
      *    任意のファイル識別子(医療機関固有項目)の設定
           STRING  WRK-PTID        DELIMITED  BY  SIZE
                   ","             DELIMITED  BY  SIZE  
                   C-UID           DELIMITED  BY  SIZE
                                   INTO       WRK-SIKIBETU
           END-STRING
      *
      *    照会番号、薬剤、特定検診の返却処理
           IF      WRK-PTID        >   ZERO
               PERFORM 200222-RES1-HENSYU-SEC
           END-IF
      *    資格確認レコードの編集処理（顔認証）
           PERFORM 200221-ONSHI-KAKU-HENSYU-SEC
      *    資格確認レコードの更新処理（保険証）
           IF      HKN-ONSHI-KAKU-IDX >   ZERO
               PERFORM 200223-ONSHI-KAKU-UPDATE-SEC
           END-IF
      *
           .
       20022-SYORI-02-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険証の資格確認検索処理
      ******************************************************************
       200220-HKN-CHECK-SEC       SECTION.
      * 
      *    保険証による資格確認の退避処理（最新の作成日時）
           PERFORM 2002201-ONSHI-KAKU-TAIHI-SEC
           IF      HKN-ONSHI-KAKU-MAX  =   ZERO 
               DISPLAY "TBL_ONSHI_KAKU NOT FOUND"
               GO  TO  200220-HKN-CHECK-EXT
           END-IF
      *??
           display "HKN-ONSHI-KAKU-MAX=" HKN-ONSHI-KAKU-MAX     
      *    保険証による資格確認テーブル検索（最新）
           IF      HKN-ONSHI-KAKU-MAX  =   1
               IF    ( QUAREQ2-RES-HKNJANUM (IDX)
                                       =   HKN-ONSKAKU-SHO-HKNJANUM(1) )
               AND   ( WRK-QUAREQ2-RES-KIGO (IDX)
                                       =   HKN-ONSKAKU-SHO-KIGO    (1) )
               AND   ( WRK-QUAREQ2-RES-NUM  (IDX) 
                                       =   HKN-ONSKAKU-SHO-NUM     (1) )
                  IF      HKN-ONSKAKU-KAKU-ENDFLG (1)
                                           =   "03"
                       IF      HKN-ONSKAKU-SHO-EDABAN (1)
                                               =   SPACE
                           MOVE    C-UID   TO  HKN-ONSKAKU-AITE-UUID (1)
                           MOVE    HKN-ONSKAKU-TBL-UUID (1)
                                           TO  ONSKAKU-AITE-UUID
      *                    保険証による資格確認レコードの更新
                           MOVE    1       TO  IDY
                                               HKN-ONSHI-KAKU-IDX    
                       ELSE                        
                           IF      QUAREQ2-RES-EDABAN   (IDX)
                                           =   HKN-ONSKAKU-SHO-EDABAN(1)
                               MOVE    C-UID
                                           TO  HKN-ONSKAKU-AITE-UUID (1)
                               MOVE    HKN-ONSKAKU-TBL-UUID (1)
                                           TO  ONSKAKU-AITE-UUID
      **                       MOVE    "02"    TO
      **                                   HKN-ONSKAKU-KAKU-ENDFLG (1)
      *                        保険証による資格確認レコードの更新
                               MOVE    1      TO  IDY
                                               HKN-ONSHI-KAKU-IDX
                           END-IF
                       END-IF
                   END-IF    
               ELSE
                  IF      HKN-ONSKAKU-KAKU-ENDFLG (1)
                                           =   "03"
                       MOVE    C-UID       TO  HKN-ONSKAKU-AITE-UUID (1)
                       MOVE    HKN-ONSKAKU-TBL-UUID (1)
                                           TO  ONSKAKU-AITE-UUID
      **               MOVE    "02"            TO
      **                                   HKN-ONSKAKU-KAKU-ENDFLG (IDY)
      *               保険証による資格確認レコードの更新
                      MOVE    1           TO  IDY
                                               HKN-ONSHI-KAKU-IDX
                   END-IF                                
               END-IF
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       HKN-ONSHI-KAKU-MAX
                   IF    ( QUAREQ2-RES-HKNJANUM (IDX)
                                       =
                                       HKN-ONSKAKU-SHO-HKNJANUM  (IDY) )
                   AND   ( WRK-QUAREQ2-RES-KIGO (IDX)
                                       =   HKN-ONSKAKU-SHO-KIGO  (IDY) )
                   AND   ( WRK-QUAREQ2-RES-NUM  (IDX)
                                       =   HKN-ONSKAKU-SHO-NUM   (IDY) )
                   AND   ( QUAREQ2-RES-EDABAN   (IDX)
                                       =   HKN-ONSKAKU-SHO-EDABAN(IDY) )
                   AND   ( QUAREQ2-RES-BIRTHDAY (IDX)
                                       =
                                        HKN-ONSKAKU-SHO-BIRTHDAY (IDY) )
                      IF      HKN-ONSKAKU-KAKU-ENDFLG (IDY)
                                           =   "03"
                           MOVE    C-UID   TO
                                           HKN-ONSKAKU-AITE-UUID (IDY)
                           MOVE    HKN-ONSKAKU-TBL-UUID (IDY)
                                           TO  ONSKAKU-AITE-UUID
      **                   MOVE    "02"        TO
      **                                   HKN-ONSKAKU-KAKU-ENDFLG (IDY)
      *                    保険証による資格確認レコードの更新
                           MOVE    1           TO  HKN-ONSHI-KAKU-IDX
                           MOVE    HKN-ONSHI-KAKU-MAX
                                           TO  IDY
                       END-IF                        
                   END-IF                
                END-PERFORM         
           END-IF
           .
       200220-HKN-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険証による資格確認の退避処理
      ******************************************************************
       2002201-ONSHI-KAKU-TAIHI-SEC SECTION.
      *
           INITIALIZE                  HKN-ONSHI-KAKU-AREA
      *    
           INITIALIZE                  WRK-ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  WRK-ONSKAKU-HOSPNUM
           MOVE    WRK-PTID        TO  WRK-ONSKAKU-PTID
           MOVE    WRK-ONSKAKU-REC TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           ELSE
               INITIALIZE                  WRK-ONSKAKU-REC
               MOVE    1               TO  FLG-ONSHI-KAKU
           END-IF
      *
           PERFORM         UNTIL   FLG-ONSHI-KAKU      =   1
                           OR      HKN-ONSHI-KAKU-OK   =   1
               IF      HKN-ONSHI-KAKU-MAX  =   ZERO            
                   ADD     1                   TO  HKN-ONSHI-KAKU-MAX
                   MOVE    WRK-ONSKAKU-REC     TO  HKN-ONSHI-KAKU-REC
                                                   (HKN-ONSHI-KAKU-MAX)
               ELSE                                    
                   IF    ( HKN-ONSKAKU-KAKU-KAKUNINYMD (1)
                                           =   ONSKAKU-KAKU-KAKUNINYMD )
                   AND   ( HKN-ONSKAKU-KAKU-KAKUNINHMS (1)
                                           =   ONSKAKU-KAKU-KAKUNINHMS )
                       ADD     1               TO  HKN-ONSHI-KAKU-MAX
                       MOVE    WRK-ONSKAKU-REC  TO  HKN-ONSHI-KAKU-REC
                                                   (HKN-ONSHI-KAKU-MAX)
                   ELSE
                       MOVE    1               TO  HKN-ONSHI-KAKU-OK
                   END-IF
               END-IF
      *             
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           END-PERFORM 
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2002201-ONSHI-KAKU-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認登録、更新処理
      *     医療扶助
      *****************************************************************
       20023-PTID-03-SEC  SECTION.
      *
      *    公費情報から患者公費険情報を検索
           PERFORM 200231-PTKOHINF-TAIHI-SEC
      *
           IF      WRK-PTKOHINF-MAX    =   1
               MOVE    WRK-PTKOH-PTID (1)  TO
                                           WRK-QUAREQ2-PTID (IDX)
           ELSE
      *        患者が存在しない場合         
      *        照会番号が設定されているとき患者ＩＤ取得
               IF      QUAREQ2-SHOKAI-NUM (IDX)
                                       NOT =   SPACE
                   INITIALIZE                  ORCSPTNUMAREA
                   MOVE    QUAREQ2-SHOKAI-NUM (IDX)
                                          TO  SPTNUM-PTNUM
                   CALL    "ORCSPTNUM"    USING   ORCSPTNUMAREA
                                                  SPA-AREA
                   IF      SPTNUM-RC       =   00
                       MOVE    SPTNUM-PTID     TO
                                               WRK-QUAREQ2-PTID (IDX)
                   ELSE
                       DISPLAY "SHOKAI-NUM ERR "
                                           QUAREQ2-SHOKAI-NUM (IDX)
                   END-IF
               END-IF    
           END-IF
      *
           .
       20023-PTID-03-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認登録、更新処理
      *     医療扶助
      *****************************************************************
       20023-SYORI-03-SEC  SECTION.
      * 
           INITIALIZE                  ONSKAKU-REC
      *
           IF      WRK-PTID        =   ZERO
               MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                       TO  QUARES1-PATIENT-ID
           ELSE
      *        患者番号取得
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    WRK-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                               SPA-AREA
               IF      SPTID-RC        =   00
                   MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
               END-IF
           END-IF
      *    UIDコード取得処理
           PERFORM 200211-UID-GET-SEC
      *    任意のファイル識別子(医療機関固有項目)の設定
           STRING  WRK-PTID        DELIMITED  BY  SIZE
                   ","             DELIMITED  BY  SIZE  
                   C-UID           DELIMITED  BY  SIZE
                                   INTO       WRK-SIKIBETU
           END-STRING
      *
      *    医療扶助単独の時、照会番号、薬剤、特定検診の返却処理
           IF      WRK-PTID        >   ZERO
               IF      WRK-QUAREQ2-RESULT-HKN
                                       =   ZERO
                   PERFORM 200222-RES1-HENSYU-SEC
               ELSE    
                   IF      QUAREQ2-SIKAKU-YUKO
                                       =   "1" OR  "2" OR  "3"
                       IF   (  QUAREQ2-SHOKAI-NUM (IDX)
                                           =   SPACE                   )
      *r07.02
                       OR
                            (( WRK-PATIENT-ID  NOT =   SPACE          )
                          AND
                             ( QUAREQ2-SHOKAI-NUM (IDX)
                                               NOT =   WRK-PATIENT-ID ))
                           MOVE    "01"    TO  ONSKAKU-SHOU-ENDFLG
                       END-IF    
                       IF      QUAREQ2-YAKUZAI-DOUIFLG (IDX)
                                           =   "1"
                           MOVE    "01"    TO  ONSKAKU-YAKUZAI-ENDFLG
                       END-IF    
                       IF      QUAREQ2-KENSIN-DOUIFLG (IDX)
                                           =   "1"
                           MOVE    "01"    TO  ONSKAKU-KENSIN-ENDFLG
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    
      *    資格確認レコードの編集処理（顔認証）
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    C-UID           TO  ONSKAKU-TBL-UUID
           MOVE    QUAREQ2-FUJYO-UUID
                                   TO  ONSKAKU-FUJYO-UUID
           IF      WRK-QUAREQ2-RESULT-HKN
                                   >   ZERO
               MOVE    KOH-ONSKAKU-TBL-UUID
                                       TO  ONSKAKU-KOUHI-UUID
           END-IF
           MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
           MOVE    ZERO            TO  ONSKAKU-SHOU-RENUMBER
           MOVE    ZERO            TO  ONSKAKU-YAKUZAI-RENUMBER
           MOVE    ZERO            TO  ONSKAKU-KENSIN-RENUMBER
           MOVE    QUAREQ2-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
           IF      WRK-PTID        >   0
               MOVE    WRK-PTID         TO  ONSKAKU-PTID
               MOVE    "1"              TO  ONSKAKU-SET-PTID
           END-IF    
           MOVE    QUAREQ2-FILE-SIKIBETU
                                   TO  ONSKAKU-FILE-SIKIBETU
           MOVE    QUAREQ2-SHORI-TIME
                                   TO  ONSKAKU-SHORI-TIME 
           MOVE    QUAREQ2-KAKUNINYMD
                                   TO  ONSKAKU-KAKUNINYMD
           MOVE    QUAREQ2-HOSPCD  TO  ONSKAKU-HOSPCD
      *    訪問診療のとき
           IF      QUAREQ2-SRYKBN  =   "2" OR  "3"
               MOVE    SPACE           TO  ONSKAKU-SHOKAI-KBN
           ELSE    
               MOVE    "1"             TO  ONSKAKU-SHOKAI-KBN
           END-IF    
           MOVE    QUAREQ2-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
           MOVE    QUAREQ2-ERR-CODE
                                   TO  ONSKAKU-ERR-CODE
           MOVE    QUAREQ2-ERR-MSG TO  ONSKAKU-ERR-MSG
           IF    (  ONSKAKU-ERR-CODE 
                                   =   SPACE )
           AND   ( WRK-QUAREQ2-ERR-CODE (IDX)
                               NOT =   SPACE )
               MOVE    WRK-QUAREQ2-ERR-CODE (IDX)
                                       TO  WRK-ERR-CODE
               MOVE    WRK-QUAREQ2-ERR-PTID (IDX)
                                       TO  WRK-ERR-PTID
           END-IF 
           PERFORM 2002211-ONSKAKU-ERR-HENSYU-SEC
           MOVE    "1"             TO  ONSKAKU-CODE-TYPE
      *                             
           MOVE    QUAREQ2-SHO-HKNJANUM
                                   TO  ONSKAKU-SHO-HKNJANUM
           MOVE    WRK-QUAREQ2-SHO-NUM
                                   TO  ONSKAKU-SHO-NUM
           MOVE    QUAREQ2-SHO-BIRTHDAY
                                   TO  ONSKAKU-SHO-BIRTHDAY
           MOVE    QUAREQ2-SHO-DOUIFLG
                                   TO  ONSKAKU-SHO-DOUIFLG
           MOVE    QUAREQ2-SHO-SHOHO-KEITAI
                                   TO  ONSKAKU-SHO-SHOHO-KEITAI
           MOVE    QUAREQ2-RESULT  TO  ONSKAKU-RESULT
           MOVE    QUAREQ2-RESULT-CODE
                                   TO  ONSKAKU-RESULT-CODE
           MOVE    QUAREQ2-RESULT-MSG
                                   TO  ONSKAKU-RESULT-MSG
           MOVE    QUAREQ2-SIKAKU-YUKO
                                   TO  ONSKAKU-SIKAKU-YUKO
      *
           MOVE    QUAREQ2-CARD-CLASS   (IDX) 
                                   TO  ONSKAKU-CARD-CLASS 
           MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                   TO  ONSKAKU-RES-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                   TO  ONSKAKU-RES-NUM 
           MOVE    WRK-QUAREQ2-RES-NAME (IDX)
                                   TO  ONSKAKU-RES-NAME 
           MOVE    WRK-QUAREQ2-RES-NAME-ETC (IDX)
                                   TO  ONSKAKU-RES-NAME-ETC 
           MOVE    WRK-QUAREQ2-RES-KANANAME (IDX)
                                   TO  ONSKAKU-RES-KANANAME 
           MOVE    WRK-QUAREQ2-RES-KANANAME-ETC (IDX)
                                   TO  ONSKAKU-RES-KANANAME-ETC
           MOVE    QUAREQ2-RES-SEX1 (IDX)
                                   TO  ONSKAKU-RES-SEX1 
           MOVE    QUAREQ2-RES-SEX2 (IDX)
                                   TO  ONSKAKU-RES-SEX2 
           MOVE    QUAREQ2-RES-BIRTHDAY (IDX)
                                   TO  ONSKAKU-RES-BIRTHDAY 
           MOVE    WRK-QUAREQ2-RES-ADRS (IDX)
                                   TO  ONSKAKU-RES-ADRS 
           MOVE    QUAREQ2-RES-POST (IDX)
                                   TO  ONSKAKU-RES-POST 
           MOVE    QUAREQ2-JOIN-SKKGETYMD (IDX)
                                  TO  ONSKAKU-KOH-TEKSTYMD (1)
           MOVE    QUAREQ2-JOIN-SKKLOSSYMD (IDX)
                                  TO  ONSKAKU-KOH-TEKEDYMD (1)
           MOVE    QUAREQ2-LOSS-JIYU  (IDX)
                                   TO  ONSKAKU-LOSS-JIYU 
           MOVE    WRK-QUAREQ2-HKNJANUM-NAME (IDX)
                                   TO  ONSKAKU-HKNJANUM-NAME 
           MOVE    QUAREQ2-KENSIN-DOUIFLG (IDX)
                                   TO  ONSKAKU-KENSIN-DOUIFLG 
           MOVE    QUAREQ2-KENSIN-TIME (IDX)
                                   TO  ONSKAKU-KENSIN-TIME 
           MOVE    QUAREQ2-KENSIN-KIGENYMD (IDX)
                                   TO  ONSKAKU-KENSIN-KIGENYMD 
           MOVE    QUAREQ2-YAKUZAI-DOUIFLG (IDX)
                                   TO  ONSKAKU-YAKUZAI-DOUIFLG 
           MOVE    QUAREQ2-YAKUZAI-TIME (IDX)
                                   TO  ONSKAKU-YAKUZAI-TIME 
           MOVE    QUAREQ2-YAKUZAI-KIGEN (IDX)
                                   TO  ONSKAKU-YAKUZAI-KIGEN 
           MOVE    QUAREQ2-SIKIBETU (IDX)
                                   TO  ONSKAKU-SIKIBETU 
           MOVE    QUAREQ2-SHINRYO-DOUIFLG (IDX)
                                   TO  ONSKAKU-SHINRYO-DOUIFLG
           MOVE    QUAREQ2-SHINRYO-TIME    (IDX)
                                   TO  ONSKAKU-SHINRYO-TIME
           MOVE    QUAREQ2-SHINRYO-KIGEN   (IDX)
                                   TO  ONSKAKU-SHINRYO-KIGEN
           MOVE    QUAREQ2-OPE-DOUIFLG (IDX)
                                   TO  ONSKAKU-OPE-DOUIFLG
           MOVE    QUAREQ2-OPE-TIME    (IDX)
                                   TO  ONSKAKU-OPE-TIME
           MOVE    QUAREQ2-OPE-KIGEN   (IDX)
                                   TO  ONSKAKU-OPE-KIGEN
      *    2025/01                             
      *    訪問診療のとき
           IF      QUAREQ2-SRYKBN  =   "2" OR  "3"
               CONTINUE
           ELSE    
               MOVE    QUAREQ2-SHOBYO-DOUIFLG      (IDX)
                                       TO  ONSKAKU-SHOBYO-DOUIFLG 
               MOVE    QUAREQ2-SHOBYO-TIME         (IDX)
                                       TO  ONSKAKU-SHOBYO-TIME 
               MOVE    QUAREQ2-SHOBYO-KIGEN        (IDX)
                                       TO  ONSKAKU-SHOBYO-KIGEN
               MOVE    QUAREQ2-KANSENSYO-DOUIFLG   (IDX)
                                       TO  ONSKAKU-KANSENSYO-DOUIFLG 
               MOVE    QUAREQ2-KANSENSYO-TIME      (IDX)
                                       TO  ONSKAKU-KANSENSYO-TIME 
               MOVE    QUAREQ2-KANSENSYO-KIGEN     (IDX)
                                       TO  ONSKAKU-KANSENSYO-KIGEN 
               MOVE    QUAREQ2-ETC-ALLERGY-DOUIFLG (IDX)
                                       TO  ONSKAKU-ETC-ALLERGY-DOUIFLG
               MOVE    QUAREQ2-ETC-ALLERGY-TIME    (IDX)
                                       TO  ONSKAKU-ETC-ALLERGY-TIME
               MOVE    QUAREQ2-ETC-ALLERGY-KIGEN   (IDX)
                                       TO  ONSKAKU-ETC-ALLERGY-KIGEN
               MOVE    QUAREQ2-YKZ-ALLERGY-DOUIFLG (IDX)
                                       TO  ONSKAKU-YKZ-ALLERGY-DOUIFLG
               MOVE    QUAREQ2-YKZ-ALLERGY-TIME    (IDX)
                                       TO  ONSKAKU-YKZ-ALLERGY-TIME
               MOVE    QUAREQ2-YKZ-ALLERGY-KIGEN   (IDX)
                                       TO  ONSKAKU-YKZ-ALLERGY-KIGEN
               MOVE    QUAREQ2-KENSA-DOUIFLG       (IDX)
                                       TO  ONSKAKU-KENSA-DOUIFLG
               MOVE    QUAREQ2-KENSA-TIME          (IDX)
                                       TO  ONSKAKU-KENSA-TIME
               MOVE    QUAREQ2-KENSA-KIGEN         (IDX)
                                       TO  ONSKAKU-KENSA-KIGEN
               MOVE    QUAREQ2-SHOHO-DOUIFLG       (IDX)
                                       TO  ONSKAKU-SHOHO-DOUIFLG
               MOVE    QUAREQ2-SHOHO-TIME          (IDX)
                                       TO  ONSKAKU-SHOHO-TIME
               MOVE    QUAREQ2-SHOHO-KIGEN         (IDX)
                                       TO  ONSKAKU-SHOHO-KIGEN
           END-IF                                 
      *    2024/12
           MOVE    QUAREQ2-ONSHI-UPYMD   (IDX)
                                   TO  ONSKAKU-ONSHI-UPYMD
           MOVE    QUAREQ2-SIKAKU-KAKUNIN-KEY   (IDX)
                                   TO  ONSKAKU-SIKAKU-KAKUNIN-KEY  
           MOVE    QUAREQ2-SIKAKU-UPDATE-FLG   (IDX)
                                   TO  ONSKAKU-SIKAKU-UPDATE-FLG
           IF    ( WRK-PATIENT-ID  NOT =   SPACE )
               IF    ( QUAREQ2-SHOKAI-NUM (IDX)
                                       NOT =   SPACE          )
               AND   ( QUAREQ2-SHOKAI-NUM (IDX)
                                       NOT =   WRK-PATIENT-ID )
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
               ELSE        
                   MOVE    QUAREQ2-SHOKAI-NUM (IDX)
                                           TO  ONSKAKU-SHOKAI-NUM
               END-IF
           ELSE
               MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
           END-IF
      *    訪問診療のとき
           IF      QUAREQ2-SRYKBN  =   "2" OR  "3"
               MOVE    QUAREQ2-SRYKBN  TO  ONSKAKU-ONSHINRYO-KBN
           END-IF
      *                             
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
      *
      *    医療扶助単独のとき
           IF      WRK-QUAREQ2-RESULT-HKN
                                   =   ZERO
               MOVE    ONSKAKU-REC     TO  KOH-ONSKAKU-REC
      *        対象の公費の資格確認レコードの更新処理
               PERFORM 200224-KOH-ONSKAKU-UPDATE-SEC
      *     
      *        資格確認レコードの追加処理（顔認証）
               IF      WRK-ERRCD       =   SPACE
                   IF      WRK-KOH-SHORI-TIME
                                       NOT =   SPACE
      *                資格確認レコードの更新処理（顔認証の公費ＵＩＤ）
                       MOVE    KOH-ONSKAKU-FUJYO-UUID
                                           TO  KOH-ONSKAKU-KOUHI-UUID
                   END-IF
               END-IF
               MOVE    KOH-ONSKAKU-REC TO  ONSKAKU-REC
           END-IF
      *
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC      NOT =   ZERO 
               MOVE   "E10"             TO  WRK-ERRCD
               DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC
           ELSE
      *        処方管理レコード作成処理
               IF      WRK-ERRCD       =   SPACE
                   IF      WRK-QUAREQ2-RESULT-HKN
                                           =   ZERO
                       PERFORM 200225-SHOHO-KANRI-INSERT-SEC
                   END-IF
               END-IF
           END-IF
           .        
       20023-SYORI-03-EXT.
           EXIT.
      *
      *****************************************************************
      *     顔認証資格確認登録、更新処理
      *     医療扶助以外（訪問診療）
      *****************************************************************
       20024-SYORI-HOU-01-SEC  SECTION.
      *
           MOVE    SPACE           TO  QUARES1-QUARES1
           INITIALIZE                  ONSKAKU-REC
      * 
      *    任意のファイル識別子(医療機関固有項目)の設定
           STRING  WRK-PTID        DELIMITED  BY  SIZE
                   ","             DELIMITED  BY  SIZE  
                   WRK-UUID        DELIMITED  BY  SIZE
                                   INTO       WRK-SIKIBETU
           END-STRING
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    WRK-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        =   00
               MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
            END-IF
      *    資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    WRK-UUID        TO  ONSKAKU-TBL-UUID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           PERFORM 900-ONSHI-KAKU-READ-SEC
           IF      FLG-ONSHI-KAKU  =   1
               MOVE    "E07"           TO  WRK-ERRCD
               GO  TO  20024-SYORI-HOU-01-EXT
           END-IF
      * 
      *    照会番号、薬剤、特定検診の返却処理
           PERFORM 200222-RES1-HENSYU-SEC
      *    資格確認レコードの編集処理（顔認証）
           MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
           MOVE    ZERO            TO  ONSKAKU-SHOU-RENUMBER
           MOVE    ZERO            TO  ONSKAKU-YAKUZAI-RENUMBER
           MOVE    ZERO            TO  ONSKAKU-KENSIN-RENUMBER
           MOVE    QUAREQ2-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
           MOVE    QUAREQ2-FILE-SIKIBETU
                                   TO  ONSKAKU-FILE-SIKIBETU
           MOVE    QUAREQ2-SHORI-TIME
                                   TO  ONSKAKU-SHORI-TIME 
           MOVE    QUAREQ2-KAKUNINYMD
                                   TO  ONSKAKU-KAKUNINYMD
           MOVE    QUAREQ2-HOSPCD  TO  ONSKAKU-HOSPCD
           MOVE    SPACE           TO  ONSKAKU-SHOKAI-KBN  
           MOVE    QUAREQ2-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
           MOVE    QUAREQ2-ERR-CODE
                                   TO  ONSKAKU-ERR-CODE
           MOVE    QUAREQ2-ERR-MSG TO  ONSKAKU-ERR-MSG
           IF    (  ONSKAKU-ERR-CODE 
                                   =   SPACE )
           AND   ( WRK-QUAREQ2-ERR-CODE (IDX)
                               NOT =   SPACE )
               MOVE    WRK-QUAREQ2-ERR-CODE (IDX)
                                       TO  WRK-ERR-CODE
               MOVE    WRK-QUAREQ2-ERR-PTID (IDX)
                                       TO  WRK-ERR-PTID
           END-IF 
           PERFORM 2002211-ONSKAKU-ERR-HENSYU-SEC
           MOVE    "1"             TO  ONSKAKU-CODE-TYPE
      *                             
           MOVE    QUAREQ2-SHO-HKNJANUM
                                   TO  ONSKAKU-SHO-HKNJANUM
           MOVE    WRK-QUAREQ2-SHO-KIGO
                                   TO  ONSKAKU-SHO-KIGO
           MOVE    WRK-QUAREQ2-SHO-NUM
                                   TO  ONSKAKU-SHO-NUM
           MOVE    QUAREQ2-SHO-EDABAN
                                   TO  ONSKAKU-SHO-EDABAN
           MOVE    QUAREQ2-SHO-BIRTHDAY
                                   TO  ONSKAKU-SHO-BIRTHDAY
           MOVE    QUAREQ2-SHO-DOUIFLG
                                   TO  ONSKAKU-SHO-DOUIFLG
           MOVE    QUAREQ2-SHO-SHOHO-KEITAI
                                   TO  ONSKAKU-SHO-SHOHO-KEITAI
           MOVE    QUAREQ2-RESULT
                                   TO  ONSKAKU-RESULT
           MOVE    QUAREQ2-RESULT-CODE
                                   TO  ONSKAKU-RESULT-CODE
           MOVE    QUAREQ2-RESULT-MSG
                                   TO  ONSKAKU-RESULT-MSG
           MOVE    QUAREQ2-SIKAKU-YUKO
                                   TO  ONSKAKU-SIKAKU-YUKO
      *
           MOVE    QUAREQ2-CARD-CLASS   (IDX) 
                                   TO  ONSKAKU-CARD-CLASS 
           MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                   TO  ONSKAKU-RES-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-KIGO (IDX) 
                                   TO  ONSKAKU-RES-KIGO
           MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                   TO  ONSKAKU-RES-NUM 
           MOVE    QUAREQ2-RES-EDABAN (IDX)
                                   TO  ONSKAKU-RES-EDABAN 
           MOVE    QUAREQ2-RES-HONKZKKBN (IDX)
                                   TO  ONSKAKU-RES-HONKZKKBN 
           MOVE    WRK-QUAREQ2-RES-HIHKNJANAME (IDX)
                                   TO  ONSKAKU-RES-HIHKNJANAME 
           MOVE    WRK-QUAREQ2-RES-NAME (IDX)
                                   TO  ONSKAKU-RES-NAME 
           MOVE    WRK-QUAREQ2-RES-NAME-ETC (IDX)
                                   TO  ONSKAKU-RES-NAME-ETC 
           MOVE    WRK-QUAREQ2-RES-KANANAME (IDX)
                                   TO  ONSKAKU-RES-KANANAME 
           MOVE    WRK-QUAREQ2-RES-KANANAME-ETC (IDX)
                                   TO  ONSKAKU-RES-KANANAME-ETC
           MOVE    QUAREQ2-RES-SEX1 (IDX)
                                   TO  ONSKAKU-RES-SEX1 
           MOVE    QUAREQ2-RES-SEX2 (IDX)
                                   TO  ONSKAKU-RES-SEX2 
           MOVE    QUAREQ2-RES-BIRTHDAY (IDX)
                                   TO  ONSKAKU-RES-BIRTHDAY 
           MOVE    WRK-QUAREQ2-RES-ADRS (IDX)
                                   TO  ONSKAKU-RES-ADRS 
           MOVE    QUAREQ2-RES-POST (IDX)
                                   TO  ONSKAKU-RES-POST 
           MOVE    QUAREQ2-JOIN-SKKGETYMD (IDX)
                                   TO  ONSKAKU-JOIN-SKKGETYMD
           MOVE    QUAREQ2-SKKGETYMD (IDX)
                                   TO  ONSKAKU-SKKGETYMD 
           MOVE    QUAREQ2-TEKSTYMD (IDX)
                                   TO  ONSKAKU-TEKSTYMD 
           MOVE    QUAREQ2-TEKEDYMD (IDX)
                                   TO  ONSKAKU-TEKEDYMD 
           MOVE    QUAREQ2-FTNRATE (IDX)
                                   TO  ONSKAKU-FTNRATE 
           MOVE    QUAREQ2-MISYUGAKU (IDX)
                                   TO  ONSKAKU-MISYUGAKU 
           MOVE    QUAREQ2-LOSS-JIYU  (IDX)
                                   TO  ONSKAKU-LOSS-JIYU 
           MOVE    WRK-QUAREQ2-HKNJANUM-NAME (IDX)
                                   TO  ONSKAKU-HKNJANUM-NAME 
           MOVE    QUAREQ2-ELDER-SKKGETYMD (IDX)
                                   TO  ONSKAKU-ELDER-SKKGETYMD 
           MOVE    QUAREQ2-ELDER-TEKSTYMD (IDX)
                                   TO  ONSKAKU-ELDER-TEKSTYMD 
           MOVE    QUAREQ2-ELDER-TEKEDYMD (IDX)
                                   TO  ONSKAKU-ELDER-TEKEDYMD 
           MOVE    QUAREQ2-ELDER-FTNRATE (IDX)
                                   TO  ONSKAKU-ELDER-FTNRATE 
           MOVE    QUAREQ2-GENDO-DOUIFLG (IDX)
                                   TO  ONSKAKU-GENDO-DOUIFLG 
           MOVE    QUAREQ2-GENDO-TIME (IDX)
                                   TO  ONSKAKU-GENDO-TIME 
           MOVE    QUAREQ2-GENDO-SHOKBN (IDX)
                                   TO  ONSKAKU-GENDO-SHOKBN 
           MOVE    QUAREQ2-GENDO-TEKKBN (IDX)
                                   TO  ONSKAKU-GENDO-TEKKBN 
           MOVE    QUAREQ2-GENDO-SKKGETYMD (IDX)
                                   TO  ONSKAKU-GENDO-SKKGETYMD 
           MOVE    QUAREQ2-GENDO-TEKSTYMD (IDX)
                                   TO  ONSKAKU-GENDO-TEKSTYMD 
           MOVE    QUAREQ2-GENDO-TEKEDYMD (IDX)
                                   TO  ONSKAKU-GENDO-TEKEDYMD 
           MOVE    QUAREQ2-GENDO-SKJGNSTYMD (IDX)
                                   TO  ONSKAKU-GENDO-SKJGNSTYMD 
           MOVE    QUAREQ2-SIKKAN-DOUIFLG (IDX)
                                   TO  ONSKAKU-SIKKAN-DOUIFLG
           MOVE    QUAREQ2-SIKKAN-TIME (IDX)
                                   TO  ONSKAKU-SIKKAN-TIME
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       3                         
               MOVE    QUAREQ2-SIKKAN-SIPKBN     (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-SIPKBN    (IDZ)
               MOVE    QUAREQ2-SIKKAN-SKJGNSTYMD (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-SKJGNSTYMD(IDZ)
               MOVE    QUAREQ2-SIKKAN-TEKSTYMD   (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-TEKSTYMD  (IDZ)
               MOVE    QUAREQ2-SIKKAN-TEKEDYMD   (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-TEKEDYMD  (IDZ)
               MOVE    QUAREQ2-SIKKAN-FTNMONEY   (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-FTNMONEY  (IDZ)
           END-PERFORM                         
           MOVE    QUAREQ2-KENSIN-DOUIFLG (IDX)
                                   TO  ONSKAKU-KENSIN-DOUIFLG 
           MOVE    QUAREQ2-KENSIN-TIME (IDX)
                                   TO  ONSKAKU-KENSIN-TIME 
           MOVE    QUAREQ2-KENSIN-KIGENYMD (IDX)
                                   TO  ONSKAKU-KENSIN-KIGENYMD 
           MOVE    QUAREQ2-YAKUZAI-DOUIFLG (IDX)
                                   TO  ONSKAKU-YAKUZAI-DOUIFLG 
           MOVE    QUAREQ2-YAKUZAI-TIME (IDX)
                                   TO  ONSKAKU-YAKUZAI-TIME 
           MOVE    QUAREQ2-YAKUZAI-KIGEN (IDX)
                                   TO  ONSKAKU-YAKUZAI-KIGEN 
           MOVE    QUAREQ2-SIKIBETU (IDX)
                                   TO  ONSKAKU-SIKIBETU 
           MOVE    QUAREQ2-SHINRYO-DOUIFLG (IDX)
                                   TO  ONSKAKU-SHINRYO-DOUIFLG
           MOVE    QUAREQ2-SHINRYO-TIME    (IDX)
                                   TO  ONSKAKU-SHINRYO-TIME
           MOVE    QUAREQ2-SHINRYO-KIGEN   (IDX)
                                   TO  ONSKAKU-SHINRYO-KIGEN
           MOVE    QUAREQ2-OPE-DOUIFLG (IDX)
                                   TO  ONSKAKU-OPE-DOUIFLG
           MOVE    QUAREQ2-OPE-TIME    (IDX)
                                   TO  ONSKAKU-OPE-TIME
           MOVE    QUAREQ2-OPE-KIGEN   (IDX)
                                   TO  ONSKAKU-OPE-KIGEN
      *    2024/12
           MOVE    QUAREQ2-ONSHI-UPYMD   (IDX)
                                   TO  ONSKAKU-ONSHI-UPYMD
           MOVE    QUAREQ2-SIKAKU-KAKUNIN-KEY   (IDX)
                                   TO  ONSKAKU-SIKAKU-KAKUNIN-KEY  
           MOVE    QUAREQ2-SIKAKU-UPDATE-FLG   (IDX)
                                   TO  ONSKAKU-SIKAKU-UPDATE-FLG
           IF    ( WRK-PATIENT-ID  NOT =   SPACE )
               IF    ( QUAREQ2-SHOKAI-NUM (IDX)
                                       NOT =   SPACE          )
               AND   ( QUAREQ2-SHOKAI-NUM (IDX)
                                       NOT =   WRK-PATIENT-ID )
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
               ELSE        
                   MOVE    QUAREQ2-SHOKAI-NUM (IDX)
                                           TO  ONSKAKU-SHOKAI-NUM
               END-IF
           ELSE
               MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
           END-IF
           MOVE    QUAREQ2-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
           MOVE    QUAREQ2-SRYKBN  TO  ONSKAKU-ONSHINRYO-KBN
      *                             
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-UPYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-UPHMS
      *
           MOVE    ONSKAKU-REC     TO  KOH-ONSKAKU-REC
      *
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               display "tbl_onshi_kaku update error" 
               MOVE    "E14"           TO  WRK-ERRCD
           END-IF
      *        
      *    処方管理レコード作成処理
           IF      WRK-ERRCD       =   SPACE
               PERFORM 200225-SHOHO-KANRI-INSERT-SEC
           END-IF
      *
           .        
       20024-SYORI-HOU-01-EXT.
           EXIT.
      *
      ******************************************************************
      *    公費情報から患者公費情報を検索
      ******************************************************************
       200231-PTKOHINF-TAIHI-SEC    SECTION.
      *
           INITIALIZE                  WRK-PTKOHINF-AREA
      *
      *    資格確認結果の保険情報が空白のとき、公費情報の検索は行わない
           IF    ( QUAREQ2-RES-HKNJANUM (IDX)
                                   =   SPACE )
           AND   ( WRK-QUAREQ2-RES-NUM  (IDX)
                                   =   SPACE )
               GO  TO  200231-PTKOHINF-TAIHI-EXT
           END-IF     
      *         
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                   TO  PTKOH-FTNJANUM
           MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                   TO  PTKOH-JKYSNUM
           MOVE    SMCNDATE-YMD    TO  PTKOH-TEKSTYMD
                                       PTKOH-TEKEDYMD
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key17"         TO  MCP-PATHNAME
               PERFORM 900-PTKOHINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           PERFORM         UNTIL   FLG-PTKOHINF        =   1
                           OR      WRK-PTKOHINF-MAX    >   1
      *        テスト患者チェック処理
               INITIALIZE                  CHK-PTID-AREA
               MOVE    PTKOH-PTID      TO  CHK-PTID
               MOVE    "PTKOH"         TO  CHK-PTID-SYU
               PERFORM 20029-PTID-CHECK-SEC
               IF      CHK-PTID-FLG    =   ZERO
                   ADD     1               TO  WRK-PTKOHINF-MAX
                   MOVE    PTKOH-PTID      TO  WRK-PTKOH-PTID
                                                   (WRK-PTKOHINF-MAX)
               END-IF 
      *    
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key17"         TO  MCP-PATHNAME
               PERFORM 900-PTKOHINF-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      WRK-PTKOHINF-MAX    >   ZERO
               GO  TO  200231-PTKOHINF-TAIHI-EXT
           END-IF
      *         
      *    患者公費情報から検索できなかったとき、月代り受給者番号を検索
           INITIALIZE                  MONTHLYNUM-REC
           MOVE    SPA-HOSPNUM     TO  MONTHLYNUM-HOSPNUM
           MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                   TO  MONTHLYNUM-XXFTNJANUM
           MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                   TO  MONTHLYNUM-JKYSNUM
           MOVE    SMCNDATE-YMD (1:6)
                                   TO  MONTHLYNUM-SRYYM
           MOVE    SMCNDATE-YMD    TO  MONTHLYNUM-XXTEKSTYMD
                                       MONTHLYNUM-XXTEKEDYMD
           MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
           MOVE    "tbl_monthlynum"
                                   TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_monthlynum"  TO  MCP-TABLE
               MOVE    "key6"         TO  MCP-PATHNAME
               PERFORM 900-MONTHLYNUM-READ-N-SEC
           ELSE
               INITIALIZE                  MONTHLYNUM-REC
               MOVE    1               TO  FLG-MONTHLYNUM
           END-IF
      *
           PERFORM         UNTIL   FLG-MONTHLYNUM      =   1
                           OR      WRK-PTKOHINF-MAX    >   1
               ADD     1               TO  WRK-PTKOHINF-MAX
               MOVE    MONTHLYNUM-PTID TO  WRK-PTKOH-PTID
                                               (WRK-PTKOHINF-MAX)
      *    
               MOVE    "tbl_monthlynum"  TO  MCP-TABLE
               MOVE    "key6"         TO  MCP-PATHNAME
               PERFORM 900-MONTHLYNUM-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_monthlynum"  TO  MCP-TABLE
           MOVE    "key6"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200231-PTKOHINF-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *    テスト患者チェック処理
      ******************************************************************
       20029-PTID-CHECK-SEC            SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    CHK-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       =   ZERO
                IF      PTINF-TSTPTNUMKBN
                                       =   "1"
                    MOVE    1              TO  CHK-PTID-FLG
                    DISPLAY CHK-PTID-SYU
                            " PTINF TSTPTNUMKBN=1 PTID=" CHK-PTID
                END-IF
           ELSE
               MOVE    1              TO  CHK-PTID-FLG
               DISPLAY CHK-PTID-SYU
                       " PTINF NOT FOUND PTID=" CHK-PTID
           END-IF 
           .
       20029-PTID-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *    UIDコード取得処理
      ******************************************************************
       200211-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
           .
       200211-UID-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認レコードの編集処理（顔認証）
      ******************************************************************
       200221-ONSHI-KAKU-HENSYU-SEC SECTION.
      *     
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    C-UID           TO  ONSKAKU-TBL-UUID
           MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
           MOVE    ZERO            TO  ONSKAKU-SHOU-RENUMBER
           MOVE    ZERO            TO  ONSKAKU-YAKUZAI-RENUMBER
           MOVE    ZERO            TO  ONSKAKU-KENSIN-RENUMBER
           MOVE    QUAREQ2-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
           IF      WRK-PTID        >   0
               MOVE    WRK-PTID         TO  ONSKAKU-PTID
               MOVE    "1"              TO  ONSKAKU-SET-PTID
           END-IF    
           MOVE    QUAREQ2-FILE-SIKIBETU
                                   TO  ONSKAKU-FILE-SIKIBETU
           MOVE    QUAREQ2-SHORI-TIME
                                   TO  ONSKAKU-SHORI-TIME 
           MOVE    QUAREQ2-KAKUNINYMD
                                   TO  ONSKAKU-KAKUNINYMD
           MOVE    QUAREQ2-HOSPCD  TO  ONSKAKU-HOSPCD
           MOVE    "1"             TO  ONSKAKU-SHOKAI-KBN
           MOVE    QUAREQ2-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
           MOVE    QUAREQ2-ERR-CODE
                                   TO  ONSKAKU-ERR-CODE
           MOVE    QUAREQ2-ERR-MSG TO  ONSKAKU-ERR-MSG
           IF    (  ONSKAKU-ERR-CODE 
                                   =   SPACE )
           AND   ( WRK-QUAREQ2-ERR-CODE (IDX)
                               NOT =   SPACE )
               MOVE    WRK-QUAREQ2-ERR-CODE (IDX)
                                       TO  WRK-ERR-CODE
               MOVE    WRK-QUAREQ2-ERR-PTID (IDX)
                                       TO  WRK-ERR-PTID
           END-IF 
           PERFORM 2002211-ONSKAKU-ERR-HENSYU-SEC
           MOVE    "1"             TO  ONSKAKU-CODE-TYPE
      *                             
           MOVE    QUAREQ2-SHO-HKNJANUM
                                   TO  ONSKAKU-SHO-HKNJANUM
           MOVE    WRK-QUAREQ2-SHO-KIGO
                                   TO  ONSKAKU-SHO-KIGO
           MOVE    WRK-QUAREQ2-SHO-NUM
                                   TO  ONSKAKU-SHO-NUM
           MOVE    QUAREQ2-SHO-EDABAN
                                   TO  ONSKAKU-SHO-EDABAN
           MOVE    QUAREQ2-SHO-BIRTHDAY
                                   TO  ONSKAKU-SHO-BIRTHDAY
           MOVE    QUAREQ2-SHO-DOUIFLG
                                   TO  ONSKAKU-SHO-DOUIFLG
           MOVE    QUAREQ2-SHO-SHOHO-KEITAI
                                   TO  ONSKAKU-SHO-SHOHO-KEITAI
           MOVE    QUAREQ2-RESULT
                                   TO  ONSKAKU-RESULT
           MOVE    QUAREQ2-RESULT-CODE
                                   TO  ONSKAKU-RESULT-CODE
           MOVE    QUAREQ2-RESULT-MSG
                                   TO  ONSKAKU-RESULT-MSG
           MOVE    QUAREQ2-SIKAKU-YUKO
                                   TO  ONSKAKU-SIKAKU-YUKO
      *
           MOVE    QUAREQ2-CARD-CLASS   (IDX) 
                                   TO  ONSKAKU-CARD-CLASS 
           MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                   TO  ONSKAKU-RES-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-KIGO (IDX) 
                                   TO  ONSKAKU-RES-KIGO
           MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                   TO  ONSKAKU-RES-NUM 
           MOVE    QUAREQ2-RES-EDABAN (IDX)
                                   TO  ONSKAKU-RES-EDABAN 
           MOVE    QUAREQ2-RES-HONKZKKBN (IDX)
                                   TO  ONSKAKU-RES-HONKZKKBN 
           MOVE    WRK-QUAREQ2-RES-HIHKNJANAME (IDX)
                                   TO  ONSKAKU-RES-HIHKNJANAME 
           MOVE    WRK-QUAREQ2-RES-NAME (IDX)
                                   TO  ONSKAKU-RES-NAME 
           MOVE    WRK-QUAREQ2-RES-NAME-ETC (IDX)
                                   TO  ONSKAKU-RES-NAME-ETC 
           MOVE    WRK-QUAREQ2-RES-KANANAME (IDX)
                                   TO  ONSKAKU-RES-KANANAME 
           MOVE    WRK-QUAREQ2-RES-KANANAME-ETC (IDX)
                                   TO  ONSKAKU-RES-KANANAME-ETC
           MOVE    QUAREQ2-RES-SEX1 (IDX)
                                   TO  ONSKAKU-RES-SEX1 
           MOVE    QUAREQ2-RES-SEX2 (IDX)
                                   TO  ONSKAKU-RES-SEX2 
           MOVE    QUAREQ2-RES-BIRTHDAY (IDX)
                                   TO  ONSKAKU-RES-BIRTHDAY 
           MOVE    WRK-QUAREQ2-RES-ADRS (IDX)
                                   TO  ONSKAKU-RES-ADRS 
           MOVE    QUAREQ2-RES-POST (IDX)
                                   TO  ONSKAKU-RES-POST 
           MOVE    QUAREQ2-JOIN-SKKGETYMD (IDX)
                                   TO  ONSKAKU-JOIN-SKKGETYMD
           MOVE    QUAREQ2-SKKGETYMD (IDX)
                                   TO  ONSKAKU-SKKGETYMD 
           MOVE    QUAREQ2-TEKSTYMD (IDX)
                                   TO  ONSKAKU-TEKSTYMD 
           MOVE    QUAREQ2-TEKEDYMD (IDX)
                                   TO  ONSKAKU-TEKEDYMD 
           MOVE    QUAREQ2-FTNRATE (IDX)
                                   TO  ONSKAKU-FTNRATE 
           MOVE    QUAREQ2-MISYUGAKU (IDX)
                                   TO  ONSKAKU-MISYUGAKU 
           MOVE    QUAREQ2-LOSS-JIYU  (IDX)
                                   TO  ONSKAKU-LOSS-JIYU 
           MOVE    WRK-QUAREQ2-HKNJANUM-NAME (IDX)
                                   TO  ONSKAKU-HKNJANUM-NAME 
           MOVE    QUAREQ2-ELDER-SKKGETYMD (IDX)
                                   TO  ONSKAKU-ELDER-SKKGETYMD 
           MOVE    QUAREQ2-ELDER-TEKSTYMD (IDX)
                                   TO  ONSKAKU-ELDER-TEKSTYMD 
           MOVE    QUAREQ2-ELDER-TEKEDYMD (IDX)
                                   TO  ONSKAKU-ELDER-TEKEDYMD 
           MOVE    QUAREQ2-ELDER-FTNRATE (IDX)
                                   TO  ONSKAKU-ELDER-FTNRATE 
           MOVE    QUAREQ2-GENDO-DOUIFLG (IDX)
                                   TO  ONSKAKU-GENDO-DOUIFLG 
           MOVE    QUAREQ2-GENDO-TIME (IDX)
                                   TO  ONSKAKU-GENDO-TIME 
           MOVE    QUAREQ2-GENDO-SHOKBN (IDX)
                                   TO  ONSKAKU-GENDO-SHOKBN 
           MOVE    QUAREQ2-GENDO-TEKKBN (IDX)
                                   TO  ONSKAKU-GENDO-TEKKBN 
           MOVE    QUAREQ2-GENDO-SKKGETYMD (IDX)
                                   TO  ONSKAKU-GENDO-SKKGETYMD 
           MOVE    QUAREQ2-GENDO-TEKSTYMD (IDX)
                                   TO  ONSKAKU-GENDO-TEKSTYMD 
           MOVE    QUAREQ2-GENDO-TEKEDYMD (IDX)
                                   TO  ONSKAKU-GENDO-TEKEDYMD 
           MOVE    QUAREQ2-GENDO-SKJGNSTYMD (IDX)
                                   TO  ONSKAKU-GENDO-SKJGNSTYMD 
           MOVE    QUAREQ2-SIKKAN-DOUIFLG (IDX)
                                   TO  ONSKAKU-SIKKAN-DOUIFLG
           MOVE    QUAREQ2-SIKKAN-TIME (IDX)
                                   TO  ONSKAKU-SIKKAN-TIME
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       3                         
               MOVE    QUAREQ2-SIKKAN-SIPKBN     (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-SIPKBN    (IDZ)
               MOVE    QUAREQ2-SIKKAN-SKJGNSTYMD (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-SKJGNSTYMD(IDZ)
               MOVE    QUAREQ2-SIKKAN-TEKSTYMD   (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-TEKSTYMD  (IDZ)
               MOVE    QUAREQ2-SIKKAN-TEKEDYMD   (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-TEKEDYMD  (IDZ)
               MOVE    QUAREQ2-SIKKAN-FTNMONEY   (IDX IDZ)
                                   TO  ONSKAKU-SIKKAN-FTNMONEY  (IDZ)
           END-PERFORM                         
           MOVE    QUAREQ2-KENSIN-DOUIFLG (IDX)
                                   TO  ONSKAKU-KENSIN-DOUIFLG 
           MOVE    QUAREQ2-KENSIN-TIME (IDX)
                                   TO  ONSKAKU-KENSIN-TIME 
           MOVE    QUAREQ2-KENSIN-KIGENYMD (IDX)
                                   TO  ONSKAKU-KENSIN-KIGENYMD 
           MOVE    QUAREQ2-YAKUZAI-DOUIFLG (IDX)
                                   TO  ONSKAKU-YAKUZAI-DOUIFLG 
           MOVE    QUAREQ2-YAKUZAI-TIME (IDX)
                                   TO  ONSKAKU-YAKUZAI-TIME 
           MOVE    QUAREQ2-YAKUZAI-KIGEN (IDX)
                                   TO  ONSKAKU-YAKUZAI-KIGEN 
           MOVE    QUAREQ2-SIKIBETU (IDX)
                                   TO  ONSKAKU-SIKIBETU 
           MOVE    QUAREQ2-SHINRYO-DOUIFLG (IDX)
                                   TO  ONSKAKU-SHINRYO-DOUIFLG
           MOVE    QUAREQ2-SHINRYO-TIME    (IDX)
                                   TO  ONSKAKU-SHINRYO-TIME
           MOVE    QUAREQ2-SHINRYO-KIGEN   (IDX)
                                   TO  ONSKAKU-SHINRYO-KIGEN
           MOVE    QUAREQ2-OPE-DOUIFLG (IDX)
                                   TO  ONSKAKU-OPE-DOUIFLG
           MOVE    QUAREQ2-OPE-TIME    (IDX)
                                   TO  ONSKAKU-OPE-TIME
           MOVE    QUAREQ2-OPE-KIGEN   (IDX)
                                   TO  ONSKAKU-OPE-KIGEN
      *    2025/01
           MOVE    QUAREQ2-SHOBYO-DOUIFLG      (IDX)
                                   TO  ONSKAKU-SHOBYO-DOUIFLG 
           MOVE    QUAREQ2-SHOBYO-TIME         (IDX)
                                   TO  ONSKAKU-SHOBYO-TIME 
           MOVE    QUAREQ2-SHOBYO-KIGEN        (IDX)
                                   TO  ONSKAKU-SHOBYO-KIGEN
           MOVE    QUAREQ2-KANSENSYO-DOUIFLG   (IDX)
                                   TO  ONSKAKU-KANSENSYO-DOUIFLG 
           MOVE    QUAREQ2-KANSENSYO-TIME      (IDX)
                                   TO  ONSKAKU-KANSENSYO-TIME 
           MOVE    QUAREQ2-KANSENSYO-KIGEN     (IDX)
                                   TO  ONSKAKU-KANSENSYO-KIGEN 
           MOVE    QUAREQ2-ETC-ALLERGY-DOUIFLG (IDX)
                                   TO  ONSKAKU-ETC-ALLERGY-DOUIFLG
           MOVE    QUAREQ2-ETC-ALLERGY-TIME    (IDX)
                                   TO  ONSKAKU-ETC-ALLERGY-TIME
           MOVE    QUAREQ2-ETC-ALLERGY-KIGEN   (IDX)
                                   TO  ONSKAKU-ETC-ALLERGY-KIGEN
           MOVE    QUAREQ2-YKZ-ALLERGY-DOUIFLG (IDX)
                                   TO  ONSKAKU-YKZ-ALLERGY-DOUIFLG
           MOVE    QUAREQ2-YKZ-ALLERGY-TIME    (IDX)
                                   TO  ONSKAKU-YKZ-ALLERGY-TIME
           MOVE    QUAREQ2-YKZ-ALLERGY-KIGEN   (IDX)
                                   TO  ONSKAKU-YKZ-ALLERGY-KIGEN
           MOVE    QUAREQ2-KENSA-DOUIFLG       (IDX)
                                   TO  ONSKAKU-KENSA-DOUIFLG
           MOVE    QUAREQ2-KENSA-TIME          (IDX)
                                   TO  ONSKAKU-KENSA-TIME
           MOVE    QUAREQ2-KENSA-KIGEN         (IDX)
                                   TO  ONSKAKU-KENSA-KIGEN
           MOVE    QUAREQ2-SHOHO-DOUIFLG       (IDX)
                                   TO  ONSKAKU-SHOHO-DOUIFLG
           MOVE    QUAREQ2-SHOHO-TIME          (IDX)
                                   TO  ONSKAKU-SHOHO-TIME
           MOVE    QUAREQ2-SHOHO-KIGEN         (IDX)
                                   TO  ONSKAKU-SHOHO-KIGEN
      *****MOVE    QUAREQ2-SHOKAI-NUM (IDX)
      *****                        TO  ONSKAKU-SHOKAI-NUM
           IF    ( WRK-PATIENT-ID  NOT =   SPACE )
               IF    ( QUAREQ2-SHOKAI-NUM (IDX)
                                       NOT =   SPACE          )
               AND   ( QUAREQ2-SHOKAI-NUM (IDX)
                                       NOT =   WRK-PATIENT-ID )
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
               ELSE        
                   MOVE    QUAREQ2-SHOKAI-NUM (IDX)
                                           TO  ONSKAKU-SHOKAI-NUM
               END-IF
           ELSE
               MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
           END-IF
      *    医療扶助と併用のとき
      *****IF      WRK-QUAREQ2-RESULT-MAX
      *****                        >   1
               MOVE    QUAREQ2-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
      *****END-IF
      *    訪問診療のとき
           IF      QUAREQ2-SRYKBN  =   "2" OR  "3"
               MOVE    QUAREQ2-SRYKBN  TO  ONSKAKU-ONSHINRYO-KBN
           END-IF
      *                             
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
      *
           MOVE    ONSKAKU-REC     TO  KOH-ONSKAKU-REC
      *
      *    対象の公費の資格確認レコードの更新処理
           PERFORM 200224-KOH-ONSKAKU-UPDATE-SEC
      *     
      *    資格確認レコードの追加処理（顔認証）
           IF      WRK-ERRCD       =   SPACE
      *??
               DISPLAY "SHORI-TIME=" WRK-KOH-SHORI-TIME
               IF      WRK-KOH-SHORI-TIME
                                   NOT =   SPACE
      *            資格確認レコードの更新処理（顔認証の公費ＵＩＤの更新）
                   MOVE    KOH-ONSKAKU-TBL-UUID
                                           TO  KOH-ONSKAKU-KOUHI-UUID
               END-IF
               PERFORM 200221-ONSHI-KAKU-INSERT-SEC
      *        処方管理レコード作成処理
               IF      WRK-ERRCD       =   SPACE
                   PERFORM 200225-SHOHO-KANRI-INSERT-SEC
               END-IF
          END-IF
            .
       200221-ONSHI-KAKU-HENSYU-EXT.
           EXIT.
      * 
      ******************************************************************
      *    処理結果コードのチェックによる設定処理
      ******************************************************************
       2002211-ONSKAKU-ERR-HENSYU-SEC  SECTION.
      *
      *    オン資側で設定済なら終了
           IF      ONSKAKU-ERR-CODE
                                   NOT =   SPACE
               GO  TO  2002211-ONSKAKU-ERR-HENSYU-EXT
           END-IF
           IF      WRK-ERR-CODE        =   SPACE
               GO  TO  2002211-ONSKAKU-ERR-HENSYU-EXT
           END-IF
      *
           MOVE    WRK-ERR-CODE        TO  ONSKAKU-ERR-CODE
           EVALUATE    WRK-ERR-CODE
               WHEN    SPACE
                   CONTINUE
               WHEN    "ORCAE01"
                   STRING  "患者ＩＤが一致しません。（"
                                               DELIMITED  BY  SIZE
                           WRK-ERR-PTID        DELIMITED  BY  SIZE
                           "）"                DELIMITED  BY  SIZE
                                               INTO    ONSKAKU-ERR-MSG
                   END-STRING
               WHEN    "ORCAE18"
                   STRING  "患者ＩＤが一致しなかったので初期化しました"
                                               DELIMITED  BY  SIZE
                           "。（"              DELIMITED  BY  SIZE
                           WRK-ERR-PTID        DELIMITED  BY  SIZE
                           "）"                DELIMITED  BY  SIZE
                                               INTO    ONSKAKU-ERR-MSG
                   END-STRING
            END-EVALUATE
            .
       2002211-ONSKAKU-ERR-HENSYU-EXT.
           EXIT.
      * 
      ******************************************************************
      *    資格確認レコードの追加処理（顔認証）
      ******************************************************************
       200221-ONSHI-KAKU-INSERT-SEC    SECTION.
      * 
           MOVE    KOH-ONSKAKU-REC TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *??
           DISPLAY "200221-ONSHI-KAKU-INSERT MCP-RC=" MCP-RC
            IF      MCP-RC      NOT =   ZERO 
                MOVE   "E11"             TO  WRK-ERRCD
                DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC 
           END-IF
            .
       200221-ONSHI-KAKU-INSERT-EXT.
           EXIT.
      ******************************************************************
      *    照会番号、薬剤、特定検診の処理
      ******************************************************************
       200222-RES1-HENSYU-SEC       SECTION.
      *
      *    要求ファイル用患者番号編集 
           MOVE    SPACE           TO  WRK-FILE-PATIENT-ID
           STRING  WRK-PATIENT-ID      DELIMITED  BY  SPACE
                   "XXXXXXXXXXXXXXXXXXXX"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-FILE-PATIENT-ID
           END-STRING                        
      *
           MOVE    WRK-PATIENT-ID  TO  QUARES1-PATIENT-ID
           MOVE    SPA-SYSYMD      TO  QUARES1-KAKUNINYMD
           MOVE    QUAREQ2-HOSPCD  TO  QUARES1-HOSPCD
      *    旧医療機関コードが設定されているとき
           IF      SYS-1051-HOSPCDF    =   SPACE
               CONTINUE
           ELSE
               MOVE    SYS-1051-HOSPCDF    TO  QUARES1-HOSPCD
           END-IF    
           MOVE    WRK-SIKIBETU    TO  QUARES1-SIKIBETU
      *
           IF      QUAREQ2-SIKAKU-YUKO
                                   =   "1" OR  "2" OR  "3"
               IF   (  QUAREQ2-SHOKAI-NUM (IDX)
                                           =   SPACE           )
      *r07.02
               OR
                    (( WRK-PATIENT-ID  NOT =   SPACE          )
                     AND
                     ( QUAREQ2-SHOKAI-NUM (IDX)
                                       NOT =   WRK-PATIENT-ID ))
                   MOVE    "01"            TO  ONSKAKU-SHOU-ENDFLG
      *                     
                   MOVE    "True"          TO  QUARES1-REF-CLASS
                   MOVE    QUAREQ2-HOSPCD  TO  QUARES1-REF-HOSPCD
      *            旧医療機関コードが設定されているとき
                   IF      SYS-1051-HOSPCDF    =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    SYS-1051-HOSPCDF
                                           TO  QUARES1-REF-HOSPCD
                   END-IF 
                   MOVE    WRK-SIKIBETU    TO  QUARES1-REF-SIKIBETU
      *************MOVE    WRK-PTID        TO  QUARES1-REF-SHOKAI-NUM
                   MOVE    WRK-PATIENT-ID  TO  QUARES1-REF-SHOKAI-NUM
                   MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                           TO  QUARES1-REF-HKNJANUM
                   MOVE    WRK-QUAREQ2-RES-KIGO (IDX)
                                           TO  QUARES1-REF-KIGO
                   MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                           TO  QUARES1-REF-NUM
                   MOVE    QUAREQ2-RES-EDABAN   (IDX)
                                           TO  QUARES1-REF-EDABAN
      *****    ELSE
      *            IF    ( WRK-PATIENT-ID
      *                                NOT =   SPACE )
      *            AND   ( WRK-PATIENT-ID
      *                                NOT =   QUAREQ2-SHOKAI-NUM(IDX) )
      *                 MOVE    WRK-PATIENT-ID
      *                                    TO  QUARES1-REF-SHOKAI-NUM
      *****        END-IF
               END-IF
      *        薬剤情報閲覧同意フラグが１（同意）
               IF      QUAREQ2-YAKUZAI-DOUIFLG (IDX)
                                       =   "1"
                   MOVE    "01"            TO  ONSKAKU-YAKUZAI-ENDFLG
                   MOVE    "True"          TO  QUARES1-DRG-CLASS
      *            
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"        TO  LNK-DAY6-IRAI
                   MOVE    SPA-SYSYMD  TO  LNK-DAY6-KIJUN
                   MOVE    "2"         TO  LNK-DAY6-ZOGENPTN
                   MOVE    -12         TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *??
      *   display "spaymd-12=" SPA-SYSYMD "#" LNK-DAY6-KEISAN "#"
      *            
                   INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
                   MOVE    SPA-HOSPNUM     TO  ONS-YKZ-M-HOSPNUM
                   MOVE    WRK-PTID        TO  ONS-YKZ-M-PTID
                   MOVE    ONSHI-YAKUZAI-MAIN-REC
                                           TO  MCPDATA-REC
                   MOVE    "key2"          TO  WRK-MCP-PATHNAME
                   PERFORM 900-ONSHI-YKZ-READ-SEC
                   IF      FLG-ONSHI-YKZ   =   ZERO
                       IF      ONS-YKZ-M-SRYYM <   LNK-DAY6-KEISAN(1:6)
                           MOVE    1               TO  FLG-ONSHI-YKZ
                       END-IF
                   END-IF
      *            
                   IF      FLG-ONSHI-YKZ   =   1
                       MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO  ONS-YKZ-M-SRYYM
                   END-IF                    
      *??
      *   display "FLG-ONSHI-YKZ=" FLG-ONSHI-YKZ "#" ONS-YKZ-M-SRYYM "#"
      *
                   MOVE    ZERO    TO  IDZZ
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       12
                       INITIALIZE          STS-AREA-DAY
                       INITIALIZE          LNK-DAY6-AREA
                       MOVE    "61"    TO  LNK-DAY6-IRAI
                       MOVE    SPA-SYSYMD (1:6)
                                       TO  LNK-DAY6-KIJUN (1:6)
                       MOVE    "01"    TO  LNK-DAY6-KIJUN (7:2)
                       MOVE    "2"     TO  LNK-DAY6-ZOGENPTN
                       COMPUTE LNK-DAY6-ZOGEN  =   IDXX    *   (-1) 
                       CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                      IF      LNK-DAY6-KEISAN(1:6)
                                           <   ONS-YKZ-M-SRYYM
                           MOVE    13          TO  IDXX
                       ELSE
                           ADD     1           TO  IDZZ
                           MOVE    QUAREQ2-HOSPCD
                                           TO  QUARES1-DRG-HOSPCD (IDZZ)
      *                    旧医療機関コードが設定されているとき
                           IF      SYS-1051-HOSPCDF    =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    SYS-1051-HOSPCDF
                                           TO  QUARES1-DRG-HOSPCD (IDZZ)
                           END-IF 
                           MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                               TO
                                           QUARES1-DRG-HKNJANUM   (IDZZ)
                           MOVE    WRK-QUAREQ2-RES-KIGO (IDX)
                                               TO
                                               QUARES1-DRG-KIGO   (IDZZ)
                           MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                               TO  QUARES1-DRG-NUM(IDZZ)
                           MOVE    QUAREQ2-RES-EDABAN   (IDX)
                                               TO
                                               QUARES1-DRG-EDABAN (IDZZ)
                           MOVE    WRK-SIKIBETU
                                               TO
                                           QUARES1-DRG-SIKIBETU   (IDZZ)
      *                    診療情報閲覧同意フラグ
                           IF      QUAREQ2-SHINRYO-DOUIFLG (IDX)
                                               =   "1"
                               MOVE    "4"         TO
                                          QUARES1-DRG-FILECATEGORY(IDZZ)
                           ELSE
                               MOVE    "2"         TO
                                          QUARES1-DRG-FILECATEGORY(IDZZ)
                           END-IF
                           MOVE    ONSKAKU-ONSHINRYO-KBN
                                               TO
                                               QUARES1-DRG-SRYKBN (IDZZ)
                           MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO
                                           QUARES1-DRG-STARTDATE  (IDZZ)
                                           QUARES1-DRG-ENDDATE    (IDZZ)
                       END-IF
                   END-PERFORM    
                   IF    ( SYS-1051-ONSHIYAKUZAI
                                           =   "1" )
                   OR    ( SYS-1051-ONSHIYKZSINRYO
                                           =   "1" )
                       ADD     1           TO  IDZZ
                       MOVE    QUAREQ2-HOSPCD
                                           TO  QUARES1-DRG-HOSPCD(IDZZ)
      *                旧医療機関コードが設定されているとき
                       IF      SYS-1051-HOSPCDF    =   SPACE
                           CONTINUE
                       ELSE
                           MOVE    SYS-1051-HOSPCDF
                                           TO  QUARES1-DRG-HOSPCD(IDZZ)
                       END-IF 
                       MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                           TO
                                           QUARES1-DRG-HKNJANUM  (IDZZ)
                       MOVE    WRK-QUAREQ2-RES-KIGO (IDX)
                                           TO  QUARES1-DRG-KIGO   (IDZZ)
                       MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                           TO  QUARES1-DRG-NUM    (IDZZ)
                       MOVE    QUAREQ2-RES-EDABAN   (IDX)
                                           TO  QUARES1-DRG-EDABAN (IDZZ)
                       MOVE    WRK-SIKIBETU
                                           TO
                                           QUARES1-DRG-SIKIBETU  (IDZZ)
      *                診療情報閲覧同意フラグ
                       IF    ( QUAREQ2-SHINRYO-DOUIFLG (IDX)
                                           =   "1" )
                       AND   ( SYS-1051-ONSHIYKZSINRYO
                                           =   "1" )
                           MOVE    "3"         TO
                                        QUARES1-DRG-FILECATEGORY  (IDZZ)
                       ELSE
                           MOVE    "1"         TO
                                        QUARES1-DRG-FILECATEGORY  (IDZZ)
                       END-IF
                       STRING  "YZKsiquc01req_"
                                           DELIMITED BY  SIZE
                               WRK-FILE-PATIENT-ID
                                           DELIMITED BY  SIZE
                               ".xml"      DELIMITED BY  SIZE
                                       INTO    QUARES1-DRG-FILE (IDZZ)
                       END-STRING
                       MOVE    ONSKAKU-ONSHINRYO-KBN
                                       TO  QUARES1-DRG-SRYKBN (IDZZ)
                       INITIALIZE          STS-AREA-DAY
                       INITIALIZE          LNK-DAY6-AREA
                       MOVE    "61"    TO  LNK-DAY6-IRAI
                       MOVE    SPA-SYSYMD
                                       TO  LNK-DAY6-KIJUN
                       MOVE    "2"     TO  LNK-DAY6-ZOGENPTN
                       MOVE    -12     TO  LNK-DAY6-ZOGEN
                       CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                       MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  QUARES1-DRG-STARTDATE (IDZZ)
                       MOVE    WRK-SPA-SYSYMD-1
                                       TO  QUARES1-DRG-ENDDATE   (IDZZ)
                   END-IF
               END-IF                            
      *        特定検診情報閲覧同意フラグが１（同意）
               IF      QUAREQ2-KENSIN-DOUIFLG (IDX)
                                       =   "1"
                   MOVE    "01"            TO  ONSKAKU-KENSIN-ENDFLG
                   MOVE    "True"          TO  QUARES1-SPEC-CLASS
                   MOVE    QUAREQ2-HOSPCD  TO  QUARES1-SPEC-HOSPCD   (1)
      *            旧医療機関コードが設定されているとき
                   IF      SYS-1051-HOSPCDF    =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    SYS-1051-HOSPCDF
                                           TO  QUARES1-SPEC-HOSPCD   (1)
                   END-IF 
                   MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                           TO  QUARES1-SPEC-HKNJANUM (1)
                   MOVE    WRK-QUAREQ2-RES-KIGO (IDX)
                                           TO  QUARES1-SPEC-KIGO     (1)
                   MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                           TO  QUARES1-SPEC-NUM      (1)
                   MOVE    QUAREQ2-RES-EDABAN   (IDX)
                                           TO  QUARES1-SPEC-EDABAN   (1)
                   MOVE    WRK-SIKIBETU    TO  QUARES1-SPEC-SIKIBETU (1)
                   MOVE    "2"             TO
                                           QUARES1-SPEC-FILECATEGORY (1)
                   MOVE    ONSKAKU-ONSHINRYO-KBN
                                           TO  QUARES1-SPEC-SRYKBN   (1)
                   IF      SYS-1051-ONSHIKENSIN 
                                           =   "1"                
                       MOVE    QUAREQ2-HOSPCD
                                           TO  QUARES1-SPEC-HOSPCD   (2)
      *                旧医療機関コードが設定されているとき
                       IF      SYS-1051-HOSPCDF    =   SPACE
                           CONTINUE
                       ELSE
                           MOVE    SYS-1051-HOSPCDF
                                           TO  QUARES1-SPEC-HOSPCD   (2)
                       END-IF 
                       MOVE    QUAREQ2-RES-HKNJANUM (IDX)
                                           TO  QUARES1-SPEC-HKNJANUM (2)
                       MOVE    WRK-QUAREQ2-RES-KIGO (IDX)
                                           TO  QUARES1-SPEC-KIGO     (2)
                       MOVE    WRK-QUAREQ2-RES-NUM  (IDX)
                                           TO  QUARES1-SPEC-NUM      (2)
                       MOVE    QUAREQ2-RES-EDABAN   (IDX) 
                                           TO  QUARES1-SPEC-EDABAN   (2)
                       MOVE    WRK-SIKIBETU
                                           TO  QUARES1-SPEC-SIKIBETU (2)
                       MOVE    "1"         TO
                                           QUARES1-SPEC-FILECATEGORY (2)
                       MOVE    ONSKAKU-ONSHINRYO-KBN
                                           TO  QUARES1-SPEC-SRYKBN   (2)
                       STRING  "TKKsiquc01req_"
                                           DELIMITED BY  SIZE
                               WRK-FILE-PATIENT-ID
                                           DELIMITED BY  SIZE
                               ".xml"      DELIMITED BY  SIZE
                                           INTO    QUARES1-SPEC-FILE (2)
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           .
       200222-RES1-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認レコードの更新処理（保険証）
      *****************************************************************
       200223-ONSHI-KAKU-UPDATE-SEC SECTION.
      *
      *??
           display "200223 UPDATE=" HKN-ONSHI-KAKU-IDX "#"
      *??         
           MOVE    SMCNDATE-YMD    TO  HKN-ONSKAKU-UPYMD
                                           (HKN-ONSHI-KAKU-IDX)
           MOVE    SMCNDATE-HMS    TO  HKN-ONSKAKU-UPHMS
                                           (HKN-ONSHI-KAKU-IDX)
           MOVE    HKN-ONSHI-KAKU-REC (HKN-ONSHI-KAKU-IDX)
                                   TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               display "tbl_onshi_kaku update error" 
               MOVE    "E14"           TO  WRK-ERRCD
           END-IF    
      *
           .
       200223-ONSHI-KAKU-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *     公費の資格確認レコードの更新処理
      *****************************************************************
       200224-KOH-ONSKAKU-UPDATE-SEC    SECTION.
      *
           MOVE    SPACE           TO  WRK-KOH-SHORI-TIME
      *
      *    保険情報の処理実行日時の日付と一致する公費の資格確認レコードを検索
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    KOH-ONSKAKU-RES-HKNJANUM
                                   TO  ONSKAKU-SHO-HKNJANUM
           MOVE    KOH-ONSKAKU-RES-KIGO
                                   TO  ONSKAKU-SHO-KIGO
           MOVE    KOH-ONSKAKU-RES-NUM
                                   TO  ONSKAKU-SHO-NUM
           MOVE    KOH-ONSKAKU-RES-EDABAN
                                   TO  ONSKAKU-SHO-EDABAN
           MOVE    "9"             TO  ONSKAKU-SHOKAI-KBN
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key11"         TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-NEXT-SEC
           ELSE
               INITIALIZE                  ONSKAKU-REC
               MOVE    1               TO  FLG-ONSHI-KAKU
           END-IF
      *
           IF      FLG-ONSHI-KAKU  =   ZERO
      *        処理実行日時の日付が一致し、公費ＵＩＤが未設定の公費の更新
               MOVE    ONSKAKU-SHORI-TIME  TO  WRK-KOH-SHORI-TIME
      *
               PERFORM     UNTIL   FLG-ONSHI-KAKU  =  1
                           OR      WRK-KOH-SHORI-TIME
                                               NOT =  ONSKAKU-SHORI-TIME
      *??
               display "200224 UPDATE=" KOH-ONSKAKU-TBL-UUID "#"
      *??
                   IF      KOH-ONSKAKU-PTID    >   0
                       IF    ( ONSKAKU-PTID        >   0            )
                       AND   ( KOH-ONSKAKU-PTID
                                               NOT =   ONSKAKU-PTID )
                           MOVE    "ORCAE01"       TO  WRK-ERR-CODE
                           MOVE    ONSKAKU-PTID    TO  WRK-ERR-PTID
                       END-IF                             
                       MOVE    KOH-ONSKAKU-PTID    TO  ONSKAKU-PTID
      *                MOVE    "1"                 TO  ONSKAKU-SET-PTID
                   END-IF
                   IF      KOH-ONSKAKU-CARD-CLASS
                                               =   "01"    
                       MOVE    KOH-ONSKAKU-TBL-UUID
                                               TO  ONSKAKU-KOUHI-UUID
                       MOVE    KOH-ONSKAKU-FUJYO-UUID
                                               TO  ONSKAKU-FUJYO-UUID
                   ELSE
                       MOVE    KOH-ONSKAKU-FUJYO-UUID
                                               TO  ONSKAKU-FUJYO-UUID
                                                   ONSKAKU-KOUHI-UUID
                   END-IF                                
                   MOVE    KOH-ONSKAKU-KAKUNINYMD
                                           TO  ONSKAKU-KAKUNINYMD
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-UPYMD
                   MOVE    SMCNDATE-HMS    TO  ONSKAKU-UPHMS
                   MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       display "tbl_onshi_kaku update error" 
                       MOVE    "E12"           TO  WRK-ERRCD
                       MOVE    1               TO  FLG-ONSHI-KAKU
                   ELSE
                       MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
                       MOVE    "key11"             TO  MCP-PATHNAME
                       PERFORM 900-ONSHI-KAKU-READ-NEXT-SEC
                   END-IF    
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200224-KOH-ONSKAKU-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *     処方管理レコード作成処理
      *****************************************************************
       200225-SHOHO-KANRI-INSERT-SEC   SECTION.
      *
           DISPLAY "20025-SHOHO=" SYS-1051-DENSHISHOHO
      *    電子処方箋区分が１以外は作成しない
           IF      SYS-1051-DENSHISHOHO
                               NOT =   "1"
               GO  TO  200225-SHOHO-KANRI-INSERT-EXT
           END-IF
      *
           INITIALIZE                  SHOHO-KANRI-REC
           MOVE    SPA-HOSPNUM     TO  SHOKANR-HOSPNUM
           MOVE    KOH-ONSKAKU-TBL-UUID
                                   TO  SHOKANR-TBL-UUID
           MOVE    1               TO  SHOKANR-RENNUM 
           MOVE    KOH-ONSKAKU-KAKU-KAKUNINYMD
                                   TO  SHOKANR-SRYYMD
           MOVE    KOH-ONSKAKU-PTID
                                   TO  SHOKANR-PTID 
      *****MOVE                    TO  SHOKANR-HAKKOKBN
           MOVE    ZERO            TO  SHOKANR-DENPNUM
           MOVE    QUAREQ2-SHO-SHOHO-KEITAI
                                   TO  SHOKANR-SHOHO-KEITAI
      *****MOVE                    TO  SHOKANR-PRESCRIPTIONID
      *****MOVE                    TO  SHOKANR-ACCESSCODE
      *****MOVE                    TO  SHOKANR-CANCEL-TIME
      *****MOVE                    TO  SHOKANR-CANCEL-UD-TIME
      *****MOVE                    TO  SHOKANR-CHANGE-TIME
      *****MOVE                    TO  SHOKANR-CHANGE-UD-TIME
           MOVE    SPA-OPID        TO  SHOKANR-OPID
           MOVE    SPA-TERMID      TO  SHOKANR-TERMID
           MOVE    SMCNDATE-YMD    TO  SHOKANR-CREYMD
           MOVE    SMCNDATE-HMS    TO  SHOKANR-CREHMS
      * 
           MOVE    SHOHO-KANRI-REC TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_shoho_kanri"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
            IF      MCP-RC      NOT =   ZERO 
                MOVE   "E13"             TO  WRK-ERRCD
                DISPLAY "TBL_SHOHO_KANRI INSERT ERR " MCP-RC 
           END-IF
           .
       200225-SHOHO-KANRI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *     全角変換処理
      *****************************************************************
       800-KANACHK-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-KANACHK-OUT-INPUT
           IF      WRK-KANACHK-MAE-INPUT
                               NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI 
               MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   2    )
                   CONTINUE
               ELSE    
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-KANACHK-OUT-INPUT
               END-IF
           END-IF
      *
           .
       800-KANACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     全角変換処理（拡張漢字対応）
      *****************************************************************
       801-KANACHK2-SEC           SECTION.
      *
           IF     SPA-ENCODING         =   2
      *        拡張漢字対応
               MOVE    SPACE           TO  WRK-KANACHK-OUT-INPUT
               IF      WRK-KANACHK-MAE-INPUT
                                   NOT =   SPACE
                   INITIALIZE                  ORCSKANACHK2AREA
                   MOVE    "2"             TO  KANACHK2-SYORI
                   MOVE    WRK-KANACHK-LEN TO  KANACHK2-MAX-CNT
                   MOVE    WRK-KANACHK-MAE-INPUT
                                           TO  KANACHK2-MAE-INPUT
                   CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
                   IF      KANACHK2-RC NOT =   ZERO
                       MOVE    KANACHK2-MAE-INPUT
                                               TO  WRK-KANACHK-OUT-INPUT
                   ELSE
                       MOVE    KANACHK2-OUT-INPUT
                                               TO  WRK-KANACHK-OUT-INPUT
                   END-IF
               END-IF
           ELSE
      *        拡張漢字不可
               PERFORM 800-KANACHK-SEC
           END-IF    
      *
           .
       801-KANACHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
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
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           EVALUATE    WRK-ERRCD
               WHEN    "E01"
                   MOVE    "照会区分が違います。"
                                                   TO  WRK-ERRMSG
               WHEN    "E07"
                   MOVE    "資格確認レコードが存在しません。"
                                                   TO  WRK-ERRMSG
               WHEN    "E10"
               WHEN    "E11"
                   MOVE    "資格確認レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
               WHEN    "E14"
                   MOVE    "資格確認レコードの更新ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
                   MOVE    "処方管理レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E15"
                   MOVE    "リクエストが空です。"
                                                   TO  WRK-ERRMSG
      *        共通エラー
               WHEN    "E89"
      *            ORCSCOMMON のエラー
                   EVALUATE    SPA-ERRCD
                       WHEN    "1001"
                           MOVE    "職員情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1002"
                           MOVE    "医療機関情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1003"
                           MOVE    "システム日付が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1005"
                           MOVE    "患者番号構成情報が取得できません。"
                                                   TO  WRK-ERRMSG
                       WHEN    "1015"
                           STRING  "グループ医療機関が不整合です。"
                               "処理を終了して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-ERRMSG
                           END-STRING
                       WHEN    OTHER
                           MOVE    "システム項目が設定できません。"
                                                   TO  WRK-ERRMSG
                   END-EVALUATE
               WHEN    "E90"
                   MOVE    "他端末で使用中です。"  TO  WRK-ERRMSG
               WHEN    "E91"
                   MOVE    "リクエスト番号が不正です。"
                                                   TO  WRK-ERRMSG
               WHEN    "E97"
                   MOVE   "送信内容に誤りがあります。"
                                                   TO  WRK-ERRMSG
               WHEN    "E98"
                   MOVE   "送信内容の読込ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E99"
                   MOVE    "ユーザＩＤが未登録です。"
                                                   TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
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
      *    オンライン資格確認照会結果読込
      *****************************************************************
       900-ONSHI-KAKU-READ-SEC           SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ONSKAKU-REC
               ELSE
                   INITIALIZE                      ONSKAKU-REC
                   MOVE    1                   TO  FLG-ONSHI-KAKU
               END-IF
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-KAKU-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険証の資格確認読込（照会区分＝２）
      ******************************************************************
       900-ONSHI-KAKU-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  WRK-ONSKAKU-REC
               IF      WRK-ONSKAKU-SHOKAI-KBN
                                           =   "2"
                   MOVE    ZERO                TO  FLG-ONSHI-KAKU
               ELSE
                   GO  TO  900-ONSHI-KAKU-READ-N-SEC
               END-IF        
           ELSE
               INITIALIZE                      WRK-ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           .
       900-ONSHI-KAKU-READ-N-EXT.
           EXIT.
      *
      ******************************************************************
      *    公費の資格確認読込
      ******************************************************************
       900-ONSHI-KAKU-READ-NEXT-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSKAKU-REC
               MOVE    ZERO                TO  FLG-ONSHI-KAKU
      *        公費ＵＩＤが設定済は対象外         　　
               IF      ONSKAKU-KOUHI-UUID
                                       NOT =   SPACE
                   GO  TO  900-ONSHI-KAKU-READ-NEXT-SEC
               END-IF
      *        処理実行日時の日付が一致しないときは対象外
               IF      KOH-ONSKAKU-SHORI-TIME (1:8)
                                       NOT =   ONSKAKU-SHORI-TIME (1:8)
                   GO  TO  900-ONSHI-KAKU-READ-NEXT-SEC
               END-IF
      *        任意のファイル識別子が空白以外は対象外
               IF      ONSKAKU-FILE-SIKIBETU
                                       NOT =   SPACE
                   GO  TO  900-ONSHI-KAKU-READ-NEXT-SEC
               END-IF
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           .
       900-ONSHI-KAKU-READ-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格薬剤情報読込
      *****************************************************************
       900-ONSHI-YKZ-READ-SEC     SECTION.
      *
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_yakuzai_main"
                                           TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  ONSHI-YAKUZAI-MAIN-REC
                   MOVE    ZERO            TO  FLG-ONSHI-YKZ
               ELSE
                   INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
                   MOVE    1               TO  FLG-ONSHI-YKZ
               END-IF
           ELSE
               INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
               MOVE    1               TO  FLG-ONSHI-YKZ
           END-IF
      *
           MOVE    "tbl_onshi_yakuzai_main"
                                       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .

       900-ONSHI-YKZ-READ-EXT.
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
      *
               IF  (   PTHKN-SAKJOKBN      =   "1"                     )
               OR  ( ( PTHKN-HKNNUM        =   "002"   OR  "031"   OR
                                               "032"   OR  "033"   OR
                                               "034"                  )
                 AND ( PTHKN-HOJOKBN       =   "A" OR  "B" OR  "C" OR
                                               "D" OR  "E" OR  "F" OR
                                               "G" OR  "H" OR  "I" OR
                                               "1" OR  "2" OR  "3"    ))
                   GO  TO  900-PTHKNINF-READ-N-SEC
               ELSE    
                   MOVE    ZERO                TO  FLG-PTHKNINF
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
      ******************************************************************
      *    患者公費情報読込
      ******************************************************************
       900-PTKOHINF-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                      PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       900-MONTHLYNUM-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-MONTHLYNUM
               MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
           ELSE
               INITIALIZE                      MONTHLYNUM-REC
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           .
       900-MONTHLYNUM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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

