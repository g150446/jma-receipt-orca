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
       PROGRAM-ID.           ORAPION003R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 保険証資格確認登録、更新処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  20/11/06    NACL−藤原    新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01     ORCAMO       21/06/10  資格確認：要求ファイル名修正
      *  05.00.02     ORCAMO       21/07/05  資格確認：照会番号に患者番号を設定
      *  05.00.03     ORCAMO       21/07/12  資格確認：住所から全角空白を除く
      *  05.01.01     ORCAMO       22/04/25  資格確認：間違って登録され
      *                                      ている照会番号の再登録依頼
      *  05.01.02    ORCAMO        22/07/15  資格確認：診療情報対応
      *  05.01.03    ORCAMO        22/08/22  保険証ＯＣＲ(アルメックス）対応
      *  05.01.04    ORCAMO        22/11/14  資格確認検索条件の変更
      *  05.01.05    ORCAMO        22/12/05  画像取込み処理変更
      *                                      （保険証ＯＣＲおよび公費医療券画像）
      *  05.01.06    ORCAMO        22/12/09  処方箋発行形態、手術情報対応
      *  05.01.07    ORCAMO        22/01/17  電子処方箋対応
      *  05.01.08    ORCAMO        23/02/15  返却された保険情報の設定がないときの対応
      *  05.01.09    ORCAMO        23/03/09  特定検診のファイル名を修正
      *  05.02.01    ORCAMO        23/12/21  医療扶助対応
      *  05.02.02    ORCAMO        24/04/08  医療扶助対応
      *                                      併用時の保険情報資格確認
      *  05.02.03    ORCAMO        24/05/13  エラーデータ対応
      *                                      被保険者証区分が空白の場合
      *  05.02.04    ORCAMO        24/06/11  エラーデータ対応
      *                                      被保険者証区分が空白のとき
      *                                      被保険者証区分の更新を行わない
      *  05.02.05    ORCAMO        24/07/26  資格取得年月日追加
      *                                      保険情報が主保険の場合、
      *                                      資格取得年月日を設定する
      *  05.02.06    ORCAMO        25/06/09  旧医療機関コード対応
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
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SHOHO-KANRI         PIC 9(01).
           03  FLG-HEIYO-ONSHI-KAKU    PIC 9(01).
      *
           03  PTHKNINF-OK             PIC 9(01).      
           03  FLG-SHOKAI-NUM          PIC 9(01).
           03  FLG-HKN-KBN             PIC X(02).
           03  FLG-HKN-OK              PIC X(02).
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
           03  IDWW                    PIC 9(04).
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
      *
           03  WRK-FILE-SIKIBETU.
               05  WRK-FILE-NM1        PIC X(50).
               05  WRK-FILE-NM2        PIC X(50).
      *
           03  WRK-UUID                PIC X(36).
           03  WRK-SHOKAI-KBN          PIC X(01).
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
      *    資格確認用エラーコード     
           03  WRK-ERR-CODE            PIC X(09).
           03  WRK-ERR-PTID            PIC 9(10).
      *    エラーコード
           03  WRK-ERRCD               PIC X(03).
           03  WRK-ERRMSG              PIC X(100).
      *    公費の処理実行日時
           03  WRK-KOH-SHORI-TIME      PIC X(14).
      *    全角変換用 
           03  WRK-KANACHK-MAE-INPUT   PIC X(5000).
           03  WRK-KANACHK-OUT-INPUT   PIC X(5000).
           03  WRK-KANACHK-LEN         PIC 9(04).
      *
           03  WRK-SPA-SYSYMD-1        PIC X(08).
      *
           03  WRK-FUJYO-UUID          PIC X(36).
           03  WRK-HKN-TBL-UUID        PIC X(36).
      *
       01  WRK-QUAREQ2-AREA.     
           03  WRK-QUAREQ2-FILE-SIKIBETU
                                       PIC X(50).
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
      *    公費の資格確認情報の追加
       01  WRK-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //WRK-ONSKAKU//.
      * 
      *    保険証資格管理情報
       01  HKN-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //HKN-ONSKAKU//.
      *
      *    顔認証の資格確認情報（公費の資格確認情報の親ＵＩＤ）
       01  KOH-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //KOH-ONSKAKU//.
      *
      *    処方箋管理更新用の資格確認情報
       01  SHOHO-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //SHOHO-ONSKAKU//.
      *
      *    併用のときの資格確認情報
       01  HEIYO-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //HEIYO-ONSKAKU//.
      *
      *    UUIDから検索した資格確認情報
       01  UUID-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //UUID-ONSKAKU//.
      *
      *    公費の資格確認情報の退避（資格有効が５のとき）
       01  TAIHI-ONSKAKU-AREA.
           02  TAIHI-ONSKAKU-REC           OCCURS  50.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //TAIHI-ONSKAKU//.
           02  TAIHI-ONSKAKU-MAX   PIC 9(02).
      *
      *    併用の資格確認情報の退避（資格有効が５のとき）
       01  TAIHI-HEI-ONSKAKU-AREA.
           02  TAIHI-HEI-ONSKAKU-REC       OCCURS  50.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //TAIHI-H-ONSKAKU//.
           02  TAIHI-HEI-ONSKAKU-MAX   PIC 9(02).
           02  FLG-TAIHI-H-SIKAKU-YUKO PIC 9(01).
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
      *    資格確認テーブル
       01  ONSKAKU-REC.       
           COPY    "CPONSHI-KAKU.INC".
      *
      *    資格確認薬剤情報
       01  ONSHI-YAKUZAI-MAIN-REC.       
           COPY    "CPONSHI-YAKUZAI-MAIN.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    処方箋管理
       01  SHOHO-KANRI-REC.
           COPY    "CPSHOHO-KANRI.INC".
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
           DISPLAY   "*  onlinequa3 start  *"
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
               MOVE   404                  TO  APILST03-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "*   onlinequa3 end   *"
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
           IF      APILST03-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAREQ2
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinequareq2" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST03-BODY       TO  APIREQ-OBJECT
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
               MOVE    "000"           TO  QUARES1-API-RESULT
           ELSE
               DISPLAY "300 WRK-ERRCD=" WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  QUARES1-API-RESULT
               MOVE    WRK-ERRMSG      TO  QUARES1-API-RESULT-MSG
           END-IF
      *
           IF      APILST03-BODY       NOT =   LOW-VALUE
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
               MOVE  QUARES1-OBJECT    TO  APILST03-BODY
               MOVE  "application/xml" TO  APILST03-CONTENT-TYPE
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
           IF      QUAREQ2-SHOKAI-KBN
                               =   "2"
               CONTINUE
           ELSE
               DISPLAY "SHOKAI-KBN=" QUAREQ2-SHOKAI-KBN
               MOVE    "E01"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険証資格確認登録、更新処理
      *****************************************************************
       2002-SIKAKU-KAKUNIN-UPDATE-SEC  SECTION.
      *
           INITIALIZE              WRK-PTID-AREA
           INITIALIZE              WRK-QUAREQ2-AREA
      *
           INITIALIZE              TAIHI-ONSKAKU-AREA
                                   TAIHI-HEI-ONSKAKU-AREA
                                   WRK-ONSKAKU-REC
                                   HKN-ONSKAKU-REC
                                   KOH-ONSKAKU-REC
                                   SHOHO-ONSKAKU-REC
                                   HEIYO-ONSKAKU-REC
                                   UUID-ONSKAKU-REC
      *
      *    ファイル識別子
           IF      QUAREQ2-FILE-SIKIBETU
                               NOT =   SPACE
               MOVE    QUAREQ2-FILE-SIKIBETU
                                       TO  WRK-QUAREQ2-FILE-SIKIBETU
               EVALUATE    WRK-QUAREQ2-FILE-SIKIBETU (1:11)
                   WHEN    "ALMocr02res"
                   WHEN    "ALMocr03res"
                       MOVE    WRK-QUAREQ2-FILE-SIKIBETU (7:2)
                                               TO  FLG-HKN-KBN
                       IF      WRK-QUAREQ2-FILE-SIKIBETU (13:8)
                                               =   "99999999"
                           MOVE    "NG"            TO  FLG-HKN-OK
                       ELSE
                           MOVE    "OK"            TO  FLG-HKN-OK
                       END-IF
               END-EVALUATE
           END-IF
      * 
      *    扶助UIDコード設定
           IF      QUAREQ2-FUJYO-UUID  =   SPACE 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       50
                       OR      QUAREQ2-CARD-CLASS (IDX)
                                   =       SPACE 
                   IF      QUAREQ2-CARD-CLASS (IDX)
                                           =   "A1"
                       PERFORM 800-UID-GET-SEC
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
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
                   OR      QUAREQ2-CARD-CLASS (IDX)
                               =       SPACE 
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
               COMPUTE WRK-QUAREQ2-RESULT-MAX  =
                                       WRK-QUAREQ2-RESULT-MAX  +   1
      *        ひとつの保険が返却されたときで保険OCRのとき                        
      *        資格確認から患者ＩＤ取得
      ***         IF      QUAREQ2-SIKAKU-YUKO
      *                                 =   "5"
      *             CONTINUE 
      *         ELSE
      *             IF      FLG-HKN-OK  NOT =   SPACE
      *                 IF      QUAREQ2-CARD-CLASS (IDX)
      *                                         =   "A1"
      *                     PERFORM 20023-PTID-03-SEC
      *                 ELSE
      *                     MOVE    IDX     TO  WRK-QUAREQ2-RESULT-HKN
      *                     IF      QUAREQ2-SHOKAI-NUM (IDX)
      *                                             =   SPACE
      *                         PERFORM 20021-PTID-01-SEC
      *                     ELSE                           
      *                         PERFORM 20022-PTID-02-SEC
      *                     END-IF
      *                 END-IF
      *
      *                 COMPUTE WRK-QUAREQ2-RESULT-MAX  =
      *                                 WRK-QUAREQ2-RESULT-MAX  +   1
      *             END-IF
      ***         END-IF   
           END-PERFORM
      *     
           IF      QUAREQ2-SIKAKU-YUKO
                                   =   "5"
      *        複数の保険が返却されたとき     
              PERFORM 20021-SYORI-01-SEC 
           ELSE
      *        ひとつの保険が返却されたとき                        
               IF      FLG-HKN-OK  NOT =   SPACE
      *            保険OCRの処理         
                   PERFORM 20023-SYORI-03-SEC
               ELSE
                   PERFORM 20022-SYORI-02-SEC
               END-IF    
           END-IF
           .        
       2002-SIKAKU-KAKUNIN-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険証資格確認登録、更新処理（複数の保険が返却されたとき） 
      *****************************************************************
       20021-SYORI-01-SEC  SECTION.
      *
           INITIALIZE              TAIHI-ONSKAKU-AREA
                                   TAIHI-HEI-ONSKAKU-AREA
      *
      *    保険証ＯＣＲのとき     
           IF      FLG-HKN-OK  NOT =   SPACE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       50
                       OR      QUAREQ2-CARD-CLASS (IDXX)
                                       =       SPACE
                   MOVE    SPACE           TO  QUARES1-QUARES1
      *
                   IF      IDXX            =   1
      *                保険証ＯＣＲ資格確認登録、更新処理
                       PERFORM 200224-HKNOCR-HENSYU-SEC
                       MOVE    KOH-ONSKAKU-REC     TO  HKN-ONSKAKU-REC
                   ELSE
                       MOVE    HKN-ONSKAKU-REC     TO  ONSKAKU-REC
      *                資格確認レコードの作成処理（枝番なしで複数のとき）
                       PERFORM 200212-01-ONSHI-KAKU-INSERT-SEC
                   END-IF
               END-PERFORM
      *
               GO  TO  20021-SYORI-01-EXT
           END-IF
      * 
      *    ＵＩＤから保険証の資格確認レコードを検索
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
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    WRK-UUID        TO  ONSKAKU-TBL-UUID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           MOVE    "2"             TO  WRK-SHOKAI-KBN
           PERFORM 900-ONSHI-KAKU-READ-SEC
           IF      FLG-ONSHI-KAKU  =   ZERO
               MOVE    ONSKAKU-REC     TO  UUID-ONSKAKU-REC
      *
               IF      ONSKAKU-FUJYO-HEIYO
                                       NOT =   "1"
                   PERFORM 200211-SYORI-01-TANDOKU-SEC
               ELSE
      *            当日分でptid、fujyo_uuid1が一致する資格確認レコードを検索
                   INITIALIZE                  HEIYO-ONSKAKU-REC
                   MOVE    SPA-HOSPNUM     TO  HEIYO-ONSKAKU-HOSPNUM
                   MOVE    ONSKAKU-PTID    TO  HEIYO-ONSKAKU-PTID
                   MOVE    ONSKAKU-FUJYO-UUID1
                                           TO  HEIYO-ONSKAKU-FUJYO-UUID1
                   MOVE    HEIYO-ONSKAKU-REC
                                           TO  MCPDATA-REC
                   PERFORM 900-ONSHI-KAKU-READ-KEY29-SEC
                   IF      TAIHI-HEI-ONSKAKU-MAX
                                           >   ZERO
                       IF      UUID-ONSKAKU-CARD-CLASS
                                               =   "A1"
      *                    当日分でptid、oya_uuidが一致する資格確認レコードを検索
                           INITIALIZE              HEIYO-ONSKAKU-REC
                           MOVE    SPA-HOSPNUM TO  HEIYO-ONSKAKU-HOSPNUM
                           MOVE    TAIHI-H-ONSKAKU-PTID     (1)
                                               TO  HEIYO-ONSKAKU-PTID
                           MOVE    TAIHI-H-ONSKAKU-OYA-UUID (1)
                                               TO HEIYO-ONSKAKU-OYA-UUID
                           MOVE    HEIYO-ONSKAKU-REC
                                               TO  MCPDATA-REC
                           PERFORM 900-ONSHI-KAKU-READ-KEY30-SEC
                           IF      TAIHI-HEI-ONSKAKU-MAX
                                               >   ZERO
                              PERFORM 200211-SYORI-01-HEIYO-SEC
                           ELSE
                               PERFORM 200211-SYORI-01-TANDOKU-SEC
                           END-IF    
                       ELSE    
                           PERFORM 200211-SYORI-01-HEIYO-SEC
                       END-IF    
                   ELSE
                       PERFORM 200211-SYORI-01-TANDOKU-SEC
                   END-IF
               END-IF    
           ELSE
               DISPLAY "FACE ONSKAU NOT FOUND=" WRK-UUID
               MOVE    "XXXXXXXXXXXXXXXXXXXX"  TO  QUARES1-PATIENT-ID
           END-IF    
           .        
       20021-SYORI-01-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険証資格確認登録、更新処理（複数の保険が返却されたとき）
      *     単独のとき 
      *****************************************************************
       200211-SYORI-01-TANDOKU-SEC   SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       50
                   OR      QUAREQ2-CARD-CLASS (IDXX)
                                   =       SPACE
               MOVE    SPACE           TO  QUARES1-QUARES1
               IF      IDXX            =   1
                   MOVE    ONSKAKU-TBL-UUID
                                           TO  ONSKAKU-OYA-UUID
                   IF    (  QUAREQ2-CARD-CLASS (2)
                                               =   SPACE )
                   AND   (  ONSKAKU-ORCA-ERR-CODE
                                               =   SPACE )
                       MOVE    "11"            TO  ONSKAKU-ORCA-ERR-CODE
                   END-IF
      ***          MOVE    IDXX            TO  IDX
      *            資格確認編集処理
                   IF      QUAREQ2-CARD-CLASS (2)
                                               =   SPACE
                       MOVE    "02"            TO  ONSKAKU-KAKU-ENDFLG
                   ELSE     
                       MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
                   END-IF     
                   PERFORM 200211-ONSKAKU-HENSYU-SEC
      *            資格確認レコードの更新処理（保険証）
                   PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
      * 
                   MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
               ELSE    
                   MOVE    HKN-ONSKAKU-REC TO  ONSKAKU-REC
      ****         MOVE    IDXX            TO  IDX
      *            資格確認レコードの作成処理（枝番なしで複数のとき）
                   PERFORM 200212-01-ONSHI-KAKU-INSERT-SEC
               END-IF
           END-PERFORM
           .        
       200211-SYORI-01-TANDOKU-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険証資格確認登録、更新処理（複数の保険が返却されたとき）
      *     併用のとき 
      *****************************************************************
       200211-SYORI-01-HEIYO-SEC   SECTION.
      * 
           EVALUATE    UUID-ONSKAKU-CARD-CLASS
             WHEN    "A1"
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       TAIHI-HEI-ONSKAKU-MAX
                 MOVE    TAIHI-HEI-ONSKAKU-REC (IDZZ)
                                           TO  ONSKAKU-REC
      *          UIDコード取得処理
                 PERFORM 800-UID-GET-SEC
                 MOVE    C-UID           TO  ONSKAKU-FUJYO-UUID
      *          資格確認レコードの更新処理（保険証）
                 PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
                 MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
      *
      *          生保の資格確認レコードの作成処理
                 PERFORM VARYING IDXX    FROM    1   BY  1
                         UNTIL   IDXX    >       1
                         OR      QUAREQ2-CARD-CLASS (IDXX)
                                         =       SPACE
                     MOVE    SPACE           TO  QUARES1-QUARES1
      *
                     MOVE    UUID-ONSKAKU-REC
                                             TO  ONSKAKU-REC
      *
                     MOVE    HKN-ONSKAKU-FUJYO-UUID
                                             TO  ONSKAKU-FUJYO-UUID
                     MOVE    HKN-ONSKAKU-TBL-UUID
                                             TO  ONSKAKU-KOUHI-UUID
                     MOVE    "1"             TO  ONSKAKU-FUJYO-HEIYO
      *              UIDコード取得処理
                     PERFORM 800-UID-GET-SEC
                     MOVE    C-UID           TO  ONSKAKU-TBL-UUID
      *              任意のファイル識別子(医療機関固有項目)の設定
                     MOVE    SPACE           TO  WRK-SIKIBETU
                     STRING  ","             DELIMITED  BY  SIZE  
                             C-UID           DELIMITED  BY  SPACE
                                             INTO       WRK-SIKIBETU
                     END-STRING
                     MOVE    WRK-SIKIBETU    TO  ONSKAKU-FILE-SIKIBETU
      *                             
                     MOVE    SPA-OPID        TO  ONSKAKU-OPID
                     MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
                     MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
                     MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
                     MOVE    SPACE           TO  ONSKAKU-UPYMD
                                                 ONSKAKU-UPHMS
      * 
                     MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                     MOVE    "DBINSERT"      TO  MCP-FUNC
                     MOVE    "tbl_onshi_kaku"
                                             TO  MCP-TABLE
                     MOVE    "key"           TO  MCP-PATHNAME
                     PERFORM 900-ORCDBMAIN-SEC
      *
                     DISPLAY "200212-ONSHI-KAKU-INSERT MCP-RC=" MCP-RC
                     IF      MCP-RC      NOT =   ZERO 
                         MOVE   "E11"             TO  WRK-ERRCD
                         DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC
                         GO  TO  200211-SYORI-01-HEIYO-EXT
                     END-IF
                 END-PERFORM
               END-PERFORM
             WHEN    OTHER
               PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       50
                   OR      QUAREQ2-CARD-CLASS (IDXX)
                                   =       SPACE
                 MOVE    SPACE           TO  QUARES1-QUARES1
                 IF      IDXX            =   1
                   MOVE    ONSKAKU-TBL-UUID
                                           TO  ONSKAKU-OYA-UUID
                   IF    (  QUAREQ2-CARD-CLASS (2)
                                               =   SPACE )
                   AND   (  ONSKAKU-ORCA-ERR-CODE
                                               =   SPACE )
                       MOVE    "11"            TO  ONSKAKU-ORCA-ERR-CODE
                   END-IF
      ***          MOVE    IDXX            TO  IDX
      *            資格確認編集処理
                   IF      QUAREQ2-CARD-CLASS (2)
                                               =   SPACE
                       MOVE    "02"            TO  ONSKAKU-KAKU-ENDFLG
                   ELSE     
                       MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
                   END-IF
                   PERFORM 200211-ONSKAKU-HENSYU-SEC
                   MOVE    SPACE           TO  ONSKAKU-KOUHI-UUID
                   MOVE    UUID-ONSKAKU-FUJYO-UUID
                                           TO  ONSKAKU-FUJYO-UUID  
      *            資格確認レコードの更新処理（保険証）
                   PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
      *             
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL   IDZZ    >       TAIHI-HEI-ONSKAKU-MAX
      *                生保のkouhi_uuid に主保険のtbl_uuid を設定
                       MOVE    TAIHI-HEI-ONSKAKU-REC (IDZZ)
                                               TO  HEIYO-ONSKAKU-REC
                       MOVE    ONSKAKU-TBL-UUID
                                               TO
                                               HEIYO-ONSKAKU-KOUHI-UUID
                       MOVE    SMCNDATE-YMD    TO  HEIYO-ONSKAKU-UPYMD
                       MOVE    SMCNDATE-HMS    TO  HEIYO-ONSKAKU-UPHMS
                       MOVE    HEIYO-ONSKAKU-REC
                                               TO  MCPDATA-REC
                       MOVE    "DBUPDATE"      TO  MCP-FUNC
                       MOVE    "tbl_onshi_kaku"
                                               TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC      NOT =   ZERO
                           display "tbl_onshi_kaku update error"
                           MOVE    "E14"           TO  WRK-ERRCD
                           GO  TO  200211-SYORI-01-HEIYO-EXT 
                       END-IF
                   END-PERFORM
      * 
                   MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
                 ELSE
                   MOVE    HKN-ONSKAKU-REC TO  ONSKAKU-REC
      *            UIDコード取得処理
                   PERFORM 800-UID-GET-SEC
                   MOVE    C-UID           TO  WRK-FUJYO-UUID
                   MOVE    WRK-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
                   MOVE    "1"             TO  ONSKAKU-FUJYO-HEIYO
      ****         MOVE    IDXX            TO  IDX
      *            資格確認レコードの作成処理（枝番なしで複数のとき）
                   PERFORM 200212-01-ONSHI-KAKU-INSERT-SEC
                   MOVE    ONSKAKU-TBL-UUID
                                           TO  WRK-HKN-TBL-UUID
      *            生保の資格確認レコードの作成処理
                  PERFORM VARYING IDZZ    FROM    1   BY  1
                          UNTIL   IDZZ    >       TAIHI-HEI-ONSKAKU-MAX
                       MOVE    TAIHI-HEI-ONSKAKU-REC (IDZZ)
                                               TO  ONSKAKU-REC
                       MOVE    WRK-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
                       MOVE    "1"             TO  ONSKAKU-FUJYO-HEIYO
                       MOVE    WRK-HKN-TBL-UUID
                                               TO  ONSKAKU-KOUHI-UUID
      *                UIDコード取得処理
                       PERFORM 800-UID-GET-SEC
                       MOVE    C-UID           TO  ONSKAKU-TBL-UUID
      *                任意のファイル識別子(医療機関固有項目)の設定
                       MOVE    SPACE           TO  WRK-SIKIBETU
                       STRING  ","             DELIMITED  BY  SIZE  
                               C-UID           DELIMITED  BY  SPACE
                                               INTO       WRK-SIKIBETU
                       END-STRING
                       MOVE    WRK-SIKIBETU    TO  ONSKAKU-FILE-SIKIBETU
      *                             
                       MOVE    SPA-OPID        TO  ONSKAKU-OPID
                       MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
                       MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
                       MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
                       MOVE    SPACE           TO  ONSKAKU-UPYMD
                                                   ONSKAKU-UPHMS
      * 
                       MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                       MOVE    "DBINSERT"      TO  MCP-FUNC
                       MOVE    "tbl_onshi_kaku"
                                               TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
      *
                       DISPLAY "200212-ONSHI-KAKU-INSERT MCP-RC=" MCP-RC
                       IF      MCP-RC      NOT =   ZERO 
                           MOVE   "E11"             TO  WRK-ERRCD
                           DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC
                           GO  TO  200211-SYORI-01-HEIYO-EXT
                       END-IF
                   END-PERFORM
                 END-IF
               END-PERFORM
           END-EVALUATE
           .        
       200211-SYORI-01-HEIYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    先頭の情報による資格確認編集処理
      *****************************************************************
       200211-ONSKAKU-HENSYU-SEC   SECTION.
      *
           IF    ( WRK-PATIENT-ID  NOT =   SPACE )
               IF    ( QUAREQ2-SHOKAI-NUM (IDXX)
                                       NOT =   SPACE          )
               AND   ( QUAREQ2-SHOKAI-NUM (IDXX)
                                       NOT =   WRK-PATIENT-ID )
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
               ELSE        
                   MOVE    QUAREQ2-SHOKAI-NUM (IDXX)
                                           TO  ONSKAKU-SHOKAI-NUM
               END-IF
           ELSE
               MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
           END-IF
           MOVE    QUAREQ2-FUJYO-UUID
                                   TO  ONSKAKU-FUJYO-UUID
      *    医療扶助単独のときは公費ＵＩＤは設定しない
           IF      QUAREQ2-FUJYO-UUID
                               NOT =   SPACE
               IF    ( WRK-QUAREQ2-RESULT-MAX
                                       =   1     )
               AND   ( QUAREQ2-CARD-CLASS (IDXX)
                                       =   "A1"  )
                   CONTINUE
               ELSE                            
                   IF  QUAREQ2-CARD-CLASS (IDXX)
                                       =   "A1"
                       MOVE    HKN-ONSKAKU-TBL-UUID
                                           TO  ONSKAKU-KOUHI-UUID
                   ELSE                        
                       MOVE    ONSKAKU-TBL-UUID
                                           TO  ONSKAKU-KOUHI-UUID
                   END-IF                        
               END-IF
           END-IF
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
           MOVE    QUAREQ2-SHORI-TIME
                                   TO  ONSKAKU-SHORI-TIME 
           MOVE    QUAREQ2-KAKUNINYMD
                                   TO  ONSKAKU-KAKUNINYMD
           MOVE    QUAREQ2-HOSPCD  TO  ONSKAKU-HOSPCD
           MOVE    QUAREQ2-FILE-SIKIBETU
                                   TO  ONSKAKU-FILE-SIKIBETU
           MOVE    QUAREQ2-SHOKAI-KBN
                                   TO  ONSKAKU-SHOKAI-KBN
           MOVE    QUAREQ2-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
           MOVE    QUAREQ2-ERR-CODE
                                   TO  ONSKAKU-ERR-CODE
           MOVE    QUAREQ2-ERR-MSG TO  ONSKAKU-ERR-MSG
           PERFORM 2002111-ONSKAKU-ERR-HENSYU-SEC
           MOVE    QUAREQ2-CODE-TYPE
                                   TO  ONSKAKU-CODE-TYPE
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
           IF      ONSKAKU-CARD-CLASS  =   "A1"
               CONTINUE
           ELSE
               MOVE    QUAREQ2-CARD-CLASS   (IDXX) 
                                       TO  ONSKAKU-CARD-CLASS
           END-IF
           MOVE    QUAREQ2-RES-HKNJANUM (IDXX)
                                   TO  ONSKAKU-RES-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-KIGO (IDXX)
                                   TO  ONSKAKU-RES-KIGO
           MOVE    WRK-QUAREQ2-RES-NUM (IDXX)
                                   TO  ONSKAKU-RES-NUM 
           MOVE    QUAREQ2-RES-EDABAN (IDXX) 
                                   TO  ONSKAKU-RES-EDABAN 
           MOVE    QUAREQ2-RES-HONKZKKBN (IDXX) 
                                   TO  ONSKAKU-RES-HONKZKKBN 
           MOVE    WRK-QUAREQ2-RES-HIHKNJANAME (IDXX)
                                   TO  ONSKAKU-RES-HIHKNJANAME 
           MOVE    WRK-QUAREQ2-RES-NAME (IDXX)
                                   TO  ONSKAKU-RES-NAME 
           MOVE    WRK-QUAREQ2-RES-NAME-ETC (IDXX)
                                   TO  ONSKAKU-RES-NAME-ETC 
           MOVE    WRK-QUAREQ2-RES-KANANAME (IDXX)
                                   TO  ONSKAKU-RES-KANANAME 
           MOVE    WRK-QUAREQ2-RES-KANANAME-ETC (IDXX)
                                   TO  ONSKAKU-RES-KANANAME-ETC
           MOVE    QUAREQ2-RES-SEX1 (IDXX)
                                   TO  ONSKAKU-RES-SEX1 
           MOVE    QUAREQ2-RES-SEX2 (IDXX)
                                   TO  ONSKAKU-RES-SEX2 
           MOVE    QUAREQ2-RES-BIRTHDAY (IDXX)
                                   TO  ONSKAKU-RES-BIRTHDAY 
           MOVE    WRK-QUAREQ2-RES-ADRS (IDXX)
                                   TO  ONSKAKU-RES-ADRS 
           MOVE    QUAREQ2-RES-POST (IDXX)
                                   TO  ONSKAKU-RES-POST 
           MOVE    QUAREQ2-SKKGETYMD (IDXX)
                                   TO  ONSKAKU-SKKGETYMD 
           MOVE    QUAREQ2-TEKSTYMD (IDXX)
                                   TO  ONSKAKU-TEKSTYMD 
           MOVE    QUAREQ2-TEKEDYMD (IDXX)
                                   TO  ONSKAKU-TEKEDYMD 
           MOVE    QUAREQ2-FTNRATE (IDXX)
                                   TO  ONSKAKU-FTNRATE 
           MOVE    QUAREQ2-MISYUGAKU (IDXX)
                                   TO  ONSKAKU-MISYUGAKU 
           MOVE    QUAREQ2-LOSS-JIYU  (IDXX)
                                   TO  ONSKAKU-LOSS-JIYU 
           MOVE    WRK-QUAREQ2-HKNJANUM-NAME (IDXX)
                                   TO  ONSKAKU-HKNJANUM-NAME 
           MOVE    QUAREQ2-ELDER-SKKGETYMD (IDXX)
                                   TO  ONSKAKU-ELDER-SKKGETYMD 
           MOVE    QUAREQ2-ELDER-TEKSTYMD (IDXX)
                                   TO  ONSKAKU-ELDER-TEKSTYMD 
           MOVE    QUAREQ2-ELDER-TEKEDYMD (IDXX)
                                   TO  ONSKAKU-ELDER-TEKEDYMD 
           MOVE    QUAREQ2-ELDER-FTNRATE (IDXX)
                                   TO  ONSKAKU-ELDER-FTNRATE
           IF      QUAREQ2-CARD-CLASS (IDXX)  =   "A1"
               MOVE    QUAREQ2-JOIN-SKKGETYMD (IDXX)
                                  TO  ONSKAKU-KOH-TEKSTYMD (1)
               MOVE    QUAREQ2-JOIN-SKKLOSSYMD (IDXX)
                                  TO  ONSKAKU-KOH-TEKEDYMD (1)
           ELSE
               MOVE    QUAREQ2-JOIN-SKKGETYMD (IDXX)
                                   TO  ONSKAKU-JOIN-SKKGETYMD
           END-IF
           MOVE    QUAREQ2-GENDO-DOUIFLG (IDXX)
                                   TO  ONSKAKU-GENDO-DOUIFLG 
           MOVE    QUAREQ2-GENDO-TIME (IDXX)
                                   TO  ONSKAKU-GENDO-TIME 
           MOVE    QUAREQ2-GENDO-SHOKBN (IDXX)
                                   TO  ONSKAKU-GENDO-SHOKBN 
           MOVE    QUAREQ2-GENDO-TEKKBN (IDXX)
                                   TO  ONSKAKU-GENDO-TEKKBN 
           MOVE    QUAREQ2-GENDO-SKKGETYMD (IDXX)
                                   TO  ONSKAKU-GENDO-SKKGETYMD 
           MOVE    QUAREQ2-GENDO-TEKSTYMD (IDXX)
                                   TO  ONSKAKU-GENDO-TEKSTYMD 
           MOVE    QUAREQ2-GENDO-TEKEDYMD (IDXX)
                                   TO  ONSKAKU-GENDO-TEKEDYMD 
           MOVE    QUAREQ2-GENDO-SKJGNSTYMD (IDXX)
                                   TO  ONSKAKU-GENDO-SKJGNSTYMD 
           MOVE    QUAREQ2-SIKKAN-DOUIFLG (IDXX)
                                   TO  ONSKAKU-SIKKAN-DOUIFLG
           MOVE    QUAREQ2-SIKKAN-TIME (IDXX)
                                   TO  ONSKAKU-SIKKAN-TIME
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       3                         
               MOVE    QUAREQ2-SIKKAN-SIPKBN     (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-SIPKBN    (IDZ)
               MOVE    QUAREQ2-SIKKAN-SKJGNSTYMD (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-SKJGNSTYMD(IDZ)
               MOVE    QUAREQ2-SIKKAN-TEKSTYMD   (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-TEKSTYMD  (IDZ)
               MOVE    QUAREQ2-SIKKAN-TEKEDYMD   (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-TEKEDYMD  (IDZ)
               MOVE    QUAREQ2-SIKKAN-FTNMONEY   (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-FTNMONEY  (IDZ)
           END-PERFORM                         
           MOVE    QUAREQ2-KENSIN-DOUIFLG (IDXX)
                                   TO  ONSKAKU-KENSIN-DOUIFLG 
           MOVE    QUAREQ2-KENSIN-TIME (IDXX)
                                   TO  ONSKAKU-KENSIN-TIME 
           MOVE    QUAREQ2-KENSIN-KIGENYMD (IDXX)
                                   TO  ONSKAKU-KENSIN-KIGENYMD 
           MOVE    QUAREQ2-YAKUZAI-DOUIFLG (IDXX)
                                   TO  ONSKAKU-YAKUZAI-DOUIFLG 
           MOVE    QUAREQ2-YAKUZAI-TIME (IDXX)
                                   TO  ONSKAKU-YAKUZAI-TIME 
           MOVE    QUAREQ2-YAKUZAI-KIGEN (IDXX)
                                   TO  ONSKAKU-YAKUZAI-KIGEN 
           MOVE    QUAREQ2-SHINRYO-DOUIFLG (IDXX)
                                   TO  ONSKAKU-SHINRYO-DOUIFLG
           MOVE    QUAREQ2-SHINRYO-TIME    (IDXX)
                                   TO  ONSKAKU-SHINRYO-TIME
           MOVE    QUAREQ2-SHINRYO-KIGEN   (IDXX)
                                   TO  ONSKAKU-SHINRYO-KIGEN
           MOVE    QUAREQ2-OPE-DOUIFLG (IDXX)
                                   TO  ONSKAKU-OPE-DOUIFLG
           MOVE    QUAREQ2-OPE-TIME    (IDXX)
                                   TO  ONSKAKU-OPE-TIME
           MOVE    QUAREQ2-OPE-KIGEN   (IDXX)
                                   TO  ONSKAKU-OPE-KIGEN
           .        
       200211-ONSKAKU-HENSYU-EXT.
           EXIT.
      * 
      ******************************************************************
      *    処理結果コードのチェックによる設定処理
      ******************************************************************
       2002111-ONSKAKU-ERR-HENSYU-SEC  SECTION.
      *
      *    オン資側で設定済なら終了
           IF      ONSKAKU-ERR-CODE
                                   NOT =   SPACE
               GO  TO  2002111-ONSKAKU-ERR-HENSYU-EXT
           END-IF
           IF      WRK-ERR-CODE        =   SPACE
               GO  TO  2002111-ONSKAKU-ERR-HENSYU-EXT
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
            END-EVALUATE
            .
       2002111-ONSKAKU-ERR-HENSYU-EXT.
           EXIT.
      * 
      ******************************************************************
      *    資格確認レコードの作成処理（枝番なしで複数のとき）
      ******************************************************************
       200212-01-ONSHI-KAKU-INSERT-SEC SECTION.
      * 
      *    UIDコード取得処理
           PERFORM 800-UID-GET-SEC
      *
      *    任意のファイル識別子(医療機関固有項目)の設定
           STRING  ","             DELIMITED  BY  SIZE  
                   C-UID           DELIMITED  BY  SPACE
                                   INTO       WRK-SIKIBETU
           END-STRING
      *
           IF      FLG-HKN-OK      =   SPACE
               MOVE    WRK-SIKIBETU    TO  ONSKAKU-FILE-SIKIBETU
           ELSE
               MOVE    QUAREQ2-FILE-SIKIBETU
                                       TO  ONSKAKU-FILE-SIKIBETU
           END-IF
           MOVE    C-UID           TO  ONSKAKU-TBL-UUID
           MOVE    HKN-ONSKAKU-TBL-UUID
                                   TO  ONSKAKU-OYA-UUID
           MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
      *
           MOVE    QUAREQ2-CARD-CLASS   (IDXX) 
                                   TO  ONSKAKU-CARD-CLASS 
           MOVE    QUAREQ2-RES-HKNJANUM (IDXX)
                                   TO  ONSKAKU-RES-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-KIGO (IDXX)
                                   TO  ONSKAKU-RES-KIGO
           MOVE    WRK-QUAREQ2-RES-NUM (IDXX)
                                   TO  ONSKAKU-RES-NUM 
           MOVE    QUAREQ2-RES-EDABAN (IDXX)
                                   TO  ONSKAKU-RES-EDABAN 
           MOVE    QUAREQ2-RES-HONKZKKBN (IDXX)
                                   TO  ONSKAKU-RES-HONKZKKBN 
           MOVE    WRK-QUAREQ2-RES-HIHKNJANAME (IDXX)
                                   TO  ONSKAKU-RES-HIHKNJANAME 
           MOVE    WRK-QUAREQ2-RES-NAME (IDXX)
                                   TO  ONSKAKU-RES-NAME 
           MOVE    WRK-QUAREQ2-RES-NAME-ETC (IDXX)
                                   TO  ONSKAKU-RES-NAME-ETC 
           MOVE    WRK-QUAREQ2-RES-KANANAME (IDXX)
                                   TO  ONSKAKU-RES-KANANAME 
           MOVE    WRK-QUAREQ2-RES-KANANAME-ETC (IDXX)
                                   TO  ONSKAKU-RES-KANANAME-ETC
           MOVE    QUAREQ2-RES-SEX1 (IDXX)
                                   TO  ONSKAKU-RES-SEX1 
           MOVE    QUAREQ2-RES-SEX2 (IDXX)
                                   TO  ONSKAKU-RES-SEX2 
           MOVE    QUAREQ2-RES-BIRTHDAY (IDXX)
                                   TO  ONSKAKU-RES-BIRTHDAY 
           MOVE    WRK-QUAREQ2-RES-ADRS (IDXX)
                                   TO  ONSKAKU-RES-ADRS 
           MOVE    QUAREQ2-RES-POST (IDXX)
                                   TO  ONSKAKU-RES-POST 
           IF      QUAREQ2-CARD-CLASS (IDXX)  =   "A1"
               CONTINUE
           ELSE
               MOVE    QUAREQ2-JOIN-SKKGETYMD (IDXX)
                                   TO  ONSKAKU-JOIN-SKKGETYMD
           END-IF
           MOVE    QUAREQ2-SKKGETYMD (IDXX)
                                   TO  ONSKAKU-SKKGETYMD 
           MOVE    QUAREQ2-TEKSTYMD (IDXX)
                                   TO  ONSKAKU-TEKSTYMD 
           MOVE    QUAREQ2-TEKEDYMD (IDXX)
                                   TO  ONSKAKU-TEKEDYMD 
           MOVE    QUAREQ2-FTNRATE (IDXX)
                                   TO  ONSKAKU-FTNRATE 
           MOVE    QUAREQ2-MISYUGAKU (IDXX)
                                   TO  ONSKAKU-MISYUGAKU 
           MOVE    QUAREQ2-LOSS-JIYU  (IDXX)
                                   TO  ONSKAKU-LOSS-JIYU 
           MOVE    WRK-QUAREQ2-HKNJANUM-NAME (IDXX)
                                   TO  ONSKAKU-HKNJANUM-NAME 
           MOVE    QUAREQ2-ELDER-SKKGETYMD (IDXX)
                                   TO  ONSKAKU-ELDER-SKKGETYMD 
           MOVE    QUAREQ2-ELDER-TEKSTYMD (IDXX)
                                   TO  ONSKAKU-ELDER-TEKSTYMD 
           MOVE    QUAREQ2-ELDER-TEKEDYMD (IDXX)
                                   TO  ONSKAKU-ELDER-TEKEDYMD 
           MOVE    QUAREQ2-ELDER-FTNRATE (IDXX)
                                   TO  ONSKAKU-ELDER-FTNRATE 
           MOVE    QUAREQ2-GENDO-DOUIFLG (IDXX)
                                   TO  ONSKAKU-GENDO-DOUIFLG 
           MOVE    QUAREQ2-GENDO-TIME (IDXX)
                                   TO  ONSKAKU-GENDO-TIME 
           MOVE    QUAREQ2-GENDO-SHOKBN (IDXX)
                                   TO  ONSKAKU-GENDO-SHOKBN 
           MOVE    QUAREQ2-GENDO-TEKKBN (IDXX)
                                   TO  ONSKAKU-GENDO-TEKKBN 
           MOVE    QUAREQ2-GENDO-SKKGETYMD (IDXX)
                                   TO  ONSKAKU-GENDO-SKKGETYMD 
           MOVE    QUAREQ2-GENDO-TEKSTYMD (IDXX)
                                   TO  ONSKAKU-GENDO-TEKSTYMD 
           MOVE    QUAREQ2-GENDO-TEKEDYMD (IDXX)
                                   TO  ONSKAKU-GENDO-TEKEDYMD 
           MOVE    QUAREQ2-GENDO-SKJGNSTYMD (IDXX)
                                   TO  ONSKAKU-GENDO-SKJGNSTYMD 
           MOVE    QUAREQ2-SIKKAN-TIME (IDXX)
                                   TO  ONSKAKU-SIKKAN-TIME
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       3                         
               MOVE    QUAREQ2-SIKKAN-SIPKBN     (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-SIPKBN    (IDZ)
               MOVE    QUAREQ2-SIKKAN-SKJGNSTYMD (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-SKJGNSTYMD(IDZ)
               MOVE    QUAREQ2-SIKKAN-TEKSTYMD   (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-TEKSTYMD  (IDZ)
               MOVE    QUAREQ2-SIKKAN-TEKEDYMD   (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-TEKEDYMD  (IDZ)
               MOVE    QUAREQ2-SIKKAN-FTNMONEY   (IDXX IDZ)
                                   TO  ONSKAKU-SIKKAN-FTNMONEY  (IDZ)
           END-PERFORM                         
           MOVE    QUAREQ2-KENSIN-DOUIFLG (IDXX)
                                   TO  ONSKAKU-KENSIN-DOUIFLG 
           MOVE    QUAREQ2-KENSIN-TIME (IDXX)
                                   TO  ONSKAKU-KENSIN-TIME 
           MOVE    QUAREQ2-KENSIN-KIGENYMD (IDXX)
                                   TO  ONSKAKU-KENSIN-KIGENYMD 
           MOVE    QUAREQ2-YAKUZAI-DOUIFLG (IDXX)
                                   TO  ONSKAKU-YAKUZAI-DOUIFLG 
           MOVE    QUAREQ2-YAKUZAI-TIME (IDXX)
                                   TO  ONSKAKU-YAKUZAI-TIME 
           MOVE    QUAREQ2-YAKUZAI-KIGEN (IDXX)
                                   TO  ONSKAKU-YAKUZAI-KIGEN 
           MOVE    QUAREQ2-SHINRYO-DOUIFLG (IDXX)
                                   TO  ONSKAKU-SHINRYO-DOUIFLG
           MOVE    QUAREQ2-SHINRYO-TIME    (IDXX)
                                   TO  ONSKAKU-SHINRYO-TIME
           MOVE    QUAREQ2-SHINRYO-KIGEN   (IDXX)
                                   TO  ONSKAKU-SHINRYO-KIGEN
           MOVE    QUAREQ2-OPE-DOUIFLG (IDXX)
                                   TO  ONSKAKU-OPE-DOUIFLG
           MOVE    QUAREQ2-OPE-TIME    (IDXX)
                                   TO  ONSKAKU-OPE-TIME
           MOVE    QUAREQ2-OPE-KIGEN   (IDXX)
                                   TO  ONSKAKU-OPE-KIGEN
      *****MOVE    QUAREQ2-FILE-SIKIBETU
      *****                         TO  ONSKAKU-SIKIBETU 
      *****MOVE    QUAREQ2-SHOKAI-NUM (IDXX)
      *****                        TO  ONSKAKU-SHOKAI-NUM
           IF    ( WRK-PATIENT-ID  NOT =   SPACE )
               IF    ( QUAREQ2-SHOKAI-NUM (IDXX)
                                       NOT =   SPACE          )
               AND   ( QUAREQ2-SHOKAI-NUM (IDXX)
                                       NOT =   WRK-PATIENT-ID )
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
               ELSE        
                   MOVE    QUAREQ2-SHOKAI-NUM (IDXX)
                                           TO  ONSKAKU-SHOKAI-NUM
               END-IF
           ELSE
               MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
           END-IF
      *                             
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
           MOVE    SPACE           TO  ONSKAKU-UPYMD
                                       ONSKAKU-UPHMS
      *    
      *    保険証ＯＣＲのときの公費処理
           IF      FLG-HKN-OK  NOT =   SPACE
               PERFORM VARYING IDWW    FROM    1   BY  1
                       UNTIL   IDWW    >       TAIHI-ONSKAKU-MAX
                   MOVE    TAIHI-ONSKAKU-REC (IDWW)
                                           TO  WRK-ONSKAKU-REC
                   MOVE    ONSKAKU-TBL-UUID
                                           TO WRK-ONSKAKU-KOUHI-UUID
      *            UIDコード取得処理
                   PERFORM 800-UID-GET-SEC
                   MOVE    C-UID           TO  WRK-ONSKAKU-TBL-UUID
      *                             
                   MOVE    SPA-OPID        TO  WRK-ONSKAKU-OPID
                   MOVE    SPA-TERMID      TO  WRK-ONSKAKU-TERMID
                   MOVE    SMCNDATE-YMD    TO  WRK-ONSKAKU-CREYMD
                   MOVE    SMCNDATE-HMS    TO  WRK-ONSKAKU-CREHMS
                   MOVE    SPACE           TO  WRK-ONSKAKU-UPYMD
                                               WRK-ONSKAKU-UPHMS
      *
                   MOVE    WRK-ONSKAKU-REC     TO  MCPDATA-REC
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
      *
                   DISPLAY "200212-ONSHI-KAKU-INSERT MCP-RC=" MCP-RC
                   IF      MCP-RC      NOT =   ZERO 
                       MOVE   "E10"             TO  WRK-ERRCD
                       DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC
                       GO  TO  200212-01-ONSHI-KAKU-INSERT-EXT
                   END-IF
               END-PERFORM
               MOVE    ONSKAKU-TBL-UUID    TO  ONSKAKU-KOUHI-UUID
           END-IF 
      * 
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           DISPLAY "200212-ONSHI-KAKU-INSERT MCP-RC=" MCP-RC
           IF      MCP-RC      NOT =   ZERO 
               MOVE   "E11"             TO  WRK-ERRCD
               DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC
           ELSE
      *        処方管理レコード作成処理
               MOVE    ONSKAKU-REC      TO  SHOHO-ONSKAKU-REC
               PERFORM 200225-SHOHO-KANRI-INSERT-SEC
           END-IF
           .
       200212-01-ONSHI-KAKU-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認レコードの更新処理（保険証）
      *****************************************************************
       200213-ONSHI-KAKU-UPDATE-SEC SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-UPYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-UPHMS
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *??
           DISPLAY "200213-ONSHI-KAKU-UPDATE MCP-RC=" MCP-RC
           IF      MCP-RC      NOT =   ZERO
               display "tbl_onshi_kaku update error" 
               MOVE    "E12"           TO  WRK-ERRCD
           ELSE
      *        処方管理レコード処理
               MOVE    ONSKAKU-REC      TO  SHOHO-ONSKAKU-REC
               PERFORM 2002131-SHOHO-KANRI-SYORI-SEC
           END-IF
      *
           .
       200213-ONSHI-KAKU-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *     処方管理レコード処理
      *****************************************************************
       2002131-SHOHO-KANRI-SYORI-SEC   SECTION.
      *
      *    電子処方箋区分が１以外は作成しない
           IF      SYS-1051-DENSHISHOHO
                               NOT =   "1"
               GO  TO  2002131-SHOHO-KANRI-SYORI-EXT
           END-IF
      *
           IF      SHOHO-ONSKAKU-SHOKAI-KBN
                                   =   "1" OR  "2"
               CONTINUE
           ELSE
               GO  TO  2002131-SHOHO-KANRI-SYORI-EXT
           END-IF
      *
           INITIALIZE                  SHOHO-KANRI-REC
           MOVE    SPA-HOSPNUM     TO  SHOKANR-HOSPNUM
           MOVE    SHOHO-ONSKAKU-TBL-UUID
                                   TO  SHOKANR-TBL-UUID
           MOVE    1               TO  SHOKANR-RENNUM
           MOVE    SHOHO-KANRI-REC TO  MCPDATA-REC
           PERFORM 900-SHOHO-KANRI-READ-SEC
      *
           IF      FLG-SHOHO-KANRI =   ZERO
               IF      SHOKANR-PTID    =   ZERO 
                   MOVE    SHOHO-ONSKAKU-PTID
                                           TO  SHOKANR-PTID
               END-IF
               MOVE    SHOHO-ONSKAKU-SHO-SHOHO-KEITAI
                                       TO  SHOKANR-SHOHO-KEITAI
               MOVE    SMCNDATE-YMD    TO  SHOKANR-UPYMD
               MOVE    SMCNDATE-HMS    TO  SHOKANR-UPHMS
               MOVE    SHOHO-KANRI-REC TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_shoho_kanri"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   display "tbl_shoho_kanri insert error" 
                   MOVE    "E19"           TO  WRK-ERRCD
               END-IF
           ELSE
               PERFORM 200225-SHOHO-KANRI-INSERT-SEC
           END-IF
           .
       2002131-SHOHO-KANRI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険証資格確認登録、更新処理（ひとつの保険が返却されたとき ）
      *****************************************************************
       20022-SYORI-02-SEC  SECTION.
      *
           MOVE    SPACE           TO  QUARES1-QUARES1
           MOVE    ZERO            TO  FLG-SHOKAI-NUM
      *     
      *    ＵＩＤから保険証の資格確認レコードを検索
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
               MOVE    1               TO  IDXX                        
               PERFORM 200221-SYORI-02-HKN-SEC
           ELSE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       50
                       OR      QUAREQ2-CARD-CLASS (IDXX)
                                       =       SPACE
                   IF      QUAREQ2-CARD-CLASS (IDXX)
                                           =   "A1"
                       PERFORM 200221-SYORI-02-FUJYO-SEC
                   ELSE
                       PERFORM 200221-SYORI-02-HKN-SEC
                   END-IF
               END-PERFORM
           END-IF 
      *
           .
       20022-SYORI-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険証資格確認登録、更新処理（ひとつの保険が返却されたとき）
      *    保険の資格確認登録
      *****************************************************************
       200221-SYORI-02-HKN-SEC  SECTION.
      *
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    WRK-UUID        TO  ONSKAKU-TBL-UUID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           MOVE    "2"             TO  WRK-SHOKAI-KBN
           PERFORM 900-ONSHI-KAKU-READ-SEC
           IF      FLG-ONSHI-KAKU  =   ZERO
               MOVE    ONSKAKU-REC     TO  UUID-ONSKAKU-REC
      *
               IF      ONSKAKU-FUJYO-HEIYO
                                   NOT =   "1"
                   CONTINUE
               ELSE    
      *            当日分でptid、fujyo_uuid1が一致する資格確認レコードを検索
                   INITIALIZE                  HEIYO-ONSKAKU-REC
                   MOVE    SPA-HOSPNUM     TO  HEIYO-ONSKAKU-HOSPNUM
                   MOVE    ONSKAKU-PTID    TO  HEIYO-ONSKAKU-PTID
                   MOVE    ONSKAKU-FUJYO-UUID1
                                           TO  HEIYO-ONSKAKU-FUJYO-UUID1
                   MOVE    HEIYO-ONSKAKU-REC
                                           TO  MCPDATA-REC
                   PERFORM 900-ONSHI-KAKU-READ-KEY29-SEC
                   IF      TAIHI-HEI-ONSKAKU-MAX
                                           >   ZERO
      *                生保のkouhi_uuid に主保険のtbl_uuid を設定
                       MOVE    TAIHI-HEI-ONSKAKU-REC (1)
                                               TO  HEIYO-ONSKAKU-REC
                       MOVE    ONSKAKU-TBL-UUID
                                               TO
                                               HEIYO-ONSKAKU-KOUHI-UUID
                       MOVE    SMCNDATE-YMD    TO  HEIYO-ONSKAKU-UPYMD
                       MOVE    SMCNDATE-HMS    TO  HEIYO-ONSKAKU-UPHMS
                       MOVE    HEIYO-ONSKAKU-REC
                                               TO  MCPDATA-REC
                       MOVE    "DBUPDATE"      TO  MCP-FUNC
                       MOVE    "tbl_onshi_kaku"
                                               TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC      NOT =   ZERO
                           display "tbl_onshi_kaku update error"
                           MOVE    "E14"           TO  WRK-ERRCD
                           GO  TO  200221-SYORI-02-HKN-EXT 
                       END-IF
                   END-IF        
               END-IF
      *
      *        MOVE    "02"            TO  ONSKAKU-KAKU-ENDFLG
               IF      ONSKAKU-RESULT-KBN
                                       =   "2"
                   MOVE    ONSKAKU-RESULT-KBN
                                           TO  ONSKAKU-ORCA-ERR-CODE
               END-IF
      *        資格確認編集処理
      *********MOVE    1               TO  IDXX
      *********IF      QUAREQ2-CARD-CLASS (2)
      *********                        =   SPACE
                   MOVE    "02"        TO  ONSKAKU-KAKU-ENDFLG
      *********ELSE     
      *********    MOVE    "03"        TO  ONSKAKU-KAKU-ENDFLG
      *********END-IF
      *
               PERFORM 200211-ONSKAKU-HENSYU-SEC
      *        併用で生保レコードが存在するとき
               IF      TAIHI-HEI-ONSKAKU-MAX
                                       >   ZERO
      *            生保レコードのfujyo_uuid を主保険のfujyo_uuidに設定 
                   MOVE    TAIHI-H-ONSKAKU-FUJYO-UUID (1)
                                           TO  ONSKAKU-FUJYO-UUID
               END-IF
               MOVE    SPACE           TO  ONSKAKU-KOUHI-UUID
      *        資格確認レコードの更新処理（保険証）
               PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
               MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
      *
               IF      QUAREQ2-SIKAKU-YUKO =   "1" OR  "2" OR  "3"
      *            マシン日付と一致する最新の顔認証資格確認レコードを検索
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
                   MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                   MOVE    "key8"          TO  WRK-MCP-PATHNAME
                   MOVE    "1"             TO  WRK-SHOKAI-KBN
                   PERFORM 900-ONSHI-KAKU-READ-SEC
                   IF      FLG-ONSHI-KAKU  =   ZERO
                       IF    ( SMCNDATE-YMD    =
                                               ONSKAKU-KAKU-KAKUNINYMD )
                       AND   ( HKN-ONSKAKU-RES-HKNJANUM
                                               =   ONSKAKU-RES-HKNJANUM)
                       AND   ( HKN-ONSKAKU-RES-KIGO
                                               =   ONSKAKU-RES-KIGO    )
                       AND   ( HKN-ONSKAKU-RES-NUM
                                               =   ONSKAKU-RES-NUM     )
                       AND   ( HKN-ONSKAKU-RES-EDABAN
                                               =   ONSKAKU-RES-EDABAN  )
                       AND   ( HKN-ONSKAKU-RES-BIRTHDAY
                                               =   ONSKAKU-RES-BIRTHDAY)
      *1-1-1                                         
                           IF      ONSKAKU-PTID    =   ZERO
                               MOVE    HKN-ONSKAKU-PTID
                                                       TO  ONSKAKU-PTID
                           END-IF                            
      *1-1-2
                           IF      ONSKAKU-SHOKAI-NUM  =   SPACE
      *                        照会番号、薬剤、特定検診の処理（顔認証）
                               PERFORM 200223-RES1-FACE-HENSYU-SEC
                           ELSE
      *1-1-3                     
                               INITIALIZE                  ORCSPTIDAREA
                               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                               MOVE    ONSKAKU-SHOKAI-NUM
                                                       TO  SPTID-PTNUM
                               CALL    "ORCSPTID"      USING
                                                       ORCSPTIDAREA
                                                       SPA-AREA
                               IF      SPTID-RC        =   00
                                   IF      SPTID-PTID
                                               NOT =   HKN-ONSKAKU-PTID
                                       MOVE    1   TO  FLG-SHOKAI-NUM
                                   END-IF    
      *                            照会番号、薬剤、特定検診の処理（保険証）
                                   PERFORM 200222-RES1-HKN-HENSYU-SEC
                               END-IF
                           END-IF 
      *                    資格確認レコードの更新処理（顔認証）
                           PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
                       ELSE
      *1-2-3
                           IF      HKN-ONSKAKU-SIKAKU-YUKO
                                               =   "1" OR  "2" OR  "3"
                               IF      HKN-ONSKAKU-SHOKAI-NUM
                                               NOT =   SPACE
                                   INITIALIZE              ORCSPTIDAREA
                                   MOVE    SPA-HOSPNUM TO  SPTID-HOSPNUM
                                   MOVE    HKN-ONSKAKU-SHOKAI-NUM
                                                       TO  SPTID-PTNUM
                                   CALL    "ORCSPTID"  USING
                                                           ORCSPTIDAREA
                                                           SPA-AREA
                                   IF      SPTID-RC    =   00
                                       IF      SPTID-PTID
                                               NOT =   HKN-ONSKAKU-PTID
                                           MOVE    1   TO
                                                       FLG-SHOKAI-NUM
                                       END-IF
                                   END-IF
                               ELSE
                                    MOVE    1      TO  FLG-SHOKAI-NUM
                               END-IF            
      *                        照会番号、薬剤、特定検診の処理（保険証）
                               PERFORM 200222-RES1-HKN-HENSYU-SEC
                           ELSE      
                               DISPLAY "HKN-ONSKAKU-SIKAKU-YUKO NOT 123"
                               MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                               TO  QUARES1-PATIENT-ID
                           END-IF
                       END-IF
                   ELSE
      *1-2-3
                       IF      HKN-ONSKAKU-SIKAKU-YUKO
                                               =   "1" OR  "2" OR  "3"
                           IF      HKN-ONSKAKU-SHOKAI-NUM  NOT =   SPACE
                               INITIALIZE              ORCSPTIDAREA
                               MOVE    SPA-HOSPNUM TO  SPTID-HOSPNUM
                               MOVE    HKN-ONSKAKU-SHOKAI-NUM
                                                   TO  SPTID-PTNUM
                               CALL    "ORCSPTID"  USING
                                                           ORCSPTIDAREA
                                                           SPA-AREA
                               IF      SPTID-RC    =   00
                                   IF      SPTID-PTID
                                               NOT =   HKN-ONSKAKU-PTID
                                       MOVE    1   TO  FLG-SHOKAI-NUM
                                   END-IF
                               END-IF
                           ELSE
                                MOVE    1      TO  FLG-SHOKAI-NUM
                           END-IF            
      *                    照会番号、薬剤、特定検診の処理（保険証）
                           PERFORM 200222-RES1-HKN-HENSYU-SEC
                       ELSE      
                           DISPLAY "HKN-ONSKAKU-SIKAKU-YUKO NOT 123"
                           MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                           TO  QUARES1-PATIENT-ID
                       END-IF
                    END-IF
               END-IF
           ELSE      
               DISPLAY "HKN ONSKAU NOT FOUND"
               MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                           TO  QUARES1-PATIENT-ID
           END-IF
      *
      *    複数の保険者が設定されている場合
      *****IF      QUAREQ2-RES-HKNJANUM (2)
      *****                        NOT =   SPACE
      *****    PERFORM VARYING IDX FROM    2   BY  1
      *****            UNTIL   IDX >       50
      *****            OR      QUAREQ2-RES-HKNJANUM (IDX)  =   SPACE
      *****        INITIALIZE                  ONSKAKU-REC
      *****        IF      ONSKAKU-ORCA-ERR-CODE
      *****                                =   SPACE
      *****            MOVE    "10"            TO  ONSKAKU-ORCA-ERR-CODE
      *****        END-IF        
      *            資格確認レコードの作成処理（保険証）
      *****        PERFORM 200221-02-ONSHI-KAKU-INSERT-SEC
      *****    END-PERFORM
      *****END-IF
      *
           .
       200221-SYORI-02-HKN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険証資格確認登録、更新処理（ひとつの保険が返却されたとき）
      *    医療扶助の資格確認登録
      *****************************************************************
       200221-SYORI-02-FUJYO-SEC  SECTION.
      *
           IF      WRK-QUAREQ2-RESULT-MAX
                                   =   1
      *        医療扶助単独の時                             
                INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    WRK-UUID        TO  ONSKAKU-TBL-UUID
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               MOVE    "key"           TO  WRK-MCP-PATHNAME
               MOVE    "2"             TO  WRK-SHOKAI-KBN
               PERFORM 900-ONSHI-KAKU-READ-SEC
               IF      FLG-ONSHI-KAKU  =   ZERO
                   MOVE    ONSKAKU-REC     TO  UUID-ONSKAKU-REC
      *
                   IF      ONSKAKU-FUJYO-HEIYO
                                       NOT =   "1"
                       CONTINUE
                   ELSE    
      *                当日分でptid、fujyo_uuid1が一致する保険の資格確認レコードを検索
                       INITIALIZE                  HEIYO-ONSKAKU-REC
                       MOVE    SPA-HOSPNUM     TO  HEIYO-ONSKAKU-HOSPNUM
                       MOVE    ONSKAKU-PTID    TO  HEIYO-ONSKAKU-PTID
                       MOVE    ONSKAKU-FUJYO-UUID1
                                               TO
                                               HEIYO-ONSKAKU-FUJYO-UUID1
                       MOVE    HEIYO-ONSKAKU-REC
                                               TO  MCPDATA-REC
                       PERFORM 900-ONSHI-KAKU-READ-KEY29-SEC
                       IF      TAIHI-HEI-ONSKAKU-MAX
                                               >   ZERO
                           IF      FLG-TAIHI-H-SIKAKU-YUKO
                                               =   1
      *                        複数該当の場合
      *                        当日分でptid、oya_uuidが一致する保険の資格確認レコードを検索
                               INITIALIZE              HEIYO-ONSKAKU-REC
                               MOVE    SPA-HOSPNUM TO
                                               HEIYO-ONSKAKU-HOSPNUM
                               MOVE    TAIHI-H-ONSKAKU-PTID     (1)
                                                   TO
                                               HEIYO-ONSKAKU-PTID
                               MOVE    TAIHI-H-ONSKAKU-OYA-UUID (1)
                                                   TO
                                               HEIYO-ONSKAKU-OYA-UUID
                               MOVE    HEIYO-ONSKAKU-REC
                                                   TO  MCPDATA-REC
                               PERFORM 900-ONSHI-KAKU-READ-KEY30-SEC
                               IF      TAIHI-HEI-ONSKAKU-MAX
                                                   >   ZERO
                                   PERFORM
                                       2002211-SYORI-02-FUJYO-HEI-SEC
                                   GO  TO  200221-SYORI-02-FUJYO-EXT
                               END-IF   
                           ELSE
      *                        複数該当以外の場合
      *                        主保険レコードをリクエストのfujyo_uuidで更新する
                               MOVE    TAIHI-HEI-ONSKAKU-REC (1)
                                               TO  HEIYO-ONSKAKU-REC 
                               MOVE    QUAREQ2-FUJYO-UUID
                                               TO
                                               HEIYO-ONSKAKU-FUJYO-UUID
                               MOVE    SMCNDATE-YMD
                                               TO  HEIYO-ONSKAKU-UPYMD
                               MOVE    SMCNDATE-HMS
                                               TO HEIYO-ONSKAKU-UPHMS
                               MOVE    HEIYO-ONSKAKU-REC
                                               TO  MCPDATA-REC
                               MOVE    "DBUPDATE"  TO  MCP-FUNC
                               MOVE    "tbl_onshi_kaku"
                                               TO  MCP-TABLE
                               MOVE    "key"       TO  MCP-PATHNAME
                               PERFORM 900-ORCDBMAIN-SEC
                               IF      MCP-RC      NOT =   ZERO
                                   display "tbl_onshi_kaku update error"
                                    MOVE    "E14"       TO  WRK-ERRCD
                                   GO  TO  200221-SYORI-02-FUJYO-EXT 
                               END-IF
                           END-IF    
                       END-IF        
                   END-IF
      *
                   IF      ONSKAKU-RESULT-KBN
                                           =   "2"
                       MOVE    ONSKAKU-RESULT-KBN
                                           TO  ONSKAKU-ORCA-ERR-CODE
                   END-IF
      *            資格確認編集処理
                   MOVE    "02"        TO  ONSKAKU-KAKU-ENDFLG
      *
                   PERFORM 200211-ONSKAKU-HENSYU-SEC
      *            併用で主保険レコードが存在するとき
                   IF      TAIHI-HEI-ONSKAKU-MAX
                                           >   ZERO
      *                 kouhi_uuid に主保険のtbl_uuid を設定
                        MOVE    TAIHI-H-ONSKAKU-TBL-UUID   (1)
                                               TO  ONSKAKU-KOUHI-UUID 
                   END-IF
      *            資格確認レコードの更新処理（保険証）
                   PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
                   MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
      *
                   IF      QUAREQ2-SIKAKU-YUKO =   "1" OR  "2" OR  "3"
      *                マシン日付と一致する最新の顔認証資格確認レコードを検索
                       INITIALIZE                  ONSKAKU-REC
                       MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
                       MOVE    SMCNDATE-YMD    TO
                                               ONSKAKU-KAKU-KAKUNINYMD
                       MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                       MOVE    "key8"          TO  WRK-MCP-PATHNAME
                       MOVE    "1"             TO  WRK-SHOKAI-KBN
                       PERFORM 900-ONSHI-KAKU-READ-SEC
                       IF      FLG-ONSHI-KAKU  =   ZERO
                           IF    ( SMCNDATE-YMD    =
                                               ONSKAKU-KAKU-KAKUNINYMD )
                           AND   ( HKN-ONSKAKU-RES-HKNJANUM
                                               =   ONSKAKU-RES-HKNJANUM)
                           AND   ( HKN-ONSKAKU-RES-NUM
                                               =   ONSKAKU-RES-NUM     )
                           AND   ( HKN-ONSKAKU-RES-BIRTHDAY
                                               =   ONSKAKU-RES-BIRTHDAY)
                               IF      ONSKAKU-PTID    =   ZERO
                                   MOVE    HKN-ONSKAKU-PTID
                                                       TO  ONSKAKU-PTID
                               END-IF                            
                               IF      ONSKAKU-SHOKAI-NUM  =   SPACE
      *                           照会番号、薬剤、特定検診の処理（顔認証）
                                   PERFORM 200223-RES1-FACE-HENSYU-SEC
                               ELSE
                                   INITIALIZE              ORCSPTIDAREA
                                   MOVE    SPA-HOSPNUM TO  SPTID-HOSPNUM
                                   MOVE    ONSKAKU-SHOKAI-NUM
                                                       TO  SPTID-PTNUM
                                   CALL    "ORCSPTID"      USING
                                                           ORCSPTIDAREA
                                                           SPA-AREA
                                   IF      SPTID-RC        =   00
                                       IF      SPTID-PTID
                                               NOT =   HKN-ONSKAKU-PTID
                                           MOVE    1   TO FLG-SHOKAI-NUM
                                       END-IF    
      *                                照会番号、薬剤、特定検診の処理（保険証）
                                       PERFORM
                                           200222-RES1-HKN-HENSYU-SEC
                                   END-IF
                               END-IF 
      *                        資格確認レコードの更新処理（顔認証）
                               PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
                           ELSE
      *1-2-3
                               IF      HKN-ONSKAKU-SIKAKU-YUKO
                                               =   "1" OR  "2" OR  "3"
                                   IF      HKN-ONSKAKU-SHOKAI-NUM
                                               NOT =   SPACE
                                       INITIALIZE          ORCSPTIDAREA
                                       MOVE    SPA-HOSPNUM
                                                       TO  SPTID-HOSPNUM
                                       MOVE    HKN-ONSKAKU-SHOKAI-NUM
                                                       TO  SPTID-PTNUM
                                       CALL    "ORCSPTID"  USING
                                                           ORCSPTIDAREA
                                                           SPA-AREA
                                       IF      SPTID-RC    =   00
                                           IF      SPTID-PTID
                                               NOT =   HKN-ONSKAKU-PTID
                                               MOVE    1   TO
                                                       FLG-SHOKAI-NUM
                                           END-IF
                                       END-IF
                                   ELSE
                                        MOVE    1      TO FLG-SHOKAI-NUM
                                   END-IF            
      *                            照会番号、薬剤、特定検診の処理（保険証）
                                   PERFORM 200222-RES1-HKN-HENSYU-SEC
                               ELSE      
                                   DISPLAY
                                    "HKN-ONSKAKU-SIKAKU-YUKO NOT 123"
                                   MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                               TO  QUARES1-PATIENT-ID
                               END-IF
                           END-IF
                       ELSE
      *1-2-3
                           IF      HKN-ONSKAKU-SIKAKU-YUKO
                                               =   "1" OR  "2" OR  "3"
                               IF      HKN-ONSKAKU-SHOKAI-NUM
                                                       NOT =   SPACE
                                   INITIALIZE              ORCSPTIDAREA
                                   MOVE    SPA-HOSPNUM TO  SPTID-HOSPNUM
                                   MOVE    HKN-ONSKAKU-SHOKAI-NUM
                                                       TO  SPTID-PTNUM
                                   CALL    "ORCSPTID"  USING
                                                           ORCSPTIDAREA
                                                           SPA-AREA
                                   IF      SPTID-RC    =   00
                                       IF      SPTID-PTID
                                               NOT =   HKN-ONSKAKU-PTID
                                           MOVE    1   TO FLG-SHOKAI-NUM
                                       END-IF
                                   END-IF
                               ELSE
                                    MOVE    1      TO  FLG-SHOKAI-NUM
                               END-IF            
      *                        照会番号、薬剤、特定検診の処理（保険証）
                               PERFORM 200222-RES1-HKN-HENSYU-SEC
                           ELSE      
                               DISPLAY "HKN-ONSKAKU-SIKAKU-YUKO NOT 123"
                               MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                               TO  QUARES1-PATIENT-ID
                           END-IF
                        END-IF
                   END-IF
               ELSE      
                   DISPLAY "HKN ONSKAU NOT FOUND"
                   MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                               TO  QUARES1-PATIENT-ID
               END-IF
           ELSE
      *        保険と医療扶助併用の時                             
      *        UIDコード取得処理
               PERFORM 800-UID-GET-SEC
      *        任意のファイル識別子(医療機関固有項目)の設定
               STRING  WRK-PTID        DELIMITED  BY  SIZE
                       ","             DELIMITED  BY  SIZE  
                       C-UID           DELIMITED  BY  SIZE
                                       INTO       WRK-SIKIBETU
               END-STRING
      *    
      *        資格確認レコードの編集処理（顔認証）
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    C-UID           TO  ONSKAKU-TBL-UUID
               MOVE    QUAREQ2-FUJYO-UUID
                                       TO  ONSKAKU-FUJYO-UUID
               MOVE    HKN-ONSKAKU-TBL-UUID
                                       TO  ONSKAKU-KOUHI-UUID
      *??      MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
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
      *****    MOVE    QUAREQ2-FILE-SIKIBETU
               MOVE    WRK-SIKIBETU    TO  ONSKAKU-FILE-SIKIBETU
               MOVE    QUAREQ2-SHORI-TIME
                                       TO  ONSKAKU-SHORI-TIME 
               MOVE    QUAREQ2-KAKUNINYMD
                                       TO  ONSKAKU-KAKUNINYMD
               MOVE    QUAREQ2-HOSPCD  TO  ONSKAKU-HOSPCD
               MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
               MOVE    QUAREQ2-RESULT-KBN
                                       TO  ONSKAKU-RESULT-KBN
               MOVE    QUAREQ2-ERR-CODE
                                       TO  ONSKAKU-ERR-CODE
               MOVE    QUAREQ2-ERR-MSG TO  ONSKAKU-ERR-MSG
      *         IF    (  ONSKAKU-ERR-CODE 
      *                                 =   SPACE )
      *         AND   ( WRK-QUAREQ2-ERR-CODE (IDXX)
      *                             NOT =   SPACE )
      *             MOVE    WRK-QUAREQ2-ERR-CODE (IDXX)
      *                                     TO  WRK-ERR-CODE
      *             MOVE    WRK-QUAREQ2-ERR-PTID (IDXX)
      *                                     TO  WRK-ERR-PTID
      *         END-IF 
      *         PERFORM 2002211-ONSKAKU-ERR-HENSYU-SEC
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
               MOVE    QUAREQ2-CARD-CLASS   (IDXX) 
                                   TO  ONSKAKU-CARD-CLASS 
               MOVE    QUAREQ2-RES-HKNJANUM (IDXX)
                                   TO  ONSKAKU-RES-HKNJANUM
               MOVE    WRK-QUAREQ2-RES-NUM (IDXX)
                                   TO  ONSKAKU-RES-NUM 
               MOVE    WRK-QUAREQ2-RES-NAME (IDXX)
                                   TO  ONSKAKU-RES-NAME 
               MOVE    WRK-QUAREQ2-RES-NAME-ETC (IDXX)
                                   TO  ONSKAKU-RES-NAME-ETC 
               MOVE    WRK-QUAREQ2-RES-KANANAME (IDXX)
                                   TO  ONSKAKU-RES-KANANAME 
               MOVE    WRK-QUAREQ2-RES-KANANAME-ETC (IDXX)
                                   TO  ONSKAKU-RES-KANANAME-ETC
               MOVE    QUAREQ2-RES-SEX1 (IDXX)
                                   TO  ONSKAKU-RES-SEX1 
               MOVE    QUAREQ2-RES-SEX2 (IDXX)
                                   TO  ONSKAKU-RES-SEX2 
               MOVE    QUAREQ2-RES-BIRTHDAY (IDXX)
                                   TO  ONSKAKU-RES-BIRTHDAY 
               MOVE    WRK-QUAREQ2-RES-ADRS (IDXX)
                                   TO  ONSKAKU-RES-ADRS 
               MOVE    QUAREQ2-RES-POST (IDXX)
                                   TO  ONSKAKU-RES-POST 
               MOVE    QUAREQ2-JOIN-SKKGETYMD (IDXX)
                                  TO  ONSKAKU-KOH-TEKSTYMD (1)
               MOVE    QUAREQ2-JOIN-SKKLOSSYMD (IDXX)
                                  TO  ONSKAKU-KOH-TEKEDYMD (1)
               MOVE    QUAREQ2-LOSS-JIYU  (IDXX)
                                   TO  ONSKAKU-LOSS-JIYU 
               MOVE    WRK-QUAREQ2-HKNJANUM-NAME (IDXX)
                                   TO  ONSKAKU-HKNJANUM-NAME 
               MOVE    QUAREQ2-KENSIN-DOUIFLG (IDXX)
                                   TO  ONSKAKU-KENSIN-DOUIFLG 
               MOVE    QUAREQ2-KENSIN-TIME (IDXX)
                                   TO  ONSKAKU-KENSIN-TIME 
               MOVE    QUAREQ2-KENSIN-KIGENYMD (IDXX)
                                   TO  ONSKAKU-KENSIN-KIGENYMD 
               MOVE    QUAREQ2-YAKUZAI-DOUIFLG (IDXX)
                                   TO  ONSKAKU-YAKUZAI-DOUIFLG 
               MOVE    QUAREQ2-YAKUZAI-TIME (IDXX)
                                   TO  ONSKAKU-YAKUZAI-TIME 
               MOVE    QUAREQ2-YAKUZAI-KIGEN (IDXX)
                                   TO  ONSKAKU-YAKUZAI-KIGEN 
               MOVE    QUAREQ2-SIKIBETU (IDXX)
                                   TO  ONSKAKU-SIKIBETU 
               MOVE    QUAREQ2-SHINRYO-DOUIFLG (IDXX)
                                   TO  ONSKAKU-SHINRYO-DOUIFLG
               MOVE    QUAREQ2-SHINRYO-TIME    (IDXX)
                                   TO  ONSKAKU-SHINRYO-TIME
               MOVE    QUAREQ2-SHINRYO-KIGEN   (IDXX)
                                   TO  ONSKAKU-SHINRYO-KIGEN
               MOVE    QUAREQ2-OPE-DOUIFLG (IDXX)
                                   TO  ONSKAKU-OPE-DOUIFLG
               MOVE    QUAREQ2-OPE-TIME    (IDXX)
                                   TO  ONSKAKU-OPE-TIME
               MOVE    QUAREQ2-OPE-KIGEN   (IDXX)
                                   TO  ONSKAKU-OPE-KIGEN
               IF    ( WRK-PATIENT-ID  NOT =   SPACE )
                   IF    ( QUAREQ2-SHOKAI-NUM (IDXX)
                                       NOT =   SPACE          )
                   AND   ( QUAREQ2-SHOKAI-NUM (IDXX)
                                       NOT =   WRK-PATIENT-ID )
                        MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
                   ELSE        
                       MOVE    QUAREQ2-SHOKAI-NUM (IDXX)
                                           TO  ONSKAKU-SHOKAI-NUM
                   END-IF
               ELSE
                   MOVE    SPACE           TO  ONSKAKU-SHOKAI-NUM
               END-IF
      *                             
               MOVE    SPA-OPID        TO  ONSKAKU-OPID
               MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
               MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
               MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
      * 
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC      NOT =   ZERO 
                   MOVE   "E16"             TO  WRK-ERRCD
                   DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC 
               END-IF
           END-IF    
      *
           .
       200221-SYORI-02-FUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険証資格確認登録、更新処理（ひとつの保険が返却されたとき）
      *     医療扶助併用のとき 
      *****************************************************************
       2002211-SYORI-02-FUJYO-HEI-SEC  SECTION.
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       TAIHI-HEI-ONSKAKU-MAX
               MOVE    TAIHI-HEI-ONSKAKU-REC (IDZZ)
                                           TO  ONSKAKU-REC
      *        UIDコード取得処理
               PERFORM 800-UID-GET-SEC
               MOVE    C-UID               TO  ONSKAKU-FUJYO-UUID
      *        資格確認レコードの更新処理（保険証）
               PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
               MOVE    ONSKAKU-REC         TO  HKN-ONSKAKU-REC
      *
      *        生保の資格確認レコードの更新処理
               INITIALIZE                      ONSKAKU-REC
               MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
               MOVE    HKN-ONSKAKU-TBL-UUID
                                           TO  ONSKAKU-KOUHI-UUID
               MOVE    "2"                 TO  WRK-SHOKAI-KBN
               MOVE    ONSKAKU-REC         TO  MCPDATA-REC
               MOVE    "key13"             TO  WRK-MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-SEC
               IF      FLG-ONSHI-KAKU      =   ZERO
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       1
                           OR      QUAREQ2-CARD-CLASS (IDXX)
                                           =       SPACE
                       MOVE    SPACE           TO  QUARES1-QUARES1
      *
      *                資格確認編集処理
                       MOVE    "02"            TO  ONSKAKU-KAKU-ENDFLG
                       PERFORM 200211-ONSKAKU-HENSYU-SEC
                       MOVE    HKN-ONSKAKU-FUJYO-UUID
                                               TO  ONSKAKU-FUJYO-UUID
      *                資格確認レコードの更新処理（保険証）
                       PERFORM 200213-ONSHI-KAKU-UPDATE-SEC
                    END-PERFORM
                END-IF
           END-PERFORM
      *
           .
       2002211-SYORI-02-FUJYO-HEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険証資格確認登録、更新処理（保険証ＯＣＲでひとつの保険が返却されたとき ）
      *****************************************************************
       20023-SYORI-03-SEC  SECTION.
      *
           MOVE    SPACE           TO  QUARES1-QUARES1
           MOVE    ZERO            TO  FLG-SHOKAI-NUM
      *
           IF    ( QUAREQ2-SIKAKU-YUKO
                                   =   "3" OR  "4" )
           AND   ( QUAREQ2-CARD-CLASS (1)
                                   =   SPACE       )
               MOVE    1               TO  IDXX                        
               PERFORM 200224-HKNOCR-HENSYU-SEC
      *
               MOVE    KOH-ONSKAKU-REC     TO  HKN-ONSKAKU-REC
           ELSE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       50
                       OR      QUAREQ2-CARD-CLASS (IDXX)
                                       =       SPACE
      *********    MOVE    SPACE           TO  QUARES1-QUARES1
      *
      *            保険証ＯＣＲ資格確認登録、更新処理
      *********    MOVE    1               TO  IDXX
                   PERFORM 200224-HKNOCR-HENSYU-SEC
      *
                   MOVE    KOH-ONSKAKU-REC     TO  HKN-ONSKAKU-REC
      *****        複数の保険者が設定されている場合
      *            IF      QUAREQ2-RES-HKNJANUM (2)
      *                                    NOT =   SPACE
      *                PERFORM VARYING IDX FROM    2   BY  1
      *                        UNTIL   IDX >       50
      *                    OR      QUAREQ2-RES-HKNJANUM (IDX)  =   SPACE
      *                    INITIALIZE                  ONSKAKU-REC
      *                    IF      ONSKAKU-ORCA-ERR-CODE
      *                                        =   SPACE
      *                        MOVE    "10"    TO  ONSKAKU-ORCA-ERR-CODE
      *                    END-IF        
      *                    資格確認レコードの作成処理（保険証）
      *                    PERFORM 200221-02-ONSHI-KAKU-INSERT-SEC
      *                 END-PERFORM
      *            END-IF
               END-PERFORM
           END-IF    
      *
           .
       20023-SYORI-03-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険証ＯＣＲ資格確認登録処理
      ******************************************************************
       200224-HKNOCR-HENSYU-SEC     SECTION.
      *
           IF      QUAREQ2-CARD-CLASS (IDX)
                                   =   "A1"
             CONTINUE
           ELSE                              
             IF      QUAREQ2-SHOKAI-NUM (IDXX)
                                   =   SPACE
      *        保険情報から患者保険情報を検索
               IF      FLG-HKN-OK      =   "OK"
                   PERFORM 2002241-PTHKNINF-TAIHI-SEC
               END-IF
      *
               IF      WRK-PTHKNINF-MAX    >   ZERO
                   MOVE    ZERO            TO  WRK-PTHKNINF-OK
                                               FLG-PTHKNINF-EDABAN
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       WRK-PTHKNINF-MAX
                           OR      WRK-PTHKNINF-OK >   ZERO
                       IF    ( WRK-PTHKN-EDABAN (IDY)
                                                   =   SPACE )
                       AND   ( WRK-PTHKN-HKNNUM (IDY)
                                               NOT =   "039" )
                           MOVE    1           TO  FLG-PTHKNINF-EDABAN
                       ELSE
                           IF      QUAREQ2-RES-EDABAN   (IDXX)
                                                   =
                                               WRK-PTHKN-EDABAN (IDY)
                               MOVE    IDY     TO  WRK-PTHKNINF-OK
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   IF      WRK-PTHKNINF-OK >   ZERO
      *                枝番まで一致した場合
                       MOVE    WRK-PTHKN-PTID (WRK-PTHKNINF-OK)
                                               TO  WRK-PTID
      *
                       INITIALIZE                  ORCSPTIDAREA
                       MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                       MOVE    WRK-PTID        TO  SPTID-PTID
                       CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                       SPA-AREA
                       IF      SPTID-RC        =   00
                           MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
                       END-IF
                   END-IF
               END-IF
             ELSE    
      *        照会番号から患者ＩＤ取得
               INITIALIZE                  ORCSPTNUMAREA
               MOVE    QUAREQ2-SHOKAI-NUM (IDXX)
                                      TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"    USING   ORCSPTNUMAREA
                                              SPA-AREA
               IF      SPTNUM-RC       =   00
                   MOVE    SPTNUM-PTID     TO  WRK-PTID
                   MOVE    SPTNUM-PTNUM    TO  WRK-PATIENT-ID
               ELSE
      *            患者が存在しない場合、保険情報から患者保険情報を検索
                   MOVE    ZERO            TO  PTHKNINF-OK
      *         
                   INITIALIZE                  PTHKNINF-REC
                   MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
                   MOVE    QUAREQ2-RES-HKNJANUM (IDXX)
                                           TO  PTHKN-HKNJANUM
                   MOVE    WRK-QUAREQ2-RES-KIGO (IDXX)
                                           TO  PTHKN-KIGO
                   MOVE    WRK-QUAREQ2-RES-NUM (IDXX)
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
                       IF      QUAREQ2-RES-EDABAN   (IDXX)
                                               =   PTHKN-EDABAN
                           MOVE    1               TO  PTHKNINF-OK
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
                       MOVE    PTHKN-PTID      TO  WRK-PTID
      *                患者番号取得
                       INITIALIZE                  ORCSPTIDAREA
                       MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                       MOVE    WRK-PTID        TO  SPTID-PTID
                       CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                        SPA-AREA
                       IF      SPTID-RC        =   00
                           MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
                       END-IF
                   END-IF
               END-IF    
             END-IF
           END-IF                            
      *
           IF      WRK-PTID        =   ZERO
               MOVE    "XXXXXXXXXXXXXXXXXXXX"
                                       TO  QUARES1-PATIENT-ID
           END-IF
      *
      *???
           display "PTID=" wrk-ptid "#" WRK-PATIENT-ID
      *
      *    UIDコード取得処理
           PERFORM 800-UID-GET-SEC
      *
      *    資格確認編集処理
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM   
           MOVE    C-UID           TO  ONSKAKU-TBL-UUID
           MOVE    WRK-PTID        TO  ONSKAKU-PTID
            IF      QUAREQ2-CARD-CLASS (2)
                                   =   SPACE
               MOVE    "02"            TO  ONSKAKU-KAKU-ENDFLG
           ELSE     
               MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
           END-IF
           IF      FLG-HKN-OK      =   "NG"    OR  "OK"
               MOVE    SPACE           TO  WRK-FILE-SIKIBETU
               UNSTRING    QUAREQ2-FILE-SIKIBETU
                                       DELIMITED  BY  "."
                                       INTO    WRK-FILE-NM1
                                               WRK-FILE-NM2
               END-UNSTRING
               STRING   WRK-FILE-NM1   DELIMITED  BY  SPACE
                       "_09.jpg"       DELIMITED  BY  SIZE
                                       INTO    ONSKAKU-HKNOCR-FILENAME
               END-STRING
           END-IF    
           IF      FLG-HKN-OK      =   "NG"
               MOVE    "3"             TO  ONSKAKU-ORCA-SHOKAI-KBN
           ELSE
               MOVE    "2"             TO  ONSKAKU-ORCA-SHOKAI-KBN
           END-IF
      *
           PERFORM 200211-ONSKAKU-HENSYU-SEC
      *                             
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
      *
           MOVE    ONSKAKU-REC     TO  KOH-ONSKAKU-REC
      *
           IF      QUAREQ2-CARD-CLASS (IDX)
                                   =   "A1"
             CONTINUE
           ELSE
      *        保険証＋公費のとき、公費の処理を行う
             IF      FLG-HKN-KBN     =   "03"
      *        対象の公費の資格確認レコードの更新処理
               PERFORM 2002242-KOH-ONSKAKU-UPDATE-SEC
      *        資格確認レコードの追加処理（顔認証）
               IF      WRK-ERRCD   NOT =   SPACE
                   GO  TO  200224-HKNOCR-HENSYU-EXT
               END-IF
      *??
               DISPLAY "SHORI-TIME=" WRK-KOH-SHORI-TIME
               IF      WRK-KOH-SHORI-TIME
                                   NOT =   SPACE
      *            公費ＵＩＤの設定
                   MOVE    KOH-ONSKAKU-TBL-UUID
                                           TO  KOH-ONSKAKU-KOUHI-UUID
               END-IF
             END-IF
             MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
           END-IF 
      *??
           DISPLAY "200221-ONSHI-KAKU-INSERT UUID=" ONSKAKU-TBL-UUID
      * 
           IF      WRK-ERRCD       =   SPACE
      *
               MOVE    KOH-ONSKAKU-REC TO  MCPDATA-REC
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
      *??
               DISPLAY "200224-HKNOCR-HENSYU MCP-RC=" MCP-RC
               IF      MCP-RC      NOT =   ZERO 
                   MOVE   "E17"             TO  WRK-ERRCD
                   DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC
               ELSE
      *            処方管理レコード作成処理
                   MOVE    KOH-ONSKAKU-REC  TO  SHOHO-ONSKAKU-REC
                   PERFORM 200225-SHOHO-KANRI-INSERT-SEC
               END-IF
           END-IF    
      *
      **     MOVE    KOH-ONSKAKU-REC     TO  HKN-ONSKAKU-REC
      *    複数の保険者が設定されている場合
      **     IF      QUAREQ2-RES-HKNJANUM (2)
      **                             NOT =   SPACE
      **         PERFORM VARYING IDX FROM    2   BY  1
      **                 UNTIL   IDX >       50
      **                 OR      QUAREQ2-RES-HKNJANUM (IDX)  =   SPACE
      **             INITIALIZE                  ONSKAKU-REC
      **             IF      ONSKAKU-ORCA-ERR-CODE
      **                                     =   SPACE
      **                 MOVE    "10"            TO  ONSKAKU-ORCA-ERR-CODE
      **             END-IF        
      *            資格確認レコードの作成処理（保険証）
      **             PERFORM 200221-02-ONSHI-KAKU-INSERT-SEC
      **         END-PERFORM
      **     END-IF
      *
           .
       200224-HKNOCR-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    保険情報から患者保険情報を検索
      ******************************************************************
       2002241-PTHKNINF-TAIHI-SEC    SECTION.
      *
           INITIALIZE                  WRK-PTHKNINF-AREA
      *
      *    保険情報の設定がないときは検索しない
           IF    ( QUAREQ2-RES-HKNJANUM (IDXX)
                                   =   SPACE )
           AND   ( WRK-QUAREQ2-RES-KIGO (IDXX)
                                   =   SPACE )
           AND   ( WRK-QUAREQ2-RES-NUM (IDXX)
                                   =   SPACE )
               GO  TO  2002241-PTHKNINF-TAIHI-EXT
           END-IF
      *         
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    QUAREQ2-RES-HKNJANUM (IDXX)
                                   TO  PTHKN-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-KIGO (IDXX)
                                   TO  PTHKN-KIGO
           MOVE    WRK-QUAREQ2-RES-NUM (IDXX)
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
                           OR      WRK-PTHKNINF-MAX    >=  10
               ADD     1               TO  WRK-PTHKNINF-MAX
               MOVE    PTHKNINF-REC    TO  WRK-PTHKNINF-REC
                                               (WRK-PTHKNINF-MAX)
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
       2002241-PTHKNINF-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *     公費の資格確認レコードの更新処理
      *****************************************************************
       2002242-KOH-ONSKAKU-UPDATE-SEC    SECTION.
      *
           MOVE    SPACE           TO  WRK-KOH-SHORI-TIME
      *
      *    保険情報の処理実行日時の日付と一致する公費の資格確認レコードを検索
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           IF      FLG-HKN-KBN     =   "03"
               MOVE    KOH-ONSKAKU-SHO-HKNJANUM
                                       TO  ONSKAKU-SHO-HKNJANUM
               MOVE    KOH-ONSKAKU-SHO-KIGO
                                       TO  ONSKAKU-SHO-KIGO
               MOVE    KOH-ONSKAKU-SHO-NUM
                                       TO  ONSKAKU-SHO-NUM
               MOVE    KOH-ONSKAKU-SHO-EDABAN
                                       TO  ONSKAKU-SHO-EDABAN
           ELSE    
               MOVE    KOH-ONSKAKU-RES-HKNJANUM
                                       TO  ONSKAKU-SHO-HKNJANUM
               MOVE    KOH-ONSKAKU-RES-KIGO
                                       TO  ONSKAKU-SHO-KIGO
               MOVE    KOH-ONSKAKU-RES-NUM
                                       TO  ONSKAKU-SHO-NUM
               MOVE    KOH-ONSKAKU-RES-EDABAN
                                       TO  ONSKAKU-SHO-EDABAN
           END-IF                        
           MOVE    "8"             TO  ONSKAKU-SHOKAI-KBN
      *****MOVE    KOH-ONSKAKU-SHORI-TIME
      *****                        TO  ONSKAKU-SHORI-TIME
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
                   MOVE    KOH-ONSKAKU-TBL-UUID
                                           TO  ONSKAKU-KOUHI-UUID
                   MOVE    KOH-ONSKAKU-FUJYO-UUID
                                           TO  ONSKAKU-FUJYO-UUID
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
                       MOVE    "E14"           TO  WRK-ERRCD
                       MOVE    1               TO  FLG-ONSHI-KAKU
                   ELSE
      *                公費の資格確認レコードを退避
                       ADD     1               TO  TAIHI-ONSKAKU-MAX
                       MOVE    ONSKAKU-REC     TO  TAIHI-ONSKAKU-REC
                                               (TAIHI-ONSKAKU-MAX)
      *
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
      *    電子処方箋区分が１以外は作成しない
           IF      SYS-1051-DENSHISHOHO
                               NOT =   "1"
               GO  TO  200225-SHOHO-KANRI-INSERT-EXT
           END-IF
      *
           IF      SHOHO-ONSKAKU-SHOKAI-KBN
                                   =   "1" OR  "2"
               CONTINUE
           ELSE
               GO  TO  200225-SHOHO-KANRI-INSERT-EXT
           END-IF
      *
           INITIALIZE                  SHOHO-KANRI-REC
           MOVE    SPA-HOSPNUM     TO  SHOKANR-HOSPNUM
           MOVE    SHOHO-ONSKAKU-TBL-UUID
                                   TO  SHOKANR-TBL-UUID
           MOVE    1               TO  SHOKANR-RENNUM 
           MOVE    SHOHO-ONSKAKU-KAKU-KAKUNINYMD
                                   TO  SHOKANR-SRYYMD
           MOVE    SHOHO-ONSKAKU-PTID    TO  SHOKANR-PTID 
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
      ******************************************************************
      *    資格確認レコードの作成処理（資格有効性５で複数の返却のとき）
      ******************************************************************
       200221-02-ONSHI-KAKU-INSERT-SEC SECTION.
      * 
      *    UIDコード取得処理
           PERFORM 800-UID-GET-SEC
      *
      *    任意のファイル識別子(医療機関固有項目)の設定
           STRING  WRK-PTID        DELIMITED  BY  SIZE
                   ","             DELIMITED  BY  SIZE  
                   C-UID           DELIMITED  BY  SIZE
                                   INTO       WRK-SIKIBETU
           END-STRING
      *
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM 
           MOVE    WRK-SIKIBETU    TO  ONSKAKU-FILE-SIKIBETU
           MOVE    C-UID           TO  ONSKAKU-TBL-UUID
           MOVE    HKN-ONSKAKU-TBL-UUID
                                   TO  ONSKAKU-OYA-UUID
           MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
           MOVE    QUAREQ2-SHORI-TIME
                                   TO  ONSKAKU-SHORI-TIME 
           MOVE    QUAREQ2-KAKUNINYMD
                                   TO  ONSKAKU-KAKUNINYMD
           MOVE    QUAREQ2-RESULT-KBN
                                   TO  ONSKAKU-RESULT-KBN
      *                             
           MOVE    QUAREQ2-CARD-CLASS   (1) 
                                   TO  ONSKAKU-CARD-CLASS 
           MOVE    QUAREQ2-RES-HKNJANUM (1)
                                   TO  ONSKAKU-RES-HKNJANUM
           MOVE    WRK-QUAREQ2-RES-KIGO (1)
                                   TO  ONSKAKU-RES-KIGO
           MOVE    WRK-QUAREQ2-RES-NUM (1)
                                   TO  ONSKAKU-RES-NUM 
           MOVE    QUAREQ2-RES-EDABAN  (1)
                                   TO  ONSKAKU-RES-EDABAN 
           MOVE    QUAREQ2-RES-HONKZKKBN (1)
                                   TO  ONSKAKU-RES-HONKZKKBN 
           MOVE    WRK-QUAREQ2-RES-HIHKNJANAME (1)
                                   TO  ONSKAKU-RES-HIHKNJANAME 
           MOVE    WRK-QUAREQ2-RES-NAME (1)
                                   TO  ONSKAKU-RES-NAME 
           MOVE    WRK-QUAREQ2-RES-NAME-ETC (1)
                                   TO  ONSKAKU-RES-NAME-ETC 
           MOVE    WRK-QUAREQ2-RES-KANANAME (1)
                                   TO  ONSKAKU-RES-KANANAME 
           MOVE    WRK-QUAREQ2-RES-KANANAME-ETC (1)
                                   TO  ONSKAKU-RES-KANANAME-ETC
           MOVE    QUAREQ2-RES-SEX1 (1)
                                   TO  ONSKAKU-RES-SEX1 
           MOVE    QUAREQ2-RES-SEX2 (1)
                                   TO  ONSKAKU-RES-SEX2 
           MOVE    QUAREQ2-RES-BIRTHDAY (1)
                                   TO  ONSKAKU-RES-BIRTHDAY 
           MOVE    WRK-QUAREQ2-RES-ADRS (1)
                                   TO  ONSKAKU-RES-ADRS 
           MOVE    QUAREQ2-RES-POST (1)
                                   TO  ONSKAKU-RES-POST 
           IF      QUAREQ2-CARD-CLASS (1)  =   "A1"
               CONTINUE
           ELSE
               MOVE    QUAREQ2-JOIN-SKKGETYMD (1)
                                   TO  ONSKAKU-JOIN-SKKGETYMD
           END-IF
           MOVE    QUAREQ2-SKKGETYMD (1)
                                   TO  ONSKAKU-SKKGETYMD 
           MOVE    QUAREQ2-TEKSTYMD (1)
                                   TO  ONSKAKU-TEKSTYMD 
           MOVE    QUAREQ2-TEKEDYMD (1)
                                   TO  ONSKAKU-TEKEDYMD 
           MOVE    QUAREQ2-FTNRATE (1)
                                   TO  ONSKAKU-FTNRATE 
           MOVE    QUAREQ2-MISYUGAKU (1)
                                   TO  ONSKAKU-MISYUGAKU 
           MOVE    QUAREQ2-LOSS-JIYU  (1)
                                   TO  ONSKAKU-LOSS-JIYU 
           MOVE    WRK-QUAREQ2-HKNJANUM-NAME (1)
                                   TO  ONSKAKU-HKNJANUM-NAME 
           MOVE    QUAREQ2-ELDER-SKKGETYMD (1)
                                   TO  ONSKAKU-ELDER-SKKGETYMD 
           MOVE    QUAREQ2-ELDER-TEKSTYMD (1)
                                   TO  ONSKAKU-ELDER-TEKSTYMD 
           MOVE    QUAREQ2-ELDER-TEKEDYMD (1)
                                   TO  ONSKAKU-ELDER-TEKEDYMD 
           MOVE    QUAREQ2-ELDER-FTNRATE (1)
                                   TO  ONSKAKU-ELDER-FTNRATE 
           MOVE    QUAREQ2-GENDO-DOUIFLG (1)
                                   TO  ONSKAKU-GENDO-DOUIFLG 
           MOVE    QUAREQ2-GENDO-TIME (1)
                                   TO  ONSKAKU-GENDO-TIME 
           MOVE    QUAREQ2-GENDO-SHOKBN (1)
                                   TO  ONSKAKU-GENDO-SHOKBN 
           MOVE    QUAREQ2-GENDO-TEKKBN (1)
                                   TO  ONSKAKU-GENDO-TEKKBN 
           MOVE    QUAREQ2-GENDO-SKKGETYMD (1)
                                   TO  ONSKAKU-GENDO-SKKGETYMD 
           MOVE    QUAREQ2-GENDO-TEKSTYMD (1)
                                   TO  ONSKAKU-GENDO-TEKSTYMD 
           MOVE    QUAREQ2-GENDO-TEKEDYMD (1)
                                   TO  ONSKAKU-GENDO-TEKEDYMD 
           MOVE    QUAREQ2-GENDO-SKJGNSTYMD (1)
                                   TO  ONSKAKU-GENDO-SKJGNSTYMD 
           MOVE    QUAREQ2-SIKKAN-TIME (1)
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
           MOVE    QUAREQ2-KENSIN-DOUIFLG (1)
                                   TO  ONSKAKU-KENSIN-DOUIFLG 
           MOVE    QUAREQ2-KENSIN-TIME (1)
                                   TO  ONSKAKU-KENSIN-TIME 
           MOVE    QUAREQ2-KENSIN-KIGENYMD (1)
                                   TO  ONSKAKU-KENSIN-KIGENYMD 
           MOVE    QUAREQ2-YAKUZAI-DOUIFLG (1)
                                   TO  ONSKAKU-YAKUZAI-DOUIFLG 
           MOVE    QUAREQ2-YAKUZAI-TIME (1)
                                   TO  ONSKAKU-YAKUZAI-TIME 
           MOVE    QUAREQ2-YAKUZAI-KIGEN (1)
                                   TO  ONSKAKU-YAKUZAI-KIGEN 
           MOVE    QUAREQ2-SHINRYO-DOUIFLG (1)
                                   TO  ONSKAKU-SHINRYO-DOUIFLG
           MOVE    QUAREQ2-SHINRYO-TIME    (1)
                                   TO  ONSKAKU-SHINRYO-TIME
           MOVE    QUAREQ2-SHINRYO-KIGEN   (1)
                                   TO  ONSKAKU-SHINRYO-KIGEN
           MOVE    QUAREQ2-OPE-DOUIFLG (1)
                                   TO  ONSKAKU-OPE-DOUIFLG
           MOVE    QUAREQ2-OPE-TIME    (1)
                                   TO  ONSKAKU-OPE-TIME
           MOVE    QUAREQ2-OPE-KIGEN   (1)
                                   TO  ONSKAKU-OPE-KIGEN
      **   MOVE    QUAREQ2-FILE-SIKIBETU
      **                           TO  ONSKAKU-SIKIBETU 
           MOVE    QUAREQ2-SHOKAI-NUM (1)
                                   TO  ONSKAKU-SHOKAI-NUM
      *                             
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
      * 
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *??
           DISPLAY "200221-02-ONSHI-KAKU-INSERT MCP-RC=" MCP-RC
           IF      MCP-RC      NOT =   ZERO 
               MOVE   "E18"             TO  WRK-ERRCD
               DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC 
           ELSE
      *        処方管理レコード作成処理
               MOVE    ONSKAKU-REC      TO  SHOHO-ONSKAKU-REC
               PERFORM 200225-SHOHO-KANRI-INSERT-SEC
           END-IF
           .
       200221-02-ONSHI-KAKU-INSERT-EXT.
           EXIT.
      *
      ******************************************************************
      *    照会番号、薬剤、特定検診の処理（保険証）
      ******************************************************************
       200222-RES1-HKN-HENSYU-SEC   SECTION.
      *
           IF      HKN-ONSKAKU-PTID
                                   =   ZERO
               GO  TO  200222-RES1-HKN-HENSYU-EXT
           END-IF
      *
           IF    ( FLG-SHOKAI-NUM  =   1   )
           OR    ( HKN-ONSKAKU-YAKUZAI-DOUIFLG
                                   =   "1" )
           OR    ( HKN-ONSKAKU-KENSIN-DOUIFLG
                                   =   "1" )
               CONTINUE
           ELSE
               GO  TO  200222-RES1-HKN-HENSYU-EXT
           END-IF
      *
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    HKN-ONSKAKU-PTID
                                   TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        =   00
               MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
           END-IF
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
           MOVE    HKN-ONSKAKU-HOSPCD
                                   TO  QUARES1-HOSPCD
      *    旧医療機関コードが設定されているとき
           IF      SYS-1051-HOSPCDF    =   SPACE
               CONTINUE
           ELSE
               MOVE    SYS-1051-HOSPCDF    TO  QUARES1-HOSPCD
           END-IF    
           MOVE    HKN-ONSKAKU-FILE-SIKIBETU
                                   TO  QUARES1-SIKIBETU
      *
           IF      FLG-SHOKAI-NUM  =   1
               MOVE    "01"            TO  ONSKAKU-SHOU-ENDFLG
      *                    
               MOVE    "True"          TO  QUARES1-REF-CLASS
               MOVE    HKN-ONSKAKU-HOSPCD
                                       TO  QUARES1-REF-HOSPCD
               MOVE    HKN-ONSKAKU-FILE-SIKIBETU
                                       TO  QUARES1-REF-SIKIBETU
      ******** MOVE    HKN-ONSKAKU-PTID
      ********                         TO  QUARES1-REF-SHOKAI-NUM
               MOVE    WRK-PATIENT-ID  TO  QUARES1-REF-SHOKAI-NUM
               MOVE    HKN-ONSKAKU-RES-HKNJANUM
                                       TO  QUARES1-REF-HKNJANUM
               MOVE    HKN-ONSKAKU-RES-KIGO
                                       TO  QUARES1-REF-KIGO
               MOVE    HKN-ONSKAKU-RES-NUM
                                       TO  QUARES1-REF-NUM
               MOVE    HKN-ONSKAKU-RES-EDABAN
                                       TO  QUARES1-REF-EDABAN
           END-IF
      *    薬剤情報閲覧同意フラグが１（同意）
           IF      HKN-ONSKAKU-YAKUZAI-DOUIFLG
                                   =   "1"
               MOVE    "01"            TO  ONSKAKU-YAKUZAI-ENDFLG
      *                                         
               MOVE    "True"          TO  QUARES1-DRG-CLASS
      *        マシン日付から一年前を取得
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY6-AREA
               MOVE    "61"        TO  LNK-DAY6-IRAI
               MOVE    SPA-SYSYMD  TO  LNK-DAY6-KIJUN
               MOVE    "2"         TO  LNK-DAY6-ZOGENPTN
               MOVE    -12         TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                           LNK-DAY6-AREA
      *??
          display "spaymd-12=" SPA-SYSYMD "#" LNK-DAY6-KEISAN "#"                                                  
      *        資格薬剤情報を検索
               INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
               MOVE    SPA-HOSPNUM     TO  ONS-YKZ-M-HOSPNUM
               MOVE    HKN-ONSKAKU-PTID
                                       TO  ONS-YKZ-M-PTID
               MOVE    ONSHI-YAKUZAI-MAIN-REC
                                       TO  MCPDATA-REC
               MOVE    "key2"          TO  WRK-MCP-PATHNAME
               PERFORM 900-ONSHI-YKZ-READ-SEC
               IF      FLG-ONSHI-YKZ   =   ZERO
                   IF      ONS-YKZ-M-SRYYM <   LNK-DAY6-KEISAN(1:6)
                       MOVE    1               TO  FLG-ONSHI-YKZ
                   END-IF
               END-IF
      *        資格薬剤情報が一年以内に存在しない場合
               IF      FLG-ONSHI-YKZ   =   1
                   MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO  ONS-YKZ-M-SRYYM
               END-IF                    
      *??
          display "FLG-ONSHI-YKZ=" FLG-ONSHI-YKZ "#" ONS-YKZ-M-SRYYM "#"                                                  
      *
               MOVE    ZERO    TO  IDZZ
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       12
                   INITIALIZE          STS-AREA-DAY
                   INITIALIZE          LNK-DAY6-AREA
                   MOVE    "61"    TO  LNK-DAY6-IRAI
                   MOVE    SPA-SYSYMD (1:6)
                                   TO  LNK-DAY6-KIJUN (1:6)
                   MOVE    "01"    TO  LNK-DAY6-KIJUN (7:2)
                   MOVE    "2"     TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN  =   IDYY    *   (-1) 
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
                   IF      LNK-DAY6-KEISAN(1:6)
                                       <   ONS-YKZ-M-SRYYM
                       MOVE    13          TO  IDYY
                   ELSE
                       ADD     1           TO  IDZZ
                       MOVE    HKN-ONSKAKU-HOSPCD
                                       TO  QUARES1-DRG-HOSPCD     (IDZZ)
                       MOVE    HKN-ONSKAKU-RES-HKNJANUM
                                       TO  QUARES1-DRG-HKNJANUM   (IDZZ)
                       MOVE    HKN-ONSKAKU-RES-KIGO
                                       TO  QUARES1-DRG-KIGO       (IDZZ)
                       MOVE    HKN-ONSKAKU-RES-NUM
                                       TO  QUARES1-DRG-NUM        (IDZZ)
                       MOVE    HKN-ONSKAKU-RES-EDABAN
                                       TO  QUARES1-DRG-EDABAN     (IDZZ)
                       MOVE    HKN-ONSKAKU-FILE-SIKIBETU
                                       TO  QUARES1-DRG-SIKIBETU   (IDZZ)
      *                診療情報閲覧同意フラグ
                       IF      ONSKAKU-SHINRYO-DOUIFLG
                                       =   "1"
                           MOVE    "4"         TO
                                       QUARES1-DRG-FILECATEGORY   (IDZZ)
                       ELSE
                           MOVE    "2"         TO
                                       QUARES1-DRG-FILECATEGORY   (IDZZ)
                       END-IF
                       MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  QUARES1-DRG-STARTDATE  (IDZZ)
                                           QUARES1-DRG-ENDDATE    (IDZZ)
                   END-IF
               END-PERFORM
               IF    ( SYS-1051-ONSHIYAKUZAI
                                       =   "1" )
               OR    ( SYS-1051-ONSHIYKZSINRYO
                                       =   "1" )
                   ADD     1           TO  IDZZ
                   MOVE    HKN-ONSKAKU-HOSPCD
                                       TO  QUARES1-DRG-HOSPCD     (IDZZ)
                   MOVE    HKN-ONSKAKU-RES-HKNJANUM
                                       TO  QUARES1-DRG-HKNJANUM   (IDZZ)
                   MOVE    HKN-ONSKAKU-RES-KIGO
                                       TO  QUARES1-DRG-KIGO       (IDZZ)
                   MOVE    HKN-ONSKAKU-RES-NUM
                                       TO  QUARES1-DRG-NUM        (IDZZ)
                   MOVE    HKN-ONSKAKU-RES-EDABAN
                                       TO  QUARES1-DRG-EDABAN     (IDZZ)
                   MOVE    HKN-ONSKAKU-FILE-SIKIBETU
                                       TO  QUARES1-DRG-SIKIBETU   (IDZZ)
      *            診療情報閲覧同意フラグ
                   IF    ( ONSKAKU-SHINRYO-DOUIFLG
                                       =   "1" )
                   AND   ( SYS-1051-ONSHIYKZSINRYO
                                       =   "1" )
                       MOVE    "3"         TO
                                       QUARES1-DRG-FILECATEGORY   (IDZZ)
                   ELSE
                       MOVE    "1"         TO
                                       QUARES1-DRG-FILECATEGORY   (IDZZ)
                   END-IF
                   STRING  "YZKsiquc01req_"
                                       DELIMITED BY  SIZE
                           WRK-FILE-PATIENT-ID
                                       DELIMITED BY  SIZE
                           ".xml"      DELIMITED BY  SIZE
                                       INTO    QUARES1-DRG-FILE   (IDZZ)
                   END-STRING
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"        TO  LNK-DAY6-IRAI
                   MOVE    SPA-SYSYMD  TO  LNK-DAY6-KIJUN
                   MOVE    "2"         TO  LNK-DAY6-ZOGENPTN
                   MOVE    -12         TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  QUARES1-DRG-STARTDATE  (IDZZ)
                   MOVE    WRK-SPA-SYSYMD-1
                                       TO  QUARES1-DRG-ENDDATE    (IDZZ)
               END-IF
           END-IF                        
      *    特定検診情報閲覧同意フラグが１（同意）
           IF      HKN-ONSKAKU-KENSIN-DOUIFLG
                                   =   "1"
               MOVE    "01"            TO  ONSKAKU-KENSIN-ENDFLG
      *
               MOVE    "True"          TO  QUARES1-SPEC-CLASS
               MOVE    HKN-ONSKAKU-HOSPCD  TO  QUARES1-SPEC-HOSPCD   (1)
               MOVE    HKN-ONSKAKU-RES-HKNJANUM
                                       TO  QUARES1-SPEC-HKNJANUM (1)
               MOVE    HKN-ONSKAKU-RES-KIGO
                                       TO  QUARES1-SPEC-KIGO     (1)
               MOVE    HKN-ONSKAKU-RES-NUM
                                       TO  QUARES1-SPEC-NUM      (1)
               MOVE    HKN-ONSKAKU-RES-EDABAN
                                       TO  QUARES1-SPEC-EDABAN   (1)
               MOVE    HKN-ONSKAKU-FILE-SIKIBETU
                                       TO  QUARES1-SPEC-SIKIBETU (1)
               MOVE    "2"             TO
                                           QUARES1-SPEC-FILECATEGORY (1)
               IF      SYS-1051-ONSHIKENSIN 
                                       =   "1"                
                   MOVE    HKN-ONSKAKU-HOSPCD
                                           TO  QUARES1-SPEC-HOSPCD   (2)
                   MOVE    HKN-ONSKAKU-RES-HKNJANUM
                                           TO  QUARES1-SPEC-HKNJANUM (2)
                   MOVE    HKN-ONSKAKU-RES-KIGO
                                           TO  QUARES1-SPEC-KIGO     (2)
                   MOVE    HKN-ONSKAKU-RES-NUM
                                           TO  QUARES1-SPEC-NUM      (2)
                   MOVE    HKN-ONSKAKU-RES-EDABAN
                                           TO  QUARES1-SPEC-EDABAN   (2)
                   MOVE    HKN-ONSKAKU-FILE-SIKIBETU
                                           TO  QUARES1-SPEC-SIKIBETU (2)
                   MOVE    "1"         TO
                                           QUARES1-SPEC-FILECATEGORY (2)
                   STRING  "TKKsiquc01req_"
                                           DELIMITED BY  SIZE
                           WRK-FILE-PATIENT-ID
                                           DELIMITED BY  SIZE
                           ".xml"      DELIMITED BY  SIZE
                                       INTO    QUARES1-SPEC-FILE (2)
                   END-STRING
               END-IF                
           END-IF
      *
           .
       200222-RES1-HKN-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    照会番号、薬剤、特定検診の処理（顔認証）
      ******************************************************************
       200223-RES1-FACE-HENSYU-SEC   SECTION.
      *
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    ONSKAKU-PTID    TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        =   00
               MOVE    SPTID-PTNUM     TO  WRK-PATIENT-ID
           END-IF
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
      *
      *    任意のファイル識別子(医療機関固有項目)の設定
           STRING  ONSKAKU-PTID    DELIMITED  BY  SIZE
                   ","             DELIMITED  BY  SIZE  
                   ONSKAKU-TBL-UUID
                                   DELIMITED  BY  SIZE
                                   INTO       WRK-SIKIBETU
           END-STRING
      * 
           MOVE    WRK-SIKIBETU    TO  ONSKAKU-FILE-SIKIBETU
           MOVE    ONSKAKU-FILE-SIKIBETU
                                   TO  QUARES1-SIKIBETU
      *
           IF      ONSKAKU-SHOKAI-NUM
                                   =   SPACE
               MOVE    "01"            TO  ONSKAKU-SHOU-ENDFLG
      *                     
               MOVE    "True"          TO  QUARES1-REF-CLASS
               MOVE    ONSKAKU-HOSPCD  TO  QUARES1-REF-HOSPCD
               MOVE    ONSKAKU-FILE-SIKIBETU
                                       TO  QUARES1-REF-SIKIBETU
      ******** MOVE    ONSKAKU-PTID        TO  QUARES1-REF-SHOKAI-NUM
               MOVE    WRK-PATIENT-ID      TO  QUARES1-REF-SHOKAI-NUM
               MOVE    ONSKAKU-RES-HKNJANUM
                                       TO  QUARES1-REF-HKNJANUM
               MOVE    ONSKAKU-RES-KIGO
                                           TO  QUARES1-REF-KIGO
               MOVE    ONSKAKU-RES-NUM
                                           TO  QUARES1-REF-NUM
               MOVE    ONSKAKU-RES-EDABAN
                                           TO  QUARES1-REF-EDABAN
           END-IF
      *    薬剤情報閲覧同意フラグが１（同意）
           IF      ONSKAKU-YAKUZAI-DOUIFLG
                                       =   "1"
               MOVE    "01"            TO  ONSKAKU-YAKUZAI-ENDFLG
      *                                         
               MOVE    "True"          TO  QUARES1-DRG-CLASS
      *        マシン日付から一年前を取得
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE    SPA-SYSYMD      TO  LNK-DAY6-KIJUN
               MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
               MOVE    -12             TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *??
          display "spaymd-12=" SPA-SYSYMD "#" LNK-DAY6-KEISAN "#"                                                  
      *        資格薬剤情報を検索
               INITIALIZE                  ONSHI-YAKUZAI-MAIN-REC
               MOVE    SPA-HOSPNUM     TO  ONS-YKZ-M-HOSPNUM
               MOVE    ONSKAKU-PTID    TO  ONS-YKZ-M-PTID
               MOVE    ONSHI-YAKUZAI-MAIN-REC
                                       TO  MCPDATA-REC
               MOVE    "key2"          TO  WRK-MCP-PATHNAME
               PERFORM 900-ONSHI-YKZ-READ-SEC
               IF      FLG-ONSHI-YKZ   =   ZERO
                   IF      ONS-YKZ-M-SRYYM <   LNK-DAY6-KEISAN(1:6)
                       MOVE    1               TO  FLG-ONSHI-YKZ
                   END-IF
               END-IF
      *        資格薬剤情報が一年以内に存在しない場合
               IF      FLG-ONSHI-YKZ   =   1
                   MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO  ONS-YKZ-M-SRYYM
               END-IF                    
      *??
          display "FLG-ONSHI-YKZ=" FLG-ONSHI-YKZ "#" ONS-YKZ-M-SRYYM "#"                                                  
      *
               MOVE    ZERO    TO  IDZZ
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       12
                   INITIALIZE          STS-AREA-DAY
                   INITIALIZE          LNK-DAY6-AREA
                   MOVE    "61"    TO  LNK-DAY6-IRAI
                   MOVE    SPA-SYSYMD (1:6)
                                   TO  LNK-DAY6-KIJUN (1:6)
                   MOVE    "01"    TO  LNK-DAY6-KIJUN (7:2)
                   MOVE    "2"     TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN  =   IDYY    *   (-1) 
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
                   IF      LNK-DAY6-KEISAN(1:6)
                                       <   ONS-YKZ-M-SRYYM
                       MOVE    13          TO  IDYY
                   ELSE
                       ADD     1           TO  IDZZ
                   MOVE    ONSKAKU-HOSPCD  TO  QUARES1-DRG-HOSPCD (IDZZ)
                   MOVE    ONSKAKU-RES-HKNJANUM
                                           TO  QUARES1-DRG-HKNJANUM
                                                                  (IDZZ)
                   MOVE    ONSKAKU-RES-KIGO
                                           TO  QUARES1-DRG-KIGO   (IDZZ)
                   MOVE    ONSKAKU-RES-NUM
                                           TO  QUARES1-DRG-NUM    (IDZZ)
                   MOVE    ONSKAKU-RES-EDABAN
                                           TO  QUARES1-DRG-EDABAN (IDZZ)
                   MOVE    ONSKAKU-FILE-SIKIBETU
                                           TO  QUARES1-DRG-SIKIBETU
                                                                  (IDZZ)
      *            診療情報閲覧同意フラグ
                   IF      ONSKAKU-SHINRYO-DOUIFLG
                                           =   "1"
                       MOVE    "4"         TO
                                           QUARES1-DRG-FILECATEGORY
                                                                  (IDZZ)
                   ELSE
                       MOVE    "2"             TO
                                           QUARES1-DRG-FILECATEGORY
                                                                  (IDZZ)
                   END-IF
                       MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  QUARES1-DRG-STARTDATE  (IDZZ)
                                           QUARES1-DRG-ENDDATE    (IDZZ)
                   END-IF
               END-PERFORM
               IF    ( SYS-1051-ONSHIYAKUZAI
                                       =   "1" )
               OR    ( SYS-1051-ONSHIYKZSINRYO
                                       =   "1" )
                       ADD     1           TO  IDZZ
                       MOVE    ONSKAKU-HOSPCD
                                           TO  QUARES1-DRG-HOSPCD (IDZZ)
                       MOVE    ONSKAKU-RES-HKNJANUM
                                           TO  QUARES1-DRG-HKNJANUM
                                                                  (IDZZ)
                       MOVE    ONSKAKU-RES-KIGO
                                           TO  QUARES1-DRG-KIGO   (IDZZ)
                       MOVE    ONSKAKU-RES-NUM
                                           TO  QUARES1-DRG-NUM    (IDZZ)
                       MOVE    ONSKAKU-RES-EDABAN
                                           TO  QUARES1-DRG-EDABAN (IDZZ)
                       MOVE    ONSKAKU-FILE-SIKIBETU
                                           TO  QUARES1-DRG-SIKIBETU
                                                                  (IDZZ)
      *                診療情報閲覧同意フラグ
                       IF    ( ONSKAKU-SHINRYO-DOUIFLG
                                           =   "1" )
                       AND   ( SYS-1051-ONSHIYKZSINRYO
                                           =   "1" )
                           MOVE    "3"         TO
                                       QUARES1-DRG-FILECATEGORY   (IDZZ)
                       ELSE
                           MOVE    "1"         TO
                                           QUARES1-DRG-FILECATEGORY
                                                                  (IDZZ)
                       END-IF                                           
                       STRING  "YZKsiquc01req_"
                                           DELIMITED BY  SIZE
                               WRK-FILE-PATIENT-ID
                                           DELIMITED BY  SIZE
                               ".xml"      DELIMITED BY  SIZE
                                        INTO    QUARES1-DRG-FILE (IDZZ)
                       END-STRING
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
                                       TO  QUARES1-DRG-STARTDATE  (IDZZ)
                       MOVE    WRK-SPA-SYSYMD-1
                                       TO  QUARES1-DRG-ENDDATE    (IDZZ)
               END-IF                            
           END-IF
      *    特定検診情報閲覧同意フラグが１（同意）
           IF      ONSKAKU-KENSIN-DOUIFLG
                                       =   "1"
               MOVE    "01"            TO  ONSKAKU-KENSIN-ENDFLG
      *
               MOVE    "True"          TO  QUARES1-SPEC-CLASS
               MOVE    ONSKAKU-HOSPCD  TO  QUARES1-SPEC-HOSPCD   (1)
               MOVE    ONSKAKU-RES-HKNJANUM
                                       TO  QUARES1-SPEC-HKNJANUM (1)
               MOVE    ONSKAKU-RES-KIGO
                                       TO  QUARES1-SPEC-KIGO     (1)
               MOVE    ONSKAKU-RES-NUM
                                       TO  QUARES1-SPEC-NUM      (1)
               MOVE    ONSKAKU-RES-EDABAN
                                       TO  QUARES1-SPEC-EDABAN   (1)
               MOVE    ONSKAKU-FILE-SIKIBETU
                                       TO  QUARES1-SPEC-SIKIBETU (1)
               MOVE    "2"             TO
                                       QUARES1-SPEC-FILECATEGORY (1)
               IF      SYS-1051-ONSHIKENSIN 
                                       =   "1"                
                   MOVE    ONSKAKU-HOSPCD
                                           TO  QUARES1-SPEC-HOSPCD   (2)
                   MOVE    ONSKAKU-RES-HKNJANUM
                                           TO  QUARES1-SPEC-HKNJANUM (2)
                   MOVE    ONSKAKU-RES-KIGO
                                           TO  QUARES1-SPEC-KIGO     (2)
                   MOVE    ONSKAKU-RES-NUM
                                           TO  QUARES1-SPEC-NUM      (2)
                   MOVE    ONSKAKU-RES-EDABAN
                                           TO  QUARES1-SPEC-EDABAN   (2)
                   MOVE    ONSKAKU-FILE-SIKIBETU
                                           TO  QUARES1-SPEC-SIKIBETU (2)
                   MOVE    "1"             TO
                                           QUARES1-SPEC-FILECATEGORY (2)
                   STRING  "TKKsiquc01req_"
                                           DELIMITED BY  SIZE
                           WRK-FILE-PATIENT-ID
                                           DELIMITED BY  SIZE
                           ".xml"          DELIMITED BY  SIZE
                                           INTO    QUARES1-SPEC-FILE (2)
                   END-STRING
               END-IF                
           END-IF
      *
           .
       200223-RES1-FACE-HENSYU-EXT.
           EXIT.
      *
      ******************************************************************
      *    UIDコード取得処理
      ******************************************************************
       800-UID-GET-SEC     SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
           .
       800-UID-GET-EXT.
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
               WHEN    "E10"
               WHEN    "E11"
               WHEN    "E16"
               WHEN    "E17"
               WHEN    "E18"
                   MOVE    "資格確認レコードの追加ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E12"
               WHEN    "E14"
                   MOVE    "資格確認レコードの更新ができませんでした。"
                                                   TO  WRK-ERRMSG
               WHEN    "E13"
               WHEN    "E19"
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
      ******************************************************************
      *    オンライン資格確認照会結果読込
      ******************************************************************
       900-ONSHI-KAKU-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-KAKU-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認読込（照会区分による）
      ******************************************************************
       900-ONSHI-KAKU-READ-N-SEC     SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSKAKU-REC
                IF      ONSKAKU-SHOKAI-KBN  =   WRK-SHOKAI-KBN
                   MOVE    ZERO                TO  FLG-ONSHI-KAKU
               ELSE
                   GO  TO  900-ONSHI-KAKU-READ-N-SEC
               END-IF        
           ELSE
               INITIALIZE                      ONSKAKU-REC
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
      *        処理実行日時の日付、任意のファイル識別子が一致しないときは対象外
               IF    ( KOH-ONSKAKU-SHORI-TIME (1:8)
                                       NOT =   ONSKAKU-SHORI-TIME (1:8))
               OR    ( KOH-ONSKAKU-FILE-SIKIBETU
                                       NOT =   ONSKAKU-FILE-SIKIBETU   )
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
      ******************************************************************
      *    オンライン資格確認照会結果読込(KEY29)
      ******************************************************************
       900-ONSHI-KAKU-READ-KEY29-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-HEIYO-ONSHI-KAKU
           INITIALIZE                      TAIHI-HEI-ONSKAKU-AREA
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N29-SEC
           ELSE
               INITIALIZE                      HEIYO-ONSKAKU-REC
               MOVE    1                   TO  FLG-HEIYO-ONSHI-KAKU
           END-IF
      *
           PERFORM             UNTIL   FLG-HEIYO-ONSHI-KAKU    =   1
               ADD     1               TO  TAIHI-HEI-ONSKAKU-MAX
               MOVE    HEIYO-ONSKAKU-REC
                                       TO  TAIHI-HEI-ONSKAKU-REC
                                               (TAIHI-HEI-ONSKAKU-MAX)
      *
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key29"             TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N29-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key29"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-KAKU-READ-KEY29-EXT.
           EXIT.
      *
      ******************************************************************
      *    オンライン資格確認照会結果読込
      ******************************************************************
       900-ONSHI-KAKU-READ-N29-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HEIYO-ONSKAKU-REC
      *
      *        当日分
               IF      HEIYO-ONSKAKU-SHORI-TIME (1:8)
                                           =   SPACE
                   IF      HEIYO-ONSKAKU-KAKU-KAKUNINYMD
                                               =   SMCNDATE-YMD
                       CONTINUE
                   ELSE
                       GO  TO  900-ONSHI-KAKU-READ-N29-SEC
                   END-IF        
               ELSE
                   IF      HEIYO-ONSKAKU-SHORI-TIME (1:8)
                                               =   SMCNDATE-YMD
                       CONTINUE
                   ELSE
                       GO  TO  900-ONSHI-KAKU-READ-N29-SEC
                   END-IF
               END-IF
      *        UUID からの資格確認結果の被保険者証区分
               EVALUATE    UUID-ONSKAKU-CARD-CLASS
                   WHEN    "A1"
      *                資格確認結果が生保の場合
                       IF      HEIYO-ONSKAKU-CARD-CLASS    =   "A1"
                           GO  TO  900-ONSHI-KAKU-READ-N29-SEC
                       END-IF
                       IF      HEIYO-ONSKAKU-SIKAKU-YUKO   =   "5"
                           MOVE    1       TO  FLG-TAIHI-H-SIKAKU-YUKO
                       END-IF  
                   WHEN    OTHER
      *                資格確認結果が主保険の場合
                       IF      HEIYO-ONSKAKU-CARD-CLASS    =   "A1"
                           CONTINUE
                       ELSE
                           GO  TO  900-ONSHI-KAKU-READ-N29-SEC
                       END-IF
              END-EVALUATE
           ELSE
               INITIALIZE                  HEIYO-ONSKAKU-REC
               MOVE    1               TO  FLG-HEIYO-ONSHI-KAKU
           END-IF
      *
           .
       900-ONSHI-KAKU-READ-N29-EXT.
           EXIT.
      *
      ******************************************************************
      *    オンライン資格確認照会結果読込(KEY30)
      ******************************************************************
       900-ONSHI-KAKU-READ-KEY30-SEC   SECTION.
      *
           MOVE    ZERO                TO  FLG-HEIYO-ONSHI-KAKU
           INITIALIZE                      TAIHI-HEI-ONSKAKU-AREA
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N30-SEC
           ELSE
               INITIALIZE                      HEIYO-ONSKAKU-REC
               MOVE    1                   TO  FLG-HEIYO-ONSHI-KAKU
           END-IF
      *
           PERFORM             UNTIL   FLG-HEIYO-ONSHI-KAKU    =   1
               ADD     1               TO  TAIHI-HEI-ONSKAKU-MAX
               MOVE    HEIYO-ONSKAKU-REC
                                       TO  TAIHI-HEI-ONSKAKU-REC
                                               (TAIHI-HEI-ONSKAKU-MAX)
      *
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N30-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-KAKU-READ-KEY30-EXT.
           EXIT.
      *
      ******************************************************************
      *    オンライン資格確認照会結果読込
      ******************************************************************
       900-ONSHI-KAKU-READ-N30-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HEIYO-ONSKAKU-REC
           ELSE
               INITIALIZE                  HEIYO-ONSKAKU-REC
               MOVE    1               TO  FLG-HEIYO-ONSHI-KAKU
           END-IF
      *
           .
       900-ONSHI-KAKU-READ-N30-EXT.
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
      *****************************************************************
      *    処方箋管理読込
      *****************************************************************
       900-SHOHO-KANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_shoho_kanri"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_shoho_kanri"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SHOHO-KANRI-REC
               ELSE
                   INITIALIZE                      SHOHO-KANRI-REC
                   MOVE    1                   TO  FLG-SHOHO-KANRI
               END-IF
           ELSE
               INITIALIZE                      SHOHO-KANRI-REC
               MOVE    1                   TO  FLG-SHOHO-KANRI
           END-IF
      *
           MOVE    "tbl_shoho_kanri"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-SHOHO-KANRI-READ-EXT.
           EXIT.
      *
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

