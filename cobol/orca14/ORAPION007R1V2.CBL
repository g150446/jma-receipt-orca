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
       PROGRAM-ID.           ORAPION007R1V2.
      ******************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 公費確認登録、更新処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  21/xx/xx    ORCAMO        新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.01    ORCAMO       22/08/22  保険証ＯＣＲ(アルメックス）対応
      *  05.01.01    ORCAMO       22/10/14  保険証は対象外
      *  05.01.05    ORCAMO       22/12/05  画像取込み処理変更
      *                                     （保険証ＯＣＲおよび公費医療券画像）
      *  05.02.01    ORCAMO        23/12/21  医療扶助対応
      *  05.02.02    ORCAMO        25/06/09  旧医療機関コード対応
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
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  PTHKNINF-OK             PIC 9(01).
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
           03  WRK-FILE-SIKIBETU.
               05  WRK-FILE-NM1        PIC X(50).
               05  WRK-FILE-NM2        PIC X(50).
      *
           03  WRK-UUID                PIC X(36).
           03  WRK-SHOKAI-KBN          PIC X(01).
           03  WRK-HKNJANUM-AREA.
               05  WRK-HKNJANUM        PIC X(08)   OCCURS  10.
               05  WRK-DATA-OCC                    OCCURS  2.
                   07  WRK-TEKSTYMD    PIC X(8).
                   07  WRK-TEKEDYMD    PIC X(8).
                   07  WRK-FTNRATE     PIC 9(03).
                   07  WRK-JGNGAK      PIC 9(06).
           03  WRK-DATA-AREA.    
               05  WRK-IN-DATA         PIC X(100).
               05  WRK-IN-KBN          PIC X(10).
               05  WRK-OUT-AREA                    OCCURS  2.
                   07  WRK-OUT-NUM     PIC 9(08).
                   07  WRK-OUT-DATA    PIC X(08).
               05  WRK-NYUGAIKBN       PIC 9(01).
               05  WRK-NYUGAIKBN-NAME  PIC X(04).
               05  WRK-OUT-D           PIC X(08).
               05  WRK-OUT-N           PIC 9(08).
               05  WRK-DATA            PIC X(20)   OCCURS  2.
               05  WRK-DATA-O          PIC X(20)   OCCURS  3.
      *    資格確認用エラーコード
           03  WRK-ERR-AREA. 
               05  WRK-ERR-CODE        PIC X(09).
               05  WRK-ONS-ERR-CODE    PIC X(09).
               05  WRK-ONS-ERR-MSG     PIC X(120).
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
      *    全角変換用 
           03  WRK-KANACHK-MAE-INPUT   PIC X(5000).
           03  WRK-KANACHK-OUT-INPUT   PIC X(5000).
           03  WRK-KANACHK-LEN         PIC 9(04).
      *
       01  WRK-QUAREQ3-AREA.     
           03  WRK-QUAREQ3-SHO-KIGO    PIC X(80).
           03  WRK-QUAREQ3-SHO-NUM     PIC X(80).
           03  WRK-QUAREQ3-FILE-SIKIBETU
                                       PIC X(50).
           03  WRK-QUAREQ3-RESULT-AREA.
               05  WRK-QUAREQ3-RES-KIGO
                                       PIC X(80).
               05  WRK-QUAREQ3-RES-NUM PIC X(80).
               05  WRK-QUAREQ3-RES-HIHKNJANAME
                                       PIC X(200).
               05  WRK-QUAREQ3-RES-NAME
                                       PIC X(200).
               05  WRK-QUAREQ3-RES-NAME-ETC
                                       PIC X(200).
               05  WRK-QUAREQ3-RES-KANANAME
                                       PIC X(200).
               05  WRK-QUAREQ3-RES-KANANAME1
                                       PIC X(200).
               05  WRK-QUAREQ3-RES-KANANAME2
                                       PIC X(200).
               05  WRK-QUAREQ3-RES-KANANAME-ETC
                                       PIC X(200).
               05  WRK-QUAREQ3-RES-ADRS
                                       PIC X(500).
               05  WRK-QUAREQ3-HKNJANUM-NAME
                                       PIC X(200).
      *
      *    保険証の資格確認情報     
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
       01  FLG-HKN-ONSHI-KAKU      PIC 9(01).                            
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
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
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
           COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    API XML読み込み用定義体
           COPY    "CPONLINEQUAREQ3.INC".
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
      *    資格確認テーブル
       01  ONSKAKU-REC.       
           COPY    "CPONSHI-KAKU.INC".
      *
      *    資格確認薬剤情報
       01  ONSHI-YAKUZAI-MAIN-REC.       
           COPY    "CPONSHI-YAKUZAI-MAIN.INC".
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
           DISPLAY   "*  onlinequa4 start  *"
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
               MOVE   404                  TO  APILST07-HTTPSTATUS
           ELSE
      *        返却領域編集
               PERFORM 300-RGTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "**********************"
           DISPLAY   "*   onlinequa4 end   *"
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
      *
           STRING  "A+"                DELIMITED  BY  SIZE
                   MCP-USER            DELIMITED  BY  SIZE
                                       INTO    WRK-OPID
           END-STRING
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
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
           IF      APILST07-BODY     NOT =   LOW-VALUE
               DISPLAY "MDCRES-OBJECT not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           INITIALIZE                      XML-ONLINEQUAREQ3
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_onlinequareq3" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST07-BODY       TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "onlinequareq3"     TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ      TO  XML-ONLINEQUAREQ3
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
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD       TO  QUARES1-API-RESULT
               MOVE    WRK-ERRMSG      TO  QUARES1-API-RESULT-MSG
           END-IF
      *
           IF      APILST07-BODY       NOT =   LOW-VALUE
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
               MOVE  QUARES1-OBJECT    TO  APILST07-BODY
               MOVE  "application/xml" TO  APILST07-CONTENT-TYPE
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
           IF      QUAREQ3-PATIENTREQ
                               =   SPACE 
               MOVE    "E15"           TO  WRK-ERRCD
               GO  TO  2001-INPUT-CHK-EXT
           END-IF
      * 
           IF      QUAREQ3-SHOKAI-KBN
                               =   "9" OR  "8"
               CONTINUE
           ELSE
               DISPLAY "SHOKAI-KBN=" QUAREQ3-SHOKAI-KBN
               MOVE    "E??"           TO  WRK-ERRCD
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
           INITIALIZE                  WRK-PTID-AREA
           INITIALIZE                  WRK-QUAREQ3-AREA
                                       HKN-ONSKAKU-REC
           MOVE    ZERO            TO  FLG-HKN-ONSHI-KAKU
      *
      *    照会番号から患者ＩＤ取得
           IF      QUAREQ3-SHOKAI-NUM (1)
                               NOT =   SPACE
               INITIALIZE                  ORCSPTNUMAREA
               MOVE    QUAREQ3-SHOKAI-NUM (1)
                                      TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"    USING   ORCSPTNUMAREA
                                              SPA-AREA
               IF      SPTNUM-RC       =   00
                   MOVE    SPTNUM-PTID     TO  WRK-PTID
                   MOVE    SPTNUM-PTNUM    TO  WRK-PATIENT-ID
               ELSE
                   DISPLAY "SHOKAI-NUM ERR " QUAREQ3-SHOKAI-NUM (1)
               END-IF
           END-IF
      *
      *    全角文字変換処理
           MOVE    QUAREQ3-SHO-KIGO
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-SHO-KIGO
           IF    ( QUAREQ3-SHO-HKNJANUM (1:2)
                                   =   "12"  )
           AND   ( QUAREQ3-SHO-HKNJANUM (7:2)
                               NOT =   SPACE )
      *        負担者番号が医療扶助のとき番号は半角のまま
               MOVE    QUAREQ3-SHO-NUM TO  WRK-QUAREQ3-SHO-NUM
           ELSE
               MOVE    QUAREQ3-SHO-NUM TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                       TO  WRK-QUAREQ3-SHO-NUM
           END-IF
      *     
      *
           MOVE    SPACE           TO  QUARES1-QUARES1
           INITIALIZE                  WRK-QUAREQ3-RESULT-AREA
      *
           MOVE    QUAREQ3-RES-KIGO (1)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-RES-KIGO
           MOVE    QUAREQ3-RES-NUM (1)
                                   TO  WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
            MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-RES-NUM
           MOVE    QUAREQ3-RES-HIHKNJANAME (1)
                                   TO  WRK-KANACHK-MAE-INPUT
           MOVE    200             TO  WRK-KANACHK-LEN
           PERFORM 801-KANACHK2-SEC                       
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-RES-HIHKNJANAME
           MOVE    QUAREQ3-RES-NAME (1)
                                   TO  WRK-KANACHK-MAE-INPUT
           MOVE    200             TO  WRK-KANACHK-LEN
           PERFORM 801-KANACHK2-SEC                       
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-RES-NAME
           MOVE    QUAREQ3-RES-NAME-ETC (1)
                                   TO  WRK-KANACHK-MAE-INPUT
           MOVE    200             TO  WRK-KANACHK-LEN
           PERFORM 801-KANACHK2-SEC                       
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-RES-NAME-ETC
           MOVE    QUAREQ3-RES-KANANAME (1)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-RES-KANANAME
           MOVE    QUAREQ3-RES-KANANAME-ETC (1)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-RES-KANANAME-ETC
           MOVE    QUAREQ3-RES-ADRS (1)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    1               TO  IDYY
           PERFORM VARYING IDZZ    FROM    1  BY  2
                   UNTIL   IDZZ    >       250
                   OR      WRK-KANACHK-OUT-INPUT (IDZZ:)
                                   =       SPACE
               IF      WRK-KANACHK-OUT-INPUT (IDZZ:2)
                                   NOT =   "　"
                   MOVE    WRK-KANACHK-OUT-INPUT (IDZZ:2)
                                       TO  WRK-QUAREQ3-RES-ADRS (IDYY:2)
                   ADD     2           TO  IDYY
               END-IF
           END-PERFORM
           MOVE    QUAREQ3-HKNJANUM-NAME (1)
                                   TO    WRK-KANACHK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-KANACHK-OUT-INPUT
                                   TO  WRK-QUAREQ3-HKNJANUM-NAME
      *    ファイル識別子
           IF      QUAREQ3-FILE-SIKIBETU
                               NOT =   SPACE
               MOVE    QUAREQ3-FILE-SIKIBETU
                                       TO  WRK-QUAREQ3-FILE-SIKIBETU
               EVALUATE    WRK-QUAREQ3-FILE-SIKIBETU (1:11)
                   WHEN    "ALMocr01res"
                   WHEN    "ALMocr02res"
                   WHEN    "ALMocr03res"
                       MOVE    WRK-QUAREQ3-FILE-SIKIBETU (7:2)
                                               TO  FLG-HKN-KBN
                       IF      WRK-QUAREQ3-FILE-SIKIBETU (13:8)
                                               =   "99999999"
                           MOVE    "NG"            TO  FLG-HKN-OK
                       ELSE
                           MOVE    "OK"            TO  FLG-HKN-OK
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    保険情報から資格確認レコードを検索
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           IF    ( QUAREQ3-FILE-SIKIBETU
                                   =   SPACE )
           OR    ( FLG-HKN-KBN     =   "01"  )
      *        顔認証の資格確認レコードを検索
               MOVE    "1"             TO  ONSKAKU-SHOKAI-KBN
               MOVE    QUAREQ3-SHO-HKNJANUM
                                       TO  ONSKAKU-RES-HKNJANUM
               MOVE    WRK-QUAREQ3-SHO-KIGO
                                       TO  ONSKAKU-RES-KIGO
               MOVE    WRK-QUAREQ3-SHO-NUM
                                       TO  ONSKAKU-RES-NUM
               MOVE    QUAREQ3-SHO-EDABAN
                                       TO  ONSKAKU-RES-EDABAN
               MOVE    "key10"         TO  WRK-MCP-PATHNAME
           ELSE    
      *        保険証の資格確認レコードを検索
               MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
               MOVE    QUAREQ3-SHO-HKNJANUM
                                       TO  ONSKAKU-SHO-HKNJANUM
               MOVE    WRK-QUAREQ3-SHO-KIGO
                                       TO  ONSKAKU-SHO-KIGO
               MOVE    WRK-QUAREQ3-SHO-NUM
                                       TO  ONSKAKU-SHO-NUM
               MOVE    QUAREQ3-SHO-EDABAN
                                       TO  ONSKAKU-SHO-EDABAN
               MOVE    "key11"         TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           ELSE
               INITIALIZE                  ONSKAKU-REC
               MOVE    1               TO  FLG-ONSHI-KAKU
           END-IF
      *
           PERFORM             UNTIL   FLG-ONSHI-KAKU  =   1
      *        処理実行日時の日付が一致し、公費ＵＩＤが未設定の場合      
               IF    ( QUAREQ3-SHORI-TIME (1:8)
                                       =   ONSKAKU-SHORI-TIME (1:8) )
               AND   ( ONSKAKU-KOUHI-UUID
                                       =   SPACE                    )
                   IF    ( QUAREQ3-FILE-SIKIBETU
                                           =   SPACE )
                   OR    ( FLG-HKN-KBN     =   "01"  )
                       MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
                       MOVE    1               TO  FLG-ONSHI-KAKU
                                                   FLG-HKN-ONSHI-KAKU
                   ELSE
      *                保険証の資格確認レコードを検索時は生年月日、ファイル識別子が一致した場合
                       IF    ( QUAREQ3-SHO-BIRTHDAY
                                           =   ONSKAKU-SHO-BIRTHDAY  )
                       AND   ( QUAREQ3-FILE-SIKIBETU
                                           =   ONSKAKU-FILE-SIKIBETU )
                           MOVE    ONSKAKU-REC TO  HKN-ONSKAKU-REC
                           MOVE    1           TO  FLG-ONSHI-KAKU
                                                   FLG-HKN-ONSHI-KAKU
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-ONSHI-KAKU  =   ZERO
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME
                                           TO  MCP-PATHNAME
                   PERFORM 900-ONSHI-KAKU-READ-N-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    資格確認レコードの作成処理（公費レコード）
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
                   OR    ( QUAREQ3-RES-HKNJANUM (IDX)
                               =   SPACE
                           AND    
                           QUAREQ3-TEKSTYMD (IDX)
                               =   SPACE
                           AND
                           QUAREQ3-TEKEDYMD (IDX)
                               =   SPACE
                           AND
                           QUAREQ3-RES-NUM (IDX)
                               =   SPACE )
      *      保険証は対象外
             IF      QUAREQ3-CARD-CLASS (IDX)
                                   =   "I1"
               CONTINUE
             ELSE
               INITIALIZE                  WRK-HKNJANUM-AREA
                                           WRK-ERR-AREA
      *        負担者番号                         
               UNSTRING    QUAREQ3-RES-HKNJANUM (IDX)
                                       DELIMITED   BY  ";"
                                       INTO    WRK-HKNJANUM (1)
                                               WRK-HKNJANUM (2)
                                               WRK-HKNJANUM (3)
                                               WRK-HKNJANUM (4)
                                               WRK-HKNJANUM (5)
                                               WRK-HKNJANUM (6)
                                               WRK-HKNJANUM (7)
                                               WRK-HKNJANUM (8)
                                               WRK-HKNJANUM (9)
                                               WRK-HKNJANUM (10)
               END-UNSTRING
      *         
      *        被保険者証有効開始年月日
               INITIALIZE                  WRK-DATA-AREA
               MOVE    QUAREQ3-TEKSTYMD (IDX)
                                       TO  WRK-IN-DATA
               MOVE    "STYMD"         TO  WRK-IN-KBN
               PERFORM 20021-KOH-HENSYU-SEC
               MOVE    WRK-OUT-DATA (1)
                                       TO  WRK-TEKSTYMD (1)
               MOVE    WRK-OUT-DATA (2)
                                       TO  WRK-TEKSTYMD (2)
      *        被保険者証有効終了年月日
               INITIALIZE                  WRK-DATA-AREA
               MOVE    QUAREQ3-TEKEDYMD (IDX)
                                       TO  WRK-IN-DATA
               MOVE    "EDYMD"         TO  WRK-IN-KBN
               PERFORM 20021-KOH-HENSYU-SEC
               MOVE    WRK-OUT-DATA (1)
                                       TO  WRK-TEKEDYMD (1)
               MOVE    WRK-OUT-DATA (2)
                                       TO  WRK-TEKEDYMD (2)
      *        被保険者証一部負担金割合
               INITIALIZE                  WRK-DATA-AREA
               MOVE    "FTNRATE"       TO  WRK-IN-KBN
               MOVE    QUAREQ3-FTNRATE (IDX)
                                       TO  WRK-IN-DATA
               PERFORM 20021-KOH-HENSYU-SEC
               MOVE    WRK-OUT-NUM (1) TO  WRK-FTNRATE (1)
               MOVE    WRK-OUT-NUM (2) TO  WRK-FTNRATE (2)
      *        被保険者証一部負担金限度額
               INITIALIZE                  WRK-DATA-AREA
               MOVE    "JGNGAK"        TO  WRK-IN-KBN
               MOVE    QUAREQ3-FTNGAKU (IDX)
                                       TO  WRK-IN-DATA
               PERFORM 20021-KOH-HENSYU-SEC
               MOVE    WRK-OUT-NUM (1) TO  WRK-JGNGAK (1)
               MOVE    WRK-OUT-NUM (2) TO  WRK-JGNGAK (2)
      *
               PERFORM VARYING IDY    FROM    1   BY  1
                       UNTIL   IDY    >       10
      *          保険者番号が空白のときは一件で終了
                 IF    ( WRK-HKNJANUM (IDY)
                                         =   SPACE )
                 AND   ( IDY             >   1     )                        
                     MOVE    10              TO  IDY
                 ELSE
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
      *            UIDコード取得処理
                   PERFORM 800-UID-GET-SEC
      *            任意のファイル識別子(医療機関固有項目)の設定
                   STRING  WRK-PTID        DELIMITED  BY  SIZE
                           ","             DELIMITED  BY  SIZE  
                           C-UID           DELIMITED  BY  SIZE
                                           INTO       WRK-SIKIBETU
                   END-STRING
      *            MOVE    WRK-SIKIBETU    TO  ONSKAKU-FILE-SIKIBETU
      *                                        ONSKAKU-SIKIBETU 
                   MOVE    C-UID           TO  ONSKAKU-TBL-UUID
                   IF      FLG-HKN-ONSHI-KAKU  =   1
                       IF      HKN-ONSKAKU-CARD-CLASS
                                                   =   "01"    
                           MOVE    HKN-ONSKAKU-TBL-UUID
                                               TO  ONSKAKU-KOUHI-UUID
                           MOVE    HKN-ONSKAKU-FUJYO-UUID
                                               TO  ONSKAKU-FUJYO-UUID
                       ELSE                        
                           MOVE    HKN-ONSKAKU-FUJYO-UUID
                                               TO  ONSKAKU-FUJYO-UUID
                                                   ONSKAKU-KOUHI-UUID
                       END-IF
                       MOVE    HKN-ONSKAKU-PTID
                                               TO  ONSKAKU-PTID
                       MOVE    HKN-ONSKAKU-KAKUNINYMD
                                               TO  ONSKAKU-KAKUNINYMD
      *                MOVE    "1"             TO  ONSKAKU-SET-PTID
                   ELSE
                       IF      WRK-PTID        >   0
                           MOVE    WRK-PTID        TO  ONSKAKU-PTID
      *                    MOVE    "1"             TO  ONSKAKU-SET-PTID
                       END-IF
      *************    MOVE    QUAREQ3-KAKUNINYMD
      *************                            TO  ONSKAKU-KAKUNINYMD
                   END-IF
                   MOVE    QUAREQ3-SHOKAI-KBN
                                           TO  ONSKAKU-SHOKAI-KBN
      *            MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
                   MOVE    QUAREQ3-SHORI-TIME
                                           TO  ONSKAKU-SHORI-TIME 
                   MOVE    QUAREQ3-RESULT-KBN
                                           TO  ONSKAKU-RESULT-KBN
                   MOVE    QUAREQ3-ERR-CODE
                                           TO  ONSKAKU-ERR-CODE
                   MOVE    QUAREQ3-ERR-MSG TO  ONSKAKU-ERR-MSG
                   IF    (  ONSKAKU-ERR-CODE 
                                           =   SPACE )
                   AND   ( WRK-ONS-ERR-CODE
                                       NOT =   SPACE )
                       MOVE    WRK-ONS-ERR-CODE    TO  ONSKAKU-ERR-CODE
                       MOVE    WRK-ONS-ERR-MSG     TO  ONSKAKU-ERR-MSG
                   END-IF
                   MOVE    "1"             TO  ONSKAKU-CODE-TYPE
                   MOVE    QUAREQ3-SHO-HKNJANUM
                                           TO  ONSKAKU-SHO-HKNJANUM
                   MOVE    WRK-QUAREQ3-SHO-KIGO
                                           TO  ONSKAKU-SHO-KIGO
                   MOVE    WRK-QUAREQ3-SHO-NUM
                                           TO  ONSKAKU-SHO-NUM
                   MOVE    QUAREQ3-SHO-EDABAN
                                           TO  ONSKAKU-SHO-EDABAN
                   MOVE    QUAREQ3-SHO-BIRTHDAY
                                           TO  ONSKAKU-SHO-BIRTHDAY
                   MOVE    WRK-HKNJANUM (IDY)
                                           TO  ONSKAKU-RES-HKNJANUM
                   MOVE    QUAREQ3-RES-NUM (IDX)
                                           TO  ONSKAKU-RES-NUM
                   MOVE    QUAREQ3-RES-BIRTHDAY (IDX)
                                           TO  ONSKAKU-RES-BIRTHDAY
                   MOVE    WRK-TEKSTYMD (1)
                                           TO  ONSKAKU-KOH-TEKSTYMD (1)
                   MOVE    WRK-TEKSTYMD (2)
                                           TO  ONSKAKU-KOH-TEKSTYMD (2)
                   MOVE    WRK-TEKEDYMD (1)
                                           TO  ONSKAKU-KOH-TEKEDYMD (1)
                   MOVE    WRK-TEKEDYMD (2)
                                           TO  ONSKAKU-KOH-TEKEDYMD (2)
                   MOVE    WRK-FTNRATE (1)
                                           TO  ONSKAKU-KOH-FTNRATE (1)
                   MOVE    WRK-FTNRATE (2)
                                           TO  ONSKAKU-KOH-FTNRATE (2)
                   MOVE    WRK-JGNGAK (1)
                                           TO  ONSKAKU-KOH-JGNGAK (1)
                   MOVE    WRK-JGNGAK (2)
                                           TO  ONSKAKU-KOH-JGNGAK (2)
                   MOVE    QUAREQ3-HOSPCD  TO  ONSKAKU-HOSPCD
                   MOVE    QUAREQ3-FILE-SIKIBETU
                                           TO  ONSKAKU-FILE-SIKIBETU
                   MOVE    QUAREQ3-SHOKAI-NUM (IDX)
                                           TO  ONSKAKU-SHOKAI-NUM
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
                   MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
      *
                   IF      FLG-HKN-OK      =   "NG"    OR  "OK"
                       MOVE    SPACE           TO  WRK-FILE-SIKIBETU
                       UNSTRING    QUAREQ3-FILE-SIKIBETU
                                               DELIMITED  BY  "."
                                               INTO    WRK-FILE-NM1
                                                       WRK-FILE-NM2
                       END-UNSTRING
                       STRING   WRK-FILE-NM1   DELIMITED  BY  SPACE
                               "_01.jpg"       DELIMITED  BY  SIZE
                                        INTO   ONSKAKU-HKNOCR-FILENAME
                       END-STRING
                   END-IF
      *                             
                   MOVE    SPA-OPID        TO  ONSKAKU-OPID
                   MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
                   MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
      *
                   DISPLAY "2002 KAKU-INSERT UUID=" ONSKAKU-TBL-UUID
      * 
                   MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
      *
                   DISPLAY "200221-ONSHI-KAKU-INSERT MCP-RC=" MCP-RC
                   IF      MCP-RC      NOT =   ZERO 
                       MOVE   "E??"             TO  WRK-ERRCD
                       DISPLAY "TBL_ONSHI_KAKU INSERT ERR " MCP-RC 
                   END-IF
                 END-IF
               END-PERFORM
             END-IF
           END-PERFORM
      *
      *    資格確認レコードの更新処理（顔認証の公費ＵＩＤの更新）
           IF      FLG-HKN-ONSHI-KAKU  =   1
               IF      HKN-ONSKAKU-CARD-CLASS
                                           =   "01"    
                   MOVE    HKN-ONSKAKU-TBL-UUID
                                           TO  HKN-ONSKAKU-KOUHI-UUID
               ELSE                            
                   MOVE    HKN-ONSKAKU-FUJYO-UUID
                                           TO  HKN-ONSKAKU-KOUHI-UUID
               END-IF
               MOVE    SMCNDATE-YMD    TO  HKN-ONSKAKU-UPYMD
               MOVE    SMCNDATE-HMS    TO  HKN-ONSKAKU-UPHMS
               MOVE    HKN-ONSKAKU-REC TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   display "tbl_onshi_kaku update error" 
                   MOVE    "E??"           TO  WRK-ERRCD
               END-IF
           END-IF
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
           .        
       2002-SIKAKU-KAKUNIN-UPDATE-EXT.
           EXIT.
      *
      ******************************************************************
      *    公費情報編集処理
      ******************************************************************
       20021-KOH-HENSYU-SEC        SECTION.
      *
           IF      WRK-IN-DATA     =   SPACE
               GO  TO  20021-KOH-HENSYU-EXT
           END-IF    
      *
           UNSTRING    WRK-IN-DATA DELIMITED   BY  ";"
                                   INTO    WRK-DATA (1)
                                           WRK-DATA (2)
           END-UNSTRING
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               INITIALIZE              WRK-NYUGAIKBN
                                       WRK-ERR-CODE
                                       WRK-OUT-D
                                       WRK-OUT-N
      *
      *  DISPLAY "INDATA=" IDXX " " WRK-DATA (IDXX)  " "
               IF      WRK-DATA (IDXX) =   SPACE
                   CONTINUE
               ELSE
                   UNSTRING    WRK-DATA (IDXX) DELIMITED   BY  SPACE
                                               INTO    WRK-DATA-O (1)
                                                       WRK-DATA-O (2)
                                                       WRK-DATA-O (3)
                   END-UNSTRING
      *
      * DISPLAY "DATA-O=" WRK-DATA-O (1) " " WRK-DATA-O (2)
      *
                   PERFORM VARYING IDYY    FROM    1   BY  1
                           UNTIL   IDYY    >       2
                           OR      WRK-DATA-O (IDYY)
                                           =       SPACE
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI
                       MOVE    "1"                 TO  KANACHK-SYORIKBN
                       MOVE    WRK-DATA-O (IDYY)   TO  KANACHK-MAE-INPUT
                       MOVE    20                  TO  KANACHK-MAX-CNT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
      *
                       IF      KANACHK-RC          =   ZERO
                           IF      KANACHK-SYUBETU     =   2     
      *                        入外区分取得                 
                               EVALUATE    WRK-DATA-O (IDYY) (1:4)
                                   WHEN    "入院"
                                       MOVE    1       TO  WRK-NYUGAIKBN
                                   WHEN    "通院"
                                       MOVE    2       TO  WRK-NYUGAIKBN
                               END-EVALUATE
                           ELSE
      *   DISPLAY "WRK-DATA-O=" IDYY " " WRK-DATA-O (IDYY)                  
                               INITIALIZE                  ORCSNUMAREA
                               MOVE    WRK-DATA-O (IDYY)   TO  SNUM-INX
                               CALL    "ORCSNUM"           USING
                                                           ORCSNUMAREA
                               EVALUATE    WRK-IN-KBN
                                   WHEN    "STYMD"
                                     IF      SNUM-RC         =   ZERO
                                       IF    ( SNUM-KETSUU     =   8
                                           AND SNUM-SEISUU     =   8 )
                                       OR    ( SNUM-OUTNUM     =   0 )  
                                           PERFORM 800-YMDCHK-SEC
                                           IF      SGDAY-RC    >   ZERO
                                               MOVE    "ORCAE04"   TO
                                                           WRK-ERR-CODE
                                          END-IF
                                       ELSE    
                                           MOVE    "ORCAE04"       TO
                                                           WRK-ERR-CODE
                                       END-IF
                                     ELSE    
                                       MOVE    "ORCAE04"
                                                       TO  WRK-ERR-CODE
                                     END-IF
                                   WHEN    "EDYMD"
                                     IF      SNUM-RC         =   ZERO
                                       IF    ( SNUM-KETSUU     =   8
                                           AND SNUM-SEISUU     =   8 )
                                       OR    ( SNUM-OUTNUM     =   0 )  
                                           PERFORM 800-YMDCHK-SEC
                                           IF      SGDAY-RC    >   ZERO
                                               MOVE    "ORCAE07"   TO
                                                           WRK-ERR-CODE
                                           END-IF
                                       ELSE    
                                           MOVE    "ORCAE07"       TO
                                                           WRK-ERR-CODE
                                       END-IF
                                     ELSE    
                                       MOVE    "ORCAE07"
                                                       TO  WRK-ERR-CODE
                                     END-IF
                                 WHEN    "FTNRATE"
                                   IF      SNUM-RC         =   ZERO
                                       IF    ( SNUM-SEISUU <   4     )
                                       AND   ( SNUM-SYOSUU =   0     )
                                       AND   ( SNUM-MINKBN =   SPACE )
                                           MOVE    SNUM-OUTNUM     TO
                                                           WRK-OUT-N
                                           MOVE    SPACE           TO
                                                           WRK-ERR-CODE
                                       ELSE     
                                           MOVE    "ORCAE10"       TO
                                                           WRK-ERR-CODE
                                       END-IF
                                   ELSE    
                                       MOVE    "ORCAE10"
                                                       TO  WRK-ERR-CODE
                                   END-IF
                                 WHEN    "JGNGAK"
                                   IF      SNUM-RC         =   ZERO
                                       IF    ( SNUM-SEISUU <   7     )
                                       AND   ( SNUM-SYOSUU =   0     )
                                       AND   ( SNUM-MINKBN =   SPACE )
                                           MOVE    SNUM-OUTNUM     TO
                                                           WRK-OUT-N
                                           MOVE    SPACE           TO
                                                           WRK-ERR-CODE
                                       ELSE     
                                           MOVE    "ORCAE13"       TO
                                                           WRK-ERR-CODE
                                       END-IF
                                   ELSE    
                                       MOVE    "ORCAE13"
                                                       TO  WRK-ERR-CODE
                                   END-IF
                               END-EVALUATE
                           END-IF
                       ELSE
                           EVALUATE    WRK-IN-KBN
                               WHEN    "STYMD"
                                   MOVE    "ORCAE03"
                                                   TO  WRK-ERR-CODE
                               WHEN    "EDYMD"
                                   MOVE    "ORCAE06"
                                                   TO  WRK-ERR-CODE
                               WHEN    "FTNRATE" 
                                   MOVE    "ORCAE09"
                                                   TO  WRK-ERR-CODE
                               WHEN    "JGNGAK" 
                                   MOVE    "ORCAE12"
                                                   TO  WRK-ERR-CODE
                           END-EVALUATE
                       END-IF    
                   END-PERFORM
      *
                   IF      WRK-ERR-CODE    =   SPACE
                       IF      WRK-NYUGAIKBN   =   ZERO
                           EVALUATE    WRK-IN-KBN
                               WHEN    "STYMD"
                               WHEN    "EDYMD"
                                   MOVE    WRK-OUT-D   TO
                                                       WRK-OUT-DATA (1)
                                                       WRK-OUT-DATA (2)
                               WHEN    "FTNRATE" 
                               WHEN    "JGNGAK" 
                                   MOVE    WRK-OUT-N   TO
                                                       WRK-OUT-NUM  (1)
                                                       WRK-OUT-NUM  (2)
                           END-EVALUATE
                       ELSE     
                           EVALUATE    WRK-IN-KBN
                               WHEN    "STYMD"
                               WHEN    "EDYMD"
                                   MOVE    WRK-OUT-D       TO
                                           WRK-OUT-DATA (WRK-NYUGAIKBN)
                               WHEN    "FTNRATE" 
                               WHEN    "JGNGAK" 
                                   MOVE    WRK-OUT-N       TO
                                           WRK-OUT-NUM  (WRK-NYUGAIKBN)
                           END-EVALUATE
                       END-IF        
                   END-IF                            
               END-IF
      *
               IF      WRK-ERR-CODE    NOT =   SPACE
                   PERFORM 20022-ONSKAKU-ERR-HENSYU-SEC
               END-IF
           END-PERFORM
      *    設定項目がひとつのとき 
      **     IF    ( WRK-ERR-CODE    =   SPACE )
      **     AND   ( WRK-DATA (2)    =   SPACE )
      **         EVALUATE    WRK-IN-KBN
      **             WHEN    "STYMD"
      **             WHEN    "EDYMD"
      **                 MOVE    WRK-OUT-DATA (1)    TO  WRK-OUT-DATA (2)
      **             WHEN    "FTNRATE" 
      **             WHEN    "JGNGAK" 
      **                 MOVE    WRK-OUT-NUM (1)    TO  WRK-OUT-NUM   (2)
      **         END-EVALUATE
      **     END-IF
           .        
       20021-KOH-HENSYU-EXT.
           EXIT.
      * 
      ******************************************************************
      *    処理結果コードのチェックによる設定処理
      ******************************************************************
       20022-ONSKAKU-ERR-HENSYU-SEC  SECTION.
      *
      *    オン資側で設定済なら終了
           IF      QUAREQ3-ERR-CODE    NOT =   SPACE
               INITIALIZE                  WRK-ERR-CODE
               GO  TO  20022-ONSKAKU-ERR-HENSYU-EXT
           END-IF
      *
      *    既に設定済なら終了
           IF      WRK-ONS-ERR-CODE    NOT =   SPACE
               INITIALIZE                  WRK-ERR-CODE
               GO  TO  20022-ONSKAKU-ERR-HENSYU-EXT
           END-IF
      *
           EVALUATE    WRK-NYUGAIKBN
               WHEN    1
                   MOVE    "入院"              TO  WRK-NYUGAIKBN-NAME
               WHEN    2
                   MOVE    "通院"              TO  WRK-NYUGAIKBN-NAME
               WHEN    OTHER 
                   MOVE    SPACE               TO  WRK-NYUGAIKBN-NAME
           END-EVALUATE
      *
           MOVE    WRK-ERR-CODE        TO  WRK-ONS-ERR-CODE
           EVALUATE    WRK-ERR-CODE
               WHEN    "ORCAE02"
                   MOVE    "有効開始年月日が未設定です。"
                                               TO  WRK-ONS-ERR-MSG
               WHEN    "ORCAE03"
                   STRING  "公費有効開始年月日（"
                                               DELIMITED   BY  SIZE
                           WRK-NYUGAIKBN-NAME  DELIMITED   BY  SIZE
                           "）が未設定です。"  DELIMITED   BY  SIZE  
                                               INTO  WRK-ONS-ERR-MSG
                   END-STRING
               WHEN    "ORCAE04"
                   STRING  "公費有効開始年月日（"
                                               DELIMITED   BY  SIZE
                           WRK-NYUGAIKBN-NAME  DELIMITED   BY  SIZE
                           "）が暦日ではありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ONS-ERR-MSG
 　                  END-STRING
                WHEN    "ORCAE05"
                   MOVE    "公費有効終了年月日が未設定です。"
                                               TO  WRK-ONS-ERR-MSG
               WHEN    "ORCAE06"
                   STRING  "公費有効終了年月日（"
                                               DELIMITED   BY  SIZE
                           WRK-NYUGAIKBN-NAME  DELIMITED   BY  SIZE
                           "）が未設定です。"  DELIMITED   BY  SIZE  
                                               INTO  WRK-ONS-ERR-MSG
                   END-STRING
               WHEN    "ORCAE07"
                   STRING  "公費有効終了年月日（"
                                               DELIMITED   BY  SIZE
                           WRK-NYUGAIKBN-NAME  DELIMITED   BY  SIZE
                           "）が暦日ではありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ONS-ERR-MSG
                   END-STRING
               WHEN    "ORCAE08"
                   MOVE    "公費負担割合が未設定です。"
                                               TO  WRK-ONS-ERR-MSG
               WHEN    "ORCAE09"
                   STRING  "公費負担割合（"
                                               DELIMITED   BY  SIZE
                           WRK-NYUGAIKBN-NAME  DELIMITED   BY  SIZE
                           "）が未設定です。"  DELIMITED   BY  SIZE  
                                               INTO  WRK-ONS-ERR-MSG
                   END-STRING
               WHEN    "ORCAE10"
                   STRING  "公費負担割合（"    DELIMITED   BY  SIZE
                           WRK-NYUGAIKBN-NAME  DELIMITED   BY  SIZE
                           "）の設定に誤りがあります。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ONS-ERR-MSG
                   END-STRING
               WHEN    "ORCAE11"
                   MOVE    "公費上限額が未設定です。"
                                               TO  WRK-ONS-ERR-MSG
               WHEN    "ORCAE12"
                   STRING  "公費上限額（"      DELIMITED   BY  SIZE
                           WRK-NYUGAIKBN-NAME  DELIMITED   BY  SIZE
                           "）が未設定です。"  DELIMITED   BY  SIZE  
                                               INTO  WRK-ONS-ERR-MSG
                   END-STRING
               WHEN    "ORCAE13"
                   STRING  "公費上限額（"      DELIMITED   BY  SIZE
                           WRK-NYUGAIKBN-NAME  DELIMITED   BY  SIZE
                           "）の設定に誤りがあります。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-ONS-ERR-MSG
                   END-STRING
               WHEN    "ORCAE14"
                   MOVE
                       "公費有効開始年月日の入院、通院が未設定です。"
                                               TO  WRK-ONS-ERR-MSG
               WHEN    "ORCAE15"
                   MOVE
                       "公費有効終了年月日の入院、通院が未設定です。"
                                               TO  WRK-ONS-ERR-MSG
               WHEN    "ORCAE16"
                   MOVE    "公費負担割合の入院、通院が未設定です。"
                                               TO  WRK-ONS-ERR-MSG
               WHEN    "ORCAE17"
                   MOVE    "公費上限額の入院、通院が未設定です。"
                                               TO  WRK-ONS-ERR-MSG
           END-EVALUATE
           .
       20022-ONSKAKU-ERR-HENSYU-EXT.
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
      *     日付チェック処理
      *****************************************************************
       800-YMDCHK-SEC             SECTION.
      * 
           INITIALIZE              ORCSGDAYAREA
           IF      SNUM-OUTEDT (8:8)   =   "00000000"  OR
                                           "99999999"
               MOVE    SNUM-OUTEDT (8:8)   TO  WRK-OUT-D
               MOVE    SPACE               TO  WRK-ERR-CODE
           ELSE
               MOVE    SNUM-OUTEDT (8:8)   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING   ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-SDAY          TO  WRK-OUT-D
                   MOVE    SPACE               TO  WRK-ERR-CODE
               END-IF
           END-IF
           DISPLAY "800 SGDAY-RC=" SGDAY-RC      
           .
       800-YMDCHK-EXT.
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
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSKAKU-REC
               MOVE    ZERO                TO  FLG-ONSHI-KAKU
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

