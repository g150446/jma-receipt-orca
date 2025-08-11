      ******************************************************************
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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSONSHI001.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 資格確認サブ
      *  管理者            : 
      *  20/09/15    NMED−藤原    新規作成
      ******************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    ORCAMO       21/04/19  資格確認：無効状態から変わらない不具合、
      *                                     双子の不具合対応
      *  05.00.02    ORCAMO       21/05/12  資格確認：強制確認対応
      *  05.00.03    ORCAMO       21/05/25  資格確認：枝番未設定対応
      *  05.00.04    ORCAMO       21/06/29  資格確認：該当なしのときの返却対応
      *  05.00.05    ORCAMO       21/07/05  資格確認：患者番号の場合に再発行対応
      *  05.00.06    ORCAMO       21/08/19  資格確認：有効本人家族区分が違うときの対応
      *  05.00.07    ORCAMO       21/08/26  資格確認：有効の場合に後期高
      *                                     齢者の本人家族区分が違うときの対応
      *  05.01.01    ORCAMO       21/10/20  資格確認：公費対応
      *  05.01.02    ORCAMO       22/01/07  資格確認：診療行為の資格確認有無対応
      *  05.01.03    ORCAMO       22/04/11  資格確認：その他エラーの対応
      *  05.01.04    ORCAMO       22/04/28  資格確認：有効の場合の保険変更を追加
      *  05.01.05    ORCAMO       22/08/22  保険証ＯＣＲ(アルメックス）対応
      *  05.01.06    ORCAMO       22/11/01  予約患者一括資格確認照会対応
      *  05.01.07    ORCAMO       22/01/17  電子処方箋対応
      *  05.02.01    ORCAMO       23/04/21  枝番未入力の対応
      *  05.02.02    ORCAMO       23/04/21  高齢者の負担割合違いの表示対応
      *  05.02.03    ORCAMO       23/07/14  番号未入力対応
      *  05.02.01    ORCAMO       23/12/21  医療扶助対応
      *  05.02.02    ORCAMO       24/04/01  医療扶助対応
      *                                     資格確認要否依頼（強制）
      *                                     患者番号変更時強制資格確認
      *  05.02.03    ORCAMO       24/04/08  医療扶助対応
      *                                     併用時の保険情報資格確認
      *  05.02.04    ORCAMO       24/05/13  エラーデータ対応
      *                                     資格有効性が空白のとき
      *                                     致命的エラー(保険者番号がないとか生年月日が不正とか) 
      *  05.02.03    ORCAMO       24/04/08  医療扶助対応
      *                                     併用時に医療扶助の受給者番号が空白のときは資格確認を行わない
      *  05.02.04    ORCAMO       24/07/02  医療扶助対応
      *                                     併用時にどちらかが該当資格なしのときのチェックの修正
      *  05.02.05    ORCAMO       24/07/16  医療扶助併用対応
      *  05.02.06    ORCAMO       24/08/13  特別療養費対応
      *  05.02.07    ORCAMO       24/11/15  オンライン診療対応
      *  05.02.08    ORCAMO       26/03/17  当日以前の資格確認を利用した場合、
      *                                     その情報の利用は行わない
      *  05.02.09    ORCAMO       25/06/09  旧医療機関コード対応
      ******************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ONSHI-KAKU      PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SHOHO-KANRI     PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-MONTHLYNUM      PIC 9(01).
           03  FLG-ONSHI-AIDLST    PIC 9(01).
           03  FLG-ONSHI-HOUMON    PIC 9(01).
           03  FLG-JOBKANRI        PIC 9(01).
      *
           03  FLG-PTHKNINF-OK     PIC 9(01).
           03  FLG-PTKOHINF-OK     PIC 9(01).
           03  FLG-ONSHI-KAKU-OK   PIC 9(01).
           03  FLG-YYK-CHK         PIC 9(01).
           03  FLG-ONSHI-HEIYO     PIC 9(01).
           03  FLG-ONSHI-HOUMON-CHK
                                   PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-PT-KANANAME1.
               05  WRK-PT-KANANAME11   PIC X(100).
               05  WRK-PT-KANANAME12   PIC X(100).
           03  WRK-RES-KANANAME1.
               05  WRK-RES-KANANAME11
                                   PIC X(100).
               05  WRK-RES-KANANAME12
                                   PIC X(100).
           03  WRK-PT-KANANAME     PIC X(100).
           03  WRK-PT-NAME         PIC X(100).
           03  WRK-RES-KANANAME    PIC X(100).
           03  WRK-RES-NAME        PIC X(100).
           03  WRK-PTNUM           PIC X(20).
      *
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-LNK-ONSHI001-KAKUNIN-FLG
                                   PIC X(03).
      *
           03  WRK-API012SUB1-RATE PIC X(03).
      *
           03  WRK-ORCSONSHI001-OUT-AREA.
               05  WRK-ORCSONSHI001-OUT-OCC OCCURS 2.
      *            被保険者証区分(確認結果内容)
                   07  WRK-LNK-CARD-CLASS
                                   PIC X(02).
      *            資格状態
                   07  WRK-LNK-ONSHI001-SIKAKU-YUKO
                                   PIC X(01).
      *            資格状態内容
                   07  WRK-LNK-ONSHI001-SIKAKU-YUKO-NM
                                   PIC X(50).
      *
           03  WRK-FUJYO-HEIYO-AREA.
               05  WRK-FUJYO-UUID  PIC X(36).
               05  WRK-HKN-TBL-UUID
                                   PIC X(36).
               05  WRK-SEH-TBL-UUID
                                   PIC X(36).
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
      * 
      *    退避患者保険情報
       01  WRK-PTHKNINF-AREA.
           02  WRK-PTHKNINF-OK     PIC 9(01). 
           02  WRK-PTHKNINF-REC.   
           COPY    "CPPTHKNINF.INC"
                                   REPLACING   //PTHKN//
                                   BY          //WRK-PTHKN//.
      *
      *    予約の資格確認テーブル
       01  YYK-ONSKAKU-AREA. 
           02  YYK-ONSKAKU-REC     OCCURS  2.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //YYK-ONSKAKU//.
      *
      *    退避資格確認テーブル
       01  HKN-ONSKAKU-REC.       
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //HKN-ONSKAKU//.
       01  SEH-ONSKAKU-REC.       
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //SEH-ONSKAKU//.
      *
      *    併用時退避用資格確認テーブル
       01  WRK-ONSKAKU-REC.       
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //WRK-ONSKAKU//.
      *
      *    併用時退避用資格確認テーブル
       01  HEIYO-ONSKAKU-AREA.
      *    1:保険 2:生保 
           02  HEIYO-ONSKAKU-REC   OCCURS  2.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING   //ONSKAKU//
                                   BY          //HEIYO-ONSKAKU//.
      * 
      *    退避患者公費情報
       01  WRK-PTKOHINF-AREA.
           02  WRK-PTKOHINF-OK     PIC 9(01). 
           02  WRK-PTKOHINF-REC.   
           COPY    "CPPTKOHINF.INC"
                                   REPLACING   //PTKOH//
                                   BY          //WRK-PTKOH//.
      *
       01  WRK-CONS-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "P02V001".
      *    ジョブ管理ＤＢのキー値（オンライン診療）
           03  WRK-CONS-ONLINE-JOB-SHELLID
                                   PIC X(08)   VALUE   "P02O001".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
           03  WRK-CONS-JOB-JOBID-MAX
                                   PIC 9(07)   VALUE   100.

      ******************************************************************
      *    ファイルレイアウト
      ******************************************************************
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
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1051.INC".
      *
      *    処方箋管理
       01  SHOHO-KANRI-REC.
           COPY    "CPSHOHO-KANRI.INC".
      *     
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    資格確認　医療扶助交付番号格納
       01  ONSHI-AIDLST-REC.
           COPY    "CPONSHI-AIDLST.INC".
      *
      *    資格確認訪問診療
       01  ONSHI-HOUMON-REC.
           COPY    "CPONSHI-HOUMON.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      ******************************************************************
      *    サブプロ用領域
      ******************************************************************
      *
           COPY    "CPPUSHONLINEQUA.INC". 
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    補助区分変換パラメタ（API用）
           COPY    "CPORAPI012SUB1.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  ST                      PIC 9(2).
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "CPORCSONSHI001.INC".
      *
      *****************************************************************
      *
       PROCEDURE                   DIVISION    USING
                                   ORCSONSHI001AREA
                                   SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      ******************************************************************
      *    初期処理
      ******************************************************************
       100-INIT-SEC                SECTION.
      *
         DISPLAY "LNK-ONSHI001-SYOKBN     =" LNK-ONSHI001-SYOKBN
         DISPLAY "LNK-ONSHI001-PTID       =" LNK-ONSHI001-PTID
         DISPLAY "LNK-ONSHI001-TBL-UUID   =" LNK-ONSHI001-TBL-UUID
         DISPLAY "LNK-ONSHI001-STARTYMD   =" LNK-ONSHI001-STARTYMD
         DISPLAY "LNK-ONSHI001-ENDYMD     =" LNK-ONSHI001-ENDYMD
         DISPLAY "LNK-ONSHI001-ONSHISINRYO=" LNK-ONSHI001-ONSHISINRYO
         DISPLAY "LNK-ONSHI001-OUT-TBL-UUID=" LNK-ONSHI001-OUT-TBL-UUID
         DISPLAY "LNK-ONSHI001-ONSHINRYO-KBN=" 
                                          LNK-ONSHI001-ONSHINRYO-KBN
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO            TO  LNK-ONSHI001-RC
           INITIALIZE                  ORCSONSHI001-OUT-AREA
      *     
      *    マシン日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
      *
      *    オンライン資格確認照会情報取得 
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1051"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 960-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1051-REC
           ELSE
               INITIALIZE                  SYS-1051-REC
           END-IF
           IF      SYS-1051-ONSHIKAKU  =   SPACE
               MOVE    "0"                 TO  SYS-1051-ONSHIKAKU
                                               SYS-1051-FUJYO
           END-IF
           IF      SYS-1051-FUJYO      =   SPACE
               IF      SYS-1051-ONSHIKAKU  =   "1"
               MOVE    "1"                 TO  SYS-1051-FUJYO
               END-IF
           END-IF
      *
      *    処理区分チェック
           EVALUATE    LNK-ONSHI001-SYOKBN
               WHEN    "01"
               WHEN    "02"
               WHEN    "03"
               WHEN    "04"
               WHEN    "05"    
               WHEN    "06"
               WHEN    "07"
                   CONTINUE
               WHEN    OTHER
                   MOVE    1               TO  LNK-ONSHI001-RC
                   MOVE    1               TO  FLG-END
                   GO  TO  100-INIT-EXT
           END-EVALUATE
      *
      *    業務コードチェック
           EVALUATE    LNK-ONSHI001-GYOUMUCD
               WHEN    "11"
               WHEN    "12"
               WHEN    "21"
               WHEN    "99"
               WHEN    SPACE     
                  CONTINUE
               WHEN    OTHER
                   MOVE    2               TO  LNK-ONSHI001-RC
                   MOVE    1               TO  FLG-END
                   GO  TO  100-INIT-EXT
           END-EVALUATE
      *
      *    TBL_UUID
      **      IF      LNK-ONSHI001-TBL-UUID
      **                           NOT =   SPACE
      **      END-IF                       
      *
      *    患者ID
           IF      LNK-ONSHI001-PTID
                                   NOT NUMERIC
               MOVE    4                   TO  LNK-ONSHI001-RC
               MOVE    1                   TO  FLG-END
               GO  TO  100-INIT-EXT
           END-IF                       
            .
       100-INIT-EXT.
           EXIT.
      *
      ******************************************************************
      *    主処理
      ******************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  WRK-PTKOHINF-AREA
                                       WRK-PTHKNINF-AREA
                                       YYK-ONSKAKU-AREA
                                       HKN-ONSKAKU-REC
                                       SEH-ONSKAKU-REC
                                       WRK-ONSKAKU-REC
                                       HEIYO-ONSKAKU-AREA

      *
      *    PTIDが未設定のとき
           IF      LNK-ONSHI001-PTID
                                   =   0
               IF      LNK-ONSHI001-SYOKBN
                                       =   "05"    OR  "06"    OR  "07"
                   CONTINUE
               ELSE
                   MOVE    "No"            TO  LNK-ONSHI001-KAKUNIN-FLG
               END-IF
           ELSE
      *        テスト患者のとき
               INITIALIZE                  PTINF-REC
               MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
               MOVE    LNK-ONSHI001-PTID
                                       TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       =   ZERO 
                   IF      PTINF-TSTPTNUMKBN
                                           =   "1"
                       MOVE    "No"        TO  LNK-ONSHI001-KAKUNIN-FLG
                       MOVE    "D"         TO  LNK-ONSHI001-SIKAKU-YUKO
                   END-IF
               ELSE        
                   MOVE    "No"            TO  LNK-ONSHI001-KAKUNIN-FLG
                   MOVE    "B"             TO  LNK-ONSHI001-SIKAKU-YUKO
               END-IF
           END-IF
      *
      *    氏名、カナ氏名の空白を除く 
           MOVE    1                   TO  IDY
           PERFORM VARYING IDX FROM    1   BY  2
                   UNTIL   IDX >       49
                   OR      PTINF-KANANAME (IDX:1) =   SPACE
               IF      PTINF-KANANAME (IDX:2) =  "　" 
                   CONTINUE
               ELSE    
                   MOVE    PTINF-KANANAME (IDX:2)
                                       TO  WRK-PT-KANANAME (IDY:2)
                   ADD     2           TO  IDY
               END-IF
           END-PERFORM
           MOVE    1                   TO  IDY
           PERFORM VARYING IDX FROM    1   BY  2
                   UNTIL   IDX >       49
                   OR      PTINF-NAME (IDX:1) =   SPACE
               IF      PTINF-NAME (IDX:2) =  "　" 
                   CONTINUE
               ELSE    
                   MOVE    PTINF-NAME (IDX:2)
                                       TO  WRK-PT-NAME (IDY:2)
                   ADD     2           TO  IDY
               END-IF
           END-PERFORM
      *    カナ氏名を姓と名に分ける（分けれない場合クリア）             
            UNSTRING  PTINF-KANANAME  DELIMITED  BY  "　" 
                                   INTO    WRK-PT-KANANAME11
                                           WRK-PT-KANANAME12
           END-UNSTRING
           IF    ( WRK-PT-KANANAME11   =   SPACE )
           OR    ( WRK-PT-KANANAME12   =   SPACE )
               MOVE    SPACE               TO  WRK-PT-KANANAME1
           END-IF
      *
           IF      LNK-ONSHI001-KAKUNIN-FLG   =   SPACE
               IF    ( LNK-ONSHI001-SYOKBN
                                       =   "05"    OR  "06"
                                       OR  "07"             )
               AND   ( LNK-ONSHI001-PTID
                                       =   0                )
                   CONTINUE
               ELSE
      *            患者番号を取得
                   INITIALIZE                      ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                   MOVE    LNK-ONSHI001-PTID   TO  SPTID-PTID
                   CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
                   IF      SPTID-RC            =   ZERO
                       MOVE    SPTID-PTNUM     TO  WRK-PTNUM
                   ELSE
                       DISPLAY "ORCSPTID ERROR"
                   END-IF
               END-IF
      *
               EVALUATE    LNK-ONSHI001-SYOKBN
                   WHEN    "01"
      *                資格確認情報返却             
                       PERFORM 2001-ONSHI-KAKU-HENKYAKU-SEC
                       MOVE    LNK-ONSHI001-KAKUNIN-FLG
                                               TO
                                           WRK-LNK-ONSHI001-KAKUNIN-FLG
                       MOVE    SPACE           TO
                                           LNK-ONSHI001-KAKUNIN-FLG
      *                診療行為で資格確認を行うとき
                       IF    ( LNK-ONSHI001-GYOUMUCD
                                               =   "21" )
                       AND   ( LNK-ONSHI001-ONSHISINRYO
                                               =   "1"  )
      *                    資格確認要否依頼             
                           PERFORM 2002-ONSHI-KAKU-IRAI-SEC
                           IF      LNK-ONSHI001-KAKUNIN-FLG
                                                  =   "Yes"
      *                        併用の時は保険のPUSH処理を行う                     
                               IF    ( WRK-PTHKNINF-OK =   1 )
                               AND   ( WRK-PTKOHINF-OK =   1 )
                                   MOVE    WRK-SEH-TBL-UUID
                                                   TO  ONSKAKU-TBL-UUID
                               END-IF
                               PERFORM 2003-PUSH-SEC
                           END-IF
                       END-IF   
                       MOVE    WRK-LNK-ONSHI001-KAKUNIN-FLG
                                               TO
                                               LNK-ONSHI001-KAKUNIN-FLG
      *                患者保険情報とのチェック処理
      *****************PERFORM 2004-ONSHI-KAKU-CHECK-SEC
                   WHEN    "02"
      *                資格確認要否依頼             
                       PERFORM 2002-ONSHI-KAKU-IRAI-SEC
      *                患者保険情報とのチェック処理
      *****************PERFORM 2004-ONSHI-KAKU-CHECK-SEC
                   WHEN    "03"
      *                資格確認情報返却             
      ****             PERFORM 2001-ONSHI-KAKU-HENKYAKU-SEC
      *                資格確認要否依頼（強制）             
                       PERFORM 2005-ONSHI-KAKU-IRAI-SEC
                       IF      LNK-ONSHI001-KAKUNIN-FLG
                                               =   "Yes"
      *                    併用の時は生保のPUSH処理を行う                     
                           IF    ( WRK-PTHKNINF-OK =   1 )
                           AND   ( WRK-PTKOHINF-OK =   1 )
                               MOVE    WRK-SEH-TBL-UUID
                                               TO  ONSKAKU-TBL-UUID
                           END-IF
                           PERFORM 2003-PUSH-SEC
                       END-IF
                   WHEN    "04"
      *                患者番号変更時強制資格確認
                       PERFORM 2006-ONSHI-KAKU-IRAI-SEC
      *****            IF      LNK-ONSHI001-KAKUNIN-FLG
      *****                                    =   "Yes"
      *                    併用の時は生保のPUSH処理を行う                     
      *****                IF    ( WRK-PTHKNINF-OK =   1 )
      *****                AND   ( WRK-PTKOHINF-OK =   1 )
      *****                    MOVE    WRK-SEH-TBL-UUID
      *****                                    TO  ONSKAKU-TBL-UUID
      *****                END-IF
      *****                PERFORM 2003-PUSH-SEC
      *****            END-IF
                   WHEN    "05"
      *                保険証ＯＣＲ再照会
                       PERFORM 2007-HKNOCR-SHOKAI-SEC
                   WHEN    "06"
                   WHEN    "07"
      *                訪問診療、オンライン診療同意一括取得
                       PERFORM 2010-HOUMON-IKKATU-SHOKAI-SEC
               END-EVALUATE
           END-IF
      *
      *    資格状態内容返却処理
           PERFORM 2008-SIKAKU-YUKO-NM-SEC
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格状態内容返却処理
      ******************************************************************
       2008-SIKAKU-YUKO-NM-SEC     SECTION.
      *
           MOVE    SPACE           TO  LNK-ONSHI001-SIKAKU-YUKO-NM
      *
           EVALUATE    LNK-ONSHI001-SIKAKU-YUKO
               WHEN    "Q"
                   MOVE    "1"         TO  LNK-ONSHI001-SIKAKU-YUKO
                   MOVE    "有効（本人家族区分　誤）"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "R"
                   MOVE    "有効（負担割合　誤）"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "S"
                   MOVE    "有効（本人家族区分、負担割合　誤）"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "T"
                   MOVE    "有効（医療扶助　未登録）"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "U"
                   MOVE    "致命的エラー(保険者情報、生年月日不正等)"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "1"
                   MOVE    "有効"      TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "2"
                   MOVE    "無効"      TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "3"
                   MOVE    "無効（新しい資格あり）"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "4"
                   MOVE    "該当資格なし"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "5"
                   MOVE    "複数該当"  TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "6"
                   MOVE    "保険の番号未入力"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "9"
                   MOVE    "資格確認中"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "A"
                   MOVE    "資格レコード無し"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "B"
                   MOVE    "該当患者なし"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "C"
                   STRING  "枝番未設定（"  DELIMITED   BY  SIZE
                           LNK-ONSHI001-EDABAN
                                           DELIMITED   BY  SIZE
                           "）"            DELIMITED   BY  SIZE
                                   INTO    LNK-ONSHI001-SIKAKU-YUKO-NM
                   END-STRING                
               WHEN    "D"
                   MOVE    "テスト患者"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "E"
                   MOVE    "疑資格"    TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "F"
                   MOVE    "保険変更"  TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               WHEN    "G"
                   MOVE    "保険変更（特別療養費関連）"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
      *
               WHEN    "W"
                   IF      WRK-LNK-CARD-CLASS (1)
                                       NOT =   "A1" 
                       STRING  "主保険："      DELIMITED   BY  SIZE
                               WRK-LNK-ONSHI001-SIKAKU-YUKO-NM (1)
                                               DELIMITED   BY  SPACE
                               "　生保："      DELIMITED   BY  SIZE
                               WRK-LNK-ONSHI001-SIKAKU-YUKO-NM (2)
                                               DELIMITED   BY  SPACE
                                   INTO    LNK-ONSHI001-SIKAKU-YUKO-NM
                       END-STRING
                   ELSE
                       STRING  "主保険："      DELIMITED   BY  SIZE
                               WRK-LNK-ONSHI001-SIKAKU-YUKO-NM (2)
                                               DELIMITED   BY  SPACE
                               "　生保："      DELIMITED   BY  SIZE
                               WRK-LNK-ONSHI001-SIKAKU-YUKO-NM (1)
                                               DELIMITED   BY  SPACE
                                   INTO    LNK-ONSHI001-SIKAKU-YUKO-NM
                       END-STRING
                   END-IF    
               WHEN    OTHER
                   DISPLAY "YUKO OTHER=" LNK-ONSHI001-SIKAKU-YUKO "#"
                   MOVE    "Z"         TO  LNK-ONSHI001-SIKAKU-YUKO
                   MOVE    "その他のエラー"
                                       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
           END-EVALUATE
           .
       2008-SIKAKU-YUKO-NM-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認情報返却処理
      ******************************************************************
       2001-ONSHI-KAKU-HENKYAKU-SEC    SECTION.
      *
      *    有効な主保険の退避処理（医保分のみ対象）
           PERFORM 20011-PTHKNINF-TAIHI-SEC
      *****IF      WRK-PTHKNINF-OK =   ZERO
      *        有効な医療扶助の退避処理
               PERFORM 20013-PTKOHINF-TAIHI-SEC
      *****END-IF    
      *
      *    予約の資格確認レコードのチェック
           PERFORM 20012-YYK-CHECK-SEC
           IF      FLG-YYK-CHK     =   9
               MOVE    YYK-ONSKAKU-REC (1)
                                       TO  ONSKAKU-REC  
      *        資格確認結果チェック
               PERFORM 20014-SIKAKU-YUKO-CHECK-SEC
               MOVE    LNK-ONSHI001-SIKAKU-YUKO
                                       TO
                                       WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
               MOVE    ONSKAKU-CARD-CLASS
                                       TO  WRK-LNK-CARD-CLASS       (1)
               IF      YYK-ONSKAKU-PTID (2)    >   ZERO
                   MOVE    YYK-ONSKAKU-REC (2)
                                           TO  ONSKAKU-REC  
      *            資格確認結果チェック
                   PERFORM 20014-SIKAKU-YUKO-CHECK-SEC
                   MOVE    LNK-ONSHI001-SIKAKU-YUKO
                                           TO
                                       WRK-LNK-ONSHI001-SIKAKU-YUKO (2)
                   MOVE    ONSKAKU-CARD-CLASS
                                           TO  WRK-LNK-CARD-CLASS   (2)
      *            資格状態内容返却処理
                   EVALUATE    WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
                                           ALSO
                                       WRK-LNK-ONSHI001-SIKAKU-YUKO (2)
                       WHEN    "1"     ALSO    "1"
                           MOVE    "1" TO  LNK-ONSHI001-SIKAKU-YUKO
                           INITIALIZE      WRK-ORCSONSHI001-OUT-AREA
                       WHEN    "9"     ALSO    "9"
                           MOVE    "9" TO  LNK-ONSHI001-SIKAKU-YUKO
                       WHEN    OTHER
                           MOVE    WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
                                       TO  LNK-ONSHI001-SIKAKU-YUKO
                           PERFORM 2008-SIKAKU-YUKO-NM-SEC
                           MOVE    LNK-ONSHI001-SIKAKU-YUKO-NM
                                           TO
                                   WRK-LNK-ONSHI001-SIKAKU-YUKO-NM (1)
                           MOVE    WRK-LNK-ONSHI001-SIKAKU-YUKO (2)
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                           PERFORM 2008-SIKAKU-YUKO-NM-SEC
                           MOVE    LNK-ONSHI001-SIKAKU-YUKO-NM
                                           TO
                                   WRK-LNK-ONSHI001-SIKAKU-YUKO-NM (2)
                           MOVE    "W" TO  LNK-ONSHI001-SIKAKU-YUKO
                   END-EVALUATE    
                   MOVE    SPACE       TO  LNK-ONSHI001-SIKAKU-YUKO-NM
               ELSE
                   MOVE    WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
               END-IF
               GO  TO  2001-ONSHI-KAKU-HENKYAKU-EXT
           END-IF
      *
      *    資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
           IF      FLG-ONSHI-KAKU  =   1
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    LNK-ONSHI001-PTID
                                       TO  ONSKAKU-PTID
               MOVE    "2"             TO  ONSKAKU-ORCA-SHOKAI-KBN
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
               IF      FLG-ONSHI-KAKU  =   1
                   MOVE    "A"         TO  LNK-ONSHI001-SIKAKU-YUKO
               END-IF
           END-IF    
           IF      FLG-ONSHI-KAKU  =   ZERO
      *        オンライン資格情報編集処理
               PERFORM 2009-ONS-KAKU-HENSYU-SEC
      *
               MOVE    ONSKAKU-TBL-UUID
                                       TO  LNK-ONSHI001-OUT-TBL-UUID
      *        資格確認結果チェック
               PERFORM 20014-SIKAKU-YUKO-CHECK-SEC
               MOVE    LNK-ONSHI001-SIKAKU-YUKO
                                       TO
                                       WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
               MOVE    ONSKAKU-CARD-CLASS
                                       TO  WRK-LNK-CARD-CLASS       (1)
      *        併用のとき、併用の資格確認取得
               IF      ONSKAKU-FUJYO-HEIYO
                                       =   "1"
                   MOVE    ZERO            TO  FLG-ONSHI-HEIYO
                   MOVE    ONSKAKU-REC     TO  WRK-ONSKAKU-REC
      *
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
                   MOVE    WRK-ONSKAKU-FUJYO-UUID1
                                           TO  ONSKAKU-FUJYO-UUID1
                   MOVE    WRK-ONSKAKU-TBL-UUID
                                           TO  ONSKAKU-TBL-UUID
                   MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                   PERFORM 900-ONSHI-KAKU-READ-KEY28-SEC
                   IF      FLG-ONSHI-HEIYO =   1
      *                オンライン資格情報編集処理
                       PERFORM 2009-ONS-KAKU-HENSYU-SEC
      *                資格確認結果チェック
                       PERFORM 20014-SIKAKU-YUKO-CHECK-SEC
                       MOVE    LNK-ONSHI001-SIKAKU-YUKO
                                           TO
                                       WRK-LNK-ONSHI001-SIKAKU-YUKO (2)
                       MOVE    ONSKAKU-CARD-CLASS
                                           TO  WRK-LNK-CARD-CLASS   (2)
      *                資格状態内容返却処理
                       EVALUATE    WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
                                           ALSO
                                       WRK-LNK-ONSHI001-SIKAKU-YUKO (2)
                           WHEN    "1"     ALSO    "1"
                               MOVE    "1" TO  LNK-ONSHI001-SIKAKU-YUKO
                               INITIALIZE      WRK-ORCSONSHI001-OUT-AREA
                           WHEN    "9"     ALSO    "9"
                               MOVE    "9" TO  LNK-ONSHI001-SIKAKU-YUKO
                           WHEN    OTHER
                               MOVE    WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                               PERFORM 2008-SIKAKU-YUKO-NM-SEC
                               MOVE    LNK-ONSHI001-SIKAKU-YUKO-NM
                                           TO
                                   WRK-LNK-ONSHI001-SIKAKU-YUKO-NM (1)
                               MOVE    WRK-LNK-ONSHI001-SIKAKU-YUKO (2)
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                               PERFORM 2008-SIKAKU-YUKO-NM-SEC
                               MOVE    LNK-ONSHI001-SIKAKU-YUKO-NM
                                           TO
                                   WRK-LNK-ONSHI001-SIKAKU-YUKO-NM (2)
                               MOVE    "W" TO  LNK-ONSHI001-SIKAKU-YUKO
                       END-EVALUATE    
                       MOVE    SPACE       TO
                                           LNK-ONSHI001-SIKAKU-YUKO-NM
                   ELSE
                       DISPLAY "system err heiyou not found fujyo-uuid="
                               WRK-ONSKAKU-FUJYO-UUID1
                       MOVE    WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                   END-IF
      *
                   MOVE    WRK-ONSKAKU-REC
                                       TO  ONSKAKU-REC
               ELSE
                   MOVE    WRK-LNK-ONSHI001-SIKAKU-YUKO (1)
                                       TO  LNK-ONSHI001-SIKAKU-YUKO
               END-IF 
           END-IF
           .
       2001-ONSHI-KAKU-HENKYAKU-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認結果チェック処理
      ******************************************************************
       20014-SIKAKU-YUKO-CHECK-SEC     SECTION.
      *
           MOVE    SPACE        TO  LNK-ONSHI001-SIKAKU-YUKO
      *
           IF      ONSKAKU-KAKU-ENDFLG
                                   =   SPACE   OR  "01"
               MOVE    "9"      TO  LNK-ONSHI001-SIKAKU-YUKO
      *        保険情報の番号が空白のとき
               IF    ( ONSKAKU-CARD-CLASS
                                       NOT =   "A1"  )
               AND   ( ONSKAKU-SHO-NUM     =   SPACE )
                   MOVE    "6"          TO  LNK-ONSHI001-SIKAKU-YUKO
               END-IF        
           ELSE
      *        致命的エラー(保険者情報、生年月日不正等)のとき
               IF    ( ONSKAKU-SIKAKU-YUKO
                                       =   SPACE )
                   MOVE    "U"     TO  LNK-ONSHI001-SIKAKU-YUKO
                   GO  TO  20014-SIKAKU-YUKO-CHECK-EXT
               END-IF
      *        疑資格のチェック
               IF    ( ONSKAKU-SIKAKU-YUKO
                                       =   "1" )
               AND   ( ONSKAKU-SHOKAI-KBN
                                       =   "2" )
                   IF    ( WRK-RES-NAME
                                       NOT =   WRK-PT-NAME     )
                   AND   ( WRK-RES-KANANAME
                                       NOT =   WRK-PT-KANANAME )
                       IF  WRK-RES-KANANAME1   NOT =   SPACE
                           IF      WRK-RES-KANANAME12
                                       NOT =   WRK-PT-KANANAME12
                               MOVE    "E" TO  LNK-ONSHI001-SIKAKU-YUKO
                               GO  TO  20014-SIKAKU-YUKO-CHECK-EXT
                           END-IF
                       ELSE
                           MOVE    "E"     TO  LNK-ONSHI001-SIKAKU-YUKO 
                           GO  TO  20014-SIKAKU-YUKO-CHECK-EXT
                       END-IF   
                   END-IF
               END-IF
      *
               IF    ( ONSKAKU-SIKAKU-YUKO
                                       =   "1"         )
               AND   ( ONSKAKU-SHOKAI-KBN
                                       =   "1" OR  "2" )
                   IF      ONSKAKU-CARD-CLASS  NOT =   "A1"
                       IF      WRK-PTHKNINF-OK =   1
                          IF    ( WRK-PTHKN-EDABAN     =   SPACE  )
                          AND   ( WRK-PTHKN-HKNNUM
                                                    NOT =   "039" )     
                               MOVE    "C" TO  LNK-ONSHI001-SIKAKU-YUKO
      *                        枝番を返却
                               MOVE    ONSKAKU-RES-EDABAN
                                           TO  LNK-ONSHI001-EDABAN
                           ELSE
                               MOVE    ONSKAKU-SIKAKU-YUKO
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                               EVALUATE    ONSKAKU-SIKAKU-YUKO
                                   WHEN    "1"
                                       IF      WRK-PTHKN-HONKZKKBN
                                           NOT =   ONSKAKU-RES-HONKZKKBN
                                           IF    ( WRK-PTHKN-HKNNUM
                                                           =   "039" )     
                                           AND   ( WRK-PTHKN-HONKZKKBN
                                                           =   "1"   )
                                           AND   ( ONSKAKU-RES-HONKZKKBN
                                                           =   SPACE )
                                               CONTINUE
                                           ELSE
                                               MOVE    "Q"     TO
                                               LNK-ONSHI001-SIKAKU-YUKO
                                           END-IF
                                       END-IF
      *                                負担割合チェック（後期高齢者、高齢受給者）
                                       PERFORM 200213-FTNRATE-CHK-SEC
                               END-EVALUATE
                           END-IF
                       ELSE
                           DISPLAY "PTHKNINF NOT FOUND"  
                       END-IF
                   ELSE
                       IF      WRK-PTKOHINF-OK =   1
                           MOVE    ONSKAKU-SIKAKU-YUKO
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                       ELSE
                           DISPLAY "PTKOHINF NOT FOUND"  
                       END-IF
                   END-IF
               ELSE
                   EVALUATE    ONSKAKU-SIKAKU-YUKO
                       WHEN    "1"
                           MOVE    ONSKAKU-SIKAKU-YUKO
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                           IF      ONSKAKU-CARD-CLASS  NOT =   "A1"
                               IF      WRK-PTHKNINF-OK =   1
                                   IF      WRK-PTHKN-HONKZKKBN
                                           NOT =   ONSKAKU-RES-HONKZKKBN
                                       IF    ( WRK-PTHKN-HKNNUM
                                                       =   "039" )     
                                       AND   ( WRK-PTHKN-HONKZKKBN
                                                       =   "1"   )
                                       AND   ( ONSKAKU-RES-HONKZKKBN
                                                       =   SPACE )
                                           CONTINUE
                                       ELSE
                                           MOVE    "Q"
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                                       END-IF        
                                   END-IF
                               END-IF
                           END-IF
                       WHEN    "2"
                       WHEN    "3"
                       WHEN    "5"
                           MOVE    ONSKAKU-SIKAKU-YUKO
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                       WHEN    "4"
      ***                  IF      ONSKAKU-RES-EDABAN
      ***                                      =   SPACE
      ***                      MOVE    "5" TO  LNK-ONSHI001-SIKAKU-YUKO
      ***                  ELSE
                               MOVE    "4" TO  LNK-ONSHI001-SIKAKU-YUKO
      ***                  END-IF
                       WHEN    "6"
                           MOVE    "T"     TO  LNK-ONSHI001-SIKAKU-YUKO
                   END-EVALUATE
               END-IF
           END-IF
      *
      *****IF    ( ONSKAKU-SHOKAI-KBN  =   "1"  )
           IF    ( LNK-ONSHI001-SIKAKU-YUKO
                                       =   "1"  )
           AND   ( ONSKAKU-PTID        >   ZERO )
           AND   ( ONSKAKU-CARD-CLASS
                                   NOT =   "A1" )
      *        資格確認のＰＴＩＤから有効な主保険とチェック処理（医保分のみ対象）
               PERFORM 20012-PTHKNINF-CHK-SEC
           END-IF
           .
       20014-SIKAKU-YUKO-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *    有効な主保険の退避処理（医保分のみ対象）
      ******************************************************************
       20011-PTHKNINF-TAIHI-SEC    SECTION.
      *
           INITIALIZE                  WRK-PTHKNINF-AREA
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  PTHKN-PTID
           MOVE    SMCNDATE-YMD    TO  PTHKN-TEKSTYMD
                                       PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           PERFORM         UNTIL   FLG-PTHKNINF        =   1
                           OR      WRK-PTHKNINF-OK     =   1
               IF      PTHKN-HKNNUM (1:1)  =   "0"
                   MOVE    PTHKNINF-REC    TO  WRK-PTHKNINF-REC
      *??
             DISPLAY "WRK-PTHKN-HKNJANUM=" WRK-PTHKN-HKNJANUM
                                 " EDABAN=" WRK-PTHKN-EDABAN "#"
      *??                
                   MOVE    1               TO  WRK-PTHKNINF-OK
               ELSE    
                   MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM 900-PTHKNINF-READ-N-SEC
               END-IF    
           END-PERFORM 
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20011-PTHKNINF-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *    公費情報から医療扶助の患者公費情報を検索
      ******************************************************************
       20013-PTKOHINF-TAIHI-SEC    SECTION.
      *
           INITIALIZE                  WRK-PTKOHINF-AREA
      *
      *    医療扶助の資格確認をしないときは検索しない
           IF      SYS-1051-FUJYO  =   "1"
               CONTINUE
           ELSE
               GO  TO  20013-PTKOHINF-TAIHI-EXT
           END-IF    
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  PTKOH-PTID
           MOVE    "012"           TO  PTKOH-KOHNUM
           MOVE    SMCNDATE-YMD    TO  PTKOH-TEKSTYMD
                                       PTKOH-TEKEDYMD
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key6"         TO  MCP-PATHNAME
               PERFORM 900-PTKOHINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           PERFORM         UNTIL   FLG-PTKOHINF        =   1
                           OR      WRK-PTKOHINF-OK     =   1
               MOVE    PTKOHINF-REC    TO  WRK-PTKOHINF-REC
               MOVE    1               TO  WRK-PTKOHINF-OK
      *    
               MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 900-PTKOHINF-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    有効な医療扶助があるとき受給者番号が空白のときは
      *    最初に検索した医療扶助から月代わり受給者番号を検索
           IF      WRK-PTKOHINF-OK     =   1
               IF      WRK-PTKOH-JKYSNUM   =   SPACE
                   INITIALIZE                      MONTHLYNUM-REC
                   MOVE    SPA-HOSPNUM         TO  MONTHLYNUM-HOSPNUM
                   MOVE    LNK-ONSHI001-PTID   TO  MONTHLYNUM-PTID
                   MOVE    WRK-PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                   MOVE    SMCNDATE-YMD (1:6)  TO  MONTHLYNUM-SRYYM
                   MOVE    "%"                 TO  MONTHLYNUM-NYUGAIKBN
                   MOVE    MONTHLYNUM-REC      TO  MCPDATA-REC
                   PERFORM 900-MONTHLYNUM-READ-SEC
                   IF    ( FLG-MONTHLYNUM          =   ZERO  )
                       MOVE    MONTHLYNUM-JKYSNUM 
                                               TO  WRK-PTKOH-JKYSNUM
                   END-IF
               END-IF
               display "20013=" wrk-ptkoh-ptid "#" WRK-PTKOH-FTNJANUM
               " " wrk-ptkoh-jkysnum
      *        負担者番号・受給者番号の設定がないとき
               IF    ( WRK-PTKOH-JKYSNUM   =   SPACE )
               OR    ( WRK-PTKOH-FTNJANUM  =   SPACE )
                   MOVE    ZERO                TO  WRK-PTKOHINF-OK
               END-IF    
           END-IF
      *
           .
       20013-PTKOHINF-TAIHI-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認レコードのチェック（予約一括資格確認）
      ******************************************************************
       20012-YYK-CHECK-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-YYK-CHK
           INITIALIZE                  YYK-ONSKAKU-AREA
      *
      *    予約の資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    "1"             TO  ONSKAKU-ORCA-SHOKAI-KBN
           MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY20-SEC
           IF      FLG-ONSHI-KAKU  =   ZERO
      *        予約の資格有効性が有効、双子以外のとき
                IF    ( ONSKAKU-SIKAKU-YUKO
                                   NOT =   "1" AND "5" )
                   IF      WRK-PTHKNINF-OK =   1
                       IF    ( ONSKAKU-SHO-HKNJANUM
                                           =   WRK-PTHKN-HKNJANUM )
                       AND   ( ONSKAKU-SHO-KIGO
                                           =   WRK-PTHKN-KIGO     )
                       AND   ( ONSKAKU-SHO-NUM
                                           =   WRK-PTHKN-NUM      )
                       AND   ( ONSKAKU-SHO-EDABAN
                                           =   WRK-PTHKN-EDABAN   )
                           MOVE    1           TO  FLG-YYK-CHK
                           MOVE    ONSKAKU-REC TO  YYK-ONSKAKU-REC (1)
                       END-IF 
                   END-IF
                   IF      FLG-YYK-CHK     =   ZERO
                       IF      WRK-PTKOHINF-OK =   1
                           IF    ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTKOH-FTNJANUM )
                           AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTKOH-JKYSNUM  )
                               MOVE    1       TO  FLG-YYK-CHK
                               MOVE    ONSKAKU-REC
                                               TO  YYK-ONSKAKU-REC (1)
                           END-IF
                       END-IF
                   END-IF            
               END-IF
      *
               IF      FLG-YYK-CHK     =   1
      *            併用のとき、併用の資格確認取得
                   IF      YYK-ONSKAKU-FUJYO-HEIYO (1)
                                           =   "1"
                       MOVE    ZERO            TO  FLG-ONSHI-HEIYO
      *
                       INITIALIZE                  ONSKAKU-REC
                       MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
                       MOVE    YYK-ONSKAKU-FUJYO-UUID1 (1)
                                               TO  ONSKAKU-FUJYO-UUID1
                       MOVE    YYK-ONSKAKU-TBL-UUID    (1)
                                               TO  ONSKAKU-TBL-UUID
                       MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                       PERFORM 900-ONSHI-KAKU-READ-KEY28-SEC
                       IF      FLG-ONSHI-HEIYO =   1
                           IF    ( ONSKAKU-SIKAKU-YUKO
                                               NOT =   "1" AND "5" )
                               IF      WRK-PTHKNINF-OK =   1
                                   IF    ( ONSKAKU-SHO-HKNJANUM
                                           =   WRK-PTHKN-HKNJANUM )
                                   AND   ( ONSKAKU-SHO-KIGO
                                           =   WRK-PTHKN-KIGO     )
                                   AND   ( ONSKAKU-SHO-NUM
                                           =   WRK-PTHKN-NUM      )
                                   AND   ( ONSKAKU-SHO-EDABAN
                                           =   WRK-PTHKN-EDABAN   )
                                       MOVE    2   TO  FLG-YYK-CHK
                                       MOVE    ONSKAKU-REC
                                                   TO
                                                   YYK-ONSKAKU-REC (2)
                                   END-IF
                               END-IF     
                               IF      FLG-YYK-CHK     =   1
                                    IF      WRK-PTKOHINF-OK =   1
                                       IF    ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTKOH-FTNJANUM )
                                       AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTKOH-JKYSNUM  )
                                           MOVE    2   TO  FLG-YYK-CHK
                                           MOVE    ONSKAKU-REC
                                                   TO
                                                   YYK-ONSKAKU-REC (2)
                                       END-IF
                                   END-IF
                               END-IF
                           ELSE
                               MOVE    ZERO        TO  FLG-YYK-CHK
                               INITIALIZE              YYK-ONSKAKU-AREA
                           END-IF
                       END-IF
                   END-IF        
               END-IF
           END-IF    
      *
           IF      FLG-YYK-CHK         =   ZERO
               GO  TO  20012-YYK-CHECK-EXT
           END-IF
      *
      *    保険証ＯＣＲの資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    "2"             TO  ONSKAKU-ORCA-SHOKAI-KBN
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
            IF      FLG-ONSHI-KAKU  =   ZERO
               GO  TO  20012-YYK-CHECK-EXT
           END-IF
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    "3"             TO  ONSKAKU-ORCA-SHOKAI-KBN
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
           IF      FLG-ONSHI-KAKU  =   ZERO
               GO  TO  20012-YYK-CHECK-EXT
           END-IF
      *
      *    顔認証の資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    SPACE           TO  ONSKAKU-ORCA-SHOKAI-KBN
           MOVE    "1"             TO  ONSKAKU-SHOKAI-KBN
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY20-SEC
           IF      FLG-ONSHI-KAKU  =   ZERO
                GO  TO  20012-YYK-CHECK-EXT
          END-IF
      *
      *    保険証の資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    SPACE           TO  ONSKAKU-ORCA-SHOKAI-KBN
           MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY20-SEC
           IF      FLG-ONSHI-KAKU  =   ZERO
                GO  TO  20012-YYK-CHECK-EXT
          END-IF
      *
      *    予約の資格有効性が有効、双子のとき
           IF    ( YYK-ONSKAKU-SIKAKU-YUKO (1)
                                   =   "1" OR  "5" )
               MOVE    8               TO  FLG-YYK-CHK
           ELSE
               MOVE    9               TO  FLG-YYK-CHK
      *
      *        併用の時
               IF      YYK-ONSKAKU-PTID (2)    >   ZERO
                   IF    ( YYK-ONSKAKU-SIKAKU-YUKO (2)
                                           =   "1" OR  "5" )
                       MOVE    8               TO  FLG-YYK-CHK
                   ELSE
                       MOVE    9               TO  FLG-YYK-CHK
                   END-IF
               END-IF
           END-IF
      *
           .
       20012-YYK-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認のＰＴＩＤから有効な主保険とチェック処理（医保分のみ対象）
      ******************************************************************
       20012-PTHKNINF-CHK-SEC    SECTION.
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    ONSKAKU-PTID    TO  PTHKN-PTID
           MOVE    SMCNDATE-YMD    TO  PTHKN-TEKSTYMD
                                       PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_pthkninf"  TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-N-SEC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           PERFORM         UNTIL   FLG-PTHKNINF        =   1
               IF      PTHKN-HKNNUM (1:1)  =   "0"
                   IF    ( ONSKAKU-RES-HKNJANUM
                                               =   PTHKN-HKNJANUM   )
                   AND   ( ONSKAKU-RES-KIGO    =   WRK-PTHKN-KIGO   )
                   AND   ( ONSKAKU-RES-NUM     =   WRK-PTHKN-NUM    )
                   AND   ( ONSKAKU-RES-EDABAN  =   WRK-PTHKN-EDABAN )
      *                特別療養費のとき 
                       IF      ONSKAKU-CARD-CLASS  =   "05"
                           IF      WRK-PTHKN-HKNNUM
                                                  NOT =   "040"   AND
                                                          "068"
                               MOVE    "G"     TO
                                               LNK-ONSHI001-SIKAKU-YUKO
                           END-IF                
                       ELSE
                           IF      WRK-PTHKN-HKNNUM   =   "040"   OR
                                                          "068"
                               MOVE    "G"     TO
                                               LNK-ONSHI001-SIKAKU-YUKO
                            END-IF                
                       END-IF
                   ELSE
                       IF    ( ONSKAKU-SHOKAI-KBN  =   "1"  )
                           MOVE    "F"     TO  LNK-ONSHI001-SIKAKU-YUKO
                       END-IF
                   END-IF
                   MOVE    1               TO  FLG-PTHKNINF
               ELSE    
                   MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM 900-PTHKNINF-READ-N-SEC
               END-IF    
           END-PERFORM 
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20012-PTHKNINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担割合チェック（後期高齢者、高齢受給者）
      *****************************************************************
       200213-FTNRATE-CHK-SEC             SECTION.
      *
      *****IF    ( WRK-PTHKN-HKNNUM    =   "039" )
           IF    ( ONSKAKU-FTNRATE NOT =   SPACE )
           OR    ( ONSKAKU-ELDER-FTNRATE
                                   NOT =   SPACE )
               CONTINUE
           ELSE
               GO  TO  200213-FTNRATE-CHK-EXT
           END-IF
      * 
           INITIALIZE                      ORAPI012SUB1AREA
           MOVE    WRK-PTHKN-HKNNUM    TO  API012SUB1-HKNNUM
           MOVE    WRK-PTHKN-HOJOKBN   TO  API012SUB1-HOJOKBN
           MOVE    SMCNDATE-YMD        TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
      *                                     
           IF      API012SUB1-ERRCD    =   1
               CONTINUE
           ELSE
               MOVE    API012SUB1-RATE     TO  WRK-API012SUB1-RATE
               IF    ( ONSKAKU-FTNRATE NOT =   SPACE )
                   IF      WRK-API012SUB1-RATE
                                       NOT =   ONSKAKU-FTNRATE
                       MOVE    "R"     TO  LNK-ONSHI001-SIKAKU-YUKO
                   END-IF
               ELSE
                   IF      WRK-API012SUB1-RATE
                                       NOT =   ONSKAKU-ELDER-FTNRATE
                       IF      LNK-ONSHI001-SIKAKU-YUKO
                                               =   "Q"
                           MOVE    "S"     TO  LNK-ONSHI001-SIKAKU-YUKO
                       ELSE 
                           MOVE    "R"     TO  LNK-ONSHI001-SIKAKU-YUKO
                       END-IF 
                   END-IF 
               END-IF
          END-IF
      *
           .
       200213-FTNRATE-CHK-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認要否依頼処理
      ******************************************************************
       2002-ONSHI-KAKU-IRAI-SEC    SECTION.
      *
      *    当日以前の資格確認を利用した場合、その情報の利用は行わない
           IF      LNK-ONSHI001-TBL-UUID
                                   NOT =   SPACE
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    LNK-ONSHI001-TBL-UUID
                                       TO  ONSKAKU-TBL-UUID
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               PERFORM 900-ONSHI-KAKU-READ-SEC
               IF      FLG-ONSHI-KAKU  =   ZERO
                   IF      ONSKAKU-KAKUNINYMD
                                       NOT =   SMCNDATE-YMD
                       IF      ONSKAKU-PTID    =   ZERO
      *                     資格確認PTID設定処理                 
                            PERFORM 20021-ONSKAKU-PTID-SET-SEC
                       END-IF
      *                                                 
                       MOVE    SPACE       TO  LNK-ONSHI001-TBL-UUID
                   END-IF
               END-IF
           END-IF
      * 
      *    有効な保険の退避処理（医保分のみ対象）
           PERFORM 20011-PTHKNINF-TAIHI-SEC
      *****IF      WRK-PTHKNINF-OK =   ZERO
      *        有効な医療扶助の退避処理
               PERFORM 20013-PTKOHINF-TAIHI-SEC
      *        有効な保険又は医療扶助が存在しないとき
               IF      WRK-PTHKNINF-OK =   ZERO
                   IF      WRK-PTKOHINF-OK =   ZERO
                       MOVE   "No"         TO  LNK-ONSHI001-KAKUNIN-FLG
                       GO  TO  2002-ONSHI-KAKU-IRAI-EXT
                   ELSE
      *                医療扶助単独で負担者番号、受給者番号が空白のとき
                       IF    ( WRK-PTKOH-FTNJANUM     =   SPACE )
                       OR    ( WRK-PTKOH-JKYSNUM      =   SPACE )
                           MOVE   "No"     TO  LNK-ONSHI001-KAKUNIN-FLG
                           GO  TO  2002-ONSHI-KAKU-IRAI-EXT
                       END-IF
                   END-IF
               END-IF    
      *****END-IF
      *
      *    予約の資格確認レコードのチェック
           PERFORM 20012-YYK-CHECK-SEC
           IF      FLG-YYK-CHK     =   8   OR  9
               IF      FLG-YYK-CHK     =   8
                   MOVE    "Yes"           TO  LNK-ONSHI001-KAKUNIN-FLG
               ELSE    
                   MOVE    "No"            TO  LNK-ONSHI001-KAKUNIN-FLG
               END-IF    
               GO  TO  2002-ONSHI-KAKU-IRAI-EXT
           END-IF
      *
      *    ＵＩＤが渡されたとき
           IF      LNK-ONSHI001-TBL-UUID
                                   NOT =   SPACE
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    LNK-ONSHI001-TBL-UUID
                                       TO  ONSKAKU-TBL-UUID
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               PERFORM 900-ONSHI-KAKU-READ-SEC
               IF      FLG-ONSHI-KAKU  =   ZERO
      *            患者登録で保険証ＯＣＲのとき
                   IF    ( LNK-ONSHI001-GYOUMUCD
                                           =   "12" )
                   AND   ( ONSKAKU-ORCA-SHOKAI-KBN
                                           =   "2"  )
                       PERFORM 20021-ONSKAKU-IRAI-HKNOCR-SEC
                   ELSE
                     IF  ( ( ONSKAKU-SHOKAI-KBN
                                           =   "1"    )
                          OR
                         ( ONSKAKU-SHOKAI-KBN
                                           =   "2" AND
                           ONSKAKU-ORCA-SHOKAI-KBN
                                           =   "2"    ) )
                     AND (   ONSKAKU-PTID    =   ZERO     )
                       MOVE   LNK-ONSHI001-PTID
                                               TO  ONSKAKU-PTID
                       MOVE    LNK-ONSHI001-TBL-UUID
                                               TO  ONSKAKU-TBL-UUID
      *                資格確認レコードの更新
                       PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
                       IF     LNK-ONSHI001-RC  =   9
                           CONTINUE
                       ELSE
                           IF      ONSKAKU-FUJYO-UUID  NOT =   SPACE
                               IF      ONSKAKU-CARD-CLASS      =   "01"
      *                           医療扶助資格確認レコード更新処理
                                  PERFORM 20021-FUJYO-ONSKAKU-UPDATE-SEC
                               END-IF
                               IF     LNK-ONSHI001-RC  =   9
                                   CONTINUE
                               ELSE
      *                            医療扶助交付番号格納レコード更新処理
                                   PERFORM 20021-ONSHI-AIDLST-UPDATE-SEC
                               END-IF
                           END-IF
                       END-IF
                       MOVE   "Yes"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       MOVE    ONSKAKU-TBL-UUID
                                               TO  LNK-ONSHI001-UID
                     ELSE
      *                訪問診療のとき
                       IF  ( ( ONSKAKU-ONSHINRYO-KBN
                                             =   "2" OR  "3" )
                              AND                      
                             ( ONSKAKU-PTID =   ZERO         ) )
      *                同意一括の資格確認レコードを利用した場合には、ptid が
      *                設定されている場合でも、薬剤等の取得依頼のため
      *               「Yes」を返却 (2025/3/21)
                       OR  (   ONSKAKU-PTID =   ZERO           )
                       OR  (   ONSKAKU-PROCESS-CLASS
                                             =   "2" OR  "3"   )      
                         MOVE   LNK-ONSHI001-PTID
                                                 TO  ONSKAKU-PTID
                         MOVE    LNK-ONSHI001-TBL-UUID
                                                 TO  ONSKAKU-TBL-UUID
      *                  資格確認レコードの更新
                         PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
                         IF     LNK-ONSHI001-RC  =   9
                             CONTINUE
                         ELSE
      *                      訪問診療レコードの更新処理
                             PERFORM 20021-ONSHI-HOUMON-UPDATE-SEC
                             IF     LNK-ONSHI001-RC  =   9
                                 CONTINUE
                             ELSE
                                 IF      ONSKAKU-FUJYO-UUID
                                                     NOT =   SPACE
                                 AND     ONSKAKU-CARD-CLASS
                                                         =   "01"
      *                            医療扶助資格確認レコード更新処理
                                   PERFORM
                                       20021-FUJYO-ONSKAKU-UPDATE-SEC
                                   IF     LNK-ONSHI001-RC  =   9
                                       CONTINUE
                                   ELSE
      *                                訪問診療レコードの更新処理
                                       MOVE    SEH-ONSKAKU-REC
                                                     TO  ONSKAKU-REC
                                       PERFORM
                                           20021-ONSHI-HOUMON-UPDATE-SEC
                                       IF     LNK-ONSHI001-RC  =   9
                                         CONTINUE
                                       ELSE
      *                                  医療扶助交付番号格納レコード更新処理
                                         PERFORM
                                           20021-ONSHI-AIDLST-UPDATE-SEC
                                       END-IF
                                       MOVE    HKN-ONSKAKU-REC
                                                         TO  ONSKAKU-REC
                                   END-IF
                                 END-IF      
                             END-IF  
                         END-IF
                         MOVE   "Yes"            TO
                                             LNK-ONSHI001-KAKUNIN-FLG
                         MOVE    ONSKAKU-TBL-UUID
                                                 TO  LNK-ONSHI001-UID
                       ELSE
                         IF      ONSKAKU-SIKAKU-YUKO
                                           =   "5"
      *                    資格確認レコードの作成
                           IF    ( WRK-PTHKNINF-OK =   1 )
                           AND   ( WRK-PTKOHINF-OK =   1 )
                           AND   ( WRK-PTHKN-NUM
                                               NOT =   SPACE )
      *                        併用のときの資格確認レコードの作成
                               PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                               MOVE    "Yes"       TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                               MOVE    WRK-SEH-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                               PERFORM 2003-PUSH-SEC
                           ELSE                        
                               PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                               IF      ONSKAKU-SHO-NUM =   SPACE
                                   MOVE    "No"    TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                               ELSE    
                                   MOVE    "Yes"   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                               END-IF
                           END-IF
                         END-IF
                       END-IF
                     END-IF
                   END-IF      
               ELSE
      *            資格確認レコードの作成
                   IF    ( WRK-PTHKNINF-OK =   1     )
                   AND   ( WRK-PTKOHINF-OK =   1     )
                   AND   ( WRK-PTHKN-NUM
                                       NOT =   SPACE )
      *                併用のときの資格確認レコードの作成
                       PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                       MOVE    "Yes"       TO
                                           LNK-ONSHI001-KAKUNIN-FLG
                       MOVE    WRK-SEH-TBL-UUID
                                           TO  LNK-ONSHI001-UID
                       PERFORM 2003-PUSH-SEC
                   ELSE                        
                       PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                       IF      ONSKAKU-SHO-NUM =   SPACE
                           MOVE     "No"       TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       ELSE    
                           MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           MOVE    ONSKAKU-TBL-UUID
                                               TO  LNK-ONSHI001-UID
                       END-IF
                   END-IF
               END-IF    
      *        公費の更新
               MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               IF      HKN-ONSKAKU-CARD-CLASS  =   "A1"
                   MOVE    HKN-ONSKAKU-FUJYO-UUID
                                           TO  ONSKAKU-KOUHI-UUID
               ELSE    
                   MOVE    LNK-ONSHI001-TBL-UUID
                                           TO  ONSKAKU-KOUHI-UUID
               END-IF
               DISPLAY "KOUHI-UUID=" ONSKAKU-KOUHI-UUID
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key13"         TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key13"         TO  MCP-PATHNAME
                   PERFORM 900-ONSHI-KAKU-READ-N2-SEC
               ELSE
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    1               TO  FLG-ONSHI-KAKU
               END-IF
      *
               PERFORM         UNTIL   FLG-ONSHI-KAKU      =   1
                   IF    ( ONSKAKU-SHOKAI-KBN
                                           =   "9" OR  "8" )
                   AND   ( ONSKAKU-PTID    =   ZERO        )
                       MOVE   LNK-ONSHI001-PTID
                                               TO  ONSKAKU-PTID
      *                資格確認レコードの更新
                       PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
                   END-IF
      *      
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key13"         TO  MCP-PATHNAME
                   PERFORM 900-ONSHI-KAKU-READ-N2-SEC
               END-PERFORM
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key13"         TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               GO  TO  2002-ONSHI-KAKU-IRAI-EXT
           END-IF
      *
      *    ＵＩＤが渡されてないとき
      *    資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
      *#20220918     
           IF      FLG-ONSHI-KAKU  =   1
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    LNK-ONSHI001-PTID
                                       TO  ONSKAKU-PTID
               MOVE    "2"             TO  ONSKAKU-ORCA-SHOKAI-KBN
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
           END-IF    
      *
      *    資格確認テーブルが存在しない場合
           IF      FLG-ONSHI-KAKU  =   1
      *        資格確認レコードの作成
               IF    ( WRK-PTHKNINF-OK =   1     )
               AND   ( WRK-PTKOHINF-OK =   1     )
               AND   ( WRK-PTHKN-NUM
                                   NOT =   SPACE )
      *          資格確認訪問診療取得処理
                 PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
      *          tbl_onshi_houmonが存在し、診療区分=2 or 3のときに併用の場合、
      *          生保の資格確認は行わない
                 IF      ONS-HOUMON-ONSHINRYO-KBN
                                      =   "2" OR  "3"
                   PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                   IF      ONSKAKU-SHO-NUM =   SPACE
                       MOVE    "No"        TO  LNK-ONSHI001-KAKUNIN-FLG
                   ELSE    
                       MOVE     "Yes"      TO  LNK-ONSHI001-KAKUNIN-FLG
                       MOVE    ONSKAKU-TBL-UUID
                                           TO  LNK-ONSHI001-UID
                   END-IF
                 ELSE
      *            併用のときの資格確認レコードの作成
                   PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                   MOVE    "Yes"       TO  LNK-ONSHI001-KAKUNIN-FLG
                   MOVE    WRK-SEH-TBL-UUID
                                       TO  LNK-ONSHI001-UID
                   PERFORM 2003-PUSH-SEC
                 END-IF  
               ELSE                        
      *            資格確認訪問診療取得処理
                   PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
                   PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                   IF      ONSKAKU-SHO-NUM =   SPACE
                       MOVE    "No"        TO  LNK-ONSHI001-KAKUNIN-FLG
                   ELSE    
                       MOVE     "Yes"      TO  LNK-ONSHI001-KAKUNIN-FLG
                       MOVE    ONSKAKU-TBL-UUID
                                           TO  LNK-ONSHI001-UID
                   END-IF
               END-IF    
           ELSE
      *    マイナンバーカードか訪問診療のレコードが存在する場合      
               IF      ONSKAKU-SHOKAI-KBN
                                       =   "1" OR  SPACE
                   MOVE   "No"            TO  LNK-ONSHI001-KAKUNIN-FLG
      *----> 該当の患者の枝番が設定されていない場合は、どうする?              
                   GO  TO  2002-ONSHI-KAKU-IRAI-EXT
               END-IF    
      *
               IF      LNK-ONSHI001-GYOUMUCD
                                       =   "12"
      *            患者登録から呼び出しの場合
                   PERFORM 20021-ONSHI-KAKU-IRAI-12-SEC
               ELSE
      *************IF      LNK-ONSHI001-GYOUMUCD
      *************                        =   "12"
      *                受付から呼び出しの場合
      *************    PERFORM 20021-ONSHI-KAKU-INSERT-SEC
      *************    MOVE     "Yes"          TO
      *************                            LNK-ONSHI001-KAKUNIN-FLG
      *************    MOVE    ONSKAKU-TBL-UUID
      *************                            TO  LNK-ONSHI001-UID
      *************ELSE
      *                患者登録以外から呼び出しの場合
                       EVALUATE    ONSKAKU-KAKU-ENDFLG
                           WHEN    SPACE
                           WHEN    "01"
                           WHEN    "03"
                           WHEN    "02"             
                               MOVE   "No"    TO
                                              LNK-ONSHI001-KAKUNIN-FLG
                       END-EVALUATE
      *************END-IF 
               END-IF    
           END-IF
      *
            .
       2002-ONSHI-KAKU-IRAI-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認PTID設定処理
      ******************************************************************
       20021-ONSKAKU-PTID-SET-SEC  SECTION.
      *
           MOVE   LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-UPYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-UPHMS
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO 
               MOVE    9               TO  LNK-ONSHI001-RC
               GO  TO  20021-ONSKAKU-PTID-SET-EXT
           END-IF
      *
           MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
      *
           IF    ( ONSKAKU-FUJYO-UUID
                               NOT =   SPACE )
           AND   ( ONSKAKU-CARD-CLASS
                               =   "01"      )
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    HKN-ONSKAKU-FUJYO-UUID
                                       TO  ONSKAKU-FUJYO-UUID
               MOVE    HKN-ONSKAKU-KAKU-KAKUNINYMD
                                       TO  ONSKAKU-KAKU-KAKUNINYMD
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key27"         TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key27"         TO  MCP-PATHNAME
                   PERFORM 900-ONSHI-KAKU-READ-N2-SEC
               ELSE
                   INITIALIZE                  ONSKAKU-REC
                   MOVE    1               TO  FLG-ONSHI-KAKU
               END-IF
      *
               PERFORM             UNTIL   FLG-ONSHI-KAKU  =   1
                   IF      ONSKAKU-CARD-CLASS
                                           =   "A1"
                       MOVE   LNK-ONSHI001-PTID
                                               TO  ONSKAKU-PTID
      *                資格確認レコードの更新
                       MOVE    SMCNDATE-YMD    TO  ONSKAKU-UPYMD
                       MOVE    SMCNDATE-HMS    TO  ONSKAKU-UPHMS
                       MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                       MOVE    "DBUPDATE"      TO  MCP-FUNC
                       MOVE    "tbl_onshi_kaku"
                                               TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC      NOT =   ZERO 
                           MOVE    9               TO  LNK-ONSHI001-RC
                           GO  TO  20021-ONSKAKU-PTID-SET-EXT
                       END-IF
                   END-IF    
      *
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key27"         TO  MCP-PATHNAME
                   PERFORM 900-ONSHI-KAKU-READ-N2-SEC
               END-PERFORM
      *
                MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key27"         TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *                                        
           INITIALIZE                  ONSHI-HOUMON-REC
           MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
           MOVE    LNK-ONSHI001-TBL-UUID
                                  TO  ONS-HOUMON-TBL-UUID 
           MOVE    "key"           TO  WRK-MCP-PATHNAME
           MOVE    ONSHI-HOUMON-REC
                                   TO  MCPDATA-REC
           PERFORM 900-ONSHI-HOUMON-READ-SEC
           IF      FLG-ONSHI-HOUMON    =   ZERO
                MOVE   LNK-ONSHI001-PTID
                                   TO  ONS-HOUMON-PTID
               MOVE    SMCNDATE-YMD
                                   TO  ONS-HOUMON-UPYMD
               MOVE    SMCNDATE-HMS
                                   TO  ONS-HOUMON-UPHMS
               MOVE    ONSHI-HOUMON-REC
                                   TO  MCPDATA-REC
               MOVE    "DBUPDATE"  TO  MCP-FUNC
               MOVE    "tbl_onshi_houmon"
                                   TO  MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC  NOT =   ZERO 
                   MOVE    9           TO  LNK-ONSHI001-RC
                   GO  TO  20021-ONSKAKU-PTID-SET-EXT
               END-IF
           END-IF    
      *                                        
           IF      HKN-ONSKAKU-FUJYO-UUID
                               NOT =   SPACE
              MOVE    HKN-ONSKAKU-FUJYO-UUID
                                       TO  ONSKAKU-FUJYO-UUID
      *        医療扶助交付番号格納レコード更新処理
               PERFORM 20021-ONSHI-AIDLST-UPDATE-SEC
           END-IF
      *
           INITIALIZE              HKN-ONSKAKU-REC    
           . 
      *
       20021-ONSKAKU-PTID-SET-EXT.
           EXIT.
      *  
      *****************************************************************
      *    患者登録で保険証ＯＣＲのとき
      *****************************************************************
       20021-ONSKAKU-IRAI-HKNOCR-SEC   SECTION.
      *
           EVALUATE    ONSKAKU-SIKAKU-YUKO
               WHEN    "5"
                   IF      ONSKAKU-PTID    =   ZERO
                       MOVE   LNK-ONSHI001-PTID
                                               TO  ONSKAKU-PTID
                   END-IF
                   STRING  ONSKAKU-PTID        DELIMITED  BY  SPACE
                           ","                 DELIMITED  BY  SIZE  
                           ONSKAKU-TBL-UUID    DELIMITED  BY  SIZE
                                       INTO    ONSKAKU-FILE-SIKIBETU
                   END-STRING
                   MOVE    ONSKAKU-RES-EDABAN
                                           TO  ONSKAKU-SHO-EDABAN
                   MOVE    SPACE           TO  ONSKAKU-KAKU-ENDFLG
      *            資格確認レコードの更新
                   PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
                   MOVE   "Yes"            TO  LNK-ONSHI001-KAKUNIN-FLG
                   MOVE    ONSKAKU-TBL-UUID
                                           TO  LNK-ONSHI001-UID
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
                   IF      ONSKAKU-SHOU-ENDFLG =   SPACE
                       IF      ONSKAKU-PTID    =   ZERO
                           MOVE   LNK-ONSHI001-PTID
                                                   TO  ONSKAKU-PTID
                       END-IF
                       MOVE    "03"        TO  ONSKAKU-KAKU-ENDFLG
      *                資格確認レコードの更新
                       PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
                       IF     LNK-ONSHI001-RC  =   9
                           CONTINUE
                       ELSE
                           IF      ONSKAKU-FUJYO-UUID  NOT =   SPACE
                               IF      ONSKAKU-CARD-CLASS      =   "01"
      *                           医療扶助資格確認レコード更新処理
                                  PERFORM 20021-FUJYO-ONSKAKU-UPDATE-SEC
                               END-IF
                               IF     LNK-ONSHI001-RC  =   9
                                   CONTINUE
                               ELSE
      *                            医療扶助交付番号格納レコード更新処理
                                   PERFORM 20021-ONSHI-AIDLST-UPDATE-SEC
                               END-IF
                           END-IF
                       END-IF
                       MOVE   "Yes"        TO  LNK-ONSHI001-KAKUNIN-FLG
                       MOVE    ONSKAKU-TBL-UUID
                                           TO  LNK-ONSHI001-UID
                   ELSE
                       MOVE   "No"         TO  LNK-ONSHI001-KAKUNIN-FLG
                   END-IF
               WHEN    OTHER
                   MOVE   "No"         TO  LNK-ONSHI001-KAKUNIN-FLG
            END-EVALUATE
            .
       20021-ONSKAKU-IRAI-HKNOCR-EXT.
           EXIT.
      *
      *****************************************************************
      *    資格確認要否依頼処理（患者登録から呼び出しの場合）
      *****************************************************************
       20021-ONSHI-KAKU-IRAI-12-SEC SECTION.
      *
      *    ＵＩＤが渡されたとき
           IF      LNK-ONSHI001-TBL-UUID   NOT =   SPACE
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    LNK-ONSHI001-TBL-UUID
                                       TO  ONSKAKU-TBL-UUID
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               PERFORM 900-ONSHI-KAKU-READ-SEC
               IF      FLG-ONSHI-KAKU  =   ZERO 
                   EVALUATE    ONSKAKU-SHOKAI-KBN
                       WHEN    "1"
      *                    マイナンバーのとき                     
                           EVALUATE    ONSKAKU-KAKU-ENDFLG
                                           ALSO    ONSKAKU-SIKAKU-YUKO
                               WHEN    "02"    ALSO    "1"
                               WHEN    "02"    ALSO    "2"
                               WHEN    "02"    ALSO    "3"
                                   IF    ( ONSKAKU-SHOKAI-NUM
                                                           =   SPACE )
                                   OR    ( ONSKAKU-YAKUZAI-DOUIFLG
                                                           =   "1"
                                               AND
                                           ONSKAKU-YAKUZAI-ENDFLG
                                                           =   SPACE )
                                   OR    ( ONSKAKU-KENSIN-DOUIFLG
                                                           =   "1"
                                               AND
                                           ONSKAKU-KENSIN-ENDFLG
                                                           =   SPACE )
                                       MOVE     "Yes"           TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       MOVE    ONSKAKU-TBL-UUID
                                                               TO
                                               LNK-ONSHI001-UID
                                   ELSE
                                       MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   END-IF
                               WHEN    OTHER
                                   MOVE   "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           END-EVALUATE
                       WHEN    "2"
      *                    保険証の時                     
                           EVALUATE    ONSKAKU-KAKU-ENDFLG
                                           ALSO    ONSKAKU-SIKAKU-YUKO
                               WHEN    "02"    ALSO    "1"
                                 IF      WRK-PTHKNINF-OK =   1
      *                            患者保険情報検索処理
                                   PERFORM 20021-PTHKNINF-CHECK-SEC
                                   IF      FLG-PTHKNINF-OK     =   1
                                       MOVE   "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
                                       MOVE     "Yes"       TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       MOVE    ONSKAKU-TBL-UUID
                                                           TO
                                               LNK-ONSHI001-UID
                                   END-IF
                                 ELSE  
      *                            患者公費情報検索処理
                                   PERFORM 20021-PTKOHINF-CHECK-SEC
                                   IF      FLG-PTKOHINF-OK     =   1
                                       MOVE   "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
                                       MOVE     "Yes"       TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       MOVE    ONSKAKU-TBL-UUID
                                                           TO
                                               LNK-ONSHI001-UID
                                   END-IF
                                 END-IF  
                               WHEN    OTHER    
                                   MOVE   "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           END-EVALUATE                    
                   END-EVALUATE
               ELSE
                   DISPLAY "20021 uid => tbl_onshi_kaku not found"
               END-IF        
           ELSE
      *    ＵＩＤが渡されなかったとき
               IF    ( WRK-PTHKNINF-OK =   1   )
               AND   ( WRK-PTKOHINF-OK =   1   )
      *********AND   ( ONSKAKU-FUJYO-HEIYO
      *********                       =   "1" )
      *            併用のとき
                   PERFORM 200211-IRAI-12-HEIYO-SEC
               ELSE
      *          以下の条件を変更したときは併用のときも変更すること
                 EVALUATE    ONSKAKU-KAKU-ENDFLG
                   WHEN    SPACE
                   WHEN    "01"
                     IF      WRK-PTHKNINF-OK =   1
      *                患者保険情報検索処理
                       PERFORM 20021-PTHKNINF-CHECK-SEC
                       IF      FLG-PTHKNINF-OK =   1
                           MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       ELSE            
      *                    資格確認訪問診療取得処理
                           PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
      *                    資格確認レコードの作成
                           PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                           IF      ONSKAKU-SHO-NUM =   SPACE
                               MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           ELSE    
                               MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                           END-IF
                       END-IF
                     ELSE  
      *                患者公費情報検索処理
                       PERFORM 20021-PTKOHINF-CHECK-SEC
                       IF      FLG-PTKOHINF-OK =   1
                           MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       ELSE            
      *                    資格確認訪問診療取得処理
                           PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
      *                    資格確認レコードの作成
                           PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                           IF      ONSKAKU-SHO-NUM =   SPACE
                               MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           ELSE    
                               MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                           END-IF
                       END-IF
                     END-IF  
                   WHEN    "02"
                       EVALUATE    ONSKAKU-SIKAKU-YUKO
                           WHEN    "1"
                           WHEN    "2"
                           WHEN    "3"
                           WHEN    "4"
                           WHEN    "5"
                           WHEN    SPACE
                             IF      WRK-PTHKNINF-OK =   1
      *                        最新の保険情報とチェック                       
                               IF      WRK-PTHKN-EDABAN    NOT =   SPACE
                                   IF      ONSKAKU-RES-BIRTHDAY
                                                               =   SPACE
                                       IF    ( ONSKAKU-SHO-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                       AND   ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                       AND   ( ONSKAKU-SHO-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                       AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTHKN-NUM      )
                                       AND   ( ONSKAKU-SHO-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                           MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE            
      *                                    資格確認訪問診療取得処理
                                           PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                    資格確認レコードの作成
                                           PERFORM
                                             20021-ONSHI-KAKU-INSERT-SEC
                                           IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                               MOVE    "No" 
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           ELSE    
                                               MOVE     "Yes"
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                           END-IF
                                       END-IF
                                   ELSE    
                                       IF    ( ONSKAKU-RES-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                       AND   ( ONSKAKU-RES-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                       AND   ( ONSKAKU-RES-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                       AND   ( ONSKAKU-RES-NUM
                                               =   WRK-PTHKN-NUM      )
                                       AND   ( ONSKAKU-RES-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                           MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE            
      *                                    資格確認訪問診療取得処理
                                           PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                    資格確認レコードの作成
                                           PERFORM
                                             20021-ONSHI-KAKU-INSERT-SEC
                                           IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                               MOVE    "No" 
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           ELSE    
                                               MOVE     "Yes"
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                           END-IF
                                       END-IF
                                   END-IF    
                               ELSE
                                   IF      ONSKAKU-RES-BIRTHDAY
                                                               =   SPACE
                                       IF    ( ONSKAKU-SHO-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                       AND   ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                       AND   ( ONSKAKU-SHO-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                       AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTHKN-NUM      )
                                       AND   ( ONSKAKU-SHO-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                           MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE            
      *                                    資格確認訪問診療取得処理
                                           PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                    資格確認レコードの作成
                                           PERFORM
                                           20021-ONSHI-KAKU-INSERT-SEC
                                           IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                               MOVE    "No" 
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           ELSE    
                                               MOVE     "Yes"
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                           END-IF
                                       END-IF
                                   ELSE    
                                       IF    ( ONSKAKU-RES-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                       AND   ( ONSKAKU-RES-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                       AND   ( ONSKAKU-RES-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                       AND   ( ONSKAKU-RES-NUM
                                               =   WRK-PTHKN-NUM      )
                                           MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE            
      *                                    資格確認訪問診療取得処理
                                           PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                    資格確認レコードの作成
                                           PERFORM
                                             20021-ONSHI-KAKU-INSERT-SEC
                                           IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                               MOVE    "No" 
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           ELSE    
                                               MOVE     "Yes"
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                           END-IF
                                       END-IF
                                   END-IF    
                               END-IF
                             ELSE
      *                        最新の公費情報とチェック                       
                               IF      ONSKAKU-RES-BIRTHDAY
                                                           =   SPACE
                                   IF    ( ONSKAKU-SHO-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                   AND   ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTKOH-FTNJANUM )
                                   AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTKOH-JKYSNUM  )
                                       MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
      *                                資格確認訪問診療取得処理
                                       PERFORM
                                           20021-ONSHI-HOUMON-CHECK-SEC
      *                                資格確認レコードの作成
                                       PERFORM
                                           20021-ONSHI-KAKU-INSERT-SEC
                                       IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                           MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE    
                                           MOVE     "Yes"
                                                               TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                       END-IF
                                   END-IF
                               ELSE
                                   IF    ( ONSKAKU-RES-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                   AND   ( ONSKAKU-RES-HKNJANUM
                                               =   WRK-PTKOH-FTNJANUM )
                                   AND   ( ONSKAKU-RES-NUM
                                               =   WRK-PTKOH-JKYSNUM  )
                                       MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
      *                                資格確認訪問診療取得処理
                                       PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                資格確認レコードの作成
                                       PERFORM
                                             20021-ONSHI-KAKU-INSERT-SEC
                                       IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                           MOVE    "No" 
                                                               TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE    
                                           MOVE     "Yes"
                                                               TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                       END-IF
                                   END-IF
                               END-IF    
                             END-IF
                           WHEN    OTHER
                              MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       END-EVALUATE                        
                   WHEN    "03"
                       IF      WRK-PTHKNINF-OK =   1
      *                    枝番なしで確認した場合      
                           IF      WRK-PTHKN-EDABAN    =   SPACE
                               MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           ELSE
                               IF      ONSKAKU-RES-EDABAN  =
                                                       WRK-PTHKN-EDABAN
      *                            資格確認訪問診療取得処理
                                   PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
      *                            資格確認レコードの作成
                                   PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                                   IF      ONSKAKU-SHO-NUM     =   SPACE
                                       MOVE    "No"                TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE    
                                       MOVE     "Yes"              TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       MOVE    ONSKAKU-TBL-UUID    TO
                                                       LNK-ONSHI001-UID
                                   END-IF
                               ELSE
      *                            間違いを入力したので早めに救いたい
                                   MOVE     "Yes"           TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   MOVE    ONSKAKU-TBL-UUID
                                                           TO
                                              LNK-ONSHI001-UID
                               END-IF
                           END-IF
                       ELSE
                           MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       END-IF
                 END-EVALUATE
               END-IF  
           END-IF    
      *
            .
       20021-ONSHI-KAKU-IRAI-12-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認要否依頼処理（患者登録から呼び出しの場合併用のとき）
      *    ＵＩＤが渡されなかったとき
      ******************************************************************
       200211-IRAI-12-HEIYO-SEC    SECTION.
      *
      *    訪問診療のとき
           IF      ONSKAKU-ONSHINRYO-KBN
                                   =   "2"
               PERFORM 2002111-IRAI-12-HEIYO-HMN-SEC
               GO  TO  200211-IRAI-12-HEIYO-EXT
           END-IF    
      *
           MOVE    ONSKAKU-REC     TO  WRK-ONSKAKU-REC
      *
      *    併用のとき、併用の資格確認取得
           IF      ONSKAKU-FUJYO-HEIYO
                                   =   "1"
               MOVE    ONSKAKU-REC     TO  WRK-ONSKAKU-REC
               MOVE    ZERO            TO  FLG-ONSHI-HEIYO
               IF      ONSKAKU-CARD-CLASS  =   "A1"
                   MOVE    ONSKAKU-REC     TO  HEIYO-ONSKAKU-REC (2)
               ELSE
                   MOVE    ONSKAKU-REC     TO  HEIYO-ONSKAKU-REC (1)
               END-IF
      *
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    WRK-ONSKAKU-FUJYO-UUID1
                                       TO  ONSKAKU-FUJYO-UUID1
               MOVE    WRK-ONSKAKU-TBL-UUID
                                       TO  ONSKAKU-TBL-UUID
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               PERFORM 900-ONSHI-KAKU-READ-KEY28-SEC
               IF      FLG-ONSHI-HEIYO =   1
                   IF      ONSKAKU-CARD-CLASS  =   "A1"
                       MOVE    ONSKAKU-REC     TO  HEIYO-ONSKAKU-REC (2)
                   ELSE
                       MOVE    ONSKAKU-REC     TO  HEIYO-ONSKAKU-REC (1)
                   END-IF    
               END-IF
           END-IF
      *
           MOVE    WRK-ONSKAKU-REC TO  ONSKAKU-REC
      *
           EVALUATE    ONSKAKU-KAKU-ENDFLG
               WHEN    SPACE
               WHEN    "01"
      *            患者保険情報検索処理
                   MOVE    HEIYO-ONSKAKU-REC (1)   TO  ONSKAKU-REC
                   PERFORM 20021-PTHKNINF-CHECK-SEC
      *            患者公費情報検索処理
                   MOVE    HEIYO-ONSKAKU-REC (2)   TO  ONSKAKU-REC
                   PERFORM 20021-PTKOHINF-CHECK-SEC
      *            
                   MOVE    WRK-ONSKAKU-REC TO  ONSKAKU-REC
                   IF    ( FLG-PTKOHINF-OK =   1 )
                   AND   ( FLG-PTHKNINF-OK =   1 )
                       MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                   ELSE            
      *                併用のときの資格確認レコードの作成
                       PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                       MOVE    "Yes"       TO
                                           LNK-ONSHI001-KAKUNIN-FLG
                       MOVE    WRK-SEH-TBL-UUID
                                           TO  LNK-ONSHI001-UID
                       PERFORM 2003-PUSH-SEC
                   END-IF
               WHEN    "02"
                   EVALUATE    ONSKAKU-SIKAKU-YUKO
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "3"
                       WHEN    "4"
                       WHEN    "5"
                       WHEN    SPACE
      *                    最新の保険情報とチェック                       
                           IF      WRK-PTHKN-EDABAN    NOT =   SPACE
                               MOVE    HEIYO-ONSKAKU-REC (1)
                                                       TO  ONSKAKU-REC
                               IF      ONSKAKU-RES-BIRTHDAY    =   SPACE
                                   IF    ( ONSKAKU-SHO-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                   AND   ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                   AND   ( ONSKAKU-SHO-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                   AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTHKN-NUM      )
                                   AND   ( ONSKAKU-SHO-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                       MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
                                       IF      WRK-PTHKN-NUM
                                                           =   SPACE
                                           MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE    
                                           MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       END-IF
                                   END-IF
                               ELSE    
                                   IF    ( ONSKAKU-RES-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                   AND   ( ONSKAKU-RES-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                   AND   ( ONSKAKU-RES-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                   AND   ( ONSKAKU-RES-NUM
                                               =   WRK-PTHKN-NUM      )
                                   AND   ( ONSKAKU-RES-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                       MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
                                       IF      WRK-PTHKN-NUM
                                                           =   SPACE
                                           MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE    
                                           MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       END-IF
                                   END-IF
                               END-IF    
                           ELSE
                               IF      ONSKAKU-RES-BIRTHDAY    =   SPACE
                                   IF    ( ONSKAKU-SHO-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                   AND   ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                   AND   ( ONSKAKU-SHO-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                   AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTHKN-NUM      )
                                   AND   ( ONSKAKU-SHO-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                       MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
                                       IF      WRK-PTHKN-NUM
                                                           =   SPACE
                                           MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE    
                                           MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       END-IF
                                   END-IF
                               ELSE    
                                   IF    ( ONSKAKU-RES-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                   AND   ( ONSKAKU-RES-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                   AND   ( ONSKAKU-RES-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                   AND   ( ONSKAKU-RES-NUM
                                               =   WRK-PTHKN-NUM      )
                                       MOVE    "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
                                       IF      WRK-PTHKN-NUM
                                                           =   SPACE
                                           MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE    
                                           MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       END-IF
                                   END-IF
                               END-IF    
                           END-IF
                           IF      LNK-ONSHI001-KAKUNIN-FLG
                                                   =   "No"
      *                        最新の公費情報とチェック                       
                               MOVE    HEIYO-ONSKAKU-REC (2)
                                                       TO  ONSKAKU-REC
                               IF      ONSKAKU-RES-BIRTHDAY    =   SPACE
                                   IF    ( ONSKAKU-SHO-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                   AND   ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTKOH-FTNJANUM )
                                   AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTKOH-JKYSNUM  )
                                       MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
                                       IF      WRK-PTKOH-JKYSNUM
                                                           =   SPACE
                                           MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE    
                                           MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       END-IF
                                   END-IF
                               ELSE
                                   IF    ( ONSKAKU-RES-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                   AND   ( ONSKAKU-RES-HKNJANUM
                                               =   WRK-PTKOH-FTNJANUM )
                                   AND   ( ONSKAKU-RES-NUM
                                               =   WRK-PTKOH-JKYSNUM  )
                                       MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   ELSE            
                                       IF      WRK-PTKOH-JKYSNUM
                                                           =   SPACE
                                           MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE    
                                           MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       END-IF
                                   END-IF
                               END-IF    
                           END-IF
                       WHEN    OTHER
                           MOVE   "No"            TO
                                           LNK-ONSHI001-KAKUNIN-FLG
                   END-EVALUATE                        
      *
                   MOVE    WRK-ONSKAKU-REC TO  ONSKAKU-REC
      *
      *            併用のときの資格確認レコードの作成
                   IF      LNK-ONSHI001-KAKUNIN-FLG
                                           =   "Yes"
                       PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                       MOVE    WRK-SEH-TBL-UUID
                                           TO  LNK-ONSHI001-UID
                       PERFORM 2003-PUSH-SEC
                   END-IF
               WHEN    "03"
      *            枝番なしで確認した場合      
                   MOVE    HEIYO-ONSKAKU-REC (1)
                                           TO  ONSKAKU-REC
      *
                   IF      WRK-PTHKN-EDABAN    =   SPACE
                       MOVE   "No"         TO  LNK-ONSHI001-KAKUNIN-FLG
                   ELSE
                       IF      ONSKAKU-RES-EDABAN  =   WRK-PTHKN-EDABAN
                           IF      WRK-PTHKN-NUM       =   SPACE
                               MOVE    "No"                TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           ELSE    
                               MOVE     "Yes"              TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           END-IF
                       ELSE
      *                    間違いを入力したので早めに救いたい
                           MOVE     "Yes"           TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       END-IF
                   END-IF
      *
                   MOVE    WRK-ONSKAKU-REC TO  ONSKAKU-REC
      *
      *            併用のときの資格確認レコードの作成
                   IF      LNK-ONSHI001-KAKUNIN-FLG
                                           =   "Yes"
                       PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                       MOVE    WRK-SEH-TBL-UUID
                                           TO  LNK-ONSHI001-UID
                       PERFORM 2003-PUSH-SEC
                   END-IF
            END-EVALUATE    
      *
            .
       200211-IRAI-12-HEIYO-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認要否依頼処理（患者登録から呼び出しの場合併用のとき）
      *    ＵＩＤが渡されなかったとき（訪問診療のとき）
      ******************************************************************
       2002111-IRAI-12-HEIYO-HMN-SEC    SECTION.
      *
           EVALUATE    ONSKAKU-KAKU-ENDFLG
               WHEN    SPACE
               WHEN    "01"
                   IF      WRK-PTHKNINF-OK =   1
      *                患者保険情報検索処理
                       PERFORM 20021-PTHKNINF-CHECK-SEC
                       IF      FLG-PTHKNINF-OK =   1
                           MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       ELSE            
      *                    資格確認訪問診療取得処理
                           PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
      *                    資格確認レコードの作成
                           PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                           IF      ONSKAKU-SHO-NUM =   SPACE
                               MOVE    "No"        TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           ELSE    
                               MOVE     "Yes"      TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                           END-IF
                       END-IF
                   END-IF  
               WHEN    "02"
                   EVALUATE    ONSKAKU-SIKAKU-YUKO
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "3"
                       WHEN    "4"
                       WHEN    "5"
                       WHEN    SPACE
                           IF      WRK-PTHKNINF-OK =   1
      *                        最新の保険情報とチェック                       
                               IF      WRK-PTHKN-EDABAN    NOT =   SPACE
                                   IF      ONSKAKU-RES-BIRTHDAY
                                                               =   SPACE
                                       IF    ( ONSKAKU-SHO-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                       AND   ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                       AND   ( ONSKAKU-SHO-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                       AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTHKN-NUM      )
                                       AND   ( ONSKAKU-SHO-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                           MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE            
      *                                    資格確認訪問診療取得処理
                                           PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                    資格確認レコードの作成
                                           PERFORM
                                             20021-ONSHI-KAKU-INSERT-SEC
                                           IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                               MOVE    "No" 
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           ELSE    
                                               MOVE     "Yes"
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                           END-IF
                                       END-IF
                                   ELSE    
                                       IF    ( ONSKAKU-RES-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                       AND   ( ONSKAKU-RES-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                       AND   ( ONSKAKU-RES-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                       AND   ( ONSKAKU-RES-NUM
                                               =   WRK-PTHKN-NUM      )
                                       AND   ( ONSKAKU-RES-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                           MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE            
      *                                    資格確認訪問診療取得処理
                                           PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                    資格確認レコードの作成
                                           PERFORM
                                             20021-ONSHI-KAKU-INSERT-SEC
                                           IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                               MOVE    "No" 
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           ELSE    
                                               MOVE     "Yes"
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                           END-IF
                                       END-IF
                                   END-IF    
                               ELSE
                                   IF      ONSKAKU-RES-BIRTHDAY
                                                               =   SPACE
                                       IF    ( ONSKAKU-SHO-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                       AND   ( ONSKAKU-SHO-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                       AND   ( ONSKAKU-SHO-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                       AND   ( ONSKAKU-SHO-NUM
                                               =   WRK-PTHKN-NUM      )
                                       AND   ( ONSKAKU-SHO-EDABAN
                                               =   WRK-PTHKN-EDABAN   )
                                           MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE            
      *                                    資格確認訪問診療取得処理
                                           PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                    資格確認レコードの作成
                                           PERFORM
                                           20021-ONSHI-KAKU-INSERT-SEC
                                           IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                               MOVE    "No" 
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           ELSE    
                                               MOVE     "Yes"
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                           END-IF
                                       END-IF
                                   ELSE    
                                       IF    ( ONSKAKU-RES-BIRTHDAY
                                               =   PTINF-BIRTHDAY     )
                                       AND   ( ONSKAKU-RES-HKNJANUM
                                               =   WRK-PTHKN-HKNJANUM )
                                       AND   ( ONSKAKU-RES-KIGO
                                               =   WRK-PTHKN-KIGO     )
                                       AND   ( ONSKAKU-RES-NUM
                                               =   WRK-PTHKN-NUM      )
                                           MOVE   "No"             TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                       ELSE            
      *                                    資格確認訪問診療取得処理
                                           PERFORM
                                            20021-ONSHI-HOUMON-CHECK-SEC
      *                                    資格確認レコードの作成
                                           PERFORM
                                             20021-ONSHI-KAKU-INSERT-SEC
                                           IF      ONSKAKU-SHO-NUM
                                                       =   SPACE
                                               MOVE    "No" 
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                           ELSE    
                                               MOVE     "Yes"
                                                   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                               MOVE    ONSKAKU-TBL-UUID
                                                   TO  LNK-ONSHI001-UID
                                           END-IF
                                       END-IF
                                   END-IF    
                               END-IF
                           END-IF
                       WHEN    OTHER
                           MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                   END-EVALUATE                        
               WHEN    "03"
                   IF      WRK-PTHKNINF-OK =   1
      *                枝番なしで確認した場合      
                       IF      WRK-PTHKN-EDABAN    =   SPACE
                           MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       ELSE
                           IF      ONSKAKU-RES-EDABAN  =
                                                       WRK-PTHKN-EDABAN
      *                        資格確認訪問診療取得処理
                               PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
      *                        資格確認レコードの作成
                               PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                               IF      ONSKAKU-SHO-NUM     =   SPACE
                                   MOVE    "No"                TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                               ELSE    
                                   MOVE     "Yes"              TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                                   MOVE    ONSKAKU-TBL-UUID    TO
                                                       LNK-ONSHI001-UID
                               END-IF
                           ELSE
      *                        間違いを入力したので早めに救いたい
                               MOVE     "Yes"           TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                               MOVE    ONSKAKU-TBL-UUID
                                                        TO
                                              LNK-ONSHI001-UID
                           END-IF
                       END-IF
                   ELSE
                       MOVE   "No"            TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                   END-IF
           END-EVALUATE
      *
            .
       2002111-IRAI-12-HEIYO-HMN-EXT.
           EXIT.
      *
      ******************************************************************
      *    医療扶助資格確認レコード更新処理
      ******************************************************************
       20021-FUJYO-ONSKAKU-UPDATE-SEC    SECTION.
      *
           INITIALIZE                  SEH-ONSKAKU-REC
           MOVE    ONSKAKU-REC     TO  HKN-ONSKAKU-REC
      *
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    HKN-ONSKAKU-FUJYO-UUID
                                   TO  ONSKAKU-FUJYO-UUID
           MOVE    HKN-ONSKAKU-KAKU-KAKUNINYMD
                                   TO  ONSKAKU-KAKU-KAKUNINYMD
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key27"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key27"         TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           ELSE
               INITIALIZE                  ONSKAKU-REC
               MOVE    1               TO  FLG-ONSHI-KAKU
           END-IF
      *
           PERFORM             UNTIL   FLG-ONSHI-KAKU  =   1
               IF  ( ( ONSKAKU-SHOKAI-KBN
                                       =   "1"         )
                    OR
                     ( ONSKAKU-SHOKAI-KBN
                                       =   "2" AND
                       ONSKAKU-ORCA-SHOKAI-KBN
                                       =   "2"         )
                    OR
      *                訪問診療のとき
                     ( ONSKAKU-ONSHINRYO-KBN
                                       =   "2" OR  "3" ) )
               AND (   ONSKAKU-PTID    =   ZERO          )
                   MOVE   LNK-ONSHI001-PTID
                                           TO  ONSKAKU-PTID
                   MOVE    "03"            TO  ONSKAKU-KAKU-ENDFLG
      *            資格確認レコードの更新
                   MOVE    SMCNDATE-YMD    TO  ONSKAKU-UPYMD
                   MOVE    SMCNDATE-HMS    TO  ONSKAKU-UPHMS
                   MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_onshi_kaku"
                                           TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO 
                       MOVE    9               TO  LNK-ONSHI001-RC
                   ELSE
                       MOVE    ONSKAKU-REC     TO  SEH-ONSKAKU-REC    
                   END-IF
               END-IF
      *
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key27"         TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           END-PERFORM
      *
            MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key27"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    HKN-ONSKAKU-REC TO  ONSKAKU-REC
      *
            .
       20021-FUJYO-ONSKAKU-UPDATE-EXT.
           EXIT.
      *
      ******************************************************************
      *    医療扶助交付番号格納レコード更新処理
      ******************************************************************
       20021-ONSHI-AIDLST-UPDATE-SEC   SECTION.
      *
           INITIALIZE                  ONSHI-AIDLST-REC
           MOVE    SPA-HOSPNUM     TO  ONS-AIDLST-HOSPNUM
           MOVE    ONSKAKU-FUJYO-UUID
                                   TO  ONS-AIDLST-FUJYO-UUID
           MOVE    ONSHI-AIDLST-REC
                                   TO  MCPDATA-REC
           MOVE    "tbl_onshi_aidlst"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_onshi_aidlst"
                                       TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-ONSHI-AIDLST-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-ONSHI-AIDLST
           END-IF
      *
           PERFORM             UNTIL   FLG-ONSHI-AIDLST  =   1
                MOVE   LNK-ONSHI001-PTID
                                       TO  ONS-AIDLST-PTID
      *        資格確認レコードの更新
               MOVE    SMCNDATE-YMD    TO  ONS-AIDLST-UPYMD
               MOVE    SMCNDATE-HMS    TO  ONS-AIDLST-UPHMS
               MOVE    ONSHI-AIDLST-REC
                                       TO  MCPDATA-REC
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_onshi_aidlst"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
                IF      MCP-RC      NOT =   ZERO 
                   MOVE    9               TO  LNK-ONSHI001-RC
               END-IF
      *
               MOVE    "tbl_onshi_aidlst"
                                       TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-ONSHI-AIDLST-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_onshi_aidlst"
                                   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
            .
       20021-ONSHI-AIDLST-UPDATE-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者保険情報検索処理
      ******************************************************************
       20021-PTHKNINF-CHECK-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTHKNINF-OK
      *
           IF    ( ONSKAKU-SHO-BIRTHDAY
                                   =   PTINF-BIRTHDAY     )
           AND   ( ONSKAKU-SHO-HKNJANUM
                                   =   WRK-PTHKN-HKNJANUM )
           AND   ( ONSKAKU-SHO-KIGO
                                   =   WRK-PTHKN-KIGO     )
           AND   ( ONSKAKU-SHO-NUM =   WRK-PTHKN-NUM      )
           AND   ( ONSKAKU-SHO-EDABAN
                                   =   WRK-PTHKN-EDABAN   )
               MOVE    1               TO  FLG-PTHKNINF-OK
           END-IF    
            .
       20021-PTHKNINF-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者公費情報検索処理（医療扶助）
      ******************************************************************
       20021-PTKOHINF-CHECK-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTKOHINF-OK
      *
           IF    ( ONSKAKU-SHO-BIRTHDAY
                                   =   PTINF-BIRTHDAY     )
           AND   ( ONSKAKU-SHO-HKNJANUM
                                   =   WRK-PTKOH-FTNJANUM )
           AND   ( ONSKAKU-SHO-NUM =   WRK-PTKOH-JKYSNUM  )
               MOVE    1               TO  FLG-PTKOHINF-OK
           END-IF    
            .
       20021-PTKOHINF-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *    資格確認レコードの作成
      ******************************************************************
       20021-ONSHI-KAKU-INSERT-SEC SECTION.
      * 
      *    UIDコード取得処理
           PERFORM 200211-UID-GET-SEC
      *
      *    医療機関情報
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 960-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC    
           END-IF
      * 
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    C-UID           TO  ONSKAKU-TBL-UUID
      *    MOVE                    TO  ONSKAKU-AITE-UUID
      *    MOVE                    TO  ONSKAKU-KAKU-RENUMBER
           MOVE    SPACE           TO  ONSKAKU-KAKU-ENDFLG
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
           MOVE    ZERO            TO  ONSKAKU-SHOU-RENUMBER
           MOVE    SPACE           TO  ONSKAKU-SHOU-ENDFLG
           MOVE    ZERO            TO  ONSKAKU-YAKUZAI-RENUMBER
           MOVE    SPACE           TO  ONSKAKU-YAKUZAI-ENDFLG
           MOVE    ZERO            TO  ONSKAKU-KENSIN-RENUMBER
           MOVE    SPACE           TO  ONSKAKU-KENSIN-ENDFLG
           MOVE    PTINF-PTID      TO  ONSKAKU-PTID 
      *    MOVE                    TO  ONSKAKU-FUKUSU-GAITO
      *    MOVE                    TO  ONSKAKU-SHORI-TIME 
      *    MOVE                    TO  ONSKAKU-KAKUNINYMD
           STRING  SYS-1001-PREFNUM
                                   DELIMITED  BY  SIZE
                   "1"             DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD DELIMITED  BY  SIZE
                                   INTO       ONSKAKU-HOSPCD
           END-STRING                        
      *    旧医療機関コードが設定されているとき
           IF      SYS-1051-HOSPCDF    =   SPACE
               CONTINUE
           ELSE
               MOVE    SYS-1051-HOSPCDF    TO  ONSKAKU-HOSPCD
           END-IF    
           STRING  PTINF-PTID      DELIMITED  BY  SPACE
                   ","             DELIMITED  BY  SIZE  
                   C-UID           DELIMITED  BY  SIZE
                                   INTO       ONSKAKU-FILE-SIKIBETU
           END-STRING                        
           MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
           MOVE    "1"             TO  ONSKAKU-CODE-TYPE
           IF      WRK-PTHKNINF-OK =   1
               MOVE    WRK-PTHKN-HKNJANUM
                                       TO  ONSKAKU-SHO-HKNJANUM
               MOVE    WRK-PTHKN-KIGO  TO  ONSKAKU-SHO-KIGO
               MOVE    WRK-PTHKN-NUM   TO  ONSKAKU-SHO-NUM 
               MOVE    WRK-PTHKN-EDABAN
                                       TO  ONSKAKU-SHO-EDABAN
           ELSE
               MOVE    WRK-PTKOH-FTNJANUM
                                       TO  ONSKAKU-SHO-HKNJANUM
               MOVE    WRK-PTKOH-JKYSNUM
                                       TO  ONSKAKU-SHO-NUM 
               MOVE    "A1"            TO  ONSKAKU-CARD-CLASS
           END-IF
           MOVE    PTINF-BIRTHDAY  TO  ONSKAKU-SHO-BIRTHDAY
           MOVE    "0"             TO  ONSKAKU-SHO-DOUIFLG
      *
      *    「Yes」で資格確認レコードを作成する場合
      *    資格確認訪問診療取得処理
           IF      ONSKAKU-SHO-NUM NOT =   SPACE
               IF    ( FLG-ONSHI-HOUMON-CHK
                                           =   1    )
      *********AND   ( FLG-ONSHI-HOUMON    =   ZERO )
                   MOVE    ONS-HOUMON-ONSHINRYO-KBN
                                           TO  ONSKAKU-ONSHINRYO-KBN
                   MOVE    ONS-HOUMON-KENSHIN-DOUIFLG
                                           TO  ONSKAKU-KENSIN-DOUIFLG
                   MOVE    ONS-HOUMON-YAKUZAI-DOUIFLG
                                           TO  ONSKAKU-YAKUZAI-DOUIFLG
                   MOVE    ONS-HOUMON-SHINRYO-DOUIFLG
                                           TO  ONSKAKU-SHINRYO-DOUIFLG
                   MOVE    ONS-HOUMON-OPE-DOUIFLG
                                           TO  ONSKAKU-OPE-DOUIFLG
               END-IF
           END-IF    
      *
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO 
               MOVE    9               TO  LNK-ONSHI001-RC
           ELSE
      *        処方管理レコード作成処理
               PERFORM 2002211-SHOHO-KANRI-INSERT-SEC
           END-IF
            .
       20021-ONSHI-KAKU-INSERT-EXT.
           EXIT.
      *
      ******************************************************************
      *    併用のときの資格確認レコードの作成（保険と生保）
      ******************************************************************
       20021-ONSHI-KAKU-HEI-INSERT-SEC SECTION.
      *
           INITIALIZE                  WRK-FUJYO-HEIYO-AREA 
      *
      *    医療機関情報
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 960-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC    
           END-IF
      *        UIDコード取得処理
               PERFORM 200211-UID-GET-SEC
               MOVE    C-UID           TO  WRK-FUJYO-UUID
      *
      *        生保の資格確認レコード作成 
      *        UIDコード取得処理
               PERFORM 200211-UID-GET-SEC
               MOVE    C-UID           TO  WRK-SEH-TBL-UUID
      *
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    WRK-SEH-TBL-UUID
                                       TO  ONSKAKU-TBL-UUID
               MOVE    WRK-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
                                           ONSKAKU-FUJYO-UUID1
               MOVE    "1"             TO  ONSKAKU-FUJYO-HEIYO 
      *        MOVE                    TO  ONSKAKU-AITE-UUID
      *        MOVE                    TO  ONSKAKU-KAKU-RENUMBER
               MOVE    SPACE           TO  ONSKAKU-KAKU-ENDFLG
               MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
               MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
               MOVE    ZERO            TO  ONSKAKU-SHOU-RENUMBER
               MOVE    SPACE           TO  ONSKAKU-SHOU-ENDFLG
               MOVE    ZERO            TO  ONSKAKU-YAKUZAI-RENUMBER
               MOVE    SPACE           TO  ONSKAKU-YAKUZAI-ENDFLG
               MOVE    ZERO            TO  ONSKAKU-KENSIN-RENUMBER
               MOVE    SPACE           TO  ONSKAKU-KENSIN-ENDFLG
               MOVE    PTINF-PTID      TO  ONSKAKU-PTID 
      *        MOVE                    TO  ONSKAKU-FUKUSU-GAITO
      *        MOVE                    TO  ONSKAKU-SHORI-TIME 
      *        MOVE                    TO  ONSKAKU-KAKUNINYMD
               STRING  SYS-1001-PREFNUM
                                       DELIMITED  BY  SIZE
                       "1"             DELIMITED  BY  SIZE
                       SYS-1001-HOSPCD DELIMITED  BY  SIZE
                                       INTO       ONSKAKU-HOSPCD
               END-STRING                        
      *        旧医療機関コードが設定されているとき
               IF      SYS-1051-HOSPCDF    =   SPACE
                   CONTINUE
               ELSE
                   MOVE    SYS-1051-HOSPCDF    TO  ONSKAKU-HOSPCD
               END-IF    
               STRING  PTINF-PTID      DELIMITED  BY  SPACE
                       ","             DELIMITED  BY  SIZE  
                       C-UID           DELIMITED  BY  SIZE
                                       INTO       ONSKAKU-FILE-SIKIBETU
               END-STRING                        
               MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
               MOVE    "1"             TO  ONSKAKU-CODE-TYPE
               MOVE    WRK-PTKOH-FTNJANUM
                                       TO  ONSKAKU-SHO-HKNJANUM
               MOVE    WRK-PTKOH-JKYSNUM
                                       TO  ONSKAKU-SHO-NUM 
               MOVE    PTINF-BIRTHDAY  TO  ONSKAKU-SHO-BIRTHDAY
               MOVE    "0"             TO  ONSKAKU-SHO-DOUIFLG
               MOVE    "A1"            TO  ONSKAKU-CARD-CLASS
      *
               MOVE    SPA-OPID        TO  ONSKAKU-OPID
               MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
               MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
               MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
               MOVE    SPA-OPID        TO  ONSKAKU-OPID
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_onshi_kaku"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO 
                   MOVE    9               TO  LNK-ONSHI001-RC
               END-IF
      * 
      *    保険の資格確認レコード作成 
      *    UIDコード取得処理
           PERFORM 200211-UID-GET-SEC
           MOVE    C-UID           TO  WRK-HKN-TBL-UUID
      *
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    WRK-HKN-TBL-UUID
                                   TO  ONSKAKU-TBL-UUID
           MOVE    WRK-FUJYO-UUID  TO  ONSKAKU-FUJYO-UUID
                                       ONSKAKU-FUJYO-UUID1
           MOVE    "1"             TO  ONSKAKU-FUJYO-HEIYO 
      *    MOVE                    TO  ONSKAKU-AITE-UUID
      *    MOVE                    TO  ONSKAKU-KAKU-RENUMBER
           MOVE    SPACE           TO  ONSKAKU-KAKU-ENDFLG
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-KAKU-KAKUNINYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-KAKU-KAKUNINHMS 
           MOVE    ZERO            TO  ONSKAKU-SHOU-RENUMBER
           MOVE    SPACE           TO  ONSKAKU-SHOU-ENDFLG
           MOVE    ZERO            TO  ONSKAKU-YAKUZAI-RENUMBER
           MOVE    SPACE           TO  ONSKAKU-YAKUZAI-ENDFLG
           MOVE    ZERO            TO  ONSKAKU-KENSIN-RENUMBER
           MOVE    SPACE           TO  ONSKAKU-KENSIN-ENDFLG
           MOVE    PTINF-PTID      TO  ONSKAKU-PTID 
      *    MOVE                    TO  ONSKAKU-FUKUSU-GAITO
      *    MOVE                    TO  ONSKAKU-SHORI-TIME 
      *    MOVE                    TO  ONSKAKU-KAKUNINYMD
           STRING  SYS-1001-PREFNUM
                                   DELIMITED  BY  SIZE
                   "1"             DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD DELIMITED  BY  SIZE
                                   INTO       ONSKAKU-HOSPCD
           END-STRING                        
      *    旧医療機関コードが設定されているとき
           IF      SYS-1051-HOSPCDF    =   SPACE
               CONTINUE
           ELSE
               MOVE    SYS-1051-HOSPCDF    TO  ONSKAKU-HOSPCD
           END-IF    
           STRING  PTINF-PTID      DELIMITED  BY  SPACE
                   ","             DELIMITED  BY  SIZE  
                   C-UID           DELIMITED  BY  SIZE
                                   INTO       ONSKAKU-FILE-SIKIBETU
           END-STRING                        
           MOVE    "2"             TO  ONSKAKU-SHOKAI-KBN
           MOVE    "1"             TO  ONSKAKU-CODE-TYPE
           MOVE    WRK-PTHKN-HKNJANUM
                                   TO  ONSKAKU-SHO-HKNJANUM
           MOVE    WRK-PTHKN-KIGO  TO  ONSKAKU-SHO-KIGO
           MOVE    WRK-PTHKN-NUM   TO  ONSKAKU-SHO-NUM 
           MOVE    WRK-PTHKN-EDABAN
                                   TO  ONSKAKU-SHO-EDABAN
           MOVE    PTINF-BIRTHDAY  TO  ONSKAKU-SHO-BIRTHDAY
           MOVE    "0"             TO  ONSKAKU-SHO-DOUIFLG
      *
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    SPA-TERMID      TO  ONSKAKU-TERMID
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-CREYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-CREHMS
           MOVE    SPA-OPID        TO  ONSKAKU-OPID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO 
               MOVE    9               TO  LNK-ONSHI001-RC
           ELSE
      *        処方管理レコード作成処理
               PERFORM 2002211-SHOHO-KANRI-INSERT-SEC
           END-IF
           .
       20021-ONSHI-KAKU-HEI-INSERT-EXT.
           EXIT.
      * 
      ******************************************************************
      *    資格確認訪問診療取得処理
      ******************************************************************
       20021-ONSHI-HOUMON-CHECK-SEC    SECTION.
      *
           INITIALIZE                  ONSHI-HOUMON-REC
           MOVE    ZERO            TO  FLG-ONSHI-HOUMON-CHK
      * 
      *    LNK-ONSHI001-TBL-UUIDが空白のときに
      *    「Yes」で資格確認レコードを作成する場合
           IF    ( LNK-ONSHI001-TBL-UUID
                                       =   SPACE )
             IF  ( LNK-ONSHI001-SYOKBN =   "02"    OR  "04" )  
             OR  ( LNK-ONSHI001-SYOKBN =   "01"
                  AND
                   LNK-ONSHI001-GYOUMUCD
                                       =   "21"             )
      *********MOVE    1               TO  FLG-ONSHI-HOUMON-CHK
      *                                 
               INITIALIZE                  ONSHI-HOUMON-REC    
               MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
               MOVE    PTINF-PTID      TO  ONS-HOUMON-PTID 
               MOVE    "key2"          TO  WRK-MCP-PATHNAME
               MOVE    ONSHI-HOUMON-REC
                                       TO  MCPDATA-REC
               PERFORM 900-ONSHI-HOUMON-READ-SEC
               IF      FLG-ONSHI-HOUMON    =   ZERO
                   MOVE    1               TO  FLG-ONSHI-HOUMON-CHK
               ELSE
                   INITIALIZE                  ONSHI-HOUMON-REC    
                   MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
                   IF      WRK-PTHKNINF-OK =   1
                       MOVE    WRK-PTHKN-HKNJANUM
                                               TO  ONS-HOUMON-HKNJANUM
                       MOVE    WRK-PTHKN-KIGO  TO  ONS-HOUMON-KIGO
                       MOVE    WRK-PTHKN-NUM   TO  ONS-HOUMON-NUM 
                       MOVE    WRK-PTHKN-EDABAN
                                               TO  ONS-HOUMON-EDABAN
                   ELSE
                       MOVE    WRK-PTKOH-FTNJANUM
                                               TO  ONS-HOUMON-HKNJANUM
                       MOVE    WRK-PTKOH-JKYSNUM
                                               TO  ONS-HOUMON-NUM  
                   END-IF
                   MOVE    "key3"          TO  WRK-MCP-PATHNAME
                   MOVE    ONSHI-HOUMON-REC
                                           TO  MCPDATA-REC
                   PERFORM 900-ONSHI-HOUMON-READ-SEC
                   IF      FLG-ONSHI-HOUMON    =   ZERO
                       MOVE    1               TO  FLG-ONSHI-HOUMON-CHK
                   END-IF    
               END-IF
             END-IF  
           END-IF                                
           .
       20021-ONSHI-HOUMON-CHECK-EXT.
           EXIT.
           .
      * 
      ******************************************************************
      *    訪問診療レコードのＰＴＩＤ更新処理
      ******************************************************************
       20021-ONSHI-HOUMON-UPDATE-SEC  SECTION.
      *
      *    患者登録の登録時にLNK-ONSHI001-TBL-UUIDが空白以外のとき
           IF    ( LNK-ONSHI001-SYOKBN =   "02"  )
           AND   ( LNK-ONSHI001-GYOUMUCD
                                       =   "12"  )
           AND   ( LNK-ONSHI001-TBL-UUID
                                   NOT =   SPACE )
               INITIALIZE                  ONSHI-HOUMON-REC    
               MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
               MOVE    ONSKAKU-TBL-UUID
                                       TO  ONS-HOUMON-TBL-UUID 
               MOVE    "key"           TO  WRK-MCP-PATHNAME
               MOVE    ONSHI-HOUMON-REC
                                       TO  MCPDATA-REC
               PERFORM 900-ONSHI-HOUMON-READ-SEC
               IF      FLG-ONSHI-HOUMON    =   ZERO
                   CONTINUE
               ELSE
                   INITIALIZE                  ONSHI-HOUMON-REC    
                   MOVE    SPA-HOSPNUM     TO  ONS-HOUMON-HOSPNUM
                   MOVE    ONSKAKU-RES-HKNJANUM
                                           TO  ONS-HOUMON-HKNJANUM
                   MOVE    ONSKAKU-RES-KIGO
                                           TO  ONS-HOUMON-KIGO
                   MOVE    ONSKAKU-RES-NUM TO  ONS-HOUMON-NUM 
                   MOVE    ONSKAKU-RES-EDABAN
                                           TO  ONS-HOUMON-EDABAN
                   MOVE    "key3"          TO  WRK-MCP-PATHNAME
                   MOVE    ONSHI-HOUMON-REC
                                           TO  MCPDATA-REC
                   PERFORM 900-ONSHI-HOUMON-READ-SEC
               END-IF
      *        
                IF      FLG-ONSHI-HOUMON    =   ZERO
                   MOVE    LNK-ONSHI001-PTID   TO  ONS-HOUMON-PTID
                   MOVE    SMCNDATE-YMD        TO  ONS-HOUMON-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ONS-HOUMON-UPHMS
                   MOVE    ONSHI-HOUMON-REC    TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    9                   TO  LNK-ONSHI001-RC
                   END-IF
               END-IF
           END-IF    
           .
       20021-ONSHI-HOUMON-UPDATE-EXT.
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
      *    資格確認レコードの更新
      ******************************************************************
       20022-ONSHI-KAKU-UPDATE-SEC SECTION.
      *
           MOVE    SMCNDATE-YMD    TO  ONSKAKU-UPYMD
           MOVE    SMCNDATE-HMS    TO  ONSKAKU-UPHMS
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO 
               MOVE    9               TO  LNK-ONSHI001-RC
           ELSE
      *        処方管理レコード処理
               PERFORM 200221-SHOHO-KANRI-SYORI-SEC
           END-IF
      *
            .
       20022-ONSHI-KAKU-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *     処方管理レコード処理
      *****************************************************************
       200221-SHOHO-KANRI-SYORI-SEC   SECTION.
      *
      *    電子処方箋区分が１以外は作成しない
           IF      SYS-1051-DENSHISHOHO
                               NOT =   "1"
               GO  TO  200221-SHOHO-KANRI-SYORI-EXT
           END-IF
      *
           IF      ONSKAKU-SHOKAI-KBN
                                   =   "1" OR  "2"
               CONTINUE
           ELSE
               GO  TO  200221-SHOHO-KANRI-SYORI-EXT
           END-IF
      *
           INITIALIZE                  SHOHO-KANRI-REC
           MOVE    SPA-HOSPNUM     TO  SHOKANR-HOSPNUM
           MOVE    ONSKAKU-TBL-UUID
                                   TO  SHOKANR-TBL-UUID
           MOVE    1               TO  SHOKANR-RENNUM
           MOVE    SHOHO-KANRI-REC TO  MCPDATA-REC
           PERFORM 900-SHOHO-KANRI-READ-SEC
      *
           IF      FLG-SHOHO-KANRI =   ZERO
               IF      SHOKANR-PTID    =   ZERO 
                   MOVE    ONSKAKU-PTID    TO  SHOKANR-PTID
               END-IF    
               MOVE    ONSKAKU-SHO-SHOHO-KEITAI
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
                   MOVE    9               TO  LNK-ONSHI001-RC
               END-IF
           ELSE
               PERFORM 2002211-SHOHO-KANRI-INSERT-SEC
           END-IF
           .
       200221-SHOHO-KANRI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     処方管理レコード作成処理
      *****************************************************************
       2002211-SHOHO-KANRI-INSERT-SEC   SECTION.
      *
      *    電子処方箋区分が１以外は作成しない
           IF      SYS-1051-DENSHISHOHO
                               NOT =   "1"
               GO  TO  2002211-SHOHO-KANRI-INSERT-EXT
           END-IF
      *
           INITIALIZE                  SHOHO-KANRI-REC
           MOVE    SPA-HOSPNUM     TO  SHOKANR-HOSPNUM
           MOVE    ONSKAKU-TBL-UUID
                                   TO  SHOKANR-TBL-UUID
           MOVE    1               TO  SHOKANR-RENNUM 
           MOVE    ONSKAKU-KAKU-KAKUNINYMD
                                   TO  SHOKANR-SRYYMD
           MOVE    ONSKAKU-PTID
                                   TO  SHOKANR-PTID 
      *****MOVE                    TO  SHOKANR-HAKKOKBN
           MOVE    ZERO            TO  SHOKANR-DENPNUM
           MOVE    ONSKAKU-SHO-SHOHO-KEITAI
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
               MOVE    9               TO  LNK-ONSHI001-RC
           END-IF
           .
       2002211-SHOHO-KANRI-INSERT-EXT.
           EXIT.
      *
      *
      ******************************************************************
      *    資格確認要否依頼処理（強制）
      ******************************************************************
       2005-ONSHI-KAKU-IRAI-SEC    SECTION.
      *
      *    有効な保険の退避処理（医保分のみ対象）
           PERFORM 20011-PTHKNINF-TAIHI-SEC
      *****IF      WRK-PTHKNINF-OK =   ZERO
      *        有効な医療扶助の退避処理
               PERFORM 20013-PTKOHINF-TAIHI-SEC
      *        有効な保険又は医療扶助が存在しないとき
               IF      WRK-PTHKNINF-OK =   ZERO
                   IF      WRK-PTKOHINF-OK =   ZERO
                       MOVE   "No"         TO  LNK-ONSHI001-KAKUNIN-FLG
                       GO  TO  2005-ONSHI-KAKU-IRAI-EXT
                   ELSE
      *                医療扶助単独で負担者番号、受給者番号が空白のとき
                       IF    ( WRK-PTKOH-FTNJANUM     =   SPACE )
                       OR    ( WRK-PTKOH-JKYSNUM      =   SPACE )
                           MOVE   "No"     TO  LNK-ONSHI001-KAKUNIN-FLG
                           GO  TO  2005-ONSHI-KAKU-IRAI-EXT
                       END-IF
                   END-IF
               END-IF    
      *****END-IF    
      *
      *    資格確認テーブル検索（最新）
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
           IF      FLG-ONSHI-KAKU  =   1
               INITIALIZE                  ONSKAKU-REC
               MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
               MOVE    LNK-ONSHI001-PTID
                                       TO  ONSKAKU-PTID
               MOVE    "2"             TO  ONSKAKU-ORCA-SHOKAI-KBN
               MOVE    ONSKAKU-REC     TO  MCPDATA-REC
               PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
           END-IF
      *
      *    資格確認テーブルが存在しない場合
           IF      FLG-ONSHI-KAKU  =   1
      *        資格確認レコードの作成
               IF    ( WRK-PTHKNINF-OK =   1     )
               AND   ( WRK-PTKOHINF-OK =   1     )
      *            併用のときの資格確認レコードの作成
                   PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                   MOVE    "Yes"       TO  LNK-ONSHI001-KAKUNIN-FLG
                   MOVE    WRK-HKN-TBL-UUID
                                       TO  ONSKAKU-TBL-UUID
                   PERFORM 2003-PUSH-SEC
               ELSE                        
      *            資格確認レコードの作成
                   PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                   IF      ONSKAKU-SHO-NUM =   SPACE
                       MOVE    "No"        TO  LNK-ONSHI001-KAKUNIN-FLG
                   ELSE    
                       MOVE    "Yes"       TO  LNK-ONSHI001-KAKUNIN-FLG
                   END-IF
               END-IF
           ELSE
      *        マイナンバーカードのレコードが存在する場合      
               IF      ONSKAKU-SHOKAI-KBN
                                       =   "1"      
                   MOVE   "No"            TO  LNK-ONSHI001-KAKUNIN-FLG
      *----> 該当の患者の枝番が設定されていない場合は、どうする?              
                   GO  TO  2005-ONSHI-KAKU-IRAI-EXT
               END-IF    
      *
               IF    ( WRK-PTHKNINF-OK =   1 )
               AND   ( WRK-PTKOHINF-OK =   1 )
                   MOVE    ONSKAKU-REC     TO  WRK-ONSKAKU-REC
      *            併用のとき、併用の資格確認取得
                   IF      ONSKAKU-FUJYO-HEIYO
                                           =   "1"
                       MOVE    ONSKAKU-REC     TO  WRK-ONSKAKU-REC
                       MOVE    ZERO            TO  FLG-ONSHI-HEIYO
                       IF      ONSKAKU-CARD-CLASS  =   "A1"
                           MOVE    ONSKAKU-REC TO  HEIYO-ONSKAKU-REC (2)
                       ELSE
                           MOVE    ONSKAKU-REC TO  HEIYO-ONSKAKU-REC (1)
                       END-IF
      *
                       INITIALIZE                  ONSKAKU-REC
                       MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
                       MOVE    WRK-ONSKAKU-FUJYO-UUID1
                                               TO  ONSKAKU-FUJYO-UUID1
                       MOVE    WRK-ONSKAKU-TBL-UUID
                                               TO  ONSKAKU-TBL-UUID
                       MOVE    ONSKAKU-REC     TO  MCPDATA-REC
                       PERFORM 900-ONSHI-KAKU-READ-KEY28-SEC
                       IF      ONSKAKU-CARD-CLASS  =   "A1"
                           MOVE    ONSKAKU-REC     TO
                                                   HEIYO-ONSKAKU-REC (2)
                       ELSE
                           MOVE    ONSKAKU-REC     TO
                                                   HEIYO-ONSKAKU-REC (1)
                       END-IF    
                       MOVE    WRK-ONSKAKU-REC TO  ONSKAKU-REC
      *                 
                       IF    ( HEIYO-ONSKAKU-KAKU-ENDFLG (1)
                                               =   "02"    OR  "03" )
                       AND   ( HEIYO-ONSKAKU-KAKU-ENDFLG (2)
                                               =   "02"    OR  "03" )
                                   MOVE   "No"         TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                       ELSE                        
      *                    併用のときの資格確認レコードの作成
                           PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                           MOVE    "Yes"       TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           MOVE    WRK-HKN-TBL-UUID
                                               TO  ONSKAKU-TBL-UUID
                           PERFORM 2003-PUSH-SEC
                       END-IF
      ***             ELSE
      *                資格確認レコードの作成
      ***                 PERFORM 20021-ONSHI-KAKU-INSERT-SEC
      ***                 IF      ONSKAKU-SHO-NUM =   SPACE
      ***                     MOVE    "No"    TO  LNK-ONSHI001-KAKUNIN-FLG
      ***                 ELSE    
      ***                     MOVE    "Yes"   TO  LNK-ONSHI001-KAKUNIN-FLG
      ***                 END-IF
                   END-IF
               ELSE    
                   EVALUATE    ONSKAKU-KAKU-ENDFLG
                       WHEN    SPACE
                       WHEN    "01"
      *                    資格確認レコードの作成
                           PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                           IF      ONSKAKU-SHO-NUM     =   SPACE
                               MOVE    "No"    TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           ELSE    
                               MOVE    "Yes"   TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                           END-IF
                       WHEN    "03"
                       WHEN    "02"             
                           MOVE   "No"         TO
                                               LNK-ONSHI001-KAKUNIN-FLG
                   END-EVALUATE
                END-IF
           END-IF
      *
            .
       2005-ONSHI-KAKU-IRAI-EXT.
           EXIT.
      *
      ******************************************************************
      *     患者番号変更時強制資格確認
      ******************************************************************
       2006-ONSHI-KAKU-IRAI-SEC    SECTION.
      *                       
      *    有効な保険の退避処理（医保分のみ対象）
           PERFORM 20011-PTHKNINF-TAIHI-SEC
      *****IF      WRK-PTHKNINF-OK =   ZERO
      *        有効な医療扶助の退避処理
               PERFORM 20013-PTKOHINF-TAIHI-SEC
      *        有効な保険又は医療扶助が存在しないとき
               IF      WRK-PTHKNINF-OK =   ZERO
                   IF      WRK-PTKOHINF-OK =   ZERO
                       MOVE   "No"         TO  LNK-ONSHI001-KAKUNIN-FLG
                       GO  TO  2006-ONSHI-KAKU-IRAI-EXT
                   ELSE
      *                医療扶助単独で負担者番号、受給者番号が空白のとき
                       IF    ( WRK-PTKOH-FTNJANUM     =   SPACE )
                       OR    ( WRK-PTKOH-JKYSNUM      =   SPACE )
                           MOVE   "No"    TO  LNK-ONSHI001-KAKUNIN-FLG
                           GO  TO  2006-ONSHI-KAKU-IRAI-EXT
                       END-IF
                   END-IF
               END-IF    
      *****END-IF
      *
      *    資格確認レコードの作成
           IF    ( WRK-PTHKNINF-OK =   1     )
           AND   ( WRK-PTKOHINF-OK =   1     )
           AND   ( WRK-PTHKN-NUM
                               NOT =   SPACE )
      *      資格確認訪問診療取得処理
             PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
      *      tbl_onshi_houmonが存在し、診療区分=2 or 3のときに併用の場合、
      *      生保の資格確認は行わない
             IF      ONS-HOUMON-ONSHINRYO-KBN
                                  =   "2" OR  "3"
                 PERFORM 20021-ONSHI-KAKU-INSERT-SEC
                 IF      ONSKAKU-SHO-NUM =   SPACE
                     MOVE    "No"        TO  LNK-ONSHI001-KAKUNIN-FLG
                 ELSE    
                     MOVE     "Yes"      TO  LNK-ONSHI001-KAKUNIN-FLG
                     MOVE    ONSKAKU-TBL-UUID
                                         TO  LNK-ONSHI001-UID
                 END-IF
             ELSE
      *          併用のときの資格確認レコードの作成
                 PERFORM 20021-ONSHI-KAKU-HEI-INSERT-SEC
                 MOVE    "Yes"       TO  LNK-ONSHI001-KAKUNIN-FLG
                 MOVE    WRK-SEH-TBL-UUID
                                     TO  LNK-ONSHI001-UID
                 PERFORM 2003-PUSH-SEC
             END-IF  
           ELSE                        
      *        資格確認訪問診療取得処理
               PERFORM 20021-ONSHI-HOUMON-CHECK-SEC
               PERFORM 20021-ONSHI-KAKU-INSERT-SEC
               IF      ONSKAKU-SHO-NUM =   SPACE
                   MOVE    "No"        TO  LNK-ONSHI001-KAKUNIN-FLG
               ELSE    
                   MOVE    "Yes"       TO  LNK-ONSHI001-KAKUNIN-FLG
                   MOVE    ONSKAKU-TBL-UUID
                                       TO  LNK-ONSHI001-UID
               END-IF
           END-IF    
            .
       2006-ONSHI-KAKU-IRAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険証ＯＣＲ再照会処理
      *****************************************************************
       2007-HKNOCR-SHOKAI-SEC      SECTION.
      *
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-TBL-UUID
                                   TO  ONSKAKU-TBL-UUID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-SEC
           IF      FLG-ONSHI-KAKU  =   ZERO
               IF      LNK-ONSHI001-PTID
                                   >   0
                   MOVE    LNK-ONSHI001-PTID
                                        TO  ONSKAKU-PTID
               END-IF
               MOVE    LNK-ONSHI001-OCR-HKNJANUM
                                       TO  ONSKAKU-SHO-HKNJANUM
               MOVE    LNK-ONSHI001-OCR-KIGO
                                       TO  ONSKAKU-SHO-KIGO
               MOVE    LNK-ONSHI001-OCR-NUM
                                       TO  ONSKAKU-SHO-NUM
               MOVE    LNK-ONSHI001-OCR-EDABAN
                                       TO  ONSKAKU-SHO-EDABAN
               MOVE    LNK-ONSHI001-OCR-BIRTHDAY
                                       TO  ONSKAKU-SHO-BIRTHDAY
               STRING  LNK-ONSHI001-PTID   DELIMITED  BY  SIZE
                       ","                 DELIMITED  BY  SIZE
                       LNK-ONSHI001-TBL-UUID
                                           DELIMITED  BY  SPACE
                                           INTO    ONSKAKU-FILE-SIKIBETU
               END-STRING
               MOVE    "2"             TO  ONSKAKU-ORCA-SHOKAI-KBN
               MOVE    SPACE           TO  ONSKAKU-LOCK-KBN
                                           ONSKAKU-KAKUNINYMD
      *        資格確認レコードの更新
               PERFORM 20022-ONSHI-KAKU-UPDATE-SEC
               MOVE     "Yes"          TO  LNK-ONSHI001-KAKUNIN-FLG
               MOVE    ONSKAKU-TBL-UUID
                                       TO  LNK-ONSHI001-UID
      *
               IF      MCP-RC          =   ZERO 
                   PERFORM 2003-PUSH-SEC
               END-IF
           END-IF
           .
       2007-HKNOCR-SHOKAI-EXT.
           EXIT.
      * 
      *****************************************************************
      *    訪問診療同意一括照会処理
      *****************************************************************
       2010-HOUMON-IKKATU-SHOKAI-SEC      SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       ST
                                       UIDIO-AREA
      *
           IF      LNK-ONSHI001-OUT-TBL-UUID
                                       =   SPACE
      *        ＵＩＤが空白のとき
      *        shellid= P02V001 でtbl_jobkanri を追加
      *        JOBID最大値取得
               INITIALIZE                      JOBKANRI-REC
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               EVALUATE    LNK-ONSHI001-ONSHINRYO-KBN
                   WHEN    "2"
                       MOVE    WRK-CONS-JOB-SHELLID
                                                   TO  JOB-SHELLID
                   WHEN    "3"
                       MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                                   TO  JOB-SHELLID
               END-EVALUATE
               MOVE    "key8"              TO  WRK-MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-SEC
               MOVE    JOB-JOBID           TO  WRK-CONS-JOB-JOBID
               IF      WRK-CONS-JOB-JOBID  =   WRK-CONS-JOB-JOBID-MAX
                   MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
               END-IF    
               ADD     1                   TO  WRK-CONS-JOB-JOBID
      *        JOBID最大値+1のデータ削除
               INITIALIZE                      ORCSJOBKANRIAREA
               MOVE    "CHK"               TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
               EVALUATE    LNK-ONSHI001-ONSHINRYO-KBN
                   WHEN    "2"
                       MOVE    WRK-CONS-JOB-SHELLID
                                                   TO  JOB-SHELLID
                   WHEN    "3"
                       MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                                   TO  JOB-SHELLID
               END-EVALUATE
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA 
                                           JOBKANRI-REC
                                           SPA-AREA
               IF      SJOBKANRI-RETURN    =   ZERO
                   INITIALIZE                      ORCSJOBKANRIAREA
                   MOVE    "DEL"               TO  SJOBKANRI-MODE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
                   EVALUATE    LNK-ONSHI001-ONSHINRYO-KBN
                       WHEN    "2"
                           MOVE    WRK-CONS-JOB-SHELLID
                                                       TO  JOB-SHELLID
                       WHEN    "3"
                           MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                                       TO  JOB-SHELLID
                   END-EVALUATE
                   CALL    "ORCSJOB"           USING
                                               ORCSJOBKANRIAREA 
                                               JOBKANRI-REC
                                               SPA-AREA
                   IF      SJOBKANRI-RETURN    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    9                   TO  LNK-ONSHI001-RC
                       GO  TO  2010-HOUMON-IKKATU-SHOKAI-EXT
                   END-IF
               END-IF    
      *        JOBID最大値+1のデータ作成
               INITIALIZE                      JOBKANRI-REC
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-CONS-JOB-JOBID  TO  JOB-JOBID
               EVALUATE    LNK-ONSHI001-ONSHINRYO-KBN
                   WHEN    "2"
                       MOVE    WRK-CONS-JOB-SHELLID
                                                   TO  JOB-SHELLID
                   WHEN    "3"
                       MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                                   TO  JOB-SHELLID
               END-EVALUATE
               MOVE    LNK-ONSHI001-STARTYMD
                                           TO  JOB-STARTYMD
               MOVE    LNK-ONSHI001-ENDYMD TO  JOB-ENDYMD
               MOVE    SMCNDATE-HMS        TO  JOB-STARTTIME
               MOVE    SMCNDATE-YMD        TO  JOB-SRYYMD
               MOVE    C-UID               TO  JOB-KARTE-UID
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    JOBKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC      NOT =   ZERO 
                   MOVE    9                   TO  LNK-ONSHI001-RC
                   GO  TO  2010-HOUMON-IKKATU-SHOKAI-EXT
               END-IF
           ELSE
      *        ＵＩＤが設定されたとき
               INITIALIZE                      JOBKANRI-REC
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               EVALUATE    LNK-ONSHI001-ONSHINRYO-KBN
                   WHEN    "2"
                       MOVE    WRK-CONS-JOB-SHELLID
                                                   TO  JOB-SHELLID
                   WHEN    "3"
                       MOVE    WRK-CONS-ONLINE-JOB-SHELLID
                                                   TO  JOB-SHELLID
               END-EVALUATE
               MOVE    LNK-ONSHI001-OUT-TBL-UUID
                                           TO  JOB-KARTE-UID
               MOVE    "key6"              TO  WRK-MCP-PATHNAME
               PERFORM 900-JOBKANRI-READ-SEC
               IF      FLG-JOBKANRI        =   ZERO
                   MOVE    LNK-ONSHI001-STARTYMD
                                               TO  JOB-STARTYMD
                   MOVE    LNK-ONSHI001-ENDYMD TO  JOB-ENDYMD
                   MOVE    SMCNDATE-HMS        TO  JOB-STARTTIME
                   MOVE    SMCNDATE-YMD        TO  JOB-SRYYMD
                   MOVE    SPACE               TO  JOB-ERRCD
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    JOBKANRI-REC        TO  MCPDATA-REC
                   MOVE    "tbl_jobkanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC      NOT =   ZERO 
                       MOVE    9                   TO  LNK-ONSHI001-RC
                       GO  TO  2010-HOUMON-IKKATU-SHOKAI-EXT
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
      *
           INITIALIZE                      PUSHQUA-REC
           MOVE    "Bulk_Qualification"
                                       TO  PUSHQUA-EVENT
           MOVE    "Rreq"              TO  PUSHQUA-CLASS
           MOVE    SPA-HOSPNUM         TO  PUSHQUA-HOSPNUM
           MOVE    WRK-HEN-DATE        TO  PUSHQUA-KAKUNINYMD
           MOVE    JOB-KARTE-UID       TO  PUSHQUA-UUID
           EVALUATE    LNK-ONSHI001-ONSHINRYO-KBN
               WHEN    "2"
                   MOVE    "V001"              TO  PUSHQUA-PROCESS-CLASS
               WHEN    "3"
                   MOVE    "O001"              TO  PUSHQUA-PROCESS-CLASS
           END-EVALUATE
           DISPLAY "PUSH=" PUSHQUA-REC
      *
           MOVE    PUSHQUA-REC         TO  MCPDATA-REC
           MOVE    "PUSHEVENT"         TO  MCP-FUNC
           MOVE    "push_onlinequa"    TO  MCP-TABLE
           MOVE    "push_onlinequa"    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       2010-HOUMON-IKKATU-SHOKAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    PUSH処理
      *****************************************************************
       2003-PUSH-SEC             SECTION.
      * 
           INITIALIZE                      PUSHQUA-REC
           MOVE    "patient_qualification"
                                       TO  PUSHQUA-EVENT
           MOVE    SPA-HOSPNUM         TO  PUSHQUA-HOSPNUM
           MOVE    WRK-PTNUM           TO  PUSHQUA-PATIEND-ID
           MOVE    WRK-HEN-DATE        TO  PUSHQUA-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  PUSHQUA-INFORMATION-TIME
           MOVE    LNK-ONSHI001-KAKUNIN-FLG
                                       TO  PUSHQUA-CLASS
           MOVE    ONSKAKU-TBL-UUID    TO  PUSHQUA-UUID
      *
      *??
             display "push=" PUSHQUA-REC "#"
      *??
           MOVE    PUSHQUA-REC         TO  MCPDATA-REC
           MOVE    "PUSHEVENT"         TO  MCP-FUNC
           MOVE    "push_onlinequa"    TO  MCP-TABLE
           MOVE    "push_onlinequa"    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       2003-PUSH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報とのチェック処理
      *****************************************************************
       2004-ONSHI-KAKU-CHECK-SEC   SECTION.
      *
           MOVE    ZERO            TO  WRK-PTHKNINF-OK
      *
           INITIALIZE                  ONSKAKU-REC
           MOVE    SPA-HOSPNUM     TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI001-PTID
                                   TO  ONSKAKU-PTID
           MOVE    ONSKAKU-REC     TO  MCPDATA-REC
           PERFORM 900-ONSHI-KAKU-READ-KEY3-SEC
      *
      *    資格確認テーブルが存在する場合
           IF      FLG-ONSHI-KAKU  =   ZERO
               IF      ONSKAKU-SIKAKU-YUKO
                                       =   "1" OR  "2"
                   INITIALIZE                  PTHKNINF-REC
                   MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
                   MOVE    LNK-ONSHI001-PTID
                                           TO  PTHKN-PTID
                   MOVE    SMCNDATE-YMD    TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
                   MOVE    PTHKNINF-REC    TO  MCPDATA-REC
                   MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key8"          TO  MCP-PATHNAME
                       PERFORM 900-PTHKNINF-READ-N-SEC
                   ELSE
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    1               TO  FLG-PTHKNINF
                   END-IF
      *
                   PERFORM         UNTIL   FLG-PTHKNINF    =   1
                                   OR      FLG-PTHKNINF-OK =   1
                       IF    ( ONSKAKU-RES-HKNJANUM
                                           =   PTHKN-HKNJANUM )
                       AND   ( ONSKAKU-RES-KIGO
                                           =   PTHKN-KIGO     )
                       AND   ( ONSKAKU-RES-NUM
                                           =   PTHKN-NUM      )
                       AND   ( ONSKAKU-RES-EDABAN
                                           =   PTHKN-EDABAN   )
                           MOVE    1               TO  WRK-PTHKNINF-OK
                       END-IF    
                       MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                       MOVE    "key8"          TO  MCP-PATHNAME
                       PERFORM 900-PTHKNINF-READ-N-SEC
                   END-PERFORM 
                   MOVE    "tbl_pthkninf"  TO  MCP-TABLE
                   MOVE    "key8"          TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
           END-IF
      *
           IF      WRK-PTHKNINF-OK     =   1
               EVALUATE    LNK-ONSHI001-SYOKBN
                   WHEN    "01"
                       MOVE    ONSKAKU-SIKAKU-YUKO
                                           TO  LNK-ONSHI001-SIKAKU-YUKO
                   WHEN    "02"
                       MOVE    "No"        TO  LNK-ONSHI001-KAKUNIN-FLG
               END-EVALUATE
           END-IF
                               
           .
       2004-ONSHI-KAKU-CHECK-EXT.
           EXIT.
      *
      ******************************************************************
      *    オンライン資格情報編集処理
      ******************************************************************
       2009-ONS-KAKU-HENSYU-SEC    SECTION.
      *
           INITIALIZE                  WRK-RES-NAME
                                       WRK-RES-KANANAME
                                       WRK-RES-KANANAME11
                                       WRK-RES-KANANAME12
      *    氏名、カナ氏名の空白を除く 
           MOVE    1                   TO  IDY
           PERFORM VARYING IDX FROM    1   BY  2
                   UNTIL   IDX >       49
                   OR      ONSKAKU-RES-KANANAME (IDX:1) =   SPACE
               IF      ONSKAKU-RES-KANANAME (IDX:2) =  "　" 
                   CONTINUE
               ELSE    
                   MOVE    ONSKAKU-RES-KANANAME (IDX:2)
                                       TO  WRK-RES-KANANAME (IDY:2)
                   ADD     2           TO  IDY
               END-IF
           END-PERFORM
           MOVE    1                   TO  IDY
           PERFORM VARYING IDX FROM    1   BY  2
                   UNTIL   IDX >       49
                   OR      ONSKAKU-RES-NAME (IDX:1) =   SPACE
               IF      ONSKAKU-RES-NAME (IDX:2) =  "　" 
                   CONTINUE
               ELSE    
                   MOVE    ONSKAKU-RES-NAME (IDX:2)
                                       TO  WRK-RES-NAME (IDY:2)
                   ADD     2           TO  IDY
               END-IF
           END-PERFORM
      *    カナ氏名を姓と名に分ける（分けれない場合クリア）             
           UNSTRING   ONSKAKU-RES-KANANAME DELIMITED  BY  "　" 
                                           INTO    WRK-RES-KANANAME11
                                                   WRK-RES-KANANAME12
           END-UNSTRING
           IF    ( WRK-RES-KANANAME11  =   SPACE )
           OR    ( WRK-RES-KANANAME12  =   SPACE )
               MOVE    SPACE               TO  WRK-RES-KANANAME1
           END-IF
          .
       2009-ONS-KAKU-HENSYU-EXT.
           EXIT. 
      *
      ******************************************************************
      *    終了処理
      ******************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "SYOKBN        =" LNK-ONSHI001-SYOKBN
           DISPLAY "GYOUMUCD      =" LNK-ONSHI001-GYOUMUCD
           DISPLAY "ONSHISINRYO   =" LNK-ONSHI001-ONSHISINRYO
           DISPLAY "PTID          =" LNK-ONSHI001-PTID
           DISPLAY "KAKUNIN-FLG   =" LNK-ONSHI001-KAKUNIN-FLG
           DISPLAY "SIKAKU-YUKO   =" LNK-ONSHI001-SIKAKU-YUKO
           DISPLAY "SIKAKU-YUKO-NM=" LNK-ONSHI001-SIKAKU-YUKO-NM
           DISPLAY "UID           =" LNK-ONSHI001-UID
           DISPLAY "TBL_UUID      =" LNK-ONSHI001-OUT-TBL-UUID
           DISPLAY "RC            =" LNK-ONSHI001-RC
      *
           .
       300-END-EXT.
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
      *
      ******************************************************************
      *    オンライン資格確認照会結果読込(UUID)
      ******************************************************************
       900-ONSHI-KAKU-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ONSKAKU-REC
                   MOVE    ZERO                TO  FLG-ONSHI-KAKU
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
      *    オンライン資格確認照会結果読込(PTID)
      ******************************************************************
       900-ONSHI-KAKU-READ-KEY3-SEC    SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-KAKU-READ-KEY3-EXT.
           EXIT.
      *
      ******************************************************************
      *    オンライン資格確認照会結果読込
      ******************************************************************
       900-ONSHI-KAKU-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSKAKU-REC
      *        公費は対象外         
               IF      ONSKAKU-SHOKAI-KBN  =   "9" OR  "8"
                   GO  TO  900-ONSHI-KAKU-READ-N-SEC
               END-IF
      *                                  
               IF      ONSKAKU-SHORI-TIME (1:8)
                                           =   SPACE
                   IF      ONSKAKU-KAKU-KAKUNINYMD
                                               =   SMCNDATE-YMD
                       MOVE    ZERO                TO  FLG-ONSHI-KAKU
                   ELSE
                       GO  TO  900-ONSHI-KAKU-READ-N-SEC
                   END-IF        
               ELSE
                   IF      ONSKAKU-SHORI-TIME (1:8)
                                               =   SMCNDATE-YMD
                       MOVE    ZERO                TO  FLG-ONSHI-KAKU
                   ELSE
                       GO  TO  900-ONSHI-KAKU-READ-N-SEC
                   END-IF
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
      *    オンライン資格確認照会結果読込(FUJYO-UUID1)
      ******************************************************************
       900-ONSHI-KAKU-READ-KEY28-SEC    SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key28"             TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N2-SEC
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           PERFORM             UNTIL   FLG-ONSHI-KAKU  =   1
                               OR      FLG-ONSHI-HEIYO =   1
      *        公費は対象外         
               IF      ONSKAKU-SHOKAI-KBN  =   "9" OR  "8"
                   MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
                   MOVE    "key28"             TO  MCP-PATHNAME
                   PERFORM 900-ONSHI-KAKU-READ-N2-SEC
               ELSE
                   MOVE    1                   TO  FLG-ONSHI-HEIYO
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key28"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-KAKU-READ-KEY28-EXT.
           EXIT.
      *
     ******************************************************************
      *    オンライン資格確認照会結果読込(PTID)
      ******************************************************************
       900-ONSHI-KAKU-READ-KEY20-SEC    SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
               MOVE    "key20"             TO  MCP-PATHNAME
               PERFORM 900-ONSHI-KAKU-READ-N-SEC
           ELSE
               INITIALIZE                      ONSKAKU-REC
               MOVE    1                   TO  FLG-ONSHI-KAKU
           END-IF
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-KAKU-READ-KEY20-EXT.
           EXIT.
      *
      ******************************************************************
      *    オンライン資格確認照会結果読込
      ******************************************************************
       900-ONSHI-KAKU-READ-N2-SEC    SECTION.
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
       900-ONSHI-KAKU-READ-N2-EXT.
           EXIT.
      *
      ******************************************************************
      *    医療扶助交付番号格納レコード読込
      ******************************************************************
       900-ONSHI-AIDLST-READ-N-SEC    SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ONSHI-AIDLST-REC
               MOVE    ZERO                TO  FLG-ONSHI-AIDLST
           ELSE
               INITIALIZE                      ONSHI-AIDLST-REC
               MOVE    1                   TO  FLG-ONSHI-AIDLST
           END-IF
      *
           .
       900-ONSHI-AIDLST-READ-N-EXT.
           EXIT.
      *
      ******************************************************************
      *    オンライン資格確認訪問診療
      ******************************************************************
       900-ONSHI-HOUMON-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
      *
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ONSHI-HOUMON-REC
                   MOVE    ZERO                TO  FLG-ONSHI-HOUMON
                ELSE
                   INITIALIZE                      ONSHI-HOUMON-REC
                   MOVE    1                   TO  FLG-ONSHI-HOUMON
               END-IF
           ELSE
               INITIALIZE                      ONSHI-HOUMON-REC
               MOVE    1                   TO  FLG-ONSHI-HOUMON
           END-IF
      *
           MOVE    "tbl_onshi_houmon"  TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-ONSHI-HOUMON-READ-EXT.
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
      *        削除分か職務上の事由の保険は対象外とする         
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
      *    保険者マスタ読込
      ******************************************************************
       900-HKNJAINF-READ-SEC       SECTION.
      *
           MOVE    HKNJAINF-REC       TO  MCPDATA-REC
      *  
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      ******************************************************************
      *    システム管理読込
      ******************************************************************
       960-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-SYSKANRI-READ-EXT.
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
       900-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_monthlynum"    TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理マスター読込処理
      *****************************************************************
       900-JOBKANRI-READ-SEC           SECTION.
      * 
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  JOBKANRI-REC
                   MOVE    ZERO                TO  FLG-JOBKANRI
               ELSE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    1                   TO  FLG-JOBKANRI
               END-IF
           ELSE
               INITIALIZE                      JOBKANRI-REC
               MOVE    1                   TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-JOBKANRI-READ-EXT.
           EXIT.
      *     
      ******************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      ******************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      ******************************************************************
      *    ＤＢＦＥＴＣＨ処理
      ******************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      ******************************************************************
      *    テーブルクローズ処理
      ******************************************************************
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
