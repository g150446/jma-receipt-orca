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
       PROGRAM-ID.             ORCSONSHI003.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : オンライン資格確認
      *  コンポーネント名  : 保険情報と患者保険情報チェックサブ
      *  返却エラーコード  : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  23/10/XX    ORCAMO        新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-ONSKAKU         PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-KIKAN-OK        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX-SK              PIC 9(04).
           03  IDX-SK2             PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-GEN             PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDK                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-FTNRATE-9           PIC 9(03).
           03  WRK-ON-RATE             PIC 9(03).
           03  WRK-FTN-RATE            PIC 9(03).
           03  WRK-HKNJANUM            PIC X(08).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HKNNUM-CK           PIC X(03).
           03  WRK-HKNJA-HKNNUM        PIC X(03).
           03  WRK-KETA                PIC 9(02).
           03  WRK-MAE-KAKUNINYMD      PIC X(08).
      *
           03  WRK-MOTOPG              PIC X(08).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *    保険マスタ
       01  HZN-PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC"  REPLACING
                                     //PTHKN-// BY //HZN-PTHKN-//.
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    補助区分変換パラメタ
           COPY    "CPORAPI012SUB1.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    保険確認年月日履歴登録サブ
           COPY    "CPORCSPTHKNRRK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    共通領域
      *     COPY    "MCPAREA".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    オンライン資格確認照会結果テーブル
       01  ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    共通領域
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *    編集パラメタ
           COPY    "CPORCSONSHI003.INC".
      *    オンライン資格確認照会結果テーブル
       01  LNK-ONSKAKU-REC.
           COPY    "CPONSHI-KAKU.INC"
                                   REPLACING
                                   //ONSKAKU-// BY //LNK-ONSKAKU-//.
      *
       PROCEDURE                    DIVISION    USING
           MCPAREA
           SPA-AREA
           ORCSONSHI003AREA
           LNK-ONSKAKU-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  LNK-ONSHI003-OUT1-AREA
           INITIALIZE                  LNK-ONSHI003-OUT2-AREA
           INITIALIZE                  LNK-ONSHI003-OUT3-AREA
           INITIALIZE                  LNK-ONSHI003-RC 
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
           IF     (LNK-ONSHI003-RC     =   ZERO)
               EVALUATE    LNK-ONSHI003-SYOKBN
               WHEN    "01"
      *            保険情報チェック
                   PERFORM 210-ONSHIKAKU-HEN-SEC
               WHEN    "02"
      *            保険の種類編集
                   PERFORM 220-ONS-HKNNUM-HEN-SEC
               WHEN    "03"
      *            予約照会から保険の枝番のみ更新
                   PERFORM 230-ONS-EDANUM-SYORI-SEC
               END-EVALUATE
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    処理区分
           IF       LNK-ONSHI003-SYOKBN    =   "01"
                                           OR  "02"
                                           OR  "03"
               CONTINUE
           ELSE
               MOVE    1                   TO  LNK-ONSHI003-RC
           END-IF
      *    システム日付
           IF      LNK-ONSHI003-SYSYMD =   SPACE
               MOVE    SPA-SYSYMD          TO  LNK-ONSHI003-SYSYMD
           END-IF
      *
           IF     (LNK-ONSKAKU-REC     =   SPACE )
               IF      LNK-ONSHI003-TBL-UUID   =   SPACE
                   MOVE    1                   TO  LNK-ONSHI003-RC
               ELSE
      *            TBL-UUID  からオン資格を検索
                   PERFORM 1001-ONSHI-KAKU-READ-SEC
                   IF      FLG-ONSKAKU         =   1
                       MOVE    1                   TO  LNK-ONSHI003-RC
                   END-IF
               END-IF
           ELSE
      *        パラメタを設定
               MOVE    LNK-ONSKAKU-REC     TO  ONSKAKU-REC
               MOVE    ONSKAKU-TBL-UUID    TO  LNK-ONSHI003-TBL-UUID
           END-IF
      *    患者ID
           IF      LNK-ONSHI003-PTID   =   ZERO
               MOVE    ONSKAKU-PTID        TO  LNK-ONSHI003-PTID
           END-IF
           IF      LNK-ONSHI003-PTID   =   ZERO
               MOVE    1                   TO  LNK-ONSHI003-RC
           END-IF
      *    保険者情報
           IF      LNK-ONSHI003-RC NOT =   ZERO
               GO  TO  100-INT-EXT
           END-IF
      *        
      *    保険情報検索
           PERFORM 210-PTHKNINF-HEN-SEC
      *
      *    対象保険なし
           IF      FLG-KIKAN-OK        =   ZERO
               INITIALIZE                  PTHKNINF-REC
               MOVE    2                   TO  LNK-ONSHI003-RC
               MOVE    "99"                TO  LNK-ONSHI003-CHK-KBN
           END-IF
           .
       100-INT-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認編集処理
      *****************************************************************
       210-ONSHIKAKU-HEN-SEC           SECTION.
      *
      *    本人家族区分
           IF      ONSKAKU-RES-HONKZKKBN   =   "1"
                                           OR  "2"
               IF      ONSKAKU-RES-HONKZKKBN   =   PTHKN-HONKZKKBN
                   CONTINUE
               ELSE
                   MOVE    "1"         TO  LNK-ONSHI003-CHK-HONKZKKBN
               END-IF
           END-IF
      *
      *    負担割合チェック
           MOVE    ZERO                TO  WRK-ON-RATE
      *    補助区分（高齢者の負担割合）
           IF      ONSKAKU-FTNRATE         =   SPACE
                                           OR   ZERO
               MOVE    ZERO                TO  WRK-ON-RATE
           ELSE
               MOVE    ONSKAKU-FTNRATE     TO  WRK-FTNRATE-9
               MOVE    WRK-FTNRATE-9       TO  WRK-ON-RATE
           END-IF
      *    高齢受給者証があれば優先（前期高齢者）
           IF      ONSKAKU-ELDER-SKKGETYMD     NOT =   SPACE
      *        高齢受給者一部負担金割合
               IF      ONSKAKU-ELDER-FTNRATE   =   SPACE
                                               OR  ZERO
                   MOVE    ZERO                TO  WRK-ON-RATE
               ELSE
                   MOVE    ONSKAKU-ELDER-FTNRATE   TO  WRK-FTNRATE-9
                   MOVE    WRK-FTNRATE-9       TO  WRK-ON-RATE
               END-IF
           END-IF
           
      *    補助区分から負担割合を取得
           INITIALIZE                  ORAPI012SUB1AREA
           MOVE    PTHKN-HKNNUM        TO  API012SUB1-HKNNUM
           MOVE    PTHKN-HOJOKBN       TO  API012SUB1-HOJOKBN
           MOVE    LNK-ONSHI003-SYSYMD TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
      *    負担割合
           MOVE    API012SUB1-RATE     TO  WRK-FTN-RATE
      *    オン資格の負担割合がある時チェック
           IF      WRK-ON-RATE     NOT =   ZERO
               IF      WRK-ON-RATE     NOT =   WRK-FTN-RATE
                   MOVE    "1"         TO  LNK-ONSHI003-CHK-FTNRATE
               END-IF
           END-IF
      *
      *    オン資格の保険の種類
           PERFORM 220-ONS-HKNNUM-HEN-SEC
      *    保険の種類が一致しない時（国保）などのみ
           IF      (LNK-ONSHI003-MAE-HKNNUM    =   SPACE )
               IF      LNK-ONSHI003-HKNNUM     =   PTHKN-HKNNUM
                   CONTINUE
               ELSE
      *            オン資格の保険番号と患者保険情報が一致しない
                   IF      PTHKN-HKNNUM        =   "068"
                                               OR  "069"
                                               OR  "040"
      *                特別療養であれば不一致
                       MOVE    "1"         TO  LNK-ONSHI003-CHK-HKNNUM
                   END-IF
               END-IF
           ELSE
      *        被保険者資格証明書で置換えあり
               IF      LNK-ONSHI003-HKNNUM     =   PTHKN-HKNNUM
                   CONTINUE
               ELSE
                   MOVE    "1"             TO  LNK-ONSHI003-CHK-HKNNUM
               END-IF
           END-IF
      *
           IF     (LNK-ONSHI003-CHK-HKNNUM     =   "1")
               OR (LNK-ONSHI003-CHK-FTNRATE    =   "1")
               OR (LNK-ONSHI003-CHK-HONKZKKBN  =   "1")
      *        不一致あり
               MOVE    "01"            TO  LNK-ONSHI003-CHK-KBN
           END-IF
           .
       210-ONSHIKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報決定処理
      *****************************************************************
       210-PTHKNINF-HEN-SEC           SECTION.
      *
      *    保険情報検索
           INITIALIZE                  HZN-PTHKNINF-REC
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    LNK-ONSHI003-PTID   TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  FLG-KIKAN-OK
           PERFORM UNTIL  (FLG-PTHKNINF    =   1 )
                      OR  (FLG-KIKAN-OK    =   1  )
      *        オン資格情報と一致する
               IF    (PTHKN-SAKJOKBN       =   SPACE )
               AND   (PTHKN-HKNJANUM       =   ONSKAKU-RES-HKNJANUM)
               AND   (PTHKN-KIGO           =   ONSKAKU-RES-KIGO )
               AND   (PTHKN-NUM            =   ONSKAKU-RES-NUM )
      *        システム日で有効であれば対象
                   IF     (PTHKN-TEKSTYMD      <=  LNK-ONSHI003-SYSYMD)
                   AND    (PTHKN-TEKEDYMD      >=  LNK-ONSHI003-SYSYMD)
                       MOVE    1               TO  FLG-KIKAN-OK
                       MOVE    PTHKNINF-REC    TO  HZN-PTHKNINF-REC
                   END-IF
               END-IF
               IF      FLG-KIKAN-OK        =   ZERO
                   MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-PTHKNINF-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       210-PTHKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    オン資格確認　保険の種類編集処理
      *****************************************************************
       220-ONS-HKNNUM-HEN-SEC        SECTION.
      *
           MOVE    ONSKAKU-RES-HKNJANUM    TO  WRK-HKNJANUM
           PERFORM 21011-HKNJAINF-HEN-SEC
      *
           MOVE    SPACE               TO  WRK-HKNNUM-CK
      *    被保険者資格証明書
           IF      ONSKAKU-CARD-CLASS  =   "05"
      *        国保を「068 特別療養」
               IF      WRK-HKNJA-HKNNUM    =   "060"
                   MOVE    "068"           TO  WRK-HKNNUM-CK
               END-IF
      *        退職国保を「069 退職特別療養費」
               IF      WRK-HKNJA-HKNNUM    =   "067"
                   MOVE    "069"           TO  WRK-HKNNUM-CK
               END-IF
      *        後期高齢者を「040 後期特療費」
               IF      WRK-HKNJA-HKNNUM    =   "039"
                   MOVE    "040"           TO  WRK-HKNNUM-CK
               END-IF
           END-IF
      *
            IF      WRK-HKNNUM-CK      =   SPACE
                MOVE    WRK-HKNJA-HKNNUM   TO  LNK-ONSHI003-HKNNUM
                MOVE    SPACE              TO  LNK-ONSHI003-MAE-HKNNUM
            ELSE
                MOVE    WRK-HKNNUM-CK      TO  LNK-ONSHI003-HKNNUM
                MOVE    WRK-HKNJA-HKNNUM   TO  LNK-ONSHI003-MAE-HKNNUM
            END-IF
      *
           .
       2101-ONS-HKNNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類編集処理
      *****************************************************************
       21011-HKNJAINF-HEN-SEC            SECTION.
      *
           MOVE    SPACE                 TO  WRK-HKNJA-HKNNUM
      *    保険者マスタ　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    ONSKAKU-RES-HKNJANUM  TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNNUM        TO  WRK-HKNJA-HKNNUM
           ELSE
      *        保険者マスタ登録なし
      *        桁数チェック
               MOVE    ZERO                TO  WRK-KETA
               PERFORM VARYING IDK         FROM    8   BY  -1
                       UNTIL   (IDK            <   1   )
                           OR  (WRK-KETA   NOT =   ZERO )
                   IF      ONSKAKU-RES-HKNJANUM(IDK:1)
                                           NOT =   SPACE
                       MOVE    IDK         TO  WRK-KETA 
                   END-IF
               END-PERFORM
      *        ６桁は国保とする
               IF     WRK-KETA             =   6
                  MOVE    "060"            TO  WRK-HKNJA-HKNNUM
               ELSE
                   PERFORM 21011-HKNNUM-HBTNUM-SEC
               END-IF
           END-IF
           .
       21011-HKNJAINF-HEN-EXT.
           EXIT.
      ****************************************************************
      *    法別番号から保険種類編集処理
      ****************************************************************
       21011-HKNNUM-HBTNUM-SEC     SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    ONSKAKU-RES-HKNJANUM (1:2)
                                       TO  HKN-HBTNUM
           MOVE    LNK-ONSHI003-SYSYMD TO  HKN-TEKSTYMD
           MOVE    LNK-ONSHI003-SYSYMD TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-READ-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
                MOVE    1                  TO  FLG-HKNNUM
           END-IF
           PERFORM UNTIL    FLG-HKNNUM     =  1
      *        主保険のみ対象
      *        支払区分のないものは対象外とする
               IF     (HKN-HKNKOHSBTKBN    =   "1"   )
                AND   (HKN-PAYKBN      NOT =   SPACE )
                   MOVE    HKN-HKNNUM      TO  WRK-HKNJA-HKNNUM
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 921-HKNNUM-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       21011-HKNNUM-HBTNUM-EXT.
           EXIT.
      *****************************************************************
      *    予約照会　処理
      *****************************************************************
       230-ONS-EDANUM-SYORI-SEC        SECTION.
      *
           IF    (HZN-PTHKNINF-REC     =   SPACE )
             OR  (ONSKAKU-ORCA-SHOKAI-KBN  NOT =  "1")
      *        予約照会以外、該当する保険がない
               MOVE    "02"                TO  LNK-ONSHI003-YYKERR
               GO  TO  230-ONS-EDANUM-SYORI-EXT
           END-IF
      *
           MOVE    HZN-PTHKNINF-REC    TO  PTHKNINF-REC
           IF      (PTHKN-HKNJANUM     =   ONSKAKU-RES-HKNJANUM)
             AND   (PTHKN-KIGO         =   ONSKAKU-RES-KIGO )
             AND   (PTHKN-NUM          =   ONSKAKU-RES-NUM )
               IF     (PTHKN-EDABAN        =   ONSKAKU-RES-EDABAN )
      *            枝番一致　更新なし
                   MOVE    "00"            TO  LNK-ONSHI003-YYKERR
               ELSE
      *            枝番不一致
                   MOVE    "01"            TO  LNK-ONSHI003-YYKERR
                   IF      PTHKN-EDABAN    =   SPACE
      *                枝番更新可
                       MOVE    "1"         TO  LNK-ONSHI003-CHK-EDABAN
                   ELSE
      *                更新不可
                       MOVE    SPACE       TO  LNK-ONSHI003-CHK-EDABAN
                   END-IF
               END-IF
           ELSE
               MOVE    "02"                TO  LNK-ONSHI003-YYKERR
           END-IF
      *
           IF     (LNK-ONSHI003-SYOKBN2    =   "UPD" )
              AND (LNK-ONSHI003-CHK-EDABAN =   "1"   )
      *        枝番更新可の時、枝番更新
               PERFORM 230-ONS-EDANUM-UPD-SEC
           END-IF
           .
       230-ONS-EDANUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報　枝番更新処理
      *****************************************************************
       230-ONS-EDANUM-UPD-SEC        SECTION.
      *
      *    保険情報検索
           PERFORM 9101-PTHKNINF-KEY-SEC
      *
           IF      (FLG-PTHKNINF       =   ZERO  )
             AND   (PTHKN-SAKJOKBN     =   SPACE )
             AND   (PTHKN-HKNJANUM     =   ONSKAKU-RES-HKNJANUM)
             AND   (PTHKN-KIGO         =   ONSKAKU-RES-KIGO )
             AND   (PTHKN-NUM          =   ONSKAKU-RES-NUM )
             AND   (PTHKN-EDABAN       =   SPACE           )
             AND   (PTHKN-EDABAN   NOT =   ONSKAKU-RES-EDABAN )
      *        枝番のみ不一致
               CONTINUE
           ELSE
               GO  TO  230-ONS-EDANUM-UPD-EXT
           END-IF
      *
      *    排他制御を行う
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD        NOT =   SPACE
      *        排他中
               MOVE    "99"                TO  LNK-ONSHI003-YYKERR
               MOVE    SLOCK-MSG           TO  LNK-ONSHI003-YYKERRMSG
      *        患者番号排他制御解除
               PERFORM 990-LOCK-OUT-SEC
               GO  TO  230-ONS-EDANUM-UPD-EXT
           END-IF
      *
           MOVE    SPACE               TO  LNK-ONSHI003-YYKERR
      *
      *    枝番更新処理
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    ONSKAKU-RES-EDABAN  TO  PTHKN-EDABAN
      *???
      *    確認年月日を変更するなら行う
      *    前回確認日付
      **    IF      PTHKN-KAKUNINYMD    NOT =   SMCNDATE-YMD
      *         MOVE    PTHKN-KAKUNINYMD    TO  WRK-MAE-KAKUNINYMD
      *        確認年月日の変更
      *         MOVE    SMCNDATE-YMD        TO  PTHKN-KAKUNINYMD
      **    END-IF
      *
           MOVE    SMCNDATE-YMD        TO  PTHKN-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTHKN-UPHMS
           MOVE    SPA-OPID            TO  PTHKN-OPID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "09"                TO  LNK-ONSHI003-YYKERR
               DISPLAY "ONSHI PTHKNINF UPDATE ERR:" MCP-RC
                                               ",KEY:" PTHKN-KEY
           END-IF
      *??
      *    確認年月日を変更するなら行う
      *     IF     (LNK-ONSHI003-YYKERR     =   SPACE )
      *        AND (WRK-MAE-KAKUNINYMD  NOT =   SPACE )
      *        確認年月日履歴処理
      *         INITIALIZE                      ORCSPTHKNRRKAREA
      *         MOVE    PTHKN-PTID          TO  ORCSHKNRRK-PTID
      *         MOVE    PTHKN-HKNID         TO  ORCSHKNRRK-HKNID
      *         MOVE    WRK-MAE-KAKUNINYMD
      *                                     TO  ORCSHKNRRK-KAKUNINYMD
      *         CALL    "ORCSPTHKNRRK"      USING
      *                                         SPA-AREA
      *                                         ORCSPTHKNRRKAREA
      *         IF      ORCSHKNRRK-RC       =   9
      *             MOVE    "09"            TO  LNK-ONSHI003-YYKERR
      *         END-IF
      **   END-IF
      *    オン資格照会を処理済み
           IF      LNK-ONSHI003-YYKERR     =   SPACE
               PERFORM 2031-ONSIKAKU-UPDATE-SEC
           END-IF
           
           IF      LNK-ONSHI003-YYKERR     =   SPACE
      *        更新終了
               MOVE    "10"                TO  LNK-ONSHI003-YYKERR
           END-IF
      *       
      *    患者番号排他制御解除
           PERFORM 990-LOCK-OUT-SEC
           .
       230-ONS-EDANUM-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    オン資格処理済み更新処理
      *****************************************************************
       2031-ONSIKAKU-UPDATE-SEC        SECTION.
      *
           PERFORM 1001-ONSHI-KAKU-READ-SEC
           IF      FLG-ONSKAKU         =   1
               MOVE    "09"            TO  LNK-ONSHI003-YYKERR
               GO  TO  2031-ONSIKAKU-UPDATE-EXT
           END-IF
      *
           MOVE    SPACE               TO  ONSKAKU-LOCK-KBN
           MOVE    "1"                 TO  ONSKAKU-MOD-FLG
      *
           MOVE    SPA-OPID            TO  ONSKAKU-OPID
           MOVE    SMCNDATE-YMD        TO  ONSKAKU-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONSKAKU-UPHMS
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "19"            TO  LNK-ONSHI003-YYKERR
               DISPLAY "P031 ONSKAKU KEY UPD ERR:"  MCP-RC
                   ",KEY:" ONSKAKU-KEY
           END-IF
           .
       2031-ONSIKAKU-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    オン資格確認検索集処理
      *****************************************************************
       1001-ONSHI-KAKU-READ-SEC        SECTION.
      *
      *    オンライン資格確認UID検索処理
           INITIALIZE                      ONSKAKU-REC
           MOVE    SPA-HOSPNUM         TO  ONSKAKU-HOSPNUM
           MOVE    LNK-ONSHI003-TBL-UUID
                                       TO  ONSKAKU-TBL-UUID
      *
           MOVE    ONSKAKU-REC         TO  MCPDATA-REC
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ONSKAKU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001-ONSHI-KAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報検索処理
      *****************************************************************
       9101-PTHKNINF-KEY-SEC        SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9101-PTHKNINF-KEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン資格確認照会結果読込
      *****************************************************************
       900-ONSKAKU-READ-SEC         SECTION.
      *
           MOVE    "tbl_onshi_kaku"    TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  ONSKAKU-REC
               MOVE    ZERO            TO  FLG-ONSKAKU
           ELSE
               INITIALIZE                  ONSKAKU-REC
               MOVE    1               TO  FLG-ONSKAKU
           END-IF
           .
       900-ONSKAKU-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO                TO  FLG-HKNJAINF
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
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
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO                TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
           .
       921-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
           MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *    排他制御処理
           MOVE    "P02"               TO  SPA-MOTOPG
           MOVE    1                   TO  SLOCK-MODE
           MOVE    LNK-ONSHI003-PTID   TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
           MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込み処理
      *****************************************************************
       920-DBFETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
